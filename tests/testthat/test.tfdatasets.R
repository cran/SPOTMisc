context("FunKerasGeneric")
skip_on_cran()

# data preparation for all tests ------------------------------------------
target <- "age"
batch_size <- 32
prop <- 2/3
cachedir <- "oml.cache"
dfCensus <- getDataCensus(target = target,
                          nobs = 1e4, cachedir = cachedir, cache.only=FALSE)
data <- getGenericTrainValTestData(dfGeneric = dfCensus,
                                   prop = prop)

dataset <-  data$trainGeneric %>%
  tensor_slices_dataset() %>%
  dataset_batch(2)

get_features <- function(df, feature_columns) {

  if (tensorflow::tf$executing_eagerly())
    example <- reticulate::iter_next(reticulate::as_iterator(df))
  else {
    example <- make_iterator_one_shot(df)
    example <- iterator_get_next(example)
  }

  k <- keras::layer_dense_features(feature_columns = feature_columns)

  if (tensorflow::tf$executing_eagerly())
    return(k(example))
  else {
    res <- k(example)
    sess <- tf$Session()
    return(sess$run(res))
  }
}



# Tests -------------------------------------------------------------------

test_that("Can create a feature_spec", {
   specList <- genericDataPrep(data=data, batch_size = batch_size)
  df <- data$trainGeneric
  df <- df[-which(names(df) == "target")]
  factorVars <-  names(df[,sapply(df, is.character)])
  numVars <- names(df[,sapply(df, is.numeric)])
  # there should be an additional "indicator_" var for each factor var:
  expect_length(specList$specGeneric_prep$features(), 2*length(factorVars) + length(numVars))
    # expect_equal(sort(names(specList$specGeneric_prep$dense_features())), sort(names(df)[-which(names(df) == "target")]))
})


test_that("Can create a feature_spec_with_embedding", {
  minLevelSizeEmbedding <- 10
  specList <- genericDataPrep(data=data, batch_size = batch_size, minLevelSizeEmbedding = minLevelSizeEmbedding)
  df <- data$trainGeneric
  df <- df[-which(names(df) == "target")]
  factorVars <-  names(df[,sapply(df, is.character)])
  numVars <- names(df[,sapply(df, is.numeric)])
  embeddingVars <- names(df %>% mutate_if(is.character, factor) %>% select_if(~ is.factor(.) & nlevels(.) > minLevelSizeEmbedding))
  noEmbeddingVars <- names(df %>% mutate_if(is.character, factor) %>% select_if(~ is.factor(.) & nlevels(.) <= minLevelSizeEmbedding))
  # there should be an additional "indicator_" var for each factor var:
  expect_length(specList$specGeneric_prep$features(), length(factorVars) + length(numVars) + length(embeddingVars) + length(noEmbeddingVars))
})

test_that("100 % FALSE (identical results, always FALSE) should lead to a nearly perfect model", {
  target <- "age"
  batch_size <- 32
  prop <- 2/3
  cachedir <- "oml.cache"
  dfCensus <- getDataCensus(target = target,
                            nobs = 1e4, cachedir = cachedir, cache.only=FALSE)
  dfCensus$target <- factor(rep("FALSE", length(dfCensus$target)))
  data <- getGenericTrainValTestData(dfGeneric = dfCensus,
                                     prop = prop)
  specList <- genericDataPrep(data=data, batch_size = batch_size)
  ## model configuration:
  cfg <-  getModelConf(list(model="dl"))
  x <- matrix(cfg$default, nrow=1)
  transformFun <- cfg$transformations
  types <- cfg$type
  lower <- cfg$lower
  upper <- cfg$upper
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 0
  if (length(transformFun) > 0) {  x <- transformX(xNat=x, fn=transformFun)}
  res <- funKerasGeneric(x, kerasConf = kerasConf, specList = specList)
  expect_equal(res[,1], 0, tolerance = 0.1)
})




