context("FunKerasGeneric")
skip_on_cran()

test_that("check funKerasCensus: does model work as the default model conf?", {
  library("keras")
  library(tensorflow)
  library("tfdatasets")
  library("SPOT")
  library("rsample")
  set.seed(1)
  target <- "age"
  nobs <- 1000
  batch_size <- 32
  prop <- 0.5
  cachedir <- "oml.cache"
  dfCensus <- getDataCensus(target = target, nobs=nobs, cachedir=cachedir)
  data <- getGenericTrainValTestData(dfGeneric = dfCensus,
                                     prop = prop)
  specList <- genericDataPrep(data=data, batch_size = batch_size)
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 0
  x <- NULL
  res0 <- evalKerasGeneric(x,
                          kerasConf,
                          specList)
  cfg <- getModelConf("dl")
  # default <- c(0, 0,   5, 0, 1e-3, 4, 0.9,  0.99,  1, 1e-7)
  default <- cfg$defaults
  # types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
  #             "integer",  "numeric",  "numeric",  "integer",
  #            "numeric")
  types <- cfg$type
  x <- matrix(default, 1,)
  transformFun <- cfg$transformations
  if (length(transformFun) > 0) {  x <- transformX(xNat=x, fn=transformFun)}
  res1 <- evalKerasGeneric(x,
                           kerasConf = kerasConf,
                           specList = specList)
  # res0 and res1 should be similar
  expect_equal(res0,res1, tolerance = 1e-1)
}
)


test_that("check selectKerasOptimizer", {
  # library("keras")
  # library("tensorflow")
  # library("reticulate")
  # library("tfruns")
  # library("GGally")
  # library("rsample")
  # library("tfdatasets")
  # library("OpenML")
  # library("mlr")
  # library("plotly")
  # library("SPOT")
  # library("farff")
  # library(Metrics)
  # library("dplyr")
  # library(SPOTMisc)
  # # FIXME: remove after testing:
  # source("~/workspace/spotmisc/R/kerasOptimizer.R")
  # source("~/workspace/spotmisc/R/funKerasGeneric.R")

  k = 1
  nobs = 1e2
  task.type <- "classif"
  target <- "age"
  nobs <- nobs
  nfactors <- "high"
  nnumericals <- "high"
  cardinality <- "high"
  cachedir <- "oml.cache"
  prop <- c(2/3)
  verbosity <- 1
  score <- matrix(NA, nrow = 7, ncol=8)
  for (i in 1:7){
  x <- matrix(c(0, 0,  5,  0.5, 1e-3, 3, 0.9,  0.999,  1, 1e-7, i), ncol = 11, nrow =1)
  val <- predDlCensus(
    x = x,
    target = target,
    task.type = task.type ,
    nobs = nobs,
    nfactors = nfactors,
    nnumericals = nnumericals,
    cardinality = cardinality,
    cachedir = cachedir,
    k = k,
    prop = prop,
    verbosity = verbosity
  )
  score[i,] <- scorePredictions(val)
  }
   expect_equal(all(is.na(score)), FALSE)
})


test_that("check possibly problematic hyperparameter settings. Part 1", {
target <- "age"
batch_size <- 32
prop <- 2/3
cachedir <- "oml.cache"
dfGeneric <- getDataCensus(target = target, nobs = 1000, cachedir = cachedir)
data <- getGenericTrainValTestData(dfGeneric = dfGeneric, prop = prop)
specList <- genericDataPrep(data=data, batch_size = batch_size)

kerasConf <- getKerasConf()

## model configuration:
cfg <-  getModelConf("dl")
transformFun <- cfg$transformations
types <- cfg$type
lower <- cfg$lower
upper <- cfg$upper
## values are already transformed:
x1 <- matrix( c(0.005575913, 0.9559457,   32, 0.6546746, 0.0003094027,   64, 0.9129210, 0.9968694, 2, 5.123768e-09, 3), nrow=1)
x2 <- matrix( c(0.389134979, 0.9171664,   16, 0.2993133, 0.0028106961,   64, 0.9478681, 0.9977315, 1, 4.564494e-09, 5), nrow=1)
x3 <- matrix( c(0.083159844, 0.8085690,   64, 0.7728650, 0.0025142961,  128, 0.9338285, 0.9908804, 3, 2.125333e-09, 7), nrow=1)
x <- rbind(x1,x2,x3)

message("objectiveFunctionEvaluation(): x after transformX().")
print(x)
res <- funKerasGeneric(x, kerasConf = kerasConf, specList = specList)
expect_equal(dim(res)[1], 3)
})

test_that("check possibly problematic hyperparameter settings. Part 2", {
  target <- "age"
  batch_size <- 32
  prop <- 2/3
  cachedir <- "oml.cache"
  dfGeneric <- getDataCensus(target = target, nobs = 1000, cachedir = cachedir)
  data <- getGenericTrainValTestData(dfGeneric = dfGeneric, prop = prop)
  specList <- genericDataPrep(data=data, batch_size = batch_size)

  ## model configuration:
  cfg <-  getModelConf("dl")

  x1 <- matrix( c(0.19217784, 0.7654053, 8, 0.7808034, 0.003765993, 4, 0.9668184, 0.9944259, 3, 6.718810e-09,   6), nrow=1)
  x2 <- matrix( c(0.02955605, 0.7214196, 4, 0.4204896, 0.009399496, 3, 0.9515076, 0.9998612, 4, 2.823828e-09,   5), nrow=1)
  x3 <- matrix( c(0.04056175, 0.9998630, 7, 0.4596196, 0.002625838, 4, 0.9872504, 0.9995384, 2, 1.009500e-09,   4), nrow=1)
  x <- rbind(x1,x2,x3)
  transformFun <- cfg$transformations
  types <- cfg$type
  lower <- cfg$lower
  upper <- cfg$upper

  kerasConf <- getKerasConf()

  ### simple function call:
  message("objectiveFunctionEvaluation(): x before transformX().")
  print(x)
  if (length(transformFun) > 0) {  x <- transformX(xNat=x, fn=transformFun)}
  message("objectiveFunctionEvaluation(): x after transformX().")
  print(x)
  res <- funKerasGeneric(x, kerasConf = kerasConf, specList = specList)
  expect_equal(dim(res)[1], 3)
})






