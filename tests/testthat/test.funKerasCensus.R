context("funKerasCensus")
skip_on_cran()

test_that("check funKerasCensus: does model work as the default model conf?", {
  seed <- 1
  library("keras")
  library(tensorflow)
  library("tfdatasets")
  library("SPOT")
  #library("SPOTMisc")
  #source("~/workspace/spotmisc/R/funKerasCensus.R")
  library("rsample")
  set.seed(1)
  target <- "age"
  nobs <- 1000
  batch_size <- 32
  prop <- 0.5
  cachedir <- "oml.cache"
  dfCensus <- getDataCensus(target = target, nobs=nobs, cachedir = cachedir)
  data <- getGenericTrainValTestData(dfGeneric = dfCensus,
                                     prop = prop)
  specList <- genericDataPrep(data=data, batch_size = batch_size)
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 0
  x <- NULL
  res0 <- evalKerasGeneric(x,
                          kerasConf,
                          specList)
  cfg <- getModelConf(list(model="dl"))
  # default <- c(0, 0,   5, 0, 1e-3, 4, 0.9,  0.999,  1, 1e-7, 5)
  default <- cfg$defaults
  # types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
  #             "integer",  "numeric",  "numeric",  "integer",
  #            "numeric", "factor")
  types <- cfg$type
  x <- matrix(default, 1,)
  transformFun <- cfg$transformations
  if (length(transformFun) > 0) {  x <- transformX(xNat=x, fn=transformFun)}
  res1 <- evalKerasGeneric(x,
                           kerasConf = kerasConf,
                           specList = specList)
  ## FIXME: res0 and res1 should be similar
  ## to be implemented: a test that checks the following equal:
  ## expect_equal(res0,res1, tolerance = 3e-1)
  ## Because it is not implemented yet, only dimensions are checked
  ## (this is not really usefull, but better than noting):
  expect_equal(dim(res0), dim(res1))
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
  verbosity <- 0
  score <- matrix(NA, nrow = 7, ncol=8)
  for (i in 1:7){
  x <- matrix(c(0, 0,  3,  0.5, 1e-3, 3, 0.9,  0.999,  1, 1e-7, i), ncol = 11, nrow =1)
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



