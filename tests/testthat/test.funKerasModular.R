context("FunKerasGeneric")
skip_on_cran()

test_that("check funKerasGeneric: does resDummy work?", {
  target <- "age"
  nobs <- 100
  batch_size <- 32
  prop <- 0.5
  cachedir <- "oml.cache"
  dfCensus <- getDataCensus(target = target, nobs=nobs, cachedir = cachedir)
  data <- getGenericTrainValTestData(dfGeneric = dfCensus,
                                     prop = prop)
  specList <- genericDataPrep(data=data, batch_size = batch_size)
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 0
  kerasConf$resDummy <- TRUE
  x <- NULL
  res <- evalKerasGeneric(x,
                          kerasConf,
                          specList)
  expect_equal(dim(res)[2],7)
}
)

