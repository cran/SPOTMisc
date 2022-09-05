context("Census")
skip_on_cran()

test_that("split data correct?", {
  library("rsample")

  target <- "age"
  cachedir <- "oml.cache"
  task.type <- "classif"
  nobs <- 1e2 # max: 229285
  nfactors = "high"
  nnumericals <- "high"
  cardinality = "high"
  data.seed <- 1
  tuner.seed <- 1
  prop <- 2/3

  dfCensus <- getDataCensus(
    task.type = task.type,
    nobs = nobs,
    nfactors = nfactors,
    nnumericals = nnumericals,
    cardinality = cardinality,
    data.seed = data.seed,
    cachedir = cachedir,
    target = target
  )

  data <- getGenericTrainValTestData(
    dfGeneric = dfCensus,
    prop = prop)
  #44:
  expect_equal(dim(data$trainGeneric)[1], round(prop*prop*nobs))
  #22:
  expect_equal(dim(data$valGeneric)[1], round((1-prop)*prop*nobs))
  #34:
  expect_equal(dim(data$testGeneric)[1], round((1-prop)*nobs)+1)

  # no validation data, only test/train split:
  prop <- c(2/3, 1)
  dfCensus <- getDataCensus(
    task.type = task.type,
    nobs = nobs,
    nfactors = nfactors,
    nnumericals = nnumericals,
    cardinality = cardinality,
    data.seed = data.seed,
    cachedir = cachedir,
    target = target
  )

  data <- getGenericTrainValTestData(
    dfGeneric = dfCensus,
    prop = prop)
  #66:
  expect_equal(dim(data$trainGeneric)[1], round(prop[1]*nobs)-1)
  #NULL:
  expect_true(is.null( dim(data$valGeneric)[1]))
  #34:
  expect_equal(dim(data$testGeneric)[1], round((1-prop[1])*nobs)+1)

  ## target
  target <- "income_class"
  prop <- 2/3
  dfCensus <- getDataCensus(
    task.type = task.type,
    nobs = nobs,
    nfactors = nfactors,
    nnumericals = nnumericals,
    cardinality = cardinality,
    data.seed = data.seed,
    cachedir = cachedir,
    target = target
  )

  data <- getGenericTrainValTestData(
    dfGeneric = dfCensus,
    prop = prop)
  #44:
  expect_equal(dim(data$trainGeneric)[1], round(prop*prop*nobs))
  #22:
  expect_equal(dim(data$valGeneric)[1], round((1-prop)*prop*nobs))
  #34:
  expect_equal(dim(data$testGeneric)[1], round((1-prop)*nobs)+1)
})
