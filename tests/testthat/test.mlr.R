context("mlr")
skip_on_cran()

test_that("test ml config: is the learner correct?", {
  target <- "age"
  model <- "ranger"
  cachedir <- "oml.cache"
  task.type <- "classif"
  nobs <- 1e2
  nfactors <- "high"
  nnumericals <- "high"
  cardinality <- "high"
  data.seed <- 1
  prop <- 2 / 3
  data <- getDataCensus(
    task.type = task.type,
    nobs = nobs,
    nfactors = nfactors,
    nnumericals = nnumericals,
    cardinality = cardinality,
    data.seed = data.seed,
    cachedir = cachedir,
    target = target
  )
  cfg <-   getMlConfig(
    target = target,
    model = model,
    data = data,
    task.type = task.type,
    nobs = nobs,
    nfactors = nfactors,
    nnumericals = nnumericals,
    cardinality = cardinality,
    data.seed = data.seed,
    prop = prop
  )
  expect_equal(cfg$learner, "classif.ranger")
})
