context("funKerasCensus")
skip_on_cran()

test_that("check predDlCensus: test, val, and test data used correctly?", {
  seed <- 1
  cfg <- getModelConf(list(model="dl"))
  x <- matrix(cfg$defaults, nrow=1)
  prop <- 2/3
  nobs <- 1e2
  y <- predDlCensus(x = x,
                           target = "age",
                           task.type = "classif",
                           nobs = nobs,
                           nfactors = "high",
                           nnumericals = "high",
                           cardinality = "high",
                           cachedir = "oml.cache",
                           k = 1,
                           prop = prop,
                           batch_size = 32,
                           verbosity = 0)
  expect_equal(dim(y$trueY), dim(y$hatY))
  expect_equal(dim(y$trueY)[1], ceiling((1-prop)*nobs))
}
)
