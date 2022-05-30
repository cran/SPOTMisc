context("FunKeras")
skip_on_cran()

test_that("test mnist result", {
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 0
  cfg <-  getModelConf(list(model="dl"))
  x <- matrix(cfg$default, nrow=1)
  transformFun <- cfg$transformations
  types <- cfg$type
  lower <- cfg$lower
  upper <- cfg$upper
  res <- evalKerasMnist(x, kerasConf, data=getMnistData(kerasConf))
  ### we have train, val, test and for each loss and acc = 6 values
  ### plus one valus, which is copied to the first position, all together 7:
  expect_equal(length(res), 7)
})

test_that("test funKerasMnist", {
  set.seed(1)
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 0
  cfg <-  getModelConf(list(model="dl"))
  x <- matrix(cfg$default, nrow=1)
  transformFun <- cfg$transformations
  types <- cfg$type
  lower <- cfg$lower
  upper <- cfg$upper
  res <- funKerasMnist(x, kerasConf = kerasConf, data = getMnistData(kerasConf))
  expect_equal(length(res), 7)
 })


test_that("test funKerasMnistDummy", {
  set.seed(1)
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 0
  kerasConf$resDummy <- TRUE
  cfg <-  getModelConf(list(model="dl"))
  x <- matrix(cfg$default, nrow=1)
  transformFun <- cfg$transformations
  types <- cfg$type
  lower <- cfg$lower
  upper <- cfg$upper
  res <- funKerasMnist(x, kerasConf = kerasConf)
  expect_equal(length(res), 7)
})

test_that("test funKerasTransferLearningDummy", {
  set.seed(1)
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 0
  kerasConf$resDummy <- TRUE
  lower <- c(1e-6, 1e-6, 1, 0.6, 0.99, 1e-9, 1)
  x <- matrix(lower, 1,)
  res <- funKerasTransferLearning(x, kerasConf = kerasConf)
  expect_equal(length(res), 7)
})


test_that("check logging keras", {
  library("SPOT")
  set.seed(1)
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 0
  kerasConf$resDummy <- TRUE
  cfg <-  getModelConf(list(model="dl"))
  x <- matrix(cfg$default, nrow=1)
  transformFun <- cfg$transformations
  types <- cfg$type
  lower <- cfg$lower
  upper <- cfg$upper
  res <- spot(x=NULL,
              funKerasMnist,
              lower = lower,
              upper = upper,
              control=list(funEvals=15,
                           noise = TRUE,
                           # optimizer=optimDE,
                           optimizer = optimLHD,
                           plots = FALSE,
                           progress = TRUE,
                           noise = TRUE,
                           seedSPOT = 1,
                           model = buildRandomForest,
                           verbosity = 0
              ),
              kerasConf=kerasConf
  )
  expect_equal(nrow(res$y), nrow(res$logInfo))
})

