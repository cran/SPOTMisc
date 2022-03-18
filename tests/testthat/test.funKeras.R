context("FunKeras")
skip_on_cran()

test_that("test mnist result", {
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 1
  lower <- c(1e-6, 1e-6, 16,0.6, 1e-9, 2, 6,0.4,0.99,1,1e-8)
  upper <- c(0.5, 0.5, 512, 1.5, 1e-2, 50, 10,0.999,0.999,10,6e-8)
  types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
             "integer",  "integer",  "numeric",  "numeric",  "integer",
             "numeric")

  x <- matrix(lower, 1,)
  res <- evalKerasMnist(x, kerasConf)
  ### we have train, val, test and for each loss and acc = 6 values
  ### plus one valus, which is copied to the first position, all together 7:
  expect_equal(length(res), 7)
})

test_that("test funKerasMnist", {
  set.seed(1)
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 1
  lower <- c(1e-6, 1e-6, 16,0.6, 1e-9, 2, 6,0.4,0.99,1,1e-8)
  x <- matrix(lower, 1,)
  res <- funKerasMnist(x, kConf = kerasConf)
  expect_equal(length(res), 7)
 })

test_that("test funKerasMnist", {
  set.seed(1)
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 1
  lower <- c(1e-6, 1e-6, 16,0.6, 1e-9, 2, 6,0.4,0.99,1,1e-8)
  x <- matrix(lower, 1,)
  res <- funKerasMnist(x, kConf = kerasConf)
  expect_equal(length(res), 7)
})

test_that("test funKerasMnistDummy", {
  set.seed(1)
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 1
  kerasConf$resDummy <- TRUE
  lower <- c(1e-6, 1e-6, 16,0.6, 1e-9, 2, 6,0.4,0.99,1,1e-8)
  x <- matrix(lower, 1,)
  res <- funKerasMnist(x, kConf = kerasConf)
  expect_equal(length(res), 7)
})

test_that("test funKerasTransferLearningDummy", {
  set.seed(1)
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 1
  kerasConf$resDummy <- TRUE
  lower <- c(1e-6, 1e-6, 1, 0.6, 0.99, 1e-9, 1)
  x <- matrix(lower, 1,)
  res <- funKerasTransferLearning(x, kConf = kerasConf)
  expect_equal(length(res), 7)
})


test_that("check logging keras", {
  library("SPOT")
  set.seed(1)
  kerasConf <- getKerasConf()
  kerasConf$verbose <- 1
  kerasConf$resDummy <- TRUE
  lower <- c(1e-6, 1e-6, 1, 0.6, 0.99, 1e-9, 1)
  res <- spot(x=NULL,
              funKerasMnist,
              lower <- c(1e-6, 1e-6, 1, 0.6, 0.99, 1e-9, 1),
              upper <- c(1e-3, 1e-3, 2, 0.7, 0.999, 1e-6, 2),
              control=list(funEvals=15,
                           noise = TRUE,
                           # optimizer=optimDE,
                           optimizer = optimLHD,
                           plots = FALSE,
                           progress = TRUE,
                           seedFun = 1, # mandatory for multi-dim responses
                           noise = TRUE,
                           seedSPOT = 1,
                           model = buildRandomForest,
                           verbosity = 1
              ),
              kConf=kerasConf
  )
  expect_equal(nrow(res$y), nrow(res$logInfo))
})

