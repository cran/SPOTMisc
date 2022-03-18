context("Keras")

test_that("check keras results are correctly combined into a matrix", {
   x <- 1
   testLoss <-  x
   negTestAccuracy <- 1-x
   validationLoss <- x/2
   negValidationAccuracy <- 1-x/2
   trainingLoss <- x/3
   negTrainingAccuracy <- 1-x/3
   y <- matrix(c(trainingLoss, negTrainingAccuracy,
   validationLoss, negValidationAccuracy,
   testLoss, negTestAccuracy), 1,6)
   kerasConf <- list()
   kerasConf$returnValue <-   "testLoss"
   expect_equal(sum(kerasCompileResult(y, kerasConf)) , 4)
   kerasConf$returnValue <-  "negTestAccuracy"
   expect_equal(sum(kerasCompileResult(y, kerasConf)) ,3)
   kerasConf$returnValue <-   "validationLoss"
   expect_equal(sum(kerasCompileResult(y, kerasConf))*2 ,7)
   kerasConf$returnValue <-   "negValidationAccuracy"
   expect_equal(sum(kerasCompileResult(y, kerasConf))*2,7)
   kerasConf$returnValue <-     "trainingLoss"
   expect_equal(sum(kerasCompileResult(y, kerasConf))*3 ,10)
   kerasConf$returnValue <-   "negTrainingAccuracy"
   expect_equal(sum(kerasCompileResult(y, kerasConf))*3 ,11)
})
