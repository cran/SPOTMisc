#' @title Generate result from keras run
#' @description Compile a matrix with training, validation, and test results
#' @param kerasConf keras configuration generated with \code{\link{getKerasConf}}
#' @param y (1x6)-dim matrix with the following entries: \code{trainingLoss},
#'  \code{negTrainingAccuracy}, \code{validationLoss}, \code{negValidationAccuracy},
#'  \code{testLoss},and \code{negTestAccuracy}.
#' @return result matrix
#'
#' @details All values should be minimized: accuracies will be negative.
#' The (1x7)-dim result matrix has the following entries
#' \describe{
#'		\item{\code{returnValue:}}{Metric used for optimization. Default: \code{"validationLoss"}.}
#'		\item{\code{trainingLoss:}}{training loss.}
#'		\item{\code{negTrainingAccuracy:}}{negative training accuracy.}
#'		\item{\code{validationLoss:}}{validation  loss.}
#'		\item{\code{negValidationAccuracy:}}{negative validation accuracy.}
#'		\item{\code{testLoss:}}{test loss.}
#'		\item{\code{negTestAccuracy:}}{negative test accuracy.}
#'  }
#'
#' @seealso \code{\link{evalKerasMnist}}
#' @seealso \code{\link{funKerasMnist}}
#' @examples
#' x <- 1
#' testLoss <-  x
#' negTestAccuracy <- 1-x
#' validationLoss <- x/2
#' negValidationAccuracy <- 1-x/2
#' trainingLoss <- x/3
#' negTrainingAccuracy <- 1-x/3
#' y <- matrix(c(trainingLoss, negTrainingAccuracy,
#' validationLoss, negValidationAccuracy,
#' testLoss, negTestAccuracy), 1,6)
#' kerasConf <- list()
#' kerasConf$returnValue <-   "testLoss"
#' sum(kerasCompileResult(y, kerasConf)) == 4
#' kerasConf$returnValue <-  "negTestAccuracy"
#' sum(kerasCompileResult(y, kerasConf)) == 3
#' kerasConf$returnValue <-   "validationLoss"
#' sum(kerasCompileResult(y, kerasConf))*2 == 7
#' kerasConf$returnValue <-   "negValidationAccuracy"
#' sum(kerasCompileResult(y, kerasConf))*2 == 7
#' kerasConf$returnValue <-     "trainingLoss"
#' sum(kerasCompileResult(y, kerasConf))*3 == 10
#' kerasConf$returnValue <-   "negTrainingAccuracy"
#' sum(kerasCompileResult(y, kerasConf))*3 == 11
#'
#' @export
kerasCompileResult <- function(y, kerasConf) {
  colNames <-
    c(
      "trainingLoss",
      "negTrainingAccuracy",
      "validationLoss",
      "negValidationAccuracy",
      "testLoss",
      "negTestAccuracy"
    )
  switch(
    kerasConf$returnValue,
    trainingLoss = {
      y <- cbind(as.matrix(y[1, 1], 1, 1), y)
      colnames(y) <- c("trainingLoss", colNames)
    },
    negTrainingAccuracy = {
      y <- cbind(as.matrix(y[1, 2], 1, 1), y)
      colnames(y) <- c("negTrainingAccuracy", colNames)
    },
    validationLoss = {
      y <- cbind(as.matrix(y[1, 3], 1, 1), y)
      colnames(y) <- c("validationLoss", colNames)
    },
    negValidationAccuracy = {
      y <- cbind(as.matrix(y[1, 4], 1, 1), y)
      colnames(y) <- c("negValidationAccuracy", colNames)
    },
    testLoss = {
      y <- cbind(as.matrix(y[1, 5], 1, 1), y)
      colnames(y) <- c("testLoss", colNames)
    },
    negTestAccuracy = {
      y <- cbind(as.matrix(y[1, 6], 1, 1), y)
      colnames(y) <- c("negTestAccuracy", colNames)
    },{
      print(kerasConf$returnValue)
      #printf("kerasConf$returnValue: %s", kerasConf$returnValue)
      stop("Wrong return value from funKerasMnist()!")
    }
  )
  return(y)
}

#' @title formatted output
#'
#' @description Combine \code{\link{sprintf}} and \code{\link{writeLines}} to
#' generate formatted output
#'
#' @param ... output to be printed
#'
#' @examples
#'
#' x <- 123
#' printf("x value: %d", x)
#'
#' @export
printf <- function(...){
  writeLines(sprintf(...))
}



#' @title generate Cats Dogs Data
#'
#' @description Generate data for \code{\link{funKerasTransferLearning}}
#'
#' @param kerasConf keras configuration. Default: \code{kerasConf = \link{getKerasConf}}
#'
#' @details
#' Standard Data from https://tensorflow.rstudio.com/
#' Modified by T. Bartz-Beielstein (tbb@bartzundbartz.de)
#'
#' @return list with test, validation, and test data
#'
#' @importFrom magrittr %<>%
#' @importFrom reticulate import
#' @importFrom reticulate py_install
#' @importFrom tfdatasets dataset_map
#' @importFrom keras dataset_mnist
#' @importFrom keras fit
#' @importFrom keras array_reshape
#' @importFrom keras to_categorical
#' @importFrom keras keras_model_sequential
#' @importFrom keras layer_dense
#' @importFrom keras layer_dropout
#' @importFrom keras compile
#' @importFrom keras optimizer_adam
#' @importFrom keras evaluate
#' @importFrom keras %<-%
#' @importFrom tensorflow tf
#'
#' @export
genCatsDogsData <- function(kerasConf=getKerasConf()) {
  trainData <- validationData <- testData <- NULL
  ## Moved to preparations: pip install tensorflow-datasets,
  ## see spotMiscVignette
  # py_install("tensorflow_datasets", pip = TRUE)

  tfds <- import("tensorflow_datasets")
  c(trainData, validationData, testData) %<-% tfds$load(
    "cats_vs_dogs",
    # Reserve 10% for validation and 10% for test
    split = c("train[:40%]", "train[40%:50%]", "train[50%:60%]"),
    as_supervised = TRUE  # Include labels
  )
  if(kerasConf$verbose > 0){
  printf("Number of training samples: %d", length(trainData))
  printf("Number of validation samples: %d", length(validationData))
  printf("Number of test samples: %d", length(testData))
  }
  ### Standardizing the data
  ## library(magrittr, include.only = "%<>%")
  size <- as.integer(c(150, 150))
  trainData      %<>% dataset_map(function(x, y)
    list(tf$image$resize(x, size), y))
  validationData %<>% dataset_map(function(x, y)
    list(tf$image$resize(x, size), y))
  testData       %<>% dataset_map(function(x, y)
    list(tf$image$resize(x, size), y))
  data <-
    list(trainData = trainData,
         validationData = validationData,
         testData = testData)
  return(data)
}

