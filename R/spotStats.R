#' @title sum of squared errors
#'
#' @param y actual value
#' @param yhat predicted value
#' @return sum of squared errors
#'
#' @export
SSE <- function (y, yhat)
{
  return((y - yhat)^2)
}

#' @title mean squared errors
#'
#' @param y actual value
#' @param yhat predicted value
#' @return mean squared errors
#'
#' @export
MSE <- function (y, yhat)
{
  return(mean(SSE(y, yhat)))
}

#' @title root mean squared errors
#'
#' @param y actual value
#' @param yhat predicted value
#' @return root mean squared errors
#'
#' @export
RMSE <- function (y, yhat)
{
  return(sqrt(MSE(y, yhat)))
}


#' @title Score results from pred
#'
#' @description errors for \code{(actual, predicted)} values.
#' Based on package \code{Metrics}.
#'
#' @param val list of matrices with true and
#' predicted values, e.g., output from
#' \code{\link{predMlCensus}}
#'
#' @importFrom Metrics accuracy
#' @importFrom Metrics ce
#' @importFrom Metrics f1
#' @importFrom Metrics logLoss
#' @importFrom Metrics mae
#' @importFrom Metrics precision
#' @importFrom Metrics recall
#'
#' @returns matrix with scores
#'
#' @export
#'
scorePredictions <- function(val) {
  k <- dim(val$trueY)[2]
  cnames <- c("accuracy",
              "ce",
              "f1",
              "logLoss",
              "mae",
              "precision",
              "recall",
              "rmse")
  score <- NULL
  n <- length(cnames)
  score <- matrix(NA, nrow = k, ncol = n)
  for (i in 1:k) {
    score[i, 1] <- accuracy(val$trueY[, i], val$hatY[, i])
    score[i, 2] <- ce(val$trueY[, i], val$hatY[, i])
    score[i, 3] <- f1(val$trueY[, i], val$hatY[, i])
    score[i, 4] <- logLoss(val$trueY[, i], val$hatY[, i])
    score[i, 5] <- mae(val$trueY[, i], val$hatY[, i])
    score[i, 6] <- precision(val$trueY[, i], val$hatY[, i])
    score[i, 7] <- recall(val$trueY[, i], val$hatY[, i])
    score[i, 8] <- RMSE(val$trueY[, i], val$hatY[, i])
  }
  colnames(score) <- cnames
  return(score)
}
