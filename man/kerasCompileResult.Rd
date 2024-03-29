% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kerasHelper.R
\name{kerasCompileResult}
\alias{kerasCompileResult}
\title{Generate result from keras run}
\usage{
kerasCompileResult(y, kerasConf)
}
\arguments{
\item{y}{(1x6)-dim matrix with the following entries: \code{trainingLoss},
\code{negTrainingAccuracy}, \code{validationLoss}, \code{negValidationAccuracy},
\code{testLoss},and \code{negTestAccuracy}.}

\item{kerasConf}{keras configuration generated with \code{\link{getKerasConf}}}
}
\value{
result matrix
}
\description{
Compile a matrix with training, validation, and test results
}
\details{
All values should be minimized: accuracies will be negative.
The (1x7)-dim result matrix has the following entries
\describe{
	\item{\code{returnValue:}}{Metric used for optimization. Default: \code{"validationLoss"}.}
	\item{\code{trainingLoss:}}{training loss.}
	\item{\code{negTrainingAccuracy:}}{negative training accuracy.}
	\item{\code{validationLoss:}}{validation  loss.}
	\item{\code{negValidationAccuracy:}}{negative validation accuracy.}
	\item{\code{testLoss:}}{test loss.}
	\item{\code{negTestAccuracy:}}{negative test accuracy.}
 }
}
\examples{
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
sum(kerasCompileResult(y, kerasConf)) == 4
kerasConf$returnValue <-  "negTestAccuracy"
sum(kerasCompileResult(y, kerasConf)) == 3
kerasConf$returnValue <-   "validationLoss"
sum(kerasCompileResult(y, kerasConf))*2 == 7
kerasConf$returnValue <-   "negValidationAccuracy"
sum(kerasCompileResult(y, kerasConf))*2 == 7
kerasConf$returnValue <-     "trainingLoss"
sum(kerasCompileResult(y, kerasConf))*3 == 10
kerasConf$returnValue <-   "negTrainingAccuracy"
sum(kerasCompileResult(y, kerasConf))*3 == 11

}
\seealso{
\code{\link{evalKerasMnist}}

\code{\link{funKerasMnist}}
}
