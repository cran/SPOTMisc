% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funKerasTransferLearning.R
\name{funKerasTransferLearning}
\alias{funKerasTransferLearning}
\title{funKerasTransferLearning}
\usage{
funKerasTransferLearning(x, kerasConf = getKerasConf(), data = NULL)
}
\arguments{
\item{x}{matrix of hyperparameter values to evaluate with the function.
Rows for points and columns for dimension.}

\item{kerasConf}{List of additional parameters passed to keras as described in \code{\link{getKerasConf}}.
Default: \code{kerasConf = getKerasConf()}.}

\item{data}{data}
}
\value{
1-column matrix with resulting function values (test loss).
}
\description{
Hyperparameter Tuning: Keras TransfewrLearning Test Function.
}
\details{
Trains a simple deep NN on the MNIST dataset.
Provides a template that can be used for other networks as well.
Standard Code from https://tensorflow.rstudio.com/
Modified by T. Bartz-Beielstein (tbb@bartzundbartz.de)
}
\examples{
\donttest{
### These examples require an activated Python environment as described in
### Bartz-Beielstein, T., Rehbach, F., Sen, A., and Zaefferer, M.:
### Surrogate Model Based Hyperparameter Tuning for Deep Learning with SPOT,
### June 2021. http://arxiv.org/abs/2105.14625.

PYTHON_RETICULATE <- FALSE
if(PYTHON_RETICULATE){
library("SPOTMisc")
library("SPOT")
kerasConf <- getKerasConf()

 # Hyperparameters:
 # "dropout" =  x[1],
 # "learning_rate" =  x[2],
 # "epochs" = x[3],
 # "beta_1" =  x[4],
 # "beta_2" =  x[5],
 # "epsilon" = x[6],
 # "optimizer" = x[7]

lower <- c(1e-6, 1e-6, 2, 0.8, 0.8, 1e-9, 1)
upper <- c(0.2, 1e-2, 5, 0.99, 0.9999, 1e-3, 2)
types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
           "integer",  "factor")

## First Example: spot call with extended verbosity. Default objective
## "validationLoss", i.e., validation loss, is used. only 20 function
## evaluations (for testing).
kerasConf$verbose <- 1
res <- spot(x = NULL,
            fun = funKerasTransferLearning,
            lower = lower,
            upper = upper,
            control = list(funEvals=20,
                           model=buildKriging,
                           noise = TRUE,
                           types = types,
                           optimizer=optimDE,
                           plots = TRUE,
                           progress = TRUE,
                           seedFun = 1,
                           seedSPOT = 1,
                           kerasConf = kerasConf)
                           )
 save(res, file = paste0("resKerasTransferLearning", as.numeric(Sys.time()),".RData"))


 ## Example: resKerasTransferLearning04
 ## Default objective function "validationLoss", i.e.,
 ## training loss
library("SPOTMisc")
library("SPOT")
kerasConf <- getKerasConf()

# Hyperparameters:
# "dropout" =  x[1],
# "learning_rate" =  x[2],
# "epochs" = x[3],
# "beta_1" =  x[4],
# "beta_2" =  x[5],
# "epsilon" = x[6],
# "optimizer" = x[7]

lower <- c(1e-6, 1e-6, 2, 0.8, 0.8, 1e-9, 1)
upper <- c(0.2, 1e-2, 5, 0.99, 0.9999, 1e-3, 2)
types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
           "integer",  "factor")

res <- spot(x = NULL,
            fun = funKerasTransferLearning,
            lower = lower,
            upper = upper,
            control = list(funEvals=100,
                           model=buildKriging,
                           noise = TRUE,
                           types = types,
                           optimizer=optimDE,
                           plots = FALSE,
                           progress = TRUE,
                           seedFun = 1,
                           seedSPOT = 1,
                           kerasConf = kerasConf))
save(res,file = paste0("resKerasTransferLearningValidationLoss04",
as.numeric(Sys.time()),".RData"))



 ## Example: resKerasTransferLearning05
 ## objective function "negValidationAccuracy", i.e.,
 ## negative validation accuracy
library("SPOTMisc")
library("SPOT")
kerasConf <- getKerasConf()

# Hyperparameters:
# "dropout" =  x[1],
# "learning_rate" =  x[2],
# "epochs" = x[3],
# "beta_1" =  x[4],
# "beta_2" =  x[5],
# "epsilon" = x[6],
# "optimizer" = x[7]

lower <- c(1e-6, 1e-6, 2, 0.8, 0.8, 1e-9, 1)
upper <- c(0.2, 1e-2, 5, 0.99, 0.9999, 1e-3, 2)
types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
           "integer",  "factor")

kerasConf$returnValue <- "negValidationAccuracy"
res <- spot(x = NULL,
            fun = funKerasTransferLearning,
            lower = lower,
            upper = upper,
            control = list(funEvals=100,
                           model=buildKriging,
                           noise = TRUE,
                           types = types,
                           optimizer=optimDE,
                           plots = FALSE,
                           progress = TRUE,
                           seedFun = 1,
                           seedSPOT = 1,
                           kerasConf = kerasConf))
save(res,file = paste0("resKerasTransferLearningNegValidationAccuracy05",
as.numeric(Sys.time()),".RData"))


 ## Example: resKerasTransferLearning06
 ## objective function "trainingLoss", i.e.,
 ## training loss

library("SPOTMisc")
library("SPOT")
kerasConf <- getKerasConf()

# Hyperparameters:
# "dropout" =  x[1],
# "learning_rate" =  x[2],
# "epochs" = x[3],
# "beta_1" =  x[4],
# "beta_2" =  x[5],
# "epsilon" = x[6],
# "optimizer" = x[7]

lower <- c(1e-6, 1e-6, 2, 0.8, 0.8, 1e-9, 1)
upper <- c(0.2, 1e-2, 5, 0.99, 0.9999, 1e-3, 2)
types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
           "integer",  "factor")

kerasConf$returnValue <- "trainingLoss"
res <- spot(x = NULL,
            fun = funKerasTransferLearning,
            lower = lower,
            upper = upper,
            control = list(funEvals=100,
                           model=buildKriging,
                           noise = TRUE,
                           types = types,
                           optimizer=optimDE,
                           plots = FALSE,
                           progress = TRUE,
                           seedFun = 1,
                           seedSPOT = 1,
                           kerasConf = kerasConf)
)
save(res, file = paste0("resKerasTransferLearningTrainingLoss06",
as.numeric(Sys.time()),".RData"))
 }
}

}
\seealso{
\code{\link{getKerasConf}}

\code{\link{evalKerasTransferLearning}}

\code{\link{evalKerasMnist}}

\code{\link[keras]{fit}}
}
