% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funKerasGeneric.R
\name{funKerasGeneric}
\alias{funKerasGeneric}
\title{funKerasGeneric}
\usage{
funKerasGeneric(x, kerasConf = NULL, specList = NULL)
}
\arguments{
\item{x}{matrix of hyperparameter values to evaluate with the function.
Rows for points and columns for dimension.}

\item{kerasConf}{List of additional parameters passed to keras as described in \code{\link{getKerasConf}}.
Default: \code{NULL}.}

\item{specList}{prepared data. See \code{\link{genericDataPrep}}.}
}
\value{
1-column matrix with resulting function values (test loss)
}
\description{
Hyperparameter Tuning: Generic Classification Objective Function.
}
\details{
Trains a simple deep NN on arbitrary data sets.
Provides a template that can be used for other networks as well.
Standard Code from \url{https://tensorflow.rstudio.com/}
Modified by T. Bartz-Beielstein (tbb@bartzundbartz.de)

Note: The WARNING "tensorflow:Layers in a Sequential model should only have a single input tensor.
     Consider rewriting this model with the Functional API"
     can be safely ignored:
     in general, Keras encourages its users to use functional models
     for multi-input layers, but there is nothing wrong with doing so.
     See: \url{https://github.com/tensorflow/recommenders/issues/188}.
}
\examples{
\donttest{
### These examples require an activated Python environment as described in
### Bartz-Beielstein, T., Rehbach, F., Sen, A., and Zaefferer, M.:
### Surrogate Model Based Hyperparameter Tuning for Deep Learning with SPOT,
### June 2021. http://arxiv.org/abs/2105.14625.
PYTHON_RETICULATE <- FALSE
if(PYTHON_RETICULATE){

## data preparation
target <- "age"
batch_size <- 32
prop <- 2/3
dfGeneric <- getDataCensus(target = target, nobs = 1000)
data <- getGenericTrainValTestData(dfGeneric = dfGeneric, prop = prop)
specList <- genericDataPrep(data=data, batch_size = batch_size)

## model configuration:
cfg <-  getModelConf(list(model="dl"))
x <- matrix(cfg$default, nrow=1)
transformFun <- cfg$transformations
types <- cfg$type
lower <- cfg$lower
upper <- cfg$upper

kerasConf <- getKerasConf()

### First example: simple function call:
message("objectiveFunctionEvaluation(): x before transformX().")
print(x)
if (length(transformFun) > 0) {  x <- transformX(xNat=x, fn=transformFun)}
message("objectiveFunctionEvaluation(): x after transformX().")
print(x)
funKerasGeneric(x, kerasConf = kerasConf, specList = specList)

### Second example: evaluation of several (three) hyperparameter settings:
xxx <- rbind(x,x,x)
funKerasGeneric(xxx, kerasConf = kerasConf, specList)

### Third example: spot call with extended verbosity:
res <- spot(x = NULL,
            fun = funKerasGeneric,
            lower = lower,
            upper = upper,
            control = list(funEvals=50,
                         handleNAsMethod = handleNAsMean,
                         noise = TRUE,
                         types = types,
                         plots = TRUE,
                         progress = TRUE,
                         seedFun = 1,
                         seedSPOT = 1,
                         transformFun=transformFun),
                         kerasConf = kerasConf,
                         specList = specList)
  }
}

}
\seealso{
\code{\link{getKerasConf}}

\code{\link{evalKerasGeneric}}

\code{\link{evalKerasGeneric}}

\code{\link[keras]{fit}}
}
