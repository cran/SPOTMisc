% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funKerasModular.R
\name{getSimpleKerasModel}
\alias{getSimpleKerasModel}
\title{getSimpleKerasModel}
\usage{
getSimpleKerasModel(specList, kerasConf = getKerasConf())
}
\arguments{
\item{specList}{spec}

\item{kerasConf}{keras configuration. Default: return value from \code{\link{getKerasConf}}.}
}
\value{
model. Fitted keras model
}
\description{
build, compile, and train a simple model  (for testing)
}
\examples{
\donttest{
target <- "age"
nobs <- 1000
batch_size <- 32
prop <- 2/3
dfCensus <- getDataCensus(target = target,
nobs = nobs)
data <- getGenericTrainValTestData(dfGeneric = dfCensus,
prop = prop)
specList <- genericDataPrep(data=data, batch_size = batch_size)
kerasConf <- getKerasConf()
simpleModel <- getSimpleKerasModel(specList = specList,
               kerasConf = kerasConf)
}
}