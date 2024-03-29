% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDataCensus.R
\name{getDataCensus}
\alias{getDataCensus}
\title{Get Census KDD data set (+variation)}
\usage{
getDataCensus(
  task.type = "classif",
  nobs = 50000,
  nfactors = "high",
  nnumericals = "high",
  cardinality = "high",
  data.seed = 1,
  cachedir = "oml.cache",
  target = NULL,
  cache.only = FALSE
)
}
\arguments{
\item{task.type}{character, either "classif" or "regr".}

\item{nobs}{integer, number of observations uniformly sampled from the full data set.}

\item{nfactors}{character, controls the number of factors (categorical features) to use.
Can be "low", "med", "high", or "full" (full corresponds to original data set).}

\item{nnumericals}{character, controls the number of numerical features to use.
Can be "low", "med", "high", or "full" (full corresponds to original data set).}

\item{cardinality}{character, controls the number of factor levels (categories)
for the categorical features. Can be "low", "med", "high" (high corresponds to original data set).}

\item{data.seed}{integer, this will be used via set.seed() to make the random subsampling reproducible.
Will not have an effect if all observations are used.}

\item{cachedir}{character. The cache directory, e.g., \code{"oml.cache"}.
Default: \code{"oml.cache"}.}

\item{target}{character "age" or "income_class". If \code{target = age}, the
numerical varible \code{age} is converted to a factor:
\code{age<-as.factor(age<40)}}

\item{cache.only}{logical. Only try to retrieve the object from cache.
Will result in error if the object is not found. Default is TRUE.}
}
\value{
census data set
}
\description{
This function downloads (or loads from cache folder) the Census
KDD Dataset (OpenML ID: 4535).
If requested, data set is changed w.r.t the number of observations, number of
numerical/categorical feature,
the cardinality of the categorical features, and the task type (regr. or classif).
}
\examples{
\donttest{
## Example downloads OpenML data, might take some time:
task.type <- "classif"
nobs <- 1e4 # max: 229285
data.seed <- 1
nfactors <- "full"
nnumericals <- "low"
cardinality <- "med"
censusData <- getDataCensus(
  task.type = task.type,
  nobs = nobs,
  nfactors = nfactors,
  nnumericals = nnumericals,
  cardinality = cardinality,
  data.seed = data.seed,
  cachedir = "oml.cache",
  target="age")
  }

}
