% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funKerasCensus.R
\name{getMlConfig}
\alias{getMlConfig}
\title{get ml config for keras on census}
\usage{
getMlConfig(
  target,
  model,
  data,
  task.type,
  nobs,
  nfactors,
  nnumericals,
  cardinality,
  data.seed,
  prop
)
}
\arguments{
\item{target}{character \code{"age"} or \code{"income_class"}}

\item{model}{character model name, e.g., \code{"dl"}}

\item{data}{data, e.g., from \code{\link{getDataCensus}}}

\item{task.type}{\code{"classif"}  (character)}

\item{nobs}{number of observations (numerical), max \code{229285}. Default: \code{1e4}}

\item{nfactors}{(character), e.g., \code{"high"}}

\item{nnumericals}{(character), e.g., \code{"high"}}

\item{cardinality}{(character), e.g., \code{"high"}}

\item{data.seed}{(numerical) seed}

\item{prop}{(numerical) split proportion (train, vals,test)}
}
\value{
cfg (list)
}
\description{
get ml config for keras on census
}
\examples{
\donttest{
### These examples require an activated Python environment as described in
### Bartz-Beielstein, T., Rehbach, F., Sen, A., and Zaefferer, M.:
### Surrogate Model Based Hyperparameter Tuning for Deep Learning with SPOT,
### June 2021. http://arxiv.org/abs/2105.14625.
PYTHON_RETICULATE <- FALSE
if(PYTHON_RETICULATE){
target <- "age"
task.type <- "classif"
nobs <- 1e2
nfactors <- "high"
nnumericals <- "high"
cardinality <- "high"
data.seed <- 1
cachedir <- "oml.cache"
model <- "ranger"

dfCensus <- getDataCensus(
task.type = task.type,
nobs = nobs,
nfactors = nfactors,
nnumericals = nnumericals,
cardinality = cardinality,
data.seed = data.seed,
cachedir = cachedir,
target = target)

cfg <- getMlConfig(
target = target,
model = model,
data = dfCensus,
task.type = task.type,
nobs = nobs,
nfactors = nfactors,
nnumericals = nnumericals,
cardinality = cardinality,
data.seed = data.seed,
prop= 2/3)
}
}
}
