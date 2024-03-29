% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funKerasGeneric.R
\name{selectTarget}
\alias{selectTarget}
\title{Select target variable in a data frame}
\usage{
selectTarget(df, target)
}
\arguments{
\item{df}{data frame}

\item{target}{character specification of the target variable}
}
\value{
df with entry target
}
\description{
Select target variable in a data frame
}
\examples{
df <- data.frame(cbind(x=1:2,
                 y=3:4))
df <- selectTarget(df=df, target="y")

}
