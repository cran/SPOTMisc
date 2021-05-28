#' @title Region Train Data
#' 
#' @description  A data set of COVID-19 cases from "2020-01-22" until "2020-05-03".
#' Time difference of 102 days.
#' 
#' @seealso \code{\link{regionTest}} and \code{\link{regionPopulation}}.
#' @seealso \code{cde20200813} and \code{DEcde20200813}, which are part of the 
#' \code{babsim.data} package, because these are relatively large data sets. 
#' 
#' @format A data frame with 32239 rows and 6 columns:
#' \describe{
#'   \item{Id}{id}
#'   \item{Province_State}{Province State ("Alabama"--"Zhejiang"). Also NA}
#'   \item{Country_Region}{Country ("Afghanistan"--"Zimbabwe")}
#'   \item{Date}{chr: yyyy-mm-dd. ("2020-01-22"--"2020-05-03")}
#'   \item{ConfirmedCases}{num}
#'   \item{Fatalities}{num}
#' }
#' @source <http://owos.gm.fh-koeln.de:8055/bartz/bartz-data/blob/cd4aad9a6640f7c36caa53c898a78568e34374e8/Covid-19/regionTrain.csv>
"regionTrain"
