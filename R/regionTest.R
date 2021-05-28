#' @title Region Test Data
#'
#' @description A data set of COVID-19 cases. Test data for predictions. 
#' From "2020-04-02" until "2020-05-14".
#' Time difference of 42 days.
#' 
#' @seealso \code{\link{regionTrain}} and \code{\link{regionPopulation}}.
#' @seealso \code{cde20200813} and \code{DEcde20200813}, which are part of the 
#' \code{babsim.data} package, because these are relatively large data sets. 
#'
#' @format A data frame with 13459 rows and 4 columns:
#' \describe{
#'   \item{ForecastId}{int  id. (1--13459)}
#'   \item{Province_State}{Province State. (""--"Zhejiang"). No NAs, but empty char.}
#'   \item{Country_Region}{Country. ("Afghanistan"--"Zimbabwe")}
#'   \item{Date}{chr: yyyy-mm-dd. ("2020-04-02"--"2020-05-14")}
#' }
#' 
#' @source <http://owos.gm.fh-koeln.de:8055/bartz/bartz-data/blob/412e9cf647a6fce875bd49ccddcd5aea8aeb4246/Covid-19/regionTest.csv>
#'
"regionTest"
