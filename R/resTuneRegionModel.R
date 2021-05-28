#' @title Tuned Region Model Data
#' 
#' @description A data set resulting from a `spot` tuning run.
#' 
#' @details 
#' Result of a \code{SPOT::tuneRegionModel} run. 
#' The data (list of 1) was generated as follows:
#' \code{data <- preprocessInputData(regionTrain, regionPopulation); 
#' resTuneRegionModel <- lapply(data[1], tuneRegionModel, pops=NULL, 
#' control=list(funEvals=6, designControl=list(size=5), model = buildLM))}.
#' To accelerate testing, this list is used internally.
#' 
#' @seealso \code{\link{regionTrain}} \code{\link{regionTest}} \code{\link{regionPopulation}}
#'
#' @format A list of 7 entries:
#' \describe{
#'		\item{\code{regionName}}{ e.g., "Afghanistan/": List of 7}
#'		\describe{
#'		\item{\code{xbest}}{Parameters of the best found solution (matrix).}
#'		\item{\code{ybest}}{Objective function value of the best found solution (matrix).}
#'		\item{\code{x}}{Archive of all evaluation parameters (matrix).}
#'		\item{\code{y}}{Archive of the respective objective function values (matrix).}
#'		\item{\code{count}}{Number of performed objective function evaluations.}
#'		\item{\code{msg}}{Message specifying the reason of termination.}
#'		\item{\code{modelFit}}{The fit of the last build model, i.e., an object returned by the last call to the function specified by \code{control$model}.}
#'      }
#'  }
#' 
#' @source <http://owos.gm.fh-koeln.de:8055/bartz/bartz-data/blob/cd4aad9a6640f7c36caa53c898a78568e34374e8/Covid-19/regionTrain.csv>
"resTuneRegionModel"
