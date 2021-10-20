#' @title spotPlot
#'
#' @description
#' A wrapper function for available plotting options in SPOT and SPOTMisc.
#' Plotting functions from SPOT -> plotdata, plotModel, plotFunction.
#' Plotting functions from SPOTMisc -> plot_parallel, plot_sensitivity.
#'
#' spotPlot provides a higher level of abstraction and the users can use every plotting
#' function only by calling spotPlot.
#'
#' @param plotType function type to be called. It should be given as either
#' "data", "model", "fun", "parallel" or "sensitivity".
#' Otherwise the function returns an error message.
#'
#' @param ... additional parameters passed to \code{plotData} or \code{plotModel},
#' \code{plotFunction}, \code{plot_parallel} or \code{plot_sensitivity}.
#'
#' @examples
#' library("SPOT")
#' set.seed(1)                              # seed
#' k <- 30                                  # sample number
#' x <- matrix( cbind(runif(k)*10, runif(k)*10), k, 2)    # create data
#' y <- funSphere(x)      # generate random test data
#' fit <- buildLM(x,y)                      # create a model
#' result <- spot(x=NULL, funSphere, c(-5, -5), c(5, 5))
#'
#' spotPlot(plotType="data", x, y, type="filled.contour")
#' spotPlot(plotType="model", object=fit, type="contour")
#' spotPlot(plotType="fun", f=function(x){rowSums(x^2)},
#'    lower=c(-10,0), upper=c(15,10), type="filled.contour")
#' spotPlot(plotType = "parallel", object=fit)
#' spotPlot(plotType = "sensitivity", object=result)
#'
#' @seealso \code{\link[SPOT]{plotData}}
#' @seealso \code{\link[SPOT]{plotModel}}
#' @seealso \code{\link[SPOT]{plotFunction}}
#'
#' @importFrom SPOT plotData
#' @importFrom SPOT plotModel
#' @importFrom SPOT plotFunction
#'
#'
#' @author Alpar Gür \email{alpar.guer@@smail.th-koeln.de}
#'
#' @export
spotPlot <- function(plotType, ...){
  switch(plotType,
         data = plotData(...),
         model = plotModel(...),
         fun = plotFunction(...),
         parallel = plot_parallel(...),
         sensitivity = plot_sensitivity(...),
         stop("Unknown plotType. Select one of the following options:
         \n\t data
         \n\t model
         \n\t fun
         \n\t parallel
         \n\t sensitivity"))
}


