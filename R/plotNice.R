
#' Plot a nice rpart tree model
#'
#' Plot model produced by \code{\link[SPOT]{buildTreeModel}}.
#'
#' @param x tree model (settings and parameters) of class \code{spotTreeModel}.
#' @param ... parameters passed to rpart.plot plotting function
#' @importFrom rpart.plot rpart.plot
#' @keywords internal
#'
#' @return The returned value is identical to that of \code{\link[rpart.plot]{prp}}.
#'
#' @export
plotnice.spotTreeModel <- function(x,...){
  rpart.plot(x$fit)
}
