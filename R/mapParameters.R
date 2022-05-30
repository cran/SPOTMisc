#' @title Map x parameters to a list of named values
#' @description numerical parameters are mapped to their meanings, e.g.,
#' \code{x[1]} to \code{"dropout rate"}.
#' @param x matrix input values.
#' @param model (char) network type, e.g., \code{"cnn"} or \code{"dl"}. Default: \code{"dl"}
#'
#' @details  For a \code{"dl"} network, the parameter vector \code{x} is mapped
#' to the following \code{FLAGS}:
#' \describe{
#'		\item{\code{x[1]: dropout}}{dropout rate first layer.}
#'		\item{\code{x[2]: dropoutfac}}{dropout factor (multiplier).}
#'		\item{\code{x[3]: units}}{number of units in the first layer.}
#'		\item{\code{x[4]: unitsfact}}{units factor (multiplier).}
#'		\item{\code{x[5]: learning_rate}}{learning rate for optimizer. See, e.g.: \code{link{optimizer_sgd}}}
#'		\item{\code{x[6]: epochs}}{number of training epochs.}
#'		\item{\code{x[7]: beta_1}}{The exponential decay rate for the 1st moment estimates. float, 0 < beta < 1. Generally close to 1.}
#'		\item{\code{x[8]: beta_2}}{The exponential decay rate for the 2nd moment estimates. float, 0 < beta < 1. Generally close to 1.}
#'		\item{\code{x[9]: layers}}{number of layers.}
#'  	\item{\code{x[10]: epsilon}}{float >= 0. Fuzz factor. If NULL, defaults to k_epsilon().}
#'  	\item{\code{x[11]: optimizer}}{integer. Specifies optimizer.}
#' }
#'
#' @returns FLAGS named list (parameter names as specified in \code{\link{getModelConf}}), e.g.,
#'   for "dl": dropout, dropoutfac, units, unitsfact, learning_rate,
#'   epochs, beta_1, beta_2, layers, epsilon, optimizer
#' @examples
#' ## First example: dense neural net
#' x <- getModelConf(list(model="dl"))$defaults
#' mapX2FLAGS(x=x, model = "dl")
#' ## Second example: convnet
#' x <- getModelConf(list(model="cnn"))$defaults
#' mapX2FLAGS(x=x, model = "cnn")
#'
#' @export
mapX2FLAGS <- function(x, model = "dl") {
  if (is.null(x)) {
    FLAGS <- NULL
  } else{
    FLAGS <- as.list(x)
    cfg <- getModelConf(list(model= model))
    names(FLAGS) <- cfg$tunepars
  }
  return(FLAGS)
}


#' @title Print parameter values from FLAG list
#' @description Simple print method for FLAG list.
#' @param FLAGS list of parameters, see \code{\link{mapX2FLAGS}}
#'
#' @export
printFLAGS <- function(FLAGS) {
  if (!is.null(FLAGS)) {
    ## FIXME: write a nicer print method
    print(FLAGS)
  }
}


#'@title Get variable names or subsets of variable names
#'@param model from \code{\link{getModelConf}}, e.g., \code{"dl"}.
#'@param i index for selecting subsets. Default is \code{"all"}.
#'@returns vector of variable names. Returns \code{NA} if wrong indices are selected.
#'
#'@examples
#'# Default is return all:
#'getVarNames(model="dl")
#'getVarNames(model="dl",i=3)
#'getVarNames(model="dl",i=c(1,3,5))
#'# var name does not exits, so return NA
#'getVarNames(model="dl",i=c(100))
#'
#'
#'@export
getVarNames <- function(model,
                        i="all"){
  if (length(i) == 1 && i == "all"){
  getModelConf(list(model = model))$tunepars
    }else{
    (getModelConf(list(model = model))$tunepars)[i]
  }
}

#'@title Get indices (positions) of variable names
#'@param model from \code{\link{getModelConf}}, e.g., \code{"dl"}.
#'@param a name of variables
#'@returns indices of variable names.
#'
#'@examples
#' getIndices(model="dl",
#'            a = c("dropout", "units"))
#'
#'@export
getIndices <- function(model,
                        a){
  if (length(a) == 1 && a == "all"){
    return(1:length(getModelConf(list(model = model))$tunepars))
  }else{
  which(getVarNames(model=model) %in% a)
  }
}

#' @title Active to all
#'
#' @description Recreates the full set of parameters from the subset of active ones.
#'
#' @param x subset of parameters
#' @param a names of the active parameters
#' @param model model (char)
#'
#' @returns y full parameters
#'
#' @examples
#' model <- "dl"
#' # indices of active variables
#' i <- c(1,3)
#' # names of active variables
#' a <- getVarNames(model=model,i=i)
#' x <- getModelConf(model=model)$defaults
#' # now matrix x has only active variables 1 and 3:
#' x <- x[1, getIndices(model=model, a=a), drop=FALSE]
#' # reconstruct the full set of parameters
#' active2All(x, a, model)
#' # 2nd example: new values to x (dropout=0.1, units=11):
#' x <- matrix(c(0.1,11), nrow=1)
#' a <- c("dropout", "units")
#' # reconstruct the full set of parameters
#' active2All(x, a, model)
#' # matrix
#' x <- rbind(x,2*x)
#' active2All(x, a, model)
#'
#'
#' @export
active2All <- function(x, a, model) {
  m <- getModelConf(list(model=model))
  n <- nrow(x)
  x0 <- matrix(rep(m$defaults ,n), nrow = n, byrow = TRUE)
  aInd <-  which(m$tunepars %in% a)
  x0[,aInd] <- x
  return(x0)
}


