#' @title  funBBOBCall
#'
#' @description
#' Call (external) BBOB Function.
#' Call the generator \code{\link[smoof]{makeBBOBFunction}} for the
#' noiseless function set of the real-parameter
#' Black-Box Optimization Benchmarking (BBOB).
#'
#' @param x matrix of points to evaluate with the function.
#'          Rows for points and columns for dimension.
#'
#' @param opt list with the following entries
#'  \describe{
#'   \item{\code{dimensions}}{[integer(1)] Problem dimension. Integer value between 2 and 40.}
#'   \item{\code{fid}}{[integer(1)] Function identifier. Integer value between 1 and 24.}
#'   \item{\code{iid}}{[integer(1)] Instance identifier. Integer value greater than or equal 1.}
#' }
#' @param ... further arguments
#'
#' @importFrom smoof makeBBOBFunction
#'
#' @return 1-column matrix with resulting function values
#'
#' @examples
#' \donttest{
#' ## Call the first instance of the 2D Sphere function
#' require("smoof")
#' require("SPOT")
#' set.seed(123)
#' x <- matrix(c(1,2),1,2)
#' funBBOBCall(x, opt = list(dimensions = 2L, fid = 1L, iid =1L))
#' spot(x=NULL, funBBOBCall,
#'        lower = c(-2,-3), upper = c(1,2),
#'        control=list(funEvals=15),
#'        opt = list(dimensions = 2L, fid = 1L, iid = 1L ))
#'        }
#' @export
#'
funBBOBCall <- function (x,
                         opt = list(),
                         ...){
  con <-list(dimensions = 2,
             fid = 23,
             iid = 1)
  con[names(opt)] <- opt
  opt <- con
  dimensions <- opt$dimensions
  fid <- opt$fid
  iid <- opt$iid
  funBBOB <- makeBBOBFunction(dimensions,
                              fid,
                              iid)
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               funBBOB # function
               ))
}

