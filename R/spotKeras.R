#' @title spotKEras
#'
#' @description  A wrapper that calls SPOT when optimizing a keras model with data
#'
#' @param x is an optional start point (or set of start points), specified as a matrix.
#' One row for each point, and one column for each optimized parameter.
#' @param fun is the objective function. It should receive a matrix x and return a matrix y.
#' In case the function uses external code and is noisy, an additional seed parameter may be used, see the \code{control$seedFun} argument below for details.
#' Mostly, fun must have format y = f(x, ...). If a noisy function requires some specific seed handling, e.g., in some other non-R code,
#' a seed can be passed to fun. For that purpose, the user must specify \code{control$noise = TRUE} and fun should be \code{fun(x, seed, ...)}
#' @param lower is a vector that defines the lower boundary of search space.
#' This determines also the dimensionality of the problem.
#' @param upper is a vector that defines the upper boundary of search space.
#' @param control is a list with control settings for spot. See \code{\link{spotControl}}.
#' @param kerasConf List of additional parameters passed to keras as described in \code{\link{getKerasConf}}.
#' @param kerasData dataset to use
#' @param ... additional parameters passed to \code{fun}.
#'
#' @return This function returns a result list.
#'
#' @examples
#' \donttest{
#' ### These examples require an activated Python environment as described in
#' ### Bartz-Beielstein, T., Rehbach, F., Sen, A., and Zaefferer, M.:
#' ### Surrogate Model Based Hyperparameter Tuning for Deep Learning with SPOT,
#' ### June 2021. http://arxiv.org/abs/2105.14625.
#' PYTHON_RETICULATE <- FALSE
#' if(PYTHON_RETICULATE){
#'
#' model <- "dl"
#' activeVars <- c("layers", "units", "epochs")
#' kerasConf <- getKerasConf()
#' kerasConf$active <-  activeVars
#' cfg <-  getModelConf("dl", active = activeVars)
#' lower <- cfg$lower
#' upper <- cfg$upper
#' types <- cfg$type
#' result <- spotKeras(x = NULL,
#'                   fun = funKerasMnist,
#'                         lower = lower,
#'                         upper = upper,
#'                         control = list(funEvals = 2,
#'                                        noise = TRUE,
#'                                        types = types,
#'                                        plots = FALSE,
#'                                        progress = TRUE,
#'                                        seedFun = 1,
#'                                        seedSPOT = 1,
#'                                        designControl = list(size = 1)),
#'                          kerasConf = kerasConf,
#'                          kerasData = getMnistData(kerasConf))
#' # The result does contain the active parameters only. To get the full vector, use
#' active2All(x=result$xbest, a=activeVars, model=model)
#' }}
#'
#' @export
spotKeras <-
  function(x = NULL,
           fun,
           lower,
           upper,
           control,
           kerasConf,
           kerasData,
           ...) {
    kerasEvalFun <- function(x, kerasConf, data) {
      message(
        paste(
          "Filling parameters, default parameters are assumed for model type:",
          kerasConf$model
        )
      )
      xFilled <- active2All(x, kerasConf$active, kerasConf$model)
      return(fun(xFilled, kerasConf, data))
    }
    if (is.null(kerasConf$active)) {
      funToUse <- fun
    } else{
      funToUse <- kerasEvalFun
    }

    res <- spot(
      x = x,
      fun = funToUse,
      lower = lower,
      upper = upper,
      control = control,
      kerasConf = kerasConf,
      data = kerasData,
      ...
    )
    message(
      paste(
        "Filling parameters, default parameters are assumed for model type:",
        kerasConf$model
      )
    )
    res$fullConfig <-
      active2All(res$xbest, kerasConf$active, kerasConf$model)
    return(res)
  }
