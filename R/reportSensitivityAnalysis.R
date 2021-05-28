#' @title Sequential Bifurcation
#'
#' @description \code{sequentialBifurcation} is a wrapper function to
#' \code{\link[sensitivity]{sb}} from the \code{\link{sensitivity}} package.
#'
#' @details The model without interaction is
#'  \code{Y = beta_0 + sum_{i=1}^p beta_i X_i},
#'  while the model with two-factor interactions is
#'  \code{Y = beta_0 + sum_{i=1}^p beta_i X_i + sum_{1 <= i < j <= p} gamma_{ij} X_i X_j}.
#'  In both cases, the factors are assumed to be uniformly distributed on [-1,1].
#'  This is a difference with Bettonvil et al. where the factors vary across [0,1] in the former case, while [-1,1] in the latter.
#'  Another difference with Bettonvil et al. is that in the current implementation, the groups are splitted right in the middle.
#' @param fun function
#' @param lower bound of natural variables. Determines the number of parameters (variables).
#' @param upper bound of natural variables
#' @param k \code{integer}  bifurcations. Must be smaller than the number of parameters.
#' @param interaction \code{logical} TRUE if two-factor interactions should be considered. Default is
#' \code{FALSE}.
#' @param verbosity \code{integer}. If larger than zero, the designs are shown.
#' @param ... optional parameters passed to fun
#'
#' @importFrom sensitivity sb
#' @importFrom sensitivity ask
#' @importFrom sensitivity tell
#' @importFrom SPOT getNatDesignFromCoded
#'
#' @references
#' B. Bettonvil and J. P. C. Kleijnen, 1996,
#' Searching for important factors in simulation models with many factors:
#' sequential bifurcations,
#' European Journal of Operational Research, 96, 180–194.
#'
#' @return sa \code{list} with sensitivity information (effects) for subgroups.
#'
#' @export
sequentialBifurcation <- function(fun,
                                  lower,
                                  upper,
                                  k,
                                  interaction = FALSE,
                                  verbosity = 0,
                                  ...) {
  # number of parameters:
  p <- length(lower)
  if (k > p) k=p
  sa <- sb(p, interaction)
  for (i in 1:k) {
    x <- ask(sa)
    y <- list()
    for (i in names(x)) {
      if (verbosity > 0) {
        print(paste0(i, "th Design:"))
        print(x[[i]])
      }
      u <- matrix(x[[i]], 1, )
      u <- getNatDesignFromCoded(u, a = lower, b = upper)
      y[[i]] <- as.numeric(fun(u, ...))
    }
    tell(sa, y)
  }
  return(sa)
}



#' @title Return effects for each subgroup
#'
#' @description  subgroups: returns the table the effects per groups.
#' Code based on the \code{sbgroups} function written by Gilles Pujol for
#' the function \code{\link[sensitivity]{sb}} in the \code{sensitivity} package.
#'
#' @param x data
#'
#' @return data frame with group names and effects
#'
#' @examples
#' \donttest{
#' require("SPOT")
#' set.seed(2)
#' # Interesting for larger n:
#' n <- 2
#' lower <- c(-0.1, rep(-10,n))
#' upper <- c(0.1, rep(10,n))
#'
#' # Model-based optimization
#' res <- spot(,funSphere,
#'              lower, upper,
#'              control=list(funEvals=30,
#'                           optimizer = optimNLOPTR))
#'
#' # Use the surrogate model for prediction
#' predictFunKriging <- function(x){
#'       predict(object = res$modelFit, x)
#'       }
#'
#' # Determine sensitivity
#' sens <- sequentialBifurcation(predictFunKriging,
#'                               lower, upper,
#'                               k=n+1, interaction = TRUE, verbosity = 0)
#'
#' # Extract group information (variable effects) from sensitivity analysis
#' ps <- subgroups(sens)
#' colors <- RColorBrewer::brewer.pal(12, "Set3")
#' barplot(ps$effect, names.arg=ps$group, col= colors)
#' }
#'
#' @export
#'
subgroups <- function(x) {
  ## sort the data on i
  y <- x$y
  y[rank(x$i)] <- y
  if (! is.null(x$ym)) {
    ym <- x$ym
    ym[rank(x$i)] <- ym
  }
  i <- sort(x$i)

  ## groups
  n <- length(i)
  lower <- i[1 : (n - 1)] + 1
  upper <- i[2 : n]

  ## effects
  if (is.null(x$ym)) {
    effect = diff(y)
  } else {
    effect = (diff(y) - diff(ym)) / 2
  }
  T <- data.frame(lower = lower, upper = upper, effect = effect)
  groupnames <- paste(T[, 1], T[, 2], sep = "-")
  groupnames[T[, 1] == T[, 2]] <- paste(T[T[, 1] == T[, 2], 1])
  return(data.frame(group = groupnames, effect = T[, 3]))
}

