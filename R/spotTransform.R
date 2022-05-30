#' @title
#' Helper function: translate levels
#'
#' @description
#' Translate existing levels of a factor into new levels.
#'
#' @param x a factor vector to be translated
#' @param translations a named list that specifies the translation: \code{list(newlevel=c(oldlevel1,oldlevel2,etc))}.
#' @return translated factor
#'
#' @export
translate_levels <- function(x, translations) {
  x <- as.character(x)
  for (i in 1:length(translations)) {
    sel <- x %in% translations[[i]]
    x[sel] <- names(translations[i])
  }
  as.factor(x)
}

#' @title
#' Helper function: transform integer to factor
#'
#' @description
#' This function re-codes a factor with pre-specified factor levels,
#' using an integer encoding as input.
#'
#' @param x an integer vector (that represents factor vector) to be transformed
#' @param lvls the original factor levels used
#'
#' @return the same factor, now coded with the original levels
#'
#' @export
int2fact <- function(x, lvls) {
  nms <- names(lvls)
  for (i in 1:length(lvls)) {
    x[[nms[i]]] <-  lvls[[i]][x[[nms[i]]]]
  }
  x
}

#' @title
#' Identity transformation
#'
#' @description
#' Parameter values can be translated,
#' e.g., to base 10 as implemented in \code{\link{trans_10pow}}.
#' \code{trans_id} implements the identity (transformation), i.e., x is mapped to x.
#'
#' @param x input
#'
#' @return \code{x}
#'
#' @examples
#' f2 <- function(x){2^x}
#' fn <- c("identity", "exp", "f2")
#' xNat <- diag(3)
#' SPOT::transformX(xNat, fn)
#'
#' @export
trans_id <- function(x) {
  x
}


#' @title
#' Mult 2 transformation
#'
#' @description
#' Parameter values can be translated,
#' implements the multiplication (transformation), i.e., x is mapped
#' to round(2x).
#'
#' @param x input
#'
#' @return \code{x}
#'
#' @examples
#' f2 <- function(x){2^x}
#' fn <- c("identity", "exp", "f2")
#' xNat <- diag(3)
#' SPOT::transformX(xNat, fn)
#'
#' @export
trans_mult2_round <- function(x) {
  round(2*x)
}

#' @title odd transformation
#' @description Generate odd numbers, i.e., \code{x -> 2x-1} for x > 0.
#' Return values are rounded using \code{round}.
#'
#' @param x input
#'
#' @return \code{x}
#'
#' @examples
#' f2 <- function(x){2^x}
#' fn <- c("trans_odd_round", "exp", "f2")
#' xNat <- diag(3)
#' SPOT::transformX(xNat, fn)
#'
#' @export
trans_odd_round <- function(x) {
  round(2*x-1)
}

#' @title 2 power x transformation
#' @description
#' Parameter values can be translated,
#' e.g., to base 10 as implemented in \code{\link{trans_10pow}}.
#' \code{trans_2pow} implements the transformation x -> 2^x.
#'
#' @param x input
#' @return \code{2^x}
#'
#' @examples
#' f2 <- function(x){2^x}
#' fn <- c("identity", "exp", "f2")
#' xNat <- diag(3)
#' SPOT::transformX(xNat, fn)
#'
#' @export
trans_2pow <- function(x) {
  2 ^ x
}

#' @title 2 power x transformation with round
#' @description
#' Parameter values can be translated,
#' e.g., to base 10 as implemented in \code{\link{trans_10pow}}.
#' \code{trans_2pow_round} implements the transformation x -> round(2^x).
#'
#' @param x input
#' @return \code{round(2^x)}
#'
#' @examples
#' f2 <- function(x){2^x}
#' fn <- c("identity", "exp", "f2")
#' xNat <- diag(3)
#' SPOT::transformX(xNat, fn)
#'
#' @export
trans_2pow_round <- function(x) {
  round(2 ^ x)
}

#' @title 10 power x transformation
#' @description
#' Parameter values can be translated,
#' e.g., to base 10.
#'
#' @param x input
#' @return \code{10^x}
#'
#' @examples
#' f2 <- function(x){2^x}
#' fn <- c("identity", "exp", "f2")
#' xNat <- diag(3)
#' SPOT::transformX(xNat, fn)
#'
#' @export
trans_10pow <- function(x) {
  10 ^ x
}


#' @title 10 power x transformation
#' @description
#' Parameter values x are transformed to \code{1-10^x}.
#' This is helpful for parameters that are likely to be set very close to (but below) a value of 1,
#' such as discount factors in reinforcement learning.
#'
#' @param x input
#' @return \code{1-10^x}
#'
#' @examples
#' f2 <- function(x){2^x}
#' fn <- c("identity", "exp", "f2")
#' xNat <- diag(3)
#' SPOT::transformX(xNat, fn)
#'
#' @export
trans_1minus10pow <- function(x) {
  1 - (10 ^ x)
}


#' @title 10 power x transformation with round
#'
#' @description
#' Parameter values can be translated,
#' e.g., to base 10 as implemented in \code{\link{trans_10pow}}.
#' \code{trans_10pow_round} implements the transformation x -> round(2^x).
#'
#' @param x input
#' @return \code{round(10^x)}
#'
#' @examples
#' f2 <- function(x){2^x}
#' fn <- c("identity", "exp", "f2")
#' xNat <- diag(3)
#' SPOT::transformX(xNat, fn)
#'
#' @export
trans_10pow_round <- function(x) {
  round(10 ^ x)
}

