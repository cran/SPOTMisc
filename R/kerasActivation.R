#' @title Select keras activation function
#'
#' @param activation integer specifying the activation function. Can be one of the following:
#' \code{1=NULL}, \code{2=RELU}
#'
#' @return activation function use with \code{\link{funKerasMnist}}.
#'
#' @export
#'
selectKerasActivation <- function(activation){
  activationList <- list("LINEAR", "RELU")
  # FIXME: remove after testing:
  message("selectKerasActivation using activation:")
  print(activationList[[activation]])

  switch(activationList[[activation]],
         LINEAR = {NULL},
         RELU = {"relu"},
         stop("selectKerasActivation: Wrong activation.")
  )
}




