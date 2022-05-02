#' @title getMnistData
#'
#' @param kerasConf List of additional parameters passed to keras as described in \code{\link{getKerasConf}}.
#' Default: \code{kerasConf = getKerasConf()}.
#'
#' @seealso \code{\link{getKerasConf}}
#' @seealso \code{\link{funKerasMnist}}
#'
#' @return list with training and test data
#'
#' @importFrom keras dataset_mnist
#' @importFrom keras array_reshape
#' @importFrom keras to_categorical
#' @examples
#' \donttest{
#' ### These examples require an activated Python environment as described in
#' ### Bartz-Beielstein, T., Rehbach, F., Sen, A., and Zaefferer, M.:
#' ### Surrogate Model Based Hyperparameter Tuning for Deep Learning with SPOT,
#' ### June 2021. http://arxiv.org/abs/2105.14625.
#' PYTHON_RETICULATE = FALSE
#' if(PYTHON_RETICULATE){
#'
#' library("SPOTMisc")
#' kerasConf <- getKerasConf()
#' mnist <- getMnistData()
#'}
#'}
#' @export
getMnistData <- function(kerasConf = getKerasConf()) {
  mnist <- dataset_mnist()
  x_train <- mnist$train$x
  y_train <- mnist$train$y
  x_test <- mnist$test$x
  y_test <- mnist$test$y

  x_train <- array_reshape(x_train, c(nrow(x_train), 28 * 28))
  x_test <- array_reshape(x_test, c(nrow(x_test), 28 * 28))

  # Transform RGB values into [0,1] range
  x_train <- x_train / 255
  x_test <- x_test / 255

  # Convert class vectors to binary class matrices
  y_train <- to_categorical(y_train, 10)
  y_test <- to_categorical(y_test, 10)

  res <- list(
    x_train = x_train,
    x_test = x_test,
    y_train = y_train,
    y_test = y_test
  )
}




