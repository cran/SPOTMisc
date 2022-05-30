#' @title Get keras configuration parameter list
#'
#' @description Configuration list for \code{keras}'s \code{\link[keras]{fit}} function.
#'
#' @details Additional parameters passed to \code{keras}, e.g.,
#' \describe{
#'   	\item{\code{activation:}}{character. Activation function in the last layer. Default: \code{"sigmoid"}.}
#'    \item{\code{active:}}{vector of active varaibles, e.g., c(1,10) specifies that the first and tenth variable will be considerer by spot.}
#'		\item{\code{callbacks:}}{List of callbacks to be called during training. Default: \code{list()}.}
#'   	\item{\code{clearSession:}}{logical. Whether to call \code{\link[keras]{k_clear_session}} or not at the end of keras modelling. Default: \code{FALSE}.}
#'    \item{\code{encoding:}}{character. Encoding used during data preparation, e.g., by \code{\link{getMnistData}}. Default: \code{"oneHot"}.}
#'  	\item{\code{loss:}}{character. Loss function for compile. Default: \code{"loss_binary_crossentropy"}.}
#'		\item{\code{metrics:}}{character. Metrics function for compile. Default: \code{"binary_accuracy"}.}
#'		\item{\code{model:}}{model specified via \code{\link{getModelConf}}. Default: \code{"dl"}.}
#'		\item{\code{nClasses:}}{Number of classes in (multi-class) classification. Specifies the number of units in the last layer (before softmax).
#'		Default: \code{1} (binary classification).}
#'		\item{\code{resDummy:}}{logical. If \code{TRUE}, generate dummy (mock up) result for testing. If \code{FALSE}, run keras and tf evaluations.
#'		Default: \code{FALSE}.}
#'		\item{\code{returnValue:}}{Return value. Can be one of \code{"trainingLoss"}, \code{"negTrainingAccuracy"},
#'			\code{"validationLoss"}, \code{"negValidationAccuracy"}, \code{"testLoss"}, or \code{"negTestAccuracy"}.}
#'		\item{\code{returnObject:}}{Return object. Can be one of \code{"evaluation"}, \code{"model"},
#'		 			\code{"pred"}.	Default: \code{"evaluation"}.}
#'    \item{\code{shuffle:}}{Logical (whether to shuffle the training data before each epoch) or string (for "batch").
#'    "batch" is a special option for dealing with the limitations of HDF5 data; it shuffles in batch-sized chunks.
#'    Has no effect when steps_per_epoch is not NULL. Default: \code{FALSE}.}
#'    \item{\code{testData:}}{Test Data on which to evaluate the loss and any model metrics at the end of the optimization using evaluate().}
#'    \item{\code{tfDevice:}}{Tensorflow device. CPU/GPU allocation. Passed to \code{tensorflow} via \code{tf$device(kerasConf$tfDevice)}. Default: \code{"/cpu:0"} (use CPU only).}
#'    \item{\code{trainData:}}{Train Data on which to evaluate the loss and any model metrics at the end of each epoch.}
#'    \item{\code{validationData:}}{Validation Data on which to evaluate the loss and any model metrics at the end of each epoch.}
#'    \item{\code{validation_data (deprecated, see validationData):}}{Data on which to evaluate the loss and any model metrics at the end of each epoch.
#'     The model will not be trained on this data. This could be a list (x_val, y_val) or a list (x_val, y_val, val_sample_weights).
#'      validation_data will override validation_split. Default: \code{NULL}.}
#'		\item{\code{validation_split:}}{Float between 0 and 1. Fraction of the training data to be
#'          used as validation data. The model will set apart this fraction of the training data,
#'          will not train on it, and will evaluate the loss and any model metrics on this data at the end of each epoch.
#'          The validation data is selected from the last samples in the x and y data provided, before shuffling. Default: \code{0.2}.}
#'    \item{\code{verbose:}}{Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch). Default: \code{0}.}
#'    }
#'
#' @return kerasConf \code{list} with configuration parameters.
#'
#' @seealso \code{\link{evalKerasMnist}}
#' @seealso \code{\link{funKerasMnist}}
#' @seealso \code{\link[keras]{fit}}
#'
#' @export
getKerasConf <- function() {
  kerasConf <- list(
    active = NULL,
    activation = "sigmoid",
    callbacks = list(),
    clearSession = FALSE,
    encoding = "oneHot",
    loss = "loss_binary_crossentropy",
    metrics = "binary_accuracy",
    model = "dl",
    nClasses = 1,
    resDummy = FALSE,
    returnValue = "validationLoss",
    returnObject = "evaluation",
    shuffle = FALSE,
    testData = NULL,
    tfDevice = "/cpu:0",
    trainData = NULL,
    validationData = NULL,
    validation_data = NULL,
    validation_split = 0.2,
    verbose = 0
  )
  return(kerasConf)
}
