#' @title evalKerasMnist
#'
#' @description Hyperparameter Tuning: Keras MNIST Classification Test Function.
#'
#' @details Trains a simple deep NN on the MNIST dataset.
#' Standard Code from https://keras.rstudio.com/
#' Modified by T. Bartz-Beielstein (tbb@bartzundbartz.de)
#'
#' @param x matrix of hyperparameter values to evaluate with the function.
#' Rows for points and columns for dimension.
#' @param kerasConf List of additional parameters passed to keras, e.g., \code{verbose:} Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch),
#' \code{callbacks:}	List of callbacks to be called during training. \code{validation_split:} Float between 0 and 1. Fraction of the training data to be
#' used as validation data. The model will set apart this fraction of the training data,
#' will not train on it, and will evaluate the loss and any model metrics on this data at the end of each epoch.
#' The validation data is selected from the last samples in the x and y data provided, before shuffling. \code{validation_data:}
#' 	Data on which to evaluate the loss and any model metrics at the end of each epoch.
#' The model will not be trained on this data. This could be a list (x_val, y_val) or a list (x_val, y_val, val_sample_weights).
#' validation_data will override validation_split. \code{shuffle:} Logical (whether to shuffle the training data before each epoch) or string (for "batch").
#' "batch" is a special option for dealing with the limitations of HDF5 data;
#' it shuffles in batch-sized chunks. Has no effect when steps_per_epoch is not NULL.
#' @seealso \code{\link{getKerasConf}}
#' @seealso \code{\link{funKerasMnist}}
#' @return list with function values (loss, accuracy, and keras model information)
#'
#' @importFrom SPOT wrapFunction
#' @importFrom keras dataset_mnist
#' @importFrom keras fit
#' @importFrom keras array_reshape
#' @importFrom keras to_categorical
#' @importFrom keras keras_model_sequential
#' @importFrom keras layer_dense
#' @importFrom keras layer_dropout
#' @importFrom keras compile
#' @importFrom keras optimizer_adam
#' @importFrom keras evaluate
#' @importFrom keras model_to_json
#' @importFrom jsonlite fromJSON
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
#' kerasConf$verbose <- 1
#' lower <- c(1e-6, 1e-6, 16,0.6, 1e-9, 10, 6,0.4,0.99,1,1e-8)
#' upper <- c(0.5, 0.5, 512, 1.5, 1e-2, 50, 10,0.999,0.999,10,6e-8)
#' types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
#'            "integer",  "integer",  "numeric",  "numeric",  "integer",
#'            "numeric")
#'
#' x <- matrix(lower, 1,)
#' res <- evalKerasMnist(x, kerasConf)
#' str(res)
#' ### The number of units for all layers can be listed as follows:
#' res$modelConf$config$layers[,2]$units
#'}
#'}
#' @export
#'
#'
evalKerasMnist <- function(x, kerasConf) {
  FLAGS <- list(
    "dropout" =  x[1],
    "dropoutfact" =  x[2],
    "units" = x[3],
    "unitsfact" = x[4],
    "lr" =  x[5],
    "epochs" = x[6],
    "batchsize" = x[7],
    "beta_1" =  x[8],
    "beta_2" =  x[9],
    "layers" =  x[10],
    "epsilon" = x[11]
  )

  # Data Preparation
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

  # Define Model

  model <- keras_model_sequential()
  units <- FLAGS$units
  dropout <- FLAGS$dropout

  model %>%
    layer_dense(units = units,
                activation = 'relu',
                input_shape = c(784)) %>%
    layer_dropout(rate = dropout)

  for (i in 1:FLAGS$layers) {
    dropout <- dropout * FLAGS$dropoutfact
    if (as.integer(units * FLAGS$unitsfact) > 10) {
      units <- as.integer(units * FLAGS$unitsfact)
    }

    model %>% layer_dense(units = units, activation = 'relu')
    if (dropout != 0) {
      model %>% layer_dropout(rate = dropout)
    }

  }

  # Adding the final layer with ten units (classes) and softmax
  model %>% layer_dense(units = 10, activation = 'softmax')

  # decayed_lr = tf.train.exponential_decay(learning_rate,
  #            global_step, 10000,
  #          0.95, staircase=True)

  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_adam(
      # learning rate (default 1e-3)
      lr = FLAGS$lr,
      #  	The exponential decay rate for the 1st moment estimates. float, 0 < beta < 1. Generally close to 1.
      beta_1 = FLAGS$beta_1,
      # The exponential decay rate for the 2nd moment estimates. float, 0 < beta < 1. Generally close to 1.
      beta_2 = FLAGS$beta_2,
      # Fuzz factor. If NULL, defaults to k_epsilon(). (default NULL)
      epsilon = FLAGS$epsilon,
      # Learning rate decay over each update. (default 0)
      decay = 0,
      # Whether to apply the AMSGrad variant of this algorithm from the paper "On the Convergence of Adam and Beyond"
      amsgrad = FALSE,
      # Gradients will be clipped when their L2 norm exceeds this value.
      clipnorm = NULL,
      # Gradients will be clipped when their absolute value exceeds this value.
      clipvalue = NULL
    ),
    metrics = c('accuracy')
  )
  # print(model)

  # Training & Evaluation
  history <- model %>% fit(
    x_train,
    y_train,
    batch_size = 2 ^ FLAGS$batchsize,
    epochs = FLAGS$epochs,
    verbose = kerasConf$verbose,
    validation_split = kerasConf$validation_split,
    shuffle = kerasConf$shuffle
  )

  # cat('val loss:', history$metrics$val_loss , '\n')
  # cat('val accuracy:',  history$metrics$val_acc, '\n')
  # plot(history)

  score <- model %>% evaluate(x_test, y_test,
                              verbose = kerasConf$verbose)
  cat('Test loss:', score[[1]], '\n')
  cat('Test accuracy:', score[[2]], '\n')

  result <- list(testLoss = score[[1]],
                 testAcc = score[[2]],
                 modelConf = fromJSON(model_to_json(model)))
  return(result)
}


#' @title funKerasMnist
#'
#' @description Hyperparameter Tuning: Keras MNIST Classification Test Function.
#'
#' @details Trains a simple deep NN on the MNIST dataset.
#' Standard Code from https://keras.rstudio.com/
#' Modified by T. Bartz-Beielstein (tbb@bartzundbartz.de)
#'
#' @param x matrix of hyperparameter values to evaluate with the function.
#' Rows for points and columns for dimension.
#' @param kConf List of additional parameters passed to keras, e.g., \code{verbose:} Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch),
#' \code{callbacks:}	List of callbacks to be called during training. \code{validation_split:} Float between 0 and 1. Fraction of the training data to be
#' used as validation data. The model will set apart this fraction of the training data,
#' will not train on it, and will evaluate the loss and any model metrics on this data at the end of each epoch.
#' The validation data is selected from the last samples in the x and y data provided, before shuffling. \code{validation_data:}
#' 	Data on which to evaluate the loss and any model metrics at the end of each epoch.
#' The model will not be trained on this data. This could be a list (x_val, y_val) or a list (x_val, y_val, val_sample_weights).
#' validation_data will override validation_split. \code{shuffle:} Logical (whether to shuffle the training data before each epoch) or string (for "batch").
#' "batch" is a special option for dealing with the limitations of HDF5 data;
#' it shuffles in batch-sized chunks. Has no effect when steps_per_epoch is not NULL.
#' @seealso \code{\link{getKerasConf}}
#' @return 1-column matrix with resulting function values (test loss)
#'
#' @importFrom SPOT wrapFunction
#' @importFrom keras dataset_mnist
#' @importFrom keras fit
#' @importFrom keras array_reshape
#' @importFrom keras to_categorical
#' @importFrom keras keras_model_sequential
#' @importFrom keras layer_dense
#' @importFrom keras layer_dropout
#' @importFrom keras compile
#' @importFrom keras optimizer_adam
#' @importFrom keras evaluate
#'
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
#' library("SPOT")
#' kerasConf <- getKerasConf()
#' lower <- c(1e-6, 1e-6, 16,0.6, 1e-9, 10, 6,0.4,0.99,1,1e-8)
#' upper <- c(0.5, 0.5, 512, 1.5, 1e-2, 50, 10,0.999,0.999,10,6e-8)
#' types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
#'            "integer",  "integer",  "numeric",  "numeric",  "integer",
#'            "numeric")
#'
#' ### First example: simple function call:
#' x <- matrix(lower, 1,)
#' funKerasMnist(x, kConf = kerasConf)
#'
#' ### Second example: evaluation of several (three) hyperparameter settings:
#' xxx <- rbind(x,x,x)
#' funKerasMnist(xxx, kConf = kerasConf)
#'
#' ### Third example: spot call with extended verbosity:
#' kerasConf$verbose <- 1
#' res <- spot(x = NULL,
#'             fun = funKerasMnist,
#'             lower = lower,
#'             upper = upper,
#'             control = list(funEvals=50,
#'                          noise = TRUE,
#'                          types = types,
#'                          plots = TRUE,
#'                          progress = TRUE,
#'                          seedFun = 1,
#'                          seedSPOT = 1),
#'                          kConf = kerasConf)
#'   }
#' }
#'
#' @export
#'
#'
funKerasMnist <- function (x, kConf) {
  y <- apply(X = x, # matrix
               MARGIN = 1, # margin (apply over rows)
               evalKerasMnist, # function
               kerasConf = kConf)
  return(matrix(y$testLoss,1,))
}


#' @title Get keras configuration parameter list
#'
#' @description Configuration list for Keras
#'
#' @details Additional parameters passed to keras, e.g., \code{verbose:} Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch),
#' \code{callbacks:}	List of callbacks to be called during training. \code{validation_split:} Float between 0 and 1. Fraction of the training data to be
#' used as validation data. The model will set apart this fraction of the training data,
#' will not train on it, and will evaluate the loss and any model metrics on this data at the end of each epoch.
#' The validation data is selected from the last samples in the x and y data provided, before shuffling. \code{validation_data:}
#' 	Data on which to evaluate the loss and any model metrics at the end of each epoch.
#' The model will not be trained on this data. This could be a list (x_val, y_val) or a list (x_val, y_val, val_sample_weights).
#' validation_data will override validation_split. \code{shuffle:} Logical (whether to shuffle the training data before each epoch) or string (for "batch").
#' "batch" is a special option for dealing with the limitations of HDF5 data;
#' it shuffles in batch-sized chunks. Has no effect when steps_per_epoch is not NULL.
#'
#' @return control list (kerasConf)
#'
#' @export
getKerasConf <- function() {
  kerasConf <- list(
    verbose = 0,
    callbacks = list(),
    validation_split = 0.2,
    validation_data = NULL,
    shuffle = FALSE
  )
  return(kerasConf)
}
