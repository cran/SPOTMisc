#' @title evalKerasTransferLearning
#'
#' @description Hyperparameter Tuning: Keras TransferLearning Test Function.
#'
#' @details Trains a transfer learning model.
#' Standard Code from https://keras.rstudio.com/
#' Modified by T. Bartz-Beielstein (tbb@bartzundbartz.de)
#'
#' @param x matrix of hyperparameter values to evaluate with the function.
#' Rows for points and columns for dimension.
#' \code{"dropout" =  x[1]},
#' \code{"learning_rate" =  x[2]},
#' \code{"epochs" = x[3]},
#' \code{"beta_1" =  x[4]},
#' \code{"beta_2" =  x[5]},
#' \code{"epsilon" = x[6]}, and
#' \code{"optimizer" = x[7]} (type: factor).
#' @param kerasConf List of additional parameters passed to keras as described in \code{\link{getKerasConf}}.
#' Default: \code{kerasConf = getKerasConf()}.
#' @param data data
#' @seealso \code{\link{getKerasConf}}
#' @seealso \code{\link{funKerasTransferLearning}}
#' @seealso \code{\link{funKerasMnist}}
#' @seealso \code{\link[keras]{fit}}
#'
#' @return list with function values (training, validation, and test loss/accuracy,
#' and keras model information)
#'
#' @importFrom SPOT wrapFunction
#' @importFrom keras evaluate
#' @importFrom keras fit
#' @importFrom keras array_reshape
#' @importFrom keras to_categorical
#' @importFrom keras keras_model
#' @importFrom keras keras_model_sequential
#' @importFrom keras k_clear_session
#' @importFrom keras layer_dense
#' @importFrom keras layer_input
#' @importFrom keras layer_dropout
#' @importFrom keras layer_rescaling
#' @importFrom keras layer_global_average_pooling_2d
#' @importFrom keras layer_random_flip
#' @importFrom keras layer_random_rotation
#' @importFrom keras loss_binary_crossentropy
#' @importFrom keras metric_binary_accuracy
#' @importFrom keras compile
#' @importFrom keras optimizer_adam
#' @importFrom keras optimizer_adamax
#' @importFrom keras model_to_json
#' @importFrom keras application_xception
#' @importFrom tfdatasets dataset_batch
#' @importFrom tfdatasets dataset_cache
#' @importFrom tfdatasets dataset_prefetch
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
#' lower <- c(1e-6, 1e-6, 1, 0.6, 0.99, 1e-9, 1)
#' x <- matrix(lower, 1,)
#' res <- evalKerasTransferLearning(x,
#'                                  kerasConf = getKerasConf()
#'                                  )
#' str(res)
#' ### The number of units for all layers can be listed as follows:
#' res$modelConf$config$layers[,2]$units
#'}
#'}
#' @export
#'
#'
evalKerasTransferLearning <- function(x,
                                      kerasConf = getKerasConf(),
                                      data = NULL) {
  FLAGS <- list(
    "dropout" =  x[1],
    "learning_rate" =  x[2],
    "epochs" = x[3],
    "beta_1" =  x[4],
    "beta_2" =  x[5],
    "epsilon" = x[6],
    "optimizer" = x[7]
  )
  if (kerasConf$resDummy)
  {
    y <- matrix(
      runif(6, min = FLAGS$dropout, max = 1 + FLAGS$dropout),
      nrow = 1,
      ncol = 6
    )
    y <- kerasCompileResult(y = y, kerasConf = kerasConf)
    message("evalKerasTransferLearning(): Returning dummy value for testing.")
    return(y)
  } else{
    d <- genCatsDogsData()
    trainData <- d$trainData
    validationData <- d$validationData
    testData <- d$testData
    ##
    dataset_cache_batch_prefetch <-
      function(dataset,
               batch_size = 32,
               buffer_size = 10) {
        dataset %>%
          dataset_cache() %>%
          dataset_batch(batch_size) %>%
          dataset_prefetch(buffer_size)
      }
    trainData      %<>% dataset_cache_batch_prefetch()
    validationData %<>% dataset_cache_batch_prefetch()
    testData       %<>% dataset_cache_batch_prefetch()

    ##
    with(tf$device("/cpu:0"), {
    data_augmentation <- keras_model_sequential() %>%
      layer_random_flip("horizontal") %>%
      layer_random_rotation(.1)

    ## select optimizer
    if (FLAGS$optimizer == 1) {
      optimizer <- optimizer_adam(
        # learning rate (default 1e-3)
        learning_rate = FLAGS$learning_rate,
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
        #TRUE,
        # Gradients will be clipped when their L2 norm exceeds this value.
        clipnorm = NULL,
        # Gradients will be clipped when their absolute value exceeds this value.
        clipvalue = NULL
      )
    } else if (FLAGS$optimizer == 2) {
      optimizer <- optimizer_adamax(
        # learning rate (default 1e-3)
        learning_rate = FLAGS$learning_rate,
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
        #TRUE,
        # Gradients will be clipped when their L2 norm exceeds this value.
        clipnorm = NULL,
        # Gradients will be clipped when their absolute value exceeds this value.
        clipvalue = NULL
      )
    } else {
      stop("Wrong optimizer.")
    }

    # Define Model
    base_model = application_xception(
      weights = "imagenet",
      # Load weights pre-trained on ImageNet.
      input_shape = c(150, 150, 3),
      include_top = FALSE # Do not include the ImageNet classifier at the top.
    )
    # Freeze the base_model
    base_model$trainable <- FALSE
    # Create new model on top
    inputs = layer_input(shape = c(150, 150, 3))
    outputs <- inputs %>%
      data_augmentation() %>%   # Apply random data augmentation
      # Pre-trained Xception weights requires that input be scaled
      # from (0, 255) to a range of (-1., +1.), the rescaling layer
      # outputs: `(inputs * scale) + offset`
      layer_rescaling(scale = 1 / 127.5, offset = -1) %>%
      # The base model contains batchnorm layers. We want to keep them in inference mode
      # when we unfreeze the base model for fine-tuning, so we make sure that the
      # base_model is running in inference mode here.
      base_model(training = FALSE) %>%
      layer_global_average_pooling_2d() %>%
      layer_dropout(FLAGS$dropout) %>%
      layer_dense(1)
    model <- keras_model(inputs, outputs)

    ## Train the top layer
    model %>% compile(
      optimizer = optimizer,
      loss = loss_binary_crossentropy(from_logits = TRUE),
      metrics = metric_binary_accuracy()
    )
    epochs <- FLAGS$epochs * 2 ## default: 20
    model %>% fit(x = trainData,
                  epochs = epochs,
                  validation_data = validationData)
    ## Do a round of fine-tuning of the entire model
    # Unfreeze the base_model. Note that it keeps running in inference mode
    # since we passed `training = FALSE` when calling it. This means that
    # the batchnorm layers will not update their batch statistics.
    # This prevents the batchnorm layers from undoing all the training
    # we've done so far.
    base_model$trainable <- TRUE

    model %>% compile(
      optimizer = optimizer,
      loss = loss_binary_crossentropy(from_logits = TRUE),
      metrics = metric_binary_accuracy()
    )
    epochs <- FLAGS$epochs ## default: 10
    history <-
      model %>% fit(x = trainData,
                    epochs = epochs,
                    validation_data = validationData)

    # evaluate on test data
    score <- model %>% evaluate(x = testData,
                                y = NULL,
                                verbose = kerasConf$verbose)
    }) ## end with
    ## matrix with six entries:

    # trainingLoss,  negTrainingAccuracy,
    # validationLoss,  negValidationAccuracy,
    # testLoss,  negTestAccuracy.
    ## Accuracies must be negative for minimization!
    y <- matrix(
      c(
        history$metrics$loss[length(history$metrics$loss)],-history$metrics$binary_accuracy[length(history$metrics$binary_accuracy)],
        history$metrics$val_loss[length(history$metrics$val_loss)],-history$metrics$val_binary_accuracy[length(history$metrics$val_binary_accuracy)],
        score[[1]],-score[[2]]
      ),
      nrow = 1,
      ncol = 6
    )

    y <- kerasCompileResult(y = y, kerasConf = kerasConf)

    if (kerasConf$verbose) {
      cat('y:', y , '\n')
    }
    if (kerasConf$clearSession) {
      keras::k_clear_session()
    }
    return(y)
  }
}

#' @title funKerasTransferLearning
#'
#' @description Hyperparameter Tuning: Keras TransfewrLearning Test Function.
#'
#' @details Trains a simple deep NN on the MNIST dataset.
#' Provides a template that can be used for other networks as well.
#' Standard Code from https://keras.rstudio.com/
#' Modified by T. Bartz-Beielstein (tbb@bartzundbartz.de)
#'
#' @param x matrix of hyperparameter values to evaluate with the function.
#' Rows for points and columns for dimension.
#' @param kerasConf List of additional parameters passed to keras as described in \code{\link{getKerasConf}}.
#' Default: \code{kerasConf = getKerasConf()}.
#' @param data data
#'
#' @seealso \code{\link{getKerasConf}}
#' @seealso \code{\link{evalKerasTransferLearning}}
#' @seealso \code{\link{evalKerasMnist}}
#' @seealso \code{\link[keras]{fit}}
#'
#' @return 1-column matrix with resulting function values (test loss).
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
#'
#' PYTHON_RETICULATE <- FALSE
#' if(PYTHON_RETICULATE){
#' library("SPOTMisc")
#' library("SPOT")
#' kerasConf <- getKerasConf()
#'
#'  # Hyperparameters:
#'  # "dropout" =  x[1],
#'  # "learning_rate" =  x[2],
#'  # "epochs" = x[3],
#'  # "beta_1" =  x[4],
#'  # "beta_2" =  x[5],
#'  # "epsilon" = x[6],
#'  # "optimizer" = x[7]
#'
#' lower <- c(1e-6, 1e-6, 2, 0.8, 0.8, 1e-9, 1)
#' upper <- c(0.2, 1e-2, 5, 0.99, 0.9999, 1e-3, 2)
#' types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
#'            "integer",  "factor")
#'
#' ## First Example: spot call with extended verbosity. Default objective
#' ## "validationLoss", i.e., validation loss, is used. only 20 function
#' ## evaluations (for testing).
#' kerasConf$verbose <- 1
#' res <- spot(x = NULL,
#'             fun = funKerasTransferLearning,
#'             lower = lower,
#'             upper = upper,
#'             control = list(funEvals=20,
#'                            model=buildKriging,
#'                            noise = TRUE,
#'                            types = types,
#'                            optimizer=optimDE,
#'                            plots = TRUE,
#'                            progress = TRUE,
#'                            seedFun = 1,
#'                            seedSPOT = 1,
#'                            kerasConf = kerasConf)
#'                            )
#'  save(res, file = paste0("resKerasTransferLearning", as.numeric(Sys.time()),".RData"))
#'
#'
#'  ## Example: resKerasTransferLearning04
#'  ## Default objective function "validationLoss", i.e.,
#'  ## training loss
#' library("SPOTMisc")
#' library("SPOT")
#' kerasConf <- getKerasConf()
#'
#' # Hyperparameters:
#' # "dropout" =  x[1],
#' # "learning_rate" =  x[2],
#' # "epochs" = x[3],
#' # "beta_1" =  x[4],
#' # "beta_2" =  x[5],
#' # "epsilon" = x[6],
#' # "optimizer" = x[7]
#'
#' lower <- c(1e-6, 1e-6, 2, 0.8, 0.8, 1e-9, 1)
#' upper <- c(0.2, 1e-2, 5, 0.99, 0.9999, 1e-3, 2)
#' types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
#'            "integer",  "factor")
#'
#' res <- spot(x = NULL,
#'             fun = funKerasTransferLearning,
#'             lower = lower,
#'             upper = upper,
#'             control = list(funEvals=100,
#'                            model=buildKriging,
#'                            noise = TRUE,
#'                            types = types,
#'                            optimizer=optimDE,
#'                            plots = FALSE,
#'                            progress = TRUE,
#'                            seedFun = 1,
#'                            seedSPOT = 1,
#'                            kerasConf = kerasConf))
#' save(res,file = paste0("resKerasTransferLearningValidationLoss04",
#' as.numeric(Sys.time()),".RData"))
#'
#'
#'
#'  ## Example: resKerasTransferLearning05
#'  ## objective function "negValidationAccuracy", i.e.,
#'  ## negative validation accuracy
#' library("SPOTMisc")
#' library("SPOT")
#' kerasConf <- getKerasConf()
#'
#' # Hyperparameters:
#' # "dropout" =  x[1],
#' # "learning_rate" =  x[2],
#' # "epochs" = x[3],
#' # "beta_1" =  x[4],
#' # "beta_2" =  x[5],
#' # "epsilon" = x[6],
#' # "optimizer" = x[7]
#'
#' lower <- c(1e-6, 1e-6, 2, 0.8, 0.8, 1e-9, 1)
#' upper <- c(0.2, 1e-2, 5, 0.99, 0.9999, 1e-3, 2)
#' types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
#'            "integer",  "factor")
#'
#' kerasConf$returnValue <- "negValidationAccuracy"
#' res <- spot(x = NULL,
#'             fun = funKerasTransferLearning,
#'             lower = lower,
#'             upper = upper,
#'             control = list(funEvals=100,
#'                            model=buildKriging,
#'                            noise = TRUE,
#'                            types = types,
#'                            optimizer=optimDE,
#'                            plots = FALSE,
#'                            progress = TRUE,
#'                            seedFun = 1,
#'                            seedSPOT = 1,
#'                            kerasConf = kerasConf))
#' save(res,file = paste0("resKerasTransferLearningNegValidationAccuracy05",
#' as.numeric(Sys.time()),".RData"))
#'
#'
#'  ## Example: resKerasTransferLearning06
#'  ## objective function "trainingLoss", i.e.,
#'  ## training loss
#'
#' library("SPOTMisc")
#' library("SPOT")
#' kerasConf <- getKerasConf()
#'
#' # Hyperparameters:
#' # "dropout" =  x[1],
#' # "learning_rate" =  x[2],
#' # "epochs" = x[3],
#' # "beta_1" =  x[4],
#' # "beta_2" =  x[5],
#' # "epsilon" = x[6],
#' # "optimizer" = x[7]
#'
#' lower <- c(1e-6, 1e-6, 2, 0.8, 0.8, 1e-9, 1)
#' upper <- c(0.2, 1e-2, 5, 0.99, 0.9999, 1e-3, 2)
#' types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
#'            "integer",  "factor")
#'
#' kerasConf$returnValue <- "trainingLoss"
#' res <- spot(x = NULL,
#'             fun = funKerasTransferLearning,
#'             lower = lower,
#'             upper = upper,
#'             control = list(funEvals=100,
#'                            model=buildKriging,
#'                            noise = TRUE,
#'                            types = types,
#'                            optimizer=optimDE,
#'                            plots = FALSE,
#'                            progress = TRUE,
#'                            seedFun = 1,
#'                            seedSPOT = 1,
#'                            kerasConf = kerasConf)
#' )
#' save(res, file = paste0("resKerasTransferLearningTrainingLoss06",
#' as.numeric(Sys.time()),".RData"))
#'  }
#' }
#'
#' @export
funKerasTransferLearning <- function (x,
                                      kerasConf = getKerasConf(),
                                      data = NULL) {
  score <- NULL
  y <- matrix(apply(
    X = x,
    # matrix
    MARGIN = 1,
    # margin (apply over rows)
    evalKerasTransferLearning,
    # function
    kerasConf = kerasConf,
    data = data
  ),
  nrow = nrow(x),
  byrow = TRUE)
  return(y)
}
