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
#' @param kerasConf List of additional parameters passed to keras as described in \code{\link{getKerasConf}}.
#' Default: \code{kerasConf = getKerasConf()}.
#' @param data mnist data set. Default: \code{\link{getMnistData}}.
#'
#' @seealso \code{\link{getKerasConf}}
#' @seealso \code{\link{funKerasMnist}}
#' @seealso \code{\link[keras]{fit}}
#'
#' @return list with function values (training, validation, and test loss/accuracy,
#' and keras model information)
#'
#' @importFrom keras fit
#' @importFrom keras keras_model_sequential
#' @importFrom keras layer_dense
#' @importFrom keras layer_dropout
#' @importFrom keras compile
#' @importFrom keras optimizer_adam
#' @importFrom keras evaluate
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
evalKerasMnist <-
  function(x,
           kerasConf = getKerasConf(),
           data = getMnistData()) {

    score <- NULL
    FLAGS <- list(
      "dropout" =  x[1],
      "dropoutfact" =  x[2],
      "units" = x[3],
      "unitsfact" = x[4],
      "learning_rate" =  x[5],
      "epochs" = x[6],
      "batchsize" = x[7],
      "beta_1" =  x[8],
      "beta_2" =  x[9],
      "layers" =  x[10],
      "epsilon" = x[11]
    )

    if (kerasConf$verbose > 0) {
      printf("dropout: %f", FLAGS$dropout)
      printf("dropoutfac: %f", FLAGS$dropoutfac)
      printf("units: %1.0f", FLAGS$units)
      printf("unitsfac: %f", FLAGS$unitsfact)
      printf("learning_rate: %f", FLAGS$learning_rate)
      printf("epochs: %1.0f", FLAGS$epochs)
      printf("batchsize: %1.0f", FLAGS$batchsize)
      printf("beta_1: %f", FLAGS$beta_1)
      printf("beta_2: %f", FLAGS$beta_2)
      printf("layers: %1.0f", FLAGS$layers)
      printf("epsilon: %f", FLAGS$epsilon)
    }

    if (kerasConf$resDummy)
    {
      y <- matrix(
        runif(6, min = FLAGS$dropout, max = 1 + FLAGS$dropout),
        nrow = 1,
        ncol = 6
      )
      y <- kerasCompileResult(y = y, kerasConf = kerasConf)
      message("evalKerasMnist(): Returning dummy value for testing.")
      return(y)
    } else{
      # Data Preparation
      x_train <- data$x_train
      y_train <- data$y_train
      x_test <- data$x_test
      y_test <- data$y_test

      # Define Model
      with(tf$device("/cpu:0"), {
      model <- keras_model_sequential()
      units <- FLAGS$units
      dropout <- FLAGS$dropout

      # 1st hidden layer with input shape
      model %>%
        layer_dense(
          units = units,
          activation = 'relu',
          input_shape = c(784)
        ) %>%
        layer_dropout(rate = dropout)

      for (i in 2:FLAGS$layers) {
        # dropout changed for next layer
        dropout <- dropout * FLAGS$dropoutfact
        if (kerasConf$verbose > 0) {
          printf("Dropout rate %f in layer %1.0f", dropout, i)
        }
        # unit changed for next layer
        # hidden layer unit should not cross output layer length i.e. 10
        units <- max(as.integer(units * FLAGS$unitsfact), 10)
        # add dense layer
        model %>% layer_dense(units = units, activation = 'relu')
        if (dropout != 0) {
          # add dropout layer
          model %>% layer_dropout(rate = dropout)
        }
      }

      # Adding the final layer with ten units (classes) and softmax
      model %>% layer_dense(units = 10, activation = 'softmax')

      # decayed_learning_rate = tf.train.exponential_decay(learning_rate,
      #            global_step, 10000,
      #          0.95, staircase=True)


      model %>% compile(
        loss = 'categorical_crossentropy',
        optimizer = optimizer_adam(
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
          # Gradients will be clipped when their L2 norm exceeds this value.
          clipnorm = NULL,
          # Gradients will be clipped when their absolute value exceeds this value.
          clipvalue = NULL
        ),
        metrics = c('accuracy')
      )
      if (kerasConf$verbose > 0) {
        print(model)
      }

      # Training & Evaluation
      history <- model %>% fit(
        x_train,
        y_train,
        batch_size = FLAGS$batchsize,
        epochs = FLAGS$epochs,
        verbose = kerasConf$verbose,
        validation_split = kerasConf$validation_split,
        shuffle = kerasConf$shuffle
      )
    if (kerasConf$verbose > 0) {
        cat('val loss:', history$metrics$val_loss , '\n')
        cat('val accuracy:',  history$metrics$val_acc, '\n')
        plot(history)
      }

      # evaluate on test data
      score <- model %>% evaluate(x_test, y_test,
                                  verbose = kerasConf$verbose)
      }) ## end with
      ## y: matrix with six entries:
      # trainingLoss,  negTrainingAccuracy,
      # validationLoss,  negValidationAccuracy,
      # testLoss,  negTestAccuracy:
      y <- matrix(
        c(
          history$metrics$loss[length(history$metrics$loss)],-history$metrics$accuracy[length(history$metrics$accuracy)],
          history$metrics$val_loss[length(history$metrics$val_loss)],-history$metrics$val_accuracy[length(history$metrics$val_accuracy)],
          score[[1]],-score[[2]]
        ),
        nrow = 1,
        ncol = 6
      )

      if (kerasConf$verbose > 0) {
        message("funKerasMnist: y matrix before kerasCompileResult()")
        print(y)
      }
      y <- kerasCompileResult(y = y, kerasConf = kerasConf)

      if (kerasConf$verbose > 0) {
        message("funKerasMnist: y matrix after kerasCompileResult()")
        print(y)
      }

      if (kerasConf$clearSession) {
        keras::k_clear_session()
      }
      return(y)
    }
  }


#' @title funKerasMnist
#'
#' @description Hyperparameter Tuning: Keras MNIST Classification Test Function.
#'
#' @details Trains a simple deep NN on the MNIST dataset.
#' Provides a template that can be used for other networks as well.
#' Standard Code from https://keras.rstudio.com/
#' Modified by T. Bartz-Beielstein (tbb@bartzundbartz.de)
#'
#' @param x matrix of hyperparameter values to evaluate with the function.
#' Rows for points and columns for dimension.
#' @param kConf List of additional parameters passed to keras as described in \code{\link{getKerasConf}}.
#' Default: \code{kConf = getKerasConf()}.
#' @param data mnist data set. Default: \code{\link{getMnistData}}.
#'
#' @seealso \code{\link{getKerasConf}}
#' @seealso \code{\link{evalKerasMnist}}
#' @seealso \code{\link[keras]{fit}}
#'
#' @return 1-column matrix with resulting function values (test loss)
#'
#' @importFrom keras fit
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
funKerasMnist <-
  function (x, kConf = getKerasConf(), data = getMnistData()) {
    y <- matrix(apply(
      X = x,
      # matrix
      MARGIN = 1,
      # margin (apply over rows)
      evalKerasMnist,
      # function
      kerasConf = kConf,
      data = data
    ),
    nrow = nrow(x),
    byrow = TRUE)
    return(y)
  }
