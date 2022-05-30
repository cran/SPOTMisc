#' @title getMnistData
#' @description Based on the setting \code{kerasConf$encoding} either one-hot encoded data or
#' tensor-shaped data are returned.The labels are converted to binary class matrices using
#' the function \code{\link[keras]{to_categorical}}.
#'
#' @seealso \code{\link{getKerasConf}}
#' @seealso \code{\link{funKerasMnist}}
#'
#' @param kerasConf List of additional parameters passed to keras as described
#' in \code{\link{getKerasConf}}. Default: \code{NULL}.
#'
#' @return list with training and test data, i.e.,
#' \code{list(x_train, x_test, y_train, y_test)}.
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
#' kerasConf$encoding <- "oneHot" # default
#' mnist <- getMnistData(kerasConf)
#' # lots of zeros, but there are also some nonzero (greyscale) values, e.g.:
#' mnist$x_train[1,150:160]
#' str(mnist$x_train[1,])
#' # y-labels are one-hot encoded. The first entry represents "5"
#' mnist$y_train[1,]
#' ##
#' kerasConf$encoding <- "tensor"
#' mnist <- getMnistData(kerasConf)
#' ## 28x28:
#' str(mnist$x_train[1,,,])
#' mnist$y_train[1,]
#'}
#'}
#' @export
getMnistData <- function(kerasConf) {
  #encodingList <- list("oneHot", "tensor")
  mnist <- dataset_mnist()
  x_train <- mnist$train$x
  y_train <- mnist$train$y
  x_test <- mnist$test$x
  y_test <- mnist$test$y
  encoding <- kerasConf$encoding
  switch(kerasConf$encoding,
         oneHot = {
           # The x data is a 3-d array (images,width,height) of grayscale values.
           # To prepare the data for a dense net, the 3-d arrays are converted
           # into matrices by reshaping width and height into a single dimension
           # (28x28 images are flattened into length 784 vectors).
           # Then, we convert the grayscale values from integers ranging between 0 to 255
           # into floating point values ranging between 0 and 1:

           x_train <-
             array_reshape(x_train, c(nrow(x_train), 28 * 28))
           x_test <- array_reshape(x_test, c(nrow(x_test), 28 * 28))

           # Transform RGB values into [0,1] range
           x_train <- x_train / 255
           x_test <- x_test / 255

           # Convert class vectors to binary class matrices
           y_train <- to_categorical(y_train, 10)
           y_test <- to_categorical(y_test, 10)
         },
         tensor = {
           x_train <- array_reshape(x_train, c(nrow(x_train), 28, 28, 1))
           x_train <- x_train / 255

           x_test <-
             array_reshape(x_test, c(nrow(x_test), 28, 28, 1))
           x_test <- x_test / 255

           y_train <- to_categorical(y_train)
           y_test <- to_categorical(y_test)

         },
         stop("getMnistData: Wrong encoding.")) # end switch

  return(list(
    x_train = x_train,
    x_test = x_test,
    y_train = y_train,
    y_test = y_test
  ))
}


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
#' @importFrom keras layer_max_pooling_2d
#' @importFrom keras layer_dropout
#' @importFrom keras layer_conv_2d
#' @importFrom keras layer_flatten
#' @importFrom keras compile
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
#' kerasConf$model <- "dl"
#' cfg <-  getModelConf(kerasConf)
#' x <- matrix(cfg$default, nrow=1)
#' if (length(cfg$transformations) > 0) {  x <- transformX(xNat=x, fn=cfg$transformations)}
#' res <- evalKerasMnist(x, kerasConf, data = getMnistData(kerasConf))
#' #
#' kerasConf$model <- "cnn"
#' kerasConf$encoding <- "tensor"
#' cfg <-  getModelConf(kerasConf)
#' x <- matrix(cfg$default, nrow=1)
#' if (length(cfg$transformations) > 0) {  x <- transformX(xNat=x, fn=cfg$transformations)}
#' res <- evalKerasMnist(x, kerasConf, data = getMnistData(kerasConf))
#'}
#'}
#' @export
#'
#'
evalKerasMnist <-
  function(x,
           kerasConf,
           data) {
    score <- NULL
    if (kerasConf$resDummy)
    {
      y <- matrix(runif(6, min = x[1], max = 1 + x[1]),
                  nrow = 1,
                  ncol = 6)
      y <- kerasCompileResult(y = y, kerasConf = kerasConf)
      message("evalKerasMnist(): Returning dummy value for testing.")
      return(y)
    } else{
      # Data Preparation
      x_train <- data$x_train
      y_train <- data$y_train
      x_test <- data$x_test
      y_test <- data$y_test

      # FIXME: put this in the config:
      kerasConf$loss <- "categorical_crossentropy"
      kerasConf$metrics <- "accuracy"
      kerasConf$clearSession <- TRUE

      # Define Model
      with(tf$device("/cpu:0"), {
        switch(kerasConf$model,
               dl = {
                 if (kerasConf$encoding != "oneHot")
                   stop("Encoding must be oneHot.")
                 FLAGS <- mapX2FLAGS(x, model = "dl")
                 model <- keras_model_sequential()
                 dropout <- FLAGS$dropout

                 # 1st hidden layer with input shape
                 model %>%
                   layer_dense(
                     units = FLAGS$units,
                     activation = 'relu',
                     input_shape = c(784)
                   ) %>%
                   layer_dropout(rate = FLAGS$dropout)

                 for (i in 2:FLAGS$layers) {
                   # dropout changed for next layer
                   FLAGS$dropout <-
                     FLAGS$dropout * FLAGS$dropoutfact
                   if (kerasConf$verbose > 0) {
                     printf("Dropout rate %f in layer %1.0f", FLAGS$dropout, i)
                   }
                   # unit changed for next layer
                   # hidden layer unit should not cross output layer length i.e. 10
                   FLAGS$units <-
                     max(as.integer(FLAGS$units * FLAGS$unitsfact), 10)
                   # add dense layer
                   model %>% layer_dense(units = FLAGS$units, activation = 'relu')
                   if (FLAGS$dropout != 0) {
                     # add dropout layer
                     model %>% layer_dropout(rate = FLAGS$dropout)
                   }
                 }

                 # Adding the final layer with ten units (classes) and softmax
                 model %>% layer_dense(units = 10, activation = 'softmax')
               },
               cnn = {
                 if (kerasConf$encoding != "tensor")
                   stop("Encoding must be tensor.")
                 ## activeX <- rep(1,length(getModelConf(model="cnn")$defaults))
                 ## 1. Get default parameters for all parameters
                 ## xDefault <- getModelConf(model="cnn")$defaults
                 ## FLAGS <- mapX2FLAGS(xDefault, model="cnn")
                 ## 2. Overwrite "active" flags with tuned parameters:
                 ##
                 size <- 28
                 FLAGS <- mapX2FLAGS(x, model = "cnn")
                 printFLAGS(FLAGS)
                 nFilters <- FLAGS$nFilters
                 model <- keras_model_sequential() %>%
                   layer_conv_2d(
                     filters = nFilters,
                     kernel_size = c(FLAGS$kernelSize, FLAGS$kernelSize),
                     activation = selectKerasActivation(FLAGS$activation),
                     input_shape = c(size, size, 1)
                   )
                 ## add more layers?
                 if (FLAGS$layers > 1) {
                   for (i in 2:FLAGS$layers) {
                     ## FIXME: better geometry handling
                     vmessage(kerasConf$verbose, "Size of the cnn in evalKerasMnist:", size)
                     if(size - FLAGS$kernelSize + 1 > max(2, FLAGS$kernelSize))
                     {
                       ## if pool size == 0, no pooling will be done, otherwise max (2x2) pooling
                       if (FLAGS$poolSize > 0) {
                         model %>%
                           layer_max_pooling_2d(pool_size = c(2,2))
                       }
                     nFilters <- i * nFilters
                     model %>%  layer_conv_2d(
                       filters = nFilters,
                       kernel_size = c(FLAGS$kernelSize, FLAGS$kernelSize),
                       activation = selectKerasActivation(FLAGS$activation),
                     )
                     size <- size - FLAGS$kernelSize + 1
                     if (FLAGS$poolSize > 0) {
                     size <- floor(size/2)
                     }
                     }
                   }
                 }
                 model <- model %>%
                   layer_flatten() %>%
                   layer_dense(units = nFilters, activation = "relu")
                 model %>%  layer_dense(units = 10, activation = "softmax")
               },
               stop("evalKerasMnist: Wrong model.")) # end switch

        model %>% compile(
          loss = kerasConf$loss,
          #rlang::sym(kerasConf$loss),
          optimizer = selectKerasOptimizer(
            FLAGS$optimizer,
            learning_rate = FLAGS$learning_rate,
            momentum = 0.0,
            decay = 0.0,
            nesterov = FALSE,
            clipnorm = NULL,
            clipvalue = NULL,
            rho = 0.9,
            epsilon = FLAGS$epsilon,
            beta_1 = FLAGS$beta_1,
            beta_2 = FLAGS$beta_2,
            amsgrad = FALSE
          )
          ,
          metrics = kerasConf$metrics
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
          history$metrics$loss[length(history$metrics$loss)],
          -history$metrics$accuracy[length(history$metrics$accuracy)],
          history$metrics$val_loss[length(history$metrics$val_loss)],
          -history$metrics$val_accuracy[length(history$metrics$val_accuracy)],
          score[[1]],
          -score[[2]]
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
#' @param kerasConf List of additional parameters passed to keras as described in \code{\link{getKerasConf}}.
#' Default: \code{kerasConf = getKerasConf()}.
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
#' ## The following two settings are default:
#' kerasConf$encoding = "oneHot"
#' kerasConf$model = "dl"
#' ## get the data with the correct encoding
#' mnist <- getMnistData(kerasConf)
#' ## get the model
#' cfg <-  getModelConf(kerasConf)
#'
#' ### First example: simple function call:
#' x <- matrix(cfg$default, nrow=1)
#' if (length(cfg$transformations) > 0) {  x <- transformX(xNat=x, fn=cfg$transformations)}
#' funKerasMnist(x, kerasConf = kerasConf, data = mnist)
#' ### Use convnet:
#' kerasConf <- getKerasConf()
#' kerasConf$encoding <- "tensor"
#' kerasConf$model <- "cnn"
#' mnist <- getMnistData(kerasConf)
#' cfg <-  getModelConf(kerasConf)
#' x <- matrix(cfg$default, nrow=1)
#' if (length(cfg$transformations) > 0) {  x <- transformX(xNat=x, fn=cfg$transformations)}
#' funKerasMnist(x, kerasConf = kerasConf, data=mnist)
#'
#' ### Second example: evaluation of several (three) hyperparameter settings:
#' x <- matrix(cfg$default, nrow=1)
#' if (length(cfg$transformations) > 0) {  x <- transformX(xNat=x, fn=cfg$transformations)}
#' xxx <- rbind(x,x,x)
#' funKerasMnist(xxx, kerasConf = kerasConf, data=mnist)
#'
#' ### Third example: spot call (dense network):
#' kerasConf <- getKerasConf()
#' kerasConf$verbose <- 0
#' kerasConf$encoding = "oneHot"
#' kerasConf$model = "dl"
#' ## get the data with the correct encoding
#' mnist <- getMnistData(kerasConf)
#' ## get the model
#' cfg <-  getModelConf(kerasConf)
#' ## max 32 training epochs
#' cfg$upper[6] <- 5
#' resDl <- spot(x = NULL,
#'             fun = funKerasMnist,
#'             lower = cfg$lower,
#'             upper = cfg$upper,
#'             control = list(funEvals=15,
#'                          transformFun = cfg$transformations,
#'                          types = cfg$type,
#'                          noise = TRUE,
#'                          plots = TRUE,
#'                          progress = TRUE,
#'                          seedFun = 1,
#'                          seedSPOT = 1),
#'                          kerasConf = kerasConf,
#'                          data = mnist)
#'
#' ### Fourth example: spot call (convnet):
#' kerasConf <- getKerasConf()
#' kerasConf$verbose <- 1
#' kerasConf$encoding <- "tensor"
#' kerasConf$model <- "cnn"
#' ## get the data with the correct encoding
#' mnist <- getMnistData(kerasConf)
#' ## get the model
#' cfg <-  getModelConf(kerasConf)
#' ## max 32 training epochs
#' cfg$upper[6] <- 5
#' resCnn <- spot(x = NULL,
#'             fun = funKerasMnist,
#'             lower = cfg$lower,
#'             upper = cfg$upper,
#'             control = list(funEvals=15,
#'                          transformFun = cfg$transformations,
#'                          types = cfg$type,
#'                          noise = TRUE,
#'                          plots = TRUE,
#'                          progress = TRUE,
#'                          seedFun = 1,
#'                          seedSPOT = 1),
#'                          kerasConf = kerasConf,
#'                          data = mnist)
#' }
#' }
#'
#' @export
funKerasMnist <-
  function (x, kerasConf, data) {
    y <- matrix(
      apply(
        X = x,
        # matrix
        MARGIN = 1,
        # margin (apply over rows)
        evalKerasMnist,
        # function
        kerasConf = kerasConf,
        data = data
      ),
      nrow = nrow(x),
      byrow = TRUE
    )
    return(y)
  }
