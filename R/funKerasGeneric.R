#' @title evalKerasGeneric model building and compile
#' @description Hyperparameter Tuning: Keras Generic Classification Function.
#' @details Trains a simple deep NN on a generic data set.
#' Standard Code from \url{https://tensorflow.rstudio.com/}.
#' Modified by T. Bartz-Beielstein.
#' @param x matrix of transformed hyperparameter values to evaluate with the function.
#' If \code{NULL}, a simple keras model will be build, which is considered default
#' (see \code{\link{getSimpleKerasModel}}).
#' @param kerasConf List of additional parameters passed to keras as described in
#' \code{\link{getKerasConf}}. If no value is specified, stop() is called.
#' @param specList prepared data. See \code{\link{genericDataPrep}}.
#' See \code{\link{getGenericTrainValTestData}}.
#'
#' @seealso \code{\link{getKerasConf}}
#' @seealso \code{\link{funKerasGeneric}}
#' @seealso \code{\link[keras]{fit}}
#'
#' @return list with function values (training, validation, and test loss/accuracy,
#' and keras model information)
#' @importFrom keras evaluate
#' @importFrom keras k_clear_session
#' @importFrom stats predict
#' @importFrom keras layer_dense_features
#' @importFrom tfdatasets dense_features
#' @importFrom tfdatasets dataset_use_spec
#' @importFrom reticulate import
#' @importFrom SPOT vmessage
#' @export
evalKerasGeneric <-  function(x = NULL,
                              kerasConf = NULL,
                              specList = NULL) {
  if (is.null(kerasConf)) {
    stop("evalKerasGeneric(): argument kerasConf is missing")
  }
  os <- reticulate::import("os")
  tf <- reticulate::import("tensorflow")
  tf$get_logger()$setLevel('ERROR')
  if (kerasConf$resDummy)
  {
    y <- kerasReturnDummy(kerasConf = kerasConf)
    y <- kerasCompileResult(y = y,
                            kerasConf = kerasConf)
    message("evalKerasGeneric(): Returning dummy value for testing.")
    return(y)
  } else if (is.null(x))
  {
    model <- getSimpleKerasModel(specList = specList,
                                 kerasConf = kerasConf)
    FLAGS <- list(epochs = 16)
  } else{
    ## map numeric x hyperparameters to FLAGS:
    FLAGS <- mapX2FLAGS(x, model = "dl")
    model <- kerasBuildCompile(FLAGS = FLAGS,
                               kerasConf = kerasConf,
                               specList = specList)
  }
  y <- try(kerasFit(
    model = model,
    specList = specList,
    FLAGS = FLAGS,
    kerasConf = kerasConf
  ))

  if (inherits(y, "try-error")) {
    y <- matrix(rep(NA, 6), nrow = 1)
    y <- kerasCompileResult(y = y,
                            kerasConf = kerasConf)
    message("evalKerasGeneric(): Returning NAs.")
    return(y)
  }

  model <- y$model
  history <- y$history
  if (kerasConf$returnObject == "model") {
    ## changed in 1.19.46: return y instead of only the model
    # return(model)
    return(y)
  }
  # evaluate on test data
  pred <- predict(model, specList$testGeneric)
  if (kerasConf$returnObject == "pred") {
    if (kerasConf$clearSession) {
      keras::k_clear_session()
    }
    return(list(trueY = specList$testGeneric$target, hatY = pred))
  }
  ## in use keras evaluation (test error):
  ## FIXME: remove after testing
  # testScore <- model %>%
  #   evaluate(
  #     specList$test_ds_generic %>% dataset_use_spec(specList$specGeneric_prep),
  #     verbose = kerasConf$verbose
  #   )
  testScore <- keras::evaluate(
    model,
    dataset_use_spec(specList$test_ds_generic, specList$specGeneric_prep),
    verbose = kerasConf$verbose
  )
  y <- kerasEvalPrediction(
    pred = pred,
    testScore = testScore,
    specList = specList,
    metrics = history$metrics,
    kerasConf = kerasConf
  )
  vmessage(kerasConf$verbose, "funKerasGeneric: evaluation on test data with keras::evaluate()", testScore)
  vmessage(kerasConf$verbose, "funKerasGeneric: evaluation with Metrics::logLoss/accuracy()", y)
  if (kerasConf$clearSession) {
    k_clear_session()
  }
  return(y)
}

#' @title evalKerasGeneric model building and compile
#'
#' @description Hyperparameter Tuning: Keras Generic Classification Function.
#'
#' @details Trains a simple deep NN on a generic data set.
#' Standard Code from \url{https://tensorflow.rstudio.com/}
#' Modified by T. Bartz-Beielstein (tbb@bartzundbartz.de)
#'
#' @param FLAGS flags. list of hyperparameter values.
#' If \code{NULL}, a simple keras model will be build, which is considered default
#' (see \code{\link{getSimpleKerasModel}}).
#' @param kerasConf List of additional parameters passed to keras as described in \code{\link{getKerasConf}}.
#' Default: \code{kerasConf = getKerasConf()}.
#' @param specList prepared data. See \code{\link{genericDataPrep}}.
#' See \code{\link{getGenericTrainValTestData}}.
#'
#' @seealso \code{\link{getKerasConf}}
#' @seealso \code{\link{funKerasGeneric}}
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
#' @importFrom stats predict
#' @importFrom keras layer_dense_features
#' @importFrom tfdatasets dense_features
#' @importFrom tfdatasets dataset_use_spec
#' @importFrom rlang sym
#' @importFrom reticulate import
#' @importFrom keras %>%
#'
#' @export
kerasBuildCompile <-  function(FLAGS,
                               kerasConf,
                               specList) {
  os <- reticulate::import("os")
  tf <- reticulate::import("tensorflow")
  tf$get_logger()$setLevel('ERROR')
  # Complex model with variable number of layers:
  # 1st hidden layer with input shape:
  # Instantiate the base model (or "template" model).
  # We recommend doing this with under a CPU device scope,
  # so that the model's weights are hosted on CPU memory.
  # Otherwise they may end up hosted on a GPU, which would
  # complicate weight sharing.
  with(tf$device("/cpu:0"), {
    model <- keras_model_sequential() %>%
      layer_dense_features(dense_features(specList$specGeneric_prep)) %>%
      layer_dense(units = FLAGS$units, activation = "relu")
    if (FLAGS$layers > 1) {
      model %>% layer_dropout(rate = FLAGS$dropout)
      for (i in 2:FLAGS$layers) {
        # unit changed for next layer
        # hidden layer unit should not cross output layer length i.e. 1
        FLAGS$units <-
          max(as.integer(FLAGS$units * FLAGS$unitsfact), 1)
        # add dense layer
        model %>% layer_dense(units = FLAGS$units, activation = 'relu')
        # dropout changed for next layer
        FLAGS$dropout <- FLAGS$dropout * FLAGS$dropoutfact
        if (FLAGS$dropout > 0) {
          # add dropout layer
          model %>% layer_dropout(rate = FLAGS$dropout)
        }
      }
    }
    ## Adding the final layer with nClasses units, where each unit represents one class
    ## in  multi-class classification and sigmoid/softmax (nClasses == 1 for binary classification)
    model %>% layer_dense(units = kerasConf$nClasses,
                          activation = kerasConf$activation)
    ## Model building is done
    if (kerasConf$loss == "loss_binary_crossentropy") {
      kerasConf$loss <- rlang::sym(kerasConf$loss)
    }
    model %>% compile(
      #loss = rlang::sym(kerasConf$loss),
      loss = kerasConf$loss,
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
  }) # end with
  return(model)
}

#' @title kerasFit fit
#'
#' @description Hyperparameter Tuning: Keras Generic Classification Function.
#'
#' @details Trains a simple deep NN on a generic data set.
#' Standard Code from \url{https://tensorflow.rstudio.com/}
#' Modified by T. Bartz-Beielstein (tbb@bartzundbartz.de)
#'
#' @param model model
#' If \code{NULL}, a simple keras model will be build, which is considered default
#' (see \code{\link{getSimpleKerasModel}}).
#' @param kerasConf List of additional parameters passed to keras as described in \code{\link{getKerasConf}}.
#' Default: \code{kerasConf = getKerasConf()}.
#' @param specList prepared data. See \code{\link{genericDataPrep}}.
#' See \code{\link{getGenericTrainValTestData}}.
#' @param FLAGS flags, see also \code{\link{mapX2FLAGS}}
#'
#' @seealso \code{\link{getKerasConf}}
#' @seealso \code{\link{funKerasGeneric}}
#' @seealso \code{\link[keras]{fit}}
#'
#' @return list with function values (training, validation, and test loss/accuracy,
#' and keras model information)
#'
#' @importFrom keras fit
#' @importFrom tfdatasets dataset_use_spec
#' @export
kerasFit <- function(model,
                     specList,
                     FLAGS,
                     kerasConf) {
  os <- reticulate::import("os")
  tf <- reticulate::import("tensorflow")
  tf$get_logger()$setLevel('ERROR')

  with(tf$device(kerasConf$tfDevice), {
    # Training & Evaluation
    history <- model %>% fit(
      dataset_use_spec(specList$train_ds_generic,
                       spec = specList$specGeneric_prep),
      epochs = FLAGS$epochs,
      validation_data = dataset_use_spec(specList$val_ds_generic,
                                         specList$specGeneric_prep),
      verbose = kerasConf$verbose
    )
  }) # end with
  ## model building, compilation, and fitting completed
  if (kerasConf$verbose > 0) {
    #printFLAGS(FLAGS)
    print(model)
    print(history$metrics)
    plot(history)
  }
  return(list(model = model, history = history))
}

#' @title funKerasGeneric
#'
#' @description Hyperparameter Tuning: Generic Classification Objective Function.
#'
#' @details Trains a simple deep NN on arbitrary data sets.
#' Provides a template that can be used for other networks as well.
#' Standard Code from \url{https://tensorflow.rstudio.com/}
#' Modified by T. Bartz-Beielstein (tbb@bartzundbartz.de)
#'
#' Note: The WARNING "tensorflow:Layers in a Sequential model should only have a single input tensor.
#'      Consider rewriting this model with the Functional API"
#'      can be safely ignored:
#'      in general, Keras encourages its users to use functional models
#'      for multi-input layers, but there is nothing wrong with doing so.
#'      See: \url{https://github.com/tensorflow/recommenders/issues/188}.
#'
#' @param x matrix of hyperparameter values to evaluate with the function.
#' Rows for points and columns for dimension.
#' @param kerasConf List of additional parameters passed to keras as described in \code{\link{getKerasConf}}.
#' Default: \code{NULL}.
#' @param specList prepared data. See \code{\link{genericDataPrep}}.
#' @seealso \code{\link{getKerasConf}}
#' @seealso \code{\link{evalKerasGeneric}}
#' @seealso \code{\link{evalKerasGeneric}}
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
#' PYTHON_RETICULATE <- FALSE
#' if(PYTHON_RETICULATE){
#'
#' ## data preparation
#' target <- "age"
#' batch_size <- 32
#' prop <- 2/3
#' dfGeneric <- getDataCensus(target = target, nobs = 1000)
#' data <- getGenericTrainValTestData(dfGeneric = dfGeneric, prop = prop)
#' specList <- genericDataPrep(data=data, batch_size = batch_size)
#'
#' ## model configuration:
#' cfg <-  getModelConf(list(model="dl"))
#' x <- matrix(cfg$default, nrow=1)
#' transformFun <- cfg$transformations
#' types <- cfg$type
#' lower <- cfg$lower
#' upper <- cfg$upper
#'
#' kerasConf <- getKerasConf()
#'
#' ### First example: simple function call:
#' message("objectiveFunctionEvaluation(): x before transformX().")
#' print(x)
#' if (length(transformFun) > 0) {  x <- transformX(xNat=x, fn=transformFun)}
#' message("objectiveFunctionEvaluation(): x after transformX().")
#' print(x)
#' funKerasGeneric(x, kerasConf = kerasConf, specList = specList)
#'
#' ### Second example: evaluation of several (three) hyperparameter settings:
#' xxx <- rbind(x,x,x)
#' funKerasGeneric(xxx, kerasConf = kerasConf, specList)
#'
#' ### Third example: spot call with extended verbosity:
#' res <- spot(x = NULL,
#'             fun = funKerasGeneric,
#'             lower = lower,
#'             upper = upper,
#'             control = list(funEvals=50,
#'                          handleNAsMethod = handleNAsMean,
#'                          noise = TRUE,
#'                          types = types,
#'                          plots = TRUE,
#'                          progress = TRUE,
#'                          seedFun = 1,
#'                          seedSPOT = 1,
#'                          transformFun=transformFun),
#'                          kerasConf = kerasConf,
#'                          specList = specList)
#'   }
#' }
#'
#' @export
funKerasGeneric <-  function (x,
                              kerasConf = NULL,
                              specList = NULL) {
  if (is.null(kerasConf)) {
    stop("funKerasGeneric(): argument kerasConf is missing")
  }
  y <- matrix(
    apply(
      X = x,
      # matrix
      MARGIN = 1,
      # margin (apply over rows)
      evalKerasGeneric,
      # function
      kerasConf = kerasConf,
      specList = specList
    ),
    nrow = nrow(x),
    byrow = TRUE
  )
  return(y)
}


#' @title Create an input pipeline using tfdatasets
#' @param batch_size batch size. Default: 32
#' @param data data. List, e.g., df$trainCensus, df$testGeneric, and df$valCensus data)
#' @param minLevelSizeEmbedding integer. Embedding will be used for
#' factor variables with more than \code{minLevelSizeEmbedding} levels. Default: \code{100}.
#' @param embeddingDim integer. Dimension used for embedding. Default: \code{floor(log(minLevelSizeEmbedding))}.
#'
#' @importFrom tfdatasets tensor_slices_dataset
#' @importFrom tfdatasets dataset_shuffle
#' @importFrom tfdatasets dataset_batch
#' @importFrom tfdatasets feature_spec
#' @importFrom tfdatasets step_numeric_column
#' @importFrom tfdatasets all_numeric
#' @importFrom tfdatasets all_nominal
#' @importFrom tfdatasets scaler_standard
#' @importFrom tfdatasets step_categorical_column_with_vocabulary_list
#' @importFrom tfdatasets fit
#' @importFrom tfdatasets step_indicator_column
#' @importFrom tfdatasets step_embedding_column
#' @importFrom tfdatasets matches
#' @importFrom magrittr %>%
#' @importFrom dplyr select_if
#' @importFrom dplyr mutate_if
#'
#' @examples
#' \donttest{
#' ### These examples require an activated Python environment as described in
#' ### Bartz-Beielstein, T., Rehbach, F., Sen, A., and Zaefferer, M.:
#' ### Surrogate Model Based Hyperparameter Tuning for Deep Learning with SPOT,
#' ### June 2021. http://arxiv.org/abs/2105.14625.
#' PYTHON_RETICULATE <- FALSE
#' if(PYTHON_RETICULATE){
#' target <- "age"
#' batch_size <- 32
#' prop <- 2/3
#' cachedir <- "oml.cache"
#' dfCensus <- getDataCensus(target = target,
#' nobs = 1000, cachedir = cachedir, cache.only=FALSE)
#' data <- getGenericTrainValTestData(dfGeneric = dfCensus,
#' prop = prop)
#' specList <- genericDataPrep(data=data, batch_size = batch_size)
#' ## Call iterator:
#' require(magrittr)
#' specList$train_ds_generic %>%
#'   reticulate::as_iterator() %>%
#'    reticulate::iter_next()
#' }
#' }
#'
#' @returns a fitted \code{FeatureSpec} object and the hold-out testGeneric (=data$testGeneric).
#' This is returned as the follwoing list.
#' \describe{
#'   \item{\code{train_ds_generic}}{train}
#'   \item{\code{val_ds_generic}}{validation}
#'   \item{\code{test_ds_generic}}{test}
#'   \item{\code{specGeneric_prep}}{feature spec object}
#'   \item{\code{testGeneric}}{data$testGeneric}
#' }
#' @export
genericDataPrep <- function(data,
                            batch_size = 32,
                            minLevelSizeEmbedding = 100,
                            embeddingDim = NULL) {
  if(is.null(embeddingDim)){
    embeddingDim <- floor(log(minLevelSizeEmbedding))
  }
  train_ds_generic <- val_ds_generic <- test_ds_generic <- NULL

  df_to_dataset <- function(df,
                            shuffle = TRUE,
                            batch_size = 32) {
    ds <- df %>%
      tensor_slices_dataset()

    if (shuffle)
      ds <- ds %>% dataset_shuffle(buffer_size = nrow(df))

    ds %>%
      dataset_batch(batch_size = batch_size)
  }

  train_ds_generic <-
    df_to_dataset(data$trainGeneric, batch_size = batch_size)
  val_ds_generic <-
    df_to_dataset(data$valGeneric, shuffle = FALSE, batch_size = batch_size)
  # test data is not used. Generated for the final evaluation.
  test_ds_generic <-
    df_to_dataset(data$testGeneric,
                  shuffle = FALSE,
                  batch_size = batch_size)

  # train_ds_generic %>%
  #   reticulate::as_iterator() %>%
  #   reticulate::iter_next()

  # factorVars <-  names(data[,sapply(data, is.factor)])
  df <- rbind(data$trainGeneric, data$valGeneric)
  embeddingVars <- names(df %>% mutate_if(is.character, factor) %>% select_if(~ is.factor(.) & nlevels(.) > minLevelSizeEmbedding))
  noEmbeddingVars <- names(df %>% mutate_if(is.character, factor) %>% select_if(~ is.factor(.) & nlevels(.) <= minLevelSizeEmbedding))

  specGeneric <- feature_spec(train_ds_generic, target ~ .)

  # specGeneric <- specGeneric %>%
  #   step_numeric_column(all_numeric(),
  #                       normalizer_fn = scaler_standard()) %>%
  #   step_categorical_column_with_vocabulary_list(all_nominal())
  #
  # specGeneric <- specGeneric %>%
  #   step_indicator_column(all_nominal()) %>%
  #   step_embedding_column(all_nominal(), dimension = 2)

  specGeneric <- specGeneric %>%
    step_numeric_column(all_numeric(),
                        normalizer_fn = scaler_standard()) %>%
    step_categorical_column_with_vocabulary_list(all_nominal()) %>%
    step_indicator_column(matches(noEmbeddingVars)) %>%
    step_embedding_column(matches(embeddingVars), dimension = embeddingDim)

  specGeneric_prep <- fit(specGeneric)

  return(
    list(
      train_ds_generic = train_ds_generic,
      val_ds_generic = val_ds_generic,
      test_ds_generic = test_ds_generic,
      specGeneric_prep = specGeneric_prep,
      testGeneric = data$testGeneric
    )
  ) # testGeneric is the "real" hold-out test data set
}



#' @title Return dummy values
#'
#' @param kerasConf keras configuration list
#'
#' @returns y row matrix of random (uniformly distributed) return values
#' @examples
#' kerasConf <- getKerasConf()
#' kerasReturnDummy(kerasConf)
#'
#' @export
kerasReturnDummy <- function(kerasConf) {
  n <- 6 # dim of y (return) values
  y <- matrix(runif(n),
              nrow = 1,
              ncol = n)
  return(y)
}


#' @title getSimpleKerasModel
#'
#' @description build, compile, and train a simple model  (for testing)
#' @importFrom keras fit
#' @importFrom keras keras_model_sequential
#' @importFrom keras layer_dense
#' @importFrom keras layer_dropout
#' @importFrom keras compile
#' @importFrom keras optimizer_adam
#' @importFrom keras evaluate
#' @importFrom stats predict
#' @importFrom keras layer_dense_features
#' @importFrom tfdatasets dense_features
#' @importFrom tfdatasets dataset_use_spec
#' @importFrom rlang sym
#'
#' @param specList spec
#' @param kerasConf keras configuration. Default: return value from \code{\link{getKerasConf}}.
#'
#' @returns model. Fitted keras model
#'
#' @examples
#' \donttest{
#' ### These examples require an activated Python environment as described in
#' ### Bartz-Beielstein, T., Rehbach, F., Sen, A., and Zaefferer, M.:
#' ### Surrogate Model Based Hyperparameter Tuning for Deep Learning with SPOT,
#' ### June 2021. http://arxiv.org/abs/2105.14625.
#' PYTHON_RETICULATE <- FALSE
#' if(PYTHON_RETICULATE){
#' target <- "age"
#' nobs <- 1000
#' batch_size <- 32
#' prop <- 2/3
#' dfCensus <- getDataCensus(target = target,
#' nobs = nobs)
#' data <- getGenericTrainValTestData(dfGeneric = dfCensus,
#' prop = prop)
#' specList <- genericDataPrep(data=data, batch_size = batch_size)
#' kerasConf <- getKerasConf()
#' simpleModel <- getSimpleKerasModel(specList = specList,
#'                kerasConf = kerasConf)
#' }
#' }
#' @export
#'
getSimpleKerasModel <- function(specList,
                                kerasConf = getKerasConf()) {
  message("getSimpleKerasModel")
  with(tf$device("/cpu:0"), {
    model <- keras_model_sequential() %>%
      layer_dense_features(dense_features(specList$specGeneric_prep)) %>%
      layer_dense(units = 32, activation = "relu") %>%
      layer_dense(units = kerasConf$nClasses,
                  activation = kerasConf$activation)
    model %>% compile(
      loss = sym(kerasConf$loss),
      optimizer = "adam",
      metrics = kerasConf$metrics
    )
  }) ## end with
  return(model)
}


#' @title Evaluate keras prediction
#' @description Evaluates prediction from keras model using several
#' metrics based on training, validation and test data
#'
#' @importFrom Metrics logLoss
#' @importFrom Metrics accuracy
#' @importFrom keras evaluate
#'
#' @param pred prediction from keras predict
#' @param testScore additional score values
#' @param specList spec with target
#' @param metrics keras metrics (history)
#' @param kerasConf keras config
#'
#' @examples
#' \donttest{
#' ### These examples require an activated Python environment as described in
#' ### Bartz-Beielstein, T., Rehbach, F., Sen, A., and Zaefferer, M.:
#' ### Surrogate Model Based Hyperparameter Tuning for Deep Learning with SPOT,
#' ### June 2021. http://arxiv.org/abs/2105.14625.
#' PYTHON_RETICULATE <- FALSE
#' if(PYTHON_RETICULATE){
#' library(tfdatasets)
#' library(keras)
#' target <- "age"
#' batch_size <- 32
#' prop <- 2/3
#' dfCensus <- getDataCensus(nobs=1000,
#'                           target = target)
#' data <- getGenericTrainValTestData(dfGeneric = dfCensus,
#' prop = prop)
#' specList <- genericDataPrep(data=data, batch_size = batch_size)
#' ## spec test data has 334 elements:
#' str(specList$testGeneric$target)
#' ## simulate test:
#' pred <- runif(length(specList$testGeneric$target))
#' kerasConf <- getKerasConf()
#' simpleModel <- getSimpleKerasModel(specList=specList,
#'                     kerasConf=kerasConf)
#' FLAGS <- list(epochs=16)
#' y <- kerasFit(model=simpleModel,
#'                specList = specList,
#'                FLAGS = FLAGS,
#'                kerasConf = kerasConf)
#'  simpeModel <- y$model
#'  history <- y$history
#' # evaluate on test data
#' pred <- predict(simpleModel, specList$testGeneric)
#' ## in use keras evaluation (test error):
#' testScore <-
#'  keras::evaluate(simpleModel,
#'          tfdatasets::dataset_use_spec(dataset=specList$test_ds_generic,
#'          spec=specList$specGeneric_prep),
#'          verbose = kerasConf$verbose)
#'  kerasEvalPrediction(pred=pred,
#'                      testScore = testScore,
#'                     specList = specList,
#'                     metrics = history$metrics,
#'                     kerasConf = kerasConf
#'                     )
#' }
#' }
#' @export
kerasEvalPrediction <- function(pred,
                                testScore = c(NA, NA),
                                specList,
                                metrics,
                                kerasConf) {
  score <- list()
  ## changed in 1.17.6: use keras model test error (via testScore)
  # score[[1]] <-
  #   Metrics::logLoss(specList$testGeneric$target, pred)
  # score[[2]] <-
  #   Metrics::accuracy(specList$testGeneric$target, as.integer(pred >
  #                                                                       0.5))
  score[[1]] <- unname(testScore[1])
  score[[2]] <- unname(testScore[2])
  ## y: matrix with six entries:
  # trainingLoss,  negTrainingAccuracy,
  # validationLoss,  negValidationAccuracy,
  # testLoss,  negTestAccuracy:
  y <- matrix(
    c(# metrics$loss[length(metrics$loss)],
      # -metrics$binary_accuracy[length(metrics$binary_accuracy)],
      # metrics$val_loss[length(metrics$val_loss)],
      # -metrics$val_binary_accuracy[length(metrics$val_binary_accuracy)],
      metrics[[1]][length(metrics[[1]])],-metrics[[2]][length(metrics[[2]])],
      metrics[[3]][length(metrics[[3]])],-metrics[[4]][length(metrics[[4]])],
      score[[1]],-score[[2]]),
    nrow = 1,
    ncol = 6
  )
  if (kerasConf$verbose > 0) {
    message("funKerasGeneric: y matrix before kerasCompileResult()")
    print(y)
  }
  y <- kerasCompileResult(y = y, kerasConf = kerasConf)
  if (kerasConf$verbose > 0) {
    message("funKerasGeneric: y matrix after kerasCompileResult()")
    print(y)
  }
  return(y)
}

#' @title prepare data frame for progress plot
#'
#' @description converts \code{result} from a \code{\link[SPOT]{spot}} run into the long format.
#'
#' @param modelList ml/dl model (character)
#' @param runNr run number (character)
#' @param directory location of the (non-default, e.g., tuned) parameter file. Note:
#' load result only when directory is specified, otherwise use (only one!) result from the workspace.
#' @param maxY max number of y values. If \code{NULL} then all y values are used.
#' @returns data frame with results:
#' \describe{
#'		\item{\code{x}}{integer representing step}
#'		\item{\code{y}}{corresponding function value at step x.}
#'		\item{\code{name}}{ml/dl model name, e.g., ranger}
#'		\item{\code{size}}{initial design size.}
#'		\item{\code{yInitMin}}{min y value before SMBO is started, based on the
#'		initial design only.}
#' }
#' @examples
#' \donttest{
#' modelList <- list("resDl")
#' runNr <- list("100")
#' result <- resDl100
#' directory <- NULL
#' prepareProgressPlot(modelList,
#'                     runNr,
#'                     directory)
#' }
#' @export
#'
prepareProgressPlot <- function(modelList,
                                runNr,
                                directory = NULL,
                                maxY = NULL) {
  dfRun <-
    data.frame(
      x = NULL,
      y = NULL,
      name = NULL,
      size = NULL,
      yInitMin = NULL
    )
  attr(dfRun, "yInitmin") <- NULL
  for (model in modelList) {
    for (run in runNr) {
      ## load result only when directory is specified,
      ## otherwise use (only one!) result from the workspace.
      if(!is.null(directory)){
        result <- NULL
      fileName <- paste0(directory, "/", model, run, ".RData")
      load(fileName)
      }
      if (is.null(maxY)) {
        maxY <- length(result$y)
      }
      maxY <- min(maxY, length(result$y))
      result$y <- result$y[1:maxY, 1]
      size <- result$control$designControl$size * result$control$designControl$replicates
      dfRun <- rbind(dfRun,
                     data.frame(
                       x = 1:maxY,
                       y = result$y,
                       name = model,
                       size = size,
                       yInitMin = min(result$y[1:size])
                     ))
      attr(dfRun, "yInitmin") <-
        c(attr(dfRun, which = "yInitmin"), min(result$y[1:size]))
    }
  }
  attr(dfRun, "ymin") <- min(dfRun$y)
  return(dfRun)
}

#' @title getGenericTrainValTestData
#'
#' @seealso \code{\link{getKerasConf}}
#' @seealso \code{\link{funKerasGeneric}}
#' @seealso \code{\link{getDataCensus}}
#'
#' @param dfGeneric data, e.g.,  obtained with \code{\link{getDataCensus}}. Default: \code{NULL}.
#' @param prop vector. proportion between train / test and train/val. Default: \code{2/3}. If one
#' value is given, the same proportion will be used for both split. Otherwise, the first
#' entry is used for the test/training split and the second value for the training/validation
#' split. If the second value is 1, the validation set is empty.
#' Given \code{prop = (p1,p2)}, the data will be partitioned as shown in the following two steps:
#'  \describe{
#'		\item{Step 1:}{\code{train1 = p1*data} and \code{test = )(1-p1)*data}}
#'		\item{Step 2:}{\code{train2 = p2*train1 = p2*p1*data} and \code{val = )(1-p2)*train1 = (1-p2)*p1*data}}
#'		 }
#' @note If \code{p2=1}, no validation data will be generated.
#'
#' @return list with training, validation and test data: trainCensus, valCensus, testCensus.
#'
#' @importFrom rsample initial_split
#' @importFrom rsample training
#' @importFrom rsample testing
#' @examples
#' \donttest{
#' ### These examples require an activated Python environment as described in
#' ### Bartz-Beielstein, T., Rehbach, F., Sen, A., and Zaefferer, M.:
#' ### Surrogate Model Based Hyperparameter Tuning for Deep Learning with SPOT,
#' ### June 2021. http://arxiv.org/abs/2105.14625.
#' PYTHON_RETICULATE <- FALSE
#' if(PYTHON_RETICULATE){
#' task.type <- "classif"
#' nobs <- 1e4
#' nfactors <- "high"
#' nnumericals <- "high"
#' cardinality <- "high"
#' data.seed <- 1
#' cachedir <- "oml.cache"
#' target = "age"
#' prop <- 2 / 3
#' dfCensus <- getDataCensus(task.type = task.type,
#' nobs = nobs, nfactors = nfactors,
#' nnumericals = nnumericals, cardinality = cardinality,
#' data.seed = data.seed, cachedir = cachedir,
#' target = target)
#' census <- getGenericTrainValTestData(dfGeneric=dfCensus,
#' prop = prop)
#' ## train data size is 2/3*2/3*10000:
#' dim(census$trainGeneric)
#' }
#' }
#' @export
getGenericTrainValTestData <- function(dfGeneric = NULL,
                                       prop = 0.5) {
  # Handle split proportions
  if (length(prop) == 1) {
    prop <- rep(prop, 2)
  }
  # target variable as double:
  dfGeneric$target <- as.double(dfGeneric$target) - 1
  ## factors as character:
  i <- sapply(dfGeneric, is.factor)
  dfGeneric[i] <- lapply(dfGeneric[i], as.character)
  ## first split between training and testing sets
  if (prop[1] == 1) {
    trainGeneric <- dfGeneric
    testGeneric <- NULL
  } else{
    splitGeneric <- initial_split(dfGeneric, prop = prop[1])
    trainGeneric <- training(splitGeneric)
    testGeneric <- testing(splitGeneric)
  }
  # split the training set into validation and training
  if (prop[2] == 1) {
    valGeneric <- NULL
  } else{
    splitGeneric <- initial_split(trainGeneric, prop = prop[2])
    trainGeneric <- training(splitGeneric)
    valGeneric <- testing(splitGeneric)
  }

  res <- list(
    trainGeneric = trainGeneric,
    valGeneric = valGeneric,
    testGeneric = testGeneric
  )
}

#' @title Select target variable in a data frame
#' @param df data frame
#' @param target character specification of the target variable
#' @return df with entry target
#'
#' @examples
#' df <- data.frame(cbind(x=1:2,
#'                  y=3:4))
#' df <- selectTarget(df=df, target="y")
#'
#' @export
selectTarget <- function(df, target) {
  df$target <- df[[target]]
  df[[target]] <- NULL
  return(df)
}
