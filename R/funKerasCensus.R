#' @title Predict machine learning models on Census data
#'
#' @param x matrix hyperparameter, e.g., result from \code{\link[SPOT]{spot}}
#' Load result data for ml model to get the hyperparamater vector x, e.g.,
#' \code{load("data/resdl11.RData")} and
#' \code{x <- result$xbest} or use default.
#' @param model character ml model, e.g., \code{"kknn"}
#' run: \code{result$xbest}. If \code{NULL}, default parameters will be used.
#' Default: \code{NULL}.
#' @param target target
#' @param task.type class/reg
#' @param nobs number of obsvervations, max: 229,285
#' @param nfactors (character) number of factor variables
#' @param nnumericals (character) number of numerical variables
#' @param cardinality (character) cardinality
#' @param cachedir cachedir
#' @param k number of repeats
#' @param prop split proportion. Default: \code{c(3/5,1)}.
#' @param verbosity verbsity. Default: 0
#'
#' @importFrom mlr train
#' @importFrom mlr getHyperPars
#' @importFrom mlr summarizeColumns
#' @importFrom mlr normalizeFeatures
#' @importFrom mlr createDummyFeatures
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#'
#' @return list of matrices with predictions and true values
#'
#' @export
predMlCensus <- function(x = NULL,
                         model = NULL,
                         target = "age",
                         task.type = "classif",
                         nobs = 1e4,
                         nfactors = "high",
                         nnumericals = "high",
                         cardinality = "high",
                         cachedir = "oml.cache",
                         k = 1,
                         prop = 2 / 3,
                         verbosity = 0) {
  trueY <- NULL # matrix(NA, ncol=k,)
  hatY  <- NULL # matrix(NA, ncol=k,)
  for (i in 1:k) {
    data.seed <- i
    dfCensus <- getDataCensus(
      task.type = task.type,
      nobs = nobs,
      nfactors = nfactors,
      nnumericals = nnumericals,
      cardinality = cardinality,
      data.seed = data.seed,
      cachedir = cachedir,
      target = target
    )
    task <- getMlrTask(dataset = dfCensus,
                       task.type = task.type,
                       data.seed = data.seed)
    # number of features in task:
    nFeatures <- sum(task$task.desc$n.feat)
    cfg <- getModelConf(list(
      task.type = task.type,
      model = model,
      nFeatures = nFeatures
    ))
    # Handle dummy
    ## The model is known, so dummy information is available
    if (cfg$dummy) {
      task <- createDummyFeatures(task)
    }
    rsmpl <- getMlrResample(
      task = task,
      dataset = dfCensus,
      data.seed = data.seed,
      prop = prop
    )
    cfg <- append(cfg, list(task = task,
                            resample = rsmpl))
    cfg$prop <- prop
    # timeout set to NA (because this is the final eval)
    predf <- getPredf(config = cfg,
                      timeout = NA)

    z <- predf(x, data.seed)
    res <- z[[7]][3]$data
    #z[[7]][3]$data$truth
    #z[[7]][3]$data$response
    trueY <-
      cbind(trueY, as.matrix(as.integer(res$truth) - 1, ncol = 1))
    hatY  <- cbind(hatY, as.matrix(as.double(res$response) - 1,
                                   ncol = 1))
  } # end for loop
  return(list(trueY = trueY, hatY = hatY))
}
#' @title Predict deep learning models on Census data
#' @importFrom SPOT transformX
#' @param x matrix with untransformed hyperparameters, e.g., result from \code{\link[SPOT]{spot}}.
#' Hyperparameters will be transformed in predDlCensus with \code{\link[SPOT]{transformX}}
#' and transformations defined in \code{\link{getModelConf}}.
#' @param target target
#' @param task.type class/reg
#' @param nobs number of obsvervations, max: 229,285
#' @param nfactors (character) number of factor variables
#' @param nnumericals (character) number of numerical variables
#' @param cardinality (character) cardinality
#' @param cachedir cache directory
#' @param k number of repeats
#' @param prop vector. proportion between train / test and train/val. Default: \code{2/3}. If one
#' value is given, the same proportion will be used for both splits. Otherwise, the first
#' entry is used for the test/training split and the second value for the training/validation
#' split. If the second value is 1, the validation set is empty.
#' Given \code{prop = (p1,p2)}, the data will be partitioned as shown in the following two steps:
#'  \describe{
#'		\item{Step 1:}{\code{train1 = p1*data} and \code{test = )(1-p1)*data}}
#'		\item{Step 2:}{\code{train2 = p2*train1 = p2*p1*data} and \code{val = )(1-p2)*train1 = (1-p2)*p1*data}}
#'		 }
#' Note: If \code{p2=1}, no validation data will be generated.
#' @param batch_size batch_size. Default: \code{32}.
#' @param verbosity verbosity. Default: 0
#' @return list of matrices with true and predicted values.
#' \describe{
#' \item{\code{trueY}}{true values}
#' \item{\code{hatY}}{predicted values}
#' }
#' @examples
#' \donttest{
#' ### These examples require an activated Python environment as described in
#' ### Bartz-Beielstein, T., Rehbach, F., Sen, A., and Zaefferer, M.:
#' ### Surrogate Model Based Hyperparameter Tuning for Deep Learning with SPOT,
#' ### June 2021. http://arxiv.org/abs/2105.14625.
#' PYTHON_RETICULATE <- FALSE
#' if(PYTHON_RETICULATE){
#' cfg <- getModelConf(list(model="dl"))
#' x <- matrix(cfg$defaults, nrow=1)
#' res <- predDlCensus(x=x, k=2)
#' }
#' }
#' @export
predDlCensus <- function(x = NULL,
                         target = "age",
                         task.type = "classif",
                         nobs = 1e4,
                         nfactors = "high",
                         nnumericals = "high",
                         cardinality = "high",
                         cachedir = "oml.cache",
                         k = 1,
                         prop = 2 / 3,
                         batch_size = 32,
                         verbosity = 0) {
  hatY <- trueY <- NULL # matrix(NA, ncol=k,)
  ## FIXME: Remove after testing:
  # i <- k <- 1
  # x <- NULL
  # target <- "age"
  # task.type <- "classif"
  # nobs <- 1e4
  # nfactors <- "high"
  # nnumericals <- "high"
  # cardinality <- "high"
  # cachedir <- "oml.cache"
  # k <- 1
  # prop <- 2/3
  # batch_size <- 32
  # verbosity <- 1
  # load("~/workspace/book/data/dl45.RData")
  # x <- result$xbest
  ## END FIXME

  kerasConf <- getKerasConf()
  kerasConf$verbose <- verbosity
  kerasConf$returnObject <- "pred"
  cfg <- getModelConf(list(model = "dl"))
  x <- matrix(x, nrow = 1)
  transformFun <- cfg$transformations
  message("predDlCensus(): x before transformX().")
  print(x)
  if (length(transformFun) > 0) {
    x <- transformX(xNat = x, fn = transformFun)
  }
  message("predDlCensus(): x after transformX().")
  print(x)
  for (i in 1:k) {
    data.seed <- i
    dfCensus <- getDataCensus(
      task.type = task.type,
      nobs = nobs,
      nfactors = nfactors,
      nnumericals = nnumericals,
      cardinality = cardinality,
      data.seed = data.seed,
      cachedir = cachedir,
      target = target
    )
    data <-
      getGenericTrainValTestData(dfGeneric = dfCensus, prop = prop)
    specList <- genericDataPrep(data = data, batch_size = batch_size)
    pred <- evalKerasGeneric(x = x,
                             kerasConf = kerasConf,
                             specList = specList)
    #trueY <- cbind(trueY, as.matrix(dataFull$testGeneric$age, ncol = 1))
    trueY <-
      cbind(trueY, as.matrix(pred$trueY, ncol = 1))
    hatY  <-
      cbind(hatY, as.matrix(as.integer(pred$hatY > 0.5),  ncol = 1))
  } # end for loop
  return(list(trueY = trueY, hatY = hatY))
}

#' @title get ml config for keras on census
#'
#' @param target character \code{"age"} or \code{"income_class"}
#' @param model character model name, e.g., \code{"dl"}
#' @param data  data, e.g., from \code{\link{getDataCensus}}
#' @param task.type \code{"classif"}  (character)
#' @param nobs number of observations (numerical), max \code{229285}. Default: \code{1e4}
#' @param nfactors (character), e.g., \code{"high"}
#' @param nnumericals (character), e.g., \code{"high"}
#' @param cardinality (character), e.g., \code{"high"}
#' @param data.seed (numerical) seed
#' @param prop (numerical) split proportion (train, vals,test)
#'
#' @returns cfg (list)
#' @examples
#' \donttest{
#' ### These examples require an activated Python environment as described in
#' ### Bartz-Beielstein, T., Rehbach, F., Sen, A., and Zaefferer, M.:
#' ### Surrogate Model Based Hyperparameter Tuning for Deep Learning with SPOT,
#' ### June 2021. http://arxiv.org/abs/2105.14625.
#' PYTHON_RETICULATE <- FALSE
#' if(PYTHON_RETICULATE){
#' target <- "age"
#' task.type <- "classif"
#' nobs <- 1e2
#' nfactors <- "high"
#' nnumericals <- "high"
#' cardinality <- "high"
#' data.seed <- 1
#' cachedir <- "oml.cache"
#' model <- "ranger"
#'
#' dfCensus <- getDataCensus(
#' task.type = task.type,
#' nobs = nobs,
#' nfactors = nfactors,
#' nnumericals = nnumericals,
#' cardinality = cardinality,
#' data.seed = data.seed,
#' cachedir = cachedir,
#' target = target)
#'
#' cfg <- getMlConfig(
#' target = target,
#' model = model,
#' data = dfCensus,
#' task.type = task.type,
#' nobs = nobs,
#' nfactors = nfactors,
#' nnumericals = nnumericals,
#' cardinality = cardinality,
#' data.seed = data.seed,
#' prop= 2/3)
#' }
#' }
#' @export
getMlConfig <- function(target,
                        model,
                        data,
                        task.type,
                        nobs,
                        nfactors,
                        nnumericals,
                        cardinality,
                        data.seed,
                        prop) {
  task <- getMlrTask(dataset = data,
                     task.type = task.type,
                     data.seed = data.seed)

  # number of features in task:
  nFeatures <- sum(task$task.desc$n.feat)

  cfg <- getModelConf(list(
    task.type = task.type,
    model = model,
    nFeatures = nFeatures
  ))
  # Handle dummy
  ## The model is known, so dummy information is available
  if (cfg$dummy) {
    task <- createDummyFeatures(task)
  }
  rsmpl <- getMlrResample(
    task = task,
    dataset = data,
    data.seed = data.seed,
    prop = prop
  )
  cfg <- append(cfg, list(task = task,
                          resample = rsmpl))
  return(cfg)
}


#' @title evaluate hyperparameter config on census data
#'
#' @param runNr run number (character)
#' @param model ml/dl model (character)
#' @param xbest best value, e.g., "xBestOcba" or "xbest"
#' @param k number of repeats (integer)
#' @param directory location of the (non-default, e.g., tuned) parameter file
#' @param target "age" or "income_class"
#' @param cachedir cache dir
#' @param task.type task type: "classif" or "regression"
#' @param nobs number of observations
#' @param nfactors factors, e.g., "high"
#' @param nnumericals numericals
#' @param cardinality cardinality
#' @param prop proportion. Default: \code{2/3}
#' @param verbosity verbosity level (0 or 1)
#'
#' @examples
#' \donttest{
#' ### These examples require an activated Python environment as described in
#' ### Bartz-Beielstein, T., Rehbach, F., Sen, A., and Zaefferer, M.:
#' ### Surrogate Model Based Hyperparameter Tuning for Deep Learning with SPOT,
#' ### June 2021. http://arxiv.org/abs/2105.14625.
#' PYTHON_RETICULATE <- FALSE
#' if(PYTHON_RETICULATE){
#' ## The following code was used to evaluate the results in the book
#' ## "Hyperparameter Tuning for Machine and Deep Learning with R - A Practical Guide"
#' ## by Bartz, Bartz-Beielstein, Zaefferer, Mersmann:
#' ##
#' modelList <- list("dl", "cvglmnet",  "kknn", "ranger", "rpart" , "svm", "xgboost")
#' runNr <- list("100", "Default")
#' directory <- "../book/data"
#' for (model in modelList){
#'   for (run in runNr){    score <- evalParamCensus(model = model,
#'                                runNr = run,
#'                                directory = directory,
#'                                prop=2/3,
#'                                k=30)
#' fileName <- paste0(directory, "/", model, run, "Evaluation.RData")
#' save(score, file = fileName)
#'  }}
#' }}
#' @export
#'
evalParamCensus <- function(runNr = "00",
                            model = "dl",
                            xbest = "xBestOcba",
                            k = 30,
                            directory = "data",
                            target = "age",
                            cachedir = "oml.cache",
                            task.type = "classif",
                            nobs = 1e4,
                            nfactors = "high",
                            nnumericals = "high",
                            cardinality = "high",
                            prop = 2 / 3,
                            verbosity = 0)
{
  # FIXME: remove after testing:
  # runNr <- "Default"
  # model <- "svm"
  # k <- 2
  # directory <- "data"
  # target <- "age"
  # cachedir <- "oml.cache"
  # task.type <- "classif"
  # nobs <- 1e3
  # nfactors <- "high"
  # nnumericals <- "high"
  # cardinality <- "high"
  # prop <- c(2/3, 1)
  # verbosity <- 1
  # End FIXME

  dfCensus <- getDataCensus(
    task.type = task.type,
    nobs = nobs,
    nfactors = nfactors,
    nnumericals = nnumericals,
    cardinality = cardinality,
    cachedir = cachedir,
    target = target
  )

  task <- getMlrTask(dataset = dfCensus,
                     task.type = task.type,
                     data.seed = 1)


  # number of features in task:
  nFeatures <- sum(task$task.desc$n.feat)

  result <- NULL
  ## get hyperparameter config x:
  if (runNr == "Default") {
    x <- getModelConf(list(
      model = model,
      task.type = task.type,
      nFeatures = nFeatures
    ))$defaults
  } else{
    load(paste0(directory, "/", model, runNr, ".RData"))
    x <- result[[xbest]]
  }
  x <- matrix(x, nrow = 1, ncol = length(x))
  ## dl evaluation:
  if (model == "dl") {
    val <- predDlCensus(
      x = x,
      target = target,
      task.type =  task.type,
      nobs = nobs,
      nfactors = nfactors,
      nnumericals = nnumericals,
      cardinality = cardinality,
      cachedir = cachedir,
      k = k,
      prop = prop,
      verbosity = verbosity
    )
  } else{
    ## ml evaluation:
    val <- predMlCensus(
      x = x,
      k = k,
      model = model,
      task.type = task.type,
      nobs = nobs,
      nfactors = nfactors,
      nnumericals = nnumericals,
      cardinality = cardinality,
      cachedir = cachedir,
      target = target,
      verbosity = verbosity,
      prop = prop[1]
    )
  }
  score <- scorePredictions(val)
  return(score)
}

#' @title prepare data frame for comparisons (boxplots, violin plots)
#'
#' @description converts \code{result} from a \code{\link[SPOT]{spot}} run
#' into the long format for ggplot.
#'
#' @param runNrMl run number (character) of ml models
#' @param runNrDl run number (character) of dl models
#' @param directory location of the (non-default, e.g., tuned) parameter file
#' @param defaultModelList default model list. Default: \code{list("dl", "cvglmnet", "kknn",
#' "ranger", "rpart" ,  "svm", "xgboost")}
#' @param tunedModelList tuned model list. Default: \code{list("dl", "cvglmnet", "kknn",
#' "ranger", "rpart" ,  "svm", "xgboost")}
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
#' ### These examples require an activated Python environment as described in
#' ### Bartz-Beielstein, T., Rehbach, F., Sen, A., and Zaefferer, M.:
#' ### Surrogate Model Based Hyperparameter Tuning for Deep Learning with SPOT,
#' ### June 2021. http://arxiv.org/abs/2105.14625.
#' PYTHON_RETICULATE <- FALSE
#' if(PYTHON_RETICULATE){
#' runNrMl <- list("15")
#' runNrDl <- list("28")
#' directory <- "../book/data"
#' prepareComparisonPlot(runNrMl,
#'                     runNrDl,
#'                     directory)
#' }
#' }
#' @export
#'
prepareComparisonPlot <- function(runNrMl,
                                  runNrDl,
                                  directory,
                                  defaultModelList =
                                    list("dl", "cvglmnet",  "kknn",
                                         "ranger", "rpart" ,  "svm",
                                         "xgboost"),
                                  tunedModelList =
                                    list("dl", "cvglmnet",  "kknn",
                                         "ranger", "rpart" ,  "svm",
                                         "xgboost")) {
  score <- NULL
  dfRun1 <- data.frame(x = NULL, y = NULL, name = NULL)
  dfRun2 <- data.frame(x = NULL, y = NULL, name = NULL)
  dfRun3 <- data.frame(x = NULL, y = NULL, name = NULL)
  dfRun <- data.frame(x = NULL, y = NULL, name = NULL)
  for (model in defaultModelList) {
    load(paste0(directory, "/", model, "DefaultEvaluation.RData"))
    dfRun1 <- rbind(dfRun1,
                    data.frame(
                      x = 1:dim(score)[1],
                      y = score[, 2],
                      name = paste0(model, "D")
                    ))
  }
  mlModelList <-
    list("cvglmnet",  "kknn",   "ranger", "rpart" ,  "svm", "xgboost")
  modelList <-
    tunedModelList[which((tunedModelList %in% mlModelList))]

  for (model in modelList) {
    load(paste0(directory, "/", model, runNrMl, "Evaluation.RData"))
    dfRun2 <- rbind(dfRun2,
                    data.frame(
                      x = 1:dim(score)[1],
                      y = score[, 2],
                      name = paste0(model, "T")
                    ))
  }
  # add dl run
  dlModelList <- list("dl")
  dlModelList <-
    tunedModelList[which((tunedModelList %in% dlModelList))]
  if (length(dlModelList) > 0) {
    for (model in dlModelList) {
      load(paste0(directory, "/", model, runNrDl, "Evaluation.RData"))
      dfRun3 <- rbind(dfRun3,
                      data.frame(
                        x = 1:dim(score)[1],
                        y = score[, 2],
                        name = paste0(model, "T")
                      ))
      dfRun <- rbind(dfRun1, dfRun2, dfRun3)
    }
  }
  else{
    dfRun <- rbind(dfRun1, dfRun2)
  }
  attr(dfRun, "ymin") <- min(dfRun$y)
  return(dfRun)
}
