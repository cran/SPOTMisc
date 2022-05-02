#' @title  Check the validity of input parameters.
#'
#' @description
#' Helper function. Check correct parameter names.
#'
#' @param chars character
#'
#' @return correct characters
#'
#' @export
#' @keywords internal
valid_inputs <- function(chars) {
  is_valid <- NULL
  for (i in 1:length(chars)) {
    ch <- chars[i]
    valid <- FALSE
    if (is.list(.GlobalEnv[[ch]])) {
      if (!is.null(.GlobalEnv[[ch]]$modelFit)) {
        valid <- TRUE
      }
    }
    is_valid <- c(is_valid, valid)
  }
  chars[is_valid]
}

#' @title Get objective function for mlr
#'
#' @description mlrTools
#' This function receives a configuration for a tuning experiment,
#' and returns an objective function to be tuned via SPOT.
#' It basically provides the result from a call to \code{\link[mlr]{resample}}:
#' \code{resample(lrn, task, resample, measures = measures, show.info = FALSE)},
#' with measures defined as \code{mmce} for classification and \code{rmse} for regression, \code{timeboth}, \code{timetrain}, and
#' \code{timepredict}.
#'
#' @details Parameter names are set, parameters are transformed
#' to the actual parameter scale, integer levels are converted
#' to factor levels for categorical parameters, and parameters
#' are set in relation to other parameters.
#'
#' @param config list
#' @param timeout integer, time in seconds after which a model (learner) evaluation will be aborted.
#'
#' @return an objective function that can be optimized via \code{\link[SPOT]{spot}}.
#'
#' @export
#' @importFrom callr r
#' @importFrom mlr mmce
#' @importFrom mlr timeboth
#' @importFrom mlr timetrain
#' @importFrom mlr timepredict
#' @importFrom mlr rmse
#' @importFrom mlr makeLearner
#' @importFrom mlr resample
#'
getObjf <- function(config, timeout = 3600) {
  force(config)
  force(timeout)

  objfun <- function(x, seed) {
    params <- as.list(x)
    ## set parameter names
    names(params) <- config$tunepars

    ## transform to actual parameter scale
    for (i in 1:length(params)) {
      params[[i]] <- config$transformations[[i]](params[[i]])
    }

    ## convert integer levels to factor levels for categorical params
    if (length(config$factorlevels) > 0)
      params <- int2fact(params, config$factorlevels)

    ## set fixed parameters (which are not tuned, but are also not set to default value)
    if (length(config$fixpars) > 0)
      params <- c(params, config$fixpars)

    #print(data.frame(params))

    ## set parameters in relation to other parameters (minbucket relative to minsplit)
    nrel <- length(config$relpars)
    if (nrel > 0) {
      for (i in 1:nrel) {
        params[names(config$relpars)[i]] <-
          with(params, eval(config$relpars[[i]]))
      }
    }

    ## generate the learner
    # print(data.frame(params))
    lrn = makeLearner(config$learner, par.vals = params)

    ## call the model evaluation in a new process via callr
    ## this ensures that the call can be timed-out even when stuck in non-R low-level code
    if (is.na(timeout)) {
      if (config$task$type == "classif") {
        measures <- list(mmce, timeboth, timetrain, timepredict)
      } else if (config$task$type == "regr") {
        measures = list(mlr::rmse, timeboth, timetrain, timepredict)
      }
      set.seed(seed)
      res <-
        try(resample(lrn,
                     config$task,
                     config$resample,
                     measures = measures,
                     show.info = FALSE))
    } else{
      res <- try(r(
        # callr::r
        function(lrn, task, resample, seed) {
          #require(mlr)
          if (task$type == "classif") {
            measures <- list(mmce, timeboth, timetrain, timepredict)
          } else if (task$type == "regr") {
            measures = list(mlr::rmse, timeboth, timetrain, timepredict)
          }
          set.seed(seed)
          resample(lrn,
                   task,
                   resample,
                   measures = measures,
                   show.info = FALSE)
        },
        timeout = timeout,
        args = list(
          lrn = lrn,
          task = config$task,
          resample = config$resample,
          seed = seed
        ),
        poll_connection = FALSE,
        package = "mlr"
      ))
    }

    timestamp <- as.numeric(Sys.time())

    ## determine a value to return in case of errors or timeouts.
    if (class(res)[1] == "try-error") {
      if (config$task$type == "classif") {
        #(based on guessing the most frequent class)
        lrn <-  makeLearner("classif.rpart", maxdepth = 1)
        measures <- list(mmce, timetrain, timepredict)
      } else if (config$task$type == "regr") {
        #(based on guessing the mean of the observations)
        lrn <-  makeLearner("regr.rpart", maxdepth = 1)
        measures = list(mlr::rmse, timetrain, timepredict)
      }
      res = resample(lrn, config$task, config$resample, measures = measures)
      # print(data.frame(res$aggr))
      return(matrix(c(
        res$aggr[1], timeout, timeout, timeout, timestamp
      ), 1))
    } else{
      # print(data.frame(res$aggr))
      return(matrix(c(res$aggr, timestamp), 1))
    }
  }
  force(objfun)
  objvecf <- function(x, seed) {
    res <- NULL
    for (i in 1:nrow(x))
      res <- rbind(res, objfun(x[i, , drop = FALSE], seed[i]))
    return(res)
  }
}


#' @title Generate an mlr task from Census KDD data set (+variation)
#'
#' @description Prepares the Census data set for mlr.
#' Performs imputation via: \code{factor = imputeMode()},
#' \code{integer = imputeMedian()},
#' \code{numeric = imputeMean()}
#'
#' @seealso \code{\link{getDataCensus}}
#'
#' @param dataset census data set
#' @param task.type character, either "classif" or "regr".
#' @param data.seed seed
#'
#' @return an mlr task with the respective data set. Generated with
#' \code{\link[mlr]{makeClassifTask}} or
#' \code{\link[mlr]{makeRegrTask}} for
#' classification and regression repectively.
#'
#' @import OpenML
#' @importFrom  mlr makeFixedHoldoutInstance
#' @importFrom mlr getTaskSize
#' @importFrom mlr makeClassifTask
#' @importFrom mlr makeRegrTask
#' @importFrom mlr getTaskSize
#' @importFrom mlr impute
#' @importFrom mlr imputeMode
#' @importFrom mlr imputeMedian
#' @importFrom mlr imputeMean
#'
#' @examples
#' \donttest{
#' ## Example downloads OpenML data, might take some time:
#' x <- getDataCensus(
#' task.type="classif",
#' nobs = 1e3,
#' nfactors = "high",
#' nnumericals = "high",
#' cardinality = "high",
#' data.seed=1,
#' cachedir= "oml.cache",
#' target = "age")
#'
#' taskdata <- getMlrTask(
#' dataset = x,
#' task.type = "classif",
#' data.seed = 1)
#' }
#' @export
getMlrTask <- function(dataset,
                       task.type = "classif",
                       data.seed = 1) {
  target <- "target"
  task.nobservations <- nrow(dataset)
  task.nfeatures <- ncol(dataset)
  task.numericals <- lapply(dataset, class) != "factor"
  task.numericals[target] <- FALSE
  task.factors <- lapply(dataset, class) == "factor"
  task.factors[target] <- FALSE
  task.nnumericals <- sum(task.numericals)
  task.nfactors <- sum(task.factors)
  task.nlevels <-
    as.numeric(lapply(dataset, function(x)
      length(unique(x))))
  task.nlevels[!task.factors] <- 0
  task.nlevels.max <- max(task.nlevels)


  if (task.type == "classif")
    task <- makeClassifTask(data = dataset, target = target)
  else if (task.type == "regr")
    task <- makeRegrTask(data = dataset, target = target)

  ## Impute missing values.
  task <- impute(task,
                 classes = list(
                   factor = imputeMode(),
                   integer = imputeMedian(),
                   numeric = imputeMean()
                 ))$task
  return(task)
}


#' @title Generate a fixed holdout instance for resampling
#'
#' @description Determines test/train split and applies
#' \code{\link[mlr]{makeFixedHoldoutInstance}}
#'
#' @param task mlr task
#' @param dataset e.g., census data set
#' @param data.seed seed
#' @param prop proportion, e.g., 2/3 take 2/3 of the data for training and 1/3
#' for test
#'
#' @return list: an mlr resample generated
#' with \code{\link[mlr]{makeFixedHoldoutInstance}}
#'
#' @seealso \code{\link{getMlrTask}}
#'
#' @importFrom  mlr makeFixedHoldoutInstance
#' @importFrom mlr getTaskSize
#'
#' @examples
#' \donttest{
#' ## Example downloads OpenML data, might take some time:
#' dataset <- getDataCensus(
#' task.type="classif",
#' nobs = 1e3,
#' nfactors = "high",
#' nnumericals = "high",
#' cardinality = "high",
#' data.seed=1,
#' cachedir= "oml.cache",
#' target = "age")
#'
#' taskdata <- getMlrTask(dataset,
#' task.type = "classif",
#' data.seed = 1)
#'
#' rsmpl <- getMlrResample(task=taskdata,
#' dataset = dataset,
#' data.seed = 1,
#' prop = 2/3)
#' }
#' @export
#'
getMlrResample <- function(task,
                           dataset,
                           data.seed = 1,
                           prop = NULL) {
  set.seed(data.seed)
  train.id <- sample(1:getTaskSize(task),
                     size = getTaskSize(task) * prop,
                     replace = FALSE)
  test.id <- (1:getTaskSize(task))[-train.id]
  rsmpl = makeFixedHoldoutInstance(train.id,
                                   test.id,
                                   getTaskSize(task))
  return(rsmpl)
}



#' @title Make mlr learner from conf and hyperparameter vector
#'
#' @description calls makelearner
#'
#' @param cfg configuration list
#' @param x hyperparameter vector
#'
#' @importFrom mlr makeLearner
#'
#' @returns mlr learner
#'
#' @export
makeLearnerFromHyperparameters <- function(x = NULL,
                                           cfg = NULL){
# Generate the learner with defaults
if (is.null(x)) {
  #lrn <- makeLearner("classif.kknn")
  lrn <- makeLearner(cfg$learner)
} else{
  # Generate the learner with tuned
  # lrn <- makeLearner("classif.kknn", k=x[1], distance=2^x[2])
  xt <- x
  for (i in 1:length(x)) {
    xt[i] <- (cfg$transformations[[i]](x[i]))
  }

  argString <- paste0("makeLearner(", "'", cfg$learner, "'", ",")
  for (i in 1:(length(cfg$tunepars) - 1)) {
    argString <- paste0(argString,
                        cfg$tunepars[i], "=xt[", i, "], ")
  }
  argString <- paste0(argString,
                      cfg$tunepars[i + 1], "=xt[", i + 1, "])")
  #argString

  # Generate the learner with tuned
  lrn <- eval(parse(text = argString))
}
  return(lrn)
}

#' @title Get predictions from mlr
#'
#' @description mlrTools
#' This function receives a configuration for a tuning experiment,
#' and returns predicted values.
#'
#' @param config list
#' @param timeout integer, time in seconds after which a model (learner) evaluation will be aborted.
#'
#' @return an prediction function that can be called via \code{\link[SPOT]{spot}}.
#' It basically provides the result from a call to \code{\link[mlr]{resample}}:
#' \code{resample(lrn, task, resample, measures = measures, show.info = FALSE)},
#'
#' @export
#' @importFrom callr r
#' @importFrom mlr mmce
#' @importFrom mlr timeboth
#' @importFrom mlr timetrain
#' @importFrom mlr timepredict
#' @importFrom mlr rmse
#' @importFrom mlr makeLearner
#' @importFrom mlr resample
#' @importFrom mlr acc
#' @importFrom mlr getMlrOptions
#' @importFrom mlr holdout
#'
getPredf <- function(config, timeout = 3600) {
  force(config)
  force(timeout)

  predfun <- function(x=NULL,
                      seed) {
    params <- as.list(x)
    ## set parameter names
    names(params) <- config$tunepars

    ## transform to actual parameter scale
    for (i in 1:length(params)) {
      params[[i]] <- config$transformations[[i]](params[[i]])
    }

    ## convert integer levels to factor levels for categorical params
    if (length(config$factorlevels) > 0)
      params <- int2fact(params, config$factorlevels)

    ## set fixed parameters (which are not tuned, but are also not set to default value)
    if (length(config$fixpars) > 0)
      params <- c(params, config$fixpars)

    #print(data.frame(params))

    ## set parameters in relation to other parameters (minbucket relative to minsplit)
    nrel <- length(config$relpars)
    if (nrel > 0) {
      for (i in 1:nrel) {
        params[names(config$relpars)[i]] <-
          with(params, eval(config$relpars[[i]]))
      }
    }

    ## generate the learner
    # print(data.frame(params))
    if(!is.na(x[1])){
        lrn = makeLearner(config$learner, par.vals = params)
    } else{
      ## default (no params)
      lrn = makeLearner(config$learner)
    }

    ## call the model evaluation in a new process via callr
    ## this ensures that the call can be timed-out even when stuck in non-R low-level code
    ## if (is.na(timeout)) {
      if (config$task$type == "classif") {
        measures <- list(mmce, acc, timeboth, timetrain, timepredict)
      } else if (config$task$type == "regr") {
        measures = list(mlr::rmse, timeboth, timetrain, timepredict)
      }
      set.seed(seed)
      # getMlrOptions()
      # res <-
      #   resample(learner = lrn,
      #           task = config$task,
      #           resampling =  config$resample,
      #            measures = measures,
      #            show.info = TRUE,
      #                keep.pred = TRUE)

      res <- holdout(
        learner=lrn,
        task = config$task,
        split = config$prop,
        stratify = FALSE,
        measures = measures,
        models = FALSE,
        keep.pred = TRUE,
        show.info = FALSE
      )
         # } else{
    #   res <- try(r(
    #     # callr::r
    #     function(lrn, task, resample, seed) {
    #       #require(mlr)
    #       if (task$type == "classif") {
    #         measures <- list(mmce, timeboth, timetrain, timepredict)
    #       } else if (task$type == "regr") {
    #         measures = list(rmse, timeboth, timetrain, timepredict)
    #       }
    #       set.seed(seed)
    #       resample(lrn,
    #                task,
    #                resample,
    #                measures = measures,
    #                show.info = FALSE)
    #     },
    #     timeout = timeout,
    #     args = list(
    #       lrn = lrn,
    #       task = config$task,
    #       resample = config$resample,
    #       seed = seed
    #     ),
    #     poll_connection = FALSE,
    #     package = "mlr"
    #   ))
    # }

    # timestamp <- as.numeric(Sys.time())

    ## determine a value to return in case of errors or timeouts.
    # if (class(res)[1] == "try-error") {
    #   if (config$task$type == "classif") {
    #     #(based on guessing the most frequent class)
    #     lrn <-  makeLearner("classif.rpart", maxdepth = 1)
    #     measures <- list(mmce, timetrain, timepredict)
    #   } else if (config$task$type == "regr") {
    #     #(based on guessing the mean of the observations)
    #     lrn <-  makeLearner("regr.rpart", maxdepth = 1)
    #     measures = list(rmse, timetrain, timepredict)
    #   }
    #   res = resample(lrn, config$task, config$resample, measures = measures)
    #   # print(data.frame(res$aggr))
    #   return(matrix(c(
    #     res$aggr[1], timeout, timeout, timeout, timestamp
    #   ), 1))
    # } else{
      #print(data.frame(res$aggr))
      #return(matrix(c(res$aggr, timestamp), 1))
      print(res)
      return(res)
    #}
  }
  force(predfun)
  objvecf <- function(x=NULL,
                      seed) {
    # res <- NULL
    # for (i in 1:nrow(x))
    # res <- rbind(res, predfun(x[i, , drop = FALSE], seed[i]))
    res <- predfun(x, seed)
    return(res)
  }
}





#' #' @title Predict machine learning models on Census data (deprecated)
#' #'
#' #' @param x matrix hyperparameter, e.g., result from \code{\link[SPOT]{spot}}
#' #' @param model character ml model, e.g., \code{"kknn"}
#' #' run: \code{result$xbest}. If \code{NULL}, default parameters will be used.
#' #' Default: \code{NULL}.
#' #' @param target target
#' #' @param task.type class/reg
#' #' @param nobs number of obsvervations, max: 229,285
#' #' @param nfactors (character) number of factor variables
#' #' @param nnumericals (character) number of numerical variables
#' #' @param cardinality (character) cardinality
#' #' @param cachedir cachedir ("oml.cache")
#' #' @param k number of repeats
#' #' @param prop split proportion. Default: \code{c(3/5,1)}.
#' #' @param verbosity verbsity. Default: 0
#' #'
#' #' @importFrom mlr train
#' #' @importFrom mlr getHyperPars
#' #' @importFrom mlr summarizeColumns
#' #' @importFrom mlr normalizeFeatures
#' #' @importFrom mlr createDummyFeatures
#' #' @importFrom dplyr bind_rows
#' #' @importFrom dplyr mutate
#' #' @importFrom dplyr select
#' #' @importFrom dplyr filter
#' #'
#' #' @return list of matrices with predictions and true values
#' #'
#' #' @export
#' predMlCensusDeprecated <- function(x = NULL,
#'                          model = NULL,
#'                          target = NULL,
#'                          task.type = "classif",
#'                          nobs = 1e4,
#'                          nfactors = "high",
#'                          nnumericals = "high",
#'                          cardinality = "high",
#'                          cachedir = "oml.cache",
#'                          k = 1,
#'                          prop = c(3/5, 1),
#'                          verbosity = 0) {
#'
#'   dataset <- NULL
#'   trueY <- NULL # matrix(NA, ncol=k,)
#'   hatY  <- NULL # matrix(NA, ncol=k,)
#'   for (i in 1:k) {
#'
#'     data.seed <- i
#'
#'     # dataOML <- getDataCensus(
#'     #   task.type = task.type,
#'     #   nobs = nobs,
#'     #   nfactors = nfactors,
#'     #   nnumericals = nnumericals,
#'     #   cardinality = cardinality,
#'     #   data.seed = data.seed,
#'     #   cachedir = cachedir,
#'     #   target = target
#'     # )
#'
#'     # FIXME BEGIN
#'     # library("dplyr")
#'     # library("OpenML")
#'     # library("mlr")
#'     # library("plotly")
#'     # library("SPOT")
#'     # library("SPOTMisc")
#'     # library("farff")
#'     # if (packageVersion("SPOT") < "2.9.0") message("Please update 'SPOT'")
#'     # if (packageVersion("SPOTMisc") < "1.10.0") message("Please update 'SPOTMisc'")
#'     #
#'     # x <- NULL
#'     # model <- "xgboost"
#'     # model <- "kknn"
#'     # model <- "rpart"
#'     # target <- "age"
#'     # task.type <- "classif"
#'     # nobs <- 1e4
#'     # nfactors <- "high"
#'     # nnumericals <- "high"
#'     # cardinality <- "high"
#'     # cachedir <- "oml.cache"
#'     # k <- 1
#'     # prop <- c(3/5, 1)
#'     # data.seed <- 1
#'     # verbosity <- 1
#'     # dataset <- NULL
#'     # trueY <- NULL # matrix(NA, ncol=k,)
#'     # hatY  <- NULL # matrix(NA, ncol=k,)
#'     # FIXME END
#'
#'     dataOML <- getDataCensus(task.type = task.type,
#'                              nobs = nobs,
#'                              nfactors = nfactors,
#'                              nnumericals = nnumericals,
#'                              cardinality = cardinality,
#'                              data.seed= data.seed,
#'                              cachedir = cachedir,
#'                              target = target)
#'     if(verbosity>0) summarizeColumns(dataOML)
#'
#'     # get trainCensus/valCensus/testCensus data
#'     data <- getCensusTrainValTestData(
#'       dfCensus = dataOML,
#'       task.type = task.type,
#'       nobs = nobs,
#'       nfactors = nfactors,
#'       nnumericals = nnumericals,
#'       cardinality = cardinality,
#'       data.seed = data.seed,
#'       prop = prop, # 4 / 5,
#'       cachedir = cachedir,
#'       DL = FALSE,
#'       target = target)
#'
#'
#'       train_orig <- data$trainCensus
#'       test_orig <- data$testCensus
#'
#'       ## recombine for easy data prep:
#'       train <- train_orig %>%
#'         dplyr::mutate(dataset = "train")
#'       test <- test_orig %>%
#'         dplyr::mutate(dataset = "test")
#'       combined <- dplyr::bind_rows(train, test)
#'
#'       # combined is similar to dataOML, but has one new column: dataset (train/test)
#'       if(verbosity>0) summarizeColumns(combined)
#'
#'       # Impute missing values by field type
#'       imp <- impute(
#'         combined,
#'         classes = list(
#'           factor = imputeMode(),
#'           integer = imputeMean(),
#'           numeric = imputeMean()
#'         )
#'       )
#'       combined <- imp$data
#'
#'       # Show column summary
#'       if(verbosity>0) summarizeColumns(combined)
#'
#'       # normalize features
#'       # combined <- normalizeFeatures(combined, target = target)
#'       # if(verbosity>0) summarizeColumns(combined)
#'
#'       # dummy encoding:
#'       if(model=="xgboost"){
#'       factorCols <- colnames(combined)[sapply(combined, is.factor)]
#'       factorCols <-  factorCols[!factorCols %in% target]
#'
#'       combined <- createDummyFeatures(
#'         combined, target = target,
#'         cols = factorCols
#'       )
#'
#'       if(verbosity>0) summarizeColumns(combined)
#'       }
#'
#'       train <- combined %>%
#'         dplyr::filter(dataset == "train") %>%
#'         dplyr::select(-dataset)
#'
#'       test <- combined %>%
#'         dplyr::filter(dataset == "test") %>%
#'         dplyr::select(-dataset)
#'
#'       trainTask <- makeClassifTask(data = train, target = target, positive = "TRUE")
#'       testTask <- makeClassifTask(data = test, target = target)
#'
#'       cfg <- getModelConf(
#'         task.type = task.type,
#'         model = model,
#'         nFeatures = sum(testTask$task.desc$n.feat))
#'       if(verbosity >0) print(cfg)
#'
#'       lrn <- makeLearnerFromHyperparameters(x=x,
#'                                             cfg=cfg)
#'       if(verbosity >0) print(getHyperPars(lrn))
#'
#'       # Create a model
#'       model <- train(lrn, task = testTask)
#'
#'       newdata.pred <- predict(model, testTask)
#'       # newdata.pred$data$truth
#'       # newdata.pred$data$response
#'
#'     trueY <- cbind(trueY, as.matrix(as.integer(newdata.pred$data$truth) -1, ncol = 1))
#'     hatY <- cbind(hatY, as.matrix(as.double(newdata.pred$data$response) - 1,
#'                                   ncol = 1))
#'
#'   } # end for loop
#'   return(list(trueY = trueY, hatY = hatY))
#' }



