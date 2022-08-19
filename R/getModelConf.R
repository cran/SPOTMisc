#' @title Get model configuration
#'
#' @description Configure machine and deep learning models
#' @param modelArgs list with information about model, active variables etc. Note:
#' \code{argList} will replace the other arguments. Use \code{argList$model} instead
#' of \code{model} etc.
#' @param model machine or deep learning model (character). One of the following:
#' \describe{
#'		\item{\code{"cvglmnet"}}{glm net.}
#'		\item{\code{"kknn"}}{nearest neighbour.}
#'		\item{\code{"ranger"}}{random forest.}
#'		\item{\code{"rpart"}}{recursive partitioning and  regression trees, \code{\link[rpart]{rpart}}}
#'		\item{\code{"svm"}}{support vector machines.}
#'		\item{\code{"xgboost"}}{gradient boosting, \code{\link[xgboost]{xgb.train}}.}
#'		\item{\code{"dl"}}{deep learning: dense network.}
#'		\item{\code{"cnn"}}{deep learning: convolutionary network}.}
#' @param task.type character, either \code{"classif"} or \code{"regr"}.
#' @param nFeatures number of features, e.g., \code{sum(task$task.desc$n.feat)}
#' @param active vector of activated tunepars, e.g., \code{c("minsplit", "maxdepth")}
#' for model \code{"rpart"}
#'
#' @return Returns returns a list of the
#' machine learning model configuration and corresponding hyperparameters:
#' \describe{
#'		\item{\code{learner}}{character: combination of task.type and model name.}
#'		\item{\code{lower}}{vector of lower bounds.}
#'		\item{\code{upper}}{vector of upper bounds.}
#'		\item{\code{fixpars}}{list of fixed parameters.}
#'		\item{\code{factorlevels}}{list of factor levels.}
#'		\item{\code{transformations}}{vector of transformations.}
#'		\item{\code{dummy}}{logical. Use dummy encoding, e.g., \code{\link[xgboost]{xgb.train}}}
#'		\item{\code{relpars}}{list of relative hyperparameters.}
#' }
#'
#' @examples
#' # Get hyperparameter names and their defaults for fitting a
#' # (recursive partitioning and  regression trees) model:
#' modelArgs <- list(model = "rpart")
#' cfg <- getModelConf(modelArgs)
#' cfg$tunepars
#' cfg$defaults
#' ## do not use anymore:
#' cfg <- getModelConf(model="rpart")
#' cfg$tunepars
#' cfg$defaults
#' modelArgs <- list(model="rpart", active = c("minsplit", "maxdepth"))
#' cfgAct <- getModelConf(modelArgs)
#' cfgAct$tunepars
#' cfgAct$defaults
#'
#' @export
#'
getModelConf <- function(modelArgs=NULL,
                         model,
                         task.type = NULL,
                         nFeatures = NULL,
                         active = NULL){

  if(is.list(modelArgs)){
  a0 <- list(model=NULL,
             task.type = NULL,
             nFeatures = NULL,
             active = NULL)
  a0[names(modelArgs)] <- modelArgs
  modelArgs <- a0
  rm(a0)
  ## unlist modelArgs list:
  model <- modelArgs$model
  task.type <- modelArgs$task.type
  nFeatures <- modelArgs$nFeatures
  active = modelArgs$active
  }

  learner <- tunepars <- lower <- upper <- type <- fixpars <- NULL
  factorlevels <- transformations <- dummy <-  relpars <- NULL

  # number of features in task:
  # nFeatures <- sum(task$task.desc$n.feat)

  ## specify learner rpart
  if(model=="rpart"){
    learner <- paste(task.type,"rpart",sep=".")
    tunepars <- c("minsplit","minbucket","cp","maxdepth")

    ## Get Actual Defaults, but without transformation
    #allpars <- getParamSet(learner)
    #pardefaults <- getDefaults(allpars,include.null = TRUE)
    #whichpars <- names(pardefaults) %in% tunepars
    #pardefaults <- pardefaults[whichpars]
    #pardefaults #note: different sorting.
    ## Hardcoded instead:
    defaults <- c(20, 1/3, -2, 30)
    ## Note: default in mlr is NULL for minbucket,
    ## which should lead to rpart default of minbucket = minsplit/3
    ##
    ## / Defaults

    lower <- c(1, ## Prob19a
               0.1, ## Prob19a
               -10, ## slightly larger than Prob19a who allow -Inf, i.e., zero on actual scale
               1) ## Prob19a
    upper <- c(300, ## larger than Prob19a (60)
               0.5, ## a bit larger than Prob19a  (60)
               0, ##Prob19a
               30) ## Prob19a, maximum in rpart
    type <-  c("integer","numeric","numeric","integer")
    fixpars <- list()
    factorlevels <- list()
    transformations <- c(trans_id,
                         trans_id,
                         trans_10pow, ##prob19a use trans_id for cp.
                         trans_id)
    dummy <- FALSE
    relpars <- list(minbucket=expression(round(max(minsplit*minbucket,1))))
  }

  ## specify learner kknn
  if(model=="kknn"){
    learner <- paste(task.type,"kknn",sep=".")
    tunepars <- c("k","distance")

    ## Get Actual Defaults, but without transformation
    #allpars <- getParamSet(learner)
    #pardefaults <- getDefaults(allpars,include.null = TRUE)
    #whichpars <- names(pardefaults) %in% tunepars
    #pardefaults <- pardefaults[whichpars]
    #pardefaults #note: different sorting.
    ## Hardcoded instead:
    defaults <- c(7,
                 log10(2))
    ##
    ## / Defaults


    lower <- c(1, ## Prob19a
               -1) ##
    upper <- c(30,  ## Prob19a
               2) ##
    type <-  c("integer","numeric")
    fixpars <- list()
    factorlevels <- list()
    transformations <- c(trans_id,
                         trans_10pow) ##?
    dummy <- FALSE
    relpars <- list()
  }

  ## specify learner EN
  if(model=="cvglmnet"){
    learner <- paste(task.type,"cvglmnet",sep=".")
    tunepars <- c("alpha","thresh")

    ## Get Actual Defaults, but without transformation
    #allpars <- getParamSet(learner)
    #pardefaults <- getDefaults(allpars,include.null = TRUE)
    #whichpars <- names(pardefaults) %in% tunepars
    #pardefaults <- pardefaults[whichpars]
    #pardefaults #note: different sorting.
    ## Hardcoded instead:
    defaults <- c(1,-7)
    ##
    ## / Defaults

    lower <- c(0, ## Prob19a
               -8) ##
    upper <- c(1, ## Prob19a
               -1) ##
    type <-  c("numeric","numeric")
    fixpars <- list()
    factorlevels <- list()
    transformations <- c(trans_id, ## Prob19a
                         trans_10pow) ## this assigns more relevance to lower values.
    dummy <- FALSE
    relpars <- list()
  }

  ## specify learner RF
  if(model=="ranger"){
    learner <- paste(task.type,"ranger",sep=".")
    tunepars <- c("num.trees","mtry","sample.fraction","replace","respect.unordered.factors")

    ## Get Actual Defaults, but without transformation
    #allpars <- getParamSet(learner)
    #pardefaults <- getDefaults(allpars,include.null = TRUE)
    #whichpars <- names(pardefaults) %in% tunepars
    #pardefaults <- pardefaults[whichpars]
    #pardefaults #note: different sorting.
    ## Hardcoded instead:
    defaults <- c(log(500,2),
                 floor(sqrt(nFeatures)),
                 1, # note: only default for replace=TRUE, else sample.fraction=0.632
                 2, #=>TRUE
                 1) #=>ignore
    ##
    ## / Defaults


    lower <- c(0, ## => 1, Prob19a
               1, ## minimum, Prob19a
               0.1, ## Prob19a
               1, ## => FALSE
               1) ## => ignore
    upper <- c(11, ## => 2048 (very similar to Prob19a)
               nFeatures, ## Prob19a but on original scale.
               1, ## Prob19a
               2, ## => TRUE
               2) ## => order
    type <-  c("numeric","integer","numeric","factor","factor")
    fixpars <- list(num.threads=1)
    factorlevels <- list(respect.unordered.factors=c("ignore","order","partition"),
                         ## Note: partition not used. extreme run time
                         replace=c(FALSE,TRUE)
    )
    transformations <- c(trans_2pow_round, ## deviates from Prob19a.
                         ## Reasoning: increasing number of trees leads to decreasing change per unit.
                         trans_id,
                         trans_id,
                         trans_id,
                         trans_id)
    dummy <- FALSE
    relpars <- list()
  }

  ## specify learner svm
  if(model=="svm"){

    learner <- paste(task.type,"svm",sep=".")
    if(task.type=="classif"){
      tunepars <- c("kernel","cost","gamma","coef0")

      ## Get Actual Defaults, but without transformation
      #allpars <- getParamSet(learner)
      #pardefaults <- getDefaults(allpars,include.null = TRUE)
      #whichpars <- names(pardefaults) %in% tunepars
      #pardefaults <- pardefaults[whichpars]
      #pardefaults #note: different sorting.
      ## Hardcoded instead:
      defaults <- c(
        1, #radial
        0,
        log(1/nFeatures,2),
        0
      )
      ##
      ## / Defaults


      #"degree" only with poly kernel
      lower <-    c(1,   ## radial
                    -10, ## Prob19a
                    -10, ## Prob19a
                    -1)  ## Rijn18a
      upper <-    c(2,  ## sigmoid
                    10, ## Prob19a
                    10, ## Prob19a
                    1)  ## Rijn18a
      type <-  c("factor","numeric","numeric","numeric")
      #factorlevels <- list(kernel= c("linear","polynomial","radial","sigmoid"))
      ## reduce to sigmoid and radial, due to shorter runtimes.
      factorlevels <- list(kernel= c("radial","sigmoid"))
      transformations <- c(trans_id,
                           trans_2pow, ## Prob19a
                           trans_2pow, ## Prob19a
                           trans_id)
    }else if(task.type=="regr"){
      tunepars <- c("kernel","cost","gamma","coef0","epsilon")

      ## Get Actual Defaults, but without transformation
      #allpars <- getParamSet(learner)
      #pardefaults <- getDefaults(allpars,include.null = TRUE)
      #whichpars <- names(pardefaults) %in% tunepars
      #pardefaults <- pardefaults[whichpars]
      #pardefaults #note: different sorting.
      ## Hardcoded instead:
      defaults <- c(
        1, #radial
        0, #see: helpLearnerParam(learner, param = "gamma")
        log(1/nFeatures,2),
        0,
        -1 #see: helpLearnerParam(learner, param = "epsilon")
      )
      ##
      ## / Defaults


      lower <-    c(1,   ## radial
                    -10, ## Prob19a
                    -10, ## Prob19a
                    -1,  ## Rijn18a
                    -8)
      #upper <-    c(4, 10,  10,  5, 1)
      upper <-    c(2,  ## sigmoid
                    10, ## Prob19a
                    10, ## Prob19a
                    1,  ## Rijn18a
                    0)
      type <-  c("factor","numeric","numeric","numeric","numeric")
      #factorlevels <- list(kernel= c("linear","polynomial","radial","sigmoid"))
      ## reduce to sigmoid and radial, due to shorter runtimes.
      factorlevels <- list(kernel= c("radial","sigmoid"))
      transformations <- c(trans_id,
                           trans_2pow, ## Prob19a
                           trans_2pow, ## Prob19a
                           trans_id,
                           trans_10pow
      )
    }
    fixpars <- list()
    dummy <- FALSE
    relpars <- list()
  }

  ## specify learner xgb
  if(model=="xgboost"){
    learner <- paste(task.type,"xgboost",sep=".")
    tunepars <- c("nrounds","eta","lambda","alpha","subsample","colsample_bytree","gamma","max_depth","min_child_weight")

    ## Get Actual Defaults, but without transformation
    # allpars <- getParamSet(learner)
    # pardefaults <- getDefaults(allpars,include.null = TRUE)
    # whichpars <- names(pardefaults) %in% tunepars
    # pardefaults <- pardefaults[whichpars]
    # pardefaults #note: different sorting.
    ## Hardcoded instead:
    defaults <-  c(0,  ## NOTE: xgboost itself has no default nrounds, but mlr chooses one, which is poor.
                  log2(0.3),
                  0,
                  -10, # untransformed default is zero. invalid. set to lower bound instead.
                  1,
                  1,
                  -10, # untransformed default is zero. invalid. set to lower bound instead.
                  6,
                  0)
    ##
    ## / Defaults

    lower <- c(0,  ## reasonable minimum.
               -10, ## Prob19a
               -10, ## Prob19a
               -10, ## Prob19a
               0.1, ## Prob19a
               1/nFeatures,
               ## slight deviation from Prob19a.
               ## reason: 0 makes no sense. At least one feature should be chosen via colsample.
               -10, ## etwas kleiner als Thom18a
               1,   ## Prob19a
               0)   ## Prob19a
    upper <- c(11,  ## set similar as random forest. (which is less than Prob19a used: 5000)
               0,  ## Prob19a
               10,  ## Prob19a
               10,  ## Prob19a
               1,   ## Prob19a
               1,   ## Prob19a
               10,  ## etwas größer als Thom18a
               15,  ## Prob19a
               7) ## Prob19a
    type <-  c("numeric","numeric","numeric","numeric","numeric","numeric","numeric","integer","numeric")
    if(task.type=="classif"){
      fixpars <- list(eval_metric="logloss",# suppress warning given when default metric is used.
                      nthread=1) #one thread, not parallel
    }else{
      fixpars <- list(eval_metric="rmse",# suppress warning given when default metric is used.
                      nthread=1) #one thread, not parallel
    }
    factorlevels <- list()
    transformations <- c(trans_2pow_round, ## differs from Prob19a
                         trans_2pow, ## Prob19a
                         trans_2pow, ## Prob19a
                         trans_2pow, ## Prob19a
                         trans_id,   ## Prob19a
                         trans_id,   ## Prob19a
                         trans_2pow, ## Thom18a
                         trans_id,   ## Prob19a
                         trans_2pow) ## Prob19a
    dummy <- TRUE
    ## TODO/Note: number of features will be changed. consider when setting bounds. problem for colsample?
    relpars <- list()
  }

  ## specify learner deep learning (dense network)
  if(model=="dl"){
    learner <- NULL
    tunepars <- c("dropout",
                  "dropoutfact",
                  "units",
                  "unitsfact",
                  "learning_rate",
                  "epochs",
                  "beta_1",
                  "beta_2",
                  "layers",
                  "epsilon",
                  "optimizer")
    lower <-   c(0,  0,  0, 0.25, 1e-6, 3, 0.9,  0.99,   1, 1e-9, 1)
    defaults <- c(0,  0,  5,  0.5, 1e-3, 4, 0.9,  0.999,  1, 1e-7, 5)
    upper <-   c(0.4, 0.5,  5,    1, 1e-2, 7, 0.99, 0.9999, 4, 1e-8, 7)

    type <-  rep("numeric", length(lower))
    type[c(3, 6, 9)] <- "integer"
    type[c(11)] <- "factor"

    transformations <- rep("identity", length(lower))
    transformations[c(3, 6)] <- "trans_2pow"
    fixpars <- list()
    factorlevels <- list()
    dummy <- FALSE
    relpars <- list()
  }
  ## specify learner cnn deep learning (convolutionary network)
  if(model=="cnn"){
    learner <- NULL
    tunepars <- c("nFilters",
                  "kernelSize",
                  "activation",
                  "poolSize", # (factor: only off=0 or on=1)
                  "learning_rate",
                  "epochs",
                  "beta_1",
                  "beta_2",
                  "layers",
                  "epsilon",
                  "optimizer")
    lower <-   c(4,  2,  1, 0, 1e-6, 3, 0.9,  0.99,   1, 1e-9, 1)
    defaults <- c(5,  2,  2, 1, 1e-3, 4, 0.9,  0.999,  1, 1e-7, 5)
    upper <-   c(6,  3,  2,  1, 1e-2, 7, 0.99, 0.9999, 3, 1e-8, 7)

    type <-  rep("numeric", length(lower))
    type[c(1,2,6,9)] <- "integer"
    type[c(3,4,11)] <- "factor"

    transformations <- rep("identity", length(lower))
    transformations[c(1,6)] <- "trans_2pow_round"
    transformations[2] <- "trans_odd_round"
    fixpars <- list()
    factorlevels <- list()
    dummy <- FALSE
    relpars <- list()
  }

  returnList <- list(learner = learner,
                     tunepars = tunepars,
                     defaults = matrix(defaults,nrow=1),
                     lower = lower,
                     upper = upper,
                     type = type,
                     fixpars = fixpars,
                     factorlevels = factorlevels,
                     transformations = transformations,
                     dummy = dummy,
                     relpars = relpars)

  if(!is.null(active)){
    activeInd <- which(tunepars %in% active)
    returnList$tunepars <- returnList$tunepars[activeInd]
    returnList$defaults <- returnList$defaults[activeInd]
    returnList$lower <- returnList$lower[activeInd]
    returnList$upper <- returnList$upper[activeInd]
    returnList$type <- returnList$type[activeInd]
    returnList$transformations <- returnList$transformations[activeInd]
  }

  return(returnList)
}
