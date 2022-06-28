#' @title Start hyperparameter optimization runs with spot based on US census data
#' @description Runs to compare standard machine learning and deep learning models
#'
#' @param modelList list of models. Default:
#' \code{list("xgboost")}
#' @param runNr character, specifies the run number. Default: \code{"000"}
#' @param SPOTVersion smallest package version number
#' @param SPOTMiscVersion smallest package version number
#' @param timebudget time budget Default:  \code{3600} (secs)
#' @param target target "age"
#' @param cachedir cache dir "oml.cache"
#' @param task.type task type "classif"
#' @param nobs number of observations 1e4
#' @param nfactors number of factorial variables "high"
#' @param nnumericals number of numerical variables "high"
#' @param cardinality cardinality "high"
#' @param data.seed 1
#' @param prop proportion 2 / 3
#' @param batch_size batch size (for dl) 32
#' @param tuner.seed seed for SPOT  1
#' @param returnValue "validationLoss"
#' @param initSizeFactor multiplier for the initial design size 2
#' @param spotModel buildKriging
#' @param spotOptim optimDE
#' @param lower NULL
#' @param upper NULL
#' @param noise TRUE
#' @param OCBA TRUE
#' @param OCBABudget 3
#' @param multiStart 2
#' @param multFun 200
#' @param handleNAsMethod handleNAsMean
#' @param imputeCriteriaFuns list(is.infinite, is.na, is.nan)
#' @param krigingTarget "ei"
#' @param krigingUseLambda TRUE
#' @param krigingReinterpolate FALSE
#' @param defaultAsStartingPoint FALSE
#' @param plots  FALSE
#' @param Rinit 2
#' @param replicates 2
#' @param resDummy FALSE
#' @param verbosity 0
#'
#' @importFrom SPOT buildKriging
#' @importFrom SPOT optimDE
#' @importFrom SPOT handleNAsMean
#' @importFrom SPOT spot
#' @importFrom utils packageVersion
#'
#' @examples
#' \donttest{
#' ### These examples require an activated Python environment as described in
#' ### Bartz-Beielstein, T., Rehbach, F., Sen, A., and Zaefferer, M.:
#' ### Surrogate Model Based Hyperparameter Tuning for Deep Learning with SPOT,
#' ### June 2021. http://arxiv.org/abs/2105.14625.
#' PYTHON_RETICULATE <- FALSE
#' if(PYTHON_RETICULATE){
#' library("dplyr")
#' library("farff")
#' library("GGally")
#' library("keras")
#' library("tensorflow")
#' library("Metrics")
#' library("mlr")
#' library("OpenML")
#' library("reticulate")
#' library("rpart")
#' library("rpart.plot")
#' library("SPOT")
#' library("SPOTMisc")
#' library("tfdatasets")
#' library("rsample")
#' startXGBCensusRun(modelList=list("xgboost"), timebudget=60, plots=TRUE)
#' }
#' }
#'
#' @export
startXGBCensusRun <-
  function(modelList = list("xgboost"),
           runNr = "000",
           SPOTVersion = "2.11.14",
           SPOTMiscVersion = "1.19.28",
           timebudget =  3600, ## secs
           target =  "age",
           cachedir =  "oml.cache",
           task.type =  "classif",
           nobs =  1e4,
           nfactors =  "high",
           nnumericals =  "high",
           cardinality =  "high",
           data.seed =  1,
           prop =  2 / 3,
           batch_size =  32,
           tuner.seed =  1,
           returnValue =  "validationLoss",
           initSizeFactor =  2,
           spotModel =  buildKriging,
           spotOptim =  optimDE,
           lower =  NULL,
           upper =  NULL,
           noise =  TRUE,
           OCBA =  TRUE,
           OCBABudget =  3,
           multiStart =  2,
           multFun =  200,
           handleNAsMethod =  handleNAsMean,
           imputeCriteriaFuns = list(is.infinite, is.na, is.nan),
           krigingTarget =  "ei",
           krigingUseLambda =  TRUE,
           krigingReinterpolate =  FALSE,
           defaultAsStartingPoint =  TRUE,
           plots =  FALSE,
           Rinit =  2,
           replicates =  2,
           resDummy =  FALSE,
           verbosity =  0) {
    if (packageVersion("SPOT") < SPOTVersion)
      stop("Please update 'SPOT'")
    if (packageVersion("SPOTMisc") < SPOTMiscVersion)
      stop("Please update 'SPOTMisc'")
    for (model in modelList) {
      ### Common to all models:
      # Timeout
      #timebudget/20 # use 1/20 times the budget before tuning is stopped
      timeout = timebudget / 20
      if ((model == "rpart") | (model == "dl")) {
        timeout = NA
      }
      ## (Down)load census data
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
      # ml models
      cfg <-   getMlConfig(
        target = target,
        model = model,
        data = dfCensus,
        task.type = task.type,
        nobs = nobs,
        nfactors = nfactors,
        nnumericals = nnumericals,
        cardinality = cardinality,
        data.seed = data.seed,
        prop = prop
      )
      objf <- getObjf(config = cfg,
                      timeout = timeout)
      # use default as a starting point
      if (defaultAsStartingPoint) {
        x0 <- matrix(cfg$defaults,
                     nrow = 1,
                     ncol = length(cfg$defaults))
      } else{
        x0 <- NULL
      }
      result <- spot(
        x = x0,
        fun = objf,
        lower = cfg$lower,
        upper = cfg$upper,
        control = list(
          types = cfg$type,
          verbosity = verbosity,
          time = list(maxTime = timebudget / 60),
          plots = plots,
          progress = TRUE,
          model = spotModel,
          optimizer = spotOptim,
          noise = noise,
          OCBA = OCBA,
          OCBABudget = OCBABudget,
          seedFun = 123,
          seedSPOT = tuner.seed,
          designControl = list(
            replicates = Rinit,
            size = initSizeFactor * length(cfg$lower)
          ),
          replicates = replicates,
          funEvals = Inf,
          modelControl = list(
            target = krigingTarget,
            useLambda = krigingUseLambda,
            reinterpolate = krigingReinterpolate
          ),
          optimizerControl = list(funEvals = multFun * length(cfg$lower)),
          multiStart = multiStart,
          # No transform necessary, because it is handled by getObjf
          parNames = cfg$tunepars,
          yImputation = list(
            handleNAsMethod = handleNAsMethod,
            imputeCriteriaFuns = imputeCriteriaFuns,
            penaltyImputation = 3
          )
        )
      )
      save(result, file = paste0(model, runNr, ".RData"))
    }
  }
