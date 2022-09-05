#' @title Start hyperparameter optimization runs with spot based on MNIST data
#' @description Runs to compare deep learning models. Note: Number of epochs is
#' limited: \code{model <- "dl"; cfg <- getModelConf(model = model); cfg$upper[6] <- 5}
#'
#' @param runNr character, specifies the run number. Default: \code{"000"}
#' @param SPOTVersion smallest package version number
#' @param SPOTMiscVersion smallest package version number
#' @param encoding encoding: \code{"oneHot"} od \code{"tensor"}. Default: \code{"tensor"}
#' @param network network: \code{"dl"} odr \code{"cnn"}. Default: \code{"cnn"}
#' @param timebudget time budget Default:  \code{3600} (secs)
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
#' library("reticulate")
#' library("rpart")
#' library("rpart.plot")
#' library("SPOT")
#' library("SPOTMisc")
#' library("tfdatasets")
#' library("rsample")
#' startMnistRun(timebudget=60, initSizeFactor = 1, verbosity = 1)
#' startMnistRun(timebudget=60, encoding="tensor", network="cnn")
#' }
#' }
#'
#' @export
startMnistRun <- function(runNr = "000",
                           SPOTVersion = "2.11.4",
                           SPOTMiscVersion = "1.19.6",
                           encoding = "tensor",
                           network = "cnn",
                           timebudget =  60,
                           ## secs
                           data.seed =  1,
                           prop =  2 / 3,
                           batch_size =  32,
                           tuner.seed =  1,
                           returnValue =  "validationLoss",
                           initSizeFactor =  1,
                           spotModel =  buildKriging,
                           spotOptim =  optimDE,
                           lower =  NULL,
                           upper =  NULL,
                           noise =  TRUE,
                           OCBA =  FALSE,
                           OCBABudget =  0,
                           multiStart =  2,
                           multFun =  200,
                           handleNAsMethod =  handleNAsMean,
                           imputeCriteriaFuns = list(is.infinite, is.na, is.nan),
                           krigingTarget =  "ei",
                           krigingUseLambda =  TRUE,
                           krigingReinterpolate =  TRUE,
                           defaultAsStartingPoint =  TRUE,
                           plots =  FALSE,
                           Rinit =  1,
                           replicates =  1,
                           resDummy =  FALSE,
                           verbosity =  0) {
  if (packageVersion("SPOT") < SPOTVersion)
    stop("Please update 'SPOT'")
  if (packageVersion("SPOTMisc") < SPOTMiscVersion)
    stop("Please update 'SPOTMisc'")
  timeout = NA
  # Deep learning hyperparameter
  model <- "dl"
  cfg <- getModelConf(model = model)
  ## reduce number of epochs to max 2^5 = 32
  cfg$upper[6] <- 5
  # Keras config
  kerasConf <- getKerasConf()
  kerasConf$verbose <- verbosity
  kerasConf$returnValue <- returnValue
  kerasConf$clearSession <- TRUE
  kerasConf$resDummy <- resDummy
  kerasConf$encoding <- encoding
  kerasConf$network <- network
  data <- getMnistData(kerasConf)
  # use default as a starting point
  if (defaultAsStartingPoint) {
    x0 <- matrix(cfg$defaults,
                 nrow = 1,
                 ncol = length(cfg$defaults))
  } else{
    x0 <- NULL
  }
  # User defined bounds instead of bounds from getModelConf:
  if (!is.null(lower))
    cfg$lower <- lower
  if (!is.null(upper))
    cfg$upper <- upper
  # Call SPOT for dl:
  result <- spot(
    x = x0,
    fun = funKerasMnist,
    lower = cfg$lower,
    upper = cfg$upper,
    control = list(
      types = cfg$type,
      verbosity = verbosity,
      time = list(maxTime = timebudget / 60),
      #convert to minutes
      plots = plots,
      progress = TRUE,
      model = spotModel,
      optimizer = spotOptim,
      noise = noise,
      OCBA = OCBA,
      OCBABudget = OCBABudget,
      seedFun = NA,
      seedSPOT = tuner.seed,
      designControl = list(replicates = Rinit,
                           size = initSizeFactor * length(cfg$lower)),
      replicates = replicates,
      funEvals = Inf,
      modelControl = list(
        target = krigingTarget,
        useLambda = krigingUseLambda,
        reinterpolate = krigingReinterpolate
      ),
      optimizerControl = list(funEvals = multFun * length(cfg$lower)),
      multiStart = multiStart,
      transformFun = cfg$transformations,
      parNames = cfg$tunepars,
      yImputation = list(
        handleNAsMethod = handleNAsMethod,
        imputeCriteriaFuns = imputeCriteriaFuns,
        penaltyImputation = 3
      )
    ),
    kerasConf = kerasConf,
    data = data
  )
  save(result, file = paste0("data/", model, runNr, ".RData"))
}
