## ----knitrSetup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages(c(
#  "benchmarkme", "callr", "emoa", "ggsci", "jsonlite", "keras", "magrittr", "mlr", "plotly", "RColorBrewer", "reticulate", "rpart.plot", "sensitivity", "smoof", "tensorflow", "tfdatasets"))

## ---- eval = FALSE------------------------------------------------------------
#  cd <project-dir>

## ---- eval = FALSE------------------------------------------------------------
#  virtualenv python

## ---- eval = FALSE------------------------------------------------------------
#  source python/bin/activate

## ---- eval = FALSE------------------------------------------------------------
#  which python

## ---- eval = FALSE------------------------------------------------------------
#  pip install numpy pandas matplotlib tensorflow tensorflow-datasets

## ---- eval=FALSE--------------------------------------------------------------
#  /Users/bartz/workspace/SPOT/python/bin/python -m pip install --upgrade pip

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("reticulate")

## ---- eval = FALSE------------------------------------------------------------
#  Sys.setenv(RETICULATE_PYTHON = "python/bin/python")

## ---- eval = FALSE------------------------------------------------------------
#  reticulate::py_config()

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("tensorflow")
#  install.packages("keras")
#  library(keras)
#  install_keras()

## ---- eval = FALSE------------------------------------------------------------
#  install_keras(tensorflow = "gpu")

## ----SPOT, include = FALSE, eval = FALSE--------------------------------------
#  ## install.packages("devtools")
#  ## devtools::install_github("r-lib/devtools")
#  url <- "http://owos.gm.fh-koeln.de:8055/bartz/spot.git"
#  devtools::install_git(url = url)

## ---- setup, eval=FALSE-------------------------------------------------------
#  library("SPOT")
#  packageVersion("SPOT")

## ---- spotNoise, eval = FALSE-------------------------------------------------
#  # noisy objective
#  res1 <- spot(,function(x)funSphere(x)+rnorm(nrow(x)),c(-2,-3),c(1,2),
#   		control=list(funEvals=40,noise=TRUE))
#  # noise with replicated evaluations
#  res2 <- spot(,function(x)funSphere(x)+rnorm(nrow(x)),c(-2,-3),c(1,2),
#   		control=list(funEvals=40,noise=TRUE,replicates=2,
#   		designControl=list(replicates=2)))
#  # and with OCBA
#  res3 <- spot(,function(x)funSphere(x)+rnorm(nrow(x)),c(-2,-3),c(1,2),
#   		control=list(funEvals=40,noise=TRUE,replicates=2,OCBA=TRUE,OCBABudget=1,
#   		designControl=list(replicates=2)))
#  # Check results with non-noisy function:
#  funSphere(res1$xbest)
#  funSphere(res2$xbest)
#  funSphere(res3$xbest)

## ----braninFactor, eval=FALSE-------------------------------------------------
#  braninFunctionFactor <- function (x) {
#     y <- (x[2]  - 5.1/(4 * pi^2) * (x[1] ^2) + 5/pi * x[1]  - 6)^2 +
#       10 * (1 - 1/(8 * pi)) * cos(x[1] ) + 10
#     if(x[3]==1)
#       y <- y +1
#     else if(x[3]==2)
#       y <- y -1
#     return(y)
#  }

## ----vecObj, eval = FALSE-----------------------------------------------------
#  objFun <- function(x){apply(x,1,braninFunctionFactor)}

## ---- spotFac, eval = FALSE---------------------------------------------------
#  set.seed(1)
#  res <- spot(fun=objFun,lower=c(-5,0,1),upper=c(10,15,3),
#              control=list(model=buildKriging,
#                           types= c("numeric","numeric","factor"),
#                           optimizer=optimLHD))
#   res$xbest
#   res$ybest

## ----splotLag, eval = FALSE---------------------------------------------------
#  ## run spot without log
#  res <- spot(fun = funSphere,
#              lower=c(0,0),
#              upper=c(100,100)
#  )
#  ## run spot with log
#  funSphereLog <- function(x){
#    cbind(funSphere(x),x)
#  }
#  res2 <- spot(fun = funSphereLog,
#              lower=c(0,0),
#              upper=c(100,100)
#  )
#  res$logInfo
#  res2$logInfo

## ---- eval=FALSE--------------------------------------------------------------
#  library("SPOTMisc")
#  library("SPOT")
#  kerasConf <- getKerasConf()
#  lower <- c(1e-6, 1e-6, 16,0.6, 1e-9, 10, 6,0.4,0.99,1,1e-8)
#  upper <- c(0.5, 0.5, 512, 1.5, 1e-2, 50, 10,0.999,0.999,10,6e-8)
#  types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
#             "integer",  "integer",  "numeric",  "numeric",  "integer",
#             "numeric")
#  
#  
#  kerasConf$verbose <- 0
#  res <- spot(x = NULL,
#              fun = funKerasMnist,
#              lower = lower,
#              upper = upper,
#              control = list(funEvals=100,
#                             noise = TRUE,
#                             types = types,
#                             plots = TRUE,
#                             progress = TRUE,
#                             seedFun = 1,
#                             seedSPOT = 1),
#              kConf = kerasConf)
#  res01 <- res
#  # save(res01, file="exp01.RData")

## ---- eval=FALSE--------------------------------------------------------------
#  library("SPOTMisc")
#  library("SPOT")
#  kerasConf <- getKerasConf()
#  lower <- c(1e-6, 1e-6, 16,0.6, 1e-9, 10, 6,0.4,0.99,1,1e-8)
#  upper <- c(0.5, 0.5, 512, 1.5, 1e-2, 50, 10,0.999,0.999,10,6e-8)
#  types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
#             "integer",  "integer",  "numeric",  "numeric",  "integer",
#             "numeric")
#  
#  
#  kerasConf$verbose <- 0
#  Ninit <- 3 * length(lower)
#  Rinit <- 3
#  replicates <- 3
#  
#  res <- spot(x = NULL,
#              fun = funKerasMnist,
#              lower = lower,
#              upper = upper,
#              control = list(funEvals=20*length(lower),
#                             noise = TRUE,
#                             types = types,
#                             replicates=replicates,
#                             designControl = list(replicates = Rinit,
#                                                                 size = Ninit),
#                             model = buildKriging,
#                             optimizer = optimLBFGSB,
#                             modelControl=list(target="ei"),
#                             plots = TRUE,
#                             progress = TRUE,
#                             seedFun = 1,
#                             seedSPOT = 1),
#              kConf = kerasConf)
#  res02 <- res
#  #save(res02, file="exp02.RData")

## ----run002, eval=FALSE-------------------------------------------------------
#  #install.packages("~/workspace/SPOTMisc_1.2.26.tar.gz", repos=NULL, type="source")
#  #install.packages("~/workspace/SPOT_2.6.2.tar.gz", repos=NULL, type="source")
#  library("keras")
#  library("SPOT")
#  library("SPOTMisc")
#  packageVersion("SPOT")
#  packageVersion("SPOTMisc")
#  set.seed(123)
#  
#  dat <- genCatsDogsData()
#  kerasConf <- getKerasConf()
#  kerasConf$trainData <- dat$trainData
#  kerasConf$validationData <- dat$validationData
#  kerasConf$testData <- dat$testData
#  
#  lower <- c(1e-6, 1e-6, 16,0.6, 1e-9, 10, 6,0.4,0.99,1,1e-8)
#  upper <- c(0.5, 0.5, 512, 1.5, 1e-2, 50, 10,0.999,0.999,10,6e-8)
#  types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
#             "integer",  "integer",  "numeric",  "numeric",  "integer",
#             "numeric")
#  ### First example: simple function call:
#  x <- matrix(lower, 1,)
#  funKerasTransferLearning(x, kConf = kerasConf)
#  kerasConf$verbose <- 1
#  res <- spot(x = NULL,
#              fun = funKerasTransferLearning,
#              lower = lower,
#              upper = upper,
#              control = list(funEvals=200,
#                             noise = TRUE,
#                             types = types,
#                             plots = TRUE,
#                             progress = TRUE,
#                             seedFun = 1,
#                             seedSPOT = 1),
#              kConf = kerasConf)
#  # save(res, file = "run002.RData")

## ---- eval = FALSE------------------------------------------------------------
#  install.packages(c(
#  "benchmarkme", "callr", "emoa", "ggsci", "jsonlite", "keras", "magrittr", "mlr", "plotly", "RColorBrewer", "reticulate", "rpart.plot", "sensitivity", "smoof", "tensorflow", "tfdatasets"))
#  install.packages(
#    "~/workspace/spotmisc/packages/SPOT_2.7.0.tar.gz",
#    repos = NULL,
#    type = "source"
#  )
#  install.packages(
#    "~/workspace/spotmisc/packages/SPOTMisc_1.6.0.tar.gz",
#    repos = NULL,
#    type = "source"
#  )
#  library("SPOTMisc")
#  packageVersion("SPOTMisc")
#  library("SPOT")
#  packageVersion("SPOT")
#  
#  mnist <- getMnistData()
#  
#  # "dropout" =  x[1]: 0.1--0.5
#  # "dropoutfact" =  x[2]: 0.85--0.95
#  # "units" = x[3]: 7-9 (128--512)
#  # "unitsfact" = x[4]:  0.5--1.5
#  # "learning_rate" =  x[5]: 1e-5--1e-2
#  # "epochs" = x[6]: 3-6 (10--50)
#  # "batchsize" = x[7]: 6-9 (64--512)
#  # "beta_1" =  x[8]: 0.9--0.999
#  # "beta_2" =  x[9]: 0.99--0.9999
#  # "layers" =  x[10]: 1--5
#  # "epsilon" = x[11]: 1e-8--1e-8
#  lower <- c(0.1, 0.85, 7, 0.5, 1e-5, 3,  6,  0.9,  0.99,    1, 1e-8)
#  upper <- c(0.5, 0.95, 9, 1.5, 1e-2, 6, 9, 0.999, 0.9999, 5, 1e-6)
#  types <-  rep("numeric", length(lower))
#  types[c(3, 6, 7)] <- "integer"
#  f2 <- function(x) {
#    2 ^ x
#  }
#  transformFun <- rep("identity", length(lower))
#  transformFun[c(3, 6, 7)] <- "f2"
#  kerasConf <- getKerasConf()
#  kerasConf$verbose <- 0
#  kerasConf$resDummy <- FALSE
#  Ninit <- 2 * length(lower)
#  Rinit <- 2
#  replicates <- 2
#  
#  res <- spot(
#    x = NULL,
#    fun = funKerasMnist,
#    lower = lower,
#    upper = upper,
#    control = list(
#      funEvals = 20 * length(lower),
#      noise = TRUE,
#      types = types,
#      transformFun = transformFun,
#      replicates = replicates,
#      designControl = list(replicates = Rinit,
#                           size = Ninit),
#      model = buildKriging,
#      optimizer = optimLBFGSB,
#      modelControl = list(target = "ei"),
#      plots = FALSE,
#      progress = TRUE,
#      seedFun = 1,
#      seedSPOT = 1
#    ),
#    kConf = kerasConf,
#    data = mnist
#  )
#  save(res, file = paste0("resKerasMnist02", as.numeric(Sys.time()), ".RData"))
#  ##
#  ## Evaluation of the default hyperparameter setting (baseline)
#  library("SPOTMisc")
#  packageVersion("SPOTMisc")
#  ##
#  source("~/workspace/spotmisc/R/funKerasMnist.R")
#  library("SPOT")
#  packageVersion("SPOT")
#  source("~/workspace/SPOT/R/spot.R")
#  source("~/workspace/SPOT/R/objectiveFunctionEvaluation.R")
#  mnist <- getMnistData()
#  # "dropout" =  x[1],
#  # "dropoutfact" =  x[2],
#  # "units" = x[3],
#  # "unitsfact" = x[4],
#  # "learning_rate" =  x[5],
#  # "epochs" = x[6],
#  # "batchsize" = x[7],
#  # "beta_1" =  x[8],
#  # "beta_2" =  x[9],
#  # "layers" =  x[10],
#  # "epsilon" = x[11]
#  lower <- c(0.4, 0.9, 8, 0.5, 0.001, 4, 3, 0.9, 0.999, 2, 1e-07)
#  upper <- lower
#  types <-  rep("numeric", length(lower))
#  types[c(3, 6, 7)] <- "integer"
#  f2 <- function(x) {
#    2 ^ x
#  }
#  transformFun <- rep("identity", length(lower))
#  transformFun[c(3, 6, 7)] <- "f2"
#  #
#  kerasConf <- getKerasConf()
#  kerasConf$resDummy <- FALSE
#  kerasConf$verbose <- 0
#  kerasConf$returnValue <- "validationLoss"
#  kerasConf$naDummy <- 1234567890
#  kerasConf$clearSession <- TRUE
#  ### Example: spot call to evaluate the default design
#  n <- 10
#  res <- spot(
#    x = NULL,
#    fun = funKerasMnist,
#    lower = lower,
#    upper = upper,
#    control = list(
#      funEvals = n,
#      transformFun = transformFun,
#      replicateResults = TRUE,
#      types = types,
#      seedFun = 1,
#      seedSPOT = 1,
#      verbosity = 0,
#      designControl=list(size=n)
#    ),
#    kConf = kerasConf,
#    data = mnist
#  )
#  save(res, file = paste0("resKerasMnist02Default", as.numeric(Sys.time()), ".RData"))

## ----resKerasMnist03, eval=FALSE----------------------------------------------
#  install.packages(c(
#  "benchmarkme", "callr", "emoa", "ggsci", "jsonlite", "keras", "magrittr", "mlr", "plotly", "RColorBrewer", "reticulate", "rpart.plot", "sensitivity", "smoof", "tensorflow", "tfdatasets"))
#  install.packages(
#    "~/workspace/spotmisc/packages/SPOT_2.7.0.tar.gz",
#    repos = NULL,
#    type = "source"
#  )
#  install.packages(
#    "~/workspace/spotmisc/packages/SPOTMisc_1.6.0.tar.gz",
#    repos = NULL,
#    type = "source"
#  )
#  library("SPOTMisc")
#  packageVersion("SPOTMisc")
#  library("SPOT")
#  packageVersion("SPOT")
#  
#  mnist <- getMnistData()
#  
#  # "dropout" =  x[1]: 0.1--0.5
#  # "dropoutfact" =  x[2]: 0.85--0.95
#  # "units" = x[3]: 7-9 (128--512)
#  # "unitsfact" = x[4]:  0.5--1.5
#  # "learning_rate" =  x[5]: 1e-5--1e-2
#  # "epochs" = x[6]: 3-6 (10--50)
#  # "batchsize" = x[7]: 6-9 (64--512)
#  # "beta_1" =  x[8]: 0.9--0.999
#  # "beta_2" =  x[9]: 0.99--0.9999
#  # "layers" =  x[10]: 1--5
#  # "epsilon" = x[11]: 1e-8--1e-8
#  lower <- c(0.1, 0.85, 7, 0.5, 1e-5, 3,  6,  0.9,  0.99,    1, 1e-8)
#  upper <- c(0.5, 0.95, 9, 1.5, 1e-2, 6, 9, 0.999, 0.9999, 5, 1e-6)
#  types <-  rep("numeric", length(lower))
#  types[c(3, 6, 7)] <- "integer"
#  f2 <- function(x) {
#    2 ^ x
#  }
#  transformFun <- rep("identity", length(lower))
#  transformFun[c(3, 6, 7)] <- "f2"
#  kerasConf <- getKerasConf()
#  kerasConf$verbose <- 0
#  kerasConf$resDummy <- FALSE
#  kerasConf$returnValue <- "trainingLoss"
#  Ninit <- 2 * length(lower)
#  Rinit <- 2
#  replicates <- 2
#  
#  res <- spot(
#    x = NULL,
#    fun = funKerasMnist,
#    lower = lower,
#    upper = upper,
#    control = list(
#      funEvals = 20 * length(lower),
#      noise = TRUE,
#      types = types,
#      transformFun = transformFun,
#      replicates = replicates,
#      designControl = list(replicates = Rinit,
#                           size = Ninit),
#      model = buildKriging,
#      optimizer = optimLBFGSB,
#      modelControl = list(target = "ei"),
#      plots = FALSE,
#      progress = TRUE,
#      seedFun = 1,
#      seedSPOT = 1
#    ),
#    kConf = kerasConf,
#    data = mnist
#  )
#  save(res, file = paste0("resKerasMnist03", as.numeric(Sys.time()),".RData"))

## ----resKerasMnist07, eval=FALSE----------------------------------------------
#  install.packages(c(
#  "benchmarkme", "callr", "emoa", "ggsci", "jsonlite", "keras", "magrittr", "mlr", "plotly", "RColorBrewer", "reticulate", "rpart.plot", "sensitivity", "smoof", "tensorflow", "tfdatasets"))
#  install.packages(
#    "~/workspace/spotmisc/packages/SPOT_2.7.0.tar.gz",
#    repos = NULL,
#    type = "source"
#  )
#  install.packages(
#    "~/workspace/spotmisc/packages/SPOTMisc_1.6.0.tar.gz",
#    repos = NULL,
#    type = "source"
#  )
#  library("SPOTMisc")
#  packageVersion("SPOTMisc")
#  library("SPOT")
#  packageVersion("SPOT")
#  
#  mnist <- getMnistData()
#  
#  # "dropout" =  x[1]: 0.1--0.5
#  # "dropoutfact" =  x[2]: 0.85--0.95
#  # "units" = x[3]: 7-9 (128--512)
#  # "unitsfact" = x[4]:  0.5--1.5
#  # "learning_rate" =  x[5]: 1e-5--1e-2
#  # "epochs" = x[6]: 3-6 (10--50)
#  # "batchsize" = x[7]: 6-9 (64--512)
#  # "beta_1" =  x[8]: 0.9--0.999
#  # "beta_2" =  x[9]: 0.99--0.9999
#  # "layers" =  x[10]: 1--5
#  # "epsilon" = x[11]: 1e-8--1e-8
#  lower <- c(0.1, 0.85, 7, 0.5, 1e-5, 3,  6,  0.9,  0.99,    1, 1e-8)
#  upper <- c(0.5, 0.95, 9, 1.5, 1e-2, 6, 9, 0.999, 0.9999, 5, 1e-6)
#  types <-  rep("numeric", length(lower))
#  types[c(3, 6, 7)] <- "integer"
#  f2 <- function(x) {
#    2 ^ x
#  }
#  transformFun <- rep("identity", length(lower))
#  transformFun[c(3, 6, 7)] <- "f2"
#  kerasConf <- getKerasConf()
#  kerasConf$verbose <- 0
#  kerasConf$resDummy <- FALSE
#  kerasConf$returnValue <- "testLoss"
#  Ninit <- 2 * length(lower)
#  Rinit <- 2
#  replicates <- 2
#  
#  res <- spot(
#    x = NULL,
#    fun = funKerasMnist,
#    lower = lower,
#    upper = upper,
#    control = list(
#      funEvals = 20 * length(lower),
#      noise = TRUE,
#      types = types,
#      transformFun = transformFun,
#      replicates = replicates,
#      designControl = list(replicates = Rinit,
#                           size = Ninit),
#      model = buildKriging,
#      optimizer = optimLBFGSB,
#      modelControl = list(target = "ei"),
#      plots = FALSE,
#      progress = TRUE,
#      seedFun = 1,
#      seedSPOT = 1
#    ),
#    kConf = kerasConf,
#    data = mnist
#  )
#  save(res, file = paste0("resKerasMnist07", as.numeric(Sys.time()),".RData"))

## ----resKerasMnist08, eval=FALSE----------------------------------------------
#  install.packages(c(
#  "benchmarkme", "callr", "emoa", "ggsci", "jsonlite", "keras", "magrittr", "mlr", "plotly", "RColorBrewer", "reticulate", "rpart.plot", "sensitivity", "smoof", "tensorflow", "tfdatasets"))
#  install.packages(
#    "~/workspace/spotmisc/packages/SPOT_2.7.0.tar.gz",
#    repos = NULL,
#    type = "source"
#  )
#  install.packages(
#    "~/workspace/spotmisc/packages/SPOTMisc_1.6.0.tar.gz",
#    repos = NULL,
#    type = "source"
#  )
#  library("SPOTMisc")
#  packageVersion("SPOTMisc")
#  library("SPOT")
#  packageVersion("SPOT")
#  
#  mnist <- getMnistData()
#  
#  # "dropout" =  x[1]: 0.1--0.5
#  # "dropoutfact" =  x[2]: 0.85--0.95
#  # "units" = x[3]: 7-9 (128--512)
#  # "unitsfact" = x[4]:  0.5--1.5
#  # "learning_rate" =  x[5]: 1e-5--1e-2
#  # "epochs" = x[6]: 3-6 (10--50)
#  # "batchsize" = x[7]: 6-9 (64--512)
#  # "beta_1" =  x[8]: 0.9--0.999
#  # "beta_2" =  x[9]: 0.99--0.9999
#  # "layers" =  x[10]: 1--5
#  # "epsilon" = x[11]: 1e-8--1e-8
#  lower <- c(0.1, 0.85, 7, 0.5, 1e-5, 3,  6,  0.9,  0.99,    1, 1e-8)
#  upper <- c(0.5, 0.95, 9, 1.5, 1e-2, 6, 9, 0.999, 0.9999, 5, 1e-6)
#  types <-  rep("numeric", length(lower))
#  types[c(3, 6, 7)] <- "integer"
#  f2 <- function(x) {
#    2 ^ x
#  }
#  transformFun <- rep("identity", length(lower))
#  transformFun[c(3, 6, 7)] <- "f2"
#  kerasConf <- getKerasConf()
#  kerasConf$verbose <- 0
#  kerasConf$resDummy <- FALSE
#  kerasConf$returnValue <- "negValidationAccuracy"
#  Ninit <- 2 * length(lower)
#  Rinit <- 2
#  replicates <- 2
#  
#  res <- spot(
#    x = NULL,
#    fun = funKerasMnist,
#    lower = lower,
#    upper = upper,
#    control = list(
#      funEvals = 20 * length(lower),
#      noise = TRUE,
#      types = types,
#      transformFun = transformFun,
#      replicates = replicates,
#      designControl = list(replicates = Rinit,
#                           size = Ninit),
#      model = buildKriging,
#      optimizer = optimLBFGSB,
#      modelControl = list(target = "ei"),
#      plots = FALSE,
#      progress = TRUE,
#      seedFun = 1,
#      seedSPOT = 1
#    ),
#    kConf = kerasConf,
#    data = mnist
#  )
#  save(res, file = paste0("resKerasMnist08", as.numeric(Sys.time()),".RData"))

## ----resKerasMnist09, eval=FALSE----------------------------------------------
#  install.packages(c(
#  "benchmarkme", "callr", "emoa", "ggsci", "jsonlite", "keras", "magrittr", "mlr", "plotly", "RColorBrewer", "reticulate", "rpart.plot", "sensitivity", "smoof", "tensorflow", "tfdatasets"))
#  install.packages(
#    "~/workspace/spotmisc/packages/SPOT_2.7.0.tar.gz",
#    repos = NULL,
#    type = "source"
#  )
#  install.packages(
#    "~/workspace/spotmisc/packages/SPOTMisc_1.6.0.tar.gz",
#    repos = NULL,
#    type = "source"
#  )
#  library("SPOTMisc")
#  packageVersion("SPOTMisc")
#  library("SPOT")
#  packageVersion("SPOT")
#  
#  mnist <- getMnistData()
#  
#  # "dropout" =  x[1]: 0.1--0.5
#  # "dropoutfact" =  x[2]: 0.85--0.95
#  # "units" = x[3]: 7-9 (128--512)
#  # "unitsfact" = x[4]:  0.5--1.5
#  # "learning_rate" =  x[5]: 1e-5--1e-2
#  # "epochs" = x[6]: 3-6 (10--50)
#  # "batchsize" = x[7]: 6-9 (64--512)
#  # "beta_1" =  x[8]: 0.9--0.999
#  # "beta_2" =  x[9]: 0.99--0.9999
#  # "layers" =  x[10]: 1--5
#  # "epsilon" = x[11]: 1e-8--1e-8
#  lower <- c(0.1, 0.85, 7, 0.5, 1e-5, 3,  6,  0.9,  0.99,    1, 1e-8)
#  upper <- c(0.5, 0.95, 9, 1.5, 1e-2, 6, 9, 0.999, 0.9999, 5, 1e-6)
#  types <-  rep("numeric", length(lower))
#  types[c(3, 6, 7)] <- "integer"
#  f2 <- function(x) {
#    2 ^ x
#  }
#  transformFun <- rep("identity", length(lower))
#  transformFun[c(3, 6, 7)] <- "f2"
#  kerasConf <- getKerasConf()
#  kerasConf$verbose <- 0
#  kerasConf$resDummy <- FALSE
#  kerasConf$returnValue <- "negTrainingAccuracy"
#  Ninit <- 2 * length(lower)
#  Rinit <- 2
#  replicates <- 2
#  
#  res <- spot(
#    x = NULL,
#    fun = funKerasMnist,
#    lower = lower,
#    upper = upper,
#    control = list(
#      funEvals = 20 * length(lower),
#      noise = TRUE,
#      types = types,
#      transformFun = transformFun,
#      replicates = replicates,
#      designControl = list(replicates = Rinit,
#                           size = Ninit),
#      model = buildKriging,
#      optimizer = optimLBFGSB,
#      modelControl = list(target = "ei"),
#      plots = FALSE,
#      progress = TRUE,
#      seedFun = 1,
#      seedSPOT = 1
#    ),
#    kConf = kerasConf,
#    data = mnist
#  )
#  save(res, file = paste0("resKerasMnist09", as.numeric(Sys.time()),".RData"))

## ----resKerasMnist10, eval=FALSE----------------------------------------------
#  install.packages(c(
#  "benchmarkme", "callr", "emoa", "ggsci", "jsonlite", "keras", "magrittr", "mlr", "plotly", "RColorBrewer", "reticulate", "rpart.plot", "sensitivity", "smoof", "tensorflow", "tfdatasets"))
#  install.packages(
#    "~/workspace/spotmisc/packages/SPOT_2.7.0.tar.gz",
#    repos = NULL,
#    type = "source"
#  )
#  install.packages(
#    "~/workspace/spotmisc/packages/SPOTMisc_1.6.0.tar.gz",
#    repos = NULL,
#    type = "source"
#  )
#  library("SPOTMisc")
#  packageVersion("SPOTMisc")
#  library("SPOT")
#  packageVersion("SPOT")
#  
#  mnist <- getMnistData()
#  
#  # "dropout" =  x[1]: 0.1--0.5
#  # "dropoutfact" =  x[2]: 0.85--0.95
#  # "units" = x[3]: 7-9 (128--512)
#  # "unitsfact" = x[4]:  0.5--1.5
#  # "learning_rate" =  x[5]: 1e-5--1e-2
#  # "epochs" = x[6]: 3-6 (10--50)
#  # "batchsize" = x[7]: 6-9 (64--512)
#  # "beta_1" =  x[8]: 0.9--0.999
#  # "beta_2" =  x[9]: 0.99--0.9999
#  # "layers" =  x[10]: 1--5
#  # "epsilon" = x[11]: 1e-8--1e-8
#  lower <- c(0.1, 0.85, 7, 0.5, 1e-5, 3,  6,  0.9,  0.99,    1, 1e-8)
#  upper <- c(0.5, 0.95, 9, 1.5, 1e-2, 6, 9, 0.999, 0.9999, 5, 1e-6)
#  types <-  rep("numeric", length(lower))
#  types[c(3, 6, 7)] <- "integer"
#  f2 <- function(x) {
#    2 ^ x
#  }
#  transformFun <- rep("identity", length(lower))
#  transformFun[c(3, 6, 7)] <- "f2"
#  kerasConf <- getKerasConf()
#  kerasConf$verbose <- 0
#  kerasConf$resDummy <- FALSE
#  kerasConf$returnValue <- "negTestAccuracy"
#  Ninit <- 2 * length(lower)
#  Rinit <- 2
#  replicates <- 2
#  
#  res <- spot(
#    x = NULL,
#    fun = funKerasMnist,
#    lower = lower,
#    upper = upper,
#    control = list(
#      funEvals = 20 * length(lower),
#      noise = TRUE,
#      types = types,
#      transformFun = transformFun,
#      replicates = replicates,
#      designControl = list(replicates = Rinit,
#                           size = Ninit),
#      model = buildKriging,
#      optimizer = optimLBFGSB,
#      modelControl = list(target = "ei"),
#      plots = FALSE,
#      progress = TRUE,
#      seedFun = 1,
#      seedSPOT = 1
#    ),
#    kConf = kerasConf,
#    data = mnist
#  )
#  save(res, file = paste0("resKerasMnist10", as.numeric(Sys.time()),".RData"))

## ---- eval=FALSE--------------------------------------------------------------
#  library("SPOTMisc")
#  library("SPOT")
#  
#  # Hyperparameters:
#  # "dropout" =  x[1],
#  # "learning_rate" =  x[2],
#  # "epochs" = x[3],
#  # "beta_1" =  x[4],
#  # "beta_2" =  x[5],
#  # "epsilon" = x[6],
#  # "optimizer" = x[7]
#  
#  lower <- c(1e-6, 1e-6, 2, 0.8, 0.8, 1e-9, 1)
#  upper <- c(0.2, 1e-2, 5, 0.99, 0.9999, 1e-3, 2)
#  types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
#                        "numeric",  "factor")
#  
#  kerasConf <- getKerasConf()
#  kerasConf$verbose <- 1
#  kerasConf$resDummy <- FALSE
#  kerasConf$naDummy <- 1234567890
#  res <- spot(x = NULL,
#              fun = funKerasTransferLearning,
#              lower = lower,
#              upper = upper,
#              control = list(funEvals=50,
#                             model=buildKriging,
#                             noise = TRUE,
#                             types = types,
#                             optimizer=optimDE,
#                             plots = FALSE,
#                             progress = TRUE,
#                             seedFun = 1,
#                             seedSPOT = 1,
#                             verbosity = 1),
#                             kConf = kerasConf
#  )
#  save(res, file = paste0("resKerasTransferLearning04", as.numeric(Sys.time()),".RData"))

## ---- eval=FALSE--------------------------------------------------------------
#  library("SPOTMisc")
#  library("SPOT")
#  kerasConf <- getKerasConf()
#  
#  # Hyperparameters:
#  # "dropout" =  x[1],
#  # "learning_rate" =  x[2],
#  # "epochs" = x[3],
#  # "beta_1" =  x[4],
#  # "beta_2" =  x[5],
#  # "epsilon" = x[6],
#  # "optimizer" = x[7]
#  
#  lower <- c(1e-6, 1e-6, 2, 0.8, 0.8, 1e-9, 1)
#  upper <- c(0.2, 1e-2, 5, 0.99, 0.9999, 1e-3, 2)
#  types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
#             "numeric",  "factor")
#  
#  ### Example: spot call with extended verbosity and negative val accuracy:
#  kerasConf$verbose <- 0
#  kerasConf$returnValue <- "negValidationAccuracy"
#  res <- spot(x = NULL,
#              fun = funKerasTransferLearning,
#              lower = lower,
#              upper = upper,
#              control = list(funEvals=50,
#                             model=buildKriging,
#                             noise = TRUE,
#                             types = types,
#                             optimizer=optimDE,
#                             plots = FALSE,
#                             progress = TRUE,
#                             seedFun = 1,
#                             seedSPOT = 1,
#                             verbosity = 1),
#                             kConf = kerasConf
#  )
#  save(res, file = paste0("resKerasTransferLearning05", as.numeric(Sys.time()),".RData"))

## ---- eval=FALSE--------------------------------------------------------------
#  library("SPOTMisc")
#  library("SPOT")
#  kerasConf <- getKerasConf()
#  
#  # Hyperparameters:
#  # "dropout" =  x[1],
#  # "learning_rate" =  x[2],
#  # "epochs" = x[3],
#  # "beta_1" =  x[4],
#  # "beta_2" =  x[5],
#  # "epsilon" = x[6],
#  # "optimizer" = x[7]
#  replicates <- 1
#  lower <- c(0.2, 1e-5, 5, 0.9, 0.999, 1e-7, 1)
#  upper <- c(0.2, 1e-5, 5, 0.9, 0.999, 1e-7, 1)
#  types <- c("numeric",  "numeric",  "integer",  "numeric",  "numeric",
#             "numeric",  "factor")
#  
#  ### Example: spot call to evaluate the default design
#  kerasConf$verbose <- 0
#  kerasConf$returnValue <- "validationLoss"
#  kerasConf$naDummy <- 1234567890
#  res <- spot(x = NULL,
#              fun = funKerasTransferLearning,
#              lower = lower,
#              upper = upper,
#              control = list(funEvals=10,
#                             model=buildKriging,
#                             noise = TRUE,
#                             types = types,
#                             optimizer = optimDE,
#                             plots = FALSE,
#                             progress = TRUE,
#                             designControl=list(replicates=replicates),
#                             seedFun = 1,
#                             seedSPOT = 1,
#                             verbosity = 1),
#                             kConf = kerasConf
#  )
#  save(res, file = paste0("resKerasTransferLearning06", as.numeric(Sys.time()),".RData"))

## ---- eval = FALSE------------------------------------------------------------
#  install.packages(c(
#  "benchmarkme", "callr", "emoa", "ggsci", "jsonlite", "keras", "magrittr", "mlr", "plotly", "RColorBrewer", "reticulate", "rpart.plot", "sensitivity", "smoof", "tensorflow", "tfdatasets"))
#  install.packages(
#    "~/workspace/spotmisc/packages/SPOT_2.8.4.tar.gz",
#    repos = NULL,
#    type = "source"
#  )
#  install.packages(
#    "~/workspace/spotmisc/packages/SPOTMisc_1.8.0.tar.gz",
#    repos = NULL,
#    type = "source"
#  )
#  library("SPOTMisc")
#  packageVersion("SPOTMisc")
#  library("SPOT")
#  packageVersion("SPOT")
#  
#  mnist <- getMnistData()
#  
#  # "dropout" =  x[1]: 0.1--0.5
#  # "dropoutfact" =  x[2]: 0.85--0.95
#  # "units" = x[3]: 7-9 (128--512)
#  # "unitsfact" = x[4]:  0.5--1.5
#  # "learning_rate" =  x[5]: 1e-5--1e-2
#  # "epochs" = x[6]: 3-6 (10--50)
#  # "batchsize" = x[7]: 6-9 (64--512)
#  # "beta_1" =  x[8]: 0.9--0.999
#  # "beta_2" =  x[9]: 0.99--0.9999
#  # "layers" =  x[10]: 1--5
#  # "epsilon" = x[11]: 1e-8--1e-8
#  lower <- c(0.1, 0.85, 7, 0.5, 1e-5, 3,  6,  0.9,  0.99,    1, 1e-8)
#  upper <- c(0.5, 0.95, 9, 1.5, 1e-2, 6, 9, 0.999, 0.9999, 5, 1e-6)
#  types <-  rep("numeric", length(lower))
#  types[c(3, 6, 7)] <- "integer"
#  f2 <- function(x) {
#    2 ^ x
#  }
#  transformFun <- rep("identity", length(lower))
#  transformFun[c(3, 6, 7)] <- "f2"
#  kerasConf <- getKerasConf()
#  kerasConf$verbose <- 0
#  kerasConf$resDummy <- TRUE
#  Ninit <- 2 * length(lower)
#  Rinit <- 2
#  replicates <- 2
#  
#  res <- spot(
#    x = NULL,
#    fun = funKerasMnist,
#    lower = lower,
#    upper = upper,
#    control = list(
#      funEvals = 20 * length(lower),
#      multiStart = 2,
#      noise = TRUE,
#      types = types,
#      transformFun = transformFun,
#      replicates = replicates,
#      designControl = list(replicates = Rinit,
#                           size = Ninit),
#      model = buildKriging,
#      optimizer = optimDE,
#      modelControl = list(target = "ei"),
#      plots = FALSE,
#      progress = TRUE,
#      seedFun = 1,
#      seedSPOT = 1,
#      verbosity=0
#    ),
#    kConf = kerasConf,
#    data = mnist
#  )
#  save(res, file = paste0("resKerasMnist11", as.numeric(Sys.time()), ".RData"))

## ---- eval=FALSE--------------------------------------------------------------
#  # install.packages("~/workspace/book/packages/SPOTMisc_1.8.2.tar.gz", repos = NULL, type = "source")
#  # install.packages("~/workspace/book/packages/SPOT_2.8.4.tar.gz", repos = NULL, type = "source")
#  library("SPOTMisc")
#  if (packageVersion("SPOTMisc") < "1.8.0") stop("Please update 'SPOT'")
#  library("SPOT")
#  if (packageVersion("SPOT") < "2.8.4") stop("Please update 'SPOT'")
#  
#   #  "dropout" =  x[1]: 0.1--0.5
#   #  "dropoutfact" =  x[2]: 1-1
#   #  "units" = x[3]: 1-1
#   #  "unitsfact" = x[4]: 1-1
#   #  "learning_rate" =  x[5]: 1e-5--1e-2
#   #  "epochs" = x[6]: 3-6
#   #  "beta_1" =  x[7]: 0.9--0.999
#   #  "beta_2" =  x[8]: 0.99--0.9999
#   #  "layers" =  x[9]: 1-1
#   #  "epsilon" = x[10]: 1e-8--1e-6
#  
#  lower <- c(0.1, 1, 1, 1, 1e-5, 3, 0.9,  0.99,    1, 1e-8)
#  upper <- c(0.5, 1, 1, 1, 1e-2, 6, 0.999, 0.9999, 1, 1e-6)
#  types <-  rep("numeric", length(lower))
#  types[c(3, 4, 6, 9)] <- "integer"
#  f2 <- function(x) {
#    2 ^ x
#  }
#  transformFun <- rep("identity", length(lower))
#  transformFun[c(3, 4, 6, 9)] <- "f2"
#  kerasConf <- getKerasConf()
#  kerasConf$verbose <- 0
#  kerasConf$resDummy <- FALSE
#  Ninit <- 2 * length(lower)
#  Rinit <- 2
#  replicates <- 2
#  
#  task.type <- "classif"
#  nobs <- 1e4 # max: 229285
#  nfactors = "high"
#  nnumericals <- "high"
#  cardinality = "high"
#  data.seed <- 1
#  tuner.seed <- 1
#  timebudget <-  3600
#  timeout <- timebudget/20
#  data <- getCensusTrainValTestData(task.type=task.type,
#                                    nobs=nobs,
#                                    nfactors = nfactors,
#                                    nnumericals = nnumericals,
#                                    cardinality = cardinality,
#                                    data.seed =1)
#  specCensusPrep <- censusDataPrep(data=data, batch_size = 32)
#  
#  res <- spot(
#    x = NULL,
#    fun = funKerasCensus,
#    lower = lower,
#    upper = upper,
#    control = list(
#      funEvals = 20 * length(lower),
#      multiStart = 2,
#      noise = TRUE,
#      types = types,
#      transformFun = transformFun,
#      replicates = replicates,
#      designControl = list(replicates = Rinit,
#                           size = Ninit),
#      model = buildKriging,
#      optimizer = optimDE,
#      modelControl = list(target = "y"),
#      plots = FALSE,
#      progress = TRUE,
#      seedFun = 1,
#      seedSPOT = 1,
#      verbosity=0
#    ),
#    kerasConf = kerasConf,
#    specCensusPrep =specCensusPrep
#  )
#  save(res, file = paste0("rescs3Census01", as.numeric(Sys.time()), ".RData"))

