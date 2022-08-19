# SPOTMisc

## 1.19.50
* URL: https://keras.rstudio.com/ (moved to https://tensorflow.rstudio.com/)

## 1.19.48
* prepareComparisonPlot accepts extended parameter list: 
  * defaultModelList and tunedModelList
  
## 1.19.46
* kerasConf$returnObject == "model" returns y instead of only the model, i.e., y$model

## 1.19.44
* BUGFIX:
  dropoutfac value in getModelConf() corrected: [0.5; 1] -> [0; 0.5]

## 1.19.42
* BUGFIX:
  * prepareProgressPlot() uses information about initial number of replicates (Rinit)

## 1.19.40
* NEW:
  * getModelConf: number of units modified: 0;5 (1;32)

## 1.19.38
* NEW:
  * further tests tfdatasets

## 1.19.36
* NEW:
  * funKerasGeneric: embedding enhanced

## 1.19.32
* BUGFIX:
  "mace" -> "race" in Census Data

## 1.19.30
* BUGFIX:
  *funKerasGeneric: embedding corrected

## 1.19.28
* BUGFIX:
  * xgboost added to suggests in DESCRIPTION

## 1.19.26
* NEWS:
  * new function trans_odd_round()
  * evalKerasMnist() checks size (simple check)
  * new transformations for model "cnn" in getModelConf(): kernel uses trans_odd_round()

## 1.19.24
* resDl100 data

## 1.19.22
* NEWS:
  * getKerasConf() moved to getKerasConf.R
  * getKerasConf(): new argument tfDevice = "/cpu:0"
  * getMnistData(), evalKerasGeneric() and funKerasGeneric() require the argument kerasConf (getKerasConf() is not used as the default)
  * https addresses in documentations use \url{https://....}
  * funKerasModular.R renamed to funKerasGeneric.R

## 1.19.20
* NEWS:
  * getModelConf() arguments modified. Function accepts a list, e.g., from getKerasConf()

## 1.19.18:
* NEWS:
  * documentation

## 1.19.16:
* NEWS
  * dataCensus: data frame
  * getDataCensus() uses SPOTMisc data frame "dataCensus" if getDataCensus() fails

## 1.19.14
* NEWS
  * spotKeras()
  
## 1.19.12
* NEWS:
  * pooling modified

## 1.19.10
* NEWS:
  * kerasActivation()

## 1.19.8
* NEWS
  * hyperparameter handling extended: mapX2Flags() uses new argument "model". Default: "dl"

## 1.19.6
* NEWS:
  * limited number of training epochs in startMnistRun.R

## 1.19.4
* NEWS:
  * MNIST updated
  * kConf -> kerasConf
  * startMnistRun()

## 1.19.2
* BUGFIX:
  tryCatch -> try in evalKerasGeneric()

## 1.19.0
* NEWS:
  * evalParamCensus() accepts new parameter xbest. Default: "xBestOcba"

## 1.18.14
* NEWS:
  * cleanup. funKerasGeneric.R removed

## 1.18.12
* NEWS:
  * getMlConfig() does not call getDataCensus(), but accepts data as an argument.
  * get_objf() renamed to getObjf()

## 1.18.10
* NEWS:
  * test predDlCensus

## 1.18.8
* BUGFIX:
  * predMlCensus transforms x values before calculation
* NEWS:
  * getDataCensus: new argument "cache.only" with default value "FALSE"
  * documentation, cleanup

## 1.18.6
* BUGFIX:
  prop corrected in evalParamCensus()
  
## 1.18.4
* NEW:
  evalParamCensus() added back

## 1.18.2
* NEWS:
  startKerasRun()

## 1.18.0
* NEWS:
  funKerasModular

## 1.17.14
* NEWS:
  * documentation

## 1.17.12
* NEWS:
  handleNA -> handleNAs

## 1.17.10
* NEWS:
  * kerasConf: naDummy removed, use spot::handleNAs instead

## 1.17.8
* NEWS: 
  startCensusRun implemented as a script, not a function

## 1.17.6
* NEWS:
  kerasEvalPrediction() uses testScore instead of Metrics

## 1.17.4
* NEWS: predictDL rewritten

## 1.17.2
* NEWS:
  * CPU/GPU handling

## 1.17.0
* NEWS:
  * tf updated

## 1.16.4
* NEW:
  *  with(tf$device("/cpu:0"), {}) is not used anymore, instead the follwoing mechanism is
     used:
     library(keras)
     use_session_with_seed(42, disable_gpu = FALSE, disable_parallel_cpu = FALSE)

## 1.16.2
* NEWS:
  * target set in  getDataCensus 

## 1.16.0
* NEWS:
  * Generic keras functions

## 1.15.24
* BUGFIX:
  * parallelPlot fixed
* NEWS:
  * spotStats
  * keras functions use "with(tf$device("/cpu:0"), {})" to force CPU usage

## 1.15.22
* BUGFIX:
  * OCBAbudget -> OCBABudget (typo)

## 1.15.20
* NEWS:
 * plots updated

## 1.15.18
* NEWS:
  * OCBA

## 1.15.16
* BUGFIX:
  * commented code

## 1.15.14
* BUGFIX:
  * getParamConf: missing task.type added

## 1.15.12
* NEWS:
  * evalParamCensus() determines nFeatures
  * cache.dir set to "oml.cache"

## 1.5.10
* BUGFIX:
  example evalParamCensus fixed

## 1.15.8
* BUGFIX:
  evalParamCensus(): x is converted to matrix

## 1.15.6
* NEWS:
  Documentation.

## 1.15.4
* NEWS:
  startCensusRun()

## 1.15.2
* NEWS:
  * Improved experimental designs and evaluations for keras 

## 1.15.0
* NEWS:
  * prepare optimizer select. This is hyperparameter x11, which can encoded as 
  type=factor.

## 1.14.2
* BUGFIX:
  * predDlCensus uses test data 

## 1.14.0
* BUGFIX:
  * getMlrResample() gets task information, i.e.: getMlrResample(task,..)

## 1.13.6
* NEWS: 
  * further test (data census)

## 1.13.4
* NEWS: 
  * further test (data census)

## 1.13.2
* NEW: additional tests

## 1.13.0
* NEW:
  funKerasCensus.R: tests

## 1.12.6
* NEW: 
  getModelConf() can handle dl, too
  funKerasCensus accepts target argumet (default = NULL)

## 1.12.4
* NEW:
  trans_mult_2 function

## 1.12.2
* BUGFIX:
  * corrected minbucket default value (1/3)

## 1.12.0
* NEWS:
  * getModelConf() includes default values (defaults)

## 1.11.18
* NEWS: cleanup and documentation:
  * test_ds_census is not used

## 1.11.16
* NEWS:
  * deep learning on census data uses embedding layer

## 1.11.14
* BUGFIX: fixing continued

## 1.11.12
* BUGFIX: still incomplete fix

## 1.11.10
* BUGFIX: target specification

## 1.11.8
* BUGFIX: prop handling in getCensusTrainValTestData()

## 1.11.6
* NEW: default param (only begin)

## 1.11.4
* NEW: predfun() cleanup

## 1.11.2
* NEW:
  * predMlCensus rewritten (old version: predMlCensusDeprecated)
  
## 1.11.0
* NEW:
  * mlr pipeline rewritten

## 1.10.4
* BUGFIX
  * filter -> dplyr::filter

## 1.10.2
* BUGFIX
  * predMlCensus() uses testData for task definition

## 1.10.0
* NEWS:
  * predMlCensus() rewritten
  * new function: makeLearnerFromHyperparameters() 
  * plot_pareto removed
  * package emoa not in DESCRIPTION

## 1.9.6
* BUGFIX in predMlCensus(): trueY col nums fixed
* getCensusTrainValTestData requires target argument
* target income_class replaced by age

## 1.9.4
* BUGFIX in predMlCensus(): duplicate task generation

## 1.9.2
* NEWS:
  * predlMlCensus
  * scorePredictions

## 1.9.0
* NEWS:
  * mlr cleanup and restructure

## 1.8.12
* BUGFIX:
  * Usage of "name" in plotSensitivity() fixed

## 1.8.10
* NEWS:
  * New plot functions (based on ggplot2)

## 1.8.8
* NEWS:
  test funKeras: test for adam
  
## 1.8.6
* NEWS:
  getCensusTrainValTestData(): prop can be a 2-dim vector

## 1.8.4
* NEWS:
  * accuracy uses as.integer(pred >0.5)

## 1.8.2
* NEWS:
  * Extended paprameter conf fpr Census DNNs

## 1.8.0
* NEWS:
  * Census data for Keras

## 1.7.6
* BUGFIX:
  * funKerasCensus: several bug fixes
  
## 1.7.4
* NEWS:
  * funKerasCensus()
  * dataKeras.R with census data

## 1.7.2
* NEWS:
  * More documentation
  * get_task_census() is based on getMlrTask() and getDataCensus()
  
## 1.7.0
* NEWS
  * hyperparameter tuning functions added
  * get_task_census(): get OpenML KDD Census data set
  * getExplan()

### 1.6.6
* NEWS:
  * resKerasMnist07 data added
  * resKerasMnist11 prepared (multi start spot)

### 1.6.4
* NEWS:
  * resKerasMnist03 updated
  * experiments 07-10 prepared

## 1.6.2
* NEWS
  * resKerasMnist02 updated

## 1.6.0
* BUGFIX:
  * apply() in funKerasMnist uses "byrow=TRUE"
* NEWS:
  * Extended logging facilities

## 1.5.20
* NEWS:
  * vignette examples updated, resKerasMnist02 uses transformX values
  * resKerasMnist02 updated
  * default resKerasMnist02 calculation uses transfromX

## 1.5.18
* NEWS:
  bachtsize parameter in funKerasMinst() is not mapped to 2^batchsize, because this is handled by SPOT via transferX()

## 1.5.14 and 16
* NEWS:
  * resKerasMnist02 updated

## 1.5.12
* NEWS:
  * getMnistData()

## 1.5.10
* CHANGES:
  * Updated results for kerasMnist02/03

## 1.5.8
* BUGFIX
  * NA handling fixed

## 1.5.6
* BUGFIX
  * correct treatment of negative accuracy values 

## 1.5.4
* BUGFIX
  * dim of MOO retrun values fixed

## 1.5.2
* NEWS
  * keras mock-up functions (resKerasDummy) for quick testing

## 1.5.0
* NEWS
  * SIR modeling and Markov analyis removed

## 1.4.4
* BUGFIX
  * SIR modelling: tests/examples skipped

## 1.4.2
* CHANGES:
  * funKerasMnist and funKerasTransferLearning return y (instead of y and a model list)

## 1.4.0
* NEW:
  * Cleanup

## 1.3.12
* NEW:
  * Data sets resKerasTransferLearning* updated
  * Data set resKerasTransferLearning06 prepared

## 1.3.10
* NEW:
  * Additional examples

## 1.3.8
* NEW:
  * neg val accuracy

## 1.3.6
* NEW:
  * handle NaN and Inf: mapped to 1234567890

## 1.3.4
* BUGFIX:
  * beta* -> beta_* 

## 1.3.2
* NEW:
  * kerasOptimizer.R

## 1.3.0
* NEW:
  * Perform and document keras examples
  * Basic vignette 
  * Additional keras data sets

## 1.2.30
* NEW:
  * kerasHelper.R

## 1.2.28
* NEW:
  * function funKerasTransferLearning

## 1.2.26
* BUGFIX:
  * correct return value in funKerasMnist "res$y"

## 1.2.22
* NEW:
  * test for funKeras*

## 1.2.20
* NEW
  * funKeras* functions extended

## 1.2.18
* BUGFIX:
  * learning_rate replaces lr
* Roxygen 7.1.2

### 1.2.16
* NEW:
  * spotPlot 
  
### 1.2.14
* CHANGES:
  * Documentation improved 

### 1.2.12
* BUGFIX:
  * funKerasMnist returns the correct test loss 
  * funKerasMnist accepts further keras configuration parameters such as verbose etc. as arguments: 
    funKerasMnist <- function (x, kConf = getKerasConf()). 

### 1.2.10
* CHANGES:
  * funKerasMnist returns a list. score can be accessed via $score

### 1.2.8
* NEW:
  * funKerasMnist accepts additional parameters: 
     * verbose 
     * callbacks	
     * validation_split
     * validation_data	
     * shuffle Logical 
     
### 1.2.6
* NEW:
   * funKerasMnist (requires a working python enviroment via reticulate)

### 1.2.4
* BUGFIX:
  * Namespaces in Imports field not imported from:‘MASS’ ‘RColorBrewer’ ‘ggplot2’ All declared Imports should be used.

