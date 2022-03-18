# SPOTMisc

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

