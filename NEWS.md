# SPOTMisc

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

