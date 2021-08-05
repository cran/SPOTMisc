#' @title
#' evalMarkovChain
#'
#' @description
#' Evaluation function for the optimization of continuous time Markov chains models using
#' \code{\link[SimInf]{SIR}} models.
#'
#' @details
#' Performs a SIR model simulation for one specific parameter setting using
#' the \code{\link{modelMarkovChain}} function and evaluates the result
#' from the simulation model output with the real data. The RMSE is
#' used as the performance metric.
#'
#' @param x vector of parameter values, i.e., parameters of the MarkovChain model to evaluate with the function.
#' \describe{
#'		\item{\code{p}}{num  [0;1] proportion of confirmed cases}
#'		\item{\code{beta}}{num Transmission rate from susceptible to infected. See \code{\link[SimInf]{SIR}}. }
#'		\item{\code{gamma}}{num Recovery rate from infected to recovered. See \code{\link[SimInf]{SIR}}. }
#'		\item{\code{CFR}}{num Case Fatalities Rate}
#' }
#'
#' @param conf a list with entries
#' \describe{
#'		\item{\code{regionData}}{ A data frame with observations of 3 variables:
#'      \describe{
#'		         \item{\code{date}}{Date, format: "2020-01-22" "2020-01-23" "2020-01-24" "2020-01-25" ...}
#'		         \item{\code{confirmed}}{num  0 0 0 0 0 0 0 0 0 0 ..}
#'		         \item{\code{fatalities}}{fatalities: num  0 0 0 0 0 0 0 0 0 0 ...}
#'               }
#'      }
#'       \item{\code{N}}{N population size}
#'  }
#'
#' @return value (log RMSE)
#'
#' @examples
#' require("SimInf")
#' data <- preprocessInputData(regionTrain, regionPopulation)
#' set.seed(123)
#' data <- data[[1]]
#' N <- attr(data, "regionPopulation")
#' # params:  x = (p, beta, gamma, CFR)
#' x <- c(0.01, 0.01, 0.1, 0.01)
#' # Simulate only 2 days
#' conf <- list(regionData = data[1:2, ], N = N)
#' evalMarkovChain(x = x, conf=conf)
#'
#' @export
#'
evalMarkovChain <- function(x, conf){
    regionData <- conf$regionData
    N <- conf$N
    days <- nrow(conf$regionData)
    p <- x[1]
    beta <- x[2]
    gamma <- x[3]
    CFR <- x[4]
    param <- c(p, beta, gamma)
    message("evalMarkovChain: p,beta,gamma,CFR: ",
            p, ", ",
            beta, ", ",
            gamma, ", ", CFR)
    m <- modelMarkovChain(x = param, days = days, N = N)
    message("evalMarkovChain: last(I), last(F) in 1e3: ", round( ( (m$X2 + m$X3)[days] )/1000), ", ",
            round( ( (m$X3 * CFR)[days] )/1000) )
    ## real counter for  infected cases includes fatalities:
    realI <- regionData$confirmed
    realF <- regionData$fatalities
    ## real confirmed = SIR infected + SIR removed,
    ## => we have to add these values for a comparison
    ## with the real confirmed values:
    hatI <- m$X2 + m$X3
    ## CFR = fatalities / recovered:
    hatF <- m$X3 * CFR
    mseI <- mean((realI - hatI) ^ 2)
    mseF <- mean((realF - hatF) ^ 2)
    if (is.nan(mseI))
        mseI <- Inf
    if (is.nan(mseF))
        mseF <- Inf
    # RMSE to evaluate model over actual data
    y <- sqrt(mseI + mseF)
    message("evalMarkovChain / 1e6: y: ", (y * 1e-6))
    return(y)
}

#' @title
#' funMarkovChain
#'
#' @description
#' Wrapper function for \code{\link{evalMarkovChain}} used by \code{\link{spot}}.
#'
#' @details
#' Optimization of Continuous Time Markov Chains (MarkovChain) models.
#'
#' @param x vector of parameter values, i.e., parameters of the MarkovChain model to evaluate with the function.
#' \describe{
#'		\item{\code{p}}{num  [0;1] proportion of confirmed cases}
#'		\item{\code{beta}}{num Transmission rate from susceptible to infected. See \code{\link[SimInf]{SIR}}. }
#'		\item{\code{gamma}}{num Recovery rate from infected to recovered. See \code{\link[SimInf]{SIR}}. }
#'		\item{\code{CFR}}{num Case Fatalities Rate}
#' }
#'
#' @param conf a list with entries
#' \describe{
#'		\item{\code{regionData}}{ A data frame with observations of 3 variables:
#'      \describe{
#'		         \item{\code{date}}{Date, format: "2020-01-22" "2020-01-23" "2020-01-24" "2020-01-25" ...}
#'		         \item{\code{confirmed}}{num  0 0 0 0 0 0 0 0 0 0 ..}
#'		         \item{\code{fatalities}}{fatalities: num  0 0 0 0 0 0 0 0 0 0 ...}
#'               }
#'      }
#'       \item{\code{N}}{N population size}
#'  }
#'
#' @return 1-column matrix with resulting function values (RMSE)
#'
#' @examples
#' \donttest{
#' data <- preprocessInputData(regionTrain, regionPopulation)
#' set.seed(123)
#' data <- data[[1]]
#' N <- attr(data, "regionPopulation")
#' # params:  x = (p, beta, gamma, CFR)
#' x <- matrix(c(0.01, 0.1, 0.01, 0.1),1,4)
#' conf <- list(regionData = data, N = N)
#' funMarkovChain(x, conf)
#' }
#' @export
#'
funMarkovChain <- function (x, conf) {
    matrix(apply(x, # matrix
                 1, # margin (apply over rows)
                 evalMarkovChain, # function
                 conf))
}


#' @title
#' tuneRegionModel
#'
#' @description
#' Perform a \code{\link{spot}} run on \code{\link{funMarkovChain}} with region data.
#' Results can be postprocessed with the function \code{\link{parseTunedRegionModel}}
#' to extract model and parameter information.
#'
#' @details
#' Note: the default number of function evaluations is very low.
#'
#'
#' @param regionData is a data.frame with observations of 3 variables:
#' \describe{
#'		\item{\code{data}}{Date, format: "2020-01-22" "2020-01-23" "2020-01-24" "2020-01-25" ...}
#'		\item{\code{confirmed}}{num  0 0 0 0 0 0 0 0 0 0 ..}
#'		\item{\code{fatalities}}{fatalities: num  0 0 0 0 0 0 0 0 0 0 ...}
#' }
#' and attributes
#'  - attr(*, "regionName")= chr "Afghanistan/"
#'  - attr(*, "regionPopulation")= int 38041754
#'
#' @param pops evaluated populations
#' @param lower lower bounds for spot optimization, @seealso \code{Link{spot}}
#' @param upper upper bounds for spot optimization, @seealso \code{Link{spot}}
#' @param control spot control list, see controlSpot
#'
#' @importFrom SPOT spot
#' @importFrom SPOT spotControl
#'
#' @return This function returns a list with:
#' \describe{
#'		\item{\code{regionName}}{ e.g., "Afghanistan/": List of 7}
#'		\describe{
#'		\item{\code{xbest}}{Parameters of the best found solution (matrix).}
#'		\item{\code{ybest}}{Objective function value of the best found solution (matrix).}
#'		\item{\code{x}}{Archive of all evaluation parameters (matrix).}
#'		\item{\code{y}}{Archive of the respective objective function values (matrix).}
#'		\item{\code{count}}{Number of performed objective function evaluations.}
#'		\item{\code{msg}}{Message specifying the reason of termination.}
#'		\item{\code{modelFit}}{The fit of the last build model, i.e., an object returned by the last call to the function specified by \code{control$model}.}
#'      }
#'  }
#'
#' @examples
#' \donttest{
#' require("SimInf")
#' require("SPOT")
#' data <- preprocessInputData(regionTrain, regionPopulation)
#' a <- c(0.01,  0.001, 0.001,   0.001)
#' b <- c(0.1,  0.01, 0.01,   0.01)
#' lapply(data[1], tuneRegionModel, pops=NULL, lower = a, upper = b,
#' control=list(funEvals=6,
#' designControl=list(size=5), model = buildLM))
#' }
#'
#' @export
tuneRegionModel <- function(regionData,
                            pops=NULL,
                            lower = NULL,
                            upper = NULL,
                            control=list()){

    regionName <- attr(regionData, "regionName")
    regionPopulation <- attr(regionData, "regionPopulation")

    ## params:  x = (p, beta, gamma, CFR)
    a <- c(0.1,  0.001, 0.001,   0.001)
    b <- c(0.7,                  0.1,   0.1,     0.5)
    if(!is.null(lower)) a <- lower
    if(!is.null(upper)) b <- upper

    con <- spotControl(length(a))
    con[names(control)] <- control
    control<-con

    conf <- list(regionData=regionData, N = regionPopulation)
    # not implemented: add previous population if available
    # addpop <- NULL
    # if (!is.null(pops)) addpop <- pops[[regionName]]

    res <- spot(x=NULL, fun = funMarkovChain, lower=a, upper=b, control=control, conf)
    attr(res, "regionName") <- regionName
    attr(res, "regionPopulation") <- regionPopulation
    return(res)
}


#' @title parseTunedRegionModel
#'
#' @description
#' Parse results from the \code{\link{tuneRegionModel}} function, i.e.,
#' results from a \code{\link{spot}} run on \code{\link{funMarkovChain}}
#'
#' @param xList list of results from \code{\link{spot}} run
#'
#' @return returns the following list of 3:
#' \describe{
#'		\item{\code{models}}{data.frame with obs. of  7 variables:
#'      \describe{
#'		          \item{\code{p}}{num, e.g., 27373033}
#'		          \item{\code{beta}}{num}
#'		           \item{\code{gamma}}{num}
#'		           \item{\code{CFR}}{num}
#'		           \item{\code{cost}}{num}
#'		           \item{\code{region}}{ chr, e.g., "Afghanistan/"}
#'		           \item{\code{regionPopulation}}{ num population}
#'                }
#'      }
#'      \item{\code{pops}}{list of x values for countries, i.e., \code{\link{spot}} population generated at
#'      each generation, e.g., \code{ Afghanistan/: num [1:6, 1:4] 32039478 28078906 23529925 11257083 9883189 ...}}.
#'      Here, 6 function evaluations were performed and the search space is 4-dim.
#'      \item{\code{y}}{function values for pops}
#'      }
#'
#' @examples
#' \donttest{
#' require("SimInf")
#' require("SPOT")
#' data <- preprocessInputData(regionTrain, regionPopulation)
#' resList <- lapply(data[1], tuneRegionModel, pops=NULL, control=list(funEvals=6,
#' designControl=list(size=5), model = buildLM))
#' parsedList <- parseTunedRegionModel(resList)
#' }
#' @export
#'
parseTunedRegionModel <- function(xList){
    models <- data.frame()
    pops <- list()
    y <- list()
    for (i in 1:length(xList)){
        res <- xList[[i]]
        rn <- attr(res, "regionName")
        regionPopulation <- attr(res, "regionPopulation")
        pops[[rn]] <- res$x
        y[[rn]] <- res$y
        best <- res$xbest
        cost <- res$ybest
        ##   (p, beta, gamma, CFR)
        r <- data.frame(p = best[1]
                        , beta = best[2]
                        , gamma = best[3]
                        , CFR = best[4]
                        , cost = cost
                        , region = rn
                        , regionPopulation = regionPopulation
                        , row.names = NULL)
        models <- rbind(models, r)
    }
    list(models = models, pops = pops, y = y)
}



#' @title generateMCPrediction
#'
#' @description
#' Predict results on test data using the tuned MarkovChain mode.
#' Result has the required data format for a submission to the Kaggle
#' COVID-19 challenge.
#'
#' @details
#' Output from \code{\link{parseTunedRegionModel}} is processed.
#'
#' @param testData 'data.frame':	obs. of  3 variables:
#' \describe{
#'		\item{\code{ForecastId}}{int  1 2 3 4 5 6 7 8 9 10 ...}
#'		\item{\code{Region}}{Factor w/ 313 levels "Afghanistan/",..: 1 1 1 1 1 1 1 1 1 1 ...}
#'		\item{\code{Date}}{Date, format: "2020-04-02" "2020-04-03" "2020-04-04" "2020-04-05" ...}
#' }
#'
#' @param models 'data.frame':	obs. of  7 variables:
#' \describe{
#'		\item{\code{p}}{num  [0;1] proportion of confirmed cases}
#'		\item{\code{beta}}{num  13.8 13.8 16.3 11.5 29.2 ...}
#'		\item{\code{gamma}}{num  13.8 13.8 16.3 11.5 29.2 ...}
#'		\item{\code{CFR}}{num  0.14 0.14 0.2319 0.0312 0.0705 ...}
#'		\item{\code{cost}}{num  658 256 1207 1091 300 ...}
#'		\item{\code{region}}{ chr, e.g., "Afghanistan/" "Albania/" "Algeria/" "Andorra/" ...}
#' }
#' @param startSimulation chr start of the simulation period, e.g., "2020-01-22".
#' startSimulation must be at or before the Date from testData. Simulations can
#' start earlier, because some use R=0. This enables a warm-up period.
#'
#' @param write logical. Default \code{FALSE}. If \code{TRUE}, results are written to the file \code{submit.csv}.
#'
#' @importFrom utils write.csv
#'
#' @return returns data.frame with obs. of the following 3 variables:
#' \describe{
#'    \item{\code{ForecastId}}{int: Forecast Id taken from the \code{regionTest} data set.}
#'    \item{\code{ConfirmedCases}}{num: Cumulative number of confirmed cases.}
#'    \item{\code{Fatalities}}{num: Cumulative number of fatalities.}
#' }
#'
#'
#' @examples
#' \donttest{
#' require("SimInf")
#' require("SPOT")
#' data <- preprocessInputData(regionTrain, regionPopulation)
#' testData <- preprocessTestData(regionTest)
#' ## Select the first region:
#' testData <- testData[testData$Region==levels(testData$Region)[1], ]
#' testData$Region <- droplevels(testData$Region)
#' ## Very small number of function evaluations:
#' n <- 6
#' res <- lapply(data[1], tuneRegionModel, pops=NULL,
#' control=list(funEvals=n, designControl=list(size=5), model = buildLM))
#' parsedList <- parseTunedRegionModel(res)
#' pred <- generateMCPrediction(testData = testData, models = parsedList$models, write = FALSE)
#' }
#' @export
#'
generateMCPrediction <- function(testData,
                               models,
                               startSimulation="2020-01-22",
                               write = FALSE){
    forecast <- data.frame()
    regionList <- levels(testData$Region)
    start <- as.Date(startSimulation)
    ## Note: start cannot be after startTest
    startTest <- min(testData$Date)
    endTest <- max(testData$Date)
    ## days = length of simulation period
    days <- 1 + as.integer(endTest - start)
    ## k = length of test period
    k <- as.double(endTest - startTest)
    for (i in 1:length(regionList)){
        regionName <- regionList[i]
        params <- models[models[,"region"] == regionName,]
        ##   (p, beta, gamma, CFR)
        N  <- params$regionPopulation
        p <- params$p
        beta <- params$beta
        gamma <- params$gamma
        CFR <- params$CFR
        x <- c(p, beta, gamma, CFR)
        ## Simulate from start ("2020-01-22")
        ## until end of test data
        m <- modelMarkovChain(x = x , days = days, N = N)
        r <- data.frame(ForecastId = (1 + days*(i-1)):(days*i),
                        ConfirmedCases = round(m$X2 + m$X3),
                        Fatalities = round(CFR *m$X3))
        ## Because start ("2020-01-22") can be before startTest
        ## select only results from the testPeriod
        r <- r[(days-k):days, ]
        r$ForecastId <- 1:(k+1)
        forecast <- rbind(forecast, r)
    }
    if (write){
        write.csv(forecast,
                  file = "submit.csv",
                  row.names=FALSE,
                  quote=FALSE)
    }
    return(forecast)
}






