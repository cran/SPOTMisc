#' @title  modelMarkovChain
#'
#' @description
#' Modeling continuous time Markov chains (MarkovChain) models using
#' \code{\link[SimInf]{SIR}} models.
#'
#' @details
#' SIR considers three compartments: S (susceptible), I (infected), and R (recovered).
#' Using the parameter vector x, the population size N, and the number of days
#' (prediction horizon),  the SIR model parameters are determined as follows.
#' N denotes the population size. First: S, the number of susceptible in each node,
#' will be calculated as N - I - R, where I is the number of infected in each node, and
#' R is the number of recovered in each node.
#'  Then, the data frame `u0` is set up:
#' \code{u0 = data.frame(S , I , R)}.
#' `u0` contains the initial number of individuals in each compartment in every node. An integer matrix (Ncomp × Nnodes)
#' is used for storing `u0` information.
#' The timespan is calculated as \code{tspan = 1:days}.
#' The \code{\link[SimInf]{SIR}} is set up and run, using \code{\link[SimInf]{run}}.
#'
#' Data are taken from the \code{regionTrain} and \code{regionPopulation} data sets
#' that were combined using the \code{\link{preprocessInputData}} function.
#' \code{regionTrain} and \code{regionPopulation} are stored in the
#' \code{babsim.data} package.
#'
#' @param x vector of three parameters. Used for parametrizing the MarkovChain model.
#' \describe{
#'		\item{\code{p}}{num  [0;1] proportion of confirmed cases}
#'		\item{\code{beta}}{num  A numeric vector with the transmission rate from susceptible to infected where each node can have
#'		a different beta value. The vector must have length 1 or nrow(u0).
#'		If the vector has length 1, but the model contains more nodes,
#'		the beta value is repeated in all nodes.}
#'		\item{\code{gamma}}{num  A numeric vector with the recovery rate from infected to recovered where each node can have
#'		a different gamma value. The vector must have length 1 or nrow(u0).
#'		If the vector has length 1, but the model contains more nodes, the beta value is repeated in all nodes.}
#'	    }
#'
#' @param days number of simulation steps, usually days (int).
#' It will be used to generate (internally) a vector (length >= 1)
#' of increasing time points where the state of each node is to be returned.
#'
#' @param N population size
#'
#' @param n number of nodes to be evaluated in the \code{\link[SimInf]{SIR}} model
#'
#' @return data.frame of days obs. of 4 variables:
#' \describe{
#'		\item{\code{t}}{num  0 1 2 3 4 5 6 7 8 9 ... (timesteps)}
#'		\item{\code{X1}}{num  1704 1490 1275 1069 880 ... (susceptible)}
#'		\item{\code{X2}}{num  1000 1178 1351 1509 1646 ... (infected)}
#'		\item{\code{X3}}{num  num  0 36.3 78.5 126.2 178.8 ... (recovered)}
#' }
#' @importFrom SimInf SIR
#' @importFrom SimInf trajectory
#' @importFrom SimInf run
#' @importFrom stats aggregate
#' @examples
#' \donttest{
#' require("SimInf")
#' data <- preprocessInputData(regionTrain, regionPopulation)
#' regionData <- data[[1]]
#' N <- attr(regionData, "regionPopulation")
#' # N_curr <- max(regionData$confirmed)
#' p <- 0.01
#' beta <- 0.1
#' gamma <- 0.01
#' # parameter vector for the SIR model: (p, beta, gamma)
#' x <- c(p, beta, gamma)
#' # Every row in the data represents one day:
#' days <- nrow(regionData)
#' modelMarkovChain(x = x, days = days, N = N)
#' }
#' @export
#'
modelMarkovChain <- function(x, days, N, n = 3){
    I  <- round(x[1] * N)
    beta  <- x[2]
    gamma <- x[3]
    R <- 0
    # Ensure that S is greater than or equal zero:
    S <- max(N - I - R, 0)
    u0 <- data.frame(S = rep(S, n), I = rep(I, n), R = rep(R, n))
    model <- SIR(u0,
             tspan = 1:days,
             beta = beta,
             gamma = gamma)
    # result <- run(model, seed = 123)
    result <- run(model)
    #df <- trajectory(model = result, node = 1)
    df <- trajectory(model = result)
    df <- aggregate(x=df, by= list(df$time), FUN = "mean")
    df <- data.frame(t = df$time, X1 = df$S, X2 = df$I, X3 = df$R)
    #df$node <- NULL
    #names(df) <- c("t", "X1", "X2", "X3")
    return(df)
}

#' @title
#' preprocessInputData
#'
#' @description
#' Combine information from the \code{regionTrain} and the \code{regionPopulation}
#' data sets.
#'
#' @details
#' Resulting data set can be used as input for the COVID-19 simulations of the
#' \code{\link{tuneRegionModel}} function.
#'
#' @param trainData data frame
#' @param populationData data frame
#' @return A large list (313 elements, 1.3 MB) containing the following information (3 lists) for each of
#' the 313 regions:
#' \describe{
#'      \item{\code{date}}{Date}
#'      \item{\code{confirmed}}{num Number of confirmed cases (cumulative)}
#'      \item{\code{fatalities}}{num Number of fatalities (cumulative)}
#' }
#'
#' @examples
#' x <- preprocessInputData(regionTrain, regionPopulation)
#' # Plot confirmed cases from the first country (Afghanistan):
#' p <- plot(x[[1]]$date, x[[1]]$confirmed)
#' @export
#'
preprocessInputData <- function(trainData,
                                populationData) {
    trainData$Region <-  ifelse(is.na(trainData$Province_State),
               paste(trainData$Country_Region
                     , ""
                     , sep = "/"),
               paste(trainData$Country_Region
                     , trainData$Province_State
                     , sep = "/")
        )

    regionNames <- levels(factor(trainData$Region))
    population <- populationData
    regions <- list()
    for (rn in regionNames) {
        regionData <- trainData[trainData$Region == rn, ]
        regionDf <- data.frame(
            date = as.Date(regionData$Date),
            confirmed = regionData$ConfirmedCases,
            fatalities = regionData$Fatalities
        )
        attr(regionDf, "regionName") <- rn
        attr(regionDf, "regionPopulation") <- population[population$regionName == rn,]$population
        regions[[rn]] <- regionDf
    }
    return(regions)
}

#' @title preprocessTestData
#'
#' @description
#' Combine Province/State and Country/Region information.
#'
#' @details
#' Locality information is merged with the \code{\link{paste}} command as follows:
#' \code{paste(testData$Country_Region, testData$Province_State, sep="/")}.
#' 4-dim data is reduced to 3-dim data.
#'
#' @param testData data frame with $n$ obs. of  4 variables:
#' \describe{
#' \item{\code{ForecastId}}{int Identifier 1 2 3 ...}
#' \item{\code{Province_State}}{chr Province/State}
#' \item{\code{Country_Region}}{chr Country/Region, e.g., "Afghanistan" ...}
#' \item{\code{Date}}{Date, format: "2020-04-02"  ...}
#'}
#'
#' @return A data frame that can be processed by \code{\link{generateMCPrediction}}
#' to predict results on test data using the tuned \code{\{link{modelMarkovChain}} model.
#' Data.frame with $n$ obs. of  3 variables:
#' \describe{
#' \item{\code{ForecastId}}{int Identifier 1 2 3 ...}
#' \item{\code{Region}}{Factor w/ m levels, e.g., "Afghanistan/",..: 1 1 1 1 1 1 1 1 1 1 ...}
#' \item{\code{Date}}{Date, format: "2020-04-02"  ...}
#'}
#'
#' @examples
#' testData <- preprocessTestData(regionTest)
#' @export
#'
preprocessTestData <- function(testData)
{
    regions <- paste(testData$Country_Region, testData$Province_State, sep="/")
    data.frame(ForecastId=testData$ForecastId,
               Region=factor(regions),
               Date=as.Date(testData$Date))
}

#' @title preprocessCdeTestData
#'
#' @description
#' Process Corona Data Explorer (CDE) Data.
#' Rename variables and factorize location information.
#'
#' @details
#' The variable \code{location} is renamed to \code{Region} and converted
#' to a factor variable.
#'
#' @param testData data frame with location information:
#' \describe{
#' \item{\code{ForecastId}}{int Identifier 1 2 3 ...}
#' \item{\code{Province_State}}{chr Province/State}
#' \item{\code{Country_Region}}{chr Country/Region, e.g., "Afghanistan" ...}
#' \item{\code{Date}}{Date, format: "2020-04-02"  ...}
#'}
#'
#' @return A data frame that can be processed by \code{\link{generateMCPrediction}}
#' to predict results on test data using the tuned \code{\{link{modelMarkovChain}} model.
#' Data.frame with $n$ obs. of  3 variables:
#' \describe{
#' \item{\code{ForecastId}}{int Identifier 1 2 3 ...}
#' \item{\code{Region}}{Factor w/ m levels, e.g., "Afghanistan/",..: 1 1 1 1 1 1 1 1 1 1 ...}
#' \item{\code{Date}}{Date, format: "2020-04-02"  ...}
#'}
#'
#' @export
#'
preprocessCdeTestData <- function(testData)
{   forecastId <- 1:length(testData$location)
    data.frame(ForecastId=forecastId,
               Region=factor(paste0(testData$location,"/")),
               Date=as.Date(testData$date))
}


#' @title plotRegionByName
#'
#' @description
#' Plot confirmed cases and fatalities (cumulative) for one country/region by region name.
#' @seealso \code{\link{plotRegion}}.
#'
#' @details
#' Data are taken from the \code{regionTrain} and \code{regionPopulation} data sets
#' that were combined using the \code{\link{preprocessInputData}} function.
#' \code{regionTrain} and \code{regionPopulation} are stored in the
#' \code{babsim.data} package.
#'
#' @param regionList A list containing a representation of the data.
#' @param country Name of a country from the list dataList
#' @importFrom graphics lines
#' @importFrom graphics plot
#'
#' @return A plot
#'
#' @examples
#' regionList <- preprocessInputData(regionTrain, regionPopulation)
#' p <- plotRegionByName(regionList = regionList, country = "Germany")
#' @export
#'
plotRegionByName <- function(regionList, country="Germany") {
    plot_colors <- c("blue", "red")
    country <- paste0(country,"/")
    regionList <- regionList[names(regionList) == country]
    attr(regionList, "names") <- NULL
    regionList <- as.data.frame(regionList)
    {plot(regionList$confirmed ~ regionList$date,
          col = plot_colors[1],
          type = "l"      ,
          main = paste0(country, "COVID-19. From ", regionList$date[1], " to ", regionList$date[length(regionList$date)]))
        lines(regionList$fatalities ~ regionList$date,
              col = plot_colors[2])}
}

#' @title plotRegion
#'
#' @description
#' Plot confirmed cases and fatalities (cumulative) for one country/region by index.
#' @seealso \code{\link{plotRegionByName}}.
#'
#' @details
#' Data are taken from the \code{regionTrain} and \code{regionPopulation} data sets
#' that were combined using the \code{\link{preprocessInputData}} function.
#' \code{regionTrain} and \code{regionPopulation} are stored in the
#' \code{babsim.data} package.
#'
#'
#' @param regionList A list containing a representation of the data.
#' @param countryIndex num Index
#' @importFrom graphics lines
#' @importFrom graphics plot
#'
#' @return A plot
#'
#' @examples
#' regionList <- preprocessInputData(regionTrain, regionPopulation)
#' p <- plotRegion(regionList = regionList, countryIndex = 1)
#'
#' @export
#'
plotRegion <- function(regionList, countryIndex=1) {
    plot_colors <- c("blue", "red")
    cName <- names(regionList[countryIndex])
    regionList <- regionList[[countryIndex]]
    regionList <- as.data.frame(regionList)
    {plot(regionList$confirmed ~ regionList$date,
          col = plot_colors[1],
          type = "l"      ,
          main = paste0(cName, "COVID-19. ", regionList$date[1], " to ", regionList$date[length(regionList$date)]))
        lines(regionList$fatalities ~ regionList$date,
              col = plot_colors[2])}
}


#' @title plotPrediction
#'
#' @description  plot predictions countries/regions by index
#'
#' @details
#' Data are taken from the \code{regionTrain} and \code{regionPopulation} data sets
#' that were combined using the \code{\link{preprocessInputData}} function.
#' \code{regionTrain} and \code{regionPopulation} are stored in the
#' \code{babsim.data} package.
#'
#' @param regionDf A list containing a representation of the data.
#' @param countryIndex num Index
#' @param ylog logical plot log y axis (log = y)
#' @importFrom graphics lines
#' @importFrom graphics plot
#'
#' @return A plot
#'
#' @examples
#' \donttest{
#' require(SPOT)
#' data <- preprocessInputData(regionTrain, regionPopulation)
#' testData <- preprocessTestData(regionTest)
#' # Select the first region:
#' testData <- testData[testData$Region==levels(testData$Region)[1], ]
#' testData$Region <- droplevels(testData$Region)
#' # Very small number of function evaluations:
#' n <- 6
#' res <- lapply(data[1], tuneRegionModel, pops=NULL,
#'                control=list(funEvals=n, designControl=list(size=5), model = buildLM))
#' parsedList <- parseTunedRegionModel(res)
#' pred <- generateMCPrediction(testData = testData, models = parsedList$models, write = FALSE)
#' quickPredict <- cbind(pred, testData$Date, testData$Region)
#' names(quickPredict) <- c("ForecastID", "confirmed", "fatalities", "date", "region")
#' p <- plotPrediction(quickPredict, 1)
#' }
#' @export
#'
plotPrediction <- function(regionDf, countryIndex=1, ylog=FALSE) {
    regionDf$confirmed <- regionDf$confirmed/1e3
    plot_colors <- c("blue", "red")
    cName <- levels(regionDf$region)[countryIndex]
    regionDf <- regionDf[regionDf$region == cName, ]
    if(ylog == FALSE)
    {plot(x=regionDf$date, y=regionDf$confirmed ,
          col = plot_colors[1],
          type = "l" ,
          ylim = c( min(regionDf$confirmed, regionDf$fatalities), max(regionDf$confirmed, regionDf$fatalities)),
          main = paste0(cName, "COVID-19. ", regionDf$date[1], " to ", regionDf$date[length(regionDf$date)]))
          lines(regionDf$fatalities ~ regionDf$date,
              col = plot_colors[2])}
    else {plot(x= regionDf$date, y= regionDf$confirmed ,
          col = plot_colors[1],
          type = "l" ,
          xlab = "Days",
          ylab = "Confirmed cases in 1000, Fatalities",
          log = "y",
          ylim = c( min(regionDf$confirmed, regionDf$fatalities), max(regionDf$confirmed, regionDf$fatalities)),
          main = paste0(cName, "COVID-19. ", regionDf$date[1], " to ", regionDf$date[length(regionDf$date)]))
          lines(regionDf$fatalities ~ regionDf$date,
              col = plot_colors[2],
              xlab = "Days",
              ylab = "Confirmed cases in 1000, Fatalities"
              )}
}



#' @title
#' preprocessCdeInputData
#'
#' @description
#' Prepare Ced Data for SIR modeling
#'
#' @details
#' Resulting data set can be used as input for the COVID-19 simulations of the
#' \code{\link{tuneRegionModel}} function.
#'
#' @param cdeData data frame
#'
#' @return A large list containing the following information (3 lists) for each region:
#' \describe{
#'      \item{\code{date}}{Date}
#'      \item{\code{confirmed}}{num Number of confirmed cases (cumulative)}
#'      \item{\code{fatalities}}{num Number of fatalities (cumulative)}
#' }
#'
#' @export
#'
preprocessCdeInputData <- function(cdeData) {
    n <- dim(cdeData)[1]
    populationLocation  <- unique(data.frame( population= cdeData$population, regionName=paste0(cdeData$location,"/")))
    cdeData <- data.frame(Country_Region = cdeData$location,
                           Date = cdeData$date,
                           ConfirmedCases = cdeData$total_cases,
                           Fatalities = cdeData$total_deaths)

    cdeData$Province_State = rep(NA,n)

    cdeData$Region <-  ifelse(is.na(cdeData$Province_State),
                                paste(cdeData$Country_Region
                                      , ""
                                      , sep = "/"),
                                paste(cdeData$Country_Region
                                      , cdeData$Province_State
                                      , sep = "/")
    )
    regionNames <- levels(factor(cdeData$Region))

    regions <- list()
    for (rn in regionNames) {
        regionData <- cdeData[cdeData$Region == rn, ]
        regionDf <- data.frame(
            date = as.Date(regionData$Date),
            confirmed = regionData$ConfirmedCases,
            fatalities = regionData$Fatalities
        )
        attr(regionDf, "regionName") <- rn
        attr(regionDf, "regionPopulation") <- populationLocation[populationLocation$regionName == rn,]$population
        regions[[rn]] <- regionDf
    }
    return(regions)
}


#' @title plotSIRModel
#'
#' @description
#' Plot of continuous time Markov chains (MarkovChain)
#'  \code{\link[SimInf]{SIR}} models.
#'
#' @details
#' SIR considers three compartments: S (susceptible), I (infected), and R (recovered).
#' The timespan is calculated as \code{tspan = 1:days}.
#'
#' @param x vector of four parameters. Used for parametrizing the MarkovChain model and calulation of fatalities.
#' \describe{
#'		\item{\code{p}}{num  [0;1] proportion of confirmed cases}
#'		\item{\code{beta}}{num  A numeric vector with the transmission rate from susceptible to infected where each node can have
#'		a different beta value. The vector must have length 1 or nrow(u0).
#'		If the vector has length 1, but the model contains more nodes,
#'		the beta value is repeated in all nodes.}
#'		\item{\code{gamma}}{num  A numeric vector with the recovery rate from infected to recovered where each node can have
#'		a different gamma value. The vector must have length 1 or nrow(u0).
#'		If the vector has length 1, but the model contains more nodes, the beta value is repeated in all nodes.}
#'	    \item{\code{CFR}}{num  [0;1] proportion of fatalities}
#'	    }
#'
#' @param days number of simulation steps, usually days (int).
#' It will be used to generate (internally) a vector (length >= 1)
#' of increasing time points where the state of each node is to be returned.
#'
#' @param N population size
#'
#' @param n number of nodes to be evaluated in the \code{\link[SimInf]{SIR}} model
#'
#' @param logy logical Plot logarithmic y axis. Default: FALSE
#'
#' @return plot
#'
#' @importFrom SimInf SIR
#' @importFrom SimInf trajectory
#' @importFrom SimInf run
#' @importFrom stats aggregate
#' @importFrom graphics plot
#'
#' @examples
#' \donttest{
#' require("SimInf")
#' # Result from \code{\link{parseTunedRegionModel}}, e.g., deModels:
#' # parameters: x = c(deModels$p, deModels$beta, deModels$gamma, deModels$CFR)
#' x = c(1e-05, 0.216764668858674, 0.204440265426977, 0.100982384347174)
#' plotSIRModel(x, days=1000, N = 83783945, n=10, logy=TRUE)
#' }
#' @export
#'
plotSIRModel <- function(x, days, N, n = 3, logy = FALSE){
    I  <- round(x[1] * N)
    beta  <- x[2]
    gamma <- x[3]
    CFR <- x[4]
    R <- 0
    # Ensure that S is greater than or equal zero:
    S <- max(N - I - R, 0)
    u0 <- data.frame(S = rep(S, n), I = rep(I, n), R = rep(R, n))
    model <- SIR(u0,
                 tspan = 1:days,
                 beta = beta,
                 gamma = gamma)
    # result <- run(model, seed = 123)
    result <- run(model)
    #plot(result)
    #df <- trajectory(model = result, node = 1)
    df <- trajectory(model = result)
    df <- aggregate(x=df, by= list(df$time), FUN = "median")
    df <- data.frame(t = df$time, X1 = df$S, X2 = df$I, X3 = df$R)
    # return(df)
    hatI <- df$X2 + df$X3
    ## CFR = fatalities / recovered:
    hatF <- df$X3 * CFR

    if (logy)
        plot(1:days, hatI, type="l", col="blue", log="y")
        else
        plot(1:days, hatI, type="l", col="blue")
    lines(1:days, hatF, col="red")
}






