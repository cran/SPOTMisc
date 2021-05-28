context("test markov")

## check for regionTrain and related data from babsim.data
# babsim.data.available <- FALSE

# skip_if_no_data <- function() {
#   if (babsim.data.available == FALSE) {
#     skip("babsim.data not available")
#   }
# }

test_that("Check SIR model compartments (plausibility).", {
  skip_on_cran()
  N <- 100
  p <- 0.9
  beta <- 0.9
  gamma <- 0.01
  x <- c(p, beta, gamma)
  days <- 100
  m <- modelMarkovChain(x = x, days = days, N = N)
  ## test if number of susceptible is always smaller than the number of infected
  expect_true((sum(m$X1 <  m$X2)) == days)
})


test_that("evalMarkovChain: check RMSE calculation (plausibility).", {
    # skip_if_no_data()
    skip_on_cran()
    data <- preprocessInputData(regionTrain, regionPopulation)
    set.seed(123)
    data <- data[[1]]
    N <- attr(data, "regionPopulation")
    ## x = (p, beta, gamma, CFR)
    x <- c(0.01, 0.01, 0.1, 0.01)
    conf <- list(regionData = data, N = N)
    res <- evalMarkovChain(x = x, conf=conf)
    ## test if RMSE is positive or zero:
    expect_true(res >= 0)
})


test_that("tuneRegionModel: check spot tuner on SIR models.",{
  # skip_if_no_data()
    skip_on_cran()
  if(getOption("spotmisc.run.full.test")){
 data <- preprocessInputData(regionTrain, regionPopulation)
 n <- 6
 res <- lapply(data[1], tuneRegionModel, pops=NULL, control=list(funEvals=n, designControl=list(size=5), model = SPOT::buildLM))
 ## budget exhausted?
 expect_true(res[[1]]$count == n)
  }
})

test_that("parseTunedRegionModel: check parsedList result layout (dimensions).",{
  # skip_if_no_data()
    skip_on_cran()
    # number of function evaluations:
    n <- 6
    # data <- preprocessInputData(regionTrain, regionPopulation)
    # res <- lapply(data[1], tuneRegionModel, pops=NULL, control=list(funEvals=n, designControl=list(size=5), model = SPOT::buildLM))
    res <- resTuneRegionModel
    parsedList <- parseTunedRegionModel(res)
    expect_true(dim( parsedList$pops[[1]])[1] == n )
})

test_that("generateMCPrediction: check predictions (generateMCPrediction).",{
  # skip_if_no_data()
  skip_on_cran()
  if(getOption("spotmisc.run.full.test")){
# data <- preprocessInputData(regionTrain, regionPopulation)
testData <- preprocessTestData(regionTest)
testData <- testData[testData$Region==levels(testData$Region)[1], ]
testData$Region <- droplevels(testData$Region)
# n <- 6
# res <- lapply(data[1], tuneRegionModel, pops=NULL, control=list(funEvals=n, designControl=list(size=5), model = SPOT::buildLM))
  res <- resTuneRegionModel
  parsedList <- parseTunedRegionModel(res)
  pred <- generateMCPrediction(testData = testData, models = parsedList$models, write = FALSE)
  expect_true(dim(pred)[2] == 3)
  }
})

test_that("preprocessInputData: check data preparation (preprocessInputData): is the number of regions (313) correct?",{
  # skip_if_no_data()
  skip_on_cran()
  data <- preprocessInputData(regionTrain, regionPopulation)
  # There should be 313 regions:
  expect_true(length(data) == 313)
})

test_that("plotPrediction: check plot", {
  # skip_if_no_data()
  skip_on_cran()
  if(getOption("spotmisc.run.full.test")){
  # data <- preprocessInputData(regionTrain, regionPopulation)
  testData <- preprocessTestData(regionTest)
  # Select the first region:
  testData <- testData[testData$Region==levels(testData$Region)[1], ]
  testData$Region <- droplevels(testData$Region)
  # # Very small number of function evaluations:
  # n <- 6
  # res <- lapply(data[1], tuneRegionModel, pops=NULL,
  #               control=list(funEvals=n, designControl=list(size=5), model = SPOT::buildLM))
  res <- resTuneRegionModel
  parsedList <- parseTunedRegionModel(res)
  pred <- generateMCPrediction(testData = testData, models = parsedList$models, write = FALSE)
  quickPredict <- cbind(pred, testData$Date, testData$Region)
  names(quickPredict) <- c("ForecastID", "confirmed", "fatalities", "date", "region")
  p <- plotPrediction(quickPredict, 1)
  # plot returns NULL, so we test for equality:
  expect_null(p)
  }
})


