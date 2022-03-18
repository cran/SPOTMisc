#' @title Get experimental design
#'
#' @description Based on \code{\link{expand.grid}} an experimental design
#' is generated. Factors are: nfactors, nnumericals, cardinality, data.seed etc.
#'
#' @return explan
#'
#' @export
#' @keywords internal
getExplan <- function() {
	model <- NULL
	## Settings that are varied
	nobs <- round(10 ^ seq(
		from = 4,
		to = 5,
		length.out = 5
	))
	nfactors = c("low", "med", "high")
	nnumericals = c("low", "med", "high")
	cardinality = c("low", "med", "high")
	data.seed = c(1, 2, 3) ## rng seed for subsampling the data
	tuner.seed = c(1, 2, 3) ## rng seed for repeated tuner runs, e.g., seedSPOT
	tuners = c("SPOT", "RS") ## note: runs with defaults will be handled separately
	models <- c("rpart", "cvglmnet", "ranger", "xgboost", "kknn", "svm")

		explan <- expand.grid(
		nobs = nobs,
		nfactors = nfactors,
		nnumericals = nnumericals,
		cardinality = cardinality,
		data.seed = data.seed,
		tuner.seed = tuner.seed,
		model = models,
		tuner = tuners
	)

	## Reduce experiment plan
	explan <-
		subset(explan, subset = !(nfactors == "med" &
																nnumericals == "low"))
	explan <-
		subset(explan, subset = !(nfactors == "med" &
																nnumericals == "high"))
	explan <-
		subset(explan, subset = !(nfactors == "high" &
																nnumericals == "med"))
	explan <-
		subset(explan, subset = !(nfactors == "low" &
																nnumericals == "med"))
	explan <-
		subset(explan, subset = !(nfactors == "low" &
																nnumericals == "low"))
	explan <-
		subset(explan, subset = !(nobs > 60000 &
																(model == "kknn" | model == "svm")))

	## cardinality doesnt matter if nfactors at low setting
	explan[explan$nfactors == "low", "cardinality"] <- NA
	explan <- unique(explan)

	## specify time limit for tuner
	explan$timebudget <- 3600 * 5
	explan$timebudget[explan$model == "rpart"] <- 300
	explan$timebudget[explan$model == "cvglmnet"] <- 3600

	## specify timeout
	explan$timeout <- explan$timebudget / 20
	## no timeout for rpart, to reduce overhead
	explan$timeout[explan$model == "rpart"] <- NA

	## Time required
	#sum(explan$timebudget)/3600/24/120

	## Length of plan
	#nrow(explan)

	return(explan)
}


















