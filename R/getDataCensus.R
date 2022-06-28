#' @title Get Census KDD data set (+variation)
#'
#' @description  This function downloads (or loads from cache folder) the Census
#' KDD Dataset (OpenML ID: 4535).
#' If requested, data set is changed w.r.t the number of observations, number of
#' numerical/categorical feature,
#' the cardinality of the categorical features, and the task type (regr. or classif).
#'
#' @param task.type character, either "classif" or "regr".
#' @param nobs integer, number of observations uniformly sampled from the full data set.
#' @param nfactors character, controls the number of factors (categorical features) to use.
#' Can be "low", "med", "high", or "full" (full corresponds to original data set).
#' @param nnumericals character, controls the number of numerical features to use.
#' Can be "low", "med", "high", or "full" (full corresponds to original data set).
#' @param cardinality character, controls the number of factor levels (categories)
#' for the categorical features. Can be "low", "med", "high" (high corresponds to original data set).
#' @param data.seed integer, this will be used via set.seed() to make the random subsampling reproducible.
#'  Will not have an effect if all observations are used.
#' @param cachedir character. The cache directory, e.g., \code{"oml.cache"}.
#' Default: \code{"oml.cache"}.
#' @param cache.only logical. Only try to retrieve the object from cache.
#' Will result in error if the object is not found. Default is TRUE.
#' @param target character "age" or "income_class". If \code{target = age}, the
#' numerical varible \code{age} is converted to a factor:
#' \code{age<-as.factor(age<40)}
#'
#' @return census data set
#'
#' @import OpenML
#'
#' @examples
#' \donttest{
#' ## Example downloads OpenML data, might take some time:
#' task.type <- "classif"
#' nobs <- 1e4 # max: 229285
#' data.seed <- 1
#' nfactors <- "full"
#' nnumericals <- "low"
#' cardinality <- "med"
#' censusData <- getDataCensus(
#'   task.type = task.type,
#'   nobs = nobs,
#'   nfactors = nfactors,
#'   nnumericals = nnumericals,
#'   cardinality = cardinality,
#'   data.seed = data.seed,
#'   cachedir = "oml.cache",
#'   target="age")
#'   }
#'
#' @export
getDataCensus <- function(task.type="classif",
                          nobs=50000,
                          nfactors = "high",
                          nnumericals = "high",
                          cardinality = "high",
                          data.seed=1,
                          cachedir="oml.cache",
                          target = NULL,
                          cache.only = FALSE
                          ){
  ## set API key to read only key
  setOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f",
               cachedir = cachedir) # default cachedir is only temporary.

  ## load CENSUS data


  dataset <- try(
    getOMLDataSet(data.id=4535,
                  cache.only=cache.only)$data)

  if(inherits(dataset, "try-error")){
    dataset <- SPOTMisc::dataCensus
    message("getOMLDataSet() failed: Using reduced local dataset 'dataCensus' from packae SPOTMisc.")
    return(dataset)}

  if(is.na(cardinality))
    cardinality <- "low"

    #Note: for slightly better balance, we could also restrict the dataset to >18yold
  #dataset <- subset(dataset,subset=V1>18)

  ## target column, to be ignored during task variation
  if(task.type=="classif"){
    #target <- "capital_gains"
    #summary(dataset$V17)
    #dataset$V17 <- as.factor(dataset$V17<400)
    #target <- "wage_per_hour"
    #summary(dataset$V6)
    #dataset$V6 <- as.factor(dataset$V6<55)
    #target <- "divdends_from_stocks"
    #summary(dataset$V19)
    #dataset$V19 <- as.factor(dataset$V19>0)
    #target <- "income_class"
    ## FIXME commented?
    #target <- "age"
    if(target == "age"){
      message("getDataCensus: Converting target age to factor.")
    dataset$V1 <- as.factor(dataset$V1<40)
    }
  }else if(task.type=="regr"){
    #target <- "capital_gains"
    #target <- "wage_per_hour"
    #target <- "divdends_from_stocks"
    target <- "age"
  }else{
    stop("invalid task specified in getTaskCensus")
  }
  #

  #target <- "age"
  #if(task.type == "classif")
  #  dataset$V1 <- as.factor(dataset$V1<35)

  ## specify properties of the data set variation

  ## age AAGE
  #summary(dataset$V1)
  ## numeric

  ## class of worker ACLSWKR
  #table(dataset$V2)
  translations <- list(government = c(" Federal government"," Local government", " State government"),
                       private = c(" Private"),
                       other = c(" Never worked" ," Self-employed-incorporated"," Self-employed-not incorporated", " Without pay"),
                       unknown = c(" Not in universe"))
  if(cardinality=="low"){
    dataset$V2 <- translate_levels(dataset$V2,translations)
  }
  ## reduction: 9 ->  4



  ## industry code ADTIND
  ### should be factor not integer
  dataset$V3 <- as.factor(dataset$V3)
  #table(dataset$V3)
  if(cardinality!="high"){
    dataset$V3 <- NULL
  }
  ## reduction: 52 -> 0
  ## remove, becomes redundant with V9



  ## occupation code ADTOCC
  ### should be facor not integer
  dataset$V4 <- as.factor(dataset$V4)
  #table(dataset$V4)
  if(cardinality!="high"){
    dataset$V4 <- NULL
  }
  ## reduction: 46 -> 0
  ## remove, becomes redundant with V10



  ## education AHGA
  #table(dataset$V5)
  translations <- list(children = " Children",
                       lt.highschool = c(" Less than 1st grade",
                                         " 9th grade"," 10th grade"," 11th grade"," 12th grade no diploma",
                                         " 1st 2nd 3rd or 4th grade"," 5th or 6th grade"," 7th and 8th grade"),
                       highschool = c(" High school graduate"),
                       gt.highschool = c(" Associates degree-academic program", " Associates degree-occup /vocational",
                                         " Bachelors degree(BA AB BS)", " Doctorate degree(PhD EdD)",
                                         " Masters degree(MA MS MEng MEd MSW MBA)",
                                         " Prof school degree (MD DDS DVM LLB JD)",
                                         " Some college but no degree"
                       ))
  if(cardinality!="high"){
    dataset$V5 <- translate_levels(dataset$V5,translations)
  }
  ## reduction: 17 -> 4



  ## wage per hour AHRSPAY
  #summary(dataset$V6)
  #sum(dataset$V6>0)
  ## numeric



  ## enrolled in edu inst last wk AHSCOL
  #table(dataset$V7)
  ## no reduction



  ## marital status AMARITL
  #table(dataset$V8)
  translations <- list(married = c(" Married-A F spouse present"," Married-civilian spouse present"," Married-spouse absent"," Separated"),
                       notmarried = c(" Divorced", " Never married", " Widowed")
  )
  if(cardinality=="low"){
    dataset$V8 <- translate_levels(dataset$V8,translations)
  }
  ##reduction: 7 -> 2



  ## major industry code AMJIND
  #table(dataset$V9)
  ## merge into divisions, see https://en.wikipedia.org/wiki/Standard_Industrial_Classification#Range
  ## see also https://www2.census.gov/programs-surveys/cps/methodology/Industry%20Codes.pdf
  translations <- list(unknown = c(" Not in universe or children"),
                       agri = c(" Agriculture",
                                " Forestry and fisheries"),
                       mining = " Mining",
                       construction = " Construction",
                       manufacturing = c(" Manufacturing-durable goods",
                                         " Manufacturing-nondurable goods"),
                       transport = c(" Communications"," Transportation"),
                       services = c(" Social services",
                                    " Utilities and sanitary services",
                                    " Other professional services",
                                    " Personal services except private HH",
                                    " Private household services",
                                    " Business and repair services",
                                    " Hospital services",
                                    " Medical except hospital",
                                    " Entertainment",
                                    " Education",
                                    " Armed Forces"
                                    ),
                       wholesale = " Wholesale trade",
                       retail = " Retail trade",
                       finances = " Finance insurance and real estate",
                       public = " Public administration"
  )
  if(cardinality=="low"){
    dataset$V9 <- translate_levels(dataset$V9,translations)
  }
  ## reduction: 24 -> 11



  ## major occupation code AMJOCC
  #table(dataset$V10)
  ## no reduction: 15



  ## mace ARACE
  #levels(dataset$V11)
  ## no reduction: 5



  ## hispanic Origin AREORGN
  #table(dataset$V12)
  translations <- list(unknown = c(" Do not know", " NA"),
                       hispanic = c(" Central or South American"," Chicano"," Cuban",
                                    " Mexican-American"," Mexican (Mexicano)"," Other Spanish"," Puerto Rican"),
                       other = c(" All other")
  )
  if(cardinality=="low"){
    dataset$V12 <- translate_levels(dataset$V12,translations)
  }
  ## reduction: 10 -> 3



  ## sex ASEX
  #table(dataset$V13)
  ## no reductions: 2



  ## member of a labor union AUNMEM
  #table(dataset$V14)
  ## no reductions: 3



  ## reason for unemployment AUNTYPE
  #table(dataset$V15)
  translations <- list(unemployed = c(" Job leaver"," Job loser - on layoff"," New entrant"," Other job loser"," Re-entrant"),
                       other = " Not in universe"
  )
  if(cardinality=="low"){
    dataset$V15 <- translate_levels(dataset$V15,translations)
  }
  ## reduction: 6 -> 2



  ## full or part time employment stat AWKSTAT
  #table(dataset$V16)
  translations <- list(childrenOrArmedForces = c(" Children or Armed Forces"),
                       noteInLaborForce = c(" Not in labor force"),
                       partTime = c(" PT for econ reasons usually FT"," PT for econ reasons usually PT",
                                    " PT for non-econ reasons usually FT"," Unemployed part- time"),
                       fullTime = c(" Full-time schedules"," Unemployed full-time")

  )
  if(cardinality=="low"){
    dataset$V16 <- translate_levels(dataset$V16,translations)
  }
  ## reduction: 8 -> 4



  ## capital gains CAPGAIN
  #summary(dataset$V17)
  ## numeric



  ## capital losses CAPLOSS
  #summary(dataset$V18)
  ## numeric



  ## divdends from stocks DIVVAL
  #summary(dataset$V19)
  ## numeric



  ## federal income tax liability FEDTAX  ???
  ## tax filer status FILESTAT  ???
  #summary(dataset$V20)
  translations <- list(filer = c(" Head of household"," Joint both 65+"," Joint both under 65",
                                 " Joint one under 65 & one 65+"," Single"),
                       nonfiler = c(" Nonfiler")

  )
  if(cardinality=="low"){
    dataset$V20 <- translate_levels(dataset$V20,translations)
  }
  ## no reduction: 6 -> 2



  ## region of previous residence GRINREG
  #summary(dataset$V21)
  ## not reduced: 6



  ## state of previous residence GRINST
  #summary(dataset$V22)
  if(cardinality!="high"){
    dataset$V22 <- NULL
  }
  ## reduction: 51 -> 0
  ## i.e., remove entirely, since 21 is a reduced form of 22



  ## detailed household and family stat HHDFMX
  #summary(dataset$V23)
  if(cardinality!="high"){
    dataset$V23 <- NULL
  }
  ## Possible reductions: 38 -> 0
  ## remove entirely, see V24



  ## detailed household summary in household HHDREL
  #summary(dataset$V24)
  translations <- list(child = c(" Child 18 or older"," Child under 18 ever married"," Child under 18 never married"),
                       other = c(" Group Quarters- Secondary individual", " Nonrelative of householder",
                                 " Other relative of householder"),
                       householder = " Householder",
                       spouse = " Spouse of householder"
  )
  if(cardinality=="low"){
    dataset$V24 <- translate_levels(dataset$V24,translations)
  }
  ## reduction:  8 -> 4



  ## instance weight MARSUPWT
  ## remove instance weight. should not be used.
  dataset$V25 <- NULL



  ## migration code-change in msa MIGMTR1
  #summary(dataset$V26)
  translations <- list(unknown = c(" ?", " Not identifiable", " Not in universe"),
                       nonmover = " Nonmover",
                       mover = c( " Abroad to MSA"," Abroad to nonMSA"," MSA to MSA"," MSA to nonMSA",
                                  " NonMSA to MSA"," NonMSA to nonMSA")
  )
  if(cardinality=="low"){
    dataset$V26 <- translate_levels(dataset$V26,translations)
  }
  ## reduction:  10 -> 3



  ## migration code-change in reg MIGMTR3
  #summary(dataset$V27)
  if(cardinality=="low"){
    dataset$V27 <- NULL
  }
  ## reduction:  9 -> 0
  ## the reduction would be redundant to V26



  ## migration code-move within reg MIGMTR4
  #summary(dataset$V28)
  if(cardinality=="low"){
    dataset$V28 <- NULL
  }
  ## reduction:  10 -> 0
  ## the reduction would be redundant to V26



  ## live in this house 1 year ago MIGSAME
  #summary(dataset$V29)
  ## no reduction:  3



  ## migration prev res in sunbelt MIGSUN
  #summary(dataset$V30)
  translations <- list(unknown = c(" ?",  " Not in universe"),
                       no = " No",
                       yes = " Yes"
  )
  if(cardinality=="low"){
    dataset$V30 <- translate_levels(dataset$V30,translations)
  }
  ## reduction:  4 -> 3



  ## num persons worked for employer NOEMP
  #summary(dataset$V31)
  ## integer



  ## family members under 18 PARENT
  #summary(dataset$V32)
  ## no reduction: 5



  ## country of birth father PEFNTVTY
  #summary(dataset$V33)
  translationsMed <- list(unknown = c(" ?"),
                          asia = c(" Cambodia"," China", " Hong Kong"," India"," Iran"," Japan"," Laos"," Philippines"," South Korea",
                                   " Taiwan"," Thailand"," Vietnam"),
                          europe = c(" England"," France"," Germany"," Greece", " Holand-Netherlands"," Hungary"," Ireland"," Italy"," Poland",
                                     " Portugal"," Scotland"," Yugoslavia"),
                          northamerica = c(" Canada"," El-Salvador", " Cuba", " Dominican-Republic"," Guatemala", " Haiti",
                                           " Honduras"," Jamaica"," Mexico"," Nicaragua"," Panama"," Puerto-Rico",
                                           " Trinadad&Tobago"," United-States"),
                          southamerica = c(" Columbia"," Ecuador"," Peru"),
                          #africa = c(), ## none present?
                          other = c(" Outlying-U S (Guam USVI etc)")
  )
  translationsLow <- list(unknown = c(" ?"),
                          us = " United-States",
                          abroad = c(" Cambodia"," China", " Hong Kong"," India"," Iran"," Japan"," Laos"," Philippines"," South Korea",
                                     " Taiwan"," Thailand"," Vietnam",
                                     " England"," France"," Germany"," Greece", " Holand-Netherlands"," Hungary"," Ireland"," Italy"," Poland",
                                     " Portugal"," Scotland"," Yugoslavia",
                                     " Canada"," El-Salvador", " Cuba", " Dominican-Republic"," Guatemala", " Haiti",
                                     " Honduras"," Jamaica"," Mexico"," Nicaragua"," Panama"," Puerto-Rico",
                                     " Trinadad&Tobago"," Columbia"," Ecuador"," Peru",
                                     " Outlying-U S (Guam USVI etc)")
  )
  if(cardinality=="med"){
    dataset$V33 <- translate_levels(dataset$V33,translationsMed)
  }
  if(cardinality=="low"){
    dataset$V33 <- translate_levels(dataset$V33,translationsLow)
  }
  ## reduction:  43 -> 6, 43 ->3



  ## country of birth mother PEMNTVTY
  #summary(dataset$V34)
  if(cardinality=="med"){
    dataset$V34 <- translate_levels(dataset$V34,translationsMed)
  }
  if(cardinality=="low"){
    dataset$V34 <- translate_levels(dataset$V34,translationsLow)
  }
  ## reduction:  43 -> 6, 43 ->3



  ## country of birth self PENATVTY
  #summary(dataset$V35)
  if(cardinality=="med"){
    dataset$V35 <- translate_levels(dataset$V35,translationsMed)
  }
  if(cardinality=="low"){
    dataset$V35 <- translate_levels(dataset$V35,translationsLow)
  }
  ## reduction:  43 -> 6, 43 ->3



  ## citizenship PRCITSHP
  #summary(dataset$V36)
  translation <- list(uscitizen = c(" Foreign born- U S citizen by naturalization",
                                    " Native- Born abroad of American Parent(s)",
                                    " Native- Born in Puerto Rico or U S Outlying",
                                    " Native- Born in the United States"),
                      othercitizen = " Foreign born- Not a citizen of U S "
  )
  if(cardinality=="low"){
    dataset$V36 <- translate_levels(dataset$V36,translation)
  }
  ## reduction:  5 -> 2


  ## see here:
  #https://www.icpsr.umich.edu/web/ICPSR/studies/02825/variables?q=tax
  #https://www.icpsr.umich.edu/web/ICPSR/studies/02825/datasets/0001/variables/SEOTR?archive=icpsr
  #https://www.icpsr.umich.edu/web/ICPSR/studies/02825/datasets/0001/variables/VET-YN?archive=icpsr
  #https://www.icpsr.umich.edu/web/ICPSR/studies/02825/datasets/0001/variables/VET-QVA?archive=icpsr
  #https://www.icpsr.umich.edu/web/ICPSR/studies/02825/datasets/0001/variables/PTOTVAL?archive=ICPSR
  #https://www.icpsr.umich.edu/web/ICPSR/studies/02825/datasets/0001/variables/TAX-INC?archive=icpsr

  ## total person income PTOTVAL
  ## ?? own business or self employed SEOTR
  #summary(dataset$V37)
  ## integer, should be 3 level factor
  dataset$V37 <- as.factor(dataset$V37)



  ## ?? fill inc questionnaire for veteran's admin VETQVA
  #summary(dataset$V38)
  ## no reduction: 3



  ## ?? veterans benefits VETYN
  #summary(dataset$V39)
  ## integer, should be 3 level factor
  dataset$V39 <- as.factor(dataset$V39)



  ## weeks worked in year WKSWORK
  #summary(dataset$V40)
  ## integer



  ## moste likely: the year of the survey (1994, 1995)
  #summary(dataset$V41)
  ## integer



  ## target variable (income > or < 50k)
  #summary(dataset$V42)


  cnames <- c(V1="age",
              V2="class_of_worker",
              V3="industry_code",
              V4="occupation_code",
              #"adjusted_gross_income"
              V5="education",
              V6="wage_per_hour",
              V7="enrolled_in_edu_inst_last_wk",
              V8="marital_status",
              V9="major_industry_code",
              V10="major_occupation_code",
              V11="race",
              V12="hispanic_origin",
              V13="sex",
              V14="member_of_a_labor_union",
              V15="reason_for_unemployment" ,
              V16="full_or_part_time_employment_stat" ,
              V17="capital_gains" ,
              V18="capital_losses" ,
              V19="divdends_from_stocks" ,
              #"federal_income_tax_liability" ,
              V20="tax_filer_status" ,
              V21="region_of_previous_residence" ,
              V22="state_of_previous_residence" ,
              V23="detailed_household_and_family_stat" ,
              V24="detailed_household_summary_in_household" ,
              #"instance_weight" ,
              V26="migration_code_change_in_msa" ,
              V27="migration_code_change_in_reg" ,
              V28="migration_code_move_within_reg" ,
              V29="live_in_this_house_1_year_ago" ,
              V30="migration_prev_res_in_sunbelt" ,
              V31="num_persons_worked_for_employer" ,
              V32="family_members_under_18" ,
              #"total_person_earnings",
              V33="country_of_birth_father",
              V34="country_of_birth_mother",
              V35="country_of_birth_self",
              V36="citizenship",
              #"total_person_income",
              V37="own_business_or_self_employed",
              #"taxable_income_amount",
              V38="fill_inc_questionnaire_for_veterans_admin",
              V39="veterans_benefits",
              V40="weeks_worked_in_year",
              V41="year_of_survey",
              V42="income_class"
  )
  ## rename columns
  cnames_current <- colnames(dataset)
  for(i in 1:ncol(dataset)){
    cn <- cnames_current[i]
    colnames(dataset)[i] <- cnames[which(names(cnames) == cn)]
  }

  num.features.low <- c()
  num.features.med <- c("age","wage_per_hour",
                        "num_persons_worked_for_employer",
                        "divdends_from_stocks",
                        "weeks_worked_in_year")
  num.features.high <-c("age","wage_per_hour","capital_gains",
                        "capital_losses","divdends_from_stocks",
                        "num_persons_worked_for_employer",
                        "weeks_worked_in_year")
  num.features.full <- c("age","wage_per_hour","capital_gains",
                         "capital_losses","divdends_from_stocks",
                         "num_persons_worked_for_employer",
                         "weeks_worked_in_year",
                         "year_of_survey")

  cat.features.low <- c()
  cat.features.med <- c("class_of_worker",
                        "education",
                        "marital_status",
                        "industry_code",
                        "major_industry_code",
                        "sex",
                        "country_of_birth_self",
                        "income_class")
  cat.features.high <- c("class_of_worker",
                         "education",
                         "marital_status",
                         "occupation_code",
                         "major_occupation_code",
                         "industry_code",
                         "major_industry_code",
                         "sex",
                         "race",
                         "hispanic_origin",
                         "detailed_household_and_family_stat",
                         "detailed_household_summary_in_household",
                         "tax_filer_status",
                         "country_of_birth_self",
                         "citizenship",
                         "income_class")
  cat.features.full <- c("class_of_worker","industry_code","occupation_code",
                         "education","enrolled_in_edu_inst_last_wk","marital_status",
                         "major_industry_code","major_occupation_code","race",
                         "hispanic_origin","sex","member_of_a_labor_union",
                         "reason_for_unemployment","full_or_part_time_employment_stat","tax_filer_status",
                         "region_of_previous_residence","state_of_previous_residence","detailed_household_and_family_stat",
                         "detailed_household_summary_in_household","migration_code_change_in_msa","migration_code_change_in_reg",
                         "migration_code_move_within_reg","live_in_this_house_1_year_ago","migration_prev_res_in_sunbelt",
                         "family_members_under_18","country_of_birth_father","country_of_birth_mother",
                         "country_of_birth_self","citizenship","own_business_or_self_employed",
                         "fill_inc_questionnaire_for_veterans_admin","veterans_benefits","income_class")

  deletenames <- NULL
  factornames <- names(dataset)[lapply(dataset,class)=="factor"]
  if(task.type=="classif")
    factornames <- factornames[factornames != target]
  if(nfactors=="high")
    deletenames <- factornames[!factornames %in% cat.features.high]
  if(nfactors=="med")
    deletenames <- factornames[!factornames %in% cat.features.med]
  if(nfactors=="low")
    deletenames <- factornames[!factornames %in% cat.features.low]
  if(length(deletenames)>0)
    dataset[deletenames] <- NULL

  deletenames <- NULL
  numnames <- names(dataset)[lapply(dataset,class)=="numeric"]
  if(task.type=="regr")
    numnames <- numnames[numnames != target]
  if(nnumericals=="high")
    deletenames <- numnames[!numnames %in% num.features.high]
  if(nnumericals=="med")
    deletenames <- numnames[!numnames %in% num.features.med]
  if(nnumericals=="low")
    deletenames <- numnames[!numnames %in% num.features.low]
  if(length(deletenames)>0)
    dataset[deletenames] <- NULL

  # FIXME: Moved to getMlrTask(), can be deleted here:
  # task.nobservations <- nrow(dataset)
  # task.nfeatures <- ncol(dataset)
  # task.numericals <- lapply(dataset,class)!="factor"
  # task.numericals[target] <- FALSE
  # task.factors <- lapply(dataset,class)=="factor"
  # task.factors[target] <- FALSE
  # task.nnumericals <- sum(task.numericals)
  # task.nfactors <- sum(task.factors)
  # task.nlevels <- as.numeric(lapply(dataset,function(x)length(unique(x))))
  # task.nlevels[!task.factors] <- 0
  # task.nlevels.max <- max(task.nlevels)
  # FIXME END

  #print(task.nlevels)
  #task.factors[cat.features.high]

  nremove <- nrow(dataset) - nobs
  set.seed(data.seed)
  if(nremove>0){
    sample.ids <- sample(1:nrow(dataset),nobs,replace=FALSE)
    dataset <- dataset[sample.ids,]
    for(i in 1:ncol(dataset)){
      #if(class(dataset[,i])=="factor")
      if(inherits(dataset[,i], "factor"))
        dataset[,i] <- droplevels(dataset[,i])
    }
  }
  ## New in 1.16.2:
  ## target is mapped to column "target"
  dataset$target <- dataset[[target]]
  dataset[[target]] <- NULL

 return(dataset)
}


#' #' @title convert types for mlr based model
#' #'
#' #' @description  type conversions, e.g., \code{(as.factor(is.character))}
#' #'
#' #' @param x data from \code{\link{getCensusTrainValTestData}}
#' #' @param target character. Target variable, e.g., \code{"income_class"}.
#' #'
#' #' @returns list, i.e.,  \code{list(trainDf, testDf, trueY)} where
#' #' \code{trueY} is the true value on the testDf.
#' #'
#' #' @export
#' convertCensusForMlr <- function(x,
#'                                 target=NULL){
#'   # convert types for mlr based model (as.factor(is.character))
#'   trainDf <- x$trainCensus
#'   trainDf[sapply(trainDf, is.character)] <- lapply(trainDf[sapply(trainDf, is.character)],
#'                                                    as.factor)
#'   #trainDf$income_class <- as.factor(trainDf$income_class)
#'   trainDf[[target]] <- as.factor(trainDf[[target]])
#'
#'   testDf <- x$testCensus
#'   testDf[sapply(testDf, is.character)] <- lapply(testDf[sapply(testDf, is.character)],
#'                                                  as.factor)
#'   # testDf$income_class <- as.factor(testDf$income_class)
#'   trueY <- as.double(testDf[[target]]) -1
#'   testDf[[target]] <- NULL
#'
#'   return(list(trainDf=trainDf, testDf=testDf, trueY=trueY))
#'   }









