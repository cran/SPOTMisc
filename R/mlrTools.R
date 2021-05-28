#' @title
#' Helper function: translate levels
#'
#' @description
#' Translate existing levels of a factor into new levels.
#'
#' @param x a factor vector to be translated
#' @param translations a named list that specifies the translation: \code{list(newlevel=c(oldlevel1,oldlevel2,etc))}.
#' @return translated factor
#'
#' @export
translate_levels <- function(x, translations) {
	x <- as.character(x)
	for (i in 1:length(translations)) {
		sel <- x %in% translations[[i]]
		x[sel] <- names(translations[i])
	}
	as.factor(x)
}

#' @title
#' Helper function: transform integer to factor
#'
#' @description
#' This function re-codes a factor with pre-specified factor levels,
#' using an integer encoding as input.
#'
#' @param x an integer vector (that represents factor vector) to be transformed
#' @param lvls the original factor levels used
#'
#' @return the same factor, now coded with the original levels
#'
#' @export
int2fact <- function(x, lvls) {
	nms <- names(lvls)
	for (i in 1:length(lvls)) {
		x[[nms[i]]] <-  lvls[[i]][x[[nms[i]]]]
	}
	x
}

#' @title
#' Identity transformation
#'
#' @description
#' Parameter values can be translated,
#' e.g., to base 10 as implemented in \code{\link{trans_10pow}}.
#' \code{trans_id} implements the identity (transformation), i.e., x is mapped to x.
#'
#' @param x input
#'
#' @return \code{x}
#'
#' @export
trans_id <- function(x) {
	x
}

#' @title 2 power x transformation
#' @description
#' Parameter values can be translated,
#' e.g., to base 10 as implemented in \code{\link{trans_10pow}}.
#' \code{trans_2pow} implements the transformation x -> 2^x.
#'
#' @param x input
#' @return \code{2^x}
#'
#' @export
trans_2pow <- function(x) {
	2 ^ x
}

#' @title 2 power x transformation with round
#' @description
#' Parameter values can be translated,
#' e.g., to base 10 as implemented in \code{\link{trans_10pow}}.
#' \code{trans_2pow_round} implements the transformation x -> round(2^x).
#'
#' @param x input
#' @return \code{round(2^x)}
#'
#' @export
trans_2pow_round <- function(x) {
	round(2 ^ x)
}

#' @title 10 power x transformation
#' @description
#' Parameter values can be translated,
#' e.g., to base 10.
#'
#' @param x input
#' @return \code{10^x}
#'
#' @export
trans_10pow <- function(x) {
	10 ^ x
}


#' @title 10 power x transformation with round
#'
#' @description
#' Parameter values can be translated,
#' e.g., to base 10 as implemented in \code{\link{trans_10pow}}.
#' \code{trans_10pow_round} implements the transformation x -> round(2^x).
#'
#' @param x input
#' @return \code{round(10^x)}
#'
#' @export
trans_10pow_round <- function(x) {
	round(10 ^ x)
}


#' @title
#' Check the validity of input parameters.
#'
#' @description
#' Helper function. Check correct parameter names.
#'
#' @param chars character
#'
#' @return correct characters
#'
#' @export
#' @keywords internal
valid_inputs <- function(chars) {
  is_valid <- NULL
  for (i in 1:length(chars)) {
    ch <- chars[i]
    valid <- FALSE
    if (is.list(.GlobalEnv[[ch]])) {
      if (!is.null(.GlobalEnv[[ch]]$modelFit)) {
        valid <- TRUE
      }
    }
    is_valid <- c(is_valid, valid)
  }
  chars[is_valid]
}


#' @title Make benchmark
#'
#' @description Run standard benchmark from the package \code{benchmarkme} and return the
#' required time in seconds
#' #'
#' @importFrom benchmarkme benchmark_std
#'
#' @return total time (in seconds)
#'
#' @export
#' @keywords internal
makeBench <- function() {
	res <- benchmark_std(runs = 1, verbose = FALSE)
	total_time <- sum(res$user) ## in seconds
	return(total_time)
}


#' @title Get budget (time, function evaluations)
#' @description
#' Determine time factor and corrected  budget and timeout.
#' Runs a benchmark to determine processing speed of assigned computing resources.
#'
#' @param runtime_bench runtime
#' @param settings settings
#'
#' @return settings
#' @export
#' @keywords internal
getBudget <- function(runtime_bench,
											settings) {
	if (is.null(runtime_bench)) {
		runtime_bench <- mean(replicate(3, makeBench())) ## in seconds
		runtime_default <- 5.6 ## in seconds
		time_multiplier <- runtime_bench / runtime_default
	}
	settings$actual_timebudget <-	time_multiplier * settings$timebudget
	settings$actual_timeout <- time_multiplier * settings$timeout
	settings$runtime_bench <- runtime_bench
	settings$time_multiplier <- time_multiplier
	return(settings)
}


#' @title Get objective function
#'
#' @description mlrTools
#' This function receives a configuration for a tuning experiment,
#' and returns an objective function to be tuned via SPOT.
#'
#' @param config list
#' @param timeout integer, time in seconds after which a model (learner) evaluation will be aborted.
#'
#' @return an objective function that can be optimized via \code{\link[SPOT]{spot}}
#'
#' @export
#' @importFrom callr r
#' @importFrom mlr mmce
#' @importFrom mlr timeboth
#' @importFrom mlr timetrain
#' @importFrom mlr timepredict
#' @importFrom mlr rmse
#' @importFrom mlr makeLearner
#' @importFrom mlr resample
#'
get_objf <- function(config,timeout=3600){
  force(config)
  force(timeout)

  objfun <- function(x,seed){
    params <- as.list(x)
    ## set parameter names
    names(params) <- config$tunepars

    ## transform to actual parameter scale
    for(i in 1:length(params)){
      params[[i]] <- config$transformations[[i]](params[[i]])
    }

    ## convert integer levels to factor levels for categorical params
    if(length(config$factorlevels)>0)
      params <- int2fact(params,config$factorlevels)

    ## set fixed parameters (which are not tuned, but are also not set to default value)
    if(length(config$fixpars)>0)
      params <- c(params,config$fixpars)

    #print(data.frame(params))

    ## set parameters in relation to other parameters (minbucket relative to minsplit)
    nrel <- length(config$relpars)
    if(nrel>0){
      for(i in 1:nrel){
        params[names(config$relpars)[i]] <- with(params,eval(config$relpars[[i]]))
      }
    }

    ## generate the learner
    # print(data.frame(params))
    lrn = makeLearner(config$learner,par.vals=params)

    ## call the model evaluation in a new process via callr
    ## this ensures that the call can be timed-out even when stuck in non-R low-level code
    if(is.na(timeout)){
      if(config$task$type=="classif"){
        measures <- list(mmce,timeboth,timetrain,timepredict)
      }else if(config$task$type=="regr"){
        measures = list(rmse,timeboth,timetrain,timepredict)
      }
      set.seed(seed)
      res <- try(resample(lrn, config$task, config$resample, measures = measures, show.info = FALSE))
    }else{
      res <- try(
        r( # callr::r
          function(lrn,task,resample,seed){
            #require(mlr)
            if(task$type=="classif"){
              measures <- list(mmce,timeboth,timetrain,timepredict)
            }else if(task$type=="regr"){
              measures = list(rmse,timeboth,timetrain,timepredict)
            }
            set.seed(seed)
            resample(lrn, task, resample, measures = measures, show.info = FALSE)
          },
          timeout=timeout,
          args = list(lrn=lrn,task=config$task,resample=config$resample,seed=seed),
          poll_connection = FALSE,
          package="mlr"
        )
      )
    }

    timestamp <- as.numeric(Sys.time())

    ## determine a value to return in case of errors or timeouts.
    if( class(res)[1] == "try-error"){
      if(config$task$type=="classif"){ #(based on guessing the most frequent class)
        lrn <-  makeLearner("classif.rpart",maxdepth=1)
        measures <- list(mmce,timetrain,timepredict)
      }else if(config$task$type=="regr"){ #(based on guessing the mean of the observations)
        lrn <-  makeLearner("regr.rpart",maxdepth=1)
        measures = list(rmse,timetrain,timepredict)
      }
      res = resample(lrn, config$task, config$resample, measures =measures)
      # print(data.frame(res$aggr))
      return(matrix(c(res$aggr[1],timeout,timeout,timeout,timestamp),1))
    }else{
      # print(data.frame(res$aggr))
      return(matrix(c(res$aggr,timestamp),1))
    }
  }
  force(objfun)
  objvecf <- function(x,seed){
    res <- NULL
    for(i in 1:nrow(x))
      res <- rbind(res,objfun(x[i,,drop=FALSE],seed[i]))
    return(res)
  }
}

#' @title Surface plot
#'
#' @description
#' A (filled) contour plot or perspective plot of a function, interactive via plotly.
#'
#' @param f function to be plotted. The function should either be able to take two vectors or one matrix specifying sample locations. i.e. \code{z=f(X)} or \code{z=f(x2,x1)} where Z is a two column matrix containing the sample locations \code{x1} and \code{x2}.
#' @param lower boundary for x1 and x2 (defaults to \code{c(0,0)}).
#' @param upper boundary (defaults to \code{c(1,1)}).
#' @param type string describing the type of the plot:  \code{"filled.contour"} (default), \code{"contour"},
#' \code{"persp"} (perspective), or \code{"persp3d"} plot.
#' Note that "persp3d" is based on the plotly package and will work in RStudio, but not in the standard RGui.
#' @param s number of samples along each dimension. e.g. \code{f} will be evaluated \code{s^2} times.
#' @param xlab lable of first axis
#' @param ylab lable of second axis
#' @param zlab lable of third axis
#' @param color.palette colors used, default is \code{terrain.color}
#' @param title of the plot
#' @param levels number of levels for the plotted function value. Will be set automatically with default NULL.. (contour plots  only)
#' @param points1 can be omitted, but if given the points in this matrix are added to the plot in form of dots. Contour plots and persp3d only. Contour plots expect matrix with two columns for coordinates. 3Dperspective expects matrix with three columns, third column giving the corresponding observed value of the plotted function.
#' @param points2 can be omitted, but if given the points in this matrix are added to the plot in form of crosses. Contour plots and persp3d only.  Contour plots expect matrix with two columns for coordinates. 3Dperspective expects matrix with three columns, third column giving the corresponding observed value of the plotted function.
#' @param pch1 pch (symbol) setting for points1 (default: 20). (contour plots only)
#' @param pch2 pch (symbol) setting for points2 (default: 8). (contour plots only)
#' @param lwd1 line width for points1 (default: 1). (contour plots only)
#' @param lwd2 line width for points2 (default: 1). (contour plots only)
#' @param cex1 cex for points1 (default: 1). (contour plots only)
#' @param cex2 cex for points2 (default: 1). (contour plots only)
#' @param col1 color for points1 (default: "black"). (contour plots only)
#' @param col2 color for points2 (default: "black"). (contour plots only)
#' @param ... additional parameters passed to \code{contour} or \code{filled.contour}
#'
#' @importFrom grDevices terrain.colors
#' @importFrom graphics filled.contour
#' @importFrom graphics points
#' @importFrom graphics axis
#' @importFrom graphics persp
#' @importFrom graphics contour
#' @importFrom plotly layout
#' @importFrom plotly plot_ly
#' @importFrom plotly add_trace
#' @importFrom plotly %>%
#' @importFrom plotly colorbar
#'
#' @return plotly visualization (based on \code{\link[plotly]{plot_ly}})
#'
#' @export
plot_function_surface <- function(f=function(x){rowSums(x^2)},
                                  lower=c(0,0) , upper=c(1,1) ,
                                  type="filled.contour",
                                  s=100,
                                  xlab="x1",ylab="x2", zlab="y",
                                  color.palette = terrain.colors,
                                  title=" ",  levels=NULL,
                                  points1, points2, pch1=20, pch2=8, lwd1=1, lwd2=1, cex1=1, cex2=1, col1="blue", col2="red",
                                  ...){
  x <- seq(lower[1], upper[1], length = s)
  y <- seq(lower[2], upper[2], length = s)
  if(length(formals(f))==1){
    fn <- function(a,b){
      f(cbind(a,b))
    }
    z <- outer(x, y, fn)
  }else if(length(formals(f))==2){
    z <- outer(x, y, f)
  }

  if(is.null(levels))
    levels=pretty(range(z[!is.na(z)]),20)

  if(type=="filled.contour"){
    p <- plotly::plot_ly(x=x,
                         y=y,
                         z=~t(z),
                         type = "contour",
                         coloraxis = 'coloraxis') %>% #,showscale=FALSE) %>%
      layout(title=zlab,
             xaxis=list(title = xlab),
             yaxis=list(title = ylab)
      ) %>%
      colorbar(title = zlab) %>%
      layout(coloraxis=list(colorscale='Greys'))
    if(!missing(points1)){
      p <- p %>% add_trace(data=points1,x=points1[,1],y=points1[,2], mode = "markers", type = "scatter",
                           marker = list(size = 5, color = col1, symbol = 200),inherit = FALSE,showlegend=FALSE, hoverinfo='none')
    }
    if(!missing(points2)){
      p <- p %>% add_trace(data=points2,x=points2[,1],y=points2[,2], mode = "markers", type = "scatter",
                           marker = list(size = 5, color = col2, symbol = 200),inherit = FALSE,showlegend=FALSE, hoverinfo='none')
    }
    p
  }else if(type=="persp3d"){ #perspective plot with plotly
    p <- plot_ly(z = ~t(z), x = x, y = y,type = "surface")
    if(!missing(points1))
      p <- p %>% add_trace(data=points1,x=points1[,1],z=points1[,3],y=points1[,2], mode = "markers", type = "scatter3d",
                           marker = list(size = 5, color = col1, symbol = 200))
    if(!missing(points2))
      p <- p %>% add_trace(data=points2,x=points2[,1],z=points2[,3],y=points2[,2], mode = "markers", type = "scatter3d",
                           marker = list(size = 6, color = col2, symbol = 200))
    p
  }
}


#' @title Surface plot of a model
#'
#' @description
#' A (filled) contour or perspective plot of a fitted model.
#'
#' @param object the result list returned by \code{\link[SPOT]{spot}}, importantly including a \code{modelFit}, and the data \code{x}, \code{y}.
#' @param which a vector with two elements, each an integer giving the two independent variables of the plot
#' (the integers are indices of the respective data set).
#' @param constant a numeric vector that states for each variable a constant value that it will take on
#' if it is not varied in the plot. This affects the parameters not selected by the \code{which} parameter.
#' By default, this will be fixed to the best known solution, i.e., the one with minimal y-value, according
#' to \code{which.min(object$y)}. The length of this numeric vector should be the same as the number of columns in \code{object$x}
#' @param xlab a vector of characters, giving the labels for each of the two independent variables.
#' @param ylab character, the value of the dependent variable predicted by the corresponding model.
#' @param type string describing the type of the plot:  \code{"filled.contour"} (default), \code{"contour"},
#' \code{"persp"} (perspective), or \code{"persp3d"} plot.
#' Note that "persp3d" is based on the plotly package and will work in RStudio, but not in the standard RGui.
#' @param ... additional parameters passed to the \code{contour} or \code{filled.contour} function.
#'
#' @return plotly visualization (based on \code{\link[plotly]{plot_ly}})
#'
#' @export
plot_surface <- function(object,which=if(ncol(object$x)>1 & tolower(type) != "singledim"){1:2}else{1},
                         constant=object$x[which.min(unlist(object$y)),], #best known solution. default.
                         xlab= paste("x",which,sep=""),ylab="y",type="filled.contour",...){
  if(!is.null(object$namesx))
    xlab=object$namesx
  if(!is.null(object$namesy))
    ylab=object$namesy

  which <- sort(which)
  xlab <- xlab[which]

  ## number of variables
  nvar <- ncol(object$x)
  ## bounds
  if(length(which) == 1){
    lower <- min(object$x)
    upper <- max(object$x)
  }else{
    lower <- apply(object$x[,which],2,min)
    upper <- apply(object$x[,which],2,max)
  }

  ## best solution
  y <- unlist(object$y)
  ibest <- which.min(y)
  xbest <- object$x[ibest,]
  ybest <- y[ibest]

  ## varied variables
  vary <-  (1:nvar) %in% which
  modelFit <- object$modelFit
  force(modelFit)
  force(nvar)
  force(vary)
  force(constant)

  ## Pre Checkup
  if(nvar < 2 & tolower(type) != "singledim"){
    stop("The specified plot type is only available for 2 or more dimensions")
  }

  ## Preparation of function for 'plotFunction()'
  if(nvar == 2){
    plotfun <- evaluateModel(modelFit)
  }else if(nvar > 2){
    plotfun2 <- evaluateModel(modelFit)
    plotfun <- function(xx){ #fix constants
      z2 <- matrix(constant,nrow(xx),nvar,byrow=TRUE)
      z2[,which(vary)[1]] <- xx[,1]
      z2[,which(vary)[2]] <- xx[,2]
      plotfun2(z2)
    }
  }else{
    stop("Dimensionality does not meet plot type")
  }

  ##Wrapper for plotFun$y
  plotfuny <- function(xx){
    res <- plotfun(xx)
    if(is.list(res)){
      return(res$y)
    }
    res
  }

  plot_function_surface(f=plotfuny,lower=lower,upper=upper,
                        type=type,
                        xlab=xlab[1],ylab=xlab[2],zlab=ylab,
                        points1=cbind(object$x[,which],y),
                        points2=matrix(c(xbest[which],ybest),1),
                        ...)
}


#' @title Sensitivity plot of a model
#'
#' @description mlrTools
#'
#' @param object the result list returned by \code{\link[SPOT]{spot}}, importantly including a \code{modelFit}, and the data \code{x}, \code{y}.
#' @param s number of samples along each dimension.
#' @param agg.sample number of samples for aggregation type (type="agg").
#' @param xlab a vector of characters, giving the labels for each of the two independent variables.
#' @param ylab character, the value of the dependent variable predicted by the corresponding model.
#' @param type string describing the type of the plot:  \code{"best"} (default) shows sensitivity around optimum, \code{"contour"},
#' \code{"persp"} (perspective), or \code{"persp3d"} plot.
#' Note that "persp3d" is based on the plotly package and will work in RStudio, but not in the standard RGui.
#' @param agg.fun function for aggregation (type="agg").
#' @param ... additional parameters (currently unused).
#'
#' @seealso \code{\link[SPOT]{plotFunction}}, \code{\link[SPOT]{plotData}}
#'
#' @importFrom ggsci pal_ucscgb
#' @importFrom stats runif
#' @importFrom stats aggregate
#' @importFrom SPOT evaluateModel
#'
#' @return plotly visualization (based on \code{\link[plotly]{plot_ly}})
#'
#' @export
#'
plot_sensitivity <- function(object,
                             s=100,
                             xlab= paste("x",1:ncol(object$x),sep=""),
                             ylab="y",
                             type="best",
                             agg.sample=100,
                             agg.fun=mean,...){
  if(!is.null(object$namesx))
    xlab=object$namesx
  if(!is.null(object$namesy))
    ylab=object$namesy

  ## number of variables
  nvar <- ncol(object$x)
  ## bounds
  lower <- apply(object$x,2,min)
  upper <- apply(object$x,2,max)

  ## best solution
  y <- unlist(object$y)
  ibest <- which.min(y)
  xbest <- object$x[ibest,]
  ybest <- y[ibest]

  ## varied variables
  modelFit <- object$modelFit
  force(modelFit)
  force(nvar)

  ## Preparation of function for evaluation
  fun <- evaluateModel(modelFit)

  ## scaled best solution
  xbest_scaled <- (xbest - lower)/(upper-lower)

  fig <- plot_ly(name = "Test", type = 'scatter', mode = 'lines')
  cols <- pal_ucscgb()(nvar)
  for(i in 1:nvar){
    xi <- seq(lower[i], upper[i], length.out = s)
    xi_scaled <- (xi - lower[i])/(upper[i]-lower[i])
    if(type=="best"){
      x <- matrix(xbest,length(xi),nvar,byrow=TRUE)
      x[,i] <- xi
      y <- fun(x)
      fig <- fig %>% add_trace(x=xi_scaled,y=y, mode = 'lines', name=xlab[i], color=cols[i])
      fig <- fig %>% add_trace(x=xbest_scaled[i],y=ybest, mode = 'markers', name=xlab[i], color=cols[i])
    }else if(type=="agg"){
      x <- matrix(runif(agg.sample*nvar*s,lower,upper),,nvar,byrow=T)
      x[,i] <- rep(xi,agg.sample)
      y <- fun(x)
      yagg <- aggregate(y~x[,i],FUN = agg.fun)
      fig <- fig %>% add_trace(x=xi_scaled,y=yagg$y, mode = 'lines', name=xlab[i], color=cols[i])
      fig <- fig %>% add_trace(x=xbest_scaled[i],y=ybest, mode = 'markers', name=xlab[i], color=cols[i])
    }
  }
  fig <- fig %>% layout(
    xaxis=list(title = "parameter range"),
    yaxis=list(title = ylab)
  )
  return(fig)
}

#' @title Parallel coordinate plot of a data set
#' @description mlrTools
#'
#' @param object the result list returned by \code{\link[SPOT]{spot}}, importantly including a \code{modelFit}, and the data \code{x}, \code{y}.
#' @param yrange a two-element vector that specifies the range of y values to consider (data outside of that range will be excluded).
#' @param yvar integer which specifies the variable that is displayed on the color scale. yvar==1 (default) means that the y-variable is shown (tuned measure). Larger integers mean that respective columns from logInfo are used (i.e., yvar specifies the respective column number, starting with 2 for the first logged value).
#' @param xlab a vector of characters, giving the labels for each of the two independent variables.
#' @param ylab character, the value of the dependent variable predicted by the corresponding model.
#' @param ... additional parameters (currently unused).
#'
#' @seealso \code{\link[SPOT]{plotFunction}}, \code{\link[SPOT]{plotData}}
#'
#' @return plotly parallel coordinate plot ('parcoords') visualization (based on \code{\link[plotly]{plot_ly}})
#'
#' @export
#' @importFrom stats as.formula
plot_parallel <- function(object,
                          yrange=NULL,
                          yvar=1,
                          xlab= paste("x",1:ncol(x),sep=""),
                          ylab="y",
                          ...){
  if(!is.null(object$namesx))
    xlab=object$namesx
  x <- object$x
  ylog <- cbind(object$y,object$logInfo)
  y <- ylog[,yvar,drop=FALSE]


  if(!is.null(object$namesy)){
    if(yvar==1){
      ylab=object$namesy
    }else if((yvar>1)&(!is.null(object$nameslog))){
      ylab=object$nameslog[yvar-1]
    }
  }

  ## build data frame
  df <- data.frame(x=x,y=y)
  colnames(df) <- c(xlab,ylab)
  if(!is.null(yrange))
    df <- df[y<=yrange[2] & y>=yrange[1],]

  dims <- list()
  for(i in 1:length(xlab)){
    dims[[i]] <- list(label=xlab[i],
                      values = as.formula(paste0('~', xlab[i]))
    )
  }

  fig <- plot_ly(type = 'parcoords',data=df,
                 line = list(color = as.formula(paste0('~', ylab)),
                             colorscale = 'Jet',
                             showscale = TRUE),
                 dimensions = dims
  )
  return(fig)
}

#' @title Pareto front (as well as non-optimal solutions)
#'
#' @description mlrTools
#'
#' @param object the result list returned by \code{\link[SPOT]{spot}}, importantly including a \code{modelFit}, and the data \code{x}, \code{y}.
#' @param xvar integer which specifies the variable that is displayed on the x axis. xvar=1 (default) means that the y-variable is shown (tuned measure). Larger integers mean that respective columns from logInfo are used (then, ylog specifies the respective column number, starting with 2 for the first logged value).
#' @param yvar integer which specifies the variable that is displayed on the color scale. yvar==1 (default) means that the y-variable is shown (tuned measure). Larger integers mean that respective columns from logInfo are used (then, ylog specifies the respective column number, starting with 2 for the first logged value).
#' @param xlab character, giving the label for the x-axis of the plot.
#' @param ylab character, giving the label for the y-axis of the plot.
#' @param ... additional parameters (currently unused).
#'
#' By default, it is assumed that all variables that are plotted (yvar,xvar) are minimized.
#' If this is not true, assign a negative sign to the respective integer of the xvar and yvar arguments.
#'
#' @seealso \code{\link[SPOT]{plotFunction}}, \code{\link[SPOT]{plotData}}
#'
#' @return plotly visualization (type 'scatter'), based on \code{\link[plotly]{plot_ly}}
#'
#' @export
#' @importFrom stats as.formula
#' @importFrom emoa is_dominated
plot_pareto <- function(object,
                        xvar = 1, yvar = 2,
                        xlab= NULL,ylab=NULL,
                        ...){
  xsign <- sign(xvar)
  ysign <- sign(yvar)
  xvar <- abs(xvar)
  yvar <- abs(yvar)

  ylog <- cbind(object$y,object$logInfo)
  y <- ylog[,c(xvar,yvar),drop=F]
  namesall <- c(object$namesy,object$nameslog)
  namesy <- namesall[c(xvar,yvar)]
  x <- data.frame(x=object$x)
  colnames(x) <- object$namesx

  ## specify the axis names (prefer arguments of names from object, over "y1" / "y2")
  if(is.null(xlab)){
    xlab <- namesy[1]
    if(is.null(xlab)){
      xlab <- "y1"
    }
  }

  if(is.null(ylab)){
    ylab <- namesy[2]
    if(is.null(ylab)){
      xlab <- "y2"
    }
  }

  ## build data frame
  df <- data.frame(y=y)
  colnames(df) <- c(xlab,ylab)
  df[,1] <- df[,1] * xsign
  df[,2] <- df[,2] * ysign

  ## build text strings for the hover info box
  texts <- NULL
  for(i in 1:nrow(x)){
    texts <- c(texts, paste(colnames(x),signif(x[i,],3),sep=": ",collapse="\n"))
  }
  nondominatedpoints <- !is_dominated(t(df))
  ## generate plot
  fig <- plot_ly(type = 'scatter',data=df,mode='markers',
                 x=df[,1],
                 y=df[,2],
                 color=nondominatedpoints,
                 colors=c("blue","red"),
                 text=texts,
                 hoverinfo='text'
  )%>%
    layout(
      legend = list(title = list(text = "Pareto-\noptimal")),
      xaxis=list(title = xlab),
      yaxis=list(title = ylab)
    )
  return(fig)
}


#' @title Prepare data for plots
#'
#'
#' @description mlrTools
#'
#' @param model a function that can be used to build a model based on the data, e.g. : \code{buildRanger} or \code{buildKriging}. Default is \code{buildRanger}, since it is fast and robust.
#' @param modelControl a list of control settings for the respective model. Default is an empty list (use default model controls).
#' @param x a matrix of x-values to be plotted (i.e., columns are the independent variables, rows are samples). Should have same number of rows as y and log.
#' @param namesx character vector, printable names for the x data. Should have same length as x has columns. Default is x1, x2, ...
#' @param y a one-column matrix of y-values to be plotted (dependent variable). Should have same number of rows as x and log.
#' @param namesy character, giving a printable name for y. Default is "y".
#' @param log matrix, a data set providing (optional) additional dependent variables (but these are not modeled). Should have same number of rows as y and x.
#' @param nameslog character vector, printable names for the log data. Should have same length as log has columns. Default is NULL (no names).
#'
#' @importFrom SPOT buildRanger
#'
#' @return list with plotting data and information
#'
#' @export
prepare_data_plot <- function(model=buildRanger,
                              modelControl=list(),
                              x,
                              namesx=paste("x",1:ncol(x),sep=""),
                              y,
                              namesy="y",
                              log=NULL,nameslog=NULL){
  modelFit <- model(x=x,y=y,control=modelControl)
  ibest <- which.min(y)
  xbest <- x[ibest,,drop=FALSE]
  ybest <- y[ibest,,drop=FALSE]
  plotobj <- list(x=x,
                  y=y,
                  logInfo=log,
                  xbest=xbest,
                  ybest=ybest,
                  namesx=namesx,
                  namesy=namesy,
                  nameslog=nameslog,
                  modelFit=modelFit
  )
  return(plotobj)
}

#' @title Prepare data (results from a tuning run) for plots
#'
#' @description
#' Preparation ot the list elements used for plotting.
#'
#' @param data a list containing the various data, e.g., as produced by a \code{\link[SPOT]{spot}} call.
#' @param model a function that can be used to build a model based on the data, e.g. : \code{buildRanger} or \code{buildKriging}. Default is \code{buildRanger}, since it is fast and robust.
#' @param modelControl a list of control settings for the respective model. Default is an empty list (use default model controls).
#' @param ... additional parameters passed to \code{\link{prepare_data_plot}}: \code{namesx}, \code{namesy}, \code{nameslog} character vectors providing names for x, y and logInfo data. Se
#'
#' @importFrom SPOT buildRanger
#'
#' @return list with plotting data and information generated with \code{\link{prepare_data_plot}}
#'
#' @export
prepare_spot_result_plot <- function(data,model=buildRanger,modelControl=list(),...){
  prepare_data_plot(model=model,
                    modelControl = modelControl,
                    x=data$x,
                    y=data$y,
                    log=data$logInfo,
                    ...
  )
}




