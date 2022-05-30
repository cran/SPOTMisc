#' @title spot plot (generic function)
#' @description
#' A wrapper function for available plotting options in SPOT and SPOTMisc.
#' Plotting functions from SPOT -> plotdata, plotModel, plotFunction.
#' Plotting functions from SPOTMisc -> plot_parallel, plot_sensitivity.
#'
#' spotPlot provides a higher level of abstraction and the users can use every plotting
#' function only by calling spotPlot.
#'
#' @param plotType function type to be called. It should be given as either
#' "data", "model", "fun", "parallel" or "sensitivity".
#' Otherwise the function returns an error message.
#'
#' @param ... additional parameters passed to \code{plotData} or \code{plotModel},
#' \code{plotFunction}, \code{plot_parallel} or \code{plot_sensitivity}.
#'
#' @examples
#' \donttest{
#' library("SPOT")
#' set.seed(1)                              # seed
#' k <- 30                                  # sample number
#' x <- matrix( cbind(runif(k)*10, runif(k)*10), k, 2)    # create data
#' y <- funSphere(x)      # generate random test data
#' fit <- buildLM(x,y)                      # create a model
#' result <- spot(x=NULL, funSphere, c(-5, -5), c(5, 5))
#'
#' spotPlot(plotType="data", x, y, type="filled.contour")
#' spotPlot(plotType="model", object=fit, type="contour")
#' spotPlot(plotType="fun", f=function(x){rowSums(x^2)},
#'    lower=c(-10,0), upper=c(15,10), type="filled.contour")
#' spotPlot(plotType = "parallel", object=fit)
#' spotPlot(plotType = "sensitivity", object=result)
#' }
#' @seealso \code{\link[SPOT]{plotData}}
#' @seealso \code{\link[SPOT]{plotModel}}
#' @seealso \code{\link[SPOT]{plotFunction}}
#'
#' @importFrom SPOT plotData
#' @importFrom SPOT plotModel
#' @importFrom SPOT plotFunction
#'
#'
#' @author Alpar Guer \email{alpar.guer@@smail.th-koeln.de}
#'
#' @export
spotPlot <- function(plotType, ...){
  switch(plotType,
         data = plotData(...),
         model = plotModel(...),
         fun = plotFunction(...),
         parallel = plot_parallel(...),
         sensitivity = plot_sensitivity(...),
         stop("Unknown plotType. Select one of the following options:
         \n\t data
         \n\t model
         \n\t fun
         \n\t parallel
         \n\t sensitivity"))
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
  # cols <- ggsci::pal_ucscgb()(nvar)
  for(i in 1:nvar){
    xi <- seq(lower[i], upper[i], length.out = s)
    xi_scaled <- (xi - lower[i])/(upper[i]-lower[i])
    if(type=="best"){
      x <- matrix(xbest,length(xi),nvar,byrow=TRUE)
      x[,i] <- xi
      y <- fun(x)
      fig <- fig %>% add_trace(x=xi_scaled,y=y, mode = 'lines', name=xlab[i]) #, color=cols[i])
      fig <- fig %>% add_trace(x=xbest_scaled[i],y=ybest, mode = 'markers', name=xlab[i]) #, color=cols[i])
    }else if(type=="agg"){
      x <- matrix(runif(agg.sample*nvar*s,lower,upper),,nvar,byrow=T)
      x[,i] <- rep(xi,agg.sample)
      y <- fun(x)
      yagg <- aggregate(y~x[,i],FUN = agg.fun)
      fig <- fig %>% add_trace(x=xi_scaled,y=yagg$y, mode = 'lines', name=xlab[i]) #, color=cols[i])
      fig <- fig %>% add_trace(x=xbest_scaled[i],y=ybest, mode = 'markers', name=xlab[i]) #, color=cols[i])
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


#' @title Prepare data for plots
#' @description Data preparation
#' @param model a function that can be used to build a model based on the data,
#' e.g. : \code{buildRanger} or \code{buildKriging}. Default is \code{buildRanger}, since it is fast and robust.
#' @param modelControl a list of control settings for the respective model.
#' Default is an empty list (use default model controls).
#' @param x a matrix of x-values to be plotted
#' (i.e., columns are the independent variables, rows are samples). Should have same number of rows as y and log.
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
#' @description
#' Preparation of the list elements used for plotting.
#'
#' @param data a list containing the various data, e.g., as produced by a \code{\link[SPOT]{spot}} call.
#' @param model a function that can be used to build a model based on the data, e.g. : \code{buildRanger}
#' or \code{buildKriging}. Default is \code{buildRanger}, since it is fast and robust.
#' @param modelControl a list of control settings for the respective model.
#' Default is an empty list (use default model controls).
#' @param ... additional parameters passed to \code{\link{prepare_data_plot}}:
#' \code{namesx}, \code{namesy}, \code{nameslog} character vectors providing names for x, y and logInfo data.
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


#' @title Sensitivity ggplot of a model
#' @description Generates a sensitivity plot.
#' @param object the result list returned by
#' \code{\link[SPOT]{spot}}, importantly including a \code{modelFit}, and the data \code{x}, \code{y}.
#' @param s number of samples along each dimension.
#' @param agg.sample number of samples for aggregation type
#' (type="agg").
#' @param xlab a vector of characters, giving the labels for each of the two independent variables.
#' @param ylab character, the value of the dependent variable predicted by the corresponding model.
#' @param type string describing the type of the plot:
#' \code{"best"} (default) shows sensitivity around optimum,
#' \code{"contour"},
#' @param agg.fun function for aggregation (type="agg").
#' @param ... additional parameters (currently unused).
#'
#' @seealso \code{\link[SPOT]{plotFunction}},
#' \code{\link[SPOT]{plotData}}
#'
#' @importFrom stats runif
#' @importFrom stats aggregate
#' @importFrom SPOT evaluateModel
#'
#' @return ggplot2 visualization
#'
#' @export
#'
plotSensitivity <- function(object,
                            s=100,
                            xlab= paste("x",1:ncol(object$x),sep=""),
                            ylab="y",
                            type="best",
                            agg.sample=100,
                            agg.fun=mean,...){
  name <- NULL
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

  dfB <- data.frame(x = NULL, y = NULL, name = NULL)
  dfBp <- data.frame(x = NULL, y = NULL, name = NULL)

  for (i in 1:nvar) {
    xi <- seq(lower[i], upper[i], length.out = s)
    xi_scaled <- (xi - lower[i]) / (upper[i] - lower[i])
    if(type=="best"){
      x <- matrix(xbest, length(xi), nvar, byrow = TRUE)
      x[, i] <- xi
      y <- fun(x)
      dfB <- rbind(dfB, data.frame(x = xi_scaled,
                                   y = y,
                                   name = xlab[i]))
      dfBp <- rbind(dfBp, data.frame(x=xbest_scaled[i],
                                     #y=ybest,
                                     y = min(y),
                                     name=xlab[i])) }
    else if(type=="agg"){
      x <- matrix(runif(agg.sample*nvar*s,
                        lower,
                        upper),,
                  nvar,
                  byrow=TRUE)
      x[,i] <- rep(xi,agg.sample)
      y <- fun(x)
      yagg <- aggregate(y~x[,i],FUN = agg.fun)
      dfB <- rbind(dfB, data.frame(x = xi_scaled,
                                   y = yagg$y,
                                   name = xlab[i]))
      dfBp <- rbind(dfBp, data.frame(x=xbest_scaled[i],
                                     #y=ybest,
                                     y = min(yagg$y),
                                     name=xlab[i])) }
    dfBp$y <- min(dfBp$y)
  }
  ggp1 <- ggplot(dfB) +
    geom_line(data=dfB, aes(x=x, y=y, color = as.factor(name))) +
    geom_point(data=dfBp, aes(x, y, color = as.factor(name))) +
    scale_color_discrete(name = "name")
  ggp1
}


#' @title Parallel coordinate plot of a data set
#' @description Parallel plot based on \code{\link[GGally]{ggparcoord}}.
#'
#' @param result the result list returned by \code{\link[SPOT]{spot}},
#' importantly including the data \code{x}, \code{y}.
#' @param yrange a two-element vector that specifies the range of y values to consider (data outside of that range will be excluded).
#' @param ylab character, the value of the dependent variable predicted by the corresponding model.
#' @param xlab character, the value of the independent variable
#' @param splineFactor	logical or numeric operator indicating whether spline interpolation should be used.
#' Numeric values will multiplied by the number of columns, TRUE will default to cubic interpolation,
#' AsIs to set the knot count directly and 0, FALSE, or non-numeric values will not use spline interpolation. See
#' \code{\link[GGally]{ggparcoord}}. Default: \code{"A"}.
#' @param colorOption A character string indicating the colormap
#' option to use. Four options are available:
#' "magma" (or "A"),
#' "inferno" (or "B"),
#' "plasma" (or "C"),
#' "viridis" (or "D", the default option) and
#' "cividis" (or "E"). See \code{\link[ggplot2]{scale_colour_viridis_d}}
#' @param  scale  method used to scale the variables. Default: \code{"uniminmax"}.
#' @param boxplot	logical operator indicating whether or not boxplots should
#' underlay the distribution of each variable
#' @param alphaLines	value of alpha scaler for the lines of the parcoord plot or
#' a column name of the data. Default: 0.1
#' @param showPoints logical operator indicating whether points
#' should be plotted or not. Default: TRUE
#' @param title	character string denoting the title of the plot. Default:
#' \code{""}.
#' @param probs quantile probabilities. Default:  \code{seq(0, 1, 0.25)}
#' @param ... additional parameters to be passed to
#' \code{\link[GGally]{ggparcoord}}.
#'
#' @seealso \code{\link[SPOT]{plotFunction}}, \code{\link[SPOT]{plotData}}
#'
#' @return plotly parallel coordinate plot ('parcoords') visualization (based on
#' \code{\link[plotly]{plot_ly}})
#'
#' @importFrom stats as.formula
#' @importFrom stats quantile
#' @importFrom ggplot2 scale_color_viridis_d
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_color_discrete
#' @importFrom GGally ggparcoord
#'
#' @examples
#' \donttest{
#' require("SPOT")
#' res <- spot(x=NULL,
#'              funSphere,
#'              lower=rep(-1,3),
#'              upper=rep(1,3),
#'              control=list(funEvals=25))
#' plotParallel(res, scale="std")
#' }
#' @export
plotParallel <- function(result,
                         xlab=NULL,
                         ylab=NULL,
                         yrange = NULL,
                         splineFactor = 1,
                         colorOption = "A",
                         scale = "uniminmax",
                         boxplot = FALSE,
                         alphaLines = 0.1,
                         showPoints = TRUE,
                         title = "",
                         probs = seq(0.25, 0.75, 0.25),
                         ...){
  if(is.null(xlab)){
  xlab= paste("x",1:ncol(result$x),sep="")
  }
  if(is.null(ylab)){
  ylab = "y"}
  df <- ggparcoordPrepare(x = result$x,
                          y = result$y,
                          xlab = xlab,
                          probs = probs)
 m <- ncol(df)
fig <-  ggparcoord(data=df,
                     columns = 1:(m-2),
                     groupColumn = m,
                     splineFactor = splineFactor,
                     scale = scale,
                     boxplot = boxplot,
                     alphaLines = alphaLines,
                     showPoints = showPoints,
                     title = title,
                   ...) +
    scale_color_viridis_d(option = colorOption)
  return(fig)
}


#' @title Build data frame for ggparcoord (parallel plot)
#' @param x elements \code{x}, e.g., result from a \code{\link[SPOT]{spot}} run.
#' @param y associated function values
#' @param ylab character, the value of the dependent variable predicted by the corresponding model.
#' @param xlab character, the value of the independent variable
#' @param probs quantile probabilities. Default:  \code{seq(0, 1, 0.25)}
#' @param yrange y interval
#' @returns data frame for \code{\link[GGally]{ggparcoord}}
#' @examples
#' require(SPOT)
#' require(GGally)
#' n <- 4 # param
#' k <- 50 # samples
#' x <- designUniformRandom(,rep(0,n), rep(1,n),control=list(size=k))
#' y <- matrix(0, nrow=k,ncol=1)
#' y <- funSphere(x)
#' result <- list(x=x, y=y)
#' df <- ggparcoordPrepare(x=result$x,
#'                         y=result$y,
#'                         xlab=result$control$parNames,
#'                         probs = c(0.25, 0.5, 0.75))
#'                         #probs = c(0.1, 0.9))  # c(0.9,0.95) )#seq(0.25, 1, 0.25))
#' m <- ncol(df)
#' splineFactor <- max(1, floor(2*m))
#' ggparcoord(data=df, columns = 1:(m-2), groupColumn = m,
#' scale = "uniminmax", boxplot = FALSE, alphaLines = 0.2,showPoints = TRUE)
#' ##
#' \donttest{
#' require(SPOT)
#' require(GGally)
#' result <- spot(x=NULL,
#'               fun=funSphere,
#'               lower=rep(-1,3),
#'               upper= rep(1,3),
#'  control=list(funEvals=20,
#'              model=buildKriging,
#'              modelControl=list(target="y")))
#'
#' df <- ggparcoordPrepare(x=result$x,
#'                         y=result$y,
#'                         xlab=result$control$parNames,
#'                         probs = c(0.25, 0.5, 0.75))  # c(0.9,0.95) )#seq(0.25, 1, 0.25))
#' m <- ncol(df)
#' splineFactor <- max(1, floor(2*m))
#' ggparcoord(data=df, columns = 1:(m-2), groupColumn = m,
#' splineFactor = splineFactor, scale = "uniminmax",
#' boxplot = FALSE, alphaLines = 0.2,showPoints = TRUE, scaleSummary = "median")
#' }
#'
#' @export
ggparcoordPrepare <- function(x,y,
                              xlab = NULL,
                              ylab = NULL,
                              probs = seq(0.25, 0.75, 0.25),
                              yrange = NULL){
  if(is.null(xlab)){
    xlab= paste("x",1:ncol(x),sep="")
  }
  if(is.null(ylab)){
    ylab = "y"}
#browser()
df <- data.frame(x = x,
                 y = y)
colnames(df) <- c(xlab,ylab)
if(!is.null(yrange)){
  df <- df[y<=yrange[2] & y>=yrange[1],]
}

y <- -df$y
qy <- quantile(y, probs = probs)
if(length(qy) != length(unique(qy))){
  stop("Quantiles w/o values. Modify probs, e.g., probs = seq(0, 1, 0.5).")
}
k <- length(qy)
n <- length(y)
v <- matrix(rep(NA, n*k ), ncol=k)
for (i in 1:k){
  v[,i] <- (y >= qy[i])
}
w <- apply(v, 1, sum)
wlevels <- as.character(100*c(0,probs))
labels <-  c(paste0(">=", wlevels,"%"))
df$bestPercent <- factor(w, labels = labels)
return(df)
}

#' @title simple progress plot
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 facet_wrap
#'
#' @param dfRun data frame, e.g., result from \code{\link{prepareProgressPlot}}.
#' @param xlabel x label
#' @param ylabel y label
#' @param aspectRatio aspect.ratio
#' @param scalesFreeFixed "free_x", "free_y" or "fixed"
#' @param nColumns number of columns
#'
#' @return p ggplot
#'
#' @export
#'
ggplotProgress <- function(dfRun,
                           xlabel ="function evaluations",
                           ylabel = "MMCE",
                           aspectRatio = 2,
                           scalesFreeFixed = "free_y",
                           nColumns = 3){
  size <- x <- y <- yInitMin <- NULL
p <- ggplot(data = dfRun, aes(x = x, y = y)) +
  geom_line(color = "grey") +
  geom_point(alpha = 0.25, pch = 20, size = 0.25) +
  geom_vline(data = dfRun, aes(xintercept = size), color = "black", linetype = "dotted") +
  geom_hline(data = dfRun, aes(yintercept =  yInitMin), color = "red", linetype = "dashed") +
  geom_hline(data = dfRun, aes(yintercept =  attr(dfRun, which = "ymin")), color = "blue", linetype = "dashed") +
  xlab(xlabel) +
  ylab(ylabel) +
  theme(aspect.ratio = aspectRatio) +
  facet_wrap(~name, ncol = nColumns,  scales = scalesFreeFixed)
return(p)
}
