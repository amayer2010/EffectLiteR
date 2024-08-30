

#' Plot conditional effects
#' 
#' Can be used to make a conditional effects plot with an effect function on the
#' y axis and a covariate on the x axis. \code{ggplot2} is used to create the plot.
#' 
#' @param obj Object of class \code{effectlite} obtained from fitting an effect 
#' model using \code{\link[EffectLiteR]{effectLite}} 
#' @param zsel Name of a covariate (character string) plotted on the x-axis. If "id" 
#' (the default) the subject index is shown on the x-axis, where subjects in the data are
#'  enumerated as 1:nrow(data).
#' @param gxsel Name of an effect function (character string) plotted on the y-axis.
#' @param colour Name of a covariate (character string) used as colour variable 
#' in the plot.
#' @param show.ci Logical. Should 95 percent confidence intervals around conditional effects be shown in the plot.
#' @param regression Specifies if a regression line should be drawn. Can be one of 
#' c("default","smooth","linear","none")
#' @param regression.ci Logical. Will be passed on to \code{\link[ggplot2]{geom_smooth}} and specifies its \code{se} argument. Notice that the confidence interval shown by \code{\link[ggplot2]{geom_smooth}} does not take uncertainty into account that comes from estimating the values of the conditional effects on the y axis.
#' @return Object of class \code{c("gg", "ggplot")}.
#' @examples
#' m1 <- effectLite(y="dv", x="x", k="k1", z="z1", control="control", data=example01)
#' conditionalEffectsPlot(m1, zsel="z1", gxsel="g1", colour="k1")
#' 
#' @export
conditionalEffectsPlot <- function(obj, zsel="id", gxsel="g1", colour="",
                                   show.ci=FALSE, regression="default",
                                   regression.ci=FALSE){
  
  stopifnot(inherits(obj, "effectlite")) 
  stopifnot(regression %in% c("default","smooth","linear","none"))
  
  condeffects <- obj@results@condeffects
  
  stopifnot(!"id" %in% names(condeffects)) ## check that nobody used id as covariate name
  stopifnot(zsel %in% names(condeffects) | zsel=="id")
  stopifnot(gxsel %in% names(condeffects))
  
  if(regression == "default"){
    if(zsel=="id"){
      regression <- "none"
    }else{
      regression <- "smooth"
    }
  }
  
  condeffects$id <- 1:nrow(condeffects)
  yselected <- round(condeffects[[gxsel]],4)    
  zselected <- condeffects[[zsel]]
  colourselected <- condeffects[[colour]]
  
  ## compute confidence intervals
  if(show.ci){
    seselected <- condeffects[[paste0("se_",gxsel)]]
    ci_upp <- yselected + stats::qnorm(0.975)*seselected
    ci_low <- yselected + stats::qnorm(0.025)*seselected
  }
  
  g1label <- "(K,Z)"
  if(!"K" %in% names(condeffects)){g1label <- "(Z)"}
  
  plotheader <- paste0("Estimated regression of ",
                       paste0(gxsel,g1label), " on ", 
                       zsel)
  if(zsel == "id"){
    plotheader <- "Estimated conditional effects in the sample"
  }
  
  p <- ggplot2::ggplot(data=condeffects, ggplot2::aes(y=yselected, x=zselected))
  p <- p + ggplot2::geom_point()
  p <- p + ggplot2::ggtitle(plotheader)
  p <- p + ggplot2::ylab(paste0(gxsel,g1label)) + ggplot2::xlab(zsel)
  
  if(regression == "smooth"){
    p <- p + ggplot2::geom_smooth(method="loess", se=regression.ci, formula=y~x)
  }else if(regression == "linear"){
    p <- p + ggplot2::geom_smooth(method="lm", se=regression.ci, formula=y~x)
  }
  
  if(show.ci & !is.null(colourselected)){
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin=ci_low, ymax=ci_upp,
                                                 colour=colourselected))
  }
  if(show.ci & is.null(colourselected)){
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin=ci_low, ymax=ci_upp))
  }  
  if(!is.null(colourselected)){
    p <- p + ggplot2::geom_point(ggplot2::aes(colour=colourselected))
    p <- p + ggplot2::guides(colour = ggplot2::guide_legend(colour))
  }
  p <- p + ggplot2::theme_bw()
  
  return(p)
  
}

