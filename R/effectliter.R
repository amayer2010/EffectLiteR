## package EffectLiteR

######################## class definitions #####################

## structure of class effectlite
# - user input (class input)
# - parameter names (class parnames)
# - generated lavaansyntax (class lavsyntax)
# - obtained results (class results)

setClass("input", representation(
  vnames="list", ## variable names
  vlevels="list", ## variable levels (for x, k, kstar and cell)
  control="character",
  ng="integer", ## number of treatment groups
  nz="integer", ## number of z
  nk="integer", ## number of unfolded categories of K  
  data="data.frame", 
  measurement="character",
  fixed.cell ="logical",
  missing="character",
  se="character", ## lavaan standard errors
  bootstrap="numeric", ## number of bootstrap draws
  interactions="character", ## type of interaction (all, 2-way, no)
  complexsurvey="list",
  homoscedasticity="logical",
  outprop="list" ## output from propensity score model
)
)

setClass("parnames", representation(
  alphas="array", 
  betas="array", 
  gammas="array", 
  gammalabels="array",
  cellmeanz="character",
  meanz="character",
  pk="character",
  px="character",
  Ezk="character",
  Pkgx="character", ## P(K=k|X=x)
  Pxgk="character", ## P(X=x|K=k)
  Ezgx="character", ## E(Z|X=x)
  Ezgk="character", ## E(Z|K=k)
  Ezkgx="character", ## E(Z*K|X=x)
  groupw="character",
  relfreq="character",
  Egx="character",
  Egxgx="character", ## E(gx|X=x)
  Egxgk="character", ## E(gx|K=k)
  Egxgxk="character", ## E(gx|X=x,K=k)
  adjmeans="character"
)
)

setClass("lavsyntax", representation(
  model="character", 
  hypotheses="list"
)
)


setClass("results", representation(
  lavresults="lavaan",
  hypotheses="data.frame",
  Egx="data.frame",
  Egxgx="data.frame",
  Egxgk="data.frame",
  Egxgxk="data.frame",
  gx="list",
  adjmeans="data.frame",
  condeffects="data.frame"
)
)


setClass("effectlite", representation(
  input="input",
  parnames="parnames",
  lavaansyntax="lavsyntax",
  results="results"
)
)


############## main function ##################

#' Estimate average and conditional effects
#' 
#' This function is the main funtion of the package and can be used to estimate
#' average and conditional effects of a treatment variable on an outcome variable,
#' taking into account any number of continuous and categorical covariates.
#' It automatically generates lavaan syntax for a multi-group structural equation
#' model, runs the model using lavaan, and extracts various average and conditional
#' effects of interest.
#' 
#' @param y Dependent variable (character string). Can be the name of a manifest variable or of a latent variable.
#' @param x Treatment variable (character string) treated as categorical variable.
#' @param k Vector of manifest variables treated as categorical covariates (character vector).
#' @param z Vector of continuous covariates (character vector). Names of both manifest and latent variables are allowed.
#' @param control Value of \code{x} that is used as control group.
#' @param measurement Measurement model. The measurement model is lavaan syntax (character string), that will be appended before the automatically generated lavaan input. It can be used to specify a measurement for a latent outcome variable and/or latent covariates. See also the example and \code{\link[EffectLiteR]{generateMeasurementModel}}.
#' @param data A data frame. 
#' @param fixed.cell logical. If \code{FALSE} (default), the group sizes are treated as stochastic rather than fixed.
#' @param missing Missing data handling. Will be passed on to \code{\link[lavaan]{sem}}.
#' @param se Type of standard errors. Will be 
#' passed on to \code{\link[lavaan]{sem}}.
#' @param bootstrap Number of bootstrap draws, if bootstrapping is used. Will be 
#' passed on to \code{\link[lavaan]{sem}}.
#' @param syntax.only logical. If \code{TRUE}, only syntax is returned and the model 
#' will not be estimated.
#' @param interactions character. Can be one of \code{c("all","none","2-way","X:K","X:Z")} and indicates the type of interaction used in the parameterization of the regression.
#' @param propscore Vector of covariates (character vector) that will be used to compute (multiple) propensity scores based on a multinomial regression without interactions. Alternatively, the user can specify a formula with the treatment variable as dependent variable for more control over the propensity score model.
#' @param ids Formula specifying cluster ID variables. Will be passed on to \code{\link[lavaan.survey]{lavaan.survey}}. See \code{\link[survey]{svydesign}} for details.
#' @param weights Formula to specify sampling weights. Currently only one weight variable is supported. Will be passed on to \code{\link[lavaan.survey]{lavaan.survey}}. See \code{\link[survey]{svydesign}} for details.
#' @param homoscedasticity logical. If \code{TRUE}, residual variances of the dependent variable are assumed to be homogeneous across cells.
#' @param ... Further arguments passed to \code{\link[lavaan]{sem}}.
#' @return Object of class effectlite.
#' @examples
#' ## Example with one categorical covariate
#' m1 <- effectLite(y="y", x="x", k="z", control="0", data=nonortho)
#' print(m1) 
#' 
#' ## Example with one categorical and one continuous covariate
#' m1 <- effectLite(y="dv", x="x", k=c("k1"), z=c("z1"), control="control", data=example01)
#' print(m1)
#' 
#' ## Example with latent outcome and latent covariate
#' measurement <- '
#' eta2 =~ 1*CPM12 + 1*CPM22
#' eta1 =~ 1*CPM11 + 1*CPM21
#' CPM11 + CPM12 ~ 0*1
#' CPM21 ~ c(m,m)*1
#' CPM22 ~ c(p,p)*1'
#'
#' m1 <- effectLite(y="eta2", x="x", z=c("eta1"), control="0", 
#'                  measurement=measurement, data=example02lv)
#' print(m1)
#' 
#'\dontrun{
#' ## Example with cluster variable and sampling weights
#' m1 <- effectLite(y="y", x="x", z="z", fixed.cell=TRUE, control="0", 
#'                     syntax.only=F, data=example_multilevel, 
#'                     ids=~cid, weights=~weights)
#' print(m1)
#' }
#' @export
#' @import lavaan
effectLite <- function(y, x, k=NULL, z=NULL, control="0", 
                       measurement=character(), data, fixed.cell=FALSE, 
                       missing="listwise", se="standard", bootstrap=1000L,
                       syntax.only=FALSE, interactions="all", 
                       propscore=NULL, ids=~0, weights=NULL, 
                       homoscedasticity=FALSE, ...){
  
  obj <- new("effectlite")  
  obj@input <- createInput(y,x,k,z,propscore,control,measurement,data, 
                           fixed.cell, missing, se, bootstrap,
                           interactions, ids, weights, homoscedasticity)
  obj@input <- computePropensityScore(obj@input)
  obj@parnames <- createParNames(obj)  
  obj@lavaansyntax <- createLavaanSyntax(obj)
  
  if(syntax.only){
    res <- obj@lavaansyntax@model    
  }else{
    obj@results <- computeResults(obj)
    res <- obj
  }
  
  return(res)  
}


################ methods #############################

#' @importMethodsFrom methods show
setMethod("show", "effectlite", function(object) {
  
  ng <- object@input@ng
  nk <- object@input@nk
  nz <- object@input@nz
  vnames <- object@input@vnames    
  vlevels <- object@input@vlevels
  gammas <- object@parnames@gammas
  gammalabels <- object@parnames@gammalabels
  
  label.g.function <- "(K,Z)"; label.covs <- ",K,Z"
  if(nk==1 & nz==0){label.g.function <- "()"; label.covs <- ""}
  if(nk>1 & nz==0){label.g.function <- "(K)"; label.covs <- ",K"}
  if(nk==1 & nz>0){label.g.function <- "(Z)"; label.covs <- ",Z"}
  
  cat("\n\n--------------------- Variables and Descriptive Statistics --------------------- \n\n")
  
  cat("Variable Names \n\n")
  cat("Outcome variable Y: ", paste0(vnames$y), "\n")
  cat("Treatment variable X: ", paste0(vnames$x), "\n")
  cat("Reference group (Control group): ", paste0(object@input@control), "\n")
  cat("Categorical covariates K: ", paste0(vnames$k), "\n")
  cat("Continuous covariates Z: ", paste0(vnames$z), "\n\n")
  
  if(nk>1){
    cat("Levels of Unfolded Categorical Covariate K \n")
    tmp <- vlevels$levels.k.original
    tmp <- tmp[length(tmp):1]
    tmp <- expand.grid(tmp)
    tmp$K <- vlevels$kstar
    tmp <- tmp[,ncol(tmp):1]
    print(tmp, row.names=F, print.gap=3)
    
    cat("\n")
    cat("Cells \n")
    tmp <- expand.grid(K=vlevels$kstar, X=vlevels$levels.x.original)[,2:1]
    tmp$Cell <- vlevels$cell
    print(tmp, print.gap=3)
    
  }
  
  if(nk==1){
    cat("Cells \n")
    tmp <- data.frame(X=vlevels$levels.x.original)
    print(tmp, row.names=F, print.gap=3)
    
  }
  
  cat("\n")
  cat("Cell Counts \n\n")
  cat("This table shows cell counts including missings. \n")
  cat("See also output under lavaan results for number of observations \n")
  cat("actually used in the analysis. \n\n")
  
  if(nk==1){
    print(ftable(object@input@data[vnames$x]), print.gap=3)
  }else{
    cellcounts <- as.formula(paste0(paste(vnames$k, collapse="+"), 
                                    "~", vnames$x))
    print(ftable(cellcounts, data=object@input@data), print.gap=3)
  }
 
  
  cat("\n\n --------------------- Regression Model --------------------- \n")
  
  tmp <- paste0("E(Y|X",label.covs,") = ")
  tmp <- paste0(tmp, "g0",label.g.function," + ")
  tmp <- paste0(tmp, paste0("g",1:(ng-1),label.g.function,"*I_X=",1:(ng-1), 
                            collapse=" + "))
  cat("\n",tmp, "\n")

  gammalabels2 <- gammalabels[,,1]
  gammalabels2[1] <- ""
  
  for(i in 1:ng){
    tmp <- paste0("  g",i-1,label.g.function," = ")
    tmp <- paste0(tmp, paste(gammas[,,i], gammalabels2, sep=" * ", collapse=" + "))
    tmp <- gsub("*  ", "", tmp, fixed=TRUE)
    if(length(gammalabels2)==1){tmp <- gsub("*", "", tmp, fixed=TRUE)}
    
    if(nchar(tmp) > 80){
      ## split g function over several lines
      tmp <- unlist(strsplit(tmp, " + ", fixed=TRUE))
      tmp <- capture.output(cat(tmp, sep=" + ", fill=80))
      tmp[2:length(tmp)] <- paste0("            + ",tmp[2:length(tmp)])
      cat(tmp, sep="\n")
    } else{
      cat(tmp, "\n")
    }
    
  }
  
    
  for(i in 1:ng){
    tmp <- paste0("g",i-1,label.g.function," Function")
    cat("\n",tmp, "\n\n")
    
    tmp <- object@results@gx[[i]]
    tmp[,2:5] <- round(tmp[,2:5], digits=3)
    print(tmp, print.gap=3, row.names=FALSE)
  }
  
  
  
  
  cat("\n\n--------------------- Main Hypotheses --------------------- \n\n")
  if(object@input@se != "standard" || object@input@interactions != "all"){
    cat("Wald tests for main hypotheses are currently not available for models with \n non-standard SEs and for models with restrictions on interactions.")
  }else{
    hypotheses <- object@results@hypotheses
    names(hypotheses) <- c("Wald Chi-Square", "df", "p-value")
    print(hypotheses, digits=3, print.gap=3)
  }
  
  cat("\n\n --------------------- Adjusted Means --------------------- \n\n")
  namesadjmeans <- paste0("Adj.Mean",0:(ng-1))
  adjmeans <- object@results@adjmeans
  row.names(adjmeans) <- namesadjmeans
  print(adjmeans, digits=3, print.gap=3)
  
  
  cat("\n\n --------------------- Average Effects --------------------- \n\n")
  namesEgx <- paste0("E[g",1:(ng-1),label.g.function,"]")
  Egx <- object@results@Egx
  row.names(Egx) <- namesEgx
  print(Egx, digits=3, print.gap=3)
  
  
  if(!(nz==0 & nk==1)){
    cat("\n\n --------------------- Effects given a Treatment Condition --------------------- \n\n")
    tmp <- expand.grid(g=1:(ng-1), x=0:(ng-1))
    namesEgxgx <- paste0("E[g",tmp$g,label.g.function,"|X=",tmp$x, "]")
    Egxgx <- object@results@Egxgx
    row.names(Egxgx) <- namesEgxgx
    print(Egxgx, digits=3, print.gap=3)
    
  }
  
  if(nk>1){
    cat("\n\n --------------------- Effects given K=k --------------------- \n\n")
    tmp <- expand.grid(g=1:(ng-1), k=0:(nk-1))
    namesEgxgk <- paste0("E[g",tmp$g,label.g.function,"|K=",tmp$k,"]")
    Egxgk <- object@results@Egxgk
    row.names(Egxgk) <- namesEgxgk
    print(Egxgk, digits=3, print.gap=3)    
  }

  if(nk>1 & nz>0){
    cat("\n\n --------------------- Effects given X=x, K=k --------------------- \n\n")
    Egxgxk <- paste0("Eg",tmp$g,"gx",tmp$x,"k",tmp$k)    
    tmp <- expand.grid(g=1:(ng-1), x=0:(ng-1), k=0:(nk-1))
    namesEgxgxk <- paste0("E[g",tmp$g,label.g.function,"|X=",tmp$x,", K=",tmp$k,"]")
    Egxgxk <- object@results@Egxgxk
    row.names(Egxgxk) <- namesEgxgxk
    print(Egxgxk, digits=3, print.gap=3)    
  }
  
  
  propscore <- object@input@vnames$propscore
  if(!is.null(propscore)){
    cat("\n\n --------------------- Output Propensity Score Model --------------------- \n\n")
    cat(object@input@outprop$formula, "\n")
    cat("\nEstimate\n")
    print(object@input@outprop$coef, digits=3, print.gap=3)
    cat("\nStandard Errors\n")
    print(object@input@outprop$se, digits=3, print.gap=3)
    cat("\nEst./SE\n")
    print(object@input@outprop$tval, digits=3, print.gap=3)
  }
    

})




#' Plot conditional effects
#' 
#' Can be used to make a conditional effects plot with an effect function on the
#' y axis and a covariate on the x axis. \code{ggplot2} is used to create the plot.
#' 
#' @param obj Object of class \code{effectlite} obtained from fitting an effect 
#' model using \code{\link[EffectLiteR]{effectLite}} 
#' @param zsel Name of a covariate (character string) plotted on the x-axis.
#' @param gxsel Name of an effect function (character string) plotted on the y-axis.
#' @param colour Name of a covariate (character string) used as colour variable 
#' in the plot.
#' @return Object of class \code{c("gg", "ggplot")}.
#' @examples
#' m1 <- effectLite(y="dv", x="x", k="k1", z="z1", control="control", data=example01)
#' conditionalEffectsPlot(m1, zsel="z1", gxsel="g1", colour="k1")
#' 
#' @export
conditionalEffectsPlot <- function(obj, zsel, gxsel="g1", colour=""){
  
  stopifnot(class(obj) == "effectlite")  
  
  condeffects <- obj@results@condeffects
  
  stopifnot(zsel %in% names(condeffects))
  stopifnot(gxsel %in% names(condeffects))
  
  yselected <- round(condeffects[[gxsel]],4)    
  zselected <- condeffects[[zsel]]
  colourselected <- condeffects[[colour]]
  
  g1label <- "(K,Z)"
  if(!"K" %in% names(condeffects)){g1label <- "(Z)"}
  
  p <- ggplot2::qplot(y=yselected, x=zselected, 
             data=condeffects,
             ylab=paste0(gxsel,g1label),
             xlab=zsel,                 
             main=paste0("Estimated regression of ",
                         paste0(gxsel,g1label), " on ", 
                         zsel))
  p <- p + ggplot2::geom_smooth(method="loess")
  p <- p + ggplot2::geom_point(ggplot2::aes(colour=colourselected))
  p <- p + ggplot2::guides(colour = ggplot2::guide_legend(colour))            
  p <- p + ggplot2::theme_bw()
  
  return(p)
    
}


################ constructor functions #########################

createInput <- function(y, x, k, z, propscore, control, measurement, data, 
                        fixed.cell, missing, se, bootstrap,
                        interactions, ids, weights, homoscedasticity){
  
  d <- data
  vnames <- list(y=y,x=x,k=k,z=z,propscore=propscore)  
    
  ## treatment variable
  if(!is.factor(d[,x])){    
    d[,x] <- as.factor(d[,x])  
  }
  stopifnot(length(levels(d[,x])) <= 10) # test if it works for > 10 (problems with subscripts?)
  
  d[,x] <- relevel(d[,x], control)
  levels.x.original <- levels(d[,x])
  levels(d[,x]) <- paste(0:(length(levels(d[,x]))-1))  
  
  ## categorical covariates
  levels.k.original <- vector("list",length(k))
  names(levels.k.original) <- k
  
  if(!is.null(k)){    
    for(i in 1:length(k)){
      d[,k[i]] <- as.factor(d[,k[i]])
      levels.k.original[[i]] <- levels(d[,k[i]])
      levels(d[,k[i]]) <- paste(0:(length(levels(d[,k[i]]))-1))
    }    
  }
  
  ## unfolded k variable
  levels.kstar.original <- vector("character")
  if(!is.null(k)){
    if(length(k)>1){
      d$kstar <- apply(d[,k],1,paste,collapse="")
      d$kstar <- as.factor(d$kstar)    
    }else{
      d$kstar <- d[,k]
    }
    levels.kstar.original <- levels(d$kstar)
    levels(d$kstar) <- paste(0:(length(levels(d$kstar))-1))
    
    ## check for empty cells
    if(any(table(d$kstar, d[,x]) == 0)){
      stop("EffectLiteR error: Empty cells are currently not allowed.")
    }    
    stopifnot(length(levels(d$kstar)) <= 10)    
    
  }else{
    d$kstar <- NULL
  }
  
  ## cell variable (xk-cells)
  if(!is.null(k)){
    cell <- expand.grid(k=levels(d$kstar), x=levels(d[,x]))
    cell <- with(cell, paste0(x,k))
    dsub <- cbind(d[,x],d$kstar)
    d$cell <- apply(dsub, 1, function(x){
      missing_ind <- sum(is.na(x)) > 0
      if(missing_ind){
        return(NA)
      }else{
        return(paste(x, collapse=""))
      }
    }) 
    d$cell <- as.factor(d$cell)    
  }else{
    cell <- levels(d[,x])
    d$cell <- d[,x]
  }  
  
  ## add vlevels for created variables
  vlevels <- list(levels.x.original=levels.x.original,
                  levels.k.original=levels.k.original,
                  levels.kstar.original=levels.kstar.original,
                  x=levels(d[,x]),
                  kstar=levels(d$kstar),
                  cell=levels(d$cell))
  
  
  ## nk
  nk <- 1L
  if(!is.null(k)){
    nk <- length(levels(d$kstar))
  }
    
  ## ng
  ng <- length(levels(d[,x]))  
  
  complexsurvey <- list(ids=ids, weights=weights)
  
  ## non-standard se only work with fixed group sizes
  if(se != "standard" & fixed.cell==FALSE){
        
    stop("EffectLiteR error: Non-standard SEs currently only work with fixed cell sizes. Please use fixed.cell=TRUE.")
    
  }
  
  res <- new("input",
             vnames=vnames, 
             vlevels=vlevels,
             ng=ng,
             nz=length(vnames$z),
             nk=nk,
             control=control,
             data=d, 
             measurement=measurement,
             fixed.cell=fixed.cell,
             missing=missing,
             se=se,
             bootstrap=bootstrap,
             interactions=interactions,
             complexsurvey=complexsurvey,
             homoscedasticity=homoscedasticity
  )
  
  return(res)
}


createParNames <- function(obj){
  
  inp <- obj@input
  ng <- inp@ng ## number of treatment groups
  nz <- inp@nz ## number of z
  nk <- inp@nk ## number of unfolded categories of K
  
  # create list for alpha, beta and gamma coefficients
  tmp <- expand.grid(z=0:nz, k=0:(nk-1), x=0:(ng-1))
  alphas <- with(tmp, array(paste0("a",x,k,z), dim=c(nz+1,nk,ng)))
  betas <- with(tmp, array(paste0("b",x,k,z), dim=c(nz+1,nk,ng)))
  gammas <- with(tmp, array(paste0("g",x,k,z), dim=c(nz+1,nk,ng)))
  
  ## for pretty printing
  gammalabels <- with(tmp, paste0("I_X=",x, " * I_K=",k, " * Z",z))
  
  ## delete entries with zeros in it
  gammalabels <- gsub("I_X=0 * ", "", gammalabels, fixed=TRUE)
  gammalabels <- gsub("I_K=0 * ", "", gammalabels, fixed=TRUE) 
  gammalabels <- gsub(" * Z0", "", gammalabels, fixed=TRUE)
  gammalabels[1] <- "Intercept"
  gammalabels <- array(gammalabels, dim=c(nz+1,nk,ng))
  
  pk <- paste0("Pk",1:nk)
  px <- paste0("Px",0:(ng-1))
  if(nz>0){
    tmp <- expand.grid(z=1:nz, k=0:(nk-1), x=0:(ng-1))
    cellmeanz <- with(tmp, paste0("mz",x,k,z))
    meanz <- paste0("Ez",1:nz)  
    tmp <- expand.grid(k=0:(nk-1), z=inp@vnames$z)
    Ezk <- with(tmp, paste0("E",z,"K",k))    
  }else{
    cellmeanz <- meanz <- Ezk <- character()
  }
  
  groupw <- paste0("gw",inp@vlevels$cell)
  relfreq <- paste0("relfreq",inp@vlevels$cell)
  Egx <- paste0("Eg",1:(ng-1))
  adjmeans <- paste0("adjmean",0:(ng-1))
  
  ## P(K=k|X=x)
  Pkgx=paste0("Pk",0:(nk-1),"gx",rep(0:(ng-1), each=nk)) 
  
  ## P(X=x|K=k)
  Pxgk=paste0("Px",0:(ng-1),"gk",rep(0:(nk-1), each=ng)) 
  
  ## E(Z|X=x)
  Ezgx <- character()
  if(nz>0){Ezgx=paste0("Ez",1:nz,"gx",rep(0:(ng-1), each=nz))} 
  
  ## E(Z|K=k)
  Ezgk <- character()
  if(nz>0){Ezgk=paste0("Ez",1:nz,"gk",rep(0:(nk-1), each=nz))} 
  
  ## E(Z*K|X=x)
  Ezkgx <- character()
  if(nz>0 & nk>1){
    tmp <- expand.grid(z=1:nz, k=0:(nk-1), x=0:(ng-1))
    Ezkgx=paste0("Ez",tmp$z,"k",tmp$k,"gx",tmp$x)
  } 
  
  ## E(gx|X=x)
  tmp <- expand.grid(g=1:(ng-1), x=0:(ng-1))
  Egxgx <- paste0("Eg",tmp$g,"gx",tmp$x)
  
  ## E(gx|K=k)
  tmp <- expand.grid(g=1:(ng-1), k=0:(nk-1))
  Egxgk <- paste0("Eg",tmp$g,"gk",tmp$k)
  
  ## E(gx|X=x,K=k)
  tmp <- expand.grid(g=1:(ng-1), x=0:(ng-1), k=0:(nk-1))
  Egxgxk <- paste0("Eg",tmp$g,"gx",tmp$x,"k",tmp$k)
  
  res <- new("parnames",
             alphas=alphas,
             betas=betas,
             gammas=gammas,
             gammalabels=gammalabels,
             cellmeanz=cellmeanz,
             meanz=meanz,
             pk=pk,
             px=px,         
             Ezk=Ezk,
             Pkgx=Pkgx,
             Pxgk=Pxgk,         
             Ezgx=Ezgx,
             Ezgk=Ezgk,             
             Ezkgx=Ezkgx,
             groupw=groupw,
             relfreq=relfreq,
             Egx=Egx,
             Egxgx=Egxgx,
             Egxgk=Egxgk,
             Egxgxk=Egxgxk,
             adjmeans=adjmeans
  )
  
  return(res)  
}


createLavaanSyntax <- function(obj) {
  inp <- obj@input
  parnames <- obj@parnames
  
  y <- inp@vnames$y
  z <- inp@vnames$z  
  ng <- inp@ng
  nz <- inp@nz
  nk <- inp@nk
  fixed.cell <- inp@fixed.cell
  alphas <- parnames@alphas
  betas <- parnames@betas
  gammas <- parnames@gammas
  
  
  ## measurement model
  model <- inp@measurement
  
  ## syntax intercepts  
  tmp <- paste0(y," ~ c(", paste(alphas[1,,],collapse=","), ")*1")
  model <- paste(model, tmp, sep="\n")
  
  ## syntax regression coefficients in each cell
  if (nz>0) {
    for (i in 1:nz) {
      tmp <- paste0(y," ~ c(", paste(alphas[i+1,,],collapse=","), ")*",z[i])
      model <- paste0(model, "\n", tmp)
    }
  }
  
  ## syntax mean z in each cell
  if (nz>0) {
    cellmeanz <- matrix(parnames@cellmeanz, nrow=nz)    
    for (i in 1:nz) {
      tmp <- paste0(z[i]," ~ c(", paste(cellmeanz[i,],collapse=","), ")*1")
      model <- paste0(model, "\n", tmp)
    }
  }
  
  ## syntax covariances between z in each cell
  if(nz > 1){
    for(i in 1:nz){
      for(k in nz:1){
        if(i < k){
          tmp <- paste0(z[i]," ~~ ", z[k])
          model <- paste0(model, "\n", tmp)
        }
      }
    }
  }
  
  ## homoscedastic residual variances
  if(inp@homoscedasticity){
    tmp <- paste0(y, " ~~ c(", 
                  paste(rep("veps", times=ng*nk),collapse=","),
                  ")*", y)
    model <- paste0(model, "\n", tmp)
  }
    
  ## compute relative frequencies
  relfreq <- obj@parnames@relfreq    
  
  if(fixed.cell){
    N <- nrow(obj@input@data)
    observed.freq <- table(obj@input@data$cell)/N
    
    # change observed frequencies if we have sampling weights
    if(!is.null(obj@input@complexsurvey$weights)){
      weights <- model.matrix(obj@input@complexsurvey$weights,
                              obj@input@data)
      if(ncol(weights) > 2){stop("EffectLiteR error: Currently only support for one
                                 weights variable")}
      weights <- weights[,-1]
      observed.freq <- tapply(weights,obj@input@data$cell,sum)
      observed.freq <- observed.freq/sum(observed.freq) ## rescale to sum to one
    }
    
    
    tmp <- paste(paste0(relfreq, " := ", observed.freq), collapse="\n")
    model <- paste0(model, "\n", tmp)        
  }else{
    
    ## syntax group weights
    tmp <- paste0("group % c(", paste(parnames@groupw, collapse=","), ")*w")
    model <- paste0(model, "\n", tmp)
    
    groupw <- obj@parnames@groupw
    
    tmp <- paste(paste0("exp(", groupw, ")"), collapse=" + ")
    tmp <- paste0("N := ",tmp)
    model <- paste0(model, "\n", tmp)
    
    tmp <- paste(paste0(relfreq, " := exp(", groupw, ")/N"), collapse="\n")
    model <- paste0(model, "\n", tmp)    
  }
  
  
  ## create temporary beta and gamma arrays (to be overwritten in next step)
  beta <- parnames@betas
  gamma <- parnames@gammas
  
  ## compute gammas based on betas
  for(q in 1:(nz+1)){
    for(k in 1:nk){
      gamma[q,k,1] <- paste(gamma[q,k,1], beta[q,k,1], sep=" := ")
      for(x in 2:ng){
        gamma[q,k,x] <- paste0(gamma[q,k,x], " := ", 
                               beta[q,k,x], "-", beta[q,k,1])
      }
    }
  }
  
  ## compute betas based on alphas
  for(q in 1:(nz+1)){
    for(x in 1:ng){
      beta[q,1,x] <- paste(beta[q,1,x], alphas[q,1,x], sep=" := ")
      if(nk>1){
        for(k in 2:nk){
          beta[q,k,x] <- paste0(beta[q,k,x], " := ", 
                                alphas[q,k,x], "-", alphas[q,1,x])
        }        
      }
    }
  }
  
  model <- paste0(model, "\n", paste(beta, collapse="\n"))  
  model <- paste0(model, "\n", paste(gamma, collapse="\n"))
  
  
  ## compute unconditional means of z  
  if (nz>0) {
    meanz <- parnames@meanz
    cellmeanz <- matrix(parnames@cellmeanz, nrow=nz)    
    relfreq <- parnames@relfreq
    for (i in 1:nz) {
      tmp <- paste0(meanz[i]," := ", paste(cellmeanz[i,], relfreq, sep="*", collapse=" + "))
      model <- paste0(model, "\n", tmp)
    }
  }
  
  ## compute unconditional probabilities of K*=k
  if (nk>1) {
    pk <- parnames@pk
    relfreq <- matrix(parnames@relfreq, nrow=nk)
    for (i in 1:nk) {
      tmp <- paste0(pk[i], " := ", paste(relfreq[i,], collapse=" + "))
      model <- paste0(model, "\n", tmp)
    }
  }
  
  ## compute unconditional probabilities of X=x
  px <- parnames@px
  relfreq <- matrix(parnames@relfreq, nrow=nk)
  for (i in 1:ng) {
    tmp <- paste0(px[i], " := ", paste(relfreq[,i], collapse=" + "))
    model <- paste0(model, "\n", tmp)
  }
  
  ## compute unconditional means of Z*K 
  if (nk>1 & nz>0) {
    
    Ezk <- array(parnames@Ezk, dim=c(nk,nz))
    cellmeanz <- array(parnames@cellmeanz, dim=c(nz,nk,ng))
    relfreq <- array(parnames@relfreq, dim=c(nk,ng))
    
    for(q in 1:nz){
      for(k in 1:nk){
        tmp <- paste0(Ezk[k,q], " := ", 
                      paste(cellmeanz[q,k,1:ng], relfreq[k,], sep="*", collapse=" + "))
        model <- paste0(model, "\n", tmp)
      }
    }
    
  }
  
  ## create vector of unconditional expectations of Z, K, Z*K
  pk <- parnames@pk[-1]
  meanz <- Ezk <- pkEzk <- character()
  
  if(nz>0){meanz <- parnames@meanz}  
  if(nk>1 & nz>0){Ezk <- c(matrix(parnames@Ezk, nrow=nk)[-1,])}  
  if(nk>1){pkEzk <- c(matrix(c(pk,Ezk), ncol=nk-1, byrow=T))}
  
  expectations <- c(1,meanz,pkEzk)
  
  ## average total effects  
  for(i in 2:ng){
    tmp <- paste0(parnames@Egx[i-1]," := ",
                  paste(gammas[,,i],expectations, sep="*", collapse=" + "))
    model <- paste0(model, "\n", tmp)
  }
  
  ## adjusted means
  for(i in 1:ng){
    tmp <- paste0(parnames@adjmeans[i]," := ",
                  paste(betas[,,i],expectations, sep="*", collapse=" + "))
    model <- paste0(model, "\n", tmp)
  }
  
  ## conditional probabilities of K=k given X=x (Pkgx)
  relfreq <- matrix(parnames@relfreq, nrow=nk)
  Pkgx <- matrix(parnames@Pkgx, nrow=nk)
  px <- parnames@px
  for(i in 1:ng){
    for(k in 1:nk){
      Pkgx[k,i] <- paste0(Pkgx[k,i], " := ", relfreq[k,i], "/", px[i])
    }
  }   
  model <- paste0(model, "\n", paste(Pkgx, collapse="\n"))  
  
  ## conditional probabilities of X=x given K=k (Pxgk)
  if(nk>1){
    relfreq <- matrix(parnames@relfreq, nrow=nk)
    Pxgk <- matrix(parnames@Pxgk, nrow=ng)
    pk <- parnames@pk
    for(i in 1:ng){
      for(k in 1:nk){
        Pxgk[i,k] <- paste0(Pxgk[i,k], " := ", relfreq[k,i], "/", pk[k])
      }
    }   
    model <- paste0(model, "\n", paste(Pxgk, collapse="\n"))      
  }
  
  ## conditional expectations of Z given X=x (Ezgx)
  Ezgx <- character()
  if(nz!=0){
    cellmeanz <- array(parnames@cellmeanz, dim=c(nz,nk,ng))
    Ezgx <- matrix(parnames@Ezgx, nrow=nz)
    Pkgx <- matrix(parnames@Pkgx, nrow=nk)
    for(i in 1:ng){
      for(q in 1:nz){
        Ezgx[q,i] <- paste0(Ezgx[q,i], " := ", 
                            paste(cellmeanz[q,1:nk,i], Pkgx[,i], sep="*", collapse=" + "))
      }
    }    
  }
  model <- paste0(model, "\n", paste(Ezgx, collapse="\n"))  
  
  
  ## conditional expectations of Z given K=k (Ezgk)
  Ezgk <- character()
  if(nz>0 & nk>1){
    cellmeanz <- array(parnames@cellmeanz, dim=c(nz,nk,ng))
    Ezgk <- matrix(parnames@Ezgk, nrow=nz)
    Pxgk <- matrix(parnames@Pxgk, nrow=ng)
    for(i in 1:nk){
      for(q in 1:nz){
        Ezgk[q,i] <- paste0(Ezgk[q,i], " := ", 
                            paste(cellmeanz[q,i,1:ng], Pxgk[,i], sep="*", collapse=" + "))
      }
    }
    model <- paste0(model, "\n", paste(Ezgk, collapse="\n"))
  }  
  
  
  
  ## conditional expectations of Z*K given X=x (Ezkgx)
  Ezkgx <- character()
  if(nz>0 & nk>1){
    Ezkgx <- array(parnames@Ezkgx, dim=c(nz,nk,ng))
    cellmeanz <- array(parnames@cellmeanz, dim=c(nz,nk,ng))
    Pkgx <- array(parnames@Pkgx, dim=c(nk,ng))
    
    for(i in 1:ng){
      for(k in 1:nk){
        for(q in 1:nz){
          Ezkgx[q,k,i] <- paste0(Ezkgx[q,k,i], " := ", cellmeanz[[q,k,i]],
                                 "*", Pkgx[k,i])
        }
      }
    }    
  }
  model <- paste0(model, "\n", paste(Ezkgx, collapse="\n"))  
  
  
  ## create matrix of conditional expectations of Z, K, Z*K given X=x
  ##TODO: maybe we can get rid of the if conditions and only use last one
  expectationsgx <- character()
  if(nz==0 & nk==1){
    expectationsgx <- matrix("1", nrow=ng)
  }
  if(nz>0 & nk==1){
    Ezgx <- matrix(parnames@Ezgx, nrow=ng, byrow=T)
    expectationsgx <- cbind("1", Ezgx)
  }
  if(nz==0 & nk>1){    
    Pkgx <- matrix(parnames@Pkgx, nrow=ng, byrow=T)[,-1]
    expectationsgx <- cbind("1", Pkgx)
  }
  if(nz>0 & nk>1){
    Ezgx <- matrix(parnames@Ezgx, nrow=ng, byrow=T)
    
    select <- c(matrix(parnames@Pkgx, nrow=ng, byrow=T)[,-1])
    Pkgx <- matrix(select, nrow=ng)
    
    select <- c(array(parnames@Ezkgx, dim=c(nz,nk,ng))[,-1,])
    Ezkgx <- array(select, dim=c(nz,nk-1,ng))
    
    tmp1 <- matrix(Pkgx[1,], nrow=nk-1, ncol=1)
    tmp2 <- matrix(Ezkgx[,,1], nrow=nk-1, ncol=nz, byrow=TRUE)
    expectationsgx <- c(1, Ezgx[1,], c(t(cbind(tmp1, tmp2))))

    for(i in 2:ng){
      tmp1 <- matrix(Pkgx[i,], nrow=nk-1, ncol=1)
      tmp2 <- matrix(Ezkgx[,,i], nrow=nk-1, ncol=nz, byrow=TRUE)      
      expectationsgx <- rbind(expectationsgx,
                              c(1, Ezgx[i,], c(t(cbind(tmp1, tmp2)))))
    }
            
  }
  
  
  ## Effects given a treatment condition  
  Egxgx <- matrix(parnames@Egxgx, ncol=ng)
  
  for(gx in 1:ng){
    for(i in 2:ng){
      tmp <- paste0(Egxgx[i-1,gx], " := ", 
                    paste(c(gammas[,,i]),expectationsgx[gx,], sep="*", collapse=" + "))
      model <- paste0(model, "\n", tmp)
      
    }    
  }
  
  
  ## Effects given a value k of K  
  Egxgk <- matrix(parnames@Egxgk, nrow=ng-1)
  Ezgk <- matrix(parnames@Ezgk, ncol=nk)
  Ezgk <- rbind("1", Ezgk)
  
  if(nk>1){
    
    for(i in 2:ng){     ## effects given K=0
      tmp <- paste0(Egxgk[i-1,1], " := ", 
                    paste(gammas[,1,i], Ezgk[,1], sep="*", collapse=" + ")) 
      model <- paste0(model, "\n", tmp)
    }
    
    for(i in 2:ng){     ## effects given K=1,...,k
      for(k in 2:nk){
        gammaselect <- c(gammas[,c(1,k),i])
        tmp <- paste0(Egxgk[i-1,k], " := ", 
                      paste(gammaselect, Ezgk[,k], sep="*", collapse=" + "))
        model <- paste0(model, "\n", tmp)
      }
    }
    
  }
  
  ## Effects given X=x and K=k
  Egxgxk <- array(parnames@Egxgxk, dim=c(ng-1,ng,nk))
  
  if(nk>1 & nz>0){
    
    for(t in 2:ng){
      for(x in 1:ng){
        for(k in 1:nk){        
          if(k==1){
            rhs <- paste0(gammas[,1,t],"*", c(1,cellmeanz[,1,x]), collapse=" + ")
            tmp <- paste(Egxgxk[t-1,x,k], ":=", rhs)
            model <- paste0(model, "\n", tmp) 
          }else{
            gammaselect <- c(gammas[,1,t], gammas[,k,t])
            cellmeanzselect <- rep(c(1,cellmeanz[,k,x]),2)
            rhs <- paste0(gammaselect,"*", cellmeanzselect, collapse=" + ")
            tmp <- paste(Egxgxk[t-1,x,k], ":=", rhs)
            model <- paste0(model, "\n", tmp)
          }
        }
      }
    }
    
  }
  
  
    ##Constraints about 2 and 3 way interactions
    stopifnot(inp@interactions %in% c("all","none","2-way","X:K","X:Z"))
    if(inp@interactions == "none"){      
      gammas <- matrix(c(parnames@gammas), ncol=ng)[-1,-1]
      model <- paste0(model, "\n", paste(gammas, "== 0", collapse="\n"))
    } 
    if(inp@interactions == "2-way"){
      if(nk>1 & nz>0){
        gammas <- parnames@gammas[2:(nz+1), 2:nk, 2:ng]
        model <- paste0(model, "\n", paste(gammas, "== 0", collapse="\n"))
      }
    }
  if(inp@interactions == "X:K"){
    if(nz>0){
      gammas <- parnames@gammas[2:(nz+1), , 2:ng]
      model <- paste0(model, "\n", paste(gammas, "== 0", collapse="\n"))
    }
  }
  if(inp@interactions == "X:Z"){
    if(nk>1){
      gammas <- parnames@gammas[, 2:nk, 2:ng]
      model <- paste0(model, "\n", paste(gammas, "== 0", collapse="\n"))
    }
  }
  
  
  ## Hypothesis 1: No average treatment effects
  hypothesis1 <- paste(parnames@Egx, "== 0", collapse="\n")
  
  ## Hypothesis 2: No covariate effects in control group
  gammas <- matrix(c(parnames@gammas), ncol=ng)[-1,1]
  hypothesis2 <- paste(gammas, "== 0", collapse="\n")
  
  ## Hypothesis 3: No treatment*covariate interaction
  gammas <- matrix(c(parnames@gammas), ncol=ng)[-1,-1]
  hypothesis3 <- paste(gammas, "== 0", collapse="\n")
  
  ## Hypothesis 4: No treatment effects
  gammas <- matrix(c(parnames@gammas), ncol=ng)[,-1]
  hypothesis4 <- paste(gammas, "== 0", collapse="\n")
  
  hypotheses <- list(hypothesis1=hypothesis1,
                     hypothesis2=hypothesis2,
                     hypothesis3=hypothesis3,
                     hypothesis4=hypothesis4)
  
  res <- new("lavsyntax",             
             model=model,
             hypotheses=hypotheses
  )
  
  return(res)
  
}


computeResults <- function(obj){
  
  sem.call <- call("sem", model=obj@lavaansyntax@model,
                   group="cell", missing=obj@input@missing,
                   se=obj@input@se, bootstrap=obj@input@bootstrap,
                   group.label=obj@input@vlevels$cell, data=obj@input@data,
                   fixed.x=FALSE, group.w.free = !obj@input@fixed.cell, 
                   mimic="mplus")
  
  m1 <- eval(sem.call)
        
  ## lavaan.survey -- complex survey designs
  ids <- obj@input@complexsurvey$ids
  weights <- obj@input@complexsurvey$weights
  
  if((ids != ~0) | (!is.null(weights))){
        
    if(!obj@input@fixed.cell){## currently only works for fixed cell sizes
      stop("EffectLiteR error: The complex survey functionality currently only works for fixed cell sizes. Please use fixed.cell=TRUE.")
    } 
    survey.design <- survey::svydesign(ids=ids, weights=weights, 
                                       data=obj@input@data)
    m1 <- lavaan.survey::lavaan.survey(lavaan.fit=m1, 
                                       survey.design=survey.design)    
  }
  
  
  est <- parameterEstimates(m1)$est ## parameter estimates
  se <- parameterEstimates(m1)$se ## standard errors
  names(se) <- names(est) <- parameterEstimates(m1)$label 
## Yves: what would be the best way to get (default) parameter names 
  tval <- est/se
  pval <- 2*(1-pnorm(abs(tval)))
  
  ng <- obj@input@ng
  nz <- obj@input@nz
  nk <- obj@input@nk  
  
  ## main hypotheses
  if(obj@input@se != "standard" | obj@input@interactions != "all"){ 
    ## no Wald Test for robust, bootstrapped SE...
    ## no Wald Test for interaction constraints (ask Yves to adjust...)
    ## maybe we could come up with something similar
    hypotheses <- data.frame()
  }else{
    if(nz==0 & nk==1){
      hypotheses <- data.frame(
        lavTestWald(m1, constraints=obj@lavaansyntax@hypotheses$hypothesis1)[1:3])    
      row.names(hypotheses) <- "No average effects"    
    }else{
      hypotheses <- data.frame(rbind(
        lavTestWald(m1, constraints=obj@lavaansyntax@hypotheses$hypothesis1)[1:3],
        lavTestWald(m1, constraints=obj@lavaansyntax@hypotheses$hypothesis2)[1:3],
        lavTestWald(m1, constraints=obj@lavaansyntax@hypotheses$hypothesis3)[1:3],
        lavTestWald(m1, constraints=obj@lavaansyntax@hypotheses$hypothesis4)[1:3]    
      ))
      row.names(hypotheses) <- c("No average effects",
                                 "No covariate effects in control group",
                                 "No treatment*covariate interaction",
                                 "No treatment effects")    
    }    
  }
    
  
  
  ## average total effects
  sdyx0 <- NA
  
  mm <- obj@input@measurement
  Pkgx <- est[obj@parnames@Pkgx][1:nk]
  vary <- meany <- numeric(nk)  
  
  ## manifest outcome variable  
  if(obj@input@vnames$y %in% names(obj@input@data)){
    
    fv <- fitted.values(m1)[1:nk] ## means and variances given X=0, K=k
    
    for(i in 1:nk){
      tmp <- fv[[i]]
      vary[i] <- tmp$cov[[obj@input@vnames$y,obj@input@vnames$y]]
      meany[i] <- tmp$mean[obj@input@vnames$y]
    }
    
  }else{ ## latent outcome variable
    
    fv.cov <- inspect(m1, what="cov.lv")[1:nk]
    fv.mean <- inspect(m1, what="mean.lv")[1:nk]    
    
    for(i in 1:nk){
      tmp.cov <- fv.cov[[i]]
      tmp.mean <- fv.mean[[i]]
      
      vary[i] <- tmp.cov[[obj@input@vnames$y,obj@input@vnames$y]]
      meany[i] <- tmp.mean[[obj@input@vnames$y]]
    }
    
  }

  meanyx0 <- sum(meany*Pkgx) ##TODO: compute parameter in model constraint
  sdyx0 <- sqrt(sum(vary*Pkgx) + sum(Pkgx*(meany-meanyx0)^2)) ## law of total variance  

  Egx <- data.frame(est[obj@parnames@Egx],
                    se[obj@parnames@Egx],
                    tval[obj@parnames@Egx],
                    pval[obj@parnames@Egx],
                    est[obj@parnames@Egx]/sdyx0)
  
  names(Egx) <- c("Estimate", "SE", "Est./SE", "p-value", "Effect Size")
  
  ## Effects given a treatment condition
  Egxgx <- data.frame(est[obj@parnames@Egxgx],
                      se[obj@parnames@Egxgx],
                      tval[obj@parnames@Egxgx],
                      pval[obj@parnames@Egxgx],
                      est[obj@parnames@Egxgx]/sdyx0)
  names(Egxgx) <- c("Estimate", "SE", "Est./SE", "p-value", "Effect Size")
  
  
  ## Effects given a a value K=k
  Egxgk <- data.frame()
  if(nk>1){
    Egxgk <- data.frame(est[obj@parnames@Egxgk],
                        se[obj@parnames@Egxgk],
                        tval[obj@parnames@Egxgk],
                        pval[obj@parnames@Egxgk],
                        est[obj@parnames@Egxgk]/sdyx0)
    names(Egxgk) <- c("Estimate", "SE", "Est./SE", "p-value", "Effect Size")    
  }

  ## Effects given X=x and K=k
  Egxgxk <- data.frame()
  if(nk>1 & nz>0){
    Egxgxk <- data.frame(est[obj@parnames@Egxgxk],
                        se[obj@parnames@Egxgxk],
                        tval[obj@parnames@Egxgxk],
                        pval[obj@parnames@Egxgxk],
                        est[obj@parnames@Egxgxk]/sdyx0)
    names(Egxgxk) <- c("Estimate", "SE", "Est./SE", "p-value", "Effect Size")    
  }
  
  
  ## g functions
  gammas <- matrix(obj@parnames@gammas, ncol=ng)
  gammalabels <- matrix(obj@parnames@gammalabels, ncol=ng)
  gx <- vector("list",ng)
  
  for(i in 1:ng){
    tmp <- data.frame(gammas[,i],
                      est[gammas[,i]],
                      se[gammas[,i]],
                      tval[gammas[,i]],
                      pval[gammas[,i]])
    names(tmp) <- c("Coefficient", "Estimate", "SE", "Est./SE", "p-value")
    rownames(tmp) <- gammalabels[,i]
    gx[[i]] <- tmp 
  }
  
  ## adjusted means
  adjmeans <- data.frame(est[obj@parnames@adjmeans],
                         se[obj@parnames@adjmeans],
                         tval[obj@parnames@adjmeans])
  names(adjmeans) <- c("Estimate", "SE", "Est./SE")
  
  ## conditional effects
  if(obj@input@se != "boot"){vcov <- computeVcovAdditionalParameters(m1)}
  condeffects <- computeConditionalEffects(obj, est, vcov)
  
  res <- new("results",
             lavresults=m1,
             hypotheses=hypotheses,
             Egx=Egx,
             Egxgx=Egxgx,
             Egxgk=Egxgk,
             Egxgxk=Egxgxk,
             gx=gx,
             adjmeans=adjmeans,
             condeffects=condeffects
  )
  
  return(res)
}



computeConditionalEffects <- function(obj, est, vcov){
  
  current.na.action <- options('na.action')
  on.exit(options(current.na.action))
  
  options(na.action='na.pass')
  
  ## required things
  z <- obj@input@vnames$z
  k <- obj@input@vnames$k
  x <- obj@input@vnames$x
  data <- obj@input@data  
  mm <- obj@input@measurement 
  nz <- obj@input@nz
  nk <- obj@input@nk
  ng <- obj@input@ng  

  if(length(mm) == 0 & obj@input@se != "boot"){
    
    if(nz==0 & nk==1){
      formula <- as.formula(" ~ 1")
      modmat <- model.matrix(formula, data=data)
      kz <- "00"
      dsub <- data.frame(matrix(vector(),nrow=nrow(data),ncol=0))
      
    }else if(nz>0 & nk==1){
      formula <- as.formula(paste0(" ~ ", paste(z, collapse=" + ")))
      modmat <- model.matrix(formula, data=data)
      kz <- paste0("0",0:nz)
      dsub <- data[,c(x,z)]
      
    }else if(nz==0 & nk>1){      
      formula <- as.formula(" ~ kstar")
      modmat <- model.matrix(formula, data=data)      
      kz <- paste0(1:nk-1,"0")
      dsub <- data[,c(x,"kstar",k)]
      names(dsub)[2] <- "K"
      
    }else if(nz>0 & nk>1){ 
      formula <- as.formula(paste0(" ~ ", 
                                   paste("kstar", z, sep="*", collapse=" + ")))
      modmat <- model.matrix(formula, data=data)            
      kz <- c(paste0(1:nk-1,"0"), paste0("0",1:nz))
      kz <- c(kz, paste0(rep(1:(nk-1),nz), rep(1:nz, each=nk-1)))
      dsub <- data[,c(x,"kstar",k,z)]
      names(dsub)[2] <- "K"
      
    }
    
    estimates <- est[paste0("g1",kz)]
    vcov_est <- vcov[paste0("g1",kz),paste0("g1",kz)]
    condeffects <- cbind(modmat %*% estimates)
    condeffects <- cbind(condeffects,
              apply(modmat,1,function(x){sqrt(t(x) %*% vcov_est %*% x)}))
    
    
    if(ng > 2){
      for(i in 3:ng){
        estimates <- est[paste0("g",i-1,kz)]
        vcov_est <- vcov[paste0("g",i-1,kz),paste0("g",i-1,kz)]
        condeffects <- cbind(condeffects, modmat %*% estimates)
        condeffects <- cbind(condeffects,
              apply(modmat,1,function(x){sqrt(t(x) %*% vcov_est %*% x)}))
      }      
    }  
    condeffects <- as.data.frame(condeffects)
    names(condeffects) <- paste0(rep(c("","se_"), times=ng-1),
                                 "g",
                                 rep(2:ng-1, each=2))
    condeffects <- cbind(dsub,condeffects)
    
    ## add variables used in the propscore model
    propscore <- obj@input@vnames$propscore
    if(!is.null(propscore)){
      
      d <- obj@input@data
      
      if(is(propscore, "formula")){      
        form <- propscore
      }else{
        form <- as.formula(paste0(x, " ~ ", paste0(propscore, collapse=" + ")))
      }
      
      dsub <- model.frame(form,data=d)
      condeffects <- condeffects[,-1]
      condeffects <- cbind(dsub, condeffects)
      
    }
    
  }else{
    condeffects <- data.frame()
  }
  
  return(condeffects)
  
}


computePropensityScore <- function(input){
  
  propscore <- input@vnames$propscore
  
  ## propensity score
  if(!is.null(propscore)){
        
    x <- input@vnames$x
    d <- input@data
    ng <- input@ng
    
    if(is(propscore, "formula")){      
      form <- propscore
      environment(form) <- environment()
      
    }else{
      form <- as.formula(paste0(x, " ~ ", paste0(propscore, collapse=" + ")))
    }
    
    mprop <- nnet::multinom(form, data=d, na.action="na.omit", trace=FALSE)
    
    ## save output
    resprop <- summary(mprop)
    outprop <- list()
    outprop$formula <- paste("Formula for Propensity Score Model",
                             deparse(form), sep="\n")
    outprop$coef <- resprop$coefficients
    outprop$se <- resprop$standard.errors
    outprop$tval <- resprop$coefficients/resprop$standard.errors
    input@outprop <- outprop
    
    ## fitted values
    dprop <- fitted(mprop)
    if(input@ng > 2){dprop <- dprop[,-1]}
    dprop <- apply(dprop,2,car::logit)       
    
    if(any(diag(var(dprop)) < 0.05)){
      warning(paste("very small variance of propensity scores \n ",
                    diag(var(dprop))))
    }
        
    dprop <- dprop[match(row.names(d), row.names(dprop)),] ## for missings    
    dprop <- as.data.frame(dprop) 
    names(dprop) <- paste0("logprop",1:(ng-1))
    input@data <- cbind(d,dprop)
    input@vnames$z <- c(input@vnames$z,paste0("logprop",1:(input@ng-1)))
    input@nz <- length(input@vnames$z)
    
  }
  
  return(input)
  
}



## helper function to compute the vcov matrix of additional paramers
## this is needed to compute the standard errors for conditional effects
## remove as soon as Yves has this functionality in lavaan
computeVcovAdditionalParameters <- function(object){
  
  ## free parameters only
  theta <- object@Fit@x
  
  # remove == constraints from parTable
  PT <- as.data.frame(object@ParTable, stringsAsFactors = FALSE)
  eq.idx <- which(PT$op == "==")
  if(length(eq.idx) > 0L) {
    PT <- PT[-eq.idx,]
  }
  partable <- as.list(PT)
  
  ## names new parameters
  def.idx <- which(partable$op == ":=")
  lhs.names <- partable$lhs[def.idx]
  
  ## def_function
  def.function <- lavaan:::lav_partable_constraints_def(partable)
  
  ## compute Jacobian
  JAC <- lav_func_jacobian_complex(func = def.function, x = theta)
  
  ## compute vcov of new parameters
  VCOV <- vcov(object, labels = FALSE)
  VCOV_np  <- JAC %*% VCOV %*% t(JAC)
  row.names(VCOV_np) <- colnames(VCOV_np) <- lhs.names
  
  return(VCOV_np)
}



############ shiny ##############

#' Shiny interface for effectLite
#' 
#' This function calls a shiny interface for effectLite.
#' 
#' @param launch.browser Option will be passed on to \code{\link[shiny]{runApp}}
#' @export
effectLiteGUI <- function(launch.browser=TRUE){  
  shiny::runApp(system.file('elrshiny', package='EffectLiteR'),
                launch.browser=launch.browser)
}


#' Generate measurement model
#' 
#' This function automatically generates \code{lavaan} syntax for the measurement model for a call to \code{\link[EffectLiteR]{effectLite}}. It is currently also used in the shiny interface.
#'
#' @param names A vector of character strings with names of latent variables.
#' @param indicators A list of vectors of character strings to specify indicators of latent variables (see example).
#' @param ncells Number of groups/cells.
#' @param model A vector of character strings of the same length as names. It is used to specify the type of measurement model for each of the latent variables. Each element can be one of \code{c("parallel","tau-equi","tau-cong")} indicating whether a parallel, essentially tau-equivalent, or tau-congeneric measurement model is used.
#' @examples
#' names <- c("etay", "etaz1", "etaz2")
#' indicators <- list("etay" = c("y1","y2","y3"), 
#'                    "etaz1" = c("z1","z2"),
#'                    "etaz2" = c("z12","z22","z32","z42"))
#' ncells = 6
#' model = c("parallel","tau-equi","tau-cong")
#' 
#' cat(generateMeasurementModel(names, indicators, ncells, model))
#' 
#' @export
generateMeasurementModel <- function(names, indicators, ncells, model){  

  stopifnot(length(names) == length(indicators))
  stopifnot(length(names) == length(model))
  for(i in 1:length(model)){
    stopifnot(model[i] %in% c("parallel","tau-equi","tau-cong"))
  }
  
  mm <- character(0)

  ## loadings
  for(i in 1:length(names)){
    for(k in 1:length(indicators[[i]])){ 
      
      if(model[i] == "parallel" | model[i] == "tau-equi"){
        loading <- 1
        
      }else if(model[i] == "tau-cong" & k==1){
        loading <- 1
        
      }else if(model[i] == "tau-cong" & k!=1){
        loading <- paste0("la1",k,i)
      }
      
      tmp <- paste0("c(",paste(rep(loading,ncells),collapse=","),")")
      tmp <- paste0(tmp,"*",indicators[[i]][k])
      tmp <- paste0(names[i], " =~ ", tmp)
      mm <- c(mm, tmp)
    }    
  }
  
  ##TODO method factors...
  
  ## intercepts
  for(i in 1:length(names)){
    for(k in 1:length(indicators[[i]])){ 
      
      if(model[i] == "parallel"){
        intercept <- 0
        
      }else if(model[i] == "tau-equi" | model[i] == "tau-cong"){
        if(k==1){
          intercept <- 0
        }else{
          intercept <- paste0("la0",k,i)
        }
      }
            
      tmp <- paste0("c(",paste(rep(intercept,ncells),collapse=","),")")
      tmp <- paste0(indicators[[i]][k], " ~ " , tmp,"*1")
      mm <- c(mm, tmp)
    }    
  }
  
  mm <- unique(mm) ## remove duplicate entries (for method factors and alike)
  mm <- paste(mm, collapse="\n")
  
  return(mm)
}



############## documentation ######################

#' EffectLiteR
#'
#' @name EffectLiteR
#' @docType package
NULL

#' Dataset nonortho.
#' 
#' A simulated dataset. The variables are:
#' 
#' \itemize{
#'   \item y. Continuous dependent variable depression.
#'   \item x. Treatment variable with values 0 (control), 1 (treat1), and 2 (treat2).
#'   \item z. Categorical covariate with values 0 (low neediness), 1 (medium neediness) and 2 (high neediness).
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 500 rows and 3 variables
#' @name nonortho
NULL



#' Dataset example01.
#' 
#' A simulated dataset. The variables are:
#' 
#' \itemize{
#'   \item x. Treatment variable with values control, treat1, and treat2.
#'   \item k1. Categorical covariate with values male and female.
#'   \item kateg2. Categorical covariate with values 1 and 2.
#'   \item z1-z3. Continuous covariates.
#'   \item dv. Coninuous dependent variable.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 2000 rows and 7 variables.
#' @name example01
NULL



#' Dataset example02lv.
#' 
#' A simulated dataset with latent variables. The variables are:
#' 
#' \itemize{
#'   \item CPM11. First indicator of latent covariate.
#'   \item CPM21. Second indicator of latent covariate.
#'   \item CPM12. First indicator of latent outcome.
#'   \item CPM22. Second indicator of latent outcome.
#'   \item x. Dichotomous treatment variable with values 0 (control), and 1 (treatment).
#'   \item k. Categorical covariate with values first, second, and third.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 300 rows and 6 variables.
#' @name example02lv
NULL


#' Dataset example_multilevel.
#' 
#' A simulated dataset with a cluster ID and sampling weights to test multilevel options. The variables are:
#' 
#' \itemize{
#'   \item y. Coninuous dependent variable.
#'   \item x. Treatment variable with values 0, 1.
#'   \item z. Continuous covariate.
#'   \item xz. Product of x and z.
#'   \item cid. Cluster ID.
#'   \item weights. Sampling weights.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 800 rows and 6 variables.
#' @name example_multilevel
NULL

