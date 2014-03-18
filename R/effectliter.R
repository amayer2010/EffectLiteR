## package effectliter

######################## class definitions #####################

## structure of class ELLavaanModel
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
  missing="character"
)
)

setClass("parnames", representation(
  alphas="array", 
  betas="array", 
  gammas="array", 
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
  gx="list",
  adjmeans="data.frame"
)
)


setClass("ELLavaanModel", representation(
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
#' model, runs the model in lavaan, and extracts various average and conditional
#' effects of interest.
#' 
#' @param y dependent variable (character string).
#' @param x treatment variable (character string).
#' @param k vector of categorical covariates (character vector).
#' @param z vector of continuous covariates (character vector).
#' @param measurement measurement model.
#' @param data a data frame. 
#' @param fixed.cell logical. If FALSE (default), the group sizes are treated as
#' stochastic rather than fixed.
#' @param missing missing data handling to be used by lavaan::sem(). Will be 
#' passed on to sem()
#' @param syntax.only logical. If TRUE, only syntax is returned and the model 
#' will not be estimated.
#' @param ... further arguments passed to lavaan::sem().
#' @return object of class ELLavaanModel.
#' @examples
#' m1 <- effectLite(y="y", x="x", z="z2", k="z1", control="0", data=daten1411)
#' print(m1) 
#' @export
#' @import lavaan
effectLite <- function(y, x, k=NULL, z=NULL, control="0", 
                       measurement=character(), data, fixed.cell=FALSE, missing="listwise",
                       syntax.only=FALSE, ...){
  
  obj <- new("ELLavaanModel")  
  obj@input <- createInput(y,x,k,z,control,measurement,data, 
                           fixed.cell, missing)
  obj@parnames <- createParNames(obj)  
  obj@lavaansyntax <- createLavaanSyntax(obj)
  
  if(syntax.only){
    res <- obj@lavaansyntax@model
    cat(res)    
  }else{
    obj@results <- computeResults(obj)
    res <- obj
  }
  
  return(res)  
}


################ methods #############################

setMethod("show", "ELLavaanModel", function(object) {
  
  ng <- object@input@ng
  nk <- object@input@nk
  nz <- object@input@nz
  vnames <- object@input@vnames    
  vlevels <- object@input@vlevels
  
  cat("\n\n------------------ Variables and Descriptive Statistics ------------------ \n\n")
  
  cat("Variable Names \n\n")
  cat("Outcome variable: ", paste0(vnames$y), "\n")
  cat("Treatment variable: ", paste0(vnames$x), "\n")
  cat("Reference group (Control group): ", paste0(object@input@control), "\n")
  cat("Categorical covariates: ", paste0(vnames$k), "\n")
  cat("Continuous covariates: ", paste0(vnames$z), "\n\n")
  
  if(nk>1){
    cat("Levels of Unfolded Categorical Covariate K \n")
    tmp <- vlevels$levels.k.original
    tmp <- tmp[length(tmp):1]
    tmp <- expand.grid(tmp)
    tmp$K <- vlevels$kstar
    tmp <- tmp[,ncol(tmp):1]
    print(tmp, row.names=F)
    
    cat("\n")
    cat("Cells \n")
    tmp <- expand.grid(K=vlevels$kstar, X=vlevels$levels.x.original)[,2:1]
    tmp$Cell <- vlevels$cell
    print(tmp)
    
  }
  
  if(nk==1){
    cat("Cells \n")
    tmp <- data.frame(X=vlevels$levels.x.original)
    print(tmp, row.names=F)
    
  }
  
  cat("\n")
  cat("Cell Counts \n\n")
  if(nk==1){
    print(ftable(object@input@data[vnames$x]))
  }else{
    cellcounts <- as.formula(paste0(paste(vnames$k, collapse="+"), 
                                    "~", vnames$x))
    print(ftable(cellcounts, data=object@input@data))
  }
  
  
  cat("\n\n------------------ Main Hypotheses ------------------ \n\n")
  print(object@results@hypotheses, digits=3)
  
  cat("\n\n ------------------ Average Effects ------------------ \n\n")
  namesEgx <- paste0("E[g",1:(ng-1),"(K,Z)]")
  Egx <- object@results@Egx
  row.names(Egx) <- namesEgx
  print(Egx, digits=3)
  
  
  cat("\n\n ------------------ Adjusted Means ------------------ \n\n")
  namesadjmeans <- paste0("Adj.Mean",0:(ng-1))
  adjmeans <- object@results@adjmeans
  row.names(adjmeans) <- namesadjmeans
  print(adjmeans, digits=3)
  
  if(!(nz==0 & nk==1)){
    cat("\n\n ------------------ Effects given a Treatment Condition ------------------ \n\n")
    tmp <- expand.grid(g=1:(ng-1), x=0:(ng-1))
    namesEgxgx <- paste0("E[g",tmp$g,"(K,Z)|X=",tmp$x, "]")
    Egxgx <- object@results@Egxgx
    row.names(Egxgx) <- namesEgxgx
    print(Egxgx, digits=3)
    
  }
  
  if(nk>1){
    cat("\n\n ------------------ Effects given K=k ------------------ \n\n")
    tmp <- expand.grid(g=1:(ng-1), k=0:(nk-1))
    namesEgxgk <- paste0("E[g",tmp$g,"(K,Z)|K=",tmp$k,"]")
    Egxgk <- object@results@Egxgk
    row.names(Egxgk) <- namesEgxgk
    print(Egxgk, digits=3)    
  }
  
  cat("\n\n ------------------ Intercept and Effect Functions ------------------ \n")
  
  for(i in 1:ng){
    tmp <- paste0("g",i-1,"(K,Z) Function")
    cat("\n",tmp, "\n\n")
    print(object@results@gx[[i]], digits=3)
  }
  
})



################ constructor functions #########################

createInput <- function(y, x, k, z, control, measurement, data, 
                        fixed.cell, missing){
  
  #   d <- data[c(y,x,k,z)] ## TODO: adjust for latent variables (indicator variables)
  
  d <- data
  vnames <- list(y=y,x=x,k=k,z=z)
  
  ## treatment variable
  if(!is.factor(d[,x])){    
    d[,x] <- as.factor(d[,x])  
  }
  stopifnot(length(levels(d[,x])) <= 10)
  
  if(!is.null(control)){
    d[,x] <- relevel(d[,x], control)  
  }
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
    stopifnot(length(levels(d$kstar)) <= 10)    
  }else{
    d$kstar <- NULL
  }
  
  ## cell variable (xk-cells)
  if(!is.null(k)){
    cell <- expand.grid(k=levels(d$kstar), x=levels(d[,x]))
    cell <- with(cell, paste0(x,k))
    d$cell <- as.factor(paste0(d[,x],d$kstar))    
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
  
  nk <- 1L
  if(!is.null(k)){
    nk <- length(levels(d$kstar))
  }
  
  res <- new("input",
             vnames=vnames, 
             vlevels=vlevels, 
             ng=length(levels(d[,x])),
             nz=length(z),
             nk=nk,
             control=control,
             data=d, 
             measurement=measurement,
             fixed.cell=fixed.cell,
             missing=missing
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
  
  res <- new("parnames",
             alphas=alphas,
             betas=betas,
             gammas=gammas,
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
  
  ## syntax group weights
  tmp <- paste0("group % c(", paste(parnames@groupw, collapse=","), ")*w")
  model <- paste0(model, "\n", tmp)
  
  ## compute relative frequencies
  relfreq <- obj@parnames@relfreq
  N <- nrow(obj@input@data)    
  
  if(fixed.cell){
    observed.freq <- table(obj@input@data$cell)/N
    tmp <- paste(paste0(relfreq, " := ", observed.freq), collapse="\n")
    model <- paste0(model, "\n", tmp)        
  }else{    
    groupw <- obj@parnames@groupw
    tmp <- paste(paste0(relfreq, " := exp(", groupw, ")/" , N), collapse="\n")
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
    
    expectationsgx <- c(1, Ezgx[1,], c(rbind(Pkgx[1,], Ezkgx[,,1])))
    for(i in 2:ng){
      expectationsgx <- rbind(expectationsgx,
                              c(1, Ezgx[i,], c(rbind(Pkgx[i,], Ezkgx[,,i]))))
    }    
  }
  
  
  ## Effects given a treatment condition  
  Egxgx <- matrix(parnames@Egxgx, ncol=ng)
  
  for(gx in 1:ng){
    for(i in 2:ng){
      tmp <- paste0(Egxgx[i-1,gx], " := ", 
                    paste(gammas[,,i],expectationsgx[gx,], sep="*", collapse=" + "))
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
  
  m1 <- sem(obj@lavaansyntax@model, group="cell", missing=obj@input@missing,
            group.label=obj@input@vlevels$cell, data=obj@input@data,
            fixed.x=F, group.w.free = TRUE, mimic="mplus") 
  
  #TODO: ask Yves for unclass(se(m1, type="user")): inspect(m1,"se") does not give SE for new parameters
  est <- unclass(coef(m1, type="user")) ## parameter estimates
  se <- m1@Fit@se ## standard errors
  names(se) <- names(est) 
  tval <- est/se
  pval <- 2*(1-pnorm(abs(tval)))
  
  ng <- obj@input@ng
  nz <- obj@input@nz
  nk <- obj@input@nk  
  
  ## main hypotheses
  if(nz==0 & nk==1){
    hypotheses <- data.frame(
      lavTestWald(m1, constraints = obj@lavaansyntax@hypotheses$hypothesis1))    
    row.names(hypotheses) <- "No average effects"    
  }else{
    hypotheses <- data.frame(rbind(
      lavTestWald(m1, constraints = obj@lavaansyntax@hypotheses$hypothesis1),
      lavTestWald(m1, constraints = obj@lavaansyntax@hypotheses$hypothesis2),
      lavTestWald(m1, constraints = obj@lavaansyntax@hypotheses$hypothesis3),
      lavTestWald(m1, constraints = obj@lavaansyntax@hypotheses$hypothesis4)    
    ))
    row.names(hypotheses) <- c("No average effects",
                               "No covariate effects in control group",
                               "No treatment*covariate interaction",
                               "No treatment effects")    
  }
  
  
  ## average total effects
  sdyx0 <- NA
  
  mm <- obj@input@measurement
  Pkgx <- est[obj@parnames@Pkgx][1:nk]
  vary <- meany <- numeric(nk)  
  
  ## manifest outcome variable  
  if(length(grep(obj@input@vnames$y,mm))==0){
    
    fv <- fitted.values(m1)[1:nk] ## means and variances given X=0, K=k
    
    for(i in 1:nk){
      tmp <- fv[[i]]
      vary[i] <- tmp$cov[[obj@input@vnames$y,obj@input@vnames$y]]
      meany[i] <- tmp$mean[obj@input@vnames$y]
    }
    
  }else if(grep(obj@input@vnames$y,mm) == 1){ ## latent outcome variable
    
    fv.cov <- inspect(m1, what="cov.lv")[1:nk]
    fv.mean <- lavaan:::computeEETA(m1@Model, samplestats=m1@SampleStats, remove.dummy.lv=TRUE)[1:nk]    
    
    for(i in 1:nk){
      tmp.cov <- fv.cov[[i]]
      tmp.mean <- fv.mean[[i]]
      names(tmp.mean) <- row.names(fv.cov[[1]])
      
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
  
  ## g functions
  gammas <- matrix(obj@parnames@gammas, ncol=ng)
  gx <- vector("list",ng)
  
  for(i in 1:ng){
    tmp <- data.frame(est[gammas[,i]],
                      se[gammas[,i]],
                      tval[gammas[,i]])
    names(tmp) <- c("Estimate", "SE", "Est./SE")
    gx[[i]] <- tmp 
  }
  
  ## adjusted means
  adjmeans <- data.frame(est[obj@parnames@adjmeans],
                         se[obj@parnames@adjmeans],
                         tval[obj@parnames@adjmeans])
  names(adjmeans) <- c("Estimate", "SE", "Est./SE")
  
  res <- new("results",
             lavresults=m1,
             hypotheses=hypotheses,
             Egx=Egx,
             Egxgx=Egxgx,
             Egxgk=Egxgk,
             gx=gx,
             adjmeans=adjmeans
  )
  
  return(res)
}

############ shiny ##############

#' Shiny interface for effectLite()
#' 
#' This function calls a shiny interface for effectLite().
#' 
#' @export
effectLiteGUI <- function(){  
  shiny::runApp(system.file('elrshiny', package='effectliter'))
}


############## documentation ######################

#' effectliter
#'
#' @name effectliter
#' @docType package
NULL

#' Dataset example01.
#' 
#' A simulated dataset. The variables are as follows:
#' 
#' \itemize{
#'   \item x. treatment variable with values control, treat1, and treat2
#'   \item k1. categorical covariate with values male and female
#'   \item kateg2. categorical covariate with values 1 and 2
#'   \item z1-z3. continuous covariates
#'   \item dv. coninuous dependent variable
#' }
#' 
#' @docType data
#' @keywords datasets
#' @format A data frame with 2000 rows and 7 variables
#' @name example01
NULL
