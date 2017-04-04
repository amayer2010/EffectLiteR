
#' Predict Conditional Effects
#' 
#' Predicts conditional treatment effects based on a fitted EffectLiteR model.
#' 
#' @param obj Object of class \code{effectlite}.
#' @param newdata An optional data.frame, containing the same continuous and 
#' categorical covariates as used when fitting the EffectLiteR model in
#' obj. Only covariates (and neither the dependent variable nor indicators for 
#' latent variables) should be included.
#' @param add.columns Used to request additional columns. Can be one or several of c("covariates", "modmat", "expected-outcomes", "prop-covariates").
#' @return Object of class \code{"data.frame"}.
#' @examples
#' m1 <- effectLite(y="dv", z=c("z1"), k=c("k1","kateg2"), x="x", 
#' control="control", data=example01)
#' newdata <- data.frame(k1="male", kateg2="1", z1=2)
#' elrPredict(m1, newdata)
#' @export
elrPredict <- function(obj, newdata=NULL, add.columns="expected-outcomes"){
  
  condeffects <- computeConditionalEffects(obj, newdata, add.columns)
  return(condeffects)
}


## TODO add documentation
computeConditionalEffects <- function(obj, newdata=NULL, 
                                      add.columns=c("covariates","expected-outcomes")){
  
  if(obj@input@method =="lm"){return(data.frame())} ## TODO change this
  
  stopifnot(inherits(obj, "effectlite"))
  
  current.na.action <- options('na.action')
  on.exit(options(current.na.action))
  
  options(na.action='na.pass')
  
  ## required things
  z <- obj@input@vnames$z
  k <- obj@input@vnames$k
  x <- obj@input@vnames$x
  
  nz <- obj@input@nz
  nk <- obj@input@nk
  ng <- obj@input@ng

  sep <- ""
  ## longer parameter names for many groups and/or covariates
  if(ng>9 | nk>9 | nz>9){sep <- "_"}
  
  latentz <- obj@input@vnames$latentz
  mm <- obj@input@measurement 
  
  if(!is.null(newdata)){
    
    stopifnot(all(c(z,k) %in% names(newdata))) ## no latent covariates in newdata

    #compute Kstar values first
    if(nk > 1){
      tmp <- obj@input@vlevels$levels.k.original
      tmp <- tmp[length(tmp):1]
      tmp <- expand.grid(tmp)
      tmp$kstar <- factor(obj@input@vlevels$kstar)
      
      ## add Kstar values to newdata
      newdata <- merge(newdata, tmp)
    }
    
    if(!x %in% names(newdata)){
      newdata[,x] <- NA
    }
    
    data <- newdata
    
  }else{
    
    data <- obj@input@data  
  }
  
  
  lavresults <- obj@results@lavresults
  est <- parameterEstimates(lavresults, fmi=FALSE)$est ## parameter estimates
  names(est) <- parameterEstimates(lavresults, fmi=FALSE)$label 
  vcov <- lavInspect(lavresults, "vcov.def", add.class = FALSE)
  
  data$id <- 1:nrow(data)
  
  ## add factor scores
  ## TODO Add newdata to lavPredict...
  if(length(latentz) > 0){
    fscores <- data.frame(do.call("rbind", lavPredict(lavresults)))
    fscores <- subset(fscores, select=latentz)
    fscores$id <- unlist(lavInspect(lavresults, "case.idx"))
    data <- merge(data,fscores)
  }
  
  ## compute formula and model.matrix  
  ## TODO use computeModelMatrix() function
  if(nz==0 & nk==1){
    formula <- as.formula(" ~ 1")
    modmat <- model.matrix(formula, data=data)
    kz <- paste0("0",sep,"0")
    dsub <- data.frame(matrix(vector(),nrow=nrow(data),ncol=0))
    
  }else if(nz>0 & nk==1){
    formula <- as.formula(paste0(" ~ ", paste(z, collapse=" + ")))
    modmat <- model.matrix(formula, data=data)
    kz <- paste0("0",sep,0:nz)
    dsub <- data[,c(x,z)]
    
  }else if(nz==0 & nk>1){      
    formula <- as.formula(" ~ kstar")
    modmat <- model.matrix(formula, data=data)      
    kz <- paste0(1:nk-1, sep, "0")
    dsub <- data[,c(x,"kstar",k)]
    names(dsub)[2] <- "K"
    
  }else if(nz>0 & nk>1){ 
    formula <- as.formula(paste0(" ~ ", 
                                 paste("kstar", z, sep="*", collapse=" + ")))
    modmat <- model.matrix(formula, data=data)            
    kz <- c(paste0(1:nk-1,sep,"0"), paste0("0",sep,1:nz))
    kz <- c(kz, paste0(rep(1:(nk-1),nz), sep, rep(1:nz, each=nk-1)))
    dsub <- data[,c(x,"kstar",k,z)]
    names(dsub)[2] <- "K"
    
  }
  
  estimates <- est[paste0("g1",sep,kz)]
  vcov_est <- vcov[paste0("g1",sep,kz),paste0("g1",sep,kz)]
  condeffects <- cbind(modmat %*% estimates)
  condeffects <- cbind(condeffects,
                       apply(modmat,1,function(x){sqrt(t(x) %*% vcov_est %*% x)}))
  
  if(ng > 2){
    for(i in 3:ng){
      estimates <- est[paste0("g",i-1,sep,kz)]
      vcov_est <- vcov[paste0("g",i-1,sep,kz),paste0("g",i-1,sep,kz)]
      condeffects <- cbind(condeffects, modmat %*% estimates)
      condeffects <- cbind(condeffects,
                           apply(modmat,1,function(x){sqrt(t(x) %*% vcov_est %*% x)}))
    }      
  }  
  
  condeffects <- as.data.frame(condeffects)
  names(condeffects) <- paste0(rep(c("","se_"), times=ng-1),
                               "g",
                               rep(2:ng-1, each=2))
  
  
  ##### What should be returned? effects + expected outcomes, model matrix, covariates?
  
  if("covariates" %in% add.columns){
    condeffects <- cbind(dsub,condeffects)
  }
  
  if("modmat" %in% add.columns){
    condeffects <- cbind(modmat,condeffects)
  }
  
  if("expected-outcomes" %in% add.columns){
    estimates <- est[paste0("b0",sep,kz)]
    trueoutcomes <- cbind(modmat %*% estimates)
    for(i in 1:(ng-1)){
      estimates <- est[paste0("b",i,sep,kz)]
      trueoutcomes <- cbind(trueoutcomes, modmat %*% estimates)
    }
    trueoutcomes <- as.data.frame(trueoutcomes)
    names(trueoutcomes) <- paste0("ExpOutc", 0:(ng-1))
    condeffects <- cbind(condeffects,trueoutcomes)
  }
  
  if("prop-covariates" %in% add.columns){
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
  }  
  
  
  return(condeffects)
  
}




# computeAggregatedEffects <- function(obj, covs){
#   
#   stopifnot(inherits(obj, "effectlite"))
#   stopifnot(length(obj@input@measurement) == 0) ## no latent variables for now
#   
#   ## required things
#   z <- obj@input@vnames$z
#   k <- obj@input@vnames$k
#   x <- obj@input@vnames$x
#   
#   nz <- obj@input@nz
#   nk <- obj@input@nk
#   ng <- obj@input@ng
#   
#   interactions <- obj@input@interactions
#   
#   ## compute formula
#   data <- obj@input@data
# 
#   if(nz==0 & nk==1){
#     formula <- as.formula(paste0(" ~ 1 + ", x))
#     
#   }else if(nz>0 & nk==1){
#     rhs <- paste(z, collapse=" + ")
#     rhs <- paste0(x, "*(", rhs, ")")
#     formula <- as.formula(paste0(" ~ ", rhs))
# 
#   }else if(nz==0 & nk>1){      
#     formula <- as.formula(paste0(" ~ ", x, "*kstar"))
# 
#   }else if(nz>0 & nk>1){
#     rhs <- paste(z, "kstar", sep="*", collapse=" + ")
#     rhs <- paste0(x, "*(", rhs, ")")
#     formula <- as.formula(paste0(" ~ ", rhs))
#   }
#   
#   modmat <- model.matrix(formula, data=data)
#   colnames(modmat)
#   
# }
# 
