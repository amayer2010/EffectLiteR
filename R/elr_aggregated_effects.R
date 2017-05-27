

#' Compute Aggregated Effects
#' 
#' Computes aggregates of conditional effects for a subset of the original dataset 
#' based on a fitted EffectLiteR model.
#' 
#' @param obj Object of class \code{effectlite}.
#' @param agg.subset Vector of integers indicating the row numbers of the 
#' original dataset for the subset used to compute the aggregated effect
#' @return Object of class \code{"data.frame"}.
#' @examples
#' m1 <- effectLite(y="dv", z=c("z1"), k=c("k1"), x="x", 
#' control="control", data=example01, fixed.cell=TRUE, fixed.z=TRUE)
#' newdata <- data.frame(k1=NA, z1=1)
#' agg.subset <- autoSelectSubset(m1, newdata)
#' computeAggregatedEffects(m1, agg.subset)
#' @export
computeAggregatedEffects <- function(obj, agg.subset){
  
  stopifnot(is.null(obj@input@complexsurvey$weights))
  
  ## required things
  nz <- obj@input@nz
  nk <- obj@input@nk
  ng <- obj@input@ng
  
  sep <- ""
  ## longer parameter names for many groups and/or covariates
  if(ng>9 | nk>9 | nz>9){sep <- "_"}
  
  ## lavaan results
  lavresults <- obj@results@lavresults
  est <- parameterEstimates(lavresults, fmi=FALSE)$est ## parameter estimates
  names(est) <- parameterEstimates(lavresults, fmi=FALSE)$label 
  vcov <- lavInspect(lavresults, "vcov.def", add.class = FALSE)
  
  ## conditional effects
  modmat <- elrPredict(obj, add.columns="modmat-only")
  kz <- attr(modmat, "kz")  
  modmat <- modmat[agg.subset, ,drop=FALSE]

  ## compute aggregated effect
  estimates <- est[paste0("g1",sep,kz)]
  vcov_est <- vcov[paste0("g1",sep,kz),paste0("g1",sep,kz)]
  
  aggeffects <- cbind(modmat %*% estimates)
  vcov_aggeffects <- modmat %*% vcov_est %*% t(modmat)
  res <- mean(aggeffects)
  
  mat <- rep(1/nrow(aggeffects), times=nrow(aggeffects))
  vcov_eff <- t(mat) %*% vcov_aggeffects %*% mat
  res <- c(res, sqrt(vcov_eff))
  
  ## more than two treatment groups
  if(ng > 2){
    for(i in 3:ng){
      estimates <- est[paste0("g",i-1,sep,kz)]
      vcov_est <- vcov[paste0("g",i-1,sep,kz),paste0("g",i-1,sep,kz)]
      
      aggeffects <- cbind(modmat %*% estimates)
      vcov_aggeffects <- modmat %*% vcov_est %*% t(modmat)
      res <- c(res, mean(aggeffects))
      
      mat <- rep(1/nrow(aggeffects), times=nrow(aggeffects))
      vcov_eff <- t(mat) %*% vcov_aggeffects %*% mat
      res <- c(res, sqrt(vcov_eff))
    
    }      
  }  
  
  names(res) <- paste0(rep(c("","se_"), times=ng-1),
                               "Agg_g",
                               rep(2:ng-1, each=2))
  
  ## add expected outcomes
  expoutc <- obj@results@condeffects[agg.subset , paste0("ExpOutc", 0:(ng-1))]
  res <- c(res, colMeans(expoutc, na.rm=TRUE))
  return(res)
}




#' Autoselect Subset for Aggregated Effects
#' 
#' Automatically selects a subset of the original dataset for computing specific
#' aggregated effects. The subset is selected such that it is as close as possible
#' to the user supplied newdata frame. The function uses exact matching for 
#' categorical covariates (and the treatment if specified) and matching based 
#' on the Mahalanobis distance for continuous covariates.
#' 
#' @param obj Object of class \code{effectlite}.
#' @param newdata A data.frame with a single row, containing the same continuous and 
#' categorical covariates (and potentially the treatment variable) as used when fitting the
#' EffectLiteR model in obj.
#' @param nsub Integer. How many data points should be used for matching
#' the continous covariates. Will be ignored if no values for continuous covariates
#' are specified.
#' @return Vector of integers indicating the rows to use for computing the aggregated
#' effects. Can directly be used in \code{\link[EffectLiteR]{computeAggregatedEffects}}
#' @examples
#' m1 <- effectLite(y="dv", z=c("z1"), k=c("k1"), x="x", 
#' control="control", data=example01, fixed.cell=TRUE, fixed.z=TRUE)
#' newdata <- data.frame(k1=NA, z1=1)
#' agg.subset <- autoSelectSubset(m1, newdata)
#' @export
autoSelectSubset <- function(obj, newdata, nsub=10){

  ## required things
  nz <- obj@input@nz
  nk <- obj@input@nk
  ng <- obj@input@ng
  k <- obj@input@vnames$k
  z <- obj@input@vnames$z
  x <- obj@input@vnames$x
  data <- obj@results@condeffects

  ## some input checks
  stopifnot(all(c(z,k) %in% names(newdata))) ## no latent covariates in newdata
  stopifnot(nrow(newdata) == 1)

  ## remove missing k and z in newdata
  k <- k[!is.na(newdata[k])]
  z <- z[!is.na(newdata[z])]
  
  ## create ID
  data$elr_id <- 1:nrow(data)
  
  ## delete rows with missings in g1
  data <- data[!is.na(data$g1),]
  
  ## exact matching on x
  if(x %in% names(newdata)){
    if(!is.na(newdata[,x])){
      
      xtmp <- data[,x]
      levels(xtmp) <- obj@input@vlevels$levels.x.original
      subset <- xtmp == as.character(newdata[,x])
      data <- data[subset,]    
    }
  }

  ## exact matching on k
  if(length(k) != 0){
    
    idx <- rep(TRUE, nrow(data))
    for (i in 1:length(k)) {
      ktmp <- data[,k[i]]
      levels(ktmp) <- unlist(obj@input@vlevels$levels.k.original[k[i]])
      idxtmp <- ktmp == as.character(newdata[1,k[i]])
      idx <- idx*idxtmp
    }
    
    subset <- which(idx==1)
    data <- data[subset,]    
  }

  ## matching on z (using Mahalanobis distance)
  if(length(z) != 0){
    
    zdata <- data[,z, drop=FALSE]
    cov.zdata <- cov(zdata, use="pairwise.complete.obs")
    znewdata <- unlist(newdata[,z, drop=FALSE])
    
    zdistance <- mahalanobis(zdata, znewdata, cov.zdata)
    
    subset <- order(zdistance)[1:nsub]
    data <- data[subset,]
    
  }

  return(data$elr_id)

}


