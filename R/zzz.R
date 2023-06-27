
## Some additional (private) helper functions


elr_compute_descriptives_z <- function(object){
  
  method <- object@input@method
  vnamesz <- object@input@vnames$z
  
  if(method=="lm"){
    
    d <- object@input@data
    d <- d[,vnamesz]
    
    means <- colMeans(d, na.rm=TRUE)
    sds <- apply(d,2,function(x){sd(x, na.rm=TRUE)})
    mins <- apply(d,2,function(x){min(x, na.rm=TRUE)})
    maxs <- apply(d,2,function(x){max(x, na.rm=TRUE)})
    
    res <- data.frame(Mean=means, SD=sds, Min=mins, Max=maxs)
    
  }else if(method=="sem"){
    
    lavres <- object@results@lavresults
    est <- parameterEstimates(lavres)$est
    names(est) <- parameterEstimates(lavres)$label 

    relfreq <- object@parnames@relfreq
    freq <- est[relfreq]
    
    ## manifest and latent variables
    cov.all <- lavInspect(lavres, what="cov.all")
    cov.all <- sapply(cov.all, function(x){diag(x)})
    
    mean.ov <- data.frame(lavInspect(lavres, what="mean.ov"))
    mean.lv <- data.frame(lavInspect(lavres, what="mean.lv"))
    mean.all <- as.matrix(rbind(mean.ov, mean.lv))
    
    means <- mean.all %*% freq
    sds <- sqrt(cov.all %*% freq + (sweep(mean.all,1,means))^2 %*% freq) ## law of total variance
    
    res <- data.frame(Mean=means, SD=sds)
    res <- res[vnamesz,]
  }
  
  return(res)
  
}
