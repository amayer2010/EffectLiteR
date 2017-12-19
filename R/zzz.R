
## Some additional (private) helper functions


elr_compute_descriptives_z <- function(object){
  
  method <- object@input@method
  vnamesz <- object@input@vnames$z
  
  if(method=="lm"){
    
    d <- object@input@data
    d <- d[,vnamesz]
    
    mean <- colMeans(d, na.rm=TRUE)
    sd <- apply(d,2,function(x){sd(x, na.rm=TRUE)})
    min <- apply(d,2,function(x){min(x)})
    max <- apply(d,2,function(x){max(x)})
    
    res <- data.frame(mean,sd,min,max)
    
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
    mean.all <- rbind(mean.ov, mean.lv)
    
    mean <- as.matrix(mean.all) %*% freq
    sd <- sqrt(cov.all %*% freq + (mean.all - mean)^2 %*% freq) ## law of total variance
    
    res <- data.frame(mean,sd)
    res <- res[vnamesz,]
  }
  
  return(res)
  
}
