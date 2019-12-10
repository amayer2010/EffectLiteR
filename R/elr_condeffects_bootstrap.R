
## bootstrap standard errors for aggregated effects
## not yet ready to use!

elrCondeffectsBoot <- function(data, m1, formula, nboot=1000, est="lm", se="boot"){
  
  if(is.na(nboot)){
    
    d <- cbind(m1@results@condeffects, data)
    Estimates <- coef(lm(formula, data=d))
    res <- data.frame(Estimates, SE=as.numeric(NA))
    
  }else{
    
    bootsamples <- elr_condeff_boot_samples(data, m1, formula, nboot)
    
    if(est=="lm"){
      d <- cbind(m1@results@condeffects, data)
      Estimates <- coef(lm(formula, data=d))
      
    }else if(est=="boot"){
      Estimates <- colMeans(bootsamples, na.rm=TRUE)
    }
    SE <- apply(bootsamples, 2, function(x){sd(x, na.rm=TRUE)})
    res <- data.frame(Estimates,SE)
  }
  
  
  return(res)

}

## generate the bootstrap samples and return all results
elr_condeff_boot_samples <- function(data, m1, formula, nboot){
  
  N <- nrow(data)
  call <- m1@call
  call$data <- quote(dboot)
  res <- vector("list", length=nboot)
  
  for(i in 1:nboot){
    
    idx <- sample(1:N, size=N, replace=TRUE)
    dboot <- data[idx,]
    
    mboot <- try(eval(call))
    
    if(inherits(mboot, "try-error")){
      res[[i]] <- NA
      
    }else{
      condeffects <- mboot@results@condeffects
      dboot <- cbind(condeffects, dboot)
      
      m1.lm <- lm(formula, data=dboot)
      res[[i]] <- coef(m1.lm)
    }
    
  }
  
  res <- as.data.frame(do.call(rbind, res))
  
  return(res)
  
}



# test <- elrCondeffectsBoot(data=example01,m1=m1,formula=g1~z1,nboot=2)

