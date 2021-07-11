

###### functions ########

require(lavaan)

elrEffects <- function(object, x, from=0, to=1, type="difference", subset=NULL){
  
  newdata0 <- newdata1 <- model.frame(object)
  newdata0[,x] <- from
  newdata1[,x] <- to
  
  tau0 <- predict(object, newdata0, type="response")
  tau1 <- predict(object, newdata1, type="response")
  
  coefs <- coef(object)
  eff <- computeEffects(coefs, object, newdata0, newdata1, type=type) ## effects
  JAC <- lav_func_jacobian_complex(computeEffects, coefs, object=object,
                                   newdata0=newdata0, newdata1=newdata1,
                                   type=type)
  vcov_eff <- JAC %*% vcov(object) %*% t(JAC) ## vcov of effects
  
  if(!is.null(subset)){
    tau0 <- tau0[subset]
    tau1 <- tau1[subset]
    eff <- eff[subset]
    vcov_eff <- vcov_eff[subset,subset]
  }
  
  N <- length(eff)
  ave <- sum(eff)/N ## average (marginal) effect
  mat <- matrix(1/N, nrow=1, ncol=N)
  ave_se <- sqrt(mat %*% vcov_eff %*% t(mat))
  
  ExpOutc0 <- mean(tau0)
  ExpOutc1 <- mean(tau1)
  
  if(type=="difference"){
    eff_expoutc <- ExpOutc1 - ExpOutc0
    
  }else if(type=="ratio"){
    eff_expoutc <- ExpOutc1/ExpOutc0
    
  }else if(type=="oddsratio"){
    eff_expoutc <- (ExpOutc1/(1-ExpOutc1))/(ExpOutc0/(1-ExpOutc0))
    
  }
  
  
  res <- list(ave=ave, ave_se=ave_se, zval=ave/ave_se,
              ExpOutc0=ExpOutc0, ExpOutc1=ExpOutc1, eff_expoutc=eff_expoutc,
              object=object, eff=eff, vcov_eff=vcov_eff,
              type=type)
  class(res) <- "elreffects"
  return(res)
  
}


computeEffects <- function(coefs, object, newdata0, newdata1, type){
  
  object[["coefficients"]] <- coefs
  
  pred0 <- predict(object, newdata0, type="response")
  pred1 <- predict(object, newdata1, type="response")
  
  eff <- NULL
  
  if(type=="difference"){
    eff <- pred1 - pred0
    
  }else if(type=="ratio"){
    eff <- pred1/pred0
    
  }else if(type=="oddsratio"){
    eff <- (pred1/(1-pred1))/(pred0/(1-pred0))
    
  }
  
  return(eff)
  
}


print.elreffects <- function(x){
  res <- unlist(x[1:6])
  res <- round(res, 3)
  
  if(x$type=="difference"){eff_expoutc_label <- "Diff_ExpOutc"}
  if(x$type=="ratio"){eff_expoutc_label <- "Ratio_ExpOutc"}
  if(x$type=="oddsratio"){eff_expoutc_label <- "OR_ExpOutc"}
  
  names(res) <- c("Estimate", "SE", "Est./SE", "ExpOutc0", "ExpOutc1",eff_expoutc_label)
  print(res)
  
}

