

#' Average and conditional effects based on generalized linear models
#' 
#' This function can be used to estimate average and conditional effects of a 
#' treatment variable on an outcome variable, taking into account any number of 
#' continuous and categorical covariates. It takes a user defined generalized
#' linear model (or another statistical model with a suitable predict method) 
#' as input and computes the corresponding effects. 
#'
#' @param object User defined generalized linear model (or another statistical model with a suitable predict method)
#' @param x Treatment variable (character string)
#' @param from from and to (values of treatment variable) specify the considered change in the treatment variable for the effect computation
#' @param to from and to (values of treatment variable) specify the considered change in the treatment variable for the effect computation
#' @param type character. Indicates the type of effect considered. Can be one of \code{"ATE"} (with aliases \code{"difference"} and \code{"Average Treatment Effect"} and \code{"Average of Differences"}), \code{"SRA"} (with alias \code{"Simple Ratio of Averages"}), or \code{"ORA"} (with alias \code{"Odds Ratio of Averages"}), \code{"ASR"} (with aliases \code{"ratio"} and \code{"Average of Simple Ratios"}), \code{"AOR"} (with aliases \code{"oddsratio"} and \code{"Average of Odds Ratios"}).
#' @param subset. Logical vector for computing effects in a subset of the data (conditional effects).
#'
#' @return Object of class elreffects
#' @examples
#' ## Example with a logistic regression
#' m1logreg <- glm(y ~ x+z1+z2+k1+k2, data=elrdata_logreg,  family=binomial)
#' elrEffects(m1logreg, "x", from="0", to="1", type="difference", subset.=NULL)
#' @export
#'
elrEffects <- function(object, x, from=0, to=1, type="difference", subset.=NULL){

  if(type %in% c("ATE", "difference", "Average Treatment Effect", "Average of Differences")){
    type <- "difference"
    
  }else if(type %in% c("ASR", "ratio", "Average of Simple Ratios")){
    type <- "ratio"
    
  }else if(type %in% c("AOR", "oddsratio", "Average of Odds Ratios")){
    type <- "oddsratio"
    
  }else if(type %in% c("SRA", "Simple Ratio of Averages")){
    type <- "sra"
    
  }else if(type %in% c("ORA", "Odds Ratio of Averages")){
    type <- "ora"
    
  }
    
  newdata0 <- newdata1 <- model.frame(object)
  newdata0[,x] <- from
  newdata1[,x] <- to
  
  tau0 <- predict(object, newdata0, type="response")
  tau1 <- predict(object, newdata1, type="response")
  
  if(!is.null(subset.)){
    tau0 <- tau0[subset.]
    tau1 <- tau1[subset.]
  }
  
  # compute effect 
  coefs <- coef(object)
  ave <- computeEffects(x = coefs, object = object,
                        newdata0 = newdata0, newdata1 = newdata1,
                        subset. = subset., type = type)
  
  # compute JAC
  JAC <- numDeriv::jacobian(func = computeEffects, x = coefs, object = object,
                            newdata0 = newdata0, newdata1 = newdata1, 
                            subset. = subset., type = type)
  
  # Delta method
  ave_vcov <- JAC %*% vcov(object) %*% t(JAC)
  
  ## TODO: Stochastic covariates as in Basu and Rathouz (2005)
  # if (!fixed.z & type %in% c("difference", "ratio", "oddsratio")){
  #   N <- length(pred0)
  #   eff <- switch(type,
  #                 difference = pred1 - pred0,
  #                 ratio = pred1/pred0,
  #                 oddsratio = (pred1/(1-pred1))/(pred0/(1-pred0)),
  #                 sra = mean(pred1) / mean(pred0), #TODO
  #                 ora = (mean(pred1) / (1 - mean(pred1))) / (mean(pred0) / (1 - mean(pred0)))) #TODO
  #   ave_vcov <- ave_vcov + var(eff)/N
  # }
  
  # SE
  ave_se <- sqrt(diag(ave_vcov))
  
  # extra
  ExpOutc0 <- mean(tau0)
  ExpOutc1 <- mean(tau1)
  
  res <- list(ave=ave, ave_se=ave_se, zval=ave/ave_se,
              ExpOutc0=ExpOutc0, ExpOutc1=ExpOutc1,
              object=object, type=type)
  
  class(res) <- "elreffects"
  return(res)
  
}


computeEffects <- function(x, object=NULL, newdata0=NULL, newdata1=NULL, 
                           subset.=NULL, type="difference"){
  
  # compute individual effects
  object[["coefficients"]] <- x
  pred0 <- predict(object, newdata0, type="response")
  pred1 <- predict(object, newdata1, type="response")
  if(!is.null(subset.)) {
    pred0 <- pred0[subset.]
    pred1 <- pred1[subset.]
  }
  
  ave <- switch(type,
                difference = mean(pred1 - pred0),
                ratio = mean(pred1/pred0),
                oddsratio = mean((pred1/(1-pred1))/(pred0/(1-pred0))),
                sra = mean(pred1) / mean(pred0),
                ora = (mean(pred1) / (1 - mean(pred1))) / (mean(pred0) / (1 - mean(pred0))))
  
  return(ave)
  
}


#' @export
print.elreffects <- function(x, ...){
  res <- unlist(x[1:5])
  res <- round(res, 3)

  names(res) <- c("Estimate", "SE", "Est./SE", "ExpOutc0", "ExpOutc1")
  print(res)
  
}

