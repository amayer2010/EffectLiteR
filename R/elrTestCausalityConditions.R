

## function to test conditional unit-treatment-homogeneity
## or other causality conditions
## test effect-homogeneity (if we are only interested in interindividual differences and not in 
## expected outcomes, we can use this condition)

elrTestCausalityConditions <- function(object, zsub, ksub=NULL, condition="unit-treatment-homogeneity"){

  if(!is.null(ksub)){
    stop("Categorical covariates are not yet supported")
  }
  
  gammas <- object@parnames@gammas
  vnamesz <- object@input@vnames$z

  stopifnot(all(zsub %in% vnamesz))
  idx_zsub <- which(vnamesz %in% zsub)
  idx_zsub <- idx_zsub + 1
  testgammas <- gammas[idx_zsub,,]
  
  if(condition=="effect-homogeneity"){
    testgammas <- testgammas[,,-1]
  }
  
  con <- paste0(testgammas, "==0")
  lavres <- object@results@lavresults
  
  res <- data.frame(lavTestWald(lavres, con))
  return(res)
}

## seems very sensitive...
# elrTestCausalityConditions(m1, zsub="z2")
# elrTestCausalityConditions(m1, zsub=c("z1","z2"))
# elrTestCausalityConditions(m1, zsub=c("z1","z3"))
# elrTestCausalityConditions(m1, zsub=c("z1","z2","k1"))
# elrTestCausalityConditions(m1, zsub=c("z1","z2","z3","k1"))
# elrTestCausalityConditions(m1, zsub=c("z1","z2","z4","k1"))


