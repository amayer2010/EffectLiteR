

## model matrix function


computeModelMatrix <- function(obj){
  
  stopifnot(length(obj@input@measurement)==0) ## no latent variables
  
  ## required things
  z <- obj@input@vnames$z
  k <- obj@input@vnames$k
  x <- obj@input@vnames$x
  
  nz <- obj@input@nz
  nk <- obj@input@nk
  ng <- obj@input@ng
  
  gammas <- obj@parnames@gammas
  constrainedgammas <- obj@parnames@constrainedgammas
  interactions <- obj@input@interactions
  
  data <- obj@input@data  

  ## X, K, Z parts
  X <- model.matrix(as.formula(paste0("~ ",x)), data=data)
  K <- model.matrix(as.formula("~ kstar"), data=data)
  tmp <- paste0(z, collapse="+")
  tmp <- paste0("~", tmp)
  Z <- model.matrix(as.formula(tmp), data=data)
  
  ## compute full model.matrix based on X,K,Z  
  modmat <- numeric()
  
  for(h in 1:ng){
    for(i in 1:nk){
      for(j in 1:(nz+1)){
        modmat <- cbind(modmat, X[,h]*K[,i]*Z[,j])
      }
    }
  }
  
  colnames(modmat) <- c(gammas)
  
  ## delete constraint parameters (if interactions != "all")
  if(interactions != "all"){
    
    idx <- which(colnames(modmat) %in% constrainedgammas)
    modmat <- modmat[,-idx]
  }
  
  return(modmat)
  
  
}




# summary(lm(data$dv ~ -1 + modmat))


