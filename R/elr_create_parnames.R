
createParNames <- function(obj){
  
  inp <- obj@input
  ng <- inp@ng ## number of treatment groups
  nz <- inp@nz ## number of z
  nk <- inp@nk ## number of unfolded categories of K
  interactions <- inp@interactions
  
  sep <- ""
  ## longer parameter names for many groups and/or covariates
  if(ng>9 | nk>9 | nz>9){sep <- "_"}
  
  # create list for alpha, beta and gamma coefficients
  tmp <- expand.grid(z=0:nz, k=0:(nk-1), x=0:(ng-1))
  alphas <- with(tmp, array(paste0("a",x,sep,k,sep,z), dim=c(nz+1,nk,ng)))
  betas <- with(tmp, array(paste0("b",x,sep,k,sep,z), dim=c(nz+1,nk,ng)))
  gammas <- with(tmp, array(paste0("g",x,sep,k,sep,z), dim=c(nz+1,nk,ng)))
  
  ## constrained gammas (if interactions != "all")
  stopifnot(interactions %in% c("all","none","2-way","X:K","X:Z","X:K,X:Z","no"))
  constrainedgammas <- character()
  
  if(interactions == "none"){
    constrainedgammas <- c(matrix(c(gammas), ncol=ng)[-1,-1])
    
  }
  if(interactions == "no"){
    unconstrainedgammas <- c(gammas[1, 1, ]) ## main effects X + g000
    if(nk>1){unconstrainedgammas <- c(unconstrainedgammas, c(gammas[1, , 1])[-1])} ## main effects K
    if(nz>0){unconstrainedgammas <- c(unconstrainedgammas, c(gammas[, 1, 1])[-1])} ## main effects Z
    
    idx <- which(c(gammas) %in% unconstrainedgammas)
    constrainedgammas <- c(gammas)[-idx]
    
  }
  if(interactions == "2-way"){
    if(nk>1 & nz>0){
      constrainedgammas <- c(gammas[2:(nz+1), 2:nk, 2:ng])
    }
  }
  if(interactions == "X:K"){
    if(nz>0){
      constrainedgammas <- c(gammas[2:(nz+1), , 2:ng])
    }
  }
  if(interactions == "X:Z"){
    if(nk>1){
      constrainedgammas <- c(gammas[, 2:nk, 2:ng])
    }
  }
  if(interactions == "X:K,X:Z"){
    if(nk>1){
      constrainedgammas <- c(gammas[2:(nz+1), 2:nk, ])
    }
  }
  
  ## add unconstrainedgammas
  if(length(constrainedgammas) != 0){
    idx <- which(gammas %in% constrainedgammas) 
    unconstrainedgammas <- c(gammas[-idx])
    
  }else if(length(constrainedgammas) == 0){
    unconstrainedgammas <- c(gammas)
  }
  
  ## for pretty printing
  gammalabels <- with(tmp, paste0("I_X=",x, " * I_K=",k, " * Z",z))
  
  ## delete entries with zeros in it
  gammalabels <- gsub("I_X=0 * ", "", gammalabels, fixed=TRUE)
  gammalabels <- gsub("I_K=0 * ", "", gammalabels, fixed=TRUE) 
  gammalabels <- gsub(" * Z0", "", gammalabels, fixed=TRUE)
  gammalabels[1] <- "Intercept"
  gammalabels <- array(gammalabels, dim=c(nz+1,nk,ng))
  
  label.g.function <- "(K,Z)" 
  label.covs <- ",K,Z"
  if(nk==1 & nz==0){label.g.function <- "()"; label.covs <- ""}
  if(nk>1 & nz==0){label.g.function <- "(K)"; label.covs <- ",K"}
  if(nk==1 & nz>0){label.g.function <- "(Z)"; label.covs <- ",Z"}
  
  label.Egx <- paste0("E[g",1:(ng-1),label.g.function,"]")
  
  pk <- paste0("Pk",0:(nk-1))
  px <- paste0("Px",0:(ng-1))
  if(nz>0){
    tmp <- expand.grid(z=1:nz, k=0:(nk-1), x=0:(ng-1))
    cellmeanz <- with(tmp, paste0("mz",x,sep,k,sep,z))
    meanz <- paste0("Ez",1:nz)  
    tmp <- expand.grid(k=0:(nk-1), z=1:nz)
    Ezk <- with(tmp, paste0("Ez",z,"k",k))    
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
  
  ## Adjusted means given k for E(gx|K=k)
  tmp <- expand.grid(x=0:(ng-1), k=0:(nk-1))
  adjmeansgk <- paste0("adjmean",tmp$x,"gk",tmp$k)
  
  ## E(gx|X=x,K=k)
  tmp <- expand.grid(g=1:(ng-1), x=0:(ng-1), k=0:(nk-1))
  Egxgxk <- paste0("Eg",tmp$g,"gx",tmp$x,"k",tmp$k)
  
  ## average effect continuous covariate Z
  AveEffZ <- character()
  if(nz>0){AveEffZ <- paste0("AveEffZ",1:nz)}
  
  res <- new("parnames",
             alphas=alphas,
             betas=betas,
             gammas=gammas,
             constrainedgammas=constrainedgammas,
             unconstrainedgammas=unconstrainedgammas,
             gammalabels=gammalabels,
             label.g.function=label.g.function,
             label.covs=label.covs,
             label.Egx=label.Egx,
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
             Egxgxk=Egxgxk,
             adjmeans=adjmeans,
             adjmeansgk=adjmeansgk,
             AveEffZ=AveEffZ
  )
  
  return(res)  
}


