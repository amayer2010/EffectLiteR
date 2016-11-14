

## some experimental first steps for the lm version
## will be merged with effectLite() in the future

effectLiteLM <- function(y, x, k=NULL, z=NULL, control="0", 
                         measurement=character(), data, fixed.cell=FALSE, 
                         fixed.z=FALSE, missing="listwise", se="standard", 
                         syntax.only=FALSE, 
                         interactions="all", propscore=NULL, ids=~0, 
                         weights=NULL, homoscedasticity=FALSE, 
                         add=character(),...){
  
  obj <- new("effectlite")
  obj@call <- match.call()
  obj@input <- createInput(y,x,k,z,propscore,control,measurement,data, 
                           fixed.cell, fixed.z, missing, se,
                           interactions, ids, weights, homoscedasticity,
                           add)
  obj@input <- computePropensityScore(obj@input)
  obj@parnames <- createParNames(obj)  
  obj@lavaansyntax <- createLMSyntax(obj)
  
  if(syntax.only){return(obj@lavaansyntax@model)}    ## elrInspect(obj, "syntax")
  obj@results <- computeResults(obj)

  return(obj)  
}





createLMSyntax <- function(obj) {
  
  inp <- obj@input
  parnames <- obj@parnames
  
  ## input information
  y <- inp@vnames$y
  z <- inp@vnames$z  
  ng <- inp@ng
  nz <- inp@nz
  nk <- inp@nk
  fixed.cell <- inp@fixed.cell
  fixed.z <- inp@fixed.z
  sampmeanz <- inp@sampmeanz
  observed.freq <- inp@observed.freq
  interactions <- inp@interactions
  
  ## parnames
  gammas <- parnames@gammas
  constrainedgammas <- parnames@constrainedgammas
  cellmeanz <- parnames@cellmeanz
  relfreq <- parnames@relfreq
  groupw <- parnames@groupw
  meanz <- parnames@meanz
  pk <- parnames@pk
  px <- parnames@px
  Ezk <- parnames@Ezk
  Egx <- parnames@Egx
  adjmeans <- parnames@adjmeans
  Pkgx <- parnames@Pkgx
  Pxgk <- parnames@Pxgk
  Ezgx <- parnames@Ezgx
  Ezgk <- parnames@Ezgk
  Ezkgx <- parnames@Ezkgx
  Egxgx <- parnames@Egxgx
  Egxgk <- parnames@Egxgk
  Egxgxk <- parnames@Egxgxk
  
  
  model <- "#### lm Syntax for EffectLiteR Model ####"
  
  ## regression model
  model <- paste0(model, "\n\n## Regression Model \n")
  tmp <- paste0("mm",1:length(gammas))
  tmp <- paste0(gammas, "*", tmp, collapse=" + ")
  model <- paste0(model, paste0(y, " ~ ", tmp))

  ## mean z in each cell
  model <- paste0(model, create_syntax_cellmeanz(z, nz, fixed.z, cellmeanz, 
                                                 sampmeanz))
  
  ## compute relative group frequencies
  model <- paste0(model, create_syntax_group_freq(fixed.cell, relfreq, 
                                                  observed.freq, groupw))
  
  ## compute unconditional means of z
  model <- paste0(model, create_syntax_ez(nz, meanz, cellmeanz, relfreq))
  
  ## compute unconditional probabilities of K*=k
  model <- paste0(model, create_syntax_pk(nk, pk, relfreq))
  
  ## compute unconditional probabilities of X=x
  model <- paste0(model, create_syntax_ex(px, ng, nk, relfreq))
  
  ## compute unconditional means of Z*K 
  model <- paste0(model, create_syntax_Ezk(ng, nk, nz, Ezk, cellmeanz, relfreq))
  
  ## compute average effects
  model <- paste0(model, create_syntax_Egx(ng,nk,nz,pk,meanz,Ezk,Egx,gammas))  
  
  ## compute adjusted means
  model <- paste0(model, create_syntax_adjmeans(ng,nk,nz,pk,meanz,Ezk,adjmeans,gammas))
  
  ## conditional probabilities of K=k given X=x (Pkgx)
  model <- paste0(model, create_syntax_Pkgx(ng, nk, relfreq, Pkgx, px))
  
  ## conditional probabilities of X=x given K=k (Pxgk)
  model <- paste0(model, create_syntax_Pxgk(ng, nk, relfreq, Pxgk, pk))
  
  ## conditional expectations of Z given X=x (Ezgx)
  model <- paste0(model, create_syntax_Ezgx(ng, nk, nz, Ezgx, Pkgx, cellmeanz))
  
  ## conditional expectations of Z given K=k (Ezgk)
  model <- paste0(model, create_syntax_Ezgk(ng,nk,nz,cellmeanz,Ezgk,Pxgk))
  
  ## conditional expectations of Z*K given X=x (Ezkgx)
  model <- paste0(model, create_syntax_Ezkgx(ng,nk,nz,Ezkgx,cellmeanz,Pkgx))
  
  ## effects given a treatment condition
  model <- paste0(model, create_syntax_Egxgx(ng,nk,nz,Pkgx,Ezgx,Ezkgx,Egxgx,gammas))
  
  ## Effects given a value k of K  
  model <- paste0(model, create_syntax_Egxgk(ng,nk,Egxgk,Ezgk,gammas))
  
  ## Effects given X=x and K=k
  model <- paste0(model, create_syntax_Egxgxk(ng,nk,nz,Egxgxk,gammas,cellmeanz))
  
  ## additional syntax
  if(length(inp@add) != 0){
    model <- paste0(model, "\n\n## Additional User Defined Syntax \n")
    model <- paste0(model, inp@add)
  }
  
  ## syntax for main hypotheses
  hypotheses <- create_syntax_hypotheses(ng, Egx, gammas)
  
  
  res <- new("lavsyntax",             
             model=model,
             hypotheses=hypotheses
  )
  
  return(res)
  
}

