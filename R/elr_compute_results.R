

computeResults <- function(obj){
  
  if(obj@input@method == "sem"){
    
    m1_sem <- computeLavaanResults(obj)
    m1_lm <- list(); class(m1_lm) <- "lm"
    est <- parameterEstimates(m1_sem, fmi=FALSE)$est ## parameter estimates
    se <- parameterEstimates(m1_sem, fmi=FALSE)$se ## standard errors
    vcov.def <- lavInspect(m1_sem, "vcov.def", add.class = FALSE)
    names(se) <- names(est) <- parameterEstimates(m1_sem, fmi=FALSE)$label 
    tval <- est/se
    pval <- 2*(1-pnorm(abs(tval)))
    
  }else if(obj@input@method == "lm"){
    
    m1_sem <- new("lavaan")
    m1_lm <- computeLMResults(obj)
    allcoefs <- computeAdditionalLMCoefficients(obj, m1_lm)
    est <- allcoefs$est
    se <- allcoefs$se
    vcov.def <- allcoefs$vcov.def ## vcov of defined parameters
    tval <- est/se
    pval <- 2*(1-pnorm(abs(tval)))
    
  }

  
  sdyx0 <- computeStdDevEffectSize(obj, est, m1_sem)
  hypotheses <- computeHypothesesResults(obj, m1_sem)
  
  ng <- obj@input@ng
  nz <- obj@input@nz
  nk <- obj@input@nk  
    
  Egx <- data.frame(est[obj@parnames@Egx],
                    se[obj@parnames@Egx],
                    tval[obj@parnames@Egx],
                    pval[obj@parnames@Egx],
                    est[obj@parnames@Egx]/sdyx0)
  
  names(Egx) <- c("Estimate", "SE", "Est./SE", "p-value", "Effect Size")
  
  ## Additional (user-defined) effects
  AdditionalEffects <- data.frame()
  if(length(obj@input@add > 0)){
    if(grepl(":=", obj@input@add)){
      
      ## TODO: improve code
      pt_tmp <- parTable(m1_sem) ## TODO: what about lm()
      pt_tmp <- subset(pt_tmp, subset=op==":=")
      npar <- nrow(pt_tmp)
      nnewpar <- length(unlist(gregexpr(":=", obj@input@add)))
      newnames <- pt_tmp$label[(npar-nnewpar+1):npar]
      
      AdditionalEffects <- data.frame(est[newnames],
                                      se[newnames],
                                      tval[newnames],
                                      pval[newnames],
                                      est[newnames]/sdyx0)
      
      names(AdditionalEffects) <- c("Estimate", "SE", "Est./SE", "p-value", 
                                    "Effect Size")
      
    }
  }
  
  
  ## Effects given a treatment condition
  Egxgx <- data.frame(est[obj@parnames@Egxgx],
                      se[obj@parnames@Egxgx],
                      tval[obj@parnames@Egxgx],
                      pval[obj@parnames@Egxgx],
                      est[obj@parnames@Egxgx]/sdyx0)
  names(Egxgx) <- c("Estimate", "SE", "Est./SE", "p-value", "Effect Size")
  
  
  ## Effects given a a value K=k
  Egxgk <- data.frame()
  if(nk>1){
    Egxgk <- data.frame(est[obj@parnames@Egxgk],
                        se[obj@parnames@Egxgk],
                        tval[obj@parnames@Egxgk],
                        pval[obj@parnames@Egxgk],
                        est[obj@parnames@Egxgk]/sdyx0)
    names(Egxgk) <- c("Estimate", "SE", "Est./SE", "p-value", "Effect Size")    
  }
  
  ## Effects given X=x and K=k
  Egxgxk <- data.frame()
  if(nk>1 & nz>0){
    Egxgxk <- data.frame(est[obj@parnames@Egxgxk],
                         se[obj@parnames@Egxgxk],
                         tval[obj@parnames@Egxgxk],
                         pval[obj@parnames@Egxgxk],
                         est[obj@parnames@Egxgxk]/sdyx0)
    names(Egxgxk) <- c("Estimate", "SE", "Est./SE", "p-value", "Effect Size")    
  }
  
  
  ## g functions
  gammas <- matrix(obj@parnames@gammas, ncol=ng)
  gammalabels <- matrix(obj@parnames@gammalabels, ncol=ng)
  gx <- vector("list",ng)
  
  for(i in 1:ng){
    tmp <- data.frame(gammas[,i],
                      est[gammas[,i]],
                      se[gammas[,i]],
                      tval[gammas[,i]],
                      pval[gammas[,i]])
    names(tmp) <- c("Coefficient", "Estimate", "SE", "Est./SE", "p-value")
    rownames(tmp) <- gammalabels[,i]
    gx[[i]] <- tmp 
  }
  
  ## adjusted means
  adjmeans <- data.frame(est[obj@parnames@adjmeans],
                         se[obj@parnames@adjmeans],
                         tval[obj@parnames@adjmeans])
  names(adjmeans) <- c("Estimate", "SE", "Est./SE")
  
  res <- new("results",
             lavresults=m1_sem,
             lmresults=m1_lm,
             est=est,
             se=se,
             vcov.def=vcov.def,
             hypotheses=hypotheses,
             Egx=Egx,
             AdditionalEffects=AdditionalEffects,
             Egxgx=Egxgx,
             Egxgk=Egxgk,
             Egxgxk=Egxgxk,
             gx=gx,
             adjmeans=adjmeans,
             condeffects=data.frame() ## we compute conditional effects later using computeConditionalEffects()
  )
  
  
  return(res)
  
}



computeLavaanResults <- function(obj){

  sem.args <- list(model=obj@syntax@model,
                   group="cell", 
                   missing=obj@input@missing,
                   se=obj@input@se,
                   fixed.x=obj@input@fixed.z,
                   group.label=obj@input@vlevels$cell, 
                   data=obj@input@data, 
                   group.w.free = !obj@input@fixed.cell)
  sem.args <- c(sem.args, obj@input@method_args)
  m1 <- do.call("sem", sem.args)
  
  ## lavaan.survey -- complex survey designs
  ids <- obj@input@complexsurvey$ids
  weights <- obj@input@complexsurvey$weights
  
  if((ids != ~0) | (!is.null(weights))){
    
    if(!obj@input@fixed.cell){## currently only works for fixed cell sizes
      stop("EffectLiteR error: The complex survey functionality currently only works for fixed cell sizes. Please use fixed.cell=TRUE.")
    } 
    survey.design <- survey::svydesign(ids=ids, weights=weights, 
                                       data=obj@input@data)
    m1 <- lavaan.survey::lavaan.survey(lavaan.fit=m1, 
                                       survey.design=survey.design)    
  }
  
  return(m1)
  
}


computeLMResults <- function(obj){
  
  data <- obj@input@data
  y <- obj@input@vnames$y
  modmat <- computeModelMatrix(obj)
  
  formu <- as.formula(paste0("data$",y," ~ -1 + modmat"))
  m1 <- lm(formu)
  
  return(m1)
}


computeHypothesesResults <- function(obj, m1_sem){
  
  if(obj@input@method =="lm"){return(data.frame())} ## TODO change this
  
  ng <- obj@input@ng
  nz <- obj@input@nz
  nk <- obj@input@nk  
  
  # any(partable(m1)$op %in% c("==",">","<"))
  ## main hypotheses
  if(obj@input@se != "standard" | obj@input@interactions != "all" |
     any(grepl("==", obj@input@add)) | any(grepl(">", obj@input@add)) |
     any(grepl("<", obj@input@add))){ 
    ## no Wald Test for robust, bootstrapped SE...
    ## no Wald Test for models with equality constraints (ask Yves to adjust...)
    ## maybe we could come up with something similar
    hypotheses <- data.frame()
  }else{
    if(nz==0 & nk==1){
      hypotheses <- try(data.frame(
        lavTestWald(m1_sem, constraints=obj@syntax@hypotheses$hypothesis1)[1:3]))    
      if(class(hypotheses) == "try-error"){
        return(data.frame())
      }
      row.names(hypotheses) <- "No average effects"    
      
    }else{
      hypotheses <- try(data.frame(rbind(
        lavTestWald(m1_sem, constraints=obj@syntax@hypotheses$hypothesis1)[1:3],
        lavTestWald(m1_sem, constraints=obj@syntax@hypotheses$hypothesis2)[1:3],
        lavTestWald(m1_sem, constraints=obj@syntax@hypotheses$hypothesis3)[1:3],
        lavTestWald(m1_sem, constraints=obj@syntax@hypotheses$hypothesis4)[1:3]    
      )))
      if(class(hypotheses) == "try-error"){
        return(data.frame())
      }
      row.names(hypotheses) <- c("No average effects",
                                 "No covariate effects in control group",
                                 "No treatment*covariate interaction",
                                 "No treatment effects")    
    }    
  }
  
  return(hypotheses)
  
}



computeStdDevEffectSize <- function(obj, est, m1_sem){
  
  
  if(obj@input@method =="lm"){ ## TODO: Change this
    
    y <- obj@input@data[, obj@input@vnames$y]
    x <- obj@input@data[, obj@input@vnames$x]
    return(sd(y[x==0]))
    
  }
  
  ## standard deviation in control group
  sdyx0 <- NA
  
  nk <- obj@input@nk
  Pkgx <- est[obj@parnames@Pkgx][1:nk]
  vary <- meany <- numeric(nk)  
  
  ## manifest outcome variable  
  if(obj@input@vnames$y %in% names(obj@input@data)){
    
    fv <- fitted.values(m1_sem)[1:nk] ## means and variances given X=0, K=k
    
    for(i in 1:nk){
      tmp <- fv[[i]]
      vary[i] <- tmp$cov[[obj@input@vnames$y,obj@input@vnames$y]]
      meany[i] <- tmp$mean[obj@input@vnames$y]
    }
    
  }else{ ## latent outcome variable
    
    fv.cov <- inspect(m1_sem, what="cov.lv")[1:nk]
    fv.mean <- inspect(m1_sem, what="mean.lv")[1:nk]    
    
    for(i in 1:nk){
      tmp.cov <- fv.cov[[i]]
      tmp.mean <- fv.mean[[i]]
      
      vary[i] <- tmp.cov[[obj@input@vnames$y,obj@input@vnames$y]]
      meany[i] <- tmp.mean[[obj@input@vnames$y]]
    }
    
  }
  
  meanyx0 <- sum(meany*Pkgx) ##TODO: compute parameter in model constraint
  sdyx0 <- sqrt(sum(vary*Pkgx) + sum(Pkgx*(meany-meanyx0)^2)) ## law of total variance  
  
  
}


## additional parameters based on lm() results

computeAdditionalLMCoefficients <- function(obj, m1_lm){
  
  coefs <- coef(m1_lm) 
  vcovs <- vcov(m1_lm)
  
  pnames <- obj@parnames@gammas
  names(coefs) <- row.names(vcovs) <- colnames(vcovs) <- pnames
  con_full <- obj@syntax@model
  
  ## compute effects
  pt <- lavaanify(con_full)
  def.function <- lavaan:::lav_partable_constraints_def(pt)
  JAC <- lav_func_jacobian_complex(func=def.function, x=coefs)
  info.r <- JAC %*% vcovs %*% t(JAC)
  se <- sqrt(diag(info.r))
  est <- def.function(coefs)
  
  names(se) <- names(est)
  
  est <- c(coefs, est)
  se <- c(sqrt(diag(vcovs)), se)
  vcov.def <- info.r
  
  return(list(est=est, se=se, vcov.def=vcov.def))
  
}


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
  
  if(nk==1){
    K <- model.matrix(as.formula("~ 1"), data=data)
    
  }else if(nk>1){
    K <- model.matrix(as.formula("~ kstar"), data=data)
  }
  
  if(nz==0){
    Z <- model.matrix(as.formula("~ 1"), data=data)
    
  }else if(nz>0){
    tmp <- paste0(z, collapse="+")
    tmp <- paste0("~", tmp)
    Z <- model.matrix(as.formula(tmp), data=data)
  }
  
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


