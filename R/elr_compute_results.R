

computeResults <- function(obj){
  
  if(obj@input@method == "sem"){
    
    m1_sem <- computeLavaanResults(obj)
    m1_lm <- list(); class(m1_lm) <- "lm"
    est <- parameterEstimates(m1_sem, fmi=FALSE)$est ## parameter estimates
    se <- parameterEstimates(m1_sem, fmi=FALSE)$se ## standard errors
    vcov.def <- lavInspect(m1_sem, "vcov.def", add.class = FALSE)
    names(se) <- names(est) <- parameterEstimates(m1_sem, fmi=FALSE)$label 
    if(length(obj@parnames@constrainedgammas) > 0){
      se[obj@parnames@constrainedgammas] <- NA
    }
    tval <- est/se
    pval <- 2*(1-pnorm(abs(tval))) ## TODO Use pt() for stat="Ftest"? (would be inconsistent with se...)
    
    ## residual degrees of freedom for F Test
    N <- lavInspect(m1_sem, "ntotal")
    p <- length(obj@parnames@gammas) - length(obj@parnames@constrainedgammas)
    resid.df <- N-p
    stat <- obj@input@test.stat
    
    hypotheses <- elrMainHypothesisTests(obj, est, vcov.def, resid.df, stat)
    hypothesesk <- elrKConditionalHypothesisTests(obj, est, vcov.def, resid.df, stat)

  }else if(obj@input@method == "lm"){
    
    m1_sem <- new("lavaan")
    m1_lm <- computeLMResults(obj)
    allcoefs <- computeAdditionalLMCoefficients(obj, m1_lm)
    est <- allcoefs$est
    se <- allcoefs$se
    if(length(obj@parnames@constrainedgammas) > 0){
      se[obj@parnames@constrainedgammas] <- NA
    }
    vcov.def <- allcoefs$vcov.def ## vcov of defined parameters
    tval <- est/se
    rdf <- m1_lm$df.residual
    pval <- 2*(1-pt(abs(tval),df=rdf)) 

    resid.df <- m1_lm$df.residual
    stat <- obj@input@test.stat
    hypotheses <- elrMainHypothesisTests(obj, est, vcov.def, resid.df, stat)
    hypothesesk <- elrKConditionalHypothesisTests(obj, est, vcov.def, resid.df, stat)
    
  }

  
  sdyx0 <- computeStdDevEffectSize(obj, est, m1_sem)
  
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
      
      pt_tmp <- lavaanify(model="a~1", constraints=obj@input@add) ## needs fake model
      pt_tmp <- pt_tmp[pt_tmp$op==":=",]
      newnames <- pt_tmp$lhs

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
  
  ## average effect of continuous covariates
  AveEffZ <- data.frame()
  if(obj@input@method == "sem"){
    if(nz>0){
      AveEffZ <- data.frame(est[obj@parnames@AveEffZ],
                            se[obj@parnames@AveEffZ],
                            tval[obj@parnames@AveEffZ],
                            pval[obj@parnames@AveEffZ])
      names(AveEffZ) <- c("Estimate", "SE", "Est./SE", "p-value")
    }
  }
  
  res <- new("results",
             lavresults=m1_sem,
             lmresults=m1_lm,
             est=est,
             se=se,
             vcov.def=vcov.def,
             hypotheses=hypotheses,
             hypothesesk=hypothesesk,
             Egx=Egx,
             AdditionalEffects=AdditionalEffects,
             Egxgx=Egxgx,
             Egxgk=Egxgk,
             Egxgxk=Egxgxk,
             gx=gx,
             adjmeans=adjmeans,
             AveEffZ=AveEffZ,
             condeffects=data.frame() ## we compute conditional effects later using computeConditionalEffects()
  )
  
  
  return(res)
  
}



computeLavaanResults <- function(obj){

  ## lavaan.survey -- complex survey designs
  ids <- obj@input@complexsurvey$ids
  weights <- obj@input@complexsurvey$weights
  
  if((ids == ~0) & is.null(weights)){
    
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
    
  }else{ # at least one lavaan.survey argument specified
    
    if(requireNamespace("lavaan.survey", quietly = TRUE)){ ## check if lavaan.survey is available
      
      if(!obj@input@fixed.cell){## currently only works for fixed cell sizes
        stop("EffectLiteR error: The complex survey functionality currently only works for fixed cell sizes. Please use fixed.cell=TRUE.")
      }
      
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
      
      survey.design <- survey::svydesign(ids=ids, weights=weights, 
                                         data=obj@input@data)
      m1 <- lavaan.survey::lavaan.survey(lavaan.fit=m1, 
                                         survey.design=survey.design)
      
    }else{ ## lavaan.survey not installed
      
      warning("EffectLiteR warning: Since lavaan.survey is not installed, the lavaan:sem arguments cluster and sampling.weights are used. Only the first specified cluster variable and/or the first  specified sampling weight are used. Consider specifying these arguments directly in the call to effectLite() instead. They will be passed on to lavaan:sem.")
      
      ids <- all.vars(ids)[1]
      if(is.na(ids)){ids <- NULL}
      
      weights <- all.vars(weights)[1]
      if(is.na(weights)){weights <- NULL}
      
      
      sem.args <- list(model=obj@syntax@model,
                       group="cell", 
                       missing=obj@input@missing,
                       se=obj@input@se,
                       fixed.x=obj@input@fixed.z,
                       group.label=obj@input@vlevels$cell, 
                       data=obj@input@data, 
                       group.w.free = !obj@input@fixed.cell,
                       cluster = ids,
                       sampling.weights = weights)
      sem.args <- c(sem.args, obj@input@method_args)
      m1 <- do.call("sem", sem.args)
      
    }
        
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



elrMainHypothesisTests <- function(obj, est, vcov.def, resid.df, stat){
  
  ng <- obj@input@ng
  nk <- obj@input@nk
  nz <- obj@input@nz
  Egx <- obj@parnames@Egx
  gammas <- obj@parnames@gammas
  constrainedgammas <- obj@parnames@constrainedgammas
  
  ## Hypothesis 1: No average treatment effects
  Wald <- try(t(est[Egx]) %*% solve(vcov.def[Egx,Egx]) %*% est[Egx], silent=TRUE)
  if(inherits(Wald, "try-error")){Wald <- NA}
  Wald.df <- length(Egx)
  Wald.pvalue <- 1 - pchisq(Wald, df=Wald.df)
  hypothesis1 <- c(Wald, Wald.df, Wald.pvalue)
  
  if(nz==0 & nk==1){
    hypotheses <- data.frame(rbind(hypothesis1))
    names(hypotheses) <- c("Wald Chi-Square", "df", "p-value")
    
    if(stat=="Ftest"){
      Fvalue <- Wald/Wald.df
      F.df1 <- Wald.df
      F.df2 <- resid.df
      F.pvalue <- 1 - pf(Fvalue, F.df1, F.df2)
      hypotheses <- data.frame(rbind(c(Fvalue, F.df1, F.df2, F.pvalue)))
      names(hypotheses) <- c("F value", "df1", "df2", "p-value")
    }
    
    row.names(hypotheses) <- "No average effects"
    
    return(hypotheses)
  }
  
  
  ## Hypothesis 2: No covariate effects in control group
  gammas_tmp <- c(matrix(c(gammas), ncol=ng)[-1,1])
  idx <- which(gammas_tmp %in% constrainedgammas)
  if(length(idx)>0){gammas_tmp <- gammas_tmp[-idx]}
  
  Wald <- try(t(est[gammas_tmp]) %*% solve(vcov.def[gammas_tmp,gammas_tmp]) %*% est[gammas_tmp], silent=TRUE)
  if(inherits(Wald, "try-error")){Wald <- NA}
  Wald.df <- length(gammas_tmp)
  Wald.pvalue <- 1 - pchisq(Wald, df=Wald.df)
  hypothesis2 <- c(Wald, Wald.df, Wald.pvalue)
  
  
  ## Hypothesis 3: No treatment*covariate interaction
  gammas_tmp <- c(matrix(c(gammas), ncol=ng)[-1,-1])
  idx <- which(gammas_tmp %in% constrainedgammas)
  if(length(idx)>0){gammas_tmp <- gammas_tmp[-idx]}
  
  Wald <- try(t(est[gammas_tmp]) %*% solve(vcov.def[gammas_tmp,gammas_tmp]) %*% est[gammas_tmp], silent=TRUE)
  if(inherits(Wald, "try-error")){Wald <- NA}
  Wald.df <- length(gammas_tmp)
  Wald.pvalue <- 1 - pchisq(Wald, df=Wald.df)
  hypothesis3 <- c(Wald, Wald.df, Wald.pvalue)

    
  ## Hypothesis 4: No treatment effects
  gammas_tmp <- matrix(c(gammas), ncol=ng)[,-1]
  idx <- which(gammas_tmp %in% constrainedgammas)
  if(length(idx)>0){gammas_tmp <- gammas_tmp[-idx]}
  
  Wald <- try(t(est[gammas_tmp]) %*% solve(vcov.def[gammas_tmp,gammas_tmp]) %*% est[gammas_tmp], silent=TRUE)
  if(inherits(Wald, "try-error")){Wald <- NA}
  Wald.df <- length(gammas_tmp)
  Wald.pvalue <- 1 - pchisq(Wald, df=Wald.df)
  hypothesis4 <- c(Wald, Wald.df, Wald.pvalue)
  
  hypotheses <- data.frame(rbind(hypothesis1,hypothesis2,hypothesis3,hypothesis4))
  names(hypotheses) <- c("Wald Chi-Square", "df", "p-value")
  
  
  if(stat=="Ftest"){
    Fvalue <- hypotheses$Wald/hypotheses$df
    F.df1 <- hypotheses$df
    F.df2 <- resid.df
    F.pvalue <- 1 - pf(Fvalue, F.df1, F.df2)
    hypotheses <- data.frame(Fvalue, F.df1, F.df2, F.pvalue)
    names(hypotheses) <- c("F value", "df1", "df2", "p-value")
  }
  
  row.names(hypotheses) <- c("No average effects",
                             "No covariate effects in control group",
                             "No treatment*covariate interaction",
                             "No treatment effects")
  
  return(hypotheses)
  
  
}






elrKConditionalHypothesisTests <- function(obj, est, vcov.def, resid.df, stat){
  
  ng <- obj@input@ng
  nk <- obj@input@nk
  hypotheses <- vector("list",length=nk)
  
  if(nk==1){
    return(data.frame())
    
  }else if(nk>1){
    Egxk <- matrix(obj@parnames@Egxgk, nrow=ng-1, ncol=nk)
    
    for(i in 1:nk){
      Egxk_tmp <- Egxk[,i]
      Wald <- try(t(est[Egxk_tmp]) %*% solve(vcov.def[Egxk_tmp,Egxk_tmp]) %*% est[Egxk_tmp], silent=TRUE)
      if(inherits(Wald, "try-error")){Wald <- NA}
      Wald.df <- length(Egxk_tmp)
      Wald.pvalue <- 1 - pchisq(Wald, df=Wald.df)
      hypothesis <- c(Wald, Wald.df, Wald.pvalue)
      
      hypotheses[[i]] <- hypothesis
    }
  }
  
  hypotheses <- do.call(rbind.data.frame, hypotheses)
  row.names(hypotheses) <- paste0("No average effects given K=", 0:(nk-1))
  colnames(hypotheses) <- c("Wald Chi-Square", "df", "p-value")

  if(stat=="Ftest"){
    Fvalue <- hypotheses$Wald/hypotheses$df
    F.df1 <- hypotheses$df
    F.df2 <- resid.df
    F.pvalue <- 1 - pf(Fvalue, F.df1, F.df2)
    hypotheses <- data.frame(Fvalue, F.df1, F.df2, F.pvalue)
    row.names(hypotheses) <- paste0("No average effects given K=", 0:(nk-1))
    names(hypotheses) <- c("F value", "df1", "df2", "p-value")
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
  
  ##TODO: Improve code (see also elr_compute_descriptives_z)
  
  ## manifest outcome variable  
  if(obj@input@vnames$y %in% names(obj@input@data)){
    
    fv <- fitted.values(m1_sem)[1:nk] ## means and variances given X=0, K=k
    
    for(i in 1:nk){
      tmp <- fv[[i]]
      vary[i] <- tmp$cov[[obj@input@vnames$y,obj@input@vnames$y]]
      meany[i] <- tmp$mean[obj@input@vnames$y]
    }
    
  }else{ ## latent outcome variable
    
    fv.cov <- lavInspect(m1_sem, what="cov.lv")[1:nk]
    fv.mean <- lavInspect(m1_sem, what="mean.lv")[1:nk]    
    
    for(i in 1:nk){
      tmp.cov <- fv.cov[[i]]
      tmp.mean <- fv.mean[[i]]
      
      vary[i] <- tmp.cov[[obj@input@vnames$y,obj@input@vnames$y]]
      meany[i] <- tmp.mean[[obj@input@vnames$y]]
    }
    
  }
  
  meanyx0 <- sum(meany*Pkgx) ##TODO: compute parameter in model constraint
  sdyx0 <- sqrt(sum(vary*Pkgx) + sum(Pkgx*(meany-meanyx0)^2)) ## law of total variance  
  
  return(sdyx0)
  
}


## additional parameters based on lm() results

computeAdditionalLMCoefficients <- function(obj, m1_lm){
  
  coefs <- coef(m1_lm) 
  vcovs <- vcov(m1_lm)
  
  pnames <- obj@parnames@betas
  if(obj@input@interactions != "all"){
    idx <- which(obj@parnames@gammas %in% obj@parnames@constrainedgammas)
    pnames <- pnames[-idx]
  }
  names(coefs) <- row.names(vcovs) <- colnames(vcovs) <- pnames
  
  con_full <- obj@syntax@model
  partable <- lavaanify(con_full)
  
  ## augment coefs, vcovs, and partable with stochastic group sizes
  if(obj@input@fixed.cell == FALSE){
    
    N <- nrow(obj@input@data)
    prop <- obj@input@observed.freq
    prop.vcm <- (diag(prop) - prop %*% t(prop)) / N
    names(prop) <- row.names(prop.vcm) <- colnames(prop.vcm) <- obj@parnames@groupw
    
    coefs <- c(prop, coefs)
    vcovs <- lav_matrix_bdiag(prop.vcm, vcovs)
    
    ## augment partable
    groupw <- obj@parnames@groupw
    tmp <- paste0("group % c(", paste(groupw, collapse=","), ")*w")
    pt2 <- lavaanify(tmp,ngroups=length(groupw))
    partable <- rbind(pt2,partable)
    nfreepars <- sum(!partable$free == 0)
    partable$free[1:nfreepars] <- 1:nfreepars
  }
  
  ## compute effects
  conparse <- lav_constraints_parse(partable)
  def.function <- conparse$def.function
  JAC <- lav_func_jacobian_complex(func=def.function, x=coefs)
  info.r <- JAC %*% vcovs %*% t(JAC)
  
  se <- sqrt(diag(info.r))
  est <- def.function(coefs)
  vcov.def <- info.r
  
  row.names(vcov.def) <- colnames(vcov.def) <- names(se) <- names(est)
  
  est <- c(coefs, est)
  se <- c(sqrt(diag(vcovs)), se)
  
  
  return(list(est=est, se=se, vcov.def=vcov.def))
  
}


## model matrix function
computeModelMatrix <- function(obj){
  
  stopifnot(length(obj@input@measurement)==0) ## no latent variables
  
  current.contrast.action <- options('contrasts')
  on.exit(options(current.contrast.action))
  
  options(contrasts=c("contr.treatment","contr.poly"))
  
  
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
  if(length(constrainedgammas) != 0){
    
    idx <- which(colnames(modmat) %in% constrainedgammas)
    modmat <- modmat[,-idx]
  }
  
  return(modmat)
  
}


