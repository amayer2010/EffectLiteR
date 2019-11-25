
setMethod("show", "effectlite", function(object) {
  
  ng <- object@input@ng
  nk <- object@input@nk
  nz <- object@input@nz
  vnames <- object@input@vnames    
  vlevels <- object@input@vlevels
  gammas <- object@parnames@gammas
  gammalabels <- object@parnames@gammalabels
  label.g.function <- object@parnames@label.g.function
  label.covs <- object@parnames@label.covs
  label.Egx <- object@parnames@label.Egx
  
  if(object@input@method == "sem"){
    if(!lavInspect(object@results@lavresults, "converged")){
      cat("\n\n------------------------------------------------------\n")
      cat("---------------------- Warning  ----------------------\n")
      cat("------------------------------------------------------ \n\n")
      cat("lavaan model has not converged \n\n")
      
    }else if(lavInspect(object@results@lavresults, "converged")){
      cat("\n\n--------------------- Message  --------------------- \n\n")
      cat(" -- model converged succesfully -- \n")
    }
  }
  
  cat("\n\n--------------------- Variables  --------------------- \n\n")
  cat("Outcome variable Y: ", paste0(vnames$y), "\n")
  cat("Treatment variable X: ", paste0(vnames$x), "  (Reference group: ", 
      paste0(object@input@control, ")\n"))
  if(!is.null(vnames$k)){
    cat("Categorical covariates K: ", paste0(vnames$k), "\n")
  }
  if(!is.null(vnames$z)){
    tmp <- "Continuous covariates in Z=("
    tmp <- paste0(tmp, paste0("Z",1:nz, collapse=","), "): ")
    tmp <- paste0(tmp, paste0(paste0("Z", 1:nz, "="), vnames$z, collapse=" "))
    cat(tmp, "\n")
  }
  if(!is.null(vnames$propscore)){
    v <- vnames$propscore
    if(is(v, "formula")){v <- all.vars(v[[3]])}  
    cat("Covariates for propensity score V: ", paste0(v), "\n")
  }
  
  cat("\nLevels of Treatment Variable X \n")
  tmp <- data.frame(vlevels$x,
                    vlevels$levels.x.original,
                    paste0("I_X=",vlevels$x))
  names(tmp) <- c("X", 
                  paste0(vnames$x, " (original)"), 
                  "Indicator")
  print(tmp, row.names=F, print.gap=3)

  
  if(nk>1){
    cat("\nLevels of Unfolded Categorical Covariate K \n")
    tmp <- vlevels$levels.k.original
    tmp <- tmp[length(tmp):1]
    tmp <- expand.grid(tmp)
    tmp$K <- vlevels$kstar
    tmp <- tmp[,ncol(tmp):1]
    tmp$Indicator <- paste0("I_K=", vlevels$kstar)
    print(tmp, row.names=F, print.gap=3)
  }
  
  
  if(nk>1){
    cat("\nCells \n")
    tmp <- expand.grid(K=vlevels$kstar, X=vlevels$levels.x.original)[,2:1]
    names(tmp)[1] <- paste0(vnames$x, " (original)")
    tmp$Cell <- vlevels$cell
    print(tmp, print.gap=3)
    
  }
  
  
  cat("\n\n --------------------- Regression Model --------------------- \n")
  
  tmp <- paste0("E(Y|X",label.covs,") = ")
  tmp <- paste0(tmp, "g0",label.g.function," + ")
  tmp <- paste0(tmp, paste0("g",1:(ng-1),label.g.function,"*I_X=",1:(ng-1), 
                            collapse=" + "))
  cat("\n",tmp, "\n")
  
  gammalabels2 <- gammalabels[,,1]
  gammalabels2[1] <- ""
  
  for(i in 1:ng){
    tmp <- paste0("  g",i-1,label.g.function," = ")
    tmp <- paste0(tmp, paste(gammas[,,i], gammalabels2, sep=" * ", collapse=" + "))
    tmp <- gsub("*  ", "", tmp, fixed=TRUE)
    if(length(gammalabels2)==1){tmp <- gsub("*", "", tmp, fixed=TRUE)}
    
    if(nchar(tmp) > 80){
      ## split g function over several lines
      tmp <- unlist(strsplit(tmp, " + ", fixed=TRUE))
      tmp <- capture.output(cat(tmp, sep=" + ", fill=80))
      tmp[2:length(tmp)] <- paste0("            + ",tmp[2:length(tmp)])
      cat(tmp, sep="\n")
    } else{
      cat(tmp, "\n")
    }
  }
  
  ## print coefficients of g-Functions  
  for(i in 1:ng){
    if(i==1){
      tmp <- paste0("Intercept Function g",i-1,label.g.function,
                    "  [Reference group: ", 
                    paste0(object@input@control), "]")
      cat("\n",tmp, "\n\n")
    }else{
      tmp <- paste0("Effect Function g",i-1,label.g.function)
      tmp <- paste0(tmp, "   [", object@input@vnames$x, 
                    ": ", object@input@vlevels$levels.x.original[i],
                    " vs. ",object@input@vlevels$levels.x.original[1], "]")
      cat("\n",tmp, "\n\n")
    }
    tmp <- object@results@gx[[i]]
    tmp[,2:5] <- round(tmp[,2:5], digits=3)
    print(tmp, print.gap=3, row.names=FALSE)
  }
  
  
  cat("\n\n--------------------- Cell Counts  --------------------- \n\n")

  cat("\n")
  cat("Cell Counts \n\n")
  cat("This table shows cell counts including missings. \n")
  cat("See also output under lavaan results for number of observations \n")
  cat("actually used in the analysis. \n\n")
  
  if(nk==1){
    print(ftable(object@input@data[vnames$x]), print.gap=3)
  }else{
    cellcounts <- as.formula(paste0(paste(vnames$k, collapse="+"), 
                                    "~", vnames$x))
    print(ftable(cellcounts, data=object@input@data), print.gap=3)
  }
  
  
  cat("\n\n--------------------- Main Hypotheses --------------------- \n\n")
  if(nrow(object@results@hypotheses)==0){
    cat("Wald tests for main hypotheses are currently not available for models with \n non-standard SEs and for models with (in-)equality constraints (e.g., on interactions).")
  }else{
    
    rowname1 <- paste0(label.Egx, collapse=" = ")
    rowname1 <- paste0("H0: No average effects: ", rowname1, " = 0")
    rowname2 <- paste0("H0: No covariate effects in control group: g0", label.g.function, " = constant")
    rowname3 <- paste0("g", 1:(ng-1), label.g.function, collapse=", ")
    rowname3 <- paste0("H0: No treatment*covariate interaction: ", rowname3, " = constant")
    rowname4 <- paste0("g", 1:(ng-1), label.g.function, collapse=" = ")
    rowname4 <- paste0("H0: No treatment effects: ", rowname4, " = 0")
    
    if(nz==0 & nk==1){
      cat(rowname1)
    }else{
      cat(paste0(c(rowname1, rowname2, rowname3, rowname4), collapse="\n"))
    }
    cat("\n\n")
    
    hypotheses <- object@results@hypotheses
    print(hypotheses, digits=3, print.gap=3)
  }
  
  cat("\n\n --------------------- Adjusted Means --------------------- \n\n")
  namesadjmeans <- paste0("Adj.Mean",0:(ng-1))
  adjmeans <- object@results@adjmeans
  row.names(adjmeans) <- namesadjmeans
  print(adjmeans, digits=3, print.gap=3)
  
  
  cat("\n\n --------------------- Average Effects --------------------- \n\n")
  Egx <- object@results@Egx
  row.names(Egx) <- label.Egx
  print(Egx, digits=3, print.gap=3)
  
  if(!(nz==0 & nk==1)){
    cat("\n\n --------------------- Effects given a Treatment Condition --------------------- \n\n")
    tmp <- expand.grid(g=1:(ng-1), x=0:(ng-1))
    namesEgxgx <- paste0("E[g",tmp$g,label.g.function,"|X=",tmp$x, "]")
    Egxgx <- object@results@Egxgx
    row.names(Egxgx) <- namesEgxgx
    print(Egxgx, digits=3, print.gap=3)
    
  }
  
  if(nk>1){
    cat("\n\n --------------------- Effects given K=k --------------------- \n\n")
    tmp <- expand.grid(g=1:(ng-1), k=0:(nk-1))
    namesEgxgk <- paste0("E[g",tmp$g,label.g.function,"|K=",tmp$k,"]")
    Egxgk <- object@results@Egxgk
    row.names(Egxgk) <- namesEgxgk
    print(Egxgk, digits=3, print.gap=3)    
  }
  
  if(nk>1 & nz>0){
    cat("\n\n --------------------- Effects given X=x, K=k --------------------- \n\n")
    Egxgxk <- paste0("Eg",tmp$g,"gx",tmp$x,"k",tmp$k)    
    tmp <- expand.grid(g=1:(ng-1), x=0:(ng-1), k=0:(nk-1))
    namesEgxgxk <- paste0("E[g",tmp$g,label.g.function,"|X=",tmp$x,", K=",tmp$k,"]")
    Egxgxk <- object@results@Egxgxk
    row.names(Egxgxk) <- namesEgxgxk
    print(Egxgxk, digits=3, print.gap=3)    
  }

  if(nk>1){
  cat("\n\n--------------------- Hypotheses given K=k --------------------- \n\n")
  if(nrow(object@results@hypothesesk)==0){
    cat("Wald tests for these hypotheses are currently not available for models with \n non-standard SEs and for models with (in-)equality constraints (e.g., on interactions).")
  }else{
    
    for(i in 1:nk){
      tmp <- paste0("E[g",1:(ng-1),label.g.function,"|K=",i-1,"]")
      tmp <- paste0(tmp, collapse=" = ")
      tmp <- paste0("H0: No average effects given K=", i-1,": ", tmp, " = 0")
      cat(tmp, "\n")
    }
    cat("\n")
    
    hypothesesk <- object@results@hypothesesk
    print(hypothesesk, digits=3, print.gap=3)
  }
    
  }
  
  # ## currently not printed  
  # if(nz>0){
  #   cat("\n\n --------------------- Average Effects of Continuous Covariates --------------------- \n\n")
  #   AveEffZ <- object@results@AveEffZ
  #   print(AveEffZ, digits=3, print.gap=3)
  #   
  # }
  
  if(length(object@input@add > 0)){
    if(grepl(":=", object@input@add)){
      cat("\n\n --------------------- User Defined Parameters/Effects --------------------- \n\n")
      AdditionalEffects <- object@results@AdditionalEffects
      print(AdditionalEffects, digits=3, print.gap=3)
    }}
  
  propscore <- object@input@vnames$propscore
  if(!is.null(propscore)){
    cat("\n\n --------------------- Propensity Score Model --------------------- \n\n")
    cat("Model equation: log[P(X=1|V)/P(X=0|V)] = h1(V)", "\n")
    if(ng > 2){
      for(i in 3:ng){
        tmp <- paste0("                ", 
                      "log[P(X=", (i-1), "|V)/P(X=0|V)] = h",(i-1),"(V)",
                      "\n")
        cat(tmp)
      }
    }
    cat("R formula for nnet::multinom: ", object@input@outprop$formula, "\n")
    cat("\nEstimate\n")
    print(object@input@outprop$coef, digits=3, print.gap=3)
    cat("\nStandard Error\n")
    print(object@input@outprop$se, digits=3, print.gap=3)
    cat("\nEst./SE\n")
    print(object@input@outprop$tval, digits=3, print.gap=3)
  }
  
  
})

