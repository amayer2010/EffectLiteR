
createInput <- function(y, x, k, z, data, method, control, measurement, 
                        fixed.cell, fixed.z, missing, se, interactions, 
                        homoscedasticity, test.stat, propscore, ids, weights,
                        add, method_args){
  
  d <- data
  
  ## fix problems with tibbles
  if(any(c("tbl_df","tbl") %in% class(d))){
    message("EffectLiteR message: tibbles are currently not supported. Converted to data.frame")
    d <- as.data.frame(d)
  }
  
  
  latentz <- z[which(!z %in% names(data))] ##TODO fix for interactions between continuous covariates
  manifestz <- z[which(z %in% names(data))]
  vnames <- list(y=y,x=x,k=k,z=z,propscore=propscore,latentz=latentz,manifestz=manifestz)  
  
  ## check consistency with method argument
  if(method=="sem"){
    if(fixed.cell=="default"){fixed.cell <- FALSE}
    if(fixed.z=="default"){fixed.z <- FALSE}
    if(homoscedasticity=="default"){homoscedasticity <- FALSE}
    if(test.stat=="default"){test.stat <- "Chisq"}
    
    if(!is.null(weights)){
      if(x=="g"){
        stop('EffectLiteR error: Please rename treatment variable ("g" is not allowed).')
      }
    }
  }

  if(method=="lm"){
    if(fixed.cell=="default"){fixed.cell <- TRUE}
    
    if(fixed.z=="default"){
      fixed.z <- TRUE
    }else if(fixed.z==FALSE){
      
      if(!is.null(z)){
        stop('EffectLiteR error: Stochastic covariates are currently not allowed with method="lm".')
      }
    }
    
    if(homoscedasticity=="default"){
      homoscedasticity <- TRUE
    }else if(homoscedasticity==FALSE){
      stop('EffectLiteR error: Heteroscedasticity is currently not allowed with method="lm".')
    }
    
    if(test.stat=="default"){test.stat <- "Ftest"}
    
    if(length(measurement) != 0){
      stop('EffectLiteR error: Measurement models are currently not allowed with method="lm".')
    }
    
    if(length(add) != 0){
      if(grepl("==", add) | grepl(">", add) | grepl("<", add)){
        stop('EffectLiteR error: Equality and inequality constraints are currently not allowed with method="lm".')
      }
    }
    
    if(ids != ~0){
      stop('EffectLiteR error: Complex survey functionality is currently not allowed with method="lm".')
    }
    
    if(!is.null(weights)){
      stop('EffectLiteR error: Complex survey functionality is currently not allowed with method="lm".')
    }
    
  }
  
    
  ## treatment variable
  if(!is.factor(d[,x])){    
    d[,x] <- as.factor(d[,x])  
  }

  if(control=="default"){control <- levels(d[,x])[1]}
  d[,x] <- relevel(d[,x], control)
  levels.x.original <- levels(d[,x])
  levels(d[,x]) <- paste(0:(length(levels(d[,x]))-1))  
  
  ## categorical covariates
  levels.k.original <- vector("list",length(k))
  names(levels.k.original) <- k
  
  if(!is.null(k)){    
    for(i in 1:length(k)){
      d[,k[i]] <- as.factor(d[,k[i]])
      levels.k.original[[i]] <- levels(d[,k[i]])
      levels(d[,k[i]]) <- paste(0:(length(levels(d[,k[i]]))-1))
    }    
  }
  
  ## unfolded k variable
  levels.kstar.original <- vector("character")
  if(!is.null(k)){
    if(length(k)>1){
      d$kstar <- apply(d[,k],1,paste,collapse="")
      d$kstar <- as.factor(d$kstar)    
    }else{
      d$kstar <- d[,k]
    }
    levels.kstar.original <- levels(d$kstar)
    levels(d$kstar) <- paste(0:(length(levels(d$kstar))-1))
    
    ## check for empty cells
    if(any(table(d$kstar, d[,x]) == 0)){
      stop("EffectLiteR error: Empty cells are currently not allowed.")
    }    
    
  }else{
    d$kstar <- NULL
  }
  
  ## nk
  nk <- 1L
  if(!is.null(k)){
    nk <- length(levels(d$kstar))
  }
  
  ## ng
  ng <- length(levels(d[,x]))  
  
  ## nz
  nz <- length(vnames$z)
  
  ## longer parameter names for many groups and/or covariates
  sep <- ""
  if(ng>9 | nk>9 | nz>9){sep <- "_"}
  
  ## cell variable (xk-cells)
  if(!is.null(k)){
    dsub <- cbind(d[,x],d$kstar) - 1 # use x=0,1,2... and k=0,1,2,... as labels
    d$cell <- apply(dsub, 1, function(x){
      missing_ind <- sum(is.na(x)) > 0
      if(missing_ind){
        return(NA)
      }else{
        return(paste(x, collapse=sep))
      }
    }) 
    
    levels.cell <- expand.grid(k=levels(d$kstar), x=levels(d[,x]))
    levels.cell <- with(levels.cell, paste0(x,sep,k))
    d$cell <- factor(d$cell, levels=levels.cell)    
  }else{
    # cell <- levels(d[,x])
    d$cell <- d[,x]
  }
  
  
  ## observed cell frequencies
  N <- nrow(d)
  observed.freq <- c(table(d$cell)/N)
    
  if(!is.null(weights)){
    weights_vector <- model.matrix(weights, d)
    if(ncol(weights_vector) > 2){stop("EffectLiteR error: Currently only support for one weights variable")}
    weights_vector <- weights_vector[,-1]
    observed.freq <- c(tapply(weights_vector, d$cell, sum))
    observed.freq <- observed.freq/sum(observed.freq) ## rescale to sum to one
    message("EffectLiteR message: The observed frequencies have been re-computed taking into account the survey weights.")
  }

  ## observed sample means for manifest covariates (fixed.z only)
  if(nz==0){
    sampmeanz <- matrix(nrow=0, ncol=0)
    
  }else if(!fixed.z){
    sampmeanz <- matrix(nrow=0, ncol=0)
    
  }else if(fixed.z){
      
    if(!fixed.cell){
      stop("EffectLiteR error: fixed.z=TRUE requires fixed.cell=TRUE")
      
    }else if(!identical(latentz, character(0))){
      stop("EffectLiteR error: fixed.z=TRUE does not work with latent covariates.")
    }
    
    sampmeanz <- NULL
    for (i in 1:nz) {
      namez <- z[i]
      tmp <- tapply(d[[namez]], d$cell, function(x){mean(x, na.rm=TRUE)})
      sampmeanz <- rbind(sampmeanz, tmp)
    }
    row.names(sampmeanz) <- z
    
  }
  
  
  ## add vlevels for created variables
  vlevels <- list(levels.x.original=levels.x.original,
                  levels.k.original=levels.k.original,
                  levels.kstar.original=levels.kstar.original,
                  x=levels(d[,x]),
                  kstar=levels(d$kstar),
                  cell=levels(d$cell))
  
  
  
  complexsurvey <- list(ids=ids, weights=weights)
  
  res <- new("input",
             method=method,
             vnames=vnames, 
             vlevels=vlevels,
             ng=ng,
             nz=nz,
             nk=nk,
             control=control,
             data=d, 
             measurement=measurement,
             add=add,
             fixed.cell=fixed.cell,
             fixed.z=fixed.z,
             missing=missing,
             observed.freq=observed.freq,
             sampmeanz=sampmeanz,
             se=se,
             interactions=interactions,
             complexsurvey=complexsurvey,
             homoscedasticity=homoscedasticity,
             test.stat=test.stat,
             method_args=method_args
  )
  
  return(res)
}

