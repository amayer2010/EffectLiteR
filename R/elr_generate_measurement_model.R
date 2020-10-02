
#' Generate measurement model
#' 
#' This function automatically generates \code{lavaan} syntax for the measurement model for a call to \code{\link[EffectLiteR]{effectLite}}. It is currently also used in the shiny interface.
#'
#' @param names A vector of character strings with names of latent variables. If not specified, names(indicators) is used.
#' @param indicators A list of vectors of character strings to specify indicators of latent variables (see example).
#' @param ncells Number of groups/cells.
#' @param model A vector of character strings of the same length as names. It is used to specify the type of measurement model for each of the latent variables. Each element can be one of \code{c("default","parallel","tau-equi","tau-cong","tau-equi-categorical","tau-cong-categorical")} indicating whether a parallel, essentially tau-equivalent, or tau-congeneric measurement model is used and whether the items are categorical or not. If "default", the function tries to guess a reasonable measurement model: Congeneric for latent variables with three or more indicators, essentially tau-equivalent for latent variables with less than three indicators and for latent variables with cross-loadings (e.g., method factors), and parallel for single-indicator latent variables. If NULL, "default" is assumed for all latent variables.
#' @param data A data set that includes the indicator variables. It is required only for categorical indicators to detect the number of categories.
#' @examples
#' ## Example with three latent variables
#' names <- c("eta", "xi1", "xi2")
#' indicators <- list("eta" = c("y1","y2","y3"), 
#'                    "xi1" = c("z1","z2"),
#'                    "xi2" = c("z12","z22","z32","z42"))
#' ncells = 6
#' model = c("parallel","tau-equi","tau-cong")
#' cat(generateMeasurementModel(names, indicators, ncells, model))
#' 
#' ## Example with method factor
#' names <- c("eta", "xi", "mf")
#' indicators <- list("eta" = c("y12","y22"), 
#'                    "xi" = c("y11","y21"),
#'                    "mf" = c("y12","y22"))
#' ncells = 2
#' cat(generateMeasurementModel(names, indicators, ncells))
#' 
#' ## Example with categorical items
#' names <- c("eta", "xi")
#' indicators <- list("eta" = paste0("y",1:7,1),
#'                    "xi" = paste0("z",1:5,1))
#' ncells = 2
#' model = c("tau-equi-categorical","tau-cong-categorical")
#' cat(generateMeasurementModel(names, indicators, ncells, model, 
#'                              data=elrdata_categorical_items))
#' 
#' @export
generateMeasurementModel <- function(names=NULL, indicators, ncells, 
                                     model=NULL, data=NULL){  
  
  if(is.null(names)){names <- names(indicators)}
  if(is.null(model)){model <- rep("default", times=length(names))}
  
  stopifnot(length(names) == length(indicators))
  stopifnot(length(names) == length(model))
  stopifnot(all(model %in% c("default","parallel","tau-equi","tau-cong",
                             "tau-equi-categorical","tau-cong-categorical")))
  if(any(model %in% c("tau-equi-categorical")) & is.null(data)){
    stop("Data argument is required for categorical indicators")}
  
  ## check for cross-loadings, i.e., an indicator appears multiple times 
  ## probably method factor...
  tmp <- unlist(indicators)
  cross.loadings <- length(unique(tmp)) < length(tmp)
  
  ## how many indicators per latent variable
  numberindicators <- unlist(lapply(indicators, length))
  
  mm <- character(0)
  
  ## loadings
  for(i in 1:length(names)){
    for(k in 1:length(indicators[[i]])){ 
      
      if(model[i] %in% c( "parallel","tau-equi","tau-equi-categorical")){
        loading <- 1

      }else if(model[i] %in% c("tau-cong", "tau-cong-categorical")){
        if(k==1){
          loading <- 1
        }else{
          loading <- paste0("la1",k,i)
        }
        
      }else if(model[i] == "default"){
        if(numberindicators[i] <= 2){
          loading <- 1
        }else if(cross.loadings){
          loading <- 1
        }else{
          if(k==1){
            loading <- 1
          }else{
            loading <- paste0("la1",k,i)
          }
        }
      }
      
      tmp <- paste0("c(",paste(rep(loading,ncells),collapse=","),")")
      tmp <- paste0(tmp,"*",indicators[[i]][k])
      tmp <- paste0(names[i], " =~ ", tmp)
      mm <- c(mm, tmp)
    }    
  }
  
  ## intercepts
  for(i in 1:length(names)){
    for(k in 1:length(indicators[[i]])){ 
      
      if(model[i] %in% c("parallel","tau-equi-categorical","tau-cong-categorical")){
        intercept <- 0
        
      }else if(model[i] %in% c("tau-equi","tau-cong")){
        if(k==1){
          intercept <- 0
        }else{
          intercept <- paste0("la0",k,i)
        }

      }else if(model[i] == "default"){
        if(cross.loadings){
          intercept <- 0
        }else{
          if(k==1){
            intercept <- 0
          }else{
            intercept <- paste0("la0",k,i)
          }
        }
      }
      
      tmp <- paste0("c(",paste(rep(intercept,ncells),collapse=","),")")
      tmp <- paste0(indicators[[i]][k], " ~ " , tmp,"*1")
      mm <- c(mm, tmp)
    }    
  }
  
  
  ## thresholds
  for(i in 1:length(names)){
    for(k in 1:length(indicators[[i]])){ 
      
      if(model[i] %in% c("tau-equi-categorical","tau-cong-categorical")){
        item <- data[,indicators[[i]][k]]
        nthresholds <- length(unique(item)) - 1
        
        for(t in 1:nthresholds){
          if(k==1 & t==1){
            threshold <- 0
            
          }else{
            threshold <- paste0("th",k,i,t)
          }
          
          tmp <- paste0("c(",paste(rep(threshold,ncells),collapse=","),")")
          tmp <- paste0(indicators[[i]][k], " | " , tmp,"*t",t)
          mm <- c(mm, tmp)
          
        }
        
      }
    }    
  }
  
  ## scales
  for(i in 1:length(names)){
    for(k in 1:length(indicators[[i]])){ 
      
      if(model[i] %in% c("tau-equi-categorical","tau-cong-categorical")){
        tmp <- paste0("c(1,",paste(rep("NA",ncells-1),collapse=","),")")
        tmp <- paste0(indicators[[i]][k], " ~*~ " , tmp,"*",indicators[[i]][k])
        mm <- c(mm, tmp)
      }
    }    
  }
  
  mm <- unique(mm) ## remove duplicate entries (for method factors and alike)
  mm <- paste(mm, collapse="\n")
  
  return(mm)
}

