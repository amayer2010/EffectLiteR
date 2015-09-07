

## helper function to compute the vcov matrix of additional paramers
## this is needed to compute the standard errors for conditional effects
## remove as soon as Yves has this functionality in lavaan
computeVcovAdditionalParameters <- function(object){
  
  ## free parameters only
  theta <- object@Fit@x
  
  # remove == constraints from parTable
  PT <- as.data.frame(object@ParTable, stringsAsFactors = FALSE)
  eq.idx <- which(PT$op == "==")
  if(length(eq.idx) > 0L) {
    PT <- PT[-eq.idx,]
  }
  partable <- as.list(PT)
  
  ## names new parameters
  def.idx <- which(partable$op == ":=")
  lhs.names <- partable$lhs[def.idx]
  
  ## def_function
  def.function <- lav_partable_constraints_def_copy(partable)
  
  ## compute Jacobian
  JAC <- try(lav_func_jacobian_complex(func = def.function, x = theta), silent = TRUE)
  if (inherits(JAC, "try-error")) {
    JAC <- lav_func_jacobian_simple(func = def.function, x = theta)
  }
  
  ## compute vcov of new parameters
  VCOV <- vcov(object, labels = FALSE)
  VCOV_np  <- JAC %*% VCOV %*% t(JAC)
  row.names(VCOV_np) <- colnames(VCOV_np) <- lhs.names
  
  return(VCOV_np)
}


## copy of Yves Rosseel's function lav_partable_constraints_def from lavaan 0.5-18
## used until a general solution for computing the vcov of derived parameters
## is available in lavaan
lav_partable_constraints_def_copy <- function(partable, con = NULL, debug = FALSE,
                                         defTxtOnly = FALSE) {
  
  # empty function
  def.function <- function() NULL
  
  # if 'con', merge partable + con
  if(!is.null(con)) {
    partable$lhs <- c(partable$lhs, con$lhs)
    partable$op  <- c(partable$op,  con$op )
    partable$rhs <- c(partable$rhs, con$rhs)
  }
  
  # get := definitions
  def.idx <- which(partable$op == ":=")
  
  # catch empty def
  if(length(def.idx) == 0L) {
    if(defTxtOnly) {
      return(character(0L))
    } else {
      return(def.function)
    }
  }
  
  # create function
  formals(def.function) <- alist(x=, ...=)
  if(defTxtOnly) {
    BODY.txt <- ""
  } else {
    BODY.txt <- paste("{\n# parameter definitions\n\n")
  }
  
  lhs.names <- partable$lhs[def.idx]
  def.labels <- all.vars( parse(file="", text=partable$rhs[def.idx]) )
  # remove the ones in lhs.names
  idx <- which(def.labels %in% lhs.names)
  if(length(idx) > 0L) def.labels <- def.labels[-idx]
  
  # get corresponding 'x' indices
  def.x.idx  <- partable$free[match(def.labels, partable$label)]
  if(any(is.na(def.x.idx))) {
    stop("lavaan ERROR: unknown label(s) in variable definition(s): ",
         paste(def.labels[which(is.na(def.x.idx))], collapse=" "))
  }
  if(any(def.x.idx == 0)) {
    stop("lavaan ERROR: non-free parameter(s) in variable definition(s): ",
         paste(def.labels[which(def.x.idx == 0)], collapse=" "))
  }
  def.x.lab  <- paste("x[", def.x.idx, "]",sep="")
  # put both the labels the function BODY
  if(length(def.x.idx) > 0L) {
    BODY.txt <- paste(BODY.txt, "# parameter labels\n",
                      paste(def.labels, " <- ",def.x.lab, collapse="\n"),
                      "\n", sep="")
  }
  
  # write the definitions literally
  BODY.txt <- paste(BODY.txt, "\n# parameter definitions\n", sep="")
  for(i in 1:length(def.idx)) {
    BODY.txt <- paste(BODY.txt,
                      lhs.names[i], " <- ", partable$rhs[def.idx[i]], "\n", sep="")
  }
  
  if(defTxtOnly) return(BODY.txt)
  
  # put the results in 'out'
  BODY.txt <- paste(BODY.txt, "\nout <- ",
                    paste("c(", paste(lhs.names, collapse=","),")\n", sep=""), sep="")
  # what to do with NA values? -> return +Inf???
  BODY.txt <- paste(BODY.txt, "out[is.na(out)] <- Inf\n", sep="")
  BODY.txt <- paste(BODY.txt, "names(out) <- ",
                    paste("c(\"", paste(lhs.names, collapse="\",\""), "\")\n", sep=""),
                    sep="")
  BODY.txt <- paste(BODY.txt, "return(out)\n}\n", sep="")
  
  body(def.function) <- parse(file="", text=BODY.txt)
  if(debug) { cat("def.function = \n"); print(def.function); cat("\n") }
  
  def.function
}
