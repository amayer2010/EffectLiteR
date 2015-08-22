myReadData <- function(file, name, header="default", sep="default",
                         dec="default", use.value.labels="default",
                         na.strings="NA"){

  ptn <- "\\.[[:alnum:]]{1,5}$"
  suf <- tolower(regmatches(name, regexpr(ptn, name)))

  ## convert arguments from shiny ui
  if(header=="yes"){header <- TRUE}
  if(header=="no"){header <- FALSE}
  if(sep=="semicolon"){sep <- ";"}
  if(sep=="white space"){sep <- ""}
  if(dec=="decimal point"){dec <- "."}
  if(dec=="decimal comma"){dec <- ","}
  if(use.value.labels=="yes"){use.value.labels <- TRUE}
  if(use.value.labels=="no"){use.value.labels <- FALSE}

  if(suf == ".csv"){
    if(header=="default"){header <- TRUE}
    if(sep=="default"){sep <- ","}
    if(dec=="default"){dec <- "."}
    return(read.csv(file, header=header, sep=sep, dec=dec,
                    na.strings=na.strings))

  }else if(suf == ".txt" || suf==".dat"){
    if(header=="default"){header <- FALSE}
    if(sep=="default"){sep <- ""}
    if(dec=="default"){dec <- "."}
    return(read.table(file, header=header, sep=sep, dec=dec,
                      na.strings=na.strings))

  }else if(suf == ".sav"){
    if(use.value.labels=="default"){use.value.labels <- TRUE}
    return(foreign::read.spss(file, to.data.frame=TRUE,
                              use.value.labels=use.value.labels))

  }else if(suf == ".xpt"){
    return(foreign::read.xport(file))
  }

}

