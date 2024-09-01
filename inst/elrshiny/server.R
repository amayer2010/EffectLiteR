
options(shiny.maxRequestSize=100*1024^2) 

shinyServer(function(input, output, session) {
  
  ## close app when browser tab is closed
  session$onSessionEnded(function() { 
    stopApp() 
  })
  
  ####### New analysis / reload button ########
  output$reload <- renderUI({
    if (input$newanalysis > 0) {
      tags$script("window.location.reload();")
    }
  })
  
  ######## Reactive Data Input ########
  dataInput <- reactive({
    inFile <- input$file1
    exdata <- input$exdata
    
    if(is.null(inFile)){      
      if(exdata==""){
        return(NULL)        
      }else if(exdata=="nonortho"){
        return(nonortho)  
      }else if(exdata=="example01"){
        return(example01)
      }else if(exdata=="example02lv"){
        return(example02lv)
      }else if(exdata=="example_multilevel"){
        return(example_multilevel)
      }else if(exdata=="MDRS2016"){
        return(MDRS2016)
      }else if(exdata=="sophonet_data_simulated"){
        return(sophonet_data_simulated)
      }else if(exdata=="elrdata_categorical_items"){
        return(elrdata_categorical_items)
      }
    }
    
    if(!is.null(inFile)){
      
      return(elrReadData(file=inFile$datapath,
                        name=inFile$name,
                        header=input$header,
                        sep=input$sep,
                        dec=input$dec,
                        na.strings=input$na.strings,
                        use.value.labels=input$vallabels))
      
    }
  })
  
  ###### Reactive Run Model #########
  model <- reactive({
        
    ## arguments for effectLite()
    d <- dataInput()
    mm <- mm()
    
    dv <- depv()
    x <- input$variablex
    
    k <- NULL; if(length(input$variablek) != 0){k <- input$variablek}
    
    ## sampling model
    if(input$fixed.cell == "default"){
      fixed.cell <- "default"
      fixed.z <- "default"
    }else{
      fixed.cell <- FALSE
      fixed.z <- FALSE
      if(input$fixed.cell == "fixed"){fixed.cell <- TRUE}
      if(input$fixed.cell == "fixed+e"){fixed.cell <- TRUE; fixed.z=TRUE} 
    }
    
    
    z <- NULL; if(length(input$variablez) != 0){z <- input$variablez}
    if(isLatentCovs()){z <- c(z,latentcov())}
    
    propscore <- NULL 
    if(length(input$propscore) != 0 & !input$propscoreformula){
      propscore <- input$propscore}
    if(input$prop.formula != "" & input$propscoreformula){
      propscore <- as.formula(input$prop.formula)}
    
    interactions <- input$interactions
    
    ids <- ~0
    if(input$ids != ""){ids <- as.formula(paste0(" ~ ", input$ids))}
    
    weights <- NULL
    if(input$weights != ""){weights <- as.formula(paste0(" ~ ", input$weights))}
    
    homoscedasticity <- input$homoscedasticity
    if(homoscedasticity=="homoscedastic"){homoscedasticity <- TRUE}
    if(homoscedasticity=="heteroscedastic"){homoscedasticity <- FALSE}
    
    test.stat <- input$test.stat
    
    if(input$add.syntax == ""){
      add <- character()
    }else{
      add <- input$add.syntax
    }
    
    elr.args <- list(y=dv, 
                     x=x,
                     k=k,
                     z=z,
                     data=d,
                     method=input$method,
                     control=input$control,
                     measurement=mm,
                     missing=input$missing,
                     estimator=input$estimator,
                     se=input$se,
                     bootstrap=input$bootstrap,
                     fixed.cell=fixed.cell,
                     fixed.z=fixed.z,
                     interactions=interactions,
                     homoscedasticity=homoscedasticity,
                     test.stat=test.stat,
                     propscore=propscore,                 
                     ids=ids,
                     weights=weights,
                     add=add)

    tryCatch(
      do.call("effectLite", elr.args)
    )  
  })
  
  ######## Reactive latent covariates ########
  isLatentCovs <- reactive({
    
    input$latentz && !is.na(input$nlatentz) && input$nlatentz>0 && input$nlatentz<11
  })
  
  
  ######## Reactive zselect for Plot 2 ########
  zSelect <- reactive({
    zselect <- input$variablez
    
    if(!is.null(input$propscore)){
      d <- dataInput()
      x <- d[[input$variablex]]    
      ng <- length(unique(na.omit(x)))
      zselect <- c(zselect, paste0("logprop",1:(ng-1)))
    }
        
    return(zselect)
  })

  ######## Reactive gxselect for Plot 3 ########
  gxSelect <- reactive({
    
    d <- dataInput()
    x <- d[[input$variablex]]    
    ng <- length(unique(na.omit(x)))
    res <- paste0("g",2:ng-1)
    
    return(res)
  })
  
  
  ######## Reactive zselect2 for Plot 3 ########
  zSelect2 <- reactive({
    
    kstar <- NULL
    if(!is.null(input$variablek)){kstar <- "K"}
    zselect <- c(input$variablez, input$variablek, kstar, input$variablex)
    
    if(isLatentCovs()){
      nameslatentz <- c(input$name.etaz1, input$name.etaz2, input$name.etaz3,
                        input$name.etaz4, input$name.etaz5, input$name.etaz6, 
                        input$name.etaz7, input$name.etaz8, input$name.etaz9, 
                        input$name.etaz10)
      nameslatentz <- nameslatentz[1:input$nlatentz]
      zselect <- c(nameslatentz, zselect)
    }
    
    d <- dataInput()
    x <- d[[input$variablex]]    
    ng <- length(unique(na.omit(x)))
    
    if(!is.null(input$propscore)){
      zselect <- c(zselect, input$propscore)
      zselect <- c(zselect, paste0("logprop",1:(ng-1)))
    }
    if(input$prop.formula != "" & input$propscoreformula){
      try({
        propscore <- as.formula(input$prop.formula)
        zselect <- c(zselect, all.vars(propscore[[3]]))
      }, silent=TRUE)
      zselect <- c(zselect, paste0("logprop",1:(ng-1)))
    }
    
    zselect <- c(zselect, paste0("g",1:(ng-1)))
    
    return(zselect)
  })

  
  ######## Reactive zselect3 (colour) for Plot 3 ########
  zSelect3 <- reactive({
    kstar <- NULL
    if(!is.null(input$variablek)){kstar <- "K"}
    zselect3 <- c("", input$variablek, kstar, input$variablex, input$variablez)
    
    if(isLatentCovs()){
      nameslatentz <- c(input$name.etaz1, input$name.etaz2, input$name.etaz3,
                        input$name.etaz4, input$name.etaz5, input$name.etaz6, 
                        input$name.etaz7, input$name.etaz8, input$name.etaz9, 
                        input$name.etaz10)
      nameslatentz <- nameslatentz[1:input$nlatentz]
      zselect3 <- c(nameslatentz, zselect3)
    }
    
    d <- dataInput()
    x <- d[[input$variablex]]    
    ng <- length(unique(na.omit(x)))
      
    if(!is.null(input$propscore)){
      zselect3 <- c(zselect3, input$propscore)
      zselect3 <- c(zselect3, paste0("logprop",1:(ng-1)))
    }
    if(input$prop.formula != "" & input$propscoreformula){
      try({
        propscore <- as.formula(input$prop.formula)
        zselect3 <- c(zselect3, all.vars(propscore[[3]]))
      }, silent=TRUE)
      zselect3 <- c(zselect3, paste0("logprop",1:(ng-1)))
    }
    
    zselect3 <- c(zselect3, paste0("g",1:(ng-1)))
    
    return(zselect3)
  })

  
  ######## Reactive wselect for Conditional Effects IV ########
  wSelect <- reactive({
    
    d <- dataInput()
    wselect <- c("", names(d))
    
    if(isLatentCovs()){
      nameslatentz <- c(input$name.etaz1, input$name.etaz2, input$name.etaz3,
                        input$name.etaz4, input$name.etaz5, input$name.etaz6, 
                        input$name.etaz7, input$name.etaz8, input$name.etaz9, 
                        input$name.etaz10)
      nameslatentz <- nameslatentz[1:input$nlatentz]
      wselect <- c(wselect, nameslatentz)
    }
    
    return(wselect)
  })
  
  
  ######## Reactive gxselect2 for Plot 4 ########
  gxSelect2 <- reactive({

    d <- dataInput()
    x <- d[[input$variablex]]
    ng <- length(unique(na.omit(x)))
    res <- paste0("g",2:ng-1)

    return(res)
  })


  ######## Reactive zselect3 (colour) for Plot 4 ########
  zSelect4 <- reactive({
    kstar <- NULL
    if(!is.null(input$variablek)){kstar <- "K"}
    zselect4 <- c("", input$variablek, kstar, input$variablex, input$variablez)
    
    if(isLatentCovs()){
      nameslatentz <- c(input$name.etaz1, input$name.etaz2, input$name.etaz3,
                        input$name.etaz4, input$name.etaz5, input$name.etaz6, 
                        input$name.etaz7, input$name.etaz8, input$name.etaz9, 
                        input$name.etaz10)
      nameslatentz <- nameslatentz[1:input$nlatentz]
      zselect4 <- c(nameslatentz, zselect4)
    }

    d <- dataInput()
    x <- d[[input$variablex]]
    ng <- length(unique(na.omit(x)))

    if(!is.null(input$propscore)){
      zselect4 <- c(zselect4, input$propscore)
      zselect4 <- c(zselect4, paste0("logprop",1:(ng-1)))
    }
    if(input$prop.formula != "" & input$propscoreformula){
      try({
        propscore <- as.formula(input$prop.formula)
        zselect4 <- c(zselect4, all.vars(propscore[[3]]))
      }, silent=TRUE)
      zselect4 <- c(zselect4, paste0("logprop",1:(ng-1)))
    }

    zselect4 <- c(zselect4, paste0("g",1:(ng-1)))

    return(zselect4)
  })
  

  ######## Reactive zselect5 user specified tests ########
  zSelect5 <- reactive({
    
    zselect5 <- c("", input$variablez)
    
    if(isLatentCovs()){
      nameslatentz <- c(input$name.etaz1, input$name.etaz2, input$name.etaz3,
                        input$name.etaz4, input$name.etaz5, input$name.etaz6, 
                        input$name.etaz7, input$name.etaz8, input$name.etaz9, 
                        input$name.etaz10)
      nameslatentz <- nameslatentz[1:input$nlatentz]
      zselect5 <- c(nameslatentz, zselect5)
    }
    
    d <- dataInput()
    x <- d[[input$variablex]]
    ng <- length(unique(na.omit(x)))
    
    if(!is.null(input$propscore)){
      zselect5 <- c(zselect5, paste0("logprop",1:(ng-1)))
    }
    if(input$prop.formula != "" & input$propscoreformula){
      zselect5 <- c(zselect5, paste0("logprop",1:(ng-1)))
    }
    
    return(zselect5)
  })
  
      
  
  ######## Reactive measurement model ########
  mm <- reactive({
    if(!input$latenty && !isLatentCovs()){
      return(character())
      
    }else if(input$latenty || isLatentCovs()){
      
      ## determine number of cells
      d <- dataInput()
      ng <- length(unique(na.omit(d[[input$variablex]])))
      nk <- 1
      if(length(input$variablek) != 0){
        for(i in 1:length(input$variablek)){
          tmpvar <- as.factor(d[[input$variablek[i]]])
          nk <- nk*length(levels(tmpvar))
        }        
      }
      ncells <- ng*nk
      
      names <- NULL
      indicators <- NULL
      mmodel <- NULL
      
      if(input$latenty){
        names$etay <- input$name.etay
        indicators$indicatorsy <- input$indicatorsy
        mmodel$mm.etay <- input$mm.etay
      }
      if(input$latentz & input$nlatentz > 0){
        names$etaz1 <- input$name.etaz1
        indicators$indicatorsz1 <- input$indicatorsz1
        mmodel$mm.etaz1 <- input$mm.etaz1
      }
      if(input$latentz & input$nlatentz > 1){
        names$etaz2 <- input$name.etaz2
        indicators$indicatorsz2 <- input$indicatorsz2
        mmodel$mm.etaz2 <- input$mm.etaz2
      }
      if(input$latentz & input$nlatentz > 2){
        names$etaz3 <- input$name.etaz3
        indicators$indicatorsz3 <- input$indicatorsz3
        mmodel$mm.etaz3 <- input$mm.etaz3
      }
      if(input$latentz & input$nlatentz > 3){
        names$etaz4 <- input$name.etaz4
        indicators$indicatorsz4 <- input$indicatorsz4
        mmodel$mm.etaz4 <- input$mm.etaz4
      }
      if(input$latentz & input$nlatentz > 4){
        names$etaz5 <- input$name.etaz5
        indicators$indicatorsz5 <- input$indicatorsz5
        mmodel$mm.etaz5 <- input$mm.etaz5
      }
      if(input$latentz & input$nlatentz > 5){
        names$etaz6 <- input$name.etaz6
        indicators$indicatorsz6 <- input$indicatorsz6
        mmodel$mm.etaz6 <- input$mm.etaz6
      }
      if(input$latentz & input$nlatentz > 6){
        names$etaz7 <- input$name.etaz7
        indicators$indicatorsz7 <- input$indicatorsz7
        mmodel$mm.etaz7 <- input$mm.etaz7
      }
      if(input$latentz & input$nlatentz > 7){
        names$etaz8 <- input$name.etaz8
        indicators$indicatorsz8 <- input$indicatorsz8
        mmodel$mm.etaz8 <- input$mm.etaz8
      }
      if(input$latentz & input$nlatentz > 8){
        names$etaz9 <- input$name.etaz9
        indicators$indicatorsz9 <- input$indicatorsz9
        mmodel$mm.etaz9 <- input$mm.etaz9
      }
      if(input$latentz & input$nlatentz > 9){
        names$etaz10 <- input$name.etaz10
        indicators$indicatorsz10 <- input$indicatorsz10
        mmodel$mm.etaz10 <- input$mm.etaz10
      }
      
      names <- unlist(names)
      mmodel <- unlist(mmodel)
      
      ## switch to default options if custom mmodel is not specified
      if(!input$custommeasmodels){mmodel <- NULL}

      mm <- generateMeasurementModel(
        names=names,
        indicators=indicators,
        ncells=ncells,
        model=mmodel,
        data=d
      )
      
      return(mm)
    }
  })
  
  ###### Reactive dependent variable ###########
  depv <- reactive({
    
    if(input$latenty){
      return(input$name.etay)
      
    }else{
      return(input$variabley)
      
    }
  })  

  ###### Reactive latent covariates ###########
  latentcov <- reactive({
    
    if(isLatentCovs()){
        
      nameslatentcov <- NULL; nameslatentcov$etaz1 <- input$name.etaz1
      if(input$nlatentz > 1){nameslatentcov$etaz2 <- input$name.etaz2}
      if(input$nlatentz > 2){nameslatentcov$etaz3 <- input$name.etaz3}
      if(input$nlatentz > 3){nameslatentcov$etaz4 <- input$name.etaz4}
      if(input$nlatentz > 4){nameslatentcov$etaz5 <- input$name.etaz5}
      if(input$nlatentz > 5){nameslatentcov$etaz6 <- input$name.etaz6}
      if(input$nlatentz > 6){nameslatentcov$etaz7 <- input$name.etaz7}
      if(input$nlatentz > 7){nameslatentcov$etaz8 <- input$name.etaz8}
      if(input$nlatentz > 8){nameslatentcov$etaz9 <- input$name.etaz9}
      if(input$nlatentz > 9){nameslatentcov$etaz10 <- input$name.etaz10}
      
      nameslatentcov <- unlist(nameslatentcov)
      return(nameslatentcov)
      
    }else{
      return(NULL)
      
    }
  })
  
  ###### Reactive subset for aggregated effects ###########
  agg.subset <- reactive({
    
    m1 <- model()
    vnamesk <- m1@input@vnames$k
    vnamesz <- m1@input@vnames$z
    vnamesx <- m1@input@vnames$x
    numberks <- length(vnamesk)
    numberzs <- length(vnamesz)
    covnames <- c(vnamesk,vnamesz,vnamesx)
    
    valuesk <- list()
    valuesz <- list()
    
    if(numberks>0){
      for(i in 1:numberks){
        valuesk <- c(valuesk, input[[paste0('aggvalk',i)]])
      }}
    
    if(numberzs>0){
      for(i in 1:numberzs){
        valuesz <- c(valuesz, input[[paste0('aggvalz',i)]])
      }}
    
    valuesx <- input[['xaggeff']]
    
    newdata <- data.frame(c(valuesk, valuesz, valuesx))
    
    nrowsagg <- input$nrowsagg
    if(is.null(nrowsagg)){nrowsagg <- 10} ## weird error handling
    
    if(nrow(newdata) > 0){
      newdata[newdata == "NA"] <- NA ## input somehow converts NA to "NA"
      names(newdata) <- covnames
      agg.subset <- autoSelectSubset(obj=m1, newdata=newdata, nsub=nrowsagg)
      
    }else{
      agg.subset <- 1:nrow(m1@input@data)
      
    }
    
    return(agg.subset)
    
  })
  
  
  ###### Update Variable Selectors UI ########
  observe({
    inFile <- input$file1
    exdata <- input$exdata
    
    if(is.null(inFile) & exdata=="")
      return(NULL)  
    
    d <- dataInput()
    
    updateSelectInput(session, "variabley", 
                      choices = c("", names(d)))
    updateSelectInput(session, "variablex", 
                      choices = c("", names(d)))
    updateSelectInput(session, "variablek", 
                      choices = c("", names(d)),
                      selected = "")
    updateSelectInput(session, "variablez", 
                      choices = c("", names(d)),
                      selected = "")
    updateSelectInput(session, "propscore", 
                      choices = c("", names(d)),
                      selected = "")
    updateSelectInput(session, "ids", 
                      choices = c("", names(d)),
                      selected = "")
    updateSelectInput(session, "weights", 
                      choices = c("", names(d)),
                      selected = "")
    updateSelectInput(session, "indicatorsy", choices = names(d))
    updateSelectInput(session, "indicatorsz1", choices = names(d))
    updateSelectInput(session, "indicatorsz2", choices = names(d))
    updateSelectInput(session, "indicatorsz3", choices = names(d))
    updateSelectInput(session, "indicatorsz4", choices = names(d))
    updateSelectInput(session, "indicatorsz5", choices = names(d))
    updateSelectInput(session, "indicatorsz6", choices = names(d))
    updateSelectInput(session, "indicatorsz7", choices = names(d))
    updateSelectInput(session, "indicatorsz8", choices = names(d))
    updateSelectInput(session, "indicatorsz9", choices = names(d))
    updateSelectInput(session, "indicatorsz10", choices = names(d))
  })

  

  ###### Update zselect for Plot 2 ########
  observe({
    zsel <- zSelect()
    updateSelectInput(session, "zselect", 
                    choices = zsel)  
  })

  ###### Update zselect for continuous covariate tests ########
  observe({
    zsel <- zSelect5()
    updateSelectInput(session, "subconcov", 
                      choices = zsel)  
  })
  

  ###### Update gxselectce4 for Conditional Effects 4 ########
  observe({
    gxsel <- gxSelect()
    updateSelectInput(session, "gxselectce4", 
                      choices = gxsel)  
  })
  
    
  
  ###### Update gxselect for Plot 3 ########
  observe({
    gxsel <- gxSelect()
    updateSelectInput(session, "gxselect", 
                      choices = gxsel)  
  })
  
  
  ###### Update zselect2 for Plot 3 ########
  observe({
    zsel <- zSelect2()
    updateSelectInput(session, "zselect2", 
                      choices = zsel)  
  })
  
  ###### Update zselect3 for Plot 3 ########
  observe({
    zsel3 <- zSelect3()
    updateSelectInput(session, "zselect3", 
                      choices = zsel3)  
  })
  
  ###### Update variablece4 for Conditional Effects IV ########
  observe({
    
    wselect <- wSelect()
    updateSelectInput(session, "variablece4", 
                      choices = wselect)  
  })
  

  ###### Update gxselect2 for Plot 4 ########
  observe({
    gxsel2 <- gxSelect2()
    updateSelectInput(session, "gxselect2",
                      choices = gxsel2)
  })

  ###### Update zselect4 for Plot 4 ########
  observe({
    zsel4 <- zSelect4()
    updateSelectInput(session, "zselect4",
                      choices = zsel4)
  })
  
  ###### Update Control Group UI ########
  observe({
    inputx <- input$variablex
    
    if(inputx==""){
      return(NULL)        
    }else{      
      d <- dataInput()
      x <- as.factor(d[,inputx])
      
      updateSelectInput(session, "control", choices = levels(x))      
    }
  })  
  
  
  ###### Output Data Table #########
  output$mytable1 = DT::renderDT({
    d <- dataInput()
    if(!is.null(d)){
      d <- format(d, digits=3)
      d <- DT::datatable(d)
    }
    d})
  

  ###### Output Conditional Effects Table #########
  output$helptextcondeffects <- renderPrint({
    if((input$variabley == "" & !input$latenty) || input$variablex == "" ){
           
      cat("Conditional effects are only shown if you have specified the dependent variable and the treatment variable.")
      
    }else{
      
      cat("This datatable shows the values and standard errors of the effect function for given values of the categorical and continuous covariates. Regression factor scores are used for latent covariates.")
    }
  })
  
  
  output$condeffs = DT::renderDT({
    if((input$variabley == "" & !input$latenty) || input$variablex == "" ){
      return(NULL)
    }else{            
      m1 <- model()
      condprint <- format(m1@results@condeffects, digits=3)
      condprint <- DT::datatable(condprint)
      condprint
    }  
  })
  
  
  ###### Output Plot 1 #########
  output$helptextplot1 <- renderPrint({
    if(input$variabley == "" || input$variablex == "" || input$latenty == TRUE){
      
      cat("Plot 1 only works for a manifest dependent variable and a treatment variable.")
      
    }else{
      
      cat("Plot 1 shows a histogram of the dependent variable in each cell.")
    }
  })
    
  output$plot1 <- renderPlot({    
    
    if(input$variabley == "" || input$variablex == "" || input$latenty == TRUE){
      return(NULL)
    }else{
      
      m1 <- model()

      y <- m1@input@data[[input$variabley]]      
      cell <- m1@input@data[["cell"]]
      dp <- na.omit(data.frame(y,cell))
      binwidth <- (range(y, na.rm=TRUE)[2]-range(y, na.rm=TRUE)[1])/30
      
      p <- ggplot2::ggplot(data=dp, ggplot2::aes(x = y))
      p <- p + ggplot2::geom_histogram(binwidth=binwidth)
      p <- p + ggplot2::ggtitle(paste0("Distribution of ", input$variabley, 
                                       " in cells"))
      p <- p + ggplot2::xlab(input$variabley)
      p <- p + ggplot2::facet_wrap( ~ cell)
      p <- p + ggplot2::theme_bw()
      print(p)
    }  
        
  })

  ###### Output Plot 2 #########  
  output$helptextplot2 <- renderPrint({
    if(input$variabley == "" || input$variablex == "" || 
         (is.null(input$variablez) & is.null(input$propscore)) || 
         input$latenty){
      
      cat("Plot 2 only works for a manifest dependent variable, a treatment variable, and at least one continuous covariate.")
      
    }else{
      
      cat("Plot 2 shows the regression of the dependent variable on the selected continuous covariate in each cell.")
    }
  })
  
  
  output$plot2 <- renderPlot({    
    
    if(input$variabley == "" || input$variablex == "" || 
         (is.null(input$variablez) & is.null(input$propscore)) || 
         input$latenty){
      
      return(NULL)
    }else{
      
      m1 <- model()
      
      y <- m1@input@data[[input$variabley]]
      zselected <- m1@input@data[[input$zselect]]
      cell <- m1@input@data[["cell"]]
      
      dp <- data.frame(y,cell,zselected)
      
      p <- ggplot2::ggplot(data=dp, ggplot2::aes(x=zselected, y=y))
      p <- p + ggplot2::geom_point()
      p <- p + ggplot2::ggtitle(paste0("Regression of ", input$variabley, " on ", 
                                       input$zselect, " in cells"))
      p <- p + ggplot2::ylab(input$variabley) + ggplot2::xlab(input$zselect)
      p <- p + ggplot2::facet_wrap( ~ cell)
      p <- p + ggplot2::geom_smooth(method = "lm", formula=y~x)
      p <- p + ggplot2::theme_bw()
      print(p)
      
      
    }
        
  })

  
  ###### Output Plot 3 #########
  output$helptextplot3 <- renderPrint({
    if(input$variabley == "" || input$variablex == "" || 
         (is.null(input$variablez) & is.null(input$variablek) & !input$latentz & 
            is.null(input$propscore))  || 
         input$latenty || input$latentz){
      
      cat("Plot 3 is only shown if a dependent variable, a treatment variable, and at least one covariate is specified.")
      
    }else{
      
      cat("Plot 3 shows the regression of the selected effect function on the selected regressor.")
    }
  })
  
  output$plot3 <- renderPlot({    
    
    if((input$variabley == "" & !input$latenty) || input$variablex == "" || 
         (is.null(input$variablez) & is.null(input$variablek) & !input$latentz & 
            is.null(input$propscore))){
      return(NULL)
    }else{
      
      m1 <- model()
      conditionalEffectsPlot(m1,
                             zsel=input$zselect2, 
                             colour=input$zselect3, 
                             gxsel=input$gxselect, 
                             show.ci=input$show.ci,
                             regression=input$regline,
                             regression.ci=input$show.cir)

    }
    
  })


  
  ###### Output Plot 4 #########
  output$helptextplot4 <- renderPrint({
    if((input$variabley == "" & !input$latenty) || input$variablex == ""){

      cat("Plot 4 is only shown if you have specified the dependent variable and the treatment variable.")

    }else{

      cat("Plot 4 shows the estimated conditional effects in the sample.")
    }
  })

  output$plot4 <- renderPlot({

    if((input$variabley == "" & !input$latenty) || input$variablex == ""){
      return(NULL)
    }else{

      m1 <- model()
      conditionalEffectsPlot(m1,
                             zsel="id",
                             colour=input$zselect4,
                             gxsel=input$gxselect2,
                             show.ci=input$show.ci2)

    }

  })
  
  
  
    
  ###### Output EffectLiteR Summary #########
  output$summary <- renderPrint({
    
    if(input$variabley == "" & input$latenty == FALSE || input$variablex == ""){      
      
      cat("Please specify the outcome variable and the treatment variable")
      
    }else{
      
      m1 <- model()
      m1
    }
  })
  
  ###### Output ELR call #########
  output$elrcall <- renderPrint({
    if(input$variabley == "" & input$latenty == FALSE || input$variablex == ""){    
      cat("Please specify the outcome variable and the treatment variable")
    }else{
      dv <- depv()
      x <- input$variablex
      
      mm <- mm()
      printmm <- "character()"
      if(length(mm) != 0){printmm <- "mm"}
      
      printk <- "NULL"
      if(length(input$variablek) != 0){
        printk <- paste0("c(\"",
                         paste(input$variablek, collapse="\",\""), 
                         "\")")
      }
      
      printfixedcell <- paste0("fixed.cell=\"", "default", "\"")
      if(input$fixed.cell == "stochastic"){printfixedcell <- "fixed.cell=FALSE"}
      if(input$fixed.cell == "fixed"){printfixedcell <- "fixed.cell=TRUE"}
      if(input$fixed.cell == "fixed+e"){printfixedcell <- "fixed.cell=TRUE"}
      
      printfixedz <- paste0("fixed.z=\"", "default", "\"")
      if(input$fixed.cell == "stochastic"){printfixedz <- "fixed.z=FALSE"}
      if(input$fixed.cell == "fixed"){printfixedz <- "fixed.z=FALSE"}
      if(input$fixed.cell == "fixed+e"){printfixedz <- "fixed.z=TRUE"}
      
      
      z <- NULL
      printz <- "NULL"
      if(length(input$variablez) != 0){z <- input$variablez}
      if(isLatentCovs()){z <- c(z,latentcov())}
      if(!is.null(z)){
        printz <- paste0("c(\"",
                         paste(z, collapse="\",\""), 
                         "\")")
      }
      
      propscore <- NULL 
      printpropscore <- "NULL"
      if(length(input$propscore) != 0 & !input$propscoreformula){
        propscore <- input$propscore
        printpropscore <- paste0("c(\"",
                                 paste(propscore, collapse="\",\""), 
                                 "\")")
      }
      if(input$prop.formula != "" & input$propscoreformula){
        printpropscore <- input$prop.formula
      }
      
      interactions <- input$interactions
      
      printids <- "~0"
      if(input$ids != ""){
        printids <- paste0(" ~ ", input$ids)
      }
      
      printweights="NULL"
      if(input$weights != ""){
        printweights <- paste0(" ~ ", input$weights)
        }
      
      homoscedasticity <- input$homoscedasticity
      test.stat <- input$test.stat
      
      printadd <- "character()"
      if(input$add.syntax != ""){printadd <- "add"}
      
      printbootstrap <- ""
      if(input$se == "boot"){
        printbootstrap <- paste0("bootstrap=",input$bootstrap, ", ")}
      
      tmp <- paste0("#### Call for effectLite #### \n\n",
                    "effectLite(",
                    "y=\"", dv, "\", ",
                    "x=\"", x, "\", ",
                    "k=", printk, ", ",
                    "z=", printz, ", ",
                    "data=data, ",
                    "method=\"", input$method, "\", ",
                    "control=\"", input$control, "\", ",
                    "measurement=", printmm, ", ",
                    "missing=\"", input$missing, "\", ",
                    "estimator=\"", input$estimator, "\", ",
                    "se=\"", input$se, "\", ",
                    printbootstrap,
                    printfixedcell, ", ",
                    printfixedz, ", ",
                    "interactions=\"", interactions, "\", ",
                    "homoscedasticity=\"", homoscedasticity, "\", ",
                    "test.stat=\"", test.stat, "\", ",
                    "propscore=", printpropscore, ", ",
                    "ids=", printids, ", ",
                    "weights=", printweights, ", ",
                    "add=", printadd,
                    ")")
      
      cat(tmp)
    }  
  })
  

  ###### Output lavaan call #########
  output$lavcall <- renderPrint({
    if(input$variabley == "" & input$latenty == FALSE || input$variablex == ""){    
      cat("")
    }else{
      m1 <- model()
      cellabel <- paste0("c(\"",
                         paste(m1@input@vlevels$cell, collapse="\",\""), 
                         "\")")
      printbootstrap <- ""
      if(input$se == "boot"){
        printbootstrap <- paste0("bootstrap=",m1@input@bootstrap, ", ")}
      
      tmp <- paste0("#### Call for lavaan::sem #### \n\n",
                  "sem(", "model=model, ",
                  "group=\"", "cell", "\", ", 
                  "missing=\"", m1@input@missing, "\", ",
                  "se=\"", m1@input@se, "\", ", 
                  printbootstrap,
                  "group.label=", cellabel, ", ",
                  "data=data, ", 
                  "fixed.x=", m1@input@fixed.z, ", ",
                  "group.w.free=", !m1@input@fixed.cell, ", ",
                  "mimic=\"", "lavaan", "\") ")
      cat(tmp, "\n")  
    }  
  })
  
  
      
  ###### Output Lavaan Syntax #########
  output$lavsyntax <- renderPrint({
    if(input$variabley == "" & input$latenty == FALSE || input$variablex == ""){    
      cat("")
    }else{
      m1 <- model()
      cat(m1@syntax@model)  
    }  
  })
  

  ###### Output Lavaan Results #########
  output$lavresults <- renderPrint({      
    
    if(input$variabley == "" & input$latenty == FALSE || input$variablex == ""){            
      
      cat("Please specify the outcome variable and the treatment variable")
      
    }else{
          
      m1 <- model()
      
      if(input$method == "sem"){
        summary(m1@results@lavresults, fit.measures=TRUE)  
        
      }else if(input$method == "lm"){
        summary(m1@results@lmresults)  
      }
      
      ## maybe there was a reason I set fit.measures=FALSE in prior versions...
    }  
  })

  ###### Output User Specified Effects #########
  output$covtests <- renderPrint({      
    
    if(input$variabley == "" & input$latenty == FALSE || input$variablex == ""){            
      
      cat("Please specify the outcome variable and the treatment variable")
      
    }else if(length(input$subconcov) == 0){
      
      cat("No subset of continuous covariates specified")
      
    }else{
      
      m1 <- model()
      covariatetests <- EffectLiteR:::elrTestCausalityConditions(m1, input$subconcov)
      print(covariatetests, digits=3, print.gap=3)
      
    }  
  })
  
  
  
  ###### Output User Specified Effects #########
  output$addeffects <- renderPrint({      
    
    if(input$variabley == "" & input$latenty == FALSE || input$variablex == ""){            
      
      cat("Please specify the outcome variable and the treatment variable")
      
    }else if(input$add.syntax == ""){
      
      cat("No user-defined parameters specified")
      
    }else{
      
      m1 <- model()
      AdditionalEffects <- m1@results@AdditionalEffects
      print(AdditionalEffects, digits=3, print.gap=3)
      
    }  
  })
  
    
  
  ###### Output User Specified Wald Test #########
  output$waldtest <- renderPrint({      
    
    if(input$variabley == "" & input$latenty == FALSE || input$variablex == ""){            
      
      cat("Please specify the outcome variable and the treatment variable")
      
    }else if(input$add.syntax.wald == ""){
      
      cat("No user-defined Wald test specified")
      
    }else{
      
      m1 <- model()
      con <- input$add.syntax.wald
      wtest <- data.frame(lavTestWald(m1@results@lavresults, con)[1:3])  
      row.names(wtest) <- "User-Specified Wald Test"  
      names(wtest) <- c("Wald Chi-Square", "df", "p-value")
      print(wtest, digits=3, print.gap=3)

    }  
  })
  

  ###### Output User Specified Informative Hypothesis Test #########
  output$iht <- renderPrint({      
    
    if(input$variabley == "" & input$latenty == FALSE || input$variablex == ""){            
      
      cat("Please specify the outcome variable and the treatment variable")
      
    }else if(input$add.syntax.iht == ""){
      
      cat("No user-defined informative hypothesis test specified")
      
    }else{
      
      m1 <- model()
      con <- input$add.syntax.iht
      test <- input$iht.test.stat
      ihtest <- data.frame(effectLite_iht(object=m1, 
                                          constraints=con, 
                                          test=test))  
      row.names(ihtest) <- "User-Specified Informative Hypothesis Test"  
      print(ihtest, digits=3, print.gap=3)
      
    }  
  })
  
    
  
  ###### Download Data (Conditional Effects Table) #######
  output$downloadConditionalEffects <- downloadHandler(
    filename = function() {
      paste('ELR-ConditionalEffects-', Sys.Date(), '.txt', sep='')
    },
    content = function(con) {
      m1 <- model()
      write.table(m1@results@condeffects, con, row.names=F, col.names=T, 
                  quote=F)
    }
  )
  
  ###### Download (transformed) Input Data #######
  output$downloadLavData <- downloadHandler(
    filename = function() {
      paste('ELR-data-', Sys.Date(), '.txt', sep='')
    },
    content = function(con) {
      m1 <- model()
      write.table(m1@input@data, con, row.names=F, col.names=T, 
                  quote=F)
    }
  )
  
  ##### Conditional effects II User Interface ######
  output$ui <- renderUI({
    
    m1 <- model()
    condeffects <- m1@results@condeffects
    vnamesk <- m1@input@vnames$k
    vlevelsk <- m1@input@vlevels$levels.k.original
    vnamesz <- m1@input@vnames$z
    numberks <- length(vnamesk)
    numberzs <- length(vnamesz)
    covnames <- c(vnamesk,vnamesz)
    uilist <- vector("list", length(covnames))
    
    if(numberks==0 & numberzs==0){return(NULL)}
    
    if(numberks>0){
    for(i in 1:numberks){
      uilist[[i]] <- selectInput(inputId = paste0('valk',i), 
                                 label = vnamesk[i], 
                                 choices = vlevelsk[[i]],
                                 width='90%')
    }}
    
    if(numberzs>0){
    for(i in 1:numberzs){
      uilist[[numberks+i]] <- numericInput(inputId = paste0('valz',i),
                label = vnamesz[i],
                value = round(mean(condeffects[[vnamesz[i]]], na.rm=T),3),
                width='90%')
    }}
    
    uilist
    
  })
  
  
  #### Help text conditional effects 2 #####
  output$helptextcondeffects2 <- renderPrint({
    if((input$variabley == "" & !input$latenty) || input$variablex == "" ){
      
      cat("Conditional effects are only available if you have specified the dependent variable and the treatment variable.")
      
    }else{
      
      cat("Conditional effects for user specified values of categorical and continuous covariates.")
    }
  })
  
  
  #### output conditional effects 2 #####
  output$outputcondeffect2 <- renderPrint({
    
    m1 <- model()
    vnamesk <- m1@input@vnames$k
    vnamesz <- m1@input@vnames$z
    numberks <- length(vnamesk)
    numberzs <- length(vnamesz)
    covnames <- c(vnamesk,vnamesz)
    
    valuesk <- list()
    valuesz <- list()

    if(numberks>0){
      for(i in 1:numberks){
        valuesk <- c(valuesk, input[[paste0('valk',i)]])
      }}
    
    if(numberzs>0){
      for(i in 1:numberzs){
        valuesz <- c(valuesz, input[[paste0('valz',i)]])
    }}
    
    
    newdata <- data.frame(c(valuesk, valuesz))
    try({names(newdata) <- covnames}, silent=TRUE)
    
    try({indeff <- round(elrPredict(m1, newdata),3)}, silent=TRUE)
    try({print(indeff, row.names=F, print.gap=3)}, silent=TRUE)
    
  })
  

  #### output descriptive stats for conditional effects 2 #####
  output$descriptivestats <- renderPrint({
    
    m1 <- model()
    vnamesz <- m1@input@vnames$z
    numberzs <- length(vnamesz)

    if(numberzs==0){
      print("No continuous covariates in the model")
      
    }else if(numberzs>0){
      print(EffectLiteR:::elr_compute_descriptives_z(m1))
      
    }
    
  })
  
  
  #### Help text conditional effects 4 #####
  output$helptextcondeff4 <- renderPrint({
    if((input$variabley == "" & !input$latenty) || input$variablex == "" ){
      
      cat("Conditional effects are only available if you have specified the dependent variable and the treatment variable.")
      
    }else{
      
      cat("Computes the regression of the selected effect function gx on the variable W. If you want standard errors for the regression coefficients, please specify the number of bootstrap draws. This may take a while to compute.")
    }
  })
  
    
  
  ##### Aggregated effects User Interface Covariates ######
  output$uiaggeff <- renderUI({
    
    m1 <- model()
    condeffects <- m1@results@condeffects
    vnamesk <- m1@input@vnames$k
    vlevelsk <- m1@input@vlevels$levels.k.original
    vnamesz <- m1@input@vnames$z
    numberks <- length(vnamesk)
    numberzs <- length(vnamesz)
    covnames <- c(vnamesk,vnamesz)
    uilist <- vector("list", length(covnames))
    
    if(numberks==0 & numberzs==0){return(NULL)}
    
    if(numberks>0){
      for(i in 1:numberks){
        uilist[[i]] <- selectInput(inputId = paste0('aggvalk',i), 
                                   label = vnamesk[i], 
                                   choices = c(NA,vlevelsk[[i]]),
                                   width='90%')
      }}
    
    if(numberzs>0){
      for(i in 1:numberzs){
        uilist[[numberks+i]] <- numericInput(inputId = paste0('aggvalz',i),
                                             label = vnamesz[i],
                                             value = NA,
                                             width='90%')
      }}
    
    uilist
    
  })
  
  #### Aggregated effects User Interface Treatment ######
  output$uiaggeff2 <- renderUI({
    
    m1 <- model()
    vnamesx <- m1@input@vnames$x
    vlevelsx <- m1@input@vlevels$levels.x.original
    uilist <- vector("list", length=1)
    
    uilist[[1]] <- selectInput(inputId = "xaggeff", 
                               label = vnamesx, 
                               choices = c(NA,vlevelsx),
                               width='90%')
    
    uilist
    
  })

  #### Aggregated effects User Interface Number Rows ######
  output$uiaggeff3 <- renderUI({
    
    
    m1 <- model()
    vnamesz <- m1@input@vnames$z
    numberzs <- length(vnamesz)

    valuesz <- list()
    if(numberzs>0){
      for(i in 1:numberzs){
        valuesz <- c(valuesz, input[[paste0('aggvalz',i)]])
      }}
    
    if(all(is.na(valuesz))){
      
      return(NULL)
      
    }else{
      
      uilist <- vector("list", length=1)
      
      uilist[[1]] <- numericInput("nrowsagg", 
                                  label="Number of rows",
                                  value=10,
                                  min = 0, 
                                  max = 5, 
                                  width='30%')
      
      return(uilist)
    }
    
  })
  
    
  
  #### Help text conditional effects 2 #####
  output$helptextaggeff <- renderPrint({
    if((input$variabley == "" & !input$latenty) || input$variablex == "" ){
      
      cat("Aggregated effects are only available if you have specified the dependent variable and the treatment variable.")
      
    }else{
      
      cat("Aggregated effects for user specified values of categorical and continuous covariates.")
    }
  })
  
  
  #### output aggregated effects #####
  output$outputaggeff <- renderPrint({
    
    m1 <- model()
    agg.subset <- agg.subset()
      
    try({aggeff <- round(computeAggregatedEffects(m1, agg.subset),3)}, silent=TRUE)
    try({print(aggeff, row.names=F, print.gap=3)}, silent=TRUE)
    
  })
  
  #### output aggregated effects table ####
  output$aggeffstable = DT::renderDT({
    if((input$variabley == "" & !input$latenty) || input$variablex == "" ){
      return(NULL)
    }else{            
      m1 <- model()
      idx <- agg.subset()
      condprint <- format(m1@results@condeffects[idx,], digits=3)
      condprint <- DT::datatable(condprint)
      condprint
    }  
  })

  
  #### output conditional effects IV #####
  output$outputcondeff4 <- renderPrint({
    
    m1 <- model()
    d <- dataInput()
    nboot <- input$bootstrapce4
    gx <- input$gxselectce4
    w <- input$variablece4
    
    if(w == ""){return(NULL)}
    
    formula <- paste0(gx, " ~ ", w)
    formula <- as.formula(formula)
    
    try({aggeff <- round(EffectLiteR:::elrCondeffectsBoot(d, m1, formula, nboot),3)}, silent=FALSE)
    try({print(aggeff, row.names=T, print.gap=3)}, silent=FALSE)
    
  })
  
  #### output aggregated effects table ####
  output$aggeffstable = DT::renderDT({
    if((input$variabley == "" & !input$latenty) || input$variablex == "" ){
      return(NULL)
    }else{            
      m1 <- model()
      idx <- agg.subset()
      condprint <- format(m1@results@condeffects[idx,], digits=3)
      condprint <- DT::datatable(condprint)
      condprint
    }  
  })    
    
  
})