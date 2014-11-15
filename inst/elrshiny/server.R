
require(ggplot2)
require(foreign)
# require(semPlot)

shinyServer(function(input, output, session) {
  
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
      }      
    }
    
    if(!is.null(inFile)){
      
     # Determine document format;
     ptn <- "\\.[[:alnum:]]{1,5}$"
     suf <- tolower(regmatches(inFile$name, regexpr(ptn, inFile$name)))
      
      if(suf == ".csv"){
        return(read.csv(inFile$datapath))  
      }else if(suf == ".txt"){
        return(read.table(inFile$datapath))    
      }else if(suf == ".sav"){
        return(read.spss(inFile$datapath, to.data.frame=TRUE,
                         use.value.labels=input$vallabels))
      }else if(suf == ".xpt"){
        return(read.xport(inFile$datapath))
      }  
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
    fixed.cell <- FALSE; if(input$fixed.cell == "fixed"){fixed.cell <- TRUE}
    
    z <- NULL; if(length(input$variablez) != 0){z <- input$variablez}
    if(input$latentz & input$nlatentz > 0){z <- c(z,latentcov())}
    
    
    tryCatch(
      effectLite(y=dv, 
                 x=x,
                 k=k,
                 z=z,
                 data=d,
                 control=input$control,
                 measurement=mm,
                 missing=input$missing,
                 se=input$se,
                 bootstrap=input$bootstrap,
                 fixed.cell=fixed.cell)
    )  
  })

  
  ######## Reactive zselect for Plot 2 ########
  zSelect <- reactive({
    zselect <- input$variablez
    return(zselect)
  })

  ######## Reactive gxselect for Plot 3 ########
  gxSelect <- reactive({
    
    d <- dataInput()
    x <- d[[input$variablex]]    
    ng <- length(unique(x))
    res <- paste0("g",2:ng-1)
    
    return(res)
  })
  
  
  ######## Reactive zselect2 for Plot 3 ########
  zSelect2 <- reactive({
    zselect <- input$variablez
    return(zselect)
  })
  
  
  ######## Reactive measurement model ########
  mm <- reactive({
    if(!input$latenty & !input$latentz){
      return(character())
      
    }else if(input$latenty | input$nlatentz > 0){
      
      d <- dataInput()
      ng <- length(unique(d[[input$variablex]]))
      nk <- 1
      if(length(input$variablek) != 0){
        for(i in 1:length(input$variablek)){
          tmpvar <- as.factor(d[[input$variablek[i]]])
          nk <- nk*length(levels(tmpvar))
        }        
      }
      
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
      
      names <- unlist(names)
      mmodel <- unlist(mmodel)
      
      mm <- generateMeasurementModel(
        names=names,
        indicators=indicators,
        ncells=ng*nk,
        model=mmodel
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
    
    if(input$latentz == TRUE & input$nlatentz > 0){
        
      nameslatentcov <- NULL; nameslatentcov$etaz1 <- input$name.etaz1
      if(input$nlatentz > 1){nameslatentcov$etaz2 <- input$name.etaz2}
      if(input$nlatentz > 2){nameslatentcov$etaz3 <- input$name.etaz3}
      if(input$nlatentz > 3){nameslatentcov$etaz4 <- input$name.etaz4}
      if(input$nlatentz > 4){nameslatentcov$etaz5 <- input$name.etaz5}
      
      nameslatentcov <- unlist(nameslatentcov)
      return(nameslatentcov)
      
    }else{
      return(NULL)
      
    }
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
    updateSelectInput(session, "indicatorsy", choices = names(d))
    updateSelectInput(session, "indicatorsz1", choices = names(d))
    updateSelectInput(session, "indicatorsz2", choices = names(d))
    updateSelectInput(session, "indicatorsz3", choices = names(d))
    updateSelectInput(session, "indicatorsz4", choices = names(d))
    updateSelectInput(session, "indicatorsz5", choices = names(d))
  })


  ###### Update zselect for Plot 2 ########
  observe({
    zsel <- zSelect()
    updateSelectInput(session, "zselect", 
                    choices = zsel)  
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
  output$mytable1 = renderDataTable({ 
    d <- dataInput()
    d
  })

  ###### Output Conditional Effects Table #########
  output$helptextcondeffects <- renderPrint({
    if(input$variabley == "" || input$variablex == "" || 
         input$latenty || input$latentz){
      
      cat("Conditional effects are only shown if you have specified the dependent variable and the treatment variable. Conditional effects are not available if there are latent variables in the analysis.")
      
    }else{
      
      cat("This datatable shows the values of the effect function for given values of the categorical and continuous covariates.")
    }
  })
    
  output$condeffs = renderDataTable({
    if(input$variabley == "" || input$variablex == "" || 
         input$latenty || input$latentz){
      return(NULL)
    }else{            
      m1 <- model()
      m1@results@condeffects
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
      dp <- data.frame(y,cell)
      binwidth <- (range(y)[2]-range(y)[1])/30

      p <- qplot(y, data=dp, geom="histogram",
                 binwidth=binwidth,
                 xlab=input$variabley,
                 main=paste0("Distribution of ", input$variabley, " in cells"))
      p <- p + facet_wrap( ~ cell)
      p <- p + theme_bw()
      print(p)
    }  
        
  })

  ###### Output Plot 2 #########  
  output$helptextplot2 <- renderPrint({
    if(input$variabley == "" || input$variablex == "" || 
         is.null(input$variablez) || input$latenty){
      
      cat("Plot 2 only works for a manifest dependent variable, a treatment variable, and at least one continuous covariate.")
      
    }else{
      
      cat("Plot 2 shows the regression of the dependent variable on the selected continuous covariate in each cell.")
    }
  })
  
  
  output$plot2 <- renderPlot({    
    
    if(input$variabley == "" || input$variablex == "" || 
         is.null(input$variablez) || input$latenty){
      
      return(NULL)
    }else{
      
      m1 <- model()
      
      y <- m1@input@data[[input$variabley]]
      zselected <- m1@input@data[[input$zselect]]
      cell <- m1@input@data[["cell"]]
      
      dp <- data.frame(y,cell,zselected)
      
      p <- qplot(y=y, x=zselected, data=dp, 
                 ylab=input$variabley,
                 xlab=input$zselect,                 
                 main=paste0("Regression of ", input$variabley, " on ", 
                             input$zselect, " in cells"))
      p <- p + facet_wrap( ~ cell)
      p <- p + geom_smooth(method = "lm")
      p <- p + theme_bw()
      print(p)                  
    }
        
  })

  
  ###### Output Plot 3 #########
  output$helptextplot3 <- renderPrint({
    if(input$variabley == "" || input$variablex == "" || 
         is.null(input$variablez) || input$latenty ||
         input$latentz){
      
      cat("Plot 3 only works for a manifest dependent variable, a treatment variable, at least one continuous covariate, and no latent covariates.")
      
    }else{
      
      cat("Plot 3 shows the regression of the selected effect function on the selected continuous covariate.")
    }
  })
  
  output$plot3 <- renderPlot({    
    
    if(input$variabley == "" || input$variablex == "" || 
         is.null(input$variablez) || input$latenty ||
         input$latentz){
      return(NULL)
    }else{
      
      m1 <- model()
      condeffects <- m1@results@condeffects
      yselected <- condeffects[[input$gxselect]]    
      zselected <- condeffects[[input$zselect2]]    
      
      g1label <- "(K,Z)"
      if(length(input$variablek) == 0){g1label <- "(Z)"}
      
      p <- qplot(y=yselected, x=zselected, data=condeffects, 
                 ylab=paste0(input$gxselect,g1label),
                 xlab=input$zselect2,                 
                 main=paste0("Estimated regression of ",
                             paste0(input$gxselect,g1label), " on ", 
                             input$zselect2))
      p <- p + geom_smooth(method="loess")
      p <- p + theme_bw()
      
      print(p)
    }
    
  })

#   ###### Output Path Diagram #########
#   output$plotpd <- renderPlot({    
#     
#     if(input$variabley == "" || input$variablex == ""){
#       return(NULL)
#     }else{
#       
#       m1 <- model()
#       lavresults <- m1@results@lavresults
#       semPaths(lavresults)
#     }  
#     
#   })
  
  
  ###### Output EffectLiteR Summary #########
  output$summary <- renderPrint({
    
    if(input$variabley == "" & input$latenty == FALSE || input$variablex == ""){      
      
      cat("Please specify the outcome variable and the treatment variable")
      
    }else{
      
      m1 <- model()
      m1
    }
  })
    
  ###### Output Lavaan Syntax #########
  output$lavsyntax <- renderPrint({      
    
    if(input$variabley == "" & input$latenty == FALSE || input$variablex == ""){            
      
      cat("Please specify the outcome variable and the treatment variable")
      
    }else{
          
      m1 <- model()
      cat(m1@lavaansyntax@model)  
    }  
  })

  ###### Output Lavaan Results #########
  output$lavresults <- renderPrint({      
    
    if(input$variabley == "" & input$latenty == FALSE || input$variablex == ""){            
      
      cat("Please specify the outcome variable and the treatment variable")
      
    }else{
          
      m1 <- model()
      summary(m1@results@lavresults, fit.measures=TRUE)  
      ## maybe there was a reason I set fit.measures=FALSE in prior versions...
    }  
  })

})