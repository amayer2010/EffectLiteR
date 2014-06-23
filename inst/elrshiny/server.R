
require(ggplot2)
require(Hmisc)
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
      if(exdata=="none"){
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
        return(spss.get(inFile$datapath))
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
    k <- NULL; if(input$variablek[1]!="None"){k <- input$variablek}
    fixed.cell <- FALSE; if(input$fixed.cell == "fixed"){fixed.cell <- TRUE}
    
    z <- NULL; if(input$variablez[1]!="None"){z <- input$variablez}
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
      if(input$variablek != "None"){
        for(i in 1:length(input$variablek)){
          nk <- nk*length(unique(d[[input$variablek[i]]]))
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
    
    if(is.null(inFile) & exdata=="none")
      return(NULL)  
    
    d <- dataInput()
    
    updateSelectInput(session, "variabley", 
                      choices = c("", names(d)))
    updateSelectInput(session, "variablex", 
                      choices = c("", names(d)))
    updateSelectInput(session, "variablek", 
                      choices = c("None", names(d)),
                      selected = "None")
    updateSelectInput(session, "variablez", 
                      choices = c("None", names(d)),
                      selected = "None")
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
                 main=paste0("Distribution of ", input$variabley, " in Cells"))
      p <- p + facet_wrap( ~ cell)
      print(p)
    }  
        
  })

  ###### Output Plot 2 #########
  output$plot2 <- renderPlot({    
    
    if(input$variabley == "" || input$variablex == "" || 
         input$variablez == "None" || input$latenty){
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
                             input$zselect, " in Cells"))
      p <- p + facet_wrap( ~ cell)
      p <- p + geom_smooth(method = "lm")
      print(p)                  
    }
        
  })

  
  ###### Output Plot 3 #########
  output$plot3 <- renderPlot({    
    
    if(input$variabley == "" || input$variablex == "" || 
         input$variablez == "None" || input$latenty ||
         input$latentz){
      return(NULL)
    }else{
      
      m1 <- model()
      condeffects <- m1@results@condeffects
      yselected <- condeffects[[input$gxselect]]    
      zselected <- condeffects[[input$zselect2]]    
      
      p <- qplot(y=yselected, x=zselected, data=condeffects, 
                 ylab=paste0(input$gxselect,"(K,Z)"),
                 xlab=input$zselect2,                 
                 main=paste0("Regression of ",
                             paste0(input$gxselect,"(K,Z)"), " on ", 
                             input$zselect2))
      p <- p + geom_smooth(method="loess")
      print(p)                  
      
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
      summary(m1@results@lavresults, fit.measures=FALSE)  
    }  
  })

})