
# library(EffectLiteRlavaan)
require(ggplot2)
require(Hmisc)

shinyServer(function(input, output, session) {
  
  ######## Reactive Data Input ########
  dataInput <- reactive({
    inFile <- input$file1
    exdata <- input$exdata
    
    if(is.null(inFile) & exdata=="none"){
      return(NULL)
    }else if(is.null(inFile) & exdata=="nonortho"){
      return(nonortho)
    }else if(is.null(inFile) & exdata=="example01"){
      return(example01)
    }else if(!is.null(inFile))
      return(spss.get(inFile$datapath))    
  })
  
  ###### Reactive Run Model #########
  model <- reactive({
    
    d <- dataInput()
    
    if(input$variablek=="None" & input$variablez=="None"){
      effectLite(y=input$variabley, 
                 x=input$variablex,
                 k=NULL,
                 z=NULL,
                 data=d,
                 control=input$control,
                 missing=input$missing,
                 fixed.cell=input$fixed.cell)      
    }else if(input$variablek=="None" & input$variablez!="None"){
      effectLite(y=input$variabley, 
                 x=input$variablex, 
                 k=NULL,
                 z=input$variablez,
                 data=d,
                 control=input$control,
                 missing=input$missing,
                 fixed.cell=input$fixed.cell)            
    }else if(input$variablek!="None" & input$variablez=="None"){
      effectLite(y=input$variabley, 
                 x=input$variablex, 
                 k=input$variablek,
                 z=NULL,
                 data=d,
                 control=input$control,
                 missing=input$missing,
                 fixed.cell=input$fixed.cell)            
    }else{
      effectLite(y=input$variabley, 
                 x=input$variablex, 
                 k=input$variablek, 
                 z=input$variablez, 
                 data=d,
                 control=input$control,
                 missing=input$missing,
                 fixed.cell=input$fixed.cell)
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

  ###### Output Plot 1 #########
  output$plot1 <- renderPlot({    

    m1 <- model()

    y <- m1@input@data[[input$variabley]]
    cell <- m1@input@data[["cell"]]
    dp <- data.frame(y,cell)

    p <- qplot(y, data=dp, geom="histogram", main="Distribution of Y in Cells")
    p <- p + facet_wrap( ~ cell)
    print(p)
        
  })

  ###### Output Plot 2 #########
  output$plot2 <- renderPlot({    
    
    z <- input$variabley
    if(z == ""){
      return(NULL)
    }else{
      
      m1 <- model()      
      y <- m1@input@data[[input$variabley]]
      cell <- m1@input@data[["cell"]]
      
      z <- m1@input@vnames$z
      zselected <- m1@input@data[[z[1]]] ## TODO: Selects only first Z
      dp <- data.frame(y,cell,zselected)
      
      p <- qplot(y=y, x=zselected, data=dp, main="Regression of Y on Z in Cells")
      p <- p + facet_wrap( ~ cell)
      p <- p + geom_smooth(method = "lm")
      print(p)
      
            
    }
        
  })
  
  
  ###### Output EffectLiteR Summary #########
  output$summary <- renderPrint({        
    m1 <- model()
    m1    
  })
    
  ###### Output Lavaan Syntax #########
  output$lavsyntax <- renderPrint({      
    m1 <- model()
    cat(m1@lavaansyntax@model)  
  })

  ###### Output Lavaan Results #########
  output$lavresults <- renderPrint({      
    m1 <- model()
    summary(m1@results@lavresults)  
  })

})