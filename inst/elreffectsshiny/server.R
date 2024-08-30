
options(shiny.maxRequestSize=100*1024^2)

shinyServer(function(input, output, session) {
  
  ## close app when browser tab is closed
  session$onSessionEnded(function() { 
    stopApp() 
  })
  
  ######## Reactive Data Input ########
  dataInput <- reactive({
    
    inFile <- input$file1
    exdata <- input$exdata
    
    if(is.null(inFile)){      
      if(exdata==""){
        return(NULL)        
      }else if(exdata=="example01"){
        return(example01)  
      }else if(exdata=="elrdata_logreg"){
        return(elrdata_logreg)  
      }else if(exdata=="elrdata_kieferetal2024"){
        return(elrdata_kieferetal2024)  
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
    
    ## arguments for glm()
    d <- dataInput()

    y <- input$variabley
    x <- input$variablex
    if(!is.factor(d[,x])){d[,x] <- as.factor(d[,x])}
    ## currently only categorical treatment in shiny (can be changed)
    
    k <- NULL
    if(length(input$variablek) != 0){
      k <- input$variablek
      for(i in 1:length(k)){
        ki <- k[i]
        if(!is.factor(d[,ki])){d[,ki] <- as.factor(d[,ki])}
      }
    }

    z <- NULL
    if(length(input$variablez) != 0){
      z <- input$variablez
      for(i in 1:length(z)){
        zi <- z[i]
        if(!is.numeric(d[,zi])){d[,zi] <- as.numeric(d[,zi])}
      }
    }

    if(input$ownformula){
      form <- as.formula(input$formula)
      
    }else{
      form <- as.formula(paste0(y, " ~ ", paste0(c(x,k,z), collapse="*")))
    }
    
    family <- input$family

    glm(formula=form, data=d, family=family)
    
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
    updateSelectInput(session, "subsetvar", 
                      choices = c("", names(d)),
                      selected = "")
  })

  ###### Update Treamtent and Control Group UI ########
  observe({
    inputx <- input$variablex
    
    if(inputx==""){
      return(NULL)        
    }else{      
      d <- dataInput()
      x <- as.factor(d[,inputx])
      
      updateSelectInput(session, "control", choices = levels(x), 
                        selected=levels(x)[1])
      updateSelectInput(session, "treat", choices = levels(x), 
                        selected=levels(x)[2])
    }
  })  
  
  
  ###### Update Value for Subsetting Variable ########
  observe({
    inputsubsetvar <- input$subsetvar
    
    if(inputsubsetvar==""){
      return(NULL)        
    }else{      
      d <- dataInput()
      var <- d[,inputsubsetvar]
      
      if(is.factor(var)){
        vals <- levels(var)
      }else{
        vals <- unique(var)
      }
      
      updateSelectInput(session, "valsubset", choices = vals, 
                        selected=vals[1])
    }
  })  
  
  ##### Conditional effects II User Interface ######
  output$ui <- renderUI({
    
    d <- dataInput()
    m1 <- model()
    
    vnamesx <- input$variablex
    vnamesz <- input$variablez

    uilist <- vector("list", length=2)
    
    if(vnamesz==""){
      uilist[[1]] <- selectInput(inputId = "valx", 
                                 label = vnamesx, 
                                 choices = levels(factor(d[,input$variablex])),
                                 width='90%')
    }

    if(vnamesx==""){
      uilist[[2]] <- numericInput(inputId = "valz",
                                  label = vnamesz,
                                  value = round(mean(d[,vnamesz], na.rm=T),3),
                                  width='90%')
    }
    
    uilist
    
  })
  

  
  ###### Output Data Table #########  
  output$mytable1 = DT::renderDT({
    d <- dataInput()
    if(!is.null(d)){
      d <- format(d, digits=3)
      d <- DT::datatable(d)
    }
    d})

  # ###### Output Regression Equation 1 #########
  # output$regequation <- renderPrint({      
  #   
  #   d <- dataInput()
  #   m1 <- model()
  #   
  #   y <- input$variabley
  #   x <- input$variablex
  #   z <- input$variablez
  #   coefs <- round(coef(m1),2)
  #   
  #   if(x==""){
  #     res <- paste0("E(", input$variabley, "|", input$variablez, ") = ",
  #                   paste0(coefs, "*", names(coefs), collapse=" + "))
  #     
  #   }
  #   
  #   if(z==""){
  #     res <- paste0("E(", input$variabley, "|", input$variablex, ") = ",
  #                   paste0(coefs, "*", names(coefs), collapse=" + "))
  #     
  #   }
  #   
  #   cat(res)
  #   
  # })
  

  ###### Output glm Results #########
  output$lmresults <- renderPrint({      

      m1 <- model()
      summary(m1)

  })

  
  ###### help elrEffects #########
  output$helpelreffects <- renderPrint({      
    
    y <- input$variabley
    x <- input$variablex
    k <- input$variablek
    z <- input$variablez
    
    kz <- NULL
    if(length(k) == 0 & length(z) != 0){kz <- ",Z"}
    if(length(k) != 0 & length(z) == 0){kz <- ",K"}
    if(length(k) != 0 & length(z) != 0){kz <- ",K,Z"}
    
    from <- input$control
    to <- input$treat
    type <- input$type
    
    ave <- "Average Effect: "
    eyx1kz <- paste0("E(",y,"|",x,"=",to,kz,")")
    eyx0kz <- paste0("E(",y,"|",x,"=",from,kz,")")
    
    res <- NULL
    if(type=="Average Treatment Effect"){
      res <- paste0(ave,"E[", eyx1kz, " - ", eyx0kz, "]", collapse=" ")
      
    }else if(type=="Simple Ratio of Averages"){
      res <- paste0(ave,"E[", eyx1kz, "] / E[", eyx0kz, "]", collapse=" ")
      
    }else if(type=="Odds Ratio of Averages"){
      res <- paste0(ave,"(E[", eyx1kz, "] / (1 - E[", eyx1kz, "]))", " : ",
                    "(E[", eyx0kz, "] / (1 - E[", eyx0kz, "]))", collapse=" ")
      
    }else if(type=="Average of Simple Ratios"){
      res <- paste0(ave,"E[", eyx1kz, " / ", eyx0kz, "]", collapse=" ")
    
    }else if(type=="Average of Odds Ratios"){
      res <- paste0(ave,"E[(", eyx1kz, " / (1-", eyx1kz, "))", " : (",
                    eyx0kz, " / (1-", eyx0kz, ")",")]", collapse=" ")
    }  
                  
    cat(res)
    
  })
  
  
  ###### elrEffects Output #########
  output$elreffects <- renderPrint({      
    
    m1 <- model()
    
    x <- input$variablex
    from <- input$control
    to <- input$treat
    type <- input$type
    
    res <- elrEffects(m1, x=x, from=from, to=to, type=type)  
    print(res)
    
  })
  

  
  ###### help elrEffectsCond #########
  output$helpelreffectscond <- renderPrint({      
    
    if(input$subsetvar == ""){
      cat("")
      
    }else{
    
    y <- input$variabley
    x <- input$variablex
    k <- input$variablek
    z <- input$variablez
    
    kz <- NULL
    if(length(k) == 0 & length(z) != 0){kz <- ",Z"}
    if(length(k) != 0 & length(z) == 0){kz <- ",K"}
    if(length(k) != 0 & length(z) != 0){kz <- ",K,Z"}
    
    from <- input$control
    to <- input$treat
    type <- input$type
    condvar <- input$subsetvar
    valcondvar <- input$valsubset
    
    # eyx1kz <- paste0("E(",y,"|",x,"=",to,kz,")")
    # eyx0kz <- paste0("E(",y,"|",x,"=",from,kz,")")
    # cond <- paste0(" | ", condvar, "=", valcondvar, collapse="")
    
    ave <- "Conditional Effect for "
    cond <- paste0(condvar, "=", valcondvar, collapse="")
    res <- paste0(ave, cond)
    
    # res <- NULL
    # if(type=="Average Treatment Effect"){
    #   res <- paste0(ave,"E[", eyx1kz, " - ", eyx0kz, cond, "]", collapse=" ")
    #   
    # }else if(type=="Average of Simple Ratios"){
    #   res <- paste0(ave,"E[", eyx1kz, " / ", eyx0kz, cond, "]", collapse=" ")
    #   
    # }else if(type=="Average of Odds Ratios"){
    #   res <- paste0(ave,"E[", eyx1kz, " / (1-", eyx1kz, ")", " : ",
    #                 eyx0kz, " / (1-", eyx0kz, ")", cond, "]", collapse=" ")
    # }  
    
    cat(res)
    }
    
  })
  
  
  ###### elrEffectsCond Output #########
  output$elreffectscond <- renderPrint({
    
    if(input$subsetvar == ""){
      cat("")
      
    }else{
      
      d <- dataInput()
      m1 <- model()
      
      x <- input$variablex
      from <- input$control
      to <- input$treat
      type <- input$type
      
      condvar <- d[,input$subsetvar]
      valcondvar <- input$valsubset
      subset <- condvar == valcondvar
      
      res <- elrEffects(m1, x=x, from=from, to=to, type=type, subset=subset)  
      print(res)
    }
    
  })
  
    
  
    
})


