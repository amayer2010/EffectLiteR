
shinyUI(fluidPage(
  titlePanel(title="EffectLiteR"),
  
  sidebarLayout(
  sidebarPanel(
    tabsetPanel(
      ######### Data ############
      tabPanel('Data',
        img(src='effectliter_logo.png', align = "right"),
        br(),
        br(),
        actionButton("newanalysis","Start a New Analysis"),
        uiOutput("reload"),
        hr(),        
        selectizeInput(inputId="exdata", label="Example Data", selected="",
                       choices= c("","nonortho","example01","example02lv"),
                       options = list(placeholder = 'choose example data')),    
        hr(),
        tryCatch(
          fileInput("file1", "Data File", 
                    accept=c(".csv", ".txt", ".sav", ".xpt", 
                              ".CSV", ".TXT", ".SAV", ".XPT"))
        ),
        helpText('Select either a .csv, .txt, .sav or a .xpt file to be uploaded. The corresponding R function (read.csv, read.table, read.spss, or read.xport) will be chosen automatically with the default settings for arguments. To read in data from inside R into the shiny interface, it is easiest to save your dataset using write.csv with the default settings. If reading your SPSS file does not work, please try saving it in SAS Transport format with file ending .xpt. Causes for errors may be special characters in file names and/or path names.')
      ),
      ########## Manifest Variables ############
      tabPanel('Manifest Variables',
               img(src='effectliter_logo.png', align = "right"),
#                submitButton(text = "Run", icon = NULL),
               conditionalPanel(
                  condition = "!input.latenty",
                  br(),
                  br(),
                  br(),
                  selectizeInput("variabley", "Dependent Variable Y", "",
                        options = list(placeholder = 'select dependent variable'))
               ),
               br(),
               selectizeInput("variablex", "Treatment Variable X", "",
                    options = list(placeholder = 'select treatment variable')),
               br(),
               selectizeInput("variablek", "Categorical Covariates K", "", 
                   multiple=TRUE, selected="",
                   options = list(placeholder = 'select categorical covariates')),
               br(),
               selectizeInput("variablez", "Continuous Covariates Z", "", 
                              multiple=TRUE, selected="",
                    options = list(placeholder = 'select continuous covariates')),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
br()
      ),
      ############ Latent Variables ###########
      tabPanel('Latent Variables',
        img(src='effectliter_logo.png', align = "right"),       
        h5("Latent Dependent Variable"),       
        checkboxInput("latenty", "The dependent variable is latent", FALSE),        
        conditionalPanel(
          condition = "input.latenty",
          p("If the dependent variable is latent, entries under manifest dependent variable will be discarded."),
          ###
          br(),
          h5("Latent Dependent Variable"),
          textInput("name.etay", "Name of Latent Dependent Variable", 
                    value = "etay"),
          selectInput("indicatorsy", "Indicators of Latent Dependent Variable", "",
                      multiple=TRUE, selectize=TRUE),
          radioButtons("mm.etay", "Measurement Model for Latent Dependent Variable",
                       choices = c("equivalent measures" = "parallel",
                                   "essentially equivalent measures" = "tau-equi",
                                   "congeneric measures" = "tau-cong"),
                       selected = "parallel")
        ),
        br(),
        hr(),
        br(),
        h5("Latent Covariates"),
        checkboxInput("latentz", "Add latent covariates", FALSE),
        conditionalPanel(
          condition = "input.latentz",
          helpText("Latent covariates will be added to the list of continuous manifest covariates (if specified)."),
          numericInput("nlatentz", "Number of Latent Covariates", 0,
                       min = 0, max = 5),
          ### Cov 1
          conditionalPanel(
            condition = "input.nlatentz > 0",
            br(),
            h5("Latent Covariate 1"),
            textInput("name.etaz1", "Name of Latent Covariate 1", 
                      value = "etaz1"),
            selectInput("indicatorsz1", "Indicators of Latent Covariate 1", "",
                        multiple=TRUE, selectize=TRUE),
            radioButtons("mm.etaz1", "Measurement Model for Latent Covariate 1",
                        choices = c("equivalent measures" = "parallel",
                                    "essentially equivalent measures" = "tau-equi",
                                    "congeneric measures" = "tau-cong"))
          ),
          ### Cov 2
          conditionalPanel(
            condition = "input.nlatentz > 1",
            br(),
            h5("Latent Covariate 2"),
            textInput("name.etaz2", "Name of Latent Covariate 2", 
                      value = "etaz2"),
            selectInput("indicatorsz2", "Indicators of Latent Covariate 2", "",
                        multiple=TRUE, selectize=TRUE),
            radioButtons("mm.etaz2", "Measurement Model for Latent Covariate 2",
                         choices = c("equivalent measures" = "parallel",
                                     "essentially equivalent measures" = "tau-equi",
                                     "congeneric measures" = "tau-cong"))
          ),  
          ### Cov 3
          conditionalPanel(
            condition = "input.nlatentz > 2",
            br(),
            h5("Latent Covariate 3"),
            textInput("name.etaz3", "Name of Latent Covariate 3", 
                      value = "etaz3"),
            selectInput("indicatorsz3", "Indicators of Latent Covariate 3", "",
                        multiple=TRUE, selectize=TRUE),
            radioButtons("mm.etaz3", "Measurement Model for Latent Covariate 3",
                         choices = c("equivalent measures" = "parallel",
                                     "essentially equivalent measures" = "tau-equi",
                                     "congeneric measures" = "tau-cong"))
          ),  
          ### Cov 4
          conditionalPanel(
            condition = "input.nlatentz > 3",
            br(),
            h5("Latent Covariate 4"),
            textInput("name.etaz4", "Name of Latent Covariate 4", 
                      value = "etaz4"),
            selectInput("indicatorsz4", "Indicators of Latent Covariate 4", "",
                        multiple=TRUE, selectize=TRUE),
            radioButtons("mm.etaz4", "Measurement Model for Latent Covariate 4",
                         choices = c("equivalent measures" = "parallel",
                                     "essentially equivalent measures" = "tau-equi",
                                     "congeneric measures" = "tau-cong"))
          ),  
          ### Cov 5
          conditionalPanel(
            condition = "input.nlatentz > 4",
            br(),
            h5("Latent Covariate 5"),
            textInput("name.etaz5", "Name of Latent Covariate 5", 
                      value = "etaz5"),
            selectInput("indicatorsz5", "Indicators of Latent Covariate 5", "",
                        multiple=TRUE, selectize=TRUE),
            radioButtons("mm.etaz5", "Measurement Model for Latent Covariate 5",
                         choices = c("equivalent measures" = "parallel",
                                     "essentially equivalent measures" = "tau-equi",
                                     "congeneric measures" = "tau-cong"))
          )            
        )
      ),
      ########## Options #############
      tabPanel('Options',
        img(src='effectliter_logo.png', align = "right"),
        br(),
        br(),
        br(),
        selectInput("control", "Control Group", ""),
        br(),
        radioButtons("missing", "Missing Data", 
                     choices=c("listwise","fiml"), 
                     selected = "listwise"),
        br(),
        radioButtons("se", "Standard Errors", 
                     choices=c("standard","boot"), ##TODO: add robust SE 
                     selected = "standard"),
        conditionalPanel(
          condition = "input.se == 'boot'",
          numericInput("bootstrap", "Number of bootstrap draws", 
                      min=1, max=5000, value=5)
        ),
        br(),
        radioButtons("fixed.cell", "Cell Sizes", 
                     choices=c("stochastic","fixed"), 
                     selected = "stochastic"),
        br(),
        h5("Additional Options"),
        checkboxInput("vallabels", "Use value labels from SPSS", TRUE)                                      
    ),
########## New Features #############
    tabPanel('New Features',
         img(src='effectliter_logo.png', align = "right"),
         helpText('Here you will find some new features that are currently under development. This means they have not yet been tested thoroughly and they may require development versions of lavaan (https://github.com/yrosseel/lavaan) or  additional packages.'),        
         br(),
         h5("Interactions"),
         radioButtons("interactions", "", 
                      choices=c("all","2-way","none"), 
                      selected = "all"),
         br()                
      )
  )),
  
  mainPanel(
    tabsetPanel(
      ######### Data Table ##########
      tabPanel('Data', dataTableOutput("mytable1")),
      
      ######### EffectLiteR ##########
      tabPanel("EffectLiteR", verbatimTextOutput("summary")),
      
      ######### lavaan Syntax ##########
      tabPanel("lavaan Syntax", verbatimTextOutput("lavsyntax")),
      
      ######### lavaan Results ##########
      tabPanel("lavaan Results", verbatimTextOutput("lavresults")),
      
      ######### Conditional Effects ##########
      tabPanel('Conditional Effects', 
               verbatimTextOutput("helptextcondeffects"),
               dataTableOutput("condeffs")),
      
      ######### Plot 1 ##########
      tabPanel("Plot 1", 
               verbatimTextOutput("helptextplot1"),
               plotOutput("plot1")
      ),
      
      ######### Plot 2 ##########
      tabPanel("Plot 2", 
        verbatimTextOutput("helptextplot2"),
        selectInput("zselect", 
            "Select continuous covariate", "", 
            multiple=FALSE,
            selectize=TRUE                 
        ),
      plotOutput("plot2")),
      
      ######### Plot 3 ##########
      tabPanel("Plot 3",
              verbatimTextOutput("helptextplot3"),
              selectInput("gxselect", 
                          "Select effect function (y-axis)", 
                          "g1(K,Z)", 
                          multiple=FALSE,
                          selectize=TRUE),
              selectInput("zselect2", 
                          "Select regressor  (x-axis)", 
                          "", 
                          multiple=FALSE,
                          selectize=TRUE),
              selectInput("zselect3", 
                          "Select colour variable", 
                          "", 
                          multiple=FALSE,
                          selectize=TRUE),
               plotOutput("plot3")) #,
#       
#       ######### Path Diagram ##########
#       tabPanel("Path Diagram", plotOutput("plotpd"))
            
    )    
  ))

))