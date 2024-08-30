
shinyUI(fluidPage(
  titlePanel(title="elrEffects"),
  
  sidebarLayout(
  sidebarPanel(
    tabsetPanel(
      ######### Data ############
      tabPanel('Data',
        br(),
        selectizeInput(inputId="exdata", label="Example Data", selected=NULL,
                       choices= c("", 
                                  "example01",
                                  "elrdata_logreg",
                                  "elrdata_kieferetal2024"),
                       options = list(placeholder = 'choose example data'),
                       width='50%'),    
        # hr(),
        tryCatch(
          fileInput("file1", "Data File", 
                    accept=c(".csv", ".txt", ".sav", ".xpt", 
                              ".CSV", ".TXT", ".SAV", ".XPT",
                             ".DAT", ".dat", ".RDS", ".rds", ".Rds"))
        ),
        helpText('Select either a .csv, .dat, .txt, .sav, .xpt or a .rds file to be uploaded. The corresponding R function (read.csv, read.table, read.spss, read.xport, or readRDS) will be chosen automatically with the default settings for arguments. Some default arguments can be overwritten (see additional options below).'),
        br(),
        h5(strong("Additional Options to Read Data")),
        br(),
        selectizeInput(inputId="vallabels", 
                       label=h5("Use value labels (SPSS data)"), 
                       selected="default",
                       choices= c("default","yes","no"),
                       width='90%'),
        selectizeInput(inputId="header", 
                       label=h5("File contains variable names (csv, dat, and txt data)"), 
                       selected="default",
                       choices= c("default","yes","no"),
                       width='90%'),
        selectizeInput(inputId="sep", 
                       label=h5("Character separating columns (csv, dat, and txt data)"), 
                       selected="default",
                       choices= c("default","semicolon","white space"),
                       width='90%'),
        selectizeInput(inputId="dec", 
                       label=h5("Decimal character (csv, dat, and txt data)"), 
                       selected="default",
                       choices= c("default","decimal point","decimal comma"),
                       width='90%'),
        textInput(inputId="na.strings", 
                  label=h5("Missing value code (csv, dat, and txt data)"), 
                  value = "NA",
                  width='90%'),
        br()
      ),
      ########## Manifest Variables ############
      tabPanel('Manifest Variables',
               br(),
               
               selectizeInput(
                    inputId="variabley", 
                    label="Dependent Variable Y", 
                    choices="",
                    options = list(placeholder = 'select dependent variable'),
                    width='60%'),
               
               selectizeInput(
                 inputId="variablex", 
                 label="Treatment Variable X", 
                 choices="",
                 options = list(placeholder = 'select treatment variable'),
                 width='60%'),
               
               conditionalPanel(
                  condition = "!input.ownformula",
                  selectizeInput(
                    "variablek", "Categorical Covariates K", "", 
                    multiple=TRUE, selected="",
                    options = list(placeholder = 'select categorical covariates')),
                  selectizeInput(
                    "variablez", "Continuous Covariates Z", "", 
                    multiple=TRUE, selected="",
                    options = list(placeholder = 'select continuous covariates'))
                ),
               
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

########## Model ############
    tabPanel('Model',
         br(),
         checkboxInput("ownformula", "Specify model formula yourself", FALSE),
         
         conditionalPanel(
           condition = "input.ownformula",
           textInput("formula", "Specify formula yourself", value = "")
         ),
         
         selectizeInput(inputId="family", 
                        label="Select Family", 
                        selected="gaussian",
                        choices= c("gaussian","binomial","poisson","Gamma"),
                        width='50%'),
         
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

########## Input elrEffects ############
tabPanel('elrEffects',
         br(),
         selectInput("treat", "Treatment Group", "", width='60%'),         
         selectInput("control", "Reference Group", "", width='60%'),
         selectInput("type",
                     "Type of Effect",
                     choices=c("Average Treatment Effect",
                               "Simple Ratio of Averages",
                               "Odds Ratio of Averages",
                               "Average of Simple Ratios",
                               "Average of Odds Ratios"), 
                     selected="difference",
                     width='60%'),
         selectizeInput(
           inputId="subsetvar", 
           label="Subset for Conditional Effect", 
           choices="",
           options = list(placeholder = 'select subsetting variable'),
           width='60%'),
         selectInput("valsubset", "Select Value", "", width='60%'),
         br(),
         br(),
         br(),
         br(),
         br(),
         br(),
         br(),
         br(),
         br()
)


  )),
  
  mainPanel(
    tabsetPanel(
      ######### Data Table ##########
      tabPanel('Data', DT::DTOutput("mytable1")),

      ######### lm Results ##########
      tabPanel("Results",
               verbatimTextOutput("lmresults")),
      
      ######### elrEffects ##########
      tabPanel("elrEffects", 
               br(),
               verbatimTextOutput("helpelreffects"),
               verbatimTextOutput("elreffects"),
               verbatimTextOutput("helpelreffectscond"),
               verbatimTextOutput("elreffectscond")
      )
      
      # tabPanel("elrEffects", 
      #          br(),
      #          verbatimTextOutput("elreffects"),
      #          column(3, wellPanel(
      #            h5("Values of Predictors"),
      #            uiOutput("ui")
      #          )) ,
      #          column(9, wellPanel(
      #            h5("Expected Outcome"),
      #            verbatimTextOutput("expoutcome")
      #          ))
      #          
      # )

    )
  ))

))