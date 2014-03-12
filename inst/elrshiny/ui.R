
shinyUI(pageWithSidebar(
  headerPanel("EffectLiteR"),
  
  sidebarPanel(
    tabsetPanel(
      tabPanel('Data',        
        selectInput("exdata", "Select Example Data", 
                    c("none","example01","nonortho"),selected="none"),
        fileInput('file1', 'Choose SPSS File', accept=c('.sav')),
        selectInput("variabley", "Dependent Variable", ""),
        selectInput("variablex", "Treatment Variable", ""),
        selectInput("variablek", "Categorical Covariates", "", multiple=TRUE),
        selectInput("variablez", "Continuous Covariates", "", multiple=TRUE)
      ),
      tabPanel('Options',
        selectInput("control", "Control Group", ""),
        radioButtons("missing", "Missing Data", 
                     choices=c("listwise","fiml"), 
                     selected = "listwise"),
        checkboxInput("fixed.cell", "Fixed Cell Sizes", FALSE)
    )
  )),
  
  mainPanel(
    tabsetPanel(
      tabPanel('Data', dataTableOutput("mytable1")),
      tabPanel("EffectLiteR", verbatimTextOutput("summary")),
      tabPanel("lavaan Syntax", verbatimTextOutput("lavsyntax")),
      tabPanel("lavaan Results", verbatimTextOutput("lavresults")),
      tabPanel("Plot 1", plotOutput("plot1")),
      tabPanel("Plot 2", plotOutput("plot2"))
    )
  )
))