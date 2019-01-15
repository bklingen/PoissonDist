library(plotly)
library(rhandsontable)
library(shinyjs)
library(V8)
library(shinyWidgets)
library(DT)

#  (C) 2018  Bernhard Klingenberg
navbarPage(
  title=a(tags$b("The Poisson Distribution"), href='http://www.artofstat.com'),
  windowTitle="Poisson Distribution",
  id="mytabs",
  tabPanel("Explore",
    sidebarLayout(
      sidebarPanel(
        helpText("The Poisson distribution specifies the probability of a certain number of 
                 events happening when each event occurs with a constant rate of ",
                 HTML("&lambda;.")),
        helpText("Change the value of ", HTML("&lambda;"), 
                 "to see how the shape of the distribution changes. Hover over the bars to find the probability."),
        sliderInput("lambda", HTML("<p>Select Rate Parameter &lambda;:</p>"),
                    min = 0, max = 10, value = 2, step = 0.05, round = -2),
        h5(tags$b("Probability Table:")),
        rHandsontableOutput("freqtable1"),
        tags$hr(),
        a(img(src="Logo.PNG"), href='http://www.artofstat.com')
      ), #end sidebar
      mainPanel(
        useShinyjs(),
        extendShinyjs(script = "js/focus.js"),
        plotlyOutput("bar")
      )
    ) #end sidebarlayout
  ), #end first tabpanel
  tabPanel("Find Probabilities",
    sidebarLayout(
       sidebarPanel(
         numericInput("lambda1", HTML("<p>Select Rate Parameter &lambda;:</p>"), value=5, min=0, step=0.5),
         selectInput("type", "Select Type of Probability:", choices=NULL),
         conditionalPanel(condition ="input.type != 'type4'", numericInput("x", "Value of x:", value=0, min=0, width="60%")),
         conditionalPanel(condition ="input.type == 'type4'", 
           fluidRow(
             column(6, numericInput("x1", HTML("Value of x<sub>1</sub>:"), value=0, min=0)),
             column(6, numericInput("x2", HTML("Value of x<sub>2</sub>:"), value=5, min=0))
           )
         ),
         tags$hr(),
         awesomeCheckbox("showprob", "Show Probability Table"),
         conditionalPanel(condition="input.showprob", 
           h5(tags$b("Probability Table:")),
           rHandsontableOutput("freqtable2")
         ),
         tags$hr(),
         a(img(src="Logo.PNG"), href='http://www.artofstat.com')
       ),
       mainPanel(
         plotlyOutput("bar1", height=330),
         br(),
         fluidRow(column(1), 
                  column(11,
                    conditionalPanel(condition="input.type=='type1'", column(11, tableOutput("probtable1"))),
                    conditionalPanel(condition="input.type=='type2'", column(11, tableOutput("probtable2"))),
                    conditionalPanel(condition="input.type=='type3'", column(11, tableOutput("probtable3"))),
                    conditionalPanel(condition="input.type=='type4'", column(11, tableOutput("probtable4")))
                 )
         )
       )
    ) #end sidebarlayout
  ), #end second tabPanel
  tabPanel("Find Quantiles",
    sidebarLayout(
      sidebarPanel(
        numericInput("lambda2", HTML("<p>Select Rate Parameter &lambda;:</p>"), value=5, min=0, step=0.5),
        selectInput("qtype", "Select Type of Quantile:", choices=list("One-tailed"="one","Two-tailed"="two")),
        conditionalPanel(condition ="input.qtype == 'one'", numericInput("p", "Percent Below:", value=95, min=0, max=100, width="60%")),
        conditionalPanel(condition ="input.qtype == 'two'", numericInput("p1", "Percent in Both Tails:", value=5, min=0, max=100, width="60%")),
        tags$hr(),
        awesomeCheckbox("showprob1", "Show Probability Table"),
        conditionalPanel(condition="input.showprob1", 
                         h5(tags$b("Probability Table:")),
                         rHandsontableOutput("freqtable3")
        ),
        tags$hr(),
        a(img(src="Logo.PNG"), href='http://www.artofstat.com')
      ),
      mainPanel(
        plotlyOutput("bar2", height=330),
        br(),
        fluidRow(column(1), 
                 column(11,tableOutput("quantable"))
                   #conditionalPanel(condition="input.qtype=='one'", column(11, tableOutput("quantable1"))),
                   #conditionalPanel(condition="input.qtype=='two'", column(11, tableOutput("quantable2")))
                 #)
        )
      )
    ) #end sidebarlayout
  ) #end third tabPanel
) #end navbar
