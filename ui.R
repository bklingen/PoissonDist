library(plotly)
library(rhandsontable)
library(shinyjs)

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
        helpText("Change the value of ", HTML("lambda;"), 
                 "to see how the shape of the distribution changes. Hover over the bars to find the probability."),
        sliderInput("lambda", HTML("<p>Select Rate Parameter &lambda;:</p>"),
                    min = 0, max = 10, value = 4, step = 0.05, round = -2),
        h5(tags$b("Probability Table:")),
        rHandsontableOutput("freqtable1")
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
         numericInput("lambda1", HTML("<p>Select Rate Parameter &lambda;:</p>"), value=5, min=0, step=1),
         selectInput("type", "Select Type of Probability:", choices=NULL),
         conditionalPanel(condition ="input.type != 'type4'", numericInput("x", "Value of x:", value=0, min=0)),
         conditionalPanel(condition ="input.type == 'type4'", 
           fluidRow(
             column(6, numericInput("x1", HTML("Value of x<sub>1</sub>:"), value=0, min=0)),
             column(6, numericInput("x2", HTML("Value of x<sub>2</sub>:"), value=5, min=0))
           )
         )
       ),
       mainPanel(
         plotlyOutput("bar1", height=330),
         br(),
         fluidRow(column(1), column(11, tableOutput("probtable")))
       )
    ) #end sidebarlayout
  ) #end second tabPanel
) #end navbar
