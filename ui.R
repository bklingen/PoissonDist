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
        helpText("The Poisson distribution specifies the probability of the number of 
                 events happening when each event occurs with a constant rate of ",
                 HTML("&lambda;."), "Change the value of ", HTML("&lambda;"), 
                 "to see how the shape of the distribution changes."),
        sliderInput("lambda", HTML("<p>Select Rate Parameter &lambda;:</p>"),
                    min = 0, max = 10, value = 5, step = 0.05, round = -2),
        tags$br(),
        rHandsontableOutput("freqtable")
      ), #end sidebar
      mainPanel(
        useShinyjs(),
        extendShinyjs(script = "js/focus.js"),
        plotlyOutput("histo")
      )
    ) #end sidebarlayout
  ), #end first tabpanel
  tabPanel("Find Probabilities",
    sidebarLayout(
       sidebarPanel(
         numericInput("lambda1", HTML("<p>Select Rate Parameter &lambda;:</p>"), value=5, min=0, step=1),
         selectInput("prob", "Select Type of Probability:",
           list("Poisson Probability: P(X = x)" = "type1", 
                "Lower Tail: P(X <= x)" = "type2",
                "Upper Tail: P(X >= x)"="type3", 
                "Interval: P(x1 <= X <= x2)"="type4"
           )
         ),
         conditionalPanel(condition ="input.prob != 'type4'", numericInput("x", "Value of x:", value=0, min=0)),
         conditionalPanel(condition ="input.prob == 'type4'", 
           fluidRow(
             column(6, numericInput("x1", "Value of x1:", value=0, min=0)),
             column(6, numericInput("x2", "Value of x2:", value=5, min=0))
           )
         )
       ),
       mainPanel(
         plotlyOutput("histo1")
       )
    ) #end sidebarlayout
  ) #end second tabPanel
) #end navbar
