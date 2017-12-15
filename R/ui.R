library(plotly)
library(rhandsontable)
library(shinyjs)

#  (C) 2016  Bernhard Klingenberg
shinyUI(
  fluidPage(
    useShinyjs(),
    extendShinyjs(script = "../js/focus.js"),
    # Application title
    titlePanel("The Poisson Distribution"),
    # Sidebar with a slider input for number of observations
    sidebarPanel(
      helpText("The Poisson distribution gives the probability of some events happening",
               "within a fixed period of time or interval of space given the constant",
               "rate of the events happening", HTML("(&lambda;)"), "provided that the",
               "events happen independently of the time since the last event."),
      conditionalPanel(condition="input.mytabs=='EU'",
                       # slider to get input for lambda
                       wellPanel(
                         sliderInput(inputId ="lambda",
                                     label = HTML("<p>Choose &lambda;:</p>"),
                                     min = 1,
                                     max = 10,
                                     value = 5,
                                     step = 1
                         )
                       )
      ),

      #wellPanel(
      h5(tags$b("Options:")),
      checkboxInput("xzoom", "Zoom in on x-axis", value=FALSE),
      conditionalPanel(condition="input.xzoom", 
                       sliderInput("xrange", "Select range of x-axis:", min=0, max=20, value=c(0,20), round=TRUE)
      ),
      checkboxInput("probtable", "Show table of probabilities", value=TRUE),
      #),
      downloadButton("save", "Download Graph")
    ), #end sidebarPanel
    
    # Show a plot of the generated Poisson distribution
    # by various operations
    mainPanel(
      tabsetPanel(
        id = "mytabs",
        tabPanel(title="Explore",
                 value="EU",
                 br(),
                 plotlyOutput("histo"),
                 br(),
                 conditionalPanel(condition="input.probtable",
                                  rHandsontableOutput("freqtable"))
        ),
        # things that are supposed to be inside second tab
        tabPanel(title="Find Probabilities",
                 value="Prob"
        ),
        # things that are supposed to be inside third tab
        tabPanel(title="Find Quantiles",
                 value="Quan"
        ))#end tabset
    ) #end mainpanel
  )) #end pagewithsidebar and shiny