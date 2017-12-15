#  (C) 2016  Bernhard Klingenberg
shinyUI(
  fluidPage(
    # Application title
    titlePanel("The Poisson Distribution"),
    # Sidebar with a slider input for number of observations
    sidebarPanel(
      helpText("The Poisson distribution gives probability of some number of events",
               "happening within a fixed period of time or interval of space given",
               "the constant rate of the events happening, provided that the events",
               "happen independently of the time since the last event."),
      conditionalPanel(condition="input.mytabs=='EU'",
                       # slider to get input for sample size
                       wellPanel(
                         sliderInput(inputId ="sampleSize0",
                                     label = HTML("<p>Choose &lambda;:</p>"),
                                     min = 1,
                                     value = 6,
                                     max = 200,
                                     step = 1
                         )
                       )
      ),

      #wellPanel(
      h5(tags$b("Options:")),
      checkboxInput("xzoom", "Zoom in on x-axis", value=FALSE),
      conditionalPanel(condition="input.xzoom", 
                       sliderInput("xrange", "Select range of x-axis:", min=0, max=6, value=c(0,6), round=TRUE)
      ),
      checkboxInput("probtable", "Show table of probabilities", value=FALSE),
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
                 plotOutput("histo"),
                 br(),
                 htmlOutput("freqtable")
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