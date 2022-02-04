library(plotly)
library(rhandsontable)
library(shinyjs)
library(V8)
library(shinyWidgets)
library(DT)

#  (C) 2020  Bernhard Klingenberg
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
                 "to see how the shape of the distribution changes. Hover over the bars in the graph to find the corresponding probability, or look at the table below."),
        sliderInput("lambda", HTML("<p>Rate Parameter &lambda;:</p>"),
                    min = 0, max = 10, value = 2, step = 0.05, round = -2),
        h5(tags$b("Probability Table:")),
        rHandsontableOutput("freqtable1"),
        tags$hr(),
        a(img(src="Logo.PNG"), href='http://www.artofstat.com')
      ), #end sidebar
      mainPanel(
        modalDialog(
          #HTML("<img src='Icon4_512_512.png'/>"),
          title = HTML("
        <h4>Work with the Binomial Distribution right on your phone with the new Art of Stat: Distributions <b> mobile app</b>!</h3>
        "),
          a(img(src="DistAppIconRound.png", width="230"), href='https://artofstat.com/mobile-apps'),
          a(img(src="IMG_5565.PNG", width="130"), href='https://artofstat.com/mobile-apps'),
          a(img(src="IMG_5566.PNG", width="130"), href='https://artofstat.com/mobile-apps'),
          #a(img(src="IMG_5357.PNG", width="130"), href='https://artofstat.com/mobile-apps'),
          tags$br(),
          #a(img(src="AppStoreLogoApple.png",width="180"), href='https://apps.apple.com/us/app/art-of-stat-explore-data/id1599474757#?platform=iphone'),
          a(img(src="AppStoreLogoApple.png",width="180"), href=' https://apps.apple.com/gb/app/art-of-stat-inference/id1578438712#?platform=iphone'),
          a(img(src="AppStoreLogoAndroid1.png",width="205"), href='https://play.google.com/store/apps/details?id=com.artofstat.inference'),
          footer = tagList(
            #a(img(src="AppStoreLogoApple.png",width="210"), href='http://www.artofstat.com'),
            #a(img(src="AppStoreLogoAndroid1.png",width="235"), href='http://www.artofstat.com'),
            HTML("<big>Search for <b>Art of Stat</b> in the App Store.<br>For more information, including screenshots, <a href=https://artofstat.com/mobile-apps>check here</a>.</big> <br>"),
            modalButton("Dismiss")
            #actionButton("ok", "OK")
          ),
          #footer = modalButton("Dismiss"),
          size = "m", #c("m", "s", "l"),
          easyClose = TRUE,
          fade = TRUE
        ),
        useShinyjs(),
        extendShinyjs(script = "js/focus.js", functions=c("focus")),
        plotlyOutput("bar", height=380)
      ) #end main Panel
    ) #end sidebarlayout
  ), #end first tabpanel
  tabPanel("Find Probabilities",
    sidebarLayout(
       sidebarPanel(
         numericInput("lambda1", HTML("<p>Rate Parameter &lambda;:</p>"), value=5, min=0, step=0.5),
         selectInput("type", "Type of Probability:", choices=NULL),
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
         conditionalPanel(condition="input.type=='type1'", uiOutput("caption1"), tableOutput("probtable1")),
         conditionalPanel(condition="input.type=='type2'", uiOutput("caption2"), tableOutput("probtable2")),
         conditionalPanel(condition="input.type=='type3'", uiOutput("caption3"), tableOutput("probtable3")),
         conditionalPanel(condition="input.type=='type4'", uiOutput("caption4"), tableOutput("probtable4"))
       )
    ) #end sidebarlayout
  ), #end second tabPanel
  tabPanel("Formulas and Properties",
           sidebarLayout(
             sidebarPanel(
               withMathJax(),
               helpText(h5("The formula for the distribution function $P(X = x)$ of the Poisson distribution is shown to the right.")),
               helpText(h5("The distribution function $P(X=x)$ finds the probability of observing a count of $x$ events, when events occur with rate $\\lambda$.")),
               #helpText(h5("The cumulative distribution function $P(X \\le x)$ gives the probability of observing $x$ events or fewer.")),
               sliderInput("lambda3", HTML("<p>Rate Parameter &lambda;:</p>"), min = 0, max = 10, value = 2, step = 0.05, round = -2),
               sliderInput(inputId = "x3", label=HTML("<p>Number of Events (x):</p>"), min=0, value=3, max=20, step=1),
               helpText(HTML("For calculations with values of &lambda; or x not selectable via the sliders, please go to the <b>Find Probability</b> tab, where you can enter any values for &lambda; and x.")),
               tags$hr(),
               a(img(src="Logo.PNG"), href='http://www.artofstat.com')
             ),
             mainPanel(
               uiOutput('pdf'),
               uiOutput('cdf'),
               h5(tags$b("Probability Table:")),
               rHandsontableOutput("freqtable3")
             )
           ) #end sidebarlayout
  ) #end fourth tabPanel
  
) #end navbar
