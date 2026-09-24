library(plotly)
library(rhandsontable)
library(shinyjs)
library(V8)
library(shinyWidgets)
library(DT)

#  (C) 2020  Bernhard Klingenberg
## `lumen` is set in global.R: TRUE for the Lumen Learning app, FALSE for Art of Stat.

store_badges <- if (!lumen) {
  div(class="column2",
      a(img(src="badge-app-store.svg", class="sidebar-badge", alt="Download on the App Store"),
        href='https://apps.apple.com/us/app/art-of-stat/id6755374228',
        target="_blank",
        `aria-label` = "Download the Art of Stat app on the App Store (opens in a new tab)"),
      a(img(src="badge-google-play.svg", class="sidebar-badge", alt="Get it on Google Play"),
        href='https://play.google.com/store/apps/details?id=com.artofstat.app',
        target="_blank",
        `aria-label` = "Get the Art of Stat app on Google Play (opens in a new tab)")
  )
}

textbook_promo <- if (!lumen) {
  tagList(
    h5(tags$b("Check out our textbook:")),
    a(img(src='textbookFullCover.png', width="150px", alt="Cover image of the Art of Stat textbook"),
      href='http://www.artofstat.com',
      target="_blank",
      `aria-label` = "Art of Stat textbook website (opens in a new tab)")
  )
}

artofstat_icon <- a(
  img(
    src = "app-artofstat.png",
    width = "85px",
    class = "sidebar-icon",
    alt = "Art of Stat mobile app icon"
  ),
  href = "https://artofstat.com/mobile-apps",
  target = "_blank",
  `aria-label` = "Art of Stat mobile app (opens in a new tab)"
)

## Sidebar footer shared by all tabs: mobile app, and (Art of Stat only) store badges and textbook
sidebar_promo <- tagList(
  tags$hr(class = "custom-hr"),
  h5(tags$b("Available as mobile app:")),
  div(class="sidebar-apps",
      div(class="column1",
          artofstat_icon
      ),
      store_badges
  ),
  tags$p(
    "More information ",
    tags$a(href = "https://artofstat.com/mobile-apps", "here.", target="_blank",
           `aria-label` = "More information about the Art of Stat mobile app (opens in a new tab)")
  ),
  textbook_promo
)

navbarPage(
  title = if (lumen) {
    HTML("<b style='color:black;'>The Poisson Distribution</b>")
  } else {
    a(tags$b("The Poisson Distribution"), href='http://www.artofstat.com')
  },
  header = tags$head(
    tags$style(HTML("
      .custom-hr {
        border: 0;
        border-top: 1px solid #808080;
        margin: 15px 0;
      }
      .sidebar-icon {
        border-radius: 22px;
      }
      .sidebar-apps {
        display: flex;
        align-items: center;
      }
      .column1 {
        width: 100px;
        padding: 2px;
      }
      .column1 a {
        display: block;
        line-height: 0;
      }
      .column2 {
        width: 160px;
        padding: 2px;
      }
      .column2 a {
        display: block;
        height: 36px;
        line-height: 0;
      }
      .column2 a + a {
        height: 34px;
        margin-top: 4px;
      }
      .sidebar-badge {
        height: 100%;
        width: auto;
        max-width: none;
        display: block;
      }
      "))
  ),
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
        sidebar_promo
      ), #end sidebar
      mainPanel(
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
         sidebar_promo
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
               sidebar_promo
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
