library(shiny)
library(shinyMobile)
library(apexcharter)
#library(plotly)
library(sever)
#library(shinyWidgets)

f7Page(
  title = "Poisson Distribution",
  tags$style(
    "
    .block {
    margin-top: 5px;
    margin-right: 5px;
    margin-bottom: 5px;
    margin-left: 5px;
    padding: 0px 0px;
    }
  
    .block-title {
    margin-top: 12px;
    margin-right: 10px;
    margin-bottom: 12px;
    margin-left: 10px;
      }
    
    .slider {
    margin-top: 2px;
    margin-right: 10px;
    margin-bottom: 10px;
    margin-left: 10px;
    padding: 0px 0px;
    }
    
    .stepper {
    margin-top: 0px;
    margin-right: 2px;
    margin-bottom: 2px;
    margin-left: 2px;
    padding: 0px 0px;
    }
    
   .card {
   margin-top: 5px;
   margin-right: 10px;
   margin-bottom: 0px;
   margin-left: 10x;
   padding: 0px 0px;
   }

   .card-content {
   margin-top: 5px;
   margin-right: 2px;
   margin-bottom: 2px;
   margin-left: 2x;
   padding: 5px 5px;
   #font-size: 16pt;
   }
   
   .card-header {
   min-height: 32px;
   justify-content: left;
   }
   
   .card-footer {
   min-height: 30px;
   }
   
   .list {
   margin-top: 5px;
   margin-right: 0px;
   margin-bottom: 5px;
   margin-left: 0px;
   padding: 2px 2px;
   }

   # not working on shinyapps.io
   # .md .tabbar-labels .tab-link {
   # padding-top: 20px;
   # }
   
   # .apexcharts-tooltip {
   #    background: #f3f3f3;
   #    color: yellow;
   # }
    
  "
  ),
  
  
  f7TabLayout(
    navbar = f7Navbar(
      title = "The Poisson Distribution",
      hairline = TRUE,
      shadow = FALSE,
      leftPanel = FALSE,
      rightPanel = TRUE
    ),
    panels = tagList(
      f7Panel(
        title = "About This App",
        side = "right",
        theme = "dark",
        HTML('The Poisson Distribution gives probabilities for the <b> number of 
        events </b> happening, when events occur at a constant rate λ. For instance, consider 
        telephone calls coming into a call center randomly, but at an average rate of λ = 3 calls per minute. 
        Then the number of calls reveived by the call center in a minute could be 0, 1, 2, 3, 4, 5, 6, or 7 and more calls. 
        The Poisson distribution gives the probability of observing exactly 0 calls, or exactly 1 call, or exactly 2 calls, etc.
        <br>
        <br>
        <b>Explore Tab:</b> Use the sliders to change the values
        of λ to see how the Poisson probabilities for the various counts change. 
        You can tab on a bar in the bar chart to see the probability of observing exactly this many events. The Table 
        of the Poisson Distribution below the chart shows these probabilities for many possible number of 
        events x, starting with 0 events.
        For showing the Poisson Distribtuion for λ > 6, please use the Find Probability tab.
        <br>
        <br>
        <b>Find Probability Tab:</b> For any given value of λ, 
        visualize the Poisson Distribution and find P(X = x), the probability of observing exactly x events, x = 0, 1, 2, 3, ... . To change the  
        value for λ, use the plus/minus buttons or simply tab in the middle of the box 
        to type in our own numerical value. You can also find cumulative (tail) probabilities, such as P(X &le; x), the probability 
        of obtaining x events or fewer (equivalently "at most x events") or P(X &ge; x), the probability 
        of obtaining x events or more (equivalently "at least x successes"). You can also find P(LB &le; X &le; UB), 
        the probability for the number of events to be between a lower bound LB and an upper bound UB, including the boundary values.
        <br>
        <br>
        <b>Poisson Formula:</b> The Poisson probabilities computed in this app are based on the following formula for obtaining
        x events, when events occur at a constant rate of λ:
        <br>
        P(X = x) = λ<sup>x</sup> e<sup>-λ</sup> / x! , 
        <br>
        for x = 0, 1, 2, 3, ... .
        <br>
        Here, x! represents x factorial, which is x*(x-1)*(x-2)*...*3*2*1.
        <br>
        The expected value of the Poisson distribution is λ, and the standard deviation is &#8730;λ.  
        '),
        effect = "cover"
      )
    ),
    use_sever(), #for nicer message on disconnet from shiny server
    f7Tabs(
      animated = FALSE,
      swipeable = FALSE,
      ############### EXPLORE TAB ########### 
      f7Tab(
        tabName = HTML("<center>Explore<br>Distribution</center>"),
        active = TRUE,
        f7Card(
          title = tagList(uiOutput("title0")),
          #plotlyOutput("distplot0", height='150px', width='100%'),
          apexchartOutput("distplot0", height='180px'),
          #echarts4rOutput("distplot0", height='180px'),
          #height="10px",
          footer = tagList(textOutput("meansd0")),
        ),
        # conditionalPanel(
        #   condition = "!input.numeric",
        f7Block(
          f7Slider(
            inputId = "l0",
            label = uiOutput("labell0"),
            min = 0,
            max = 10,
            value = 2,
            step = 0.2,
            scale = FALSE,
            scaleSteps = 0.2,
            scaleSubSteps = 0,
            vertical = FALSE,
            verticalReversed = FALSE,
            labels = NULL,
            color = "yellow",
            noSwipping = TRUE
          )
        ),
        tags$br(),
        # ),
        # conditionalPanel(
        #   condition = "input.numeric",
        #   f7Row(
        #     f7Col(
        #       f7Text(
        #         inputId = 'n00',
        #         label = 'Number of Trials n:',
        #         value = 10
        #         #placeholder = 10
        #       )
        #     ),
        #     f7Col(
        #       f7Text(
        #         inputId = 'p00',
        #         label = 'Probability p:',
        #         value = 0.3
        #         #placeholder = 0.3
        #       )
        #     )
        #   )
        # ),
        # f7Block(
        #   f7Toggle(
        #   inputId = 'numeric',
        #   label = 'Enter numerical values for n and p:',
        #   color = 'yellow'
        #   )
        # ),
        # f7Block(
        #   prettyCheckbox(
        #     inputId = 'numeric',
        #     label = 'Enter numerical values for n and p:',
        #     status = "warning",
        #     outline = TRUE,
        #     fill = FALSE,
        #     bigger = TRUE
        #   )
        # ),
        f7Card(
          title = uiOutput("titleprobtable0"),
          tableOutput("probtable0"),
          footer = tagList(textOutput("meansd00")),
          #height="20px"
        )
      ),
      ############### FIND PROB TAB ###########
      f7Tab(
        tabName = HTML("<center>Find<br>Probability</center>"),
        active = FALSE,
        f7Flex(
          f7Card(
            title = "Rate of Events (λ):",
            f7Stepper(
              inputId = "l",
              label = "",
              min = 0,
              max = 10000,
              value = 2,
              step = 1,
              color = "yellow",
              #size = 'large',
              #raised = TRUE,
              #fill = TRUE,
              rounded = TRUE,
              decimalPoint=1,
              manual = TRUE
            )
          )
        ),
        f7Card(
          title = uiOutput("title"),
          apexchartOutput("distplot", height='150px'),
          footer = tagList(textOutput("meansd"))
        ),
        f7Flex(
          conditionalPanel(condition='input.type != "Interval"',
                           f7Col(
                             f7Card(
                               title="Value of x:",
                               f7Stepper(
                                 inputId = "x",
                                 label = "",
                                 min = 0,
                                 max= 10000,
                                 value = 2,
                                 step = 1,
                                 #size='large',
                                 color = "yellow",
                                 decimalPoint=0,
                                 manual=TRUE
                               )
                             )
                           )
          ),
          conditionalPanel(condition='input.type == "Interval"',
                           f7Col(
                             f7Card(
                               title="Lower Bound:",
                               f7Stepper(
                                 inputId = "x1",
                                 label = "",
                                 min = 0,
                                 max= 10000,
                                 value = 1,
                                 step = 1,
                                 color = "yellow"
                               )
                             )
                           ),
                           f7Col(
                             f7Card(
                               title="Upper Bound:",
                               f7Stepper(
                                 inputId = "x2",
                                 label = "",
                                 min = 0,
                                 max= 10000,
                                 value = 3,
                                 step = 1,
                                 color = "yellow"
                               )
                             )
                           )
          ),
          f7Block(
            f7Select(
              inputId = "type",
              label = "Type of Probability:",
              choices = c("P(X = x)",
                          "Lower Tail",
                          "Upper Tail",
                          "Interval"
              ),
              selected = "P(X = x)"
            )
          )
        ), #close Flex
        f7Card(
          title = "Probability:",
          uiOutput("probtablex")
        ),
        tags$br(),
        f7Card(
          title = uiOutput("titleprobtable"),
          tableOutput("probtable"),
          footer = tagList(textOutput("meansd1"))
        )
      ) #close Tab
    ) #close Tabs
  ) #close Layout
) #close Page