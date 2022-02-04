apexPoissonPlot <- function(plotdata, lambda, type=0, title="", subtitle="") {
  # if(type==0) {
  #   dtick=10
  #   myYtickAmount <- 3
  #   myfontSize <- '14px'
  #   myfontWeight <- 500
  # } else {
  #   dtick=5
  #   myYtickAmount <- 2
  #   myfontSize <- '13px'
  #   myfontWeight <- 300
  # }
  plot <- apex(data=plotdata,
               type='column',
               mapping=aes(x=factor(x), y=Probability, fillColor=color)
  ) %>%
    # ax_colors(mycolors) %>%
    # ax_title(
    #   text = title,
    #   align = "left",
    #   margin = -15,
    #   style = list(
    #     color = "white", #gray(0.75),
    #     fontSize = '15px',
    #     #fontFamily = list('Arial', 'Helvetica'),
    #     fontWeight = 500
    #   )
  # ) %>%
  # ax_subtitle(
  #   text = subtitle,
  #   align = "left",
  #   margin = 0,
  #   offsetY = 0,
  #   style = list(
  #     color = "white", #gray(0.75),
  #     fontSize = '14px',
  #     #fontFamily = list('Arial', 'Helvetica'),
  #     fontWeight = 500
  #   )
  # ) %>%
  ax_xaxis(
    title = list(
      text = "Number of Events",
      offsetY = -10,
      style = list(
        color = "white", #gray(0.75),
        fontSize = '14',
        #fontFamily = 'Helvetica, Arial',
        fontWeight = 500
      )
    ),
    #min = 1,
    #max = 21,
    tickAmount = 10,
    #forceNiceScale = TRUE,
    tickPlacement = 'on',
    axisTicks = list(
      color = "white"
    ),
    labels = list(
      rotate = 0,
      style = list(
        colors = "white"
      )
    ),
    axisBorder = list(
      show = TRUE,
      color = "white"
    )
  ) %>%
    ax_yaxis(
      title = list(
        text = "Probability",
        style = list(
          color = "white", #gray(0.75),
          fontSize = '13px', #myfontSize,
          #fontFamily = 'Courier New'
          fontWeight = 400 #myfontWeight
        )
      ),
      tickAmount = 3, #myYtickAmount,
      forceNiceScale = TRUE,
      #decimalsInFloat = 2
      axisTicks = list(
        show = TRUE,
        color = 'white'
      ),
      labels = list(
        formatter = format_num(".2f"),
        style = list(
          colors = "white"
        )
      ),
      axisBorder = list(
        show = FALSE,
        color = "white"
      )
    ) %>%
    ax_grid(
      #borderColor = "white",
      yaxis = list(
        lines = list(
          show=FALSE
        )
      ),
      padding = list(
        top = -22,
        bottom = -15
        #left = 20,
        #right = 0
      )
    ) %>%
    ax_chart(
      toolbar = list(show=FALSE),
      zoom = list(enabled = FALSE),
      animations = list(
        speed=100
      ),
      animateGradually = list(
        delay = 550
      ),
      dynamicAnimation = list(
        speed=100
      )
    ) %>%
    ax_tooltip(
      enabled = TRUE,
      #followCursor = FALSE,
      theme="dark",
      x = list(
        show = FALSE,
        formatter = htmlwidgets::JS("function(value) {return ' Events: ' +value}")
      ),
      y = list(
        # title = list(
        #   formatter = htmlwidgets::JS("function(value) {return ' Successes: ' +value}")
        # ),
        formatter = format_num(".3f") #htmlwidgets::JS("function(value) {return ' Successes: ' +value}") #
      ),
      fixed = list(
        enabled = TRUE,
        position = 'topright'
      ),
      marker = list(
        show = FALSE
      )
    )
}

# binomialPlot <- function(plotdata, n, p, x=NULL, type="Binomial: P(X = x)") {
#   if (n<=15) dtick=1 else dtick=NA
#   mycolors <- c('#FDE7B5', 'orange')
#   g <- plot_ly(
#     data = plotdata,
#     type = "bar",
#     x = ~x,
#     y = ~Probability,
#     color = ~ highlight,
#     colors = mycolors,
#     #marker = list(color = '#FDE7B5'),
#     hovertemplate = 'Probability: %{y:.3%}<extra></extra>'
#     #height=150
#   ) %>% 
#   layout(
#     title = list(
#       text = "" #paste0("n = ", n,", p = ", p),
#       #font = list(color="white", size=14)
#     ),
#     xaxis = list(
#       title = "Number of Successes",
#       titlefont = list(color="white", size=12),
#       ticks = 'outside',
#       dtick = dtick,
#       zeroline = FALSE,
#       showgrid = FALSE,
#       color = "white",
#       fixedRange=TRUE
#     ),
#     yaxis = list(
#       title = "Probability",
#       titlefont = list(color="white", size=12),
#       #showline = FALSE,
#       #showticklabels = FALSE,
#       ticks = 'outside',
#       zeroline = TRUE,
#       showgrid = FALSE,
#       color = "white",
#       fixedRange=TRUE
#     ),
#     margin = list(t=0, r=10, b=0, l=50),
#     paper_bgcolor='rgba(1,1,1,0)',
#     plot_bgcolor='rgba(1,1,1,0)',
#     hovermode = "x",
#     showlegend = FALSE,
#     dragmode=FALSE
#   )
#   g <- g %>% config(
#     #mathjax = "cdn",
#     displaylogo = FALSE,
#     displayModeBar = FALSE
#     #modeBarButtons = list(list("toImage"))
#   )
#   
#   return(g)
# }

# echartsBinomialPlot <- function(plotdata, n, p, x=NULL, type=NULL) {
#   e_charts(data=plotdata, x=x) %>% e_bar(serie=Probability) %>% e_theme("dark")
# }
