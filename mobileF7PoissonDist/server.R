library(shiny)
library(shinyMobile)
library(apexcharter)
#library(plotly)
library(sever)
#library(kableExtra)

mycol1 <- '#b7f7b2'   #'#ffdf3d'
mycol2 <- '#14ff00'   #'#fffef0'

shinyServer(function(input, output, session) {

## Handle shiny disconnnect messages gracefully on mobile device
sever::sever(
  tagList(
    h1("Please Reload the App"),
    #p("After a certain time of inactivity, the app needs to be reloaded."),
    shiny::tags$button(
      "Reload",
      style = "color:#000;background-color:#fff;",
      class = "button button-raised",
      onClick = "location.reload();"
    )
    # f7_reload_button(
    #   text = "Reload",
    #   color = '#FDE7B5'
    # )
  ),
  bg_color = "#000"
)

########  EXPLORE TAB  #########
lambda0 <- reactive({
  lambda <- input$l0
  #req(lambda>=0)
})

plotdata0 <- reactive({
  x <- 0:20
  y <- dpois(x, lambda0())
  data.frame(x=x, Probability=y, color=mycol1)
})

observeEvent(list(input$l0),{
  updateF7Stepper(inputId = "l", value = lambda0())
})

output$title0 <- renderUI({
  HTML(paste0("Graph of Distribution: &lambda; = ",lambda0()))
})

output$titleprobtable0 <- renderUI({
  HTML(paste0("Table of Poisson Probabilities <br> &lambda; = ",lambda0()))
})

output$meansd0 <- output$meansd00 <- renderText({
  mu <- lambda0()
  sig <- sqrt(lambda0())
  paste0("Mean: ", mu, ", Standard Deviation: ",format(sig,digits=4))
})

output$distplot0 <- renderApexchart({
  # if(lambda0() <= 6)  df <- plotdata0()[1:13,]
  # else df <- plotdata0()
  apexPoissonPlot(plotdata0(), lambda0(), type=0)
})

# observeEvent(list(input$l0), {
#   xmax <- qpois(0.999, lambda())
#   print(xmax)
#   if(lambda0() > 6) {
#     apexchartProxy("distplot0") %>%
#       ax_proxy_options(
#         list(
#           xaxis = list(
#             #min = min,
#             max = 20,
#             #range = range,
#             tickAmount = 5,
#             labels = list(
#               formatter = format_num(".0f")
#             )
#           )
#           # yaxis = list(
#           #   show = FALSE,
#           #   min = 0,
#           #   max = ymax
#           # )
#         )
#       )
#   } else {
#     if(lambda0() > 6) {
#       apexchartProxy("distplot0") %>%
#         ax_proxy_options(
#           list(
#             xaxis = list(
#               min = 0,
#               max = 12,
#               #range = range,
#               tickAmount = 12,
#               labels = list(
#                 formatter = format_num(".0f")
#               )
#             )
#           )
#         )
#     }
#     
#   }
# 
# })


output$labell0 <- renderUI({
  HTML(paste0("Rate of Events: &lambda; = ", lambda0()))
})

output$probtable0 <- renderTable({
  df <- plotdata0()[,c(1,2)]
  df <- cbind(df, ppois(df$x, lambda0()), ppois(df$x-1, lambda0(), lower=FALSE))
  colnames(df) <- c("x", "P(X = x)", HTML("P(X &le; x)"), HTML("P(X &ge; x)"))
  return(df)
},
striped=FALSE,
hover=TRUE,
bordered = TRUE,
digits=4,
sanitize.text.function = function(x){x}
)

########  PROBABILITY TAB  #########
lambda <- reactive({
  lambda <- input$l
  #req(lambda>=0)
})

plotdata <- reactive({
  xmin <- qpois(0.00005, lambda())
  xmax <- qpois(0.99995, lambda())
  x <- seq(xmin, xmax)
  y <- dpois(x, lambda())
  z <- factor(mycol2, levels=c(mycol2,mycol1), ordered=TRUE)
  df <- data.frame(x=x, Probability=y, color=z)
  xnew <- if(input$type!="Interval") input$x else req(c(input$x1,input$x2))
  if(xnew >= xmin && xnew <= xmax) {
    switch(input$type,
           "P(X = x)" = df$color[xnew+1-xmin] <- mycol1,
           "Lower Tail" = df$color[1:(xnew+1-xmin)] <- mycol1,
           "Upper Tail" = df$color[-(1:(xnew-xmin))] <- mycol1,
           "Interval" = df$color[(xnew[1]:xnew[2])+1-xmin] <- mycol1
    )
  }
  return(df)
})

xVal <- reactiveValues(x=0, min=0, max=12, mytext1=NULL, mytext2=NULL)

observeEvent(list(input$l), {
  xnew <- max(0, floor(lambda())-3)
  xVal$x <- xnew
  updateF7Stepper(inputId = "x", value = xnew)
  updateF7Stepper(inputId = "x1", value = max(0,floor(lambda())-3))
  updateF7Stepper(inputId = "x2", value = ceiling(lambda())+1)
})

observeEvent(list(input$l, input$x, input$x1, input$x2, input$type), {
  x <- input$x
  switch(input$type,
         "P(X = x)" = {
           myprob <- dpois(x, lambda())
           xVal$mytext1 <- paste0("P(X = ",x,") = ", format(round(myprob,3), nsmall=3, scientific=FALSE))
           xVal$mytext2 <- paste0("P(X = ",x,") = ", format(myprob, digits=5, nsmall=5, scientific=FALSE))
         },
         "Lower Tail" = {
           myprob <- ppois(x, lambda())
           xVal$mytext1 <- paste0("P(X &le; ",x,") = ", format(round(myprob,3), nsmall=3, scientific=FALSE))
           xVal$mytext2 <- paste0("P(X &le; ",x,") = ", format(myprob, digits=5, nsmall=5, scientific=FALSE))
         },
         "Upper Tail" = {
           myprob <- ppois(x-1, lambda(), lower=FALSE)
           xVal$mytext1 <- paste0("P(X &ge; ",x,") = ", format(round(myprob,3), nsmall=3, scientific=FALSE))
           xVal$mytext2 <- paste0("P(X &ge; ",x,") = ", format(myprob, digits=5, nsmall=5, scientific=FALSE))
         },
         "Interval" = {
           x1 <- req(input$x1); x2 <- req(input$x2)
           if(x1>x2) {
             xVal$mytext1 <- ""
             xVal$mytext2 <- HTML("Warning: x<sub>lower</sub> needs to be less than x<sub>upper</sub> !")
           } else {
             myprob <- ppois(x2, lambda()) - ppois(x1-1, lambda())
             xVal$mytext1 <- paste0("P(",x1," &le; X &le; ",x2,") = ", format(round(myprob, 3), nsmall=3, scientific=FALSE))
             xVal$mytext2 <- paste0("P(",x1," &le; X &le; ",x2,") = ", format(myprob, digits=5, nsmall=5, scientific=FALSE))
           }
         }
  )
})

output$title <- renderUI({
  HTML(paste0("Poisson Distribution: &lambda; = ", lambda(),"<br>", xVal$mytext1))
})

output$meansd <- output$meansd1 <- renderText({
  paste0("Mean: ", lambda(),", Standard Deviation: ",format(sqrt(lambda()),digits=4))
})

output$distplot <- renderApexchart({
  df <- req(plotdata())
  df <- df[df$Probability>=0.001,]
  #binomialPlot(df, n(), p(), x=x, type=input$type)
  apexPoissonPlot(df, lambda(), type=1)
})

output$probtablex <- renderUI({
  HTML(xVal$mytext2)
})

output$titleprobtable <- renderUI({
  HTML(paste0("Table of Poisson Probabilities <br> &lambda; = ",lambda()))
})

output$probtable <- renderTable({
  df <- plotdata()[,c(1,2)]
  df <- cbind(df, ppois(df$x, lambda()), ppois(df$x-1, lambda(), lower=FALSE))
  colnames(df) <- c("x", "P(X = x)", HTML("P(X &le; x)"), HTML("P(X &ge; x)"))
  return(df)
},
striped=FALSE,
hover=TRUE,
bordered = TRUE,
digits=4,
sanitize.text.function = function(x){x}
)

})