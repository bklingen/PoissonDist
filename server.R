library(plotly)
library(rhandsontable)
library(dplyr)
library(readr)
library(rlang)
library(RColorBrewer)

mycol <- c(brewer.pal(6, "Paired")[3],brewer.pal(9, "PiYG")[5])
ProbsNames <- list("type1", "type2", "type3", "type4")
ProbsCodes <- list(HTML("Poisson Probability: P(X = x)"), HTML("Lower Tail: P(X &le; x)"), HTML("Upper Tail: P(X &ge; x)"), HTML("Interval: P(x<sub>1</sub> &le; X &le; x<sub>2</sub>)"))
Probs <- setNames(ProbsNames, ProbsCodes)

pct <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

shinyServer(function(input, output, session){
  
rv <- reactiveValues(df1 = NULL, hovered1 = NULL, df2 = NULL, hovered2 = NULL)

output$bar <- renderPlotly({
  df <- data_frame(
    xs = 0:25,
    ys = dpois(xs, input$lambda)
    #hovertext = paste0("<b>Probability:</b> ", pct(ys), "<br>")
  )
  df$Prob <- pct(df$ys)
  myhovertext <- c(rbind(paste("<b>", c("Number of Events","Probability"), ":</b> ", sep=""),
                         lapply(c("xs","Prob"), sym),
                         c("<br>","<br>")))
  updateNumericInput(session, "lambda1", value=input$lambda)
  if(input$type!="type4") updateNumericInput(session, "x", value=floor(input$lambda))
  else {
    updateNumericInput(session, "x1", value=max(0, floor(input$lambda-2)))
    updateNumericInput(session, "x2", value=floor(input$lambda+2))
  }
  rv$df1 <- df
  plot_ly(data = df, x = ~factor(xs), y = ~ys, type = "bar", source = "plot1",
          marker = list(color=mycol[1], line = list(color = '#000000', width = 1)),
          text = ~do.call(paste0,myhovertext), hoverinfo = "text+x") %>%
    layout(xaxis = list(title = "Number of Events", range = c(-0.9,20.9), ticks="outside"),
           yaxis = list(title = "Probability", range = c(0, .41), showline=FALSE, rangemode='tozero'),
           title = "The Poisson Distribution",
           hovermode = 'x'
          ) %>%
    config(collaborate = FALSE, displaylogo = FALSE, modeBarButtonsToRemove = list('resetScale2d', 'sendDataToCloud', 'zoom2d', 'zoomIn2d', 'zoomOut2d', 'pan2d', 'select2d', 'lasso2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'hoverClosestGl2d', 'hoverClosestPie', 'toggleHover', 'resetViews', 'toggleSpikelines'))
})

output$freqtable1 <- renderRHandsontable({
  rv$df1 %>%
    mutate(ys = formatC(ys, format = "f", digits = 3)) %>%
    select(xs, ys) %>%
    rhandsontable(readOnly = TRUE, height = 200, index=rv$hovered1,
                  colHeaders = c("x", "P(X=x)"), rowHeaders = FALSE) %>%
    hot_table(stretchH = "all") %>%
    hot_cols(manualColumnResize = TRUE, columnSorting = TRUE, halign = "htCenter",
             renderer = read_file(file = "js/highlight.js"))
})

observeEvent(event_data("plotly_hover", source = "plot1"), {
  eventdata <- req(event_data("plotly_hover", source = "plot1"))
  pointNumber <- as.numeric(eventdata$pointNumber)[1]
  #curveNumber <- as.numeric(eventdata$curveNumber)[1]
  #req(curveNumber == 0)
  rv$hovered1 <- pointNumber
  js$focus(id = "freqtable1", hovered = rv$hovered1, last = nrow(rv$df1) - 1)
})

############################
## Find Probability Panel ##
############################
  
updateSelectizeInput(session, "type",
  choices = Probs,
  options = list(render = I("
                            {
                            item:   function(item, escape) { return '<div>' + item.label + '</div>'; },
                            option: function(item, escape) { return '<div>' + item.label + '</div>'; }
                            }
                            "))
  )

output$bar1 <- renderPlotly({
  lambda <- req(input$lambda1,cancelOutput = TRUE)
  sd <- sqrt(lambda)
  if(lambda<10){
    min <- floor(max(0,lambda-4.5*sd))
    max <- ceiling(max(10, lambda+4.5*sd))
  } else{
    min <- floor(max(0,lambda-3.5*sd))
    max <- ceiling(max(10, lambda+3.5*sd))
  }
  df <- data_frame(
    xs = min:max,
    ys = dpois(xs, lambda)
  )
  df$Prob <- pct(df$ys)
  df$selected <- req(input$x,cancelOutput = TRUE) == df$xs
  if(any(df$selected)) mycol1 <- mycol[2:1] else mycol1=mycol[2]
  rv$df2 <- df
  subtitle <- switch(input$type,
    "type1" = paste0("P(X = ", input$x, ")")
  )
  myhovertext <- c(rbind(paste("<b>", c("Number of Events","Probability"), ":</b> ", sep=""),
                         lapply(c("xs","Prob"), sym),
                         c("<br>","<br>")))
  plot_ly(data = df, x = ~factor(xs), y = ~ys, color=~selected, colors=mycol1, type = "bar", source = "plot2",
          marker = list(line = list(color = '#000000', width = 1)),
          text = ~do.call(paste0,myhovertext), hoverinfo = "text+x",
          height = 330) %>%
    layout(xaxis = list(title = "Number of Events x",ticks="outside"),
           yaxis = list(title = "Poisson Probability: P(X = x)"),
           showlegend = FALSE,
           hovermode = 'x',
           margin = list(t=60, b=45)
    ) %>%
    add_annotations(text="The Poisson Distribution", showarrow=FALSE, font=list(size=18), x=0.5, xref='paper', xanchor='top', y=1.27, yref='paper') %>%
    add_annotations(text=paste(subtitle, "=", pct(dpois(input$x,input$lambda1))), showarrow=FALSE, font=list(size=14), x=0.5, xref='paper', xanchor='top', y=1.17, yref='paper') %>%
    config(collaborate = FALSE, displaylogo = FALSE, modeBarButtonsToRemove = list('resetScale2d', 'sendDataToCloud', 'zoom2d', 'zoomIn2d', 'zoomOut2d', 'pan2d', 'select2d', 'lasso2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'hoverClosestGl2d', 'hoverClosestPie', 'toggleHover', 'resetViews', 'toggleSpikelines'))
})

# output$freqtable2 <- renderRHandsontable({
#   rv$df2 %>%
#     mutate(ys = formatC(ys, format = "f", digits = 3)) %>%
#     select(xs, ys) %>%
#     rhandsontable(readOnly = TRUE, height = 200, index=rv$hovered2,
#                   colHeaders = c("x", "P(X=x)"), rowHeaders = FALSE) %>%
#     hot_table(stretchH = "all") %>%
#     hot_cols(manualColumnResize = TRUE, columnSorting = TRUE, halign = "htCenter",
#              renderer = read_file(file = "js/highlight.js"))
# })
# 
# observeEvent(event_data("plotly_hover", source = "plot2"), {
#   eventdata <- req(event_data("plotly_hover", source = "plot2"))
#   pointNumber <- as.numeric(eventdata$pointNumber)[1]
#   #curveNumber <- as.numeric(eventdata$curveNumber)[1]
#   #req(curveNumber == 0)
#   rv$hovered2 <- pointNumber
#   js$focus(id = "freqtable2", hovered = rv$hovered2, last = nrow(rv$df2) - 1)
# })

output$probtable <- renderTable({
  switch(input$type,
    "type1" = {df <- data.frame(x=input$x, y=dpois(input$x, lambda = input$lambda1))
               colnames(df) <- c(" x ", HTML(" P(X = x) "))
    }
  )
  df
}, border=TRUE, striped=FALSE, hover=TRUE, digits=4,
caption = "<b> <u> <span style='color:#000000'> Probability: </u> </b>",
caption.placement = getOption("xtable.caption.placement", "top")
)


  
})