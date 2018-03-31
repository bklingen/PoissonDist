library(plotly)
library(rhandsontable)
library(dplyr)
library(readr)
library(rlang)
library(RColorBrewer)

mycol <- c(brewer.pal(6, "Paired")[3], brewer.pal(9, "PiYG")[c(5,8,6)])
ProbsNames <- list("type1", "type2", "type3", "type4")
ProbsCodes <- list(HTML("Poisson Probability: P(X = x)"), HTML("Lower Tail: P(X &le; x)"), HTML("Upper Tail: P(X &ge; x)"), HTML("Interval: P(x<sub>1</sub> &le; X &le; x<sub>2</sub>)"))
Probs <- setNames(ProbsNames, ProbsCodes)

pct <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

shinyServer(function(input, output, session){
  
rv <- reactiveValues(df1 = NULL, hovered1 = NULL, df2 = NULL, hovered2 = NULL, df3 = NULL, hovered3 = NULL)

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
  if(input$type!="type4") x <- req(input$x,cancelOutput = TRUE)
  else {x1 <- req(input$x1,cancelOutput = TRUE); x2 <- req(input$x2,cancelOutput = TRUE)}
  df$selected <- switch(input$type,
    "type1" = {subtitle <- paste0("P(X = ", x, ") = ", pct(dpois(x,lambda))); df$xs == x},
    "type2" = {subtitle <- paste0("P(X ≤ ", x, ") = ", pct(ppois(x,lambda))); df$xs <= x},
    "type3" = {subtitle <- paste0("P(X ≥ ", x, ") = ", pct(1-ppois(x-1,lambda))); df$xs >= x},
    "type4" = {subtitle <- paste0("P(",x1," ≤ X ≤ ",x2,") = ", pct(ppois(x2,lambda)-ppois(x1-1,lambda))); (df$xs >= x1) & (df$xs <= x2)}
  )
  if(any(df$selected)) mycol1 <- mycol[2:1] else mycol1=mycol[2] #gray bars and green bars
  rv$df2 <- df
  myhovertext <- c(rbind(paste("<b>", c("Number of Events","Probability"), ":</b> ", sep=""),
                         lapply(c("xs","Prob"), sym),
                         c("<br>","<br>")))
  plot_ly(data = df, x = ~factor(xs), y = ~ys, color=~selected, colors=mycol1, type = "bar", source = "plot2",
          marker = list(line = list(color = '#000000', width = 1)),
          text = ~do.call(paste0,myhovertext), hoverinfo = "text+x",
          height = 330) %>%
    layout(xaxis = list(title = "Number of Events x",ticks="outside"),
           yaxis = list(title = "Probability P(X = x)"),
           showlegend = FALSE,
           hovermode = 'x',
           margin = list(t=60, b=45)
    ) %>%
    add_annotations(text="The Poisson Distribution", showarrow=FALSE, font=list(size=17), x=0.5, xref='paper', xanchor='top', y=1.27, yref='paper') %>%
    add_annotations(text=subtitle, font=list(size=14, color=mycol[3]), showarrow=FALSE, font=list(size=13), x=0.5, xref='paper', xanchor='top', y=1.17, yref='paper') %>%
    config(collaborate = FALSE, displaylogo = FALSE, modeBarButtonsToRemove = list('resetScale2d', 'sendDataToCloud', 'zoom2d', 'zoomIn2d', 'zoomOut2d', 'pan2d', 'select2d', 'lasso2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'hoverClosestGl2d', 'hoverClosestPie', 'toggleHover', 'resetViews', 'toggleSpikelines'))
})

output$freqtable2 <- renderRHandsontable({
  rv$df2 %>%
    mutate(ys = formatC(ys, format = "f", digits = 3)) %>%
    select(xs, ys) %>%
    rhandsontable(readOnly = TRUE, height = 150, width=180, index=rv$hovered2,
                  colHeaders = c("x", "P(X = x)"), rowHeaders = FALSE) %>%
    hot_table(stretchH = "all") %>%
    hot_cols(manualColumnResize = TRUE, columnSorting = TRUE, halign = "htCenter",
             renderer = read_file(file = "js/highlight.js"))
})

# observeEvent(event_data("plotly_hover", source = "plot2"), {
#   if(!input$showprob) return(NULL)
#   eventdata <- req(event_data("plotly_hover", source = "plot2"))
#   pointNumber <- as.numeric(eventdata$x)
#   #curveNumber <- as.numeric(eventdata$curveNumber)[1]
#   #req(curveNumber == 0)
#   rv$hovered2 <- pointNumber
#   js$focus(id = "freqtable2", hovered = rv$hovered2, last = nrow(rv$df2) - 1)
# })

# output$probtable <- renderTable({
#   switch(input$type,
#     "type1" = {df <- data.frame(x=input$x, y=dpois(input$x, lambda = input$lambda1))
#                colnames(df) <- c(" x ", "$P(X = x)$")},
#     "type2" = {df <- data.frame(x=input$x, y=ppois(input$x, lambda = input$lambda1))
#                colnames(df) <- c(" x ", " $P(X \\le x)$ ")},
#     "type3" = {df <- data.frame(x=input$x, y=1-ppois(input$x-1, lambda = input$lambda1))
#                colnames(df) <- c(" x "," P(X ≥ x) ")},
#     "type4" = {df <- data.frame(x1=input$x1, x2=input$x2, y=1-ppois(input$x-1, lambda = input$lambda1))
#     colnames(df) <- c(" x<sub>1</sub> ", " x<sub>2</sub> ", " P(x<sub>1</sub> ≤ X ≤ x<sub>1</sub>) ")}
#   )
#   return(df)
# }, border=TRUE, striped=FALSE, hover=TRUE, digits=4,
# caption = "<b> <u> <span style='color:#000000'> Probability: </u> </b>",
# caption.placement = getOption("xtable.caption.placement", "top"),
# sanitize.text.function = function(x){x} #for printing math symbols
# ## this works but would need a separate table for each type
# #include.colnames=FALSE,
# #add.to.row = list(pos = list(0), command = " <tr> <th> x </th> <th> P(X &le; x) </th> </tr>" ) 
# )

output$probtable1 <- renderTable({
  df <- data.frame(x=input$x, y=dpois(input$x, lambda = input$lambda1))
  return(df)
}, border=TRUE, striped=FALSE, hover=TRUE, digits=4,
caption = "<b> <u> <span style='color:#000000'> Probability: </u> </b>",
caption.placement = getOption("xtable.caption.placement", "top"),
#sanitize.text.function = function(x){x} #for printing math symbols, doesn't seem to work
## this works but would need a separate table for each type
include.colnames=FALSE,
add.to.row = list(pos = list(0), command = " <tr> <th> x </th> <th> P(X = x) </th> </tr>" ) 
)

output$probtable2 <- renderTable({
  df <- data.frame(x=input$x, y=ppois(input$x, lambda = input$lambda1))
  return(df)
}, border=TRUE, striped=FALSE, hover=TRUE, digits=4,
caption = "<b> <u> <span style='color:#000000'> Probability: </u> </b>",
caption.placement = getOption("xtable.caption.placement", "top"),
#sanitize.text.function = function(x){x} #for printing math symbols, doesn't seem to work
## this works but would need a separate table for each type
include.colnames=FALSE,
add.to.row = list(pos = list(0), command = " <tr> <th> x </th> <th> P(X &le; x) </th> </tr>" ) 
)

output$probtable3 <- renderTable({
  df <- data.frame(x=input$x, y=1-ppois(input$x-1, lambda = input$lambda1))
  return(df)
}, border=TRUE, striped=FALSE, hover=TRUE, digits=4,
caption = "<b> <u> <span style='color:#000000'> Probability: </u> </b>",
caption.placement = getOption("xtable.caption.placement", "top"),
#sanitize.text.function = function(x){x} #for printing math symbols, doesn't seem to work
## this works but would need a separate table for each type
include.colnames=FALSE,
add.to.row = list(pos = list(0), command = " <tr> <th> x </th> <th> P(X &ge; x) </th> </tr>" ) 
)

output$probtable4 <- renderTable({
  df <- data.frame(x1=input$x1, x2=input$x2, y=ppois(input$x2,input$lambda1)-ppois(input$x1-1, lambda = input$lambda1))
  return(df)
}, border=TRUE, striped=FALSE, hover=TRUE, digits=4,
caption = "<b> <u> <span style='color:#000000'> Probability: </u> </b>",
caption.placement = getOption("xtable.caption.placement", "top"),
#sanitize.text.function = function(x){x} #for printing math symbols, doesn't seem to work
## this works but would need a separate table for each type
include.colnames=FALSE,
add.to.row = list(pos = list(0), command = " <tr> <th> x<sub>1</sub> </th> <th> x<sub>2</sub> </th> <th> P(x<sub>1</sub> &le; X &le; x<sub>2</sub>) </th> </tr>" ) 
)

######################
## Quantile Tab ######
######################

output$bar2 <- renderPlotly({
  lambda <- req(input$lambda2,cancelOutput = TRUE)
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
  if(input$qtype=="one") {p <- req(input$p/100,cancelOutput = TRUE); q <- qpois(p, lambda=lambda)}
  else {p <- req(input$p1/100,cancelOutput = TRUE); q <- qpois(c(p/2,1-p/2), lambda=lambda)}
  switch(input$qtype,
    "one" = {subtitle <- paste0("P(X ≤ ", q, ") = ", pct(ppois(q,lambda)), " and P(X ≤ ", q-1, ") = ", pct(ppois(q-1,lambda))); df$selected <- df$xs <= q},
    "two" = {subtitle <- paste0("P( X ≤ ",q[1],") + P(X ≥ ", q[2],") = ", pct(1-(ppois(q[2],lambda)-ppois(q[1]-1,lambda)))); df$selected <- (df$xs <= q[1]) | (df$xs >= q[2])}
  )
  if(any(df$selected)) mycol1 <- mycol[c(2,4)] else mycol1=mycol[2] #gray bars and green bars
  rv$df3 <- df
  myhovertext <- c(rbind(paste("<b>", c("Number of Events","Probability"), ":</b> ", sep=""),
                         lapply(c("xs","Prob"), sym),
                         c("<br>","<br>")))
  plot_ly(data = df, x = ~factor(xs), y = ~ys, color=~selected, colors=mycol1, type = "bar", source = "plot3",
          marker = list(line = list(color = '#000000', width = 1)),
          text = ~do.call(paste0,myhovertext), hoverinfo = "text+x",
          height = 330) %>%
    add_trace(x=factor(q), y=0, type="scatter", mode="markers", marker=list(color=mycol[1], size=15, line = list(color = '#000000', width = 2)),
              text=paste0(100*p, "% Quantile: ",q), hoverinfo="text", inherit=FALSE) %>%
    layout(xaxis = list(title = "Number of Events x",ticks="outside"),
           yaxis = list(title = "Probability P(X = x)", rangemode = "tozero"),
           showlegend = FALSE,
           hovermode = 'x',
           margin = list(t=60, b=45)
    ) %>%
    add_annotations(text="The Poisson Distribution", showarrow=FALSE, font=list(size=17), x=0.5, xref='paper', xanchor='top', y=1.27, yref='paper') %>%
    add_annotations(text=subtitle, showarrow=FALSE, font=list(size=14, color=mycol[3]), x=0.5, xref='paper', xanchor='top', y=1.17, yref='paper') %>%
    config(collaborate = FALSE, displaylogo = FALSE, modeBarButtonsToRemove = list('resetScale2d', 'sendDataToCloud', 'zoom2d', 'zoomIn2d', 'zoomOut2d', 'pan2d', 'select2d', 'lasso2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'hoverClosestGl2d', 'hoverClosestPie', 'toggleHover', 'resetViews', 'toggleSpikelines'))
})

output$quantable <- renderTable({
  switch(input$qtype,
    "one" = {p <- input$p; df <- data.frame(q=qpois(p/100, lambda = input$lambda2))
      colnames(df) <- paste0(input$p, "% Quantile ")},
    "two" = {p <- input$p1; df <- data.frame(q1=qpois(p/200, lambda = input$lambda2), q2=qpois(1-p/200, lambda = input$lambda2))
               colnames(df) <- c(paste0(p/2, "% Quantile "),paste0(100-p/2, "% Quantile "))}
  )
  return(df)
}, border=TRUE, striped=FALSE, hover=TRUE, digits=0,
caption = "<b> <u> <span style='color:#000000'> Quantiles: </u> </b>",
caption.placement = getOption("xtable.caption.placement", "top"),
sanitize.text.function = function(x){x} #for printing math symbols
## this works but would need a separate table for each type
#include.colnames=FALSE,
#add.to.row = list(pos = list(0), command = " <tr> <th> x </th> <th> P(X &le; x) </th> </tr>" )
)


output$freqtable3 <- renderRHandsontable({
  rv$df3 %>%
    mutate(ys = formatC(ys, format = "f", digits = 3)) %>%
    select(xs, ys) %>%
    rhandsontable(readOnly = TRUE, height = 150, width=180, index=rv$hovered2,
                  colHeaders = c("x", "P(X = x)"), rowHeaders = FALSE) %>%
    hot_table(stretchH = "all") %>%
    hot_cols(manualColumnResize = TRUE, columnSorting = TRUE, halign = "htCenter",
             renderer = read_file(file = "js/highlight.js"))
})


  
})