library(plotly)
library(rhandsontable)
library(dplyr)
library(readr)

source("utils.R")

shinyServer(function(input, output, session){
  
  rv <- reactiveValues(df = NULL, hovered = NULL)
  
  output$histo <- renderPlotly({
    rv$df <- data_frame(
      xs = 0:20,
      ys = dpois(xs, input$lambda),
      hovertext = paste0("<b>Probability:</b> ", pct(ys), "<br>")
    )
    
    #validate(need(input$xrange[1] < input$xrange[2], "Please select a valid range."))
    
    plot_ly(data = rv$df, x = ~factor(xs), y = ~ys, text = ~hovertext, hoverinfo = "text", type = "bar", source = "source") %>%
      layout(xaxis = list(range = input$xrange, title = "x"),
             yaxis = list(range = c(0, .4), title = "P(X=x)"))
  })
  
  output$freqtable <- renderRHandsontable({
    rv$df %>%
      mutate(ys = formatC(ys, format = "f", digits = 3)) %>%
      select(xs, ys) %>%
      rhandsontable(readOnly = TRUE, height = 200, index=rv$hovered,
                    colHeaders = c("x", "P(X=x)"), rowHeaders = FALSE) %>%
      hot_table(stretchH = "all") %>%
      hot_cols(manualColumnResize = TRUE, columnSorting = TRUE, halign = "htCenter",
               renderer = read_file(file = "js/highlight.js"))
  })
  
  observeEvent(event_data("plotly_hover", source = "source"), {
    eventdata <- event_data("plotly_hover", source = "source")
    req(!is.null(eventdata))
    pointNumber <- as.numeric(eventdata$pointNumber)[1]
    curveNumber <- as.numeric(eventdata$curveNumber)[1]
    req(curveNumber == 0)
    
    rv$hovered <- pointNumber
    js$focus(id = "freqtable", hovered = rv$hovered, last = nrow(rv$df) - 1)
  })
})