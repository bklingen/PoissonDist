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
    
    validate(need(input$xrange[1] < input$xrange[2], "Please select a valid range."))
    
    plot_ly(data = rv$df, x = ~xs, y = ~ys, text = ~hovertext, hoverinfo = "text", type = "bar", source = "source") %>%
      layout(xaxis = list(range = input$xrange, title = "k"),
             yaxis = list(range = c(0, .4), title = "P(X=k)"))
  })
  
  output$freqtable <- renderRHandsontable({
    rv$df %>%
      mutate(ys = formatC(ys, format = "f", digits = 3)) %>%
      select(xs, ys) %>%
      rhandsontable(readOnly = TRUE, height = 230, index=rv$hovered,
                    colHeaders = c("Number of occurrences", ("Probability")), rowHeaders = FALSE) %>%
      hot_table(stretchH = "all") %>%
      hot_cols(manualColumnResize = TRUE, columnSorting = TRUE, halign = "htCenter",
               renderer = read_file(file = "../js/highlight.js")) %>%
      hot_col(1, colWidths = 25)
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