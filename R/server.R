library(plotly)

source("utils.R")

shinyServer(function(input, output, session){
  
  output$histo <- renderPlotly({
    lambda <- input$lambda
    
    xs <- 0:20
    ys <- dpois(xs, lambda)
    hovertext <- paste0("<b>Probability:</b> ", pct(ys), "<br>")
    
    plot_ly(x = xs, y = ys, text = hovertext, hoverinfo = "text", type = "bar") %>%
      layout(yaxis = list(range = c(0, .4)))
  })
})