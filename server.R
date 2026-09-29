library(shiny)
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

## a number as entered, without trailing zeros (0.125, not 0.13 or 0.1250)
num <- function(v) format(signif(v, 10), scientific = FALSE, trim = TRUE, drop0trailing = TRUE)

## lambda must be a number >= 0
validLambda <- function(lambda) !is.null(lambda) && !is.na(lambda) && lambda >= 0
## Inside a render function: show a message instead of the output when lambda is invalid.
checkLambda <- function(lambda) {
  validate(need(!is.na(lambda) && lambda >= 0, "The rate λ has to be a number of at least 0."))
}

htmlSigns <- function(s) gsub("≥", "&ge;", gsub("≤", "&le;", s))  # for HTML tables

poisProb <- function(type, lambda, x = NULL, x1 = NULL, x2 = NULL) {
  ## Probability for the "Find Probabilities" tab, plus its label.
  ## X only takes whole numbers, so for a non-whole x the label adds the
  ## equivalent whole-number event, e.g. P(X >= 12.5) = P(X >= 13).
  ## Returns list(label, prob, from, to): from/to are the values of X included.
  switch(type,
    "type1" = {
      k <- if (x == round(x)) x else NA
      list(label = paste0("P(X = ", x, ")"),
           prob = if (is.na(k)) 0 else dpois(k, lambda), from = k, to = k)
    },
    "type2" = {
      k <- floor(x)
      list(label = paste0("P(X ≤ ", x, ")", if (k != x) paste0(" = P(X ≤ ", k, ")")),
           prob = ppois(k, lambda), from = 0, to = k)
    },
    "type3" = {
      k <- ceiling(x)
      list(label = paste0("P(X ≥ ", x, ")", if (k != x) paste0(" = P(X ≥ ", k, ")")),
           prob = ppois(k - 1, lambda, lower.tail = FALSE), from = k, to = Inf)
    },
    "type4" = {
      a <- ceiling(x1); b <- floor(x2)
      list(label = paste0("P(", x1, " ≤ X ≤ ", x2, ")",
                          if (a != x1 || b != x2) paste0(" = P(", a, " ≤ X ≤ ", b, ")")),
           prob = if (a <= b) ppois(b, lambda) - ppois(a - 1, lambda) else 0, from = a, to = b)
    }
  )
}

## Highlights the table rows listed in `index` (0-based)
highlightRenderer <- "function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        td.style.color = 'black';
        if (instance.params) {
           hrows = instance.params.index
           hrows = hrows instanceof Array ? hrows : [hrows]
         }
         if (instance.params && hrows.includes(row)) td.style.background = '#B2DF8A'
      }
   "

shinyServer(function(input, output, session){

## Start-up promo for the Art of Stat mobile app. Art of Stat only; the modal lives in artofstat_modal.R.
if (!lumen) {
  source("artofstat_modal.R", local = TRUE)
  observe(show_artofstat_modal())
}

rv <- reactiveValues(df1 = NULL, hovered1 = NULL, df2 = NULL, hovered2 = NULL)

#################
## Explore Tab ##
#################

## Copy lambda from Explore to the other tabs
observeEvent(input$lambda, {
  updateNumericInput(session, "lambda1", value=input$lambda)
  updateNumericInput(session, "lambda4", value=input$lambda)
  updateSliderInput(session, "lambda3", value=input$lambda)
})

output$bar <- renderPlotly({
  lambda <- req(input$lambda)
  df <- data.frame(xs = 0:25)
  df$ys <- dpois(df$xs, lambda)
  df$Prob <- pct(df$ys)
  myhovertext <- c(rbind(paste("<b>", c("Number of Events","Probability"), ":</b> ", sep=""),
                         lapply(c("xs","Prob"), sym),
                         c("<br>","<br>")))
  rv$df1 <- df
  ## y-axis fixed at 0.52 so shapes can be compared across lambda; taller for small lambda, where P(X = 0) is larger
  ymax <- max(0.52, 1.05 * max(df$ys))
  plot_ly(data = df, x = ~factor(xs), y = ~ys, type = "bar", source = "plot1",
          marker = list(color=mycol[1], line = list(color = '#000000', width = 1)),
          hovertext = ~do.call(paste0,myhovertext), hoverinfo = "text+x") %>%
    layout(xaxis = list(title = "Number of Events", range = c(-0.9,20.9), ticks="outside"),
           yaxis = list(title = "Probability", range = c(0, ymax), showline=FALSE, rangemode='tozero'),
           hovermode = 'x',
           margin = list(t=50, b=45)
          ) %>%
    add_annotations(text=paste0("The Poisson Distribution with λ = ", lambda), showarrow=FALSE, font=list(size=17), x=0.5, xref='paper', xanchor='top', y=1.16, yref='paper') %>%
    add_annotations(text=paste0("Mean = ", lambda, ", Standard Deviation = ", round(sqrt(lambda),3)), font=list(size=13), showarrow=FALSE, x=0.5, xref='paper', xanchor='top', y=1.08, yref='paper') %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = list('resetScale2d', 'sendDataToCloud', 'zoom2d', 'zoomIn2d', 'zoomOut2d', 'pan2d', 'select2d', 'lasso2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'hoverClosestGl2d', 'hoverClosestPie', 'toggleHover', 'resetViews', 'toggleSpikelines'))
})

output$freqtable1 <- renderRHandsontable({
  req(rv$df1) %>%
    mutate(ys = formatC(ys, format = "f", digits = 3)) %>%
    select(xs, ys) %>%
    rhandsontable(readOnly = TRUE, height = 160, index=rv$hovered1,
                  colHeaders = c("x", "P(X=x)"), rowHeaders = FALSE) %>%
    hot_table(stretchH = "all") %>%
    hot_cols(manualColumnResize = TRUE, columnSorting = TRUE, halign = "htCenter", renderer = highlightRenderer)
})

observeEvent(event_data("plotly_hover", source = "plot1"), {
  eventdata <- req(event_data("plotly_hover", source = "plot1"))
  pointNumber <- as.numeric(eventdata$pointNumber)[1]
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

## x, x1 and x2 follow lambda, so the selected values stay where the distribution is
observeEvent(input$lambda1, {
  lambda <- input$lambda1
  if (!validLambda(lambda)) return()
  updateNumericInput(session, "x", value=floor(lambda))
  updateNumericInput(session, "x1", value=qpois(0.25, lambda))
  updateNumericInput(session, "x2", value=qpois(0.75, lambda))
})

output$bar1 <- renderPlotly({
  lambda <- req(input$lambda1, cancelOutput = TRUE)
  checkLambda(lambda)
  sd <- sqrt(lambda)
  if(lambda<10){
    min <- floor(max(0,lambda-4.5*sd))
    max <- ceiling(max(10, lambda+4.5*sd))
  } else{
    min <- floor(max(0,lambda-3.5*sd))
    max <- ceiling(max(10, lambda+3.5*sd))
  }
  df <- data.frame(xs = min:max)
  df$ys <- dpois(df$xs, lambda)
  df$Prob <- pct(df$ys)
  if(input$type!="type4") x <- req(input$x,cancelOutput = TRUE)
  else {
    x1 <- req(input$x1,cancelOutput = TRUE); x2 <- req(input$x2,cancelOutput = TRUE)
    if(x1>x2) {updateNumericInput(session, "x1", value=min(x1,x2)); updateNumericInput(session, "x2", value=max(x1,x2))}
  }
  pp <- if(input$type != "type4") poisProb(input$type, lambda, x = x) else poisProb("type4", lambda, x1 = min(x1,x2), x2 = max(x1,x2))
  subtitle <- paste0(pp$label, " = ", pct(pp$prob))
  df$selected <- if(is.na(pp$from)) rep(FALSE, nrow(df)) else (df$xs >= pp$from) & (df$xs <= pp$to)
  mycol1 <- c("FALSE" = mycol[2], "TRUE" = mycol[1])  # gray = not included, green = included; named so all-selected bars are still green
  rv$df2 <- df
  rv$hovered2 <- which(df$selected) - 1  # table rows (0-based) of the values included
  myhovertext <- c(rbind(paste("<b>", c("Number of Events","Probability"), ":</b> ", sep=""),
                         lapply(c("xs","Prob"), sym),
                         c("<br>","<br>")))
  plot_ly(data = df, x = ~factor(xs), y = ~ys, color=~selected, colors=mycol1, type = "bar", source = "plot2",
          marker = list(line = list(color = '#000000', width = 1)),
          hovertext = ~do.call(paste0,myhovertext), hoverinfo = "text+x"
          ) %>%
    layout(xaxis = list(title = "Number of Events x",ticks="outside"),
           yaxis = list(title = "Probability P(X = x)"),
           showlegend = FALSE,
           hovermode = 'x',
           margin = list(t=60, b=45)
    ) %>%
    add_annotations(text=paste0("The Poisson Distribution with λ = ", lambda), showarrow=FALSE, font=list(size=17), x=0.5, xref='paper', xanchor='top', y=1.27, yref='paper') %>%
    add_annotations(text=subtitle, font=list(size=14, color=mycol[3]), showarrow=FALSE, x=0.5, xref='paper', xanchor='top', y=1.17, yref='paper') %>%
    config(displaylogo = FALSE,
           modeBarButtons = list(list('toImage','autoScale2d', 'resetScale2d'))
    )
})

output$freqtable2 <- renderRHandsontable({
  checkLambda(req(input$lambda1))
  req(rv$df2) %>%
    mutate(ys = formatC(ys, format = "f", digits = 3)) %>%
    select(xs, ys) %>%
    rhandsontable(readOnly = TRUE, height = 150, width=180, index=rv$hovered2,
                  colHeaders = c("x", "P(X = x)"), rowHeaders = FALSE) %>%
    hot_table(stretchH = "all") %>%
    hot_cols(manualColumnResize = TRUE, columnSorting = TRUE, halign = "htCenter", renderer = highlightRenderer)
})

## scroll the probability table to the first value included
observeEvent(list(input$showprob, rv$hovered2), {
  if(!isTRUE(input$showprob) || length(rv$hovered2) == 0) return()
  first <- rv$hovered2[1]; last <- nrow(rv$df2) - 1
  shinyjs::delay(500, js$focus(id = "freqtable2", hovered = first, last = last))  # wait until the table is drawn
})

## Tables under the graph: lambda, x and the probability as a percentage, like the graph's subtitle
probTable <- function(type) {
  lambda <- req(input$lambda1)
  checkLambda(lambda)
  if(type != "type4") {
    x <- req(input$x)
    pp <- poisProb(type, lambda, x = x)
    df <- data.frame(lambda = num(lambda), x = num(x), y = pct(pp$prob))
    colnames(df) <- c("&lambda;", "Value of x", paste0("Probability<br>", htmlSigns(pp$label)))
  } else {
    x1 <- req(input$x1); x2 <- req(input$x2)
    pp <- poisProb("type4", lambda, x1 = x1, x2 = x2)
    df <- data.frame(lambda = num(lambda), x1 = num(x1), x2 = num(x2), y = pct(pp$prob))
    colnames(df) <- c("&lambda;", "Value of x<sub>1</sub>", "Value of x<sub>2</sub>", paste0("Probability<br>", htmlSigns(pp$label)))
  }
  df
}

output$caption1 <- renderUI(HTML("<b> <u> <span style='color:#000000'> Poisson Probability: </u> </b>"))
output$probtable1 <- renderTable(probTable("type1"), border=FALSE, striped=FALSE, hover=TRUE, align="c",
                                 sanitize.text.function = function(x) x)

output$caption2 <- renderUI(HTML("<b> <u> <span style='color:#000000'> Cumulative Probability (Lower Tail): </u> </b>"))
output$probtable2 <- renderTable(probTable("type2"), border=FALSE, striped=FALSE, hover=TRUE, align="c",
                                 sanitize.text.function = function(x) x)

output$caption3 <- renderUI(HTML("<b> <u> <span style='color:#000000'> Cumulative Probability (Upper Tail): </u> </b>"))
output$probtable3 <- renderTable(probTable("type3"), border=FALSE, striped=FALSE, hover=TRUE, align="c",
                                 sanitize.text.function = function(x) x)

output$caption4 <- renderUI(HTML("<b> <u> <span style='color:#000000'> Interval Probability: </u> </b>"))
output$probtable4 <- renderTable(probTable("type4"), border=FALSE, striped=FALSE, hover=TRUE, align="c",
                                 sanitize.text.function = function(x) x)

#################
## Formula Tab ##
#################
output$pdf <- renderUI({
  l <- req(input$lambda3)
  x <- req(input$x3)
  ## Numbers are shown as entered; rounded values (4 significant digits) are marked with an approximately-equal sign.
  rounded <- function(v) v != 0 && abs(signif(v, 4) - v) > 1e-12 * abs(v)
  sci <- function(v) {  # 4 significant digits; powers of 10 for very small or large values
    if (v == 0) return("0")
    e <- floor(log10(abs(v)))
    if (e < -4 || e >= 7) sprintf("%s \\times 10^{%d}", format(signif(v / 10^e, 4), drop0trailing = TRUE), e)
    else format(signif(v, 4), scientific = FALSE, drop0trailing = TRUE, big.mark = ",")
  }
  lx <- l^x
  el <- exp(-l)
  fx <- factorial(x)
  fxtxt <- if (fx < 1e15) format(fx, scientific = FALSE, big.mark = ",") else sci(fx)
  prob <- dpois(x, l)
  rel <- function(r) if (r) "&\\approx" else "&="
  withMathJax(
    h4(HTML("<u> General Formula for Poisson Probability: </u>")),
    h4(sprintf("$$P(X = x) = \\frac{\\lambda^x e^{-\\lambda}}{x!}, x=0,1,2\\ldots$$")),
    br(),
    h4(sprintf("Here, with \\(\\lambda=%s\\) and \\(x=%s\\), we get:", num(l), num(x))),
    h4(paste0("$$\\begin{aligned} P(X = ", num(x), ") ",
      sprintf("&= \\frac{%s^{%s} e^{-%s}}{%s!} \\\\", num(l), num(x), num(l), num(x)),
      sprintf("%s \\frac{%s \\times %s}{%s} \\\\", rel(rounded(lx) || rounded(el) || fx >= 1e15), sci(lx), sci(el), fxtxt),
      sprintf("%s %s", rel(rounded(prob)), sci(prob)),
      " \\end{aligned}$$"))
  )
})

###########################
## Simulate Numbers Tab  ##
###########################

## Results of the simulations since the last Reset (or change of lambda)
sim <- reactiveValues(numbers = NULL, lambda = NULL, runs = NULL)
clearSim <- function() {
  sim$numbers <- NULL; sim$lambda <- NULL; sim$runs <- NULL
}
observeEvent(input$simReset, clearSim())
observeEvent(input$lambda4, clearSim(), ignoreInit = TRUE)

validNsims <- function(m) !is.null(m) && !is.na(m) && m >= 1 && m <= 10000 && m == round(m)
## decimals for mean and standard deviation: those of lambda, plus 2 (as in the mobile app)
simDecimals <- function(lambda) {
  s <- num(lambda); (if (grepl(".", s, fixed = TRUE)) nchar(sub(".*\\.", "", s)) else 0) + 2
}

observeEvent(input$simulate, {
  lambda <- input$lambda4; m <- input$nsims
  if (!validLambda(lambda) || !validNsims(m)) return()  # the graph shows what is wrong
  x <- rpois(m, lambda)
  sim$numbers <- x; sim$lambda <- lambda
  sim$runs <- rbind(sim$runs, data.frame(size = m, mean = mean(x), sd = if (m > 1) sd(x) else NA))
})

## counts for all values in the plotted range (zeros included):
## 0 (or the 0.1st percentile for lambda > 6) to the 99.9th percentile, widened to include all simulated values
simCounts <- reactive({
  lambda <- req(input$lambda4)
  checkLambda(lambda)
  lo <- if (lambda <= 6) 0 else qpois(0.001, lambda)
  hi <- max(qpois(0.999, lambda), if (lambda <= 2) 10 else if (lambda <= 5) 12 else 0)
  x <- sim$numbers
  if (!is.null(x)) { lo <- min(lo, min(x)); hi <- max(hi, max(x)) }
  xs <- lo:hi
  data.frame(xs = xs, count = if (is.null(x)) rep(0, length(xs)) else tabulate(x - lo + 1, nbins = length(xs)))
})

output$simbar <- renderPlotly({
  lambda <- req(input$lambda4)
  checkLambda(lambda)
  validate(need(validNsims(input$nsims), "The number to simulate has to be a whole number between 1 and 10,000."))
  df <- simCounts()
  m <- length(sim$numbers)
  subtitle <- if (m == 0) "Press Simulate to generate random numbers" else
    paste0("Histogram of ", m, " Simulated Number", if (m > 1) "s")
  ## y-axis: whole-number ticks; one tick per count only for small counts
  yax <- list(title = "Frequency", rangemode = 'tozero', tickformat = "d")
  if (max(df$count) <= 10) yax$dtick <- 1
  myhovertext <- c(rbind(paste("<b>", c("Number of Events","Frequency"), ":</b> ", sep=""),
                         lapply(c("xs","count"), sym),
                         c("<br>","<br>")))
  plot_ly(data = df, x = ~factor(xs), y = ~count, type = "bar",
          marker = list(color = mycol[1], line = list(color = '#000000', width = 1)),
          hovertext = ~do.call(paste0, myhovertext), hoverinfo = "text+x") %>%
    layout(xaxis = list(title = "Number of Events", ticks="outside"),
           yaxis = yax,
           showlegend = FALSE,
           hovermode = 'x',
           margin = list(t=65, b=45)
    ) %>%
    add_annotations(text=paste0("The Poisson Distribution with λ = ", lambda), showarrow=FALSE, font=list(size=17), x=0.5, xref='paper', xanchor='top', y=1.18, yref='paper') %>%
    add_annotations(text=paste0("<b> ", subtitle, "</b>"), font=list(size=14, color=mycol[3]), showarrow=FALSE, x=0.5, xref='paper', xanchor='top', y=1.1, yref='paper') %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = list('resetScale2d', 'sendDataToCloud', 'zoom2d', 'zoomIn2d', 'zoomOut2d', 'pan2d', 'select2d', 'lasso2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'hoverClosestGl2d', 'hoverClosestPie', 'toggleHover', 'resetViews', 'toggleSpikelines'))
})

output$simNumbersTitle <- renderUI({
  m <- length(sim$numbers)
  txt <- if (m == 0) "Random Numbers Simulated:" else
    paste0(m, " Random Number", if (m > 1) "s", " Simulated from a Poisson Distribution with λ = ", num(sim$lambda), ":")
  HTML(paste0("<b> <u> <span style='color:#000000'> ", txt, " </span> </u> </b>"))
})

## the simulated numbers in a wide, read-only text box (can be selected and copied)
output$simNumbers <- renderUI({
  x <- req(sim$numbers)
  txt <- paste(x, collapse = " ")
  tags$textarea(txt, readonly = NA, rows = min(4, ceiling(nchar(txt) / 110)), class = "form-control",  # up to 4 lines, then scrolls
                style = "width: 100%; max-width: 100%; resize: vertical; font-family: monospace; background-color: white;")
})

observe(shinyjs::toggleState("simDownload", condition = !is.null(sim$numbers)))
output$simDownload <- downloadHandler(
  filename = function() paste0("poisson_simulation_lambda", num(sim$lambda), ".csv"),
  content = function(file) {
    write.csv(data.frame(number_of_events = req(sim$numbers)), file, row.names = FALSE)
  }
)

output$simFreq <- renderTable({
  df <- simCounts()
  total <- sum(df$count)
  out <- data.frame(x = as.character(df$xs), count = as.character(df$count),
                    pct = if (total > 0) sprintf("%.2f", 100 * df$count / total) else rep("-", nrow(df)))
  out <- rbind(out, data.frame(x = "<b>Total</b>", count = paste0("<b>", total, "</b>"), pct = "<b>100.00</b>"))
  colnames(out) <- c("Number of Events", "Count", "Percent (%)")
  out
}, align = "c", striped = TRUE, hover = TRUE, sanitize.text.function = function(x) x)

output$simStats <- renderTable({
  runs <- req(sim$runs)
  d <- simDecimals(sim$lambda)
  data.frame(Simulation = seq_len(nrow(runs)), Size = runs$size,
             Mean = formatC(runs$mean, format = "f", digits = d),
             `St. Dev.` = ifelse(is.na(runs$sd), "-", formatC(runs$sd, format = "f", digits = d)),
             check.names = FALSE)
}, align = "c", striped = TRUE, hover = TRUE)

output$freqtable3 <- renderRHandsontable({
  l <- req(input$lambda3)
  df <- data.frame(xs = 0:20)
  df %>%
    mutate(ys = formatC(dpois(xs, l), format = "f", digits = 3),
           cumprob = formatC(ppois(xs, l), format = "f", digits = 3)
    ) %>%
    rhandsontable(readOnly = TRUE, height = 180, width=350, index=input$x3,
                  colHeaders = c("x", "P(X = x)", "P(X <= x)"), rowHeaders = FALSE) %>%
    hot_table(stretchH = "all") %>%
    hot_cols(manualColumnResize = TRUE, columnSorting = TRUE, halign = "htCenter", renderer = highlightRenderer)
})

})
