# TRUE: Lumen Learning app. FALSE: Art of Stat app.
# This setting and the Google Analytics tag below (Art of Stat only) are the only
# differences between the two repositories.
# Shiny sources this file before ui.R and server.R.
lumen <- FALSE

# Google Analytics 4 (Art of Stat apps only; the Lumen apps' global.R must not have this).
# ui.R adds ga_tag to the page header when it exists. An empty ga_id means no tracking.
ga_id <- "G-51J33WCCKE"
ga_tag <- if (nzchar(ga_id)) {
  shiny::tagList(
    shiny::tags$script(async = NA, src = paste0("https://www.googletagmanager.com/gtag/js?id=", ga_id)),
    shiny::tags$script(shiny::HTML(paste0(
      "window.dataLayer = window.dataLayer || [];",
      "function gtag(){dataLayer.push(arguments);}",
      "gtag('js', new Date());",
      "gtag('config', '", ga_id, "');"
    )))
  )
}
