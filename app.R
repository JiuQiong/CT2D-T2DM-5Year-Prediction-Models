# Cloud entry: frozen five-year models only; no original cohort is read.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
Sys.setenv(TZ = "Asia/Shanghai")
library(shiny)
library(bslib)
library(htmltools)
for (module in c("config/dictionary.R", "R/engine.R", "R/presentation.R", "R/ui.R", "R/server.R")) {
  source(module, local = TRUE, encoding = "UTF-8")
}
bundles <- load_prediction_bundles("models/five_year")
dictionary <- variable_dictionary()
invisible(lapply(bundles, function(b) validate_dictionary(dictionary, b$features)))
css <- paste(readLines("www/styles.css", encoding = "UTF-8"), collapse = "\n")
shiny::shinyApp(ui = build_ui(bundles, dictionary), server = make_server(bundles, dictionary, css))
