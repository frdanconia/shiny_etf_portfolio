#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  library(tidyquant) # due to issue: https://github.com/business-science/tidyquant/issues/116
  callModule(mod_universe_server, "universe_ui_1")
  callModule(mod_finder_server, "finder_ui_1")
}
