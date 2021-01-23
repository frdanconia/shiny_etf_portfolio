#' @import shiny
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    tagList(
      nav_(
        h3("Shiny ETF Portfolio"),
        c(
          "raw_data" = "Info",
          "universe" = "ETF Universe",
          "finder" = "ETF Finder"
          # "about" = "About"
        )
      )
      , 
      tags$div(
        class="container", 
        fluidRow(id = "raw_data",  mod_raw_ui("raw_ui_1")) %>%
          tagAppendAttributes(style = "display:none"),
        fluidRow(
          id = "universe", mod_universe_ui("universe_ui_1")) %>% 
          tagAppendAttributes(style = "display:none"),
        fluidRow(
          id = "finder", mod_finder_ui("finder_ui_1")
        ) %>% tagAppendAttributes(style = "display:none")
        # fluidRow(id = "about", mod_about_ui("about_ui_1")) %>% 
        #   tagAppendAttributes(style = "display:none")
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'shinyETF')
  )
 
  tags$head(
    golem::activate_js(),
    # golem::favicon(),
    tags$title("Shiny ETF"),
    tags$link(
      rel="stylesheet", 
      type="text/css", 
      href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css",
      integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T",
      crossorigin="anonymous"
    ), 
    tags$script(
      src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js",
      integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM",
      crossorigin="anonymous"
    ),
    tags$link(
      rel="stylesheet", 
      type="text/css", 
      href="www/custom.css"
    ), 
    tags$script(src="www/script.js")
  )
}
