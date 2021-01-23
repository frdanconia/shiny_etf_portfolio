# Module UI
  
#' @title   mod_raw_ui and mod_raw_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_raw
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_raw_ui <- function(id){
  ns <- NS(id)
  tagList(
    includeMarkdown(
      system.file("app/www/home.md", package = "shinyETF")
    )
  )
}
    
# Module Server
    
#' @rdname mod_raw
#' @export
#' @keywords internal
    
mod_raw_server <- function(input, output, session){
  
  ns <- session$ns
}
    
## To be copied in the UI
# mod_raw_ui("raw_ui_1")
    
## To be copied in the server
# callModule(mod_raw_server, "raw_ui_1")
 
