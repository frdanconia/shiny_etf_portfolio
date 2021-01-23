# Module UI
  
#' @title   mod_universe_ui and mod_universe_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_universe
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import magrittr
#' @import dplyr
#' @import gt
mod_universe_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    col_2(
      selectInput(ns("asset_class_select"), 
                  label = h6('Asset Class'), 
                  choices = unique(xetra$asset.class),
                  multiple = TRUE,
                  selected = c('Bond', 'Equity', 'Commodity', 'Corporate')),
      # tags$div(align = "center", style = "padding-left:2em"),
      selectInput(ns("region_select"), 
                  label = h6('Region'), 
                  choices = unique(xetra$region),
                  multiple = TRUE,
                  selected = c('Global', 'Europe', 'US', 'Emerging Markets', 'Other Developed', 'Commodity')),
      # tags$div(align = "left", style = "padding-left:2em"),
      selectInput(ns("smartBeta_select"), 
                  label = h6('Smart Beta'), 
                  choices = unique(xetra$smartBeta),
                  multiple = TRUE,
                  selected = c('None', 'High Dividend', 'Low Beta',
                               'Momentum', 'Multi-Factor', 'Quality', 
                               'Value', 'Volatility')),
      # tags$div(align = "left", style = "padding-left:2em"),
      selectInput(ns("currencyHeadge_select"), 
                  label = h6('Currency Headge'), 
                  choices = unique(xetra$currencyHeadge),
                  multiple = TRUE,
                  selected = c('None', 'Hedged')),
      # tags$div(align = "left", style = "padding-left:2em"),
      sliderInput(ns("ter_slider"),
                  label = h6("TER"),
                  min = min(xetra$ter, na.rm = T),
                  max = max(xetra$ter, na.rm = T),
                  value = c(min(xetra$ter, na.rm = T), max(xetra$ter, na.rm = T)),
                  step = 0.0001
      ),
      selectInput(ns("replication_method_select"), 
                  label = h6('Replication Method'), 
                  choices = unique(xetra$replication_method),
                  multiple = TRUE,
                  selected = c('Full Replication', 'Optimised', 'Swap-based')),
      shiny::dateRangeInput(ns('listing_date_select'),
                            label = h6('Min Listing Date'),
                            start = min(xetra$listing_date),
                            end = lubridate::today() - lubridate::years(5),
                            min = min(xetra$listing_date),
                            max = lubridate::today()-1),
      actionButton(ns("go"), 
                   label = "Show ETFs", 
                   icon = icon("arrow-down")) %>% 
        tags$div(align = "left", style = "padding-left:.05em"),
      HTML("&nbsp;")
    ),
    col_10(
      mainPanel(width = 12,
                shinydashboard::valueBoxOutput(ns('total'), width = 3), 
                shinydashboard::valueBoxOutput(ns('selected'), width = 3), 
                shinydashboard::valueBoxOutput(ns('avgter'), width = 3)
                ),
      mainPanel(width = 12,
                gt::gt_output(ns('universe'))
                )
      )
    )
}
    
# Module Server
    
#' @rdname mod_universe
#' @export
#' @keywords internal
    
mod_universe_server <- function(input, output, session){
  
  ns <- session$ns
  
  data <- eventReactive( input$go , {
    
    df <- xetra %>% 
      dplyr::filter(asset.class %in% input$asset_class_select) %>% 
      dplyr::filter(region %in% input$region_select) %>% 
      dplyr::filter(smartBeta %in% input$smartBeta_select) %>% 
      dplyr::filter(currencyHeadge %in% input$currencyHeadge_select) %>% 
      dplyr::filter(ter >= input$ter_slider[1] & ter <= input$ter_slider[2]) %>% 
      # dplyr::filter(use_of_profits %in% input$use_of_profits_select) %>%
      dplyr::filter(replication_method %in% input$replication_method_select) %>%
      dplyr::filter(listing_date >= input$listing_date_select[1] & listing_date <= input$listing_date_select[2]) %>%
      dplyr::arrange(asset.class, ter, listing_date)
    
    return(df) 
    
  })
  
  output$total <- shinydashboard::renderValueBox({
    
    shinydashboard::valueBox(
      subtitle = h6('ETFs available'),
      value = xetra %>% NROW()
    )
    
  })
  
  output$selected <- shinydashboard::renderValueBox({
    
    shinydashboard::valueBox(
      subtitle = h6('ETFs seleceted'),
      value = data() %>% NROW()
    )
    
  })
  
  output$avgter <- shinydashboard::renderValueBox({
    
    shinydashboard::valueBox(
      subtitle = h6('average seleceted TER'),
      value = round(mean(data()$ter), digits = 4)
    )
    
  })
  
  output$universe <- gt::render_gt({
    
    dat <- data() 
    
    dat %>% 
      dplyr::select(-idNotation) %>% 
      dplyr::rename('Asset Class' = asset.class,
                    TER = ter, 
                    Name = etf,
                    ISIN = isin,
                    # Symbol = idNotation, 
                    Benchmark = benchmark, 
                    Region = region, 
                    'Smart Beta' = smartBeta,
                    'Currency Headge' = currencyHeadge,
                    'Market Cap' = marketCap, 
                    'Listing Date' = listing_date, 
                    'Use of Profits' = use_of_profits, 
                    'Replication Method' = replication_method) %>% 
      gt::gt() %>% 
      gt::tab_header(title = gt::md('**ETF Overview**')) %>% 
      gt::tab_options(table.font.size = 10,
                      column_labels.font.size = 12,
                      column_labels.font.weight = 'bold',
                      heading.title.font.size = 16) %>% 
      gt::cols_align(align = "left")
    
  })
}
    
## To be copied in the UI
# mod_universe_ui("universe_ui_1")
    
## To be copied in the server
# callModule(mod_universe_server, "universe_ui_1")
 
