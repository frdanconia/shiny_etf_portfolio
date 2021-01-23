# Module UI
  
#' @title   mod_finder_ui and mod_finder_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_finder
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import magrittr
#' @import vroom
#' @import dplyr
#' @import gt
#' @import highcharter
#' @import lubridate
#' @import purrr
#' @import tidyr
#' @import stringr
#' @import quantmod
#' @import timetk

mod_finder_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    col_2(
      h4('Core'),
      selectInput(ns("Asset1"), 
                  label = h6('Equity'), 
                  choices = xetra %>% 
                    dplyr::filter(asset.class == 'Equity', region == 'Global') %>% 
                    dplyr::pull(etf),
                  selected = 'iShares Edge MSCI World Minimum Volatility UCITS ETF EUR Hedged (Acc)'
      ),
      selectInput(ns("Asset2"), 
                  label = h6('Bond'), 
                  choices = xetra %>% 
                    dplyr::filter(asset.class == 'Bond') %>% 
                    dplyr::pull(etf),
                  selected = 'Xtrackers II Global Inflation-Linked Bond UCITS ETF 5C'),
      selectInput(ns("Asset3"), 
                  label = h6('Commodity'), 
                  choices = xetra %>% 
                    dplyr::filter(asset.class == 'Commodity') %>% 
                    dplyr::pull(etf),
                  selected = 'ComStage CBK Commodity ex-Agriculture Monthly EUR Hedged UCITS ETF'),
      h4('Satelite'),
      selectInput(ns("Asset4"), 
                  label = h6('Corporate'), 
                  choices = xetra %>% 
                    dplyr::filter(asset.class == 'Corporate') %>% 
                    dplyr::pull(etf),
                  selected = 'Xtrackers II EUR High Yield Corporate Bond UCITS ETF 1C'),
      selectInput(ns("Asset5"), 
                  label = h6('Equity'), 
                  choices = xetra %>% 
                    dplyr::filter(asset.class == 'Equity', region != 'Global') %>% 
                    dplyr::pull(etf),
                  selected = 'Deka MDAX UCITS ETF'),
      actionButton(ns("go"), 
                   label = "Get Price Data", 
                   icon = icon("arrow-down")) %>% 
        tags$div(align = "left", style = "padding-left:.05em"),
      HTML("&nbsp;")
    ),
    col_10(
      mainPanel(width = 12,
                tabsetPanel(
                  tabPanel(title = h5('ETF Returns'), highcharter::highchartOutput(ns('etf_daily_returns'))),
                  tabPanel(title = h5('ETF Growth'), highcharter::highchartOutput(ns('etf_growth')))
                ),
                tabsetPanel(
                  tabPanel(title = h5('Meta'), gt::gt_output(ns('etf_info'))),
                  tabPanel(title = h5('Performance'), gt::gt_output(ns('etf_performance')))
                )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_finder
#' @export
#' @keywords internal
    
mod_finder_server <- function(input, output, session){
  
  ns <- session$ns
  
  ns <- session$ns
  
  data <- eventReactive(input$go, {
    
    etfs <- c(input$Asset1, input$Asset2, input$Asset3, input$Asset4, input$Asset5)
    
    data <- xetra %>% 
      dplyr::filter(etf %in% etfs) %>% 
      import.etf.data() %>% 
      dplyr::mutate(asset = case_when(asset.class == 'Bond' ~ 'Core_Bonds',
                                      asset.class == 'Commodity' ~ 'Core_Commodity',
                                      asset.class == 'Corporate' ~ 'Satelite_Corporate',
                                      asset.class == 'Equity' & region == 'Global' ~ 'Core_Equity',
                                      asset.class == 'Equity' & region != 'Global' ~ 'Satelite_Equity')) %>% 
      dplyr::select(asset, everything())
    
  })
  
  output$etf_info <- gt::render_gt({
    
    data() %>% 
      dplyr::select(asset, asset.class, isin, 
                    region, smartBeta, currencyHeadge, marketCap, 
                    listing_date, recent.trading.date,
                    n.trade.days) %>%
      dplyr::rename('Asset'    = asset,
                    'Asset Class'     = asset.class,
                    'ISIN'            = isin,
                    'Region'          = region,
                    'Smart Beta'      = smartBeta,
                    'Currency Headge' = currencyHeadge,
                    'Market Cap'      = marketCap,
                    'Listing Date'    = listing_date,
                    'Last Date'       = recent.trading.date,
                    'Trading Days'    = n.trade.days) %>% 
      gt::gt() %>% 
      gt::tab_header(title = gt::md('**Metadata**')) %>% 
      gt::tab_options(table.font.size = 10,
                      table.width = gt::pct(100),
                      column_labels.font.size = 12,
                      column_labels.font.weight = 'bold',
                      heading.title.font.size = 16) %>% 
      gt::cols_align(align = "left")
    
  })
  
  output$etf_performance <- gt::render_gt({
    
    data() %>% 
      dplyr::select(asset, asset.class, isin, table.AnnualizedReturns) %>% 
      tidyr::unnest(cols = table.AnnualizedReturns) %>% 
      gt::gt() %>% 
      gt::tab_header(title = gt::md('**Performance**')) %>% 
      gt::tab_footnote(footnote = 'Risk-free rate is 0%',
                       locations = gt::cells_column_labels(columns = vars(`Shape-Ratio p.a.`))) %>% 
      gt::fmt_percent(columns = vars('Return p.a.', 'StdDev p.a'),
                      decimals = 2,
                      drop_trailing_zeros = FALSE) %>% 
      gt::fmt_number(columns = 'Shape-Ratio p.a.', 
                     decimals = 4,
                     drop_trailing_zeros = FALSE) %>% 
      gt::tab_options(table.font.size = 10,
                      table.width = gt::pct(100),
                      column_labels.font.size = 12,
                      column_labels.font.weight = 'bold',
                      heading.title.font.size = 16) %>% 
      gt::cols_align(align = "left")
    
  })
  
  output$etf_daily_returns <- highcharter::renderHighchart({
    
    data() %>% 
      plot.daily.return()
    
  })
  
  output$etf_growth <- highcharter::renderHighchart({
    
    data() %>% 
      plot.growth()
    
  })
}
    
## To be copied in the UI
# mod_finder_ui("finder_ui_1")
    
## To be copied in the server
# callModule(mod_finder_server, "finder_ui_1")
 
