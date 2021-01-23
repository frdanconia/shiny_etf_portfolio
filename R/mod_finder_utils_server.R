# Price data --------------------------------------------------------------

import.onvista.price.data <- function(df, start.date = lubridate::today() - lubridate::years(5)) {
  
  # onvista provides 5 years of time series historie for free
  start.date.formated <- paste(str_sub(start.date, 9L,10L), str_sub(start.date, 6L, 7L), lubridate::year(start.date), sep = '.')
  
  df <- df %>%
    # group_nest(idNotation, .key = 'meta') %>%
    mutate(prices.daily = map2(.x = idNotation, .y= start.date.formated, 
                               .f = ~ vroom(
                                 file = paste0(
                                   'https://www.onvista.de/etf/snapshotHistoryCSV?idNotation=',
                                   .x,
                                   '&datetimeTzStartRange=',
                                   .y,
                                   '&timeSpan=5Y&codeResolution=1D'
                                 ),
                                 delim = ';',
                                 col_types = list(
                                   Datum = vroom::col_character(include_na = TRUE),
                                   Volumen = vroom::col_double(include_na = TRUE)
                                 )
                               )
    )
    ) %>% 
    dplyr::mutate(prices.daily = purrr::map(.x = prices.daily,
                                            .f = ~ dplyr::mutate(., Datum = lubridate::dmy(Datum)) %>%
                                              dplyr::mutate_at(., .vars = c('Eröffnung', 'Hoch', 'Tief', 'Schluss'), ~round(./100, digits = 3)) %>%
                                              dplyr::rename(., date = Datum, open = Eröffnung, high = Hoch, low = Tief, close = Schluss, volume = Volumen)
                                            )
                  )
  
  return(df)
}

import.etf.data <- function(df, all.etfs = FALSE, n.complete.days = 300) {
  # import time series data from onvista finance
  
  # future::plan("multiprocess")
  
  df <- df %>% 
    dplyr::group_nest(asset.class, 
                      idNotation, isin, 
                      etf, benchmark,
                      region, smartBeta, currencyHeadge, marketCap, listing_date,
                      .key = 'meta.data') %>% 
    import.onvista.price.data(start.date = lubridate::today() - lubridate::years(5)) %>% 
    adjust.price.periods(period = 'weeks', name = 'weekly') %>% 
    adjust.price.periods(period = 'months', name = 'monthly') %>%
    adjust.price.periods(period = 'quarters', name = 'quarterly') %>%
    adjust.price.periods(period = 'years', name = 'yearly') %>% 
    calculate.return(period = 'daily') %>% 
    calculate.return(period = 'weekly') %>%
    calculate.return(period = 'monthly') %>%
    calculate.return(period = 'quarterly') %>%
    calculate.return(period = 'yearly') %>% 
    calculate.Annualized.return.stats() %>% 
    dplyr::mutate(n.trade.days = purrr::map_int(prices.daily, nrow),
                  n.complete.days = purrr::map_int(prices.daily, ~na.omit(.) %>% nrow),
                  n.na.days = purrr::map_int(prices.daily, ~filter(., is.na(.$close)) %>% nrow),
                  recent.trading.date = purrr::map(prices.daily, ~max(.$date)),
                  complete.ratio = round(n.complete.days/n.trade.days, digits = 3),
                  missing.ratio = round(n.na.days/n.trade.days, digits = 3)) %>%
    tidyr::unnest(recent.trading.date)
  
  if (all.etfs) {
    
    df <- df %>% 
      dplyr::filter(recent.trading.date > lubridate::today()-5, n.complete.days > {{n.complete.days}}) 
  }
  
  
  return(df)
}

assign.asset.class <- function(df, complete.ratio = 0.0) {
  
  df <- df %>% 
    dplyr::filter(complete.ratio >= {{complete.ratio}}) %>% 
    # asset class classification
    dplyr::mutate(asset.class = dplyr::case_when(stringr::str_detect(benchmark, 'Corporate|Fallen Angel Bond|High Yield') ~'Corporate',
                                                 stringr::str_detect(benchmark, 'EONIA|FED') ~ 'Money Market',
                                                 stringr::str_detect(benchmark, 'iBoxx|Government|Treasury|Sovereign|Inflation|Bund|Morgan GBI EMU Index|MTS|Bond|iTraxx') ~ 'Bond',
                                                 stringr::str_detect(benchmark, 'STOXX|MSCI|S&P|Dow Jones|FTSE|Nikkei|Nasdaq|NASDAQ|DAX|Equity|Russell|Quality|Sustainability|Solactive|ATX|SmallCap|NYSE Arca Gold Bugs Index|OMX Stockholm|Low Carbon 100 Europe|TA-25|CSI 300|NMX 30|Gold Miners|Wide Moat|CAC|TOPIX|Nifty|Hang Seng|Minimum Variance|Scientific Beta|EMQQ') ~ 'Equity',
                                                 stringr::str_detect(benchmark, 'Commodity|Agriculture|Enhanced Roll|RICI|Livestock') ~ 'Commodity',
                                                 TRUE ~ 'Other')) %>% 
    # regional classification
    dplyr::mutate(region = dplyr::case_when(asset.class == 'Commodity' ~ 'Commodity',
                                            stringr::str_detect(benchmark, 'SMIT|Malaysia|Thailand|ex Japan|Asia|Africa|East|CSI|Emerging Markets|Nifty|China|Russia|Mexico|Philippines|Taiwan|Pakistan|India|Brazil|Vietnam|Poland|Turkey|Indonesia|Kuwait|Bangladesh|LatAM') ~ 'Emerging Markets',
                                            stringr::str_detect(benchmark, 'World|WORLD|Global|FactSet') ~ 'Global',
                                            stringr::str_detect(benchmark, 'Japanese|Japan|Nikkei|TOPIX|Singapore|Australian|TA-25|MSCI Pacific|Australia|Canada|Korea')~ 'Other Developed',
                                            stringr::str_detect(benchmark, 'Europe|EURO|Eurozone|EUR|EMU|DAX|Germany|Bund|Italy|FTSE MIB|Spain|ATX|OMX|United Kingdom|FTSE All-Share|FTSE 100|Euro|EONIA|France|STOXX') ~ 'Europe',
                                            stringr::str_detect(benchmark, 'USA|US|North America|S&P|KLD|U.S.|Wide Moat|Russell|NASDAQ|Nasdaq|United States|Dow Jones Industrial|FED') ~ 'US',
                                            TRUE ~ 'Other')) %>% 
    # smart Beta classification
    dplyr::mutate(smartBeta = case_when(stringr::str_detect(benchmark, 'Value') ~ 'Value',
                                        stringr::str_detect(benchmark, 'Quality') ~ 'Quality',
                                        stringr::str_detect(benchmark, 'Volatility|Low Vol|Low Risk|Minimum Variance') ~ 'Volatility',
                                        stringr::str_detect(benchmark, 'Buyback') ~ 'Buyback',
                                        stringr::str_detect(benchmark, 'High Dividend|Income') ~ 'High Dividend',
                                        stringr::str_detect(benchmark, 'Multi-Factor|Multi Factor|Multiple-Factor|Equity Factor|Scientific Beta') ~ 'Multi-Factor',
                                        stringr::str_detect(benchmark, 'Wide Moat') ~ 'Wide Moat', 
                                        stringr::str_detect(benchmark, 'Momentum') ~ 'Momentum',
                                        stringr::str_detect(benchmark, 'Low Beta') ~ 'Low Beta',
                                        stringr::str_detect(benchmark, 'Lev|Leverage') ~ 'Leverage',
                                        stringr::str_detect(benchmark, 'Short') ~ 'Short',
                                        TRUE ~ 'None')) %>% 
    dplyr::mutate(currencyHeadge = case_when(stringr::str_detect(benchmark, 'Hedge|Hedged|hedged') ~ 'Hedged',
                                             TRUE ~ 'None')) %>% 
    dplyr::mutate(marketCap = dplyr::case_when(stringr::str_detect(benchmark, 'small|Small|TecDAX|SDAX') ~ 'SmallCap',
                                               stringr::str_detect(benchmark, 'Mid-Cap|Mid Cap|MDAX') ~ 'MidCap',
                                               stringr::str_detect(benchmark, 'Large|EMQQ') ~ 'LargeCap',
                                               stringr::str_detect(benchmark, 'All-Cap|All Cap') ~ 'AllCap',
                                               asset.class == 'Equity' ~ 'LargeCap',
                                               TRUE ~ 'Not applicable')) %>% 
    dplyr::mutate(asset.class = dplyr::case_when(marketCap != 'Not applicable' ~ 'Equity',
                                                 TRUE ~ asset.class),
                  asset.class = dplyr::case_when(isin %in% c('DE000A14ZT85', 
                                                             'DE000A12GJD2', 
                                                             'LU1287022708',
                                                             'LU1838002480', 
                                                             'LU1079842321',
                                                             'LU1079841273',
                                                             'IE00BDFBTQ78',
                                                             'IE00BDFBTK17',
                                                             'IE00BDFBTR85') ~ 'Equity',
                                                 isin %in% c('IE00BDS67326') ~ 'Bond',
                                                 isin %in% c('LU0292106167') ~ 'Commodity',
                                                 TRUE ~ asset.class),
                  region =      dplyr::case_when(isin %in% c('DE000A14ZT85',
                                                             'IE00BDFBTK17',
                                                             'IE00BDFBTR85',
                                                             'LU0322250712',
                                                             'LU0378437767',
                                                             'DE000A2H9VH7',
                                                             'DE000A2N5XE0', 
                                                             'LU1838002480',
                                                             'DE000A2H9VJ3',
                                                             'DE000A2H9VG9') ~ 'Global',
                                                 isin %in% c('IE00BDS67326', 
                                                             'LU1681043912', 
                                                             'LU1681043086', 
                                                             'LU0292109005', 
                                                             'IE00BFNM3P36',
                                                             'IE00BQXKVQ19') ~ 'Emerging Markets',
                                                 isin %in% c('DE000A2N5XC4') ~ 'Other Developed',
                                                 isin %in% c('LU1681046931',
                                                             'LU1681044647',
                                                             'LU1681044720',
                                                             'LU1287023342',
                                                             'LU1829219556',
                                                             'LU1829219713',
                                                             'LU1829219986',
                                                             'LU0321462870',
                                                             'LU0290359032',
                                                             'LU1686830065') ~ 'Europe',
                                                 asset.class == 'Commodity' ~ 'Commodity',
                                                 TRUE ~ region)
    ) %>% 
    dplyr::select(asset.class, etf, isin, benchmark, region, smartBeta, currencyHeadge, marketCap, everything()) 
  
  return(df)
}

adjust.price.periods <- function(df, period = 'weeks', name = 'weekly') {
  
  df <- df %>% 
    dplyr::mutate(price = purrr::map(prices.daily, ~ tidyquant::tq_transmute(.,
                                                                             select     = close, 
                                                                             mutate_fun = to.period,
                                                                             period     = {{period}},
                                                                             indexAt    = "last")
    )
    ) %>% 
    dplyr::rename_at(.vars = c('price'), ~paste(., name, sep = '.'))
  
  
  return(df)
}

calculate.return <- function(df, period = 'weekly') {
  # compute periodical returns
  
  df <- df %>% 
    dplyr::mutate(return = map(prices.daily, ~ tidyquant::tq_transmute(.,
                                                                       select     = close, 
                                                                       mutate_fun = periodReturn, 
                                                                       period     = {{period}}, 
                                                                       type       = "arithmetic") # arithmetic/log
    )) %>% 
    dplyr::rename_at(.vars = c('return'), ~paste(., period, sep = '.'))
  
  
  return(df)
}

calculate.Annualized.return.stats <- function(df) {
  
  df <- df %>% 
    dplyr::mutate(table.AnnualizedReturns = purrr::map(return.daily, ~ tidyquant::tq_performance(.,
                                                                                                 Ra = daily.returns,
                                                                                                 performance_fun = table.AnnualizedReturns,
                                                                                                 scale = 252,
                                                                                                 geometric = TRUE) %>% 
                                                         dplyr::select(., AnnualizedReturn, AnnualizedStdDev, `AnnualizedSharpe(Rf=0%)`) %>% 
                                                         dplyr::rename(., 
                                                                       'Return p.a.' = AnnualizedReturn,
                                                                       'StdDev p.a'  = AnnualizedStdDev,
                                                                       'Shape-Ratio p.a.' = `AnnualizedSharpe(Rf=0%)`)
    )
    ) 
  
  return(df)
}

# Vizualization -----------------------------------------------------------

plot.daily.return <- function(df) {
  
  min.date <- df  %>% 
    dplyr::select(asset, return.daily) %>% 
    dplyr::mutate(min_date = purrr::map(return.daily, ~summarise(.,min_date = min(date)))) %>% 
    tidyr::unnest(min_date) %>% 
    dplyr::select(-return.daily) %>% 
    tidyr::spread(key = asset, value = min_date) 
  
  df <- df %>% 
    dplyr::select(asset, return.daily) %>% 
    tidyr::unnest(cols = c(return.daily)) %>% 
    tidyr::spread(key = asset, value = daily.returns) %>% 
    replace(., is.na(.), 0) %>% 
    dplyr::mutate(Core_Equity        = ifelse(date < min.date$Core_Equity, NA, Core_Equity),
                  Core_Bonds         = ifelse(date < min.date$Core_Bonds, NA, Core_Bonds),
                  Core_Commodity     = ifelse(date < min.date$Core_Commodity, NA, Core_Commodity),
                  Satelite_Corporate = ifelse(date < min.date$Satelite_Corporate, NA, Satelite_Corporate),
                  Satelite_Equity    = ifelse(date < min.date$Satelite_Equity, NA, Satelite_Equity)) %>%
    timetk::tk_xts(date_var = date, select = -date)
  
  hc <- highcharter::highchart(type = "stock") %>% 
    highcharter::hc_title(text = "Daily Returns") %>%
    highcharter::hc_add_series(df$Core_Equity, name = "Core Equity") %>%
    highcharter::hc_add_series(df$Core_Bonds, name = "Core Bonds") %>%
    highcharter::hc_add_series(df$Core_Commodity, name = "Core Commodity") %>%
    highcharter::hc_add_series(df$Satelite_Corporate, name = "Satelite Corporate") %>%
    highcharter::hc_add_series(df$Satelite_Equity, color = "pink", name = 'Satelite Equity') %>%
    highcharter::hc_add_theme(highcharter::hc_theme_google()) %>%
    highcharter::hc_navigator(enabled = FALSE) %>%
    highcharter::hc_scrollbar(enabled = FALSE) %>%
    highcharter::hc_legend(enabled = TRUE) %>% 
    highcharter::hc_exporting(enabled = TRUE)
  
  return(hc)
}

plot.growth <- function(df) {
  
  start.date <- df %>% 
    dplyr::select(asset, return.daily) %>% 
    dplyr::mutate(start.date = furrr::future_map(return.daily, ~min(.$date, na.rm = TRUE))) %>% 
    tidyr::unnest(start.date) %>% 
    dplyr::summarise(start.date = max(start.date)) %>%
    dplyr::pull()
  
  df <- df %>% 
    dplyr::mutate(growth = purrr::map(return.daily, ~
                                        dplyr::filter(., date >= start.date) %>% 
                                        replace(., is.na(.), 0) %>%
                                        dplyr::transmute(date, growth = accumulate(1 + .$daily.returns, `*`)))) %>% 
    dplyr::select(asset, growth) %>% 
    unnest(cols = c(growth)) %>% 
    tibble::rowid_to_column() %>% 
    tidyr::spread(key = asset, value = growth) %>% 
    dplyr::select(-rowid) %>% 
    tidyr::fill(everything(), .direction = "down") %>%
    timetk::tk_xts(date_var = date, select = -date)
  
  hc <- highcharter::highchart(type = "stock") %>%
    highcharter::hc_title(text = "1 Euro Growth") %>%
    highcharter::hc_add_series(df$Core_Equity, name = "Core Equity") %>%
    highcharter::hc_add_series(df$Core_Bonds, name = "Core Bonds") %>%
    highcharter::hc_add_series(df$Core_Commodity, name = "Core Commodity") %>%
    highcharter::hc_add_series(df$Satelite_Corporate, name = "Satelite Corporate") %>%
    highcharter::hc_add_series(df$Satelite_Equity, color = "pink", name = 'Satelite Equity') %>%
    highcharter::hc_add_theme(highcharter::hc_theme_google()) %>%
    highcharter::hc_navigator(enabled = FALSE) %>%
    highcharter::hc_scrollbar(enabled = FALSE) %>%
    highcharter::hc_legend(enabled = TRUE) %>%
    highcharter::hc_exporting(enabled = TRUE)
  
  return(hc)
}

