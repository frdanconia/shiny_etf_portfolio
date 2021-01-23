generate_10k_growth <- function(symbol, starting_date) {
  data <-
    quantmod::getSymbols(symbol,
                         from = starting_date,
                         src = "yahoo",
                         auto.assign = FALSE)
  
  dividend <-
    quantmod::getDividends(symbol, from = starting_date, src = "yahoo")
  
  if (nrow(dividend) > 0) {
    close <- data[, paste0(symbol, ".", "Close")]
    add_index <- which(index(close) %in% index(dividend))
    cumsum_dividend <- cumsum(dividend)
    
    data_10k <- data.frame()
    
    for (i in 1:length(add_index)) {
      if (i > 1) {
        data_10k <- head(data_10k, -1)
      }
      if (i < length(add_index)) {
        data_10k <-
          rbind(data_10k, close[add_index[i]:add_index[i + 1] - 1] + cumsum_dividend[[i]])
      }
    }
    
    if (nrow(data_10k) < nrow(data)) {
      rest_of_data <-
        close[(nrow(data_10k) + 1):nrow(close), ] + cumsum_dividend[[nrow(cumsum_dividend)]]
      data_10k <- rbind(data_10k, rest_of_data)
    }
    
    
    data_10k <- tibble::rownames_to_column(data_10k, "date")
    data_10k$date <- as.Date(data_10k$date , "%Y-%m-%d")
    
    colnames(data_10k) <- c("date", "price")
  } else {
    data_10k <-
      data.frame(price = data[, paste0(symbol, ".", "Close")])
    
    data_10k <- tibble::rownames_to_column(data_10k, "date")
    data_10k$date <- as.Date(data_10k$date , "%Y-%m-%d")
    colnames(data_10k) <- c("date", "price")
  }
  
  data_10k$price <- scale(data_10k$price)
  data_10k <- xts(data_10k[, -1], order.by = data_10k[, 1])
  
  return(data_10k)
}
