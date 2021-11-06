#' Get percent change
#' @param x numeric vector
#' @param from_start logical indicating if percent change should be calculated since start or day-by-day. Default is FALSE
#' @param digits number of digits using in rounding
#' @return numeric vector with percent change
get_percent_change <- function(x, from_start = FALSE, digits = 5) {
  x_lag <- c(NA, x)
  x <- c(x, NA)
  if (from_start) {
    y <- round((x - x[1]) / x[1], digits)
  } else {
    y <- round((x - x_lag) / x, digits)
  }
  y <- 100 * y[-length(y)]
  return(y)
}

#' Loading data from Federal Reserve bank (FRED)
#' @description `load_fred` takes character symbol and loads data from Federal Reserve bank (FRED)
#' @param symbol symbol to load
#' @importFrom quantmod getSymbols
#'
#' @return data.frame with data from FRED
#' @export

load_fred <- function(symbol) {

  df <- quantmod::getSymbols(Symbols = symbol, src = "FRED")
  df <- data.frame(mget(df))
  df$Date <- rownames(df) %>% as.Date()
  df <- dplyr::as_tibble(df)
  df$tmp <- get_percent_change(df[[symbol]])
  variable <- paste0(symbol, "_change")
  df <- df %>%
    dplyr::rename(!!variable := tmp)

  return(df)

}
