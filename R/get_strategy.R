#' Get strategy
#' @param data stock data
#' @param strategy string specifying indicator
#' @param ... additional parameters passed to strategy definition using `buy_value` and `sell_value`
#' @export
#'
get_strategy <- function(data, strategy, ...) {

  if ("SMA" %in% strategy) {
    data <- get_strategy_sma(data = data)
  }

  if ("RSI" %in% strategy) {
    data <- get_strategy_rsi(data = data, ...)
  }

  return(data)

}

#' Get strategy SMA
#' @param data stock data
#' @export
#'
get_strategy_sma <- function(data) {

  data %>%
    dplyr::mutate(SMA14 = TTR::SMA(Close, n = 14),
                  SMA14_diff = Close - SMA14,
                  buy_sma = dplyr::if_else(SMA14_diff > 0.5, 1, 0),
                  sell_sma = dplyr::if_else(SMA14_diff < -0.5, 1, 0))

}

#' Get strategy RSI
#' @param data stock data
#' @param buy_value value between 0 and 100 for which the strategy buys the stock. Default is 55
#' @param sell_value value between 0 and 100 for which the strategy sells the stock. Default is 45
#' @export
#'
get_strategy_rsi <- function(data, buy_value = 55, sell_value = 45) {

  data <- data %>%
    dplyr::mutate(RSI = TTR::RSI(Close, n = 14),
                  buy_rsi = dplyr::if_else(RSI > buy_value, 1, 0),
                  sell_rsi = dplyr::if_else(RSI < sell_value, 1, 0))

}
