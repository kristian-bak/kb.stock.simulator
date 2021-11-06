#' Get strategy
#' @param data stock data
#' @param strategy string specifying indicator
#' @param ... additional parameters passed to strategy definition using `buy_value` and `sell_value`
#' @export
#'
get_strategy <- function(data, strategy, ...) {

  if ("SMA" %in% strategy) {
    data <- get_strategy_sma(data = data, ...)
  }

  if ("RSI" %in% strategy) {
    data <- get_strategy_rsi(data = data, ...)
  }

  if ("BB" %in% strategy) {
    data <- get_strategy_bb(data = data, ...)
  }

  return(data)

}

#' Get strategy SMA
#' @param data stock data
#' @param eps_buy numeric value specifying how much larger SMA should be than close stock price
#' before event is triggered. `eps` = 1 corresponds to buy condition of SMA > Close
#' @details `eps` = 1.02 means SMA should be 2 % larger than the closing price before
#' the strategy indicates a buy. Similarly `eps` = 1.02 means SMA should be 2 % smaller
#' than the closing price before selling. Larger values of `eps` leads to fewer buy/sell
#' signals. If inputs are selected such that no buy signals is obtained, an error
#' meesage is returned.
#' @param eps_sell numeric valuing specifying when to sell in percentage deviation
#' @export
#'
get_strategy_sma <- function(data, eps_buy = 1, eps_sell = 1) {

  data %>%
    dplyr::mutate(SMA14 = TTR::SMA(Close, n = 14),
                  minimum_body = pmin(Open, Close),
                  buy_sma = dplyr::if_else(minimum_body * eps_buy > SMA14, 1, 0),
                  sell_sma = dplyr::if_else(High * eps_sell < SMA14, 1, 0))

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

#' Get strategy Bollinger bands
#' @param data stock data
#' @export
#'
get_strategy_bb <- function(data) {

  bb <- TTR::BBands(HLC = data %>%
                      dplyr::select(High, Low, Close))

  data %>%
    dplyr::mutate(lower_bb = bb[, "dn"],
                  upper_bb = bb[, "up"],
                  buy_bb = dplyr::if_else(Close > upper_bb, 1, 0),
                  sell_bb = dplyr::if_else(Close < lower_bb, 1, 0))

}
