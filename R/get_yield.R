#' Get yield
#' @param data stock data
#' @param data_event data with events
#' @export
get_yield <- function(data, data_event, digits = 2) {

  price_buy <- data_event %>%
    dplyr::filter(event == "buy") %>%
    dplyr::select(Close) %>%
    dplyr::pull()

  price_sell <- data_event %>%
    dplyr::filter(event != "buy") %>%
    dplyr::select(Close) %>%
    dplyr::pull()

  yield_vec <- (price_sell - price_buy) / price_buy
  yield <- prod(1 + yield_vec) ## the total yield is a product of all the yields as second volume depends on the first yield
  yield <- 100 * (yield - 1)
  yield <- round(yield, digits)
  yield_vec <- round(100 * yield_vec, digits)

  price_day_one <- data %>%
    dplyr::slice(1) %>%
    dplyr::select(Close) %>%
    dplyr::pull()

  price_day_n <- data %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::select(Close) %>%
    dplyr::pull()

  hodl_yield <- 100 * (price_day_n - price_day_one) /  price_day_one
  hodl_yield <- round(hodl_yield, digits)
  out <- list("my_yield" = list("Total_yield" = yield,
                                "All_yield" = yield_vec),
              "hodl_yield" = hodl_yield)

  return(out)
}
