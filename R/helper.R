#' Get event info
#' @param data_event data.frame with events obtained from `get_events`
#' @param var_event string specifying "buy_days" or "sell_days"
#' @param var_info string specifying which variable to select (should be "Date" or "Close")
#' @details This is a helper function for `plot.stock`
get_event_info <- function(data_event, var_event, var_info) {

  data_event %>%
    dplyr::filter(get(var_event) == 1) %>%
    dplyr::select(var_info) %>%
    dplyr::pull()

}

#' Get plot info
#' @param p plotly object
#' @param event string specifiying the event as "buy" or "sell"
#' @param event_days vector with dates for when the event is occurring
#' @param even_values vector with stock closing price at `event_days`
#' @details This is a helper function for `plot.stock`
get_plot_info <- function(p, event, event_days, event_values) {

  if (length(event_days) == 0) {
    return(p)
  }

  p %>%
    plotly::add_annotations(x = event_days,
                            y = event_values,
                            arrowcolor = 'black',
                            xref = "x",
                            yref = "y",
                            axref = "x",
                            ayref = "y",
                            text = event,
                            showarrow = TRUE,
                            ax = event_days,
                            ay = event_values * 0.98)

}

#' Get indicator
#' @param data stock data
#' @return a string specifying the selected indicator
#' @details The function is a helper function for `plot.stock`
get_indicator <- function(data) {

  indicators <- c("BB", "SMA14", "RSI")

  indicators[indicators %in% names(data) %>% which()]
}

#' Get candlestick legend
#' @param ticker string specifying the stock
#' @return a string
#' @details The function is a helper function for `plot.stock`
get_candlestick_legend <- function(ticker = NULL) {

  if (is.null(ticker)) {
    str <- "Stock price"
  } else {
    str <- ticker
  }

  return(str)

}
