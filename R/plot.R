#' Plotting the trades
#' @param obj object of class stock which is obtained from `get_performance`
#' @export

plot.stock <- function(obj) {

  get_date <- function(data, n) {
    data %>%
      dplyr::select(Date) %>%
      dplyr::slice(n) %>%
      dplyr::pull()
  }

  data <- obj$data
  data_event <- obj$data_event

  str_text1 <- paste0("My return: ", obj$my_yield, " %")
  str_text2 <- paste0("Hodl return: ", obj$hodl_yield, " %")
  str_text <- paste0(c(str_text1, "<br>", str_text2), collapse = " ")

  start_day <- get_date(data = data, n = 1)
  end_day <- get_date(data = data, n = dplyr::n())
  text_day <- start_day + floor((as.numeric(end_day - start_day)) / 2)
  y_val <- max(c(data$Open, data$High, data$Low, data$Close), na.rm = TRUE)
  a <- list(x = text_day,
            y = y_val,
            text = str_text,
            showarrow = FALSE)

  get_event_info <- function(data_event, var_event, var_info) {

    data_event %>%
      dplyr::filter(get(var_event) == 1) %>%
      dplyr::select(var_info) %>%
      dplyr::pull()

  }

  buy_days <- get_event_info(
    data_event = data_event,
    var_event = "buy_day",
    var_info = "Date"
  )

  buy_values <- get_event_info(
    data_event = data_event,
    var_event = "buy_day",
    var_info = "Close"
  )

  sell_days <- get_event_info(
    data_event = data_event,
    var_event = "sell_day",
    var_info = "Date"
  )

  sell_values <- get_event_info(
    data_event = data_event,
    var_event = "sell_day",
    var_info = "Close"
  )

  plotly::plot_ly(data = data, x = ~Date, type = "candlestick",
                  open = ~Open, close = ~Close, low = ~Low, high = ~High) %>%
    plotly::add_annotations(x = buy_days,
                            y = buy_values,
                            arrowcolor = 'black',
                            xref = "x",
                            yref = "y",
                            axref = "x",
                            ayref = "y",
                            text = "Buy",
                            showarrow = TRUE,
                            ax = buy_days,
                            ay = buy_values * 0.9) %>%
    plotly::add_annotations(x = sell_days,
                            y = sell_values,
                            arrowcolor = 'black',
                            xref = "x",
                            yref = "y",
                            axref = "x",
                            ayref = "y",
                            text = "Sell",
                            showarrow = TRUE,
                            ax = sell_days,
                            ay = sell_values * 0.9) %>%
    plotly::layout(xaxis = list(rangeslider = list(visible = FALSE)),
                   annotations = a)

}
