#' Plotting the trades
#' @param obj object of class stock which is obtained from `get_performance`
#' @param add_indicator logical specifying whether or not the indicator should be plotted as well.
#' Default is TRUE.
#' @param ... additional parameters allowing to change legend on plot
#' @export

plot.stock <- function(obj, add_indicator = TRUE, ...) {

  get_date <- function(data, n) {
    data %>%
      dplyr::select(Date) %>%
      dplyr::slice(n) %>%
      dplyr::pull()
  }

  data <- obj$data
  data_event <- obj$data_event

  str_text1 <- paste0("My return: ", obj$my_yield$Total_yield, " %")
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

  str_indicator <- get_indicator(data = data)
  str_candlestick <- get_candlestick_legend(...)

  plotly::plot_ly(data = data, x = ~Date, type = "candlestick",
                  open = ~Open, close = ~Close, low = ~Low, high = ~High, name = str_candlestick) %>%
    get_plot_info(event = "buy", event_days = buy_days, event_values = buy_values) %>%
    get_plot_info(event = "sell", event_days = sell_days, event_values = sell_values) %>%
    plotly::add_lines(data = data, x = ~Date, y = ~get(str_indicator), name = str_indicator, inherit = FALSE) %>%
    plotly::layout(xaxis = list(rangeslider = list(visible = FALSE)),
                   yaxis = list(title = "Stock price"),
                   annotations = a)

}

#' Plot prediction
#' @param obj model object
#' @param data data.frame used to plot and calculate predictions based on
#' @return list with plot and performance stats
#' @export
plot_prediction <- function(obj, data) {

  plot_data <- data %>%
    dplyr::mutate(p = predict(obj, newdata = ., type = "response"),
                  abs_error = abs(p - Close),
                  rel_error = 100 * (abs(p - Close) / Close))

  p <- plotly::plot_ly(
    data = plot_data,
    x = ~Date,
    y = ~Close,
    type = "scatter",
    mode = "lines",
    name = "Stock") %>%
    plotly::add_trace(y = ~p, name = "Prediction")

  num_cor <- cor(plot_data$Close, plot_data$p, use = "na")

  adj_r_squared <- summary(obj)$adj.r.squared

  df_stats <-  plot_data %>%
    dplyr::summarise(MAE = mean(abs_error, na.rm = TRUE),
                     MARE = mean(rel_error, na.rm = TRUE))

  df_stats <- cbind(df_stats,
                    dplyr::tibble(cor = num_cor,
                                  adj_r_squared = adj_r_squared))

  out <- list("plot" = p,
              "stats" = df_stats)

  return(out)

}
