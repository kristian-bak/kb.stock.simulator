#' Get performance
#' @param data stock data
#' @param strategy string specifying indicator(s) to get performance based on.
#' @param ... additional parameters passed to strategy definition using `buy_value` and `sell_value`
#' @export
#'
get_performance <- function(data, strategy = "SMA", ...) {

  data <- get_strategy(data = data, strategy = strategy, ...)

  data_event <- get_events(data = data, strategy = strategy)

  yield <- get_yield(data = data, data_event = data_event, digits = 3)

  out <- c(list("data" = data,
                "data_event" = data_event),
           yield)

  class(out) <- "stock"

  return(out)

}
