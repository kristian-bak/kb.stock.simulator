#' Get events
#' @param data stock data
#'
get_events <- function(data, strategy) {

  buy_var <- paste0("buy_", tolower(strategy))
  sell_var <- paste0("sell_", tolower(strategy))

  data <- data %>%
    dplyr::mutate(buy_id = cumsum_with_reset(get(buy_var)),
                  sell_id = cumsum_with_reset(get(sell_var)))

  data_event <- data %>%
    dplyr::filter(buy_id == 1 | sell_id == 1) %>%
    dplyr::mutate(buy_day = cumsum_with_reset(buy_id),
                  sell_day = cumsum_with_reset(sell_id)) %>%
    dplyr::filter(buy_day == 1 | sell_day == 1)

  first_event_is_sell <- data_event %>%
    dplyr::slice(1) %>%
    dplyr::select(sell_day) %>%
    dplyr::pull()

  if (first_event_is_sell) {
    data_event <- data_event %>%
      dplyr::slice(2:dplyr::n())
  }

  data_event <- data_event %>%
    add_hodler(data = data)

  return(data_event)

}
