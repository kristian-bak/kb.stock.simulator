#' Add hodler row to data event table
#' @param data stock data from load_data
#' @param data_event data with buy and sell events
#' @export
add_hodler <- function(data, data_event) {

  data_event <- data_event %>%
    dplyr::mutate(event = dplyr::case_when(buy_day == 1 ~ "buy",
                                           sell_day == 1 ~ "sell",
                                           TRUE ~ "no event"))

  data_last_event <- data_event %>%
    dplyr::slice(dplyr::n())

  event <- data_last_event %>%
    dplyr::select(event) %>%
    dplyr::pull()

  if (event %in% c("sell", "no event")) {
    return(data_event)
  } else if (event == "buy") {
    data_hodler <- data %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::mutate(buy_day = 0,
                    sell_day = 0,
                    event = "hodl")
  }

  out <- dplyr::bind_rows(data_event, data_hodler)

  return(out)

}
