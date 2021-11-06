#' Load data from Yahoo.
#' @param ticker ticker code from Yahoo.
#' @param from load data `from` untill today where the format of `from` is yyyy-mm-dd.
#' @return A data.frame
#' @examples
#' # Load NOVO data since 2021-01-01
#' data <- load_data(ticker = "NOVO-B.CO", from = "2021-01-01")
#' data
#' @export
#' @details Davs
#'
load_data <- function(ticker, from = "2014-01-01") {

  data <- catch_error(
    quantmod::getSymbols(ticker, auto.assign = FALSE, from = from, src = "yahoo")
  )

  if (!is.null(data$error)) {
    stop(paste0("Load data failed for ", ticker))
  }

  data <- data$value %>%
    as.data.frame() %>%
    dplyr::mutate(Date = rownames(.) %>% as.Date()) %>%
    dplyr::as_tibble() %>%
    dplyr::rename_with(.data = ., ~ gsub(paste0(ticker, "."), "", .x)) %>%
    dplyr::mutate(Change = price_change(Close, digits = 4)) %>%
    dplyr::relocate(Date, Open, High, Low, Close, Adjusted, Change, Volume)

  return(data)

}
