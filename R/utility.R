#' An extension of tryCatch
#' @param expr expression to evalute
#' @return A list with value, warning and error message
#' @export
#'
catch_error <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value = value, warning = warn, error = err)
}

#' Daily price change in percent
#' @param x stock price
#' @return A numerical vector with prince changes in percent
#' @export
#'
price_change <- function(x, digits = 5) {
  x_lag <- c(NA, x)
  x <- c(x, NA)
  y <- round((x - x_lag) / x, digits)
  y <- 100 * y[-length(y)]
  return(y)
}
