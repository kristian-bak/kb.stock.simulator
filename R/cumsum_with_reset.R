#' @title Cumulative sum with reset
#' @param x binary vector
#' @description `cumsum_with_reset` calculates the cumulative sum of x with 0 as the reset value
#' @export
#'
cumsum_with_reset <- function(x) {

  n <- length(x)
  x[is.na(x)] <- 0
  y <- rep(NA, n)
  if (x[1] == 1) {
    y[1] <- 1
  } else {
    y[1] <- 0
  }

  for (i in 2:n) {
    if (x[i] == 1) {
      if (x[i - 1] == 1) {
        y[i] <- y[i - 1] + 1
      } else {
        y[i] <- 1
      }
    } else {
      y[i] <- 0
    }
  }
  return(y)
}
