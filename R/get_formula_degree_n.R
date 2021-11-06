#' Get formula degree n
#' @param y string specifying response variabel
#' @param x string specifying predictor x
#' @param n degree of polynomium
#' @return formula expression of the form y ~ x + x^2 + x^3 + ... + x^n
get_formula_degree_n <- function(y, x, n) {

  terms <- paste0("I(", x, "^", 2:n, ")")
  terms <- paste0(terms, collapse = " + ")

  form <- paste(y, "~", x, "+", terms)

  form <- as.formula(form)

  return(form)

}
