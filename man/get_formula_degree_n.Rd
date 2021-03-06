% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_formula_degree_n.R
\name{get_formula_degree_n}
\alias{get_formula_degree_n}
\title{Get formula degree n}
\usage{
get_formula_degree_n(y, x, n)
}
\arguments{
\item{y}{string specifying response variabel}

\item{x}{string specifying predictor x}

\item{n}{degree of polynomium}
}
\value{
formula expression of the form y ~ x + x^2 + x^3 + ... + x^n
}
\description{
Get formula degree n
}
