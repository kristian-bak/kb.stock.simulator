
## Document
devtools::document()

## Load
devtools::load_all()

## Test
devtools::test()

## Check
devtools::check()

## Add package
usethis::use_package("dplyr")
usethis::use_package("quantmod")
usethis::use_package("shiny")
usethis::use_package("shinydashboard")
usethis::use_package("plotly")
usethis::use_package("DT")
usethis::use_package("readxl")
usethis::use_package("purrr")
usethis::use_package("TTR")

## Add C++
usethis::use_rcpp()
Rcpp::compileAttributes()

## Status on packages
renv::status()

## Add packages
renv::snapshot()

## Add pipe operator
usethis::use_pipe()

## Add license
usethis::use_mit_license()

## Add test:
usethis::use_test("load_data")

devtools::install()

devtools::build()

## Run app:
run_app()

## R start R session before running test coverage
devtools::test_coverage()
