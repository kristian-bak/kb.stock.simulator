
## Document
devtools::document()

## Load
devtools::load_all()

## Check
devtools::check()

## Add package
usethis::use_package("dplyr")
usethis::use_package("quantmod")
usethis::use_package("shiny")
usethis::use_package("shinydashboard")
usethis::use_package("plotly")
usethis::use_package("DT")

## Status on packages
renv::status()

## Add packages
renv::snapshot()

## Add pipe operator
usethis::use_pipe()

## Add license
usethis::use_mit_license()

devtools::install()

devtools::build()

## Run app:
run_app()

