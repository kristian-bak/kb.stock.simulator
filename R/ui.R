#' Tab 1
#' @export
#'
ui_tab1 <- function() {
  tabItem(tabName = "Stock_simulator",
          h2("Simulator"),
          fluidRow(
            column(2,
              selectInput(
                 inputId = "select_stock",
                 label = "Select stock",
                 choices = c("RIOT", "DANSKE.CO")
                 )
              ),
            column(2,
              dateInput(
                inputId = "date_from",
                label = "From",
                value = "2020-01-01",
                min = "2000-01-01",
                max = Sys.Date()),
            ),
            column(2,
              numericInput(
                inputId = "num_training_days",
                label = "Training days",
                value = 200,
                min = 50,
                max = 1000),
            ),
            column(2,
              numericInput(
                inputId = "num_test_days",
                label = "Test days",
                value = NA,
                min = 0,
                max = 1000)
            ),
            column(2,
                   numericInput(
                     inputId = "num_test_days_remain",
                     label = "Test days remaining",
                     value = NA,
                     min = 0,
                     max = 1000)
            ),
            column(1,
              actionButton(
                inputId = "go_load",
                label = "Load data"),
              style = "margin-top: 24px"
            )
          ),
          br(),
          plotly::plotlyOutput("stock_data"),
          plotly::plotlyOutput("MACD"),
          plotly::plotlyOutput("RSI"),
          fluidRow(
            column(1,
                   actionButton(
                     inputId = "go_next",
                     label = "Next day"),
                   style = "margin-top: 24px"),
            column(1,
                   actionButton(
                     inputId = "go_buy",
                     label = "Buy"),
                   style = "margin-top: 24px"),
            column(1,
                   actionButton(
                     inputId = "go_sell",
                     label = "Sell"),
                   style = "margin-top: 24px"),
            column(1,
                   actionButton(
                     inputId = "go_save",
                     label = "Save"),
                   style = "margin-top: 24px"),
            column(2,
                   actionButton(
                     inputId = "go_clear_everything",
                     label = "Clear everything"),
                   style = "margin-top: 24px"),
            column(2,
                   textInput(
                     inputId = "text_status",
                     label = "Status",
                     value = NA)
                   )
          ),
          dataTableOutput("table_stats"),
          dataTableOutput("table_transaction")
  )
}

ui_sidebar <- function() {
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Stock simulator",
               tabName = "Stock_simulator",
               icon = icon("chart-line")
      )
    )
  )
}
