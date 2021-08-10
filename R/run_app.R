#' Run app
#' @export
#'
run_app <- function() {

  library(shiny)
  library(shinydashboard)
  library(plotly)
  library(dplyr)
  library(DT)
  library(TTR)

  ui <- dashboardPage(
    dashboardHeader(title = "Stock Simulator"),
    ui_sidebar(),
    dashboardBody(
      tabItems(
        ui_tab1()
      )
    )
  )

  server <- function(input, output) {

    react_var <- reactiveValues(
      all_data = data.frame(),
      plot_data = data.frame(),
      transaction_data = data.frame(),
      table_stats = data.frame(),
      num_test_days = NA,
      num_test_days_remain = NA,
      status = NA
    )

    observeEvent(input$go_load, {

      react_var$all_data <- react_var$plot_data <- load_data(
        ticker = input$select_stock,
        from = input$date_from
      )

      react_var$plot_data <- react_var$plot_data %>%
        slice(1:input$num_training_days)

      react_var$num_test_days <- nrow(react_var$all_data) - input$num_training_days

      updateNumericInput(inputId = "num_test_days", value = react_var$num_test_days)

    })

    observe({

      req(input$go_load)
      react_var$num_test_days_remain <- nrow(react_var$all_data) - nrow(react_var$plot_data)
      updateNumericInput(inputId = "num_test_days_remain", value = react_var$num_test_days_remain)

    })

    output$stock_data <- renderPlotly({

      if (nrow(react_var$plot_data) == 0) {
        return()
      }

      MA <- TTR::SMA(x = react_var$plot_data$Close, n = 20)

      fig <- plotly::plot_ly(data = react_var$plot_data, x = ~Date, type = "candlestick",
                             open = ~Open, close = ~Close, low = ~Low, high = ~High) %>%
        add_lines(x = react_var$plot_data$Date, y = MA, inherit = FALSE,
                  line = list(color = "black"), name = "MA20") %>%
        layout(title = "Candlestick Chart",
               showlegend = FALSE,
               yaxis = list(title = "Stock price"),
               xaxis = list(rangeslider = list(visible = F)))

      if (nrow(react_var$transaction_data) > 0) {

        buy_data <- react_var$transaction_data %>%
          filter(Event == "buy")

        sell_data <- react_var$transaction_data %>%
          filter(Event == "sell")

        if (nrow(buy_data) > 0) {

          fig <- fig %>%
            plotly::add_annotations(x = buy_data$Date,
                                    y = buy_data$Price,
                                    arrowcolor = 'black',
                                    xref = "x",
                                    yref = "y",
                                    axref = "x",
                                    ayref = "y",
                                    text = "Buy",
                                    showarrow = TRUE,
                                    ax = buy_data$Date,
                                    ay = buy_data$Price * 0.9)

        }

        if (nrow(sell_data) > 0) {

          fig <- fig %>%
            plotly::add_annotations(x = sell_data$Date,
                                    y = sell_data$Price,
                                    arrowcolor = 'black',
                                    xref = "x",
                                    yref = "y",
                                    axref = "x",
                                    ayref = "y",
                                    text = "Sell",
                                    showarrow = TRUE,
                                    ax = sell_data$Date,
                                    ay = sell_data$Price * 0.9)
        }

      }

      return(fig)

    })

    output$RSI <- renderPlotly({

      if (nrow(react_var$plot_data) == 0) {
        return()
      }

      RSI <- TTR::RSI(react_var$plot_data$Close)

      line_options <- list(color = 'rgb(22, 96, 167)', dash = 'dot')

      plot_ly(
        data = react_var$plot_data, x = ~Date, y = RSI,
        type = "scatter", mode = "lines", name = "RSI") %>%
        add_lines(y = 30, line = line_options, name = "RSI = 30") %>%
        add_lines(y = 70, line = line_options, name = "RSI = 70") %>%
        layout(showlegend = FALSE, title = "RSI")

    })

    output$MACD <- renderPlotly({

      if (nrow(react_var$plot_data) == 0) {
        return()
      }

      MACD_res <- data.frame(TTR::MACD(react_var$plot_data$Close, Fast = 12, nSlow = 26))
      MACD <- MACD_res$macd
      Signal <- MACD_res$signal

      plot_ly(data = react_var$plot_data, x = ~Date, y = MACD,
              type = "scatter", mode = "lines", name = "MACD") %>%
        add_lines(y = Signal, name = "Signal") %>%
        layout(showlegend = FALSE, title = "MACD")

    })

    observeEvent(input$go_next, {

      data_next_day <- react_var$all_data %>%
        slice(nrow(react_var$plot_data) + 1)

      react_var$plot_data <- dplyr::union(react_var$plot_data,
                                          data_next_day)

    })

    get_date <- reactive({
      react_var$plot_data %>%
        slice(n()) %>%
        select(Date) %>%
        pull()
    })

    observeEvent(input$go_buy, {

      if (react_var$status == "Holding") {
        showNotification(
          ui = "You cannot buy when you already are in a holding position",
          duration = 3,
          type = "error"
        )
        return()
      }

      transaction_data <- react_var$transaction_data
      plot_data <- react_var$plot_data

      today <- get_date()

      price_buy <- plot_data %>%
        slice(n()) %>%
        select(Close) %>%
        pull()

      df <- data.frame(Date = today,
                       Event = "buy",
                       Price = price_buy,
                       Return = NA)

      react_var$transaction_data <- rbind(transaction_data, df)

    })

    observeEvent(input$go_sell, {

      if (react_var$status == "Staying away") {
        showNotification(
          ui = "You cannot sell without holding any stocks",
          duration = 3,
          type = "error"
        )
        return()
      }

      transaction_data <- react_var$transaction_data
      plot_data <- react_var$plot_data

      today <- get_date()

      data_today <- plot_data %>%
        slice(n())

      price_buy <- transaction_data %>%
        filter(Event == "buy") %>%
        select(Price) %>%
        pull()

      price_sell <- data_today %>%
        select(Close) %>%
        pull()

      afkast <- round(100 * ((price_sell - price_buy) / price_buy), 2)

      df <- data.frame(Date = today,
                       Event = "sell",
                       Price = price_sell,
                       Return = afkast)

      react_var$transaction_data <- rbind(transaction_data, df)

    })

    output$table_transaction <- renderDataTable(options = list(dom = 't'), {

      react_var$transaction_data

    })

    observe({

      req(input$go_load)

      if (nrow(react_var$transaction_data) == 0) {
        react_var$status <- "Staying away"
      } else {
        last_event <- react_var$transaction_data %>%
          filter(Date == max(Date)) %>%
          select(Event) %>%
          pull()

        if (last_event == "buy") {
          react_var$status <- "Holding"
        } else if (last_event == "sell") {
          react_var$status <- "Staying away"
        }
      }

      updateTextInput(inputId = "text_status", value = react_var$status)

    })

    output$table_stats <- renderDataTable(options = list(dom = 't'), {

      if (nrow(react_var$all_data) == 0) {
        return()
      }

      data_start_day <- react_var$all_data %>%
        slice(input$num_training_days)

      data_today <- react_var$all_data %>%
        filter(Date == get_date())

      hodl_return <- 100 * ((data_today$Close - data_start_day$Close) / data_start_day$Close)
      hodl_return <- paste0(round(hodl_return, 2), " %")

      if (nrow(react_var$transaction_data) < 1) {
        table_stats <- data.frame(Hodl_return = hodl_return,
                                  My_return = NA,
                                  Accuracy = NA)

        react_var$table_stats <- table_stats

        return(react_var$table_stats)
      }

      transaction_data <- react_var$transaction_data

      ## If current return is unrealized, it will be max date and have Return = NA
      current_return <- transaction_data %>%
        filter(Date == max(Date) & is.na(Return)) %>%
        mutate(Return = 100 * ((data_today$Close - Price) / Price) %>% round(3)) %>%
        select(Return) %>%
        pull()

      ## My return calculates the return among realized trades.
      my_return <- transaction_data %>%
        filter(!is.na(Return)) %>%
        select(Return) %>%
        pull()

      ## My return combines realized and unrealized trades.
      my_return <- c(my_return, current_return)

      my_avg_return <- my_return %>% mean() %>% round(2)
      my_avg_return <- paste0(my_avg_return, " %")

      my_acc <- round(100 * (sum(my_return > 0) / length(my_return)), 2)
      my_acc <- paste0(my_acc, " %")

      table_stats <- data.frame(Hodl_return = hodl_return,
                                My_return = my_avg_return,
                                Accuracy = my_acc)

      react_var$table_stats <- table_stats

      return(react_var$table_stats)

    })

    observeEvent(input$go_clear_everything, {

      react_var$transaction_data <- data.frame()
      react_var$table_stats <- data.frame()

      react_var$plot_data <- react_var$plot_data %>%
        slice(1:input$num_training_days)

    })

    observeEvent(input$go_save, {

      out <- reactiveValuesToList(react_var)
      out$all_data <- NULL
      out$plot_data <- NULL
      out$today <- get_date()

      str_date <- gsub("-|:| ", "_", Sys.time())
      file_name <- paste0("dev/appdata/", input$select_stock, "_", str_date, ".RDS")
      saveRDS(object = out, file = file_name)

    })

  }

  shinyApp(ui, server)

}
