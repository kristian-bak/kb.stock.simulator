ticker <- "NOVO-B.CO"
data <- load_data(ticker = ticker, from = "2021-01-01")

performance <- get_performance(
  data = data,
  strategy = "SMA",
  eps_buy = 1,
  eps_sell = 1
)

performance

plot(performance, add_indicator = TRUE, ticker = ticker)

performance <- get_performance(
  data = data,
  strategy = "RSI",
  buy_value = 60,
  sell_value = 40
)

performance

plot(performance)

performance <- get_performance(
  data = data,
  strategy = "BB"
)

plot(performance)

