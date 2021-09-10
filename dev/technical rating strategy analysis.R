library(TTR)
library(dplyr)

data <- load_data("NOVO-B.CO", from = "2021-01-01")
performance <- get_performance(data = data, strategy = "SMA", buy_value = 60, sell_value = 40)
performance

plot(performance)

