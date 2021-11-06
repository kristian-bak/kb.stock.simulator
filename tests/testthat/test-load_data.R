test_that("Load data works", {

  data <- load_data(ticker = "NOVO-B.CO", from = "2021-01-01")

  cols <- c("Date", "Open", "High", "Low", "Close", "Adjusted", "Change", "Volume")

  expect_identical(names(data), cols)

  price <- data %>%
    dplyr::filter(Date == "2021-01-05") %>%
    dplyr::pull(Close) %>%
    round(2)

  expect_equal(price, 423.95)

})
