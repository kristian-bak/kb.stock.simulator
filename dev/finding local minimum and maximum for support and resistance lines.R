ticker <- "NOVO-B.CO"
data <- load_data(ticker = ticker, from = "2019-01-01")

data <- data %>%
  dplyr::mutate(SMA14 = TTR::SMA(Close, n = 14),
                time = 1:dplyr::n())

m1 <- lm(SMA14 ~ time, data = data)
m2 <- lm(SMA14 ~ time + I(time^2), data = data)
m3 <- lm(SMA14 ~ time + I(time^2) + I(time^3), data = data)
m4 <- lm(SMA14 ~ time + I(time^2) + I(time^3) + I(time^4), data = data)
m5 <- lm(Close ~ time + I(time^2) + I(time^3) + I(time^4) + I(time^5), data = data)
m6 <- lm(SMA14 ~ splines::ns(time, df = 1), data = data)
m7 <- lm(SMA14 ~ splines::ns(time, df = 3), data = data)
m8 <- lm(SMA14 ~ splines::ns(time, df = 6), data = data)
m9 <- lm(Close ~ splines::ns(time, df = 100), data = data)
m10 <- lm(SMA14 ~ splines::ns(time, df = 100), data = data)
m11 <- lm(SMA14 ~ poly(time, degree = 2), data = data)
m12 <- lm(formula = get_formula_degree_n(y = "Close", x = "time", n = 100),
          data = data)


summary(m9)

summary(m2)
summary(m12)
identical(m2, m11)

data <- data %>%
  dplyr::mutate(p1 = predict(m1, newdata = data),
                p2 = predict(m2, newdata = data),
                p3 = predict(m3, newdata = data),
                p4 = predict(m4, newdata = data),
                p5 = predict(m5, newdata = data),
                p6 = predict(m6, newdata = data),
                p7 = predict(m7, newdata = data),
                p8 = predict(m8, newdata = data),
                p9 = predict(m9, newdata = data),
                p10 = predict(m10, newdata = data),
                p12 = predict(m12, newdata = data))

plotly::plot_ly(data = data, x = ~Date, type = "candlestick",
                open = ~Open, close = ~Close, low = ~Low, high = ~High, name = "Stock") %>%
  plotly::layout(xaxis = list(rangeslider = list(visible = FALSE))) %>%
  plotly::add_lines(x = ~Date, y = ~SMA14, name = "SMA14", inherit = FALSE) %>%
  plotly::add_lines(x = ~Date, y = ~p5, name = "poly 5", inherit = FALSE) %>%
  plotly::add_lines(x = ~Date, y = ~p9, name = "Spline df 100", inherit = FALSE) %>%
  plotly::add_lines(x = ~Date, y = ~p12, name = "poly 100", inherit = FALSE)


f <- function(x) (x^2 + x)


get_beta <- function(model) {
  beta <- model %>%
    coef() %>%
    as.numeric()
}

beta <- get_beta(model = model)

poly5 <- function(x, beta) {
  beta[1] + beta[2] * I(x) + beta[3] * I(x^2) + beta[4] * I(x^3) +
    beta[5] * (x^4) +
    beta[6] * (x^5)
}



derivative <- function(f) {
  g <- function(x) {}
  body(g) <- D(body(f), 'x')
  return(g)
}

body(poly5)

poly5(x, beta = beta)

polynom::as.function.polynomial(beta)

derivative(f = f(x, beta = beta))

p <- polynom::as.polynomial(beta)
p
class(p)
p
polynom::poly.from.values(p)

p(1)

derivative(as.function(p))





uniroot(f = fd, lower = -100, upper = 100)
