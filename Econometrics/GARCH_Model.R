# Run to clear working directory
rm(list=ls())

# Import necessary libraries
library(psych)
library(quantmod)
library(rugarch)
library(PerformanceAnalytics)
library(ggplot2)

# Importing daily Nasdaq (QQQ) Adjusted Close prices over a 10 year period
nasdaq_prices <- getSymbols.yahoo("QQQ", src = 'google', from = '2011-03-01', to = '2021-03-01', auto.assign = F)[,4]

# Calculating returns series
nasdaq_rets <- na.omit(periodReturn(nasdaq_prices, period = 'daily', type = 'arithmetic'))

# Plotting daily Nasdaq returns from 03/01/2011 to 03/01/2021
chartSeries(nasdaq_rets, theme = 'white.mono')
# Plotting daily Adjusted Close prices from 03/01/2011 to 03/01/2021
barChart(nasdaq_prices, theme = 'white.mono')

# Summary statistics of Adj. Close prices and returns
describe(nasdaq_prices)
describe(nasdaq_rets)

# Plotting the rolling volatility of Nasdaq returns
chart.RollingPerformance(R = nasdaq_rets,
                         main = "Nasdaq QQQ ETF rolling daily volatility",
                         FUN = "sd.annualized")

# Rolling volatility plot with altered scale (for higher resolution plot)
chart.RollingPerformance(R = nasdaq_rets,
                         scale = 200,
                         width = 200,
                         main = "Nasdaq QQQ ETF rolling daily volatility",
                         FUN = "sd.annualized")

# Specifying GARCH model parameters
s1 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,0)),
                mean.model=list(armaOrder=c(1,1)), distribution.model = 'norm')

garch_model <- ugarchfit(spec = s1, data = nasdaq_rets)
garch_model

# Plot model results  
plot(garch_model, which = 'all')

# Forecasting
garch_forecast <- ugarchforecast(garch_model, n.ahead = 42)
garch_forecast

# Plot garch model forecasts for Nasdaq QQQ Etf returns
par(mfrow=c(2,1))
plot(garch_forecast, which = 1)
plot(garch_forecast, which = 3)