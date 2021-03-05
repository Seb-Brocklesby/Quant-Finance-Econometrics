# Clearing global environment
rm(list=ls())

# Import libraries
library(psych)
library(tseries)
library(vars)
library(forecast)

# Import Dataset
data=read.csv('FXRatesNZ_2020.csv', header=T)
head(data)
tail(data)

attach(data) # Let's us use the variable names

# Convert the Variables into Time-Series
NZD_USD_TS = ts(nzdperusd, frequency=12, start=c(1973,6), end=c(2019,12)) # Leaving out 2020 data to forecast later
NZD_AUD_TS = ts(nzdperaud, frequency=12, start=c(1973,6), end=c(2019,12)) # Leaving out 2020 data to forecast later
NZD_UKP_TS = ts(nzdperukp, frequency=12, start=c(1973,6), end=c(2019,12)) # Leaving out 2020 data to forecast later
TWI_TS = ts(twi, frequency=12, start=c(1973, 6), end=c(2019,12)) # Leaving out 2020 data to forecast later

# Plotting the data
par(mfrow=c(2,2))
ts.plot(NZD_AUD_TS)
ts.plot(NZD_UKP_TS)
ts.plot(NZD_USD_TS)
ts.plot(TWI_TS)
dev.off()

# Summary statistics
describe(data[-1]) 
summary(data[-1])

# Testing for Normality #
shapiro.test(NZD_AUD_TS)
# Pvalue = 8.238e-08 < 0.05
# Conclusion: NZD/AUD is NOT normally distributed.

shapiro.test(NZD_UKP_TS)
# Pvalue = 1.151e-08 < 0.05
# Conclusion: NZD/UKP is NOT normally distributed.

# Testing for Autocorrelation #
Box.test(NZD_AUD_TS,lag=1,type='Ljung-Box')
# Pvalue = 2.2e-16 < 0.05
# Conclusion: NZD/AUD is autocorrelated

Box.test(NZD_UKP_TS,lag=1,type='Ljung-Box')
# Pvalue = 2.2e-16 < 0.05
# Conclusion: NZD/UKP is autocorrelated

# Testing for Stationarity #
adf.test(NZD_AUD_TS)
# Pvalue = 0.0715 > 0.05
# Conclusion: NZD/AUD is NOT stationary.

adf.test(NZD_UKP_TS)
# Pvalue = 0.4131 > 0.05
# Conclusion: NZD/UKP is NOT stationary.

# ACF and PACF plots #
acf(NZD_AUD_TS,lag.max = 36)
acf(NZD_UKP_TS,lag.max = 36)

pacf(NZD_AUD_TS,lag.max = 36)
pacf(NZD_UKP_TS,lag.max = 36)

### ARIMA Modelling ###
D_NZD_AUD = diff(NZD_AUD_TS) # Taking the first difference of NZD/AUD
adf.test(D_NZD_AUD)
# P-value: 0.01 < 0.05
# Conclusion: First difference of NZD/USD IS stationary

## NZD/USD DIFF ACF and PACF
acf(D_NZD_AUD, lag.max = 36)
pacf(D_NZD_AUD, lag.max = 36)

# Fitting ARIMA Models
auto.arima(D_NZD_AUD, max.p = 12, max.q = 12)

arima1 = arima(D_NZD_AUD, order = c(2, 0, 1))
summary(arima1)
arima2 = arima(D_NZD_AUD, order = c(2, 0, 2))
summary(arima2)
arima3 = arima(D_NZD_AUD, order = c(3, 0, 1))
summary(arima3)
arima4 = arima(D_NZD_AUD, order = c(0, 0, 2))
summary(arima4)

arima5 = arima(NZD_AUD_TS, order = c(2, 1, 1))
summary(arima5)

# Residuals of final ARIMA model
res1=resid(arima1)

# Testing stationarity in residuals
adf.test(res1)
# p-value = 0.01 < 0.05
# Conclusion: residuals of final model are stationary

### Forecasting ###
T_fut_1 = 12 # Number of periods (months) to forecast
NZD_AUD_hat2 = forecast(NZD_AUD_TS, T_fut_1)
summary(NZD_AUD_hat2)

### Plot Results and save to pdf format.
pdf('NZD_AUD_Forecast')
plot(NZD_AUD_hat2)
dev.off()