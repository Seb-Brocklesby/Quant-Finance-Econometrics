GARCH Volatility Modelling
================
Seb Brocklesby
05/03/2021

``` r
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
```

``` r
# Plotting daily Nasdaq returns from 03/01/2011 to 03/01/2021
chartSeries(nasdaq_rets, theme = 'white.mono')
```

![](GARCH_Forecast_Nasdaq_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# Plotting daily Adjusted Close prices from 03/01/2011 to 03/01/2021
barChart(nasdaq_prices, theme = 'white.mono')
```

![](GARCH_Forecast_Nasdaq_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
# Summary statistics of Adj. Close prices and returns
describe(nasdaq_prices)
```

    ##    vars    n   mean sd median trimmed   mad   min    max  range skew kurtosis
    ## X1    1 2516 129.81 64 110.34   121.4 63.93 50.03 336.45 286.42 1.06     0.68
    ##      se
    ## X1 1.28

``` r
describe(nasdaq_rets)
```

    ##    vars    n mean   sd median trimmed  mad   min  max range  skew kurtosis se
    ## X1    1 2516    0 0.01      0       0 0.01 -0.12 0.08   0.2 -0.49     9.06  0

``` r
# Plotting the rolling volatility of Nasdaq returns
chart.RollingPerformance(R = nasdaq_rets,
                         main = "Nasdaq QQQ ETF rolling daily volatility",
                         FUN = "sd.annualized")
```

![](GARCH_Forecast_Nasdaq_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# Rolling volatility plot with altered scale (for higher resolution plot)
chart.RollingPerformance(R = nasdaq_rets,
                         scale = 200,
                         width = 200,
                         main = "Nasdaq QQQ ETF rolling daily volatility",
                         FUN = "sd.annualized")
```

![](GARCH_Forecast_Nasdaq_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
# Specifying GARCH model parameters
s1 <- ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,0)),
                mean.model=list(armaOrder=c(1,1)), distribution.model = 'norm')

garch_model <- ugarchfit(spec = s1, data = nasdaq_rets)
print(garch_model)
```

    ## 
    ## *---------------------------------*
    ## *          GARCH Model Fit        *
    ## *---------------------------------*
    ## 
    ## Conditional Variance Dynamics    
    ## -----------------------------------
    ## GARCH Model  : sGARCH(1,0)
    ## Mean Model   : ARFIMA(1,0,1)
    ## Distribution : norm 
    ## 
    ## Optimal Parameters
    ## ------------------------------------
    ##         Estimate  Std. Error   t value Pr(>|t|)
    ## mu      0.001063    0.000085   12.4663        0
    ## ar1     0.910769    0.006998  130.1452        0
    ## ma1    -0.963650    0.001478 -652.1811        0
    ## omega   0.000090    0.000004   23.6002        0
    ## alpha1  0.452320    0.047757    9.4712        0
    ## 
    ## Robust Standard Errors:
    ##         Estimate  Std. Error    t value Pr(>|t|)
    ## mu      0.001063    0.000118     8.9906 0.000000
    ## ar1     0.910769    0.019707    46.2145 0.000000
    ## ma1    -0.963650    0.000593 -1624.3245 0.000000
    ## omega   0.000090    0.000009     9.7923 0.000000
    ## alpha1  0.452320    0.119567     3.7830 0.000155
    ## 
    ## LogLikelihood : 7694.841 
    ## 
    ## Information Criteria
    ## ------------------------------------
    ##                     
    ## Akaike       -6.1128
    ## Bayes        -6.1012
    ## Shibata      -6.1128
    ## Hannan-Quinn -6.1085
    ## 
    ## Weighted Ljung-Box Test on Standardized Residuals
    ## ------------------------------------
    ##                         statistic   p-value
    ## Lag[1]                      1.496 2.213e-01
    ## Lag[2*(p+q)+(p+q)-1][5]     6.905 1.630e-06
    ## Lag[4*(p+q)+(p+q)-1][9]    13.069 3.566e-04
    ## d.o.f=2
    ## H0 : No serial correlation
    ## 
    ## Weighted Ljung-Box Test on Standardized Squared Residuals
    ## ------------------------------------
    ##                         statistic  p-value
    ## Lag[1]                      7.776 0.005295
    ## Lag[2*(p+q)+(p+q)-1][2]    80.902 0.000000
    ## Lag[4*(p+q)+(p+q)-1][5]   164.684 0.000000
    ## d.o.f=1
    ## 
    ## Weighted ARCH LM Tests
    ## ------------------------------------
    ##             Statistic Shape Scale P-Value
    ## ARCH Lag[2]     146.0 0.500 2.000       0
    ## ARCH Lag[4]     188.9 1.397 1.611       0
    ## ARCH Lag[6]     229.1 2.222 1.500       0
    ## 
    ## Nyblom stability test
    ## ------------------------------------
    ## Joint Statistic:  4.1882
    ## Individual Statistics:             
    ## mu     0.7299
    ## ar1    0.1187
    ## ma1    0.1366
    ## omega  2.4238
    ## alpha1 1.4857
    ## 
    ## Asymptotic Critical Values (10% 5% 1%)
    ## Joint Statistic:          1.28 1.47 1.88
    ## Individual Statistic:     0.35 0.47 0.75
    ## 
    ## Sign Bias Test
    ## ------------------------------------
    ##                     t-value      prob sig
    ## Sign Bias           3.25530 0.0011478 ***
    ## Negative Sign Bias  0.85563 0.3922868    
    ## Positive Sign Bias  0.07542 0.9398862    
    ## Joint Effect       16.45035 0.0009167 ***
    ## 
    ## 
    ## Adjusted Pearson Goodness-of-Fit Test:
    ## ------------------------------------
    ##   group statistic p-value(g-1)
    ## 1    20     222.7    1.007e-36
    ## 2    30     240.7    3.265e-35
    ## 3    40     262.5    6.385e-35
    ## 4    50     273.7    5.509e-33
    ## 
    ## 
    ## Elapsed time : 1.442003

``` r
# Plot model results  
par(mfrow=c(2,2))
plot(garch_model, which = 1)
plot(garch_model, which = 2)
```

    ## 
    ## please wait...calculating quantiles...

``` r
plot(garch_model, which = 3)
plot(garch_model, which = 4)
```

![](GARCH_Forecast_Nasdaq_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
plot(garch_model, which = 5)
plot(garch_model, which = 6)
plot(garch_model, which = 7)
plot(garch_model, which = 8)
```

![](GARCH_Forecast_Nasdaq_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
plot(garch_model, which = 9)
plot(garch_model, which = 10)
plot(garch_model, which = 11)
plot(garch_model, which = 12)
```

![](GARCH_Forecast_Nasdaq_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
# Forecasting
garch_forecast <- ugarchforecast(garch_model, n.ahead = 42)
print(garch_forecast)
```

    ## 
    ## *------------------------------------*
    ## *       GARCH Model Forecast         *
    ## *------------------------------------*
    ## Model: sGARCH
    ## Horizon: 42
    ## Roll Steps: 0
    ## Out of Sample: 0
    ## 
    ## 0-roll forecast [T0=2021-02-26]:
    ##        Series    Sigma
    ## T+1  0.003084 0.009507
    ## T+2  0.002903 0.011442
    ## T+3  0.002739 0.012217
    ## T+4  0.002590 0.012552
    ## T+5  0.002453 0.012701
    ## T+6  0.002329 0.012768
    ## T+7  0.002216 0.012798
    ## T+8  0.002114 0.012811
    ## T+9  0.002020 0.012817
    ## T+10 0.001935 0.012820
    ## T+11 0.001857 0.012821
    ## T+12 0.001786 0.012822
    ## T+13 0.001722 0.012822
    ## T+14 0.001663 0.012822
    ## T+15 0.001609 0.012822
    ## T+16 0.001561 0.012822
    ## T+17 0.001516 0.012822
    ## T+18 0.001476 0.012822
    ## T+19 0.001439 0.012822
    ## T+20 0.001406 0.012822
    ## T+21 0.001375 0.012822
    ## T+22 0.001347 0.012822
    ## T+23 0.001322 0.012822
    ## T+24 0.001299 0.012822
    ## T+25 0.001278 0.012822
    ## T+26 0.001259 0.012822
    ## T+27 0.001241 0.012822
    ## T+28 0.001225 0.012822
    ## T+29 0.001211 0.012822
    ## T+30 0.001198 0.012822
    ## T+31 0.001186 0.012822
    ## T+32 0.001175 0.012822
    ## T+33 0.001165 0.012822
    ## T+34 0.001156 0.012822
    ## T+35 0.001148 0.012822
    ## T+36 0.001140 0.012822
    ## T+37 0.001133 0.012822
    ## T+38 0.001127 0.012822
    ## T+39 0.001121 0.012822
    ## T+40 0.001116 0.012822
    ## T+41 0.001112 0.012822
    ## T+42 0.001107 0.012822

``` r
# Plot garch model forecasts for Nasdaq QQQ Etf returns
par(mfrow=c(2,1))
plot(garch_forecast, which = 1)
plot(garch_forecast, which = 3)
```

![](GARCH_Forecast_Nasdaq_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
