library(readxl)
library(TSA)
library(tseries)
library(quantmod)


#nvidia = read_excel("C:/Users/hemal/OneDrive - Oklahoma A and M System/3 Fall 2023 Courseworks/Time series analysis/Homework 6/NVDA.xlsx")
#nvidia.ts = ts(nvidia$Close)

symbol <- "AAPL"
getSymbols(symbol, src = "yahoo")

closing_prices <- Cl(AAPL)
head(closing_prices)

plot(closing_prices)

aapl_ret = diff(log(closing_prices))
aapl_ret <- na.omit(aapl_ret)
plot(aapl_ret)
abline(h=0)

adf.test(aapl_ret) # testing for stationarity


#####################
acf((aapl_ret$AAPL.Close))
pacf((aapl_ret$AAPL.Close))
eacf((aapl_ret$AAPL.Close))
plot(armasubsets((aapl_ret$AAPL.Close), nma = 15, nar = 15))

# double differencing
aapl_ret = diff(aapl_ret$AAPL.Close)
aapl_ret <- na.omit(aapl_ret)
plot(aapl_ret)
acf(aapl_ret$AAPL.Close) # MA(1)
pacf(aapl_ret$AAPL.Close) # MA(4)
eacf(aapl_ret$AAPL.Close) # ARMA(1,1)
plot(armasubsets(aapl_ret$AAPL.Close), nma = 15, nar = 15) # ARMA(1,2)
####################


# The candidate model is MA(1)
arima(aapl_ret) # white noise model
aapl_model = arima(aapl_ret, order = c(0,0,1), include.mean = F) # MLE MA(1)
aapl_model

# Residual analysis
# i
plot(rstandard(aapl_model), ylab = "Standardized Residuals", type="o")
abline(h=0)

# ii
acf(residuals(aapl_model))

Box.test(rstandard(aapl_model), type="Ljung-Box", lag=10, fitdf = 1)

# iii
qqnorm(rstandard(aapl_model))
qqline(rstandard(aapl_model))

hist(rstandard(aapl_model))

shapiro.test(rstandard(aapl_model))


# Overparamaterized model
new_aapl_model = arima(aapl_ret, order = c(1,0,1), include.mean = F) # MLE ARMA(1,1)

# Forecasting
predict(new_aapl_model,3)

plot(new_aapl_model, n.ahead=3)
