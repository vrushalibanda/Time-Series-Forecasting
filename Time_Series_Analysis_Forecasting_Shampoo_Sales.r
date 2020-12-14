
# USE FORECAST & ZOO LIBRARY
library(forecast)
library(zoo)

#Set working directory for locating files.
#setwd("C:/Users/kishm/OneDrive/Documents/MSBA/BAN 673 - Time Series Analytics")

#Create data frame.
shampoo.data<-read.csv("Shampoo_Sales.csv")

# See the first 6 records of the file and last 6 records of the data set
head(shampoo.data)
tail(shampoo.data)

## USE ts() FUNCTION TO CREATE TIME SERIES DATA SET for ShampooSales.
sales.ts <- ts(shampoo.data$Sales, 
               start = c(1995, 1), end = c(1997, 12), freq = 12)
head(sales.ts)

## Use plot() to plot time series data  
plot(sales.ts, 
     xlab = "Time", ylab = "Shampoo Sales", 
     ylim = c(100, 700), bty = "l",
     xaxt = "n", xlim = c(1995, 1998.25), main = "Shampoo sales", lwd = 3, col="blue") 
axis(1, at = seq(1995, 1998.25, 1), labels = format(seq(1995, 1998.25, 1)))


# Use stl() function to plot times series components of the original data. 
sales.stl <- stl(sales.ts, s.window = "periodic")
autoplot(sales.stl, main = "Shampoo sales Time Series Components")

# Use Acf() function to identify autocorrelation and plot autocorrelation
autocor <- Acf(sales.ts, lag.max = 12, main = "Autocorrelation for Shampoo Sales")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

#Forecasting Methods
## Data Driven 
## 1 : Seasonal Naive-Basemodel
ShampooSales.snaive.pred <- snaive(sales.ts)
round(accuracy(ShampooSales.snaive.pred$fitted, sales.ts), 3)

# Plot the predictions for seasonal naive forecast.
plot(ShampooSales.snaive.pred$fitted, 
     xlab = "Time", ylab = "Shampoo Sales", 
     ylim = c(100, 700), bty = "l",
     xaxt = "n", xlim = c(1995, 1998.25), main = "Seasonal Naive Forecast", col = "blue", lwd =2) 
axis(1, at = seq(1995, 1998.25, 1), labels = format(seq(1995, 1998.25, 1)))
lines(ShampooSales.snaive.pred$fitted, col = "blue", lwd = 2)

##2: NAIVE FORECASTS
ShampooSales.naive.pred <- naive(sales.ts)
round(accuracy(ShampooSales.naive.pred$fitted, sales.ts), 3)

# Plot the predictions for naive forecast.
plot(ShampooSales.naive.pred$fitted, 
     xlab = "Time", ylab = "Shampoo Sales", 
     ylim = c(100, 700), bty = "l",
     xaxt = "n", xlim = c(1995, 1998.25), main = "Naive Forecast", col = "blue", lwd =2) 
axis(1, at = seq(1995, 1998.25, 1), labels = format(seq(1995, 1998.25, 1)))
lines(ShampooSales.naive.pred$fitted, col = "blue", lwd = 2)

##3 : Moving Average - Trailing
#Create trailing moving average with window (number of periods) k = 2,4,6,12
ma.trailing_2 <- rollmean(sales.ts, k = 2, align = "right")
ma.trailing_4 <- rollmean(sales.ts, k = 4, align = "right")
ma.trailing_6 <- rollmean(sales.ts, k = 6, align = "right")
ma.trailing_12 <- rollmean(sales.ts, k = 12, align = "right")

ma.trail_2 <- c(rep(NA, length(sales.ts) - length(ma.trailing_2)), ma.trailing_2)
ma.trail_4 <- c(rep(NA, length(sales.ts) - length(ma.trailing_4)), ma.trailing_4)
ma.trail_6 <- c(rep(NA, length(sales.ts) - length(ma.trailing_6)), ma.trailing_6)
ma.trail_12 <- c(rep(NA, length(sales.ts) - length(ma.trailing_12)), ma.trailing_12)

ma_trailing_tab <- cbind(sales.ts, ma.trail_2, ma.trail_4, ma.trail_6,ma.trail_12)
ma_trailing_tab

## Create trailing MA forecast for 12 periods into the future.
ma.trailing_2.pred <- forecast(ma.trailing_2, h=12, level = 0)
ma.trailing_2.pred

ma.trailing_4.pred <- forecast(ma.trailing_4, h=12, level = 0)
ma.trailing_4.pred

ma.trailing_6.pred <- forecast(ma.trailing_6, h=12, level = 0)
ma.trailing_6.pred

ma.trailing_12.pred <- forecast(ma.trailing_12, h=12, level = 0)
ma.trailing_12.pred

#Accuarcy comparison
round(accuracy(ma.trail_2, sales.ts), 3) 
round(accuracy(ma.trail_4, sales.ts), 3)
round(accuracy(ma.trail_6, sales.ts), 3)
round(accuracy(ma.trail_12, sales.ts), 3)
round(accuracy(naive(sales.ts)$fitted,sales.ts),3)

## Plot original data and trailing MA

plot(sales.ts, 
     xlab = "Time", ylab = "Shampoo Sales(in thousands$)", ylim = c(100,700), bty = "l",
     xlim = c(1995, 1999.25), main = "Trailing Moving Average") 
axis(1, at = seq(1995, 1997.25, 1), labels = format(seq(1995, 1997.25, 1)))
lines(ma.trailing_2, col = "red", lwd = 2, lty = 2)
lines(ma.trailing_4, col = "green", lwd = 2, lty = 3)
lines(ma.trailing_6, col = "brown", lwd = 2, lty = 1)
lines(ma.trailing_12,col = "blue",  lwd = 2, lty = 4)
legend(1995,600, legend = c("Original Shampoo sales","Trailing MA, k=2","Trailing MA, k=4", 
                            "Trailing MA, k=6","Trailing MA, k=12"), 
       col = c("black","red", "green", "brown","blue"), 
       lty = c(1, 1, 5), lwd =c(1, 2, 2), bty = "n")

##3 : Advanced Exponential Smoothing (Holt-Winter's Model)
hw.ZZZ <- ets(sales.ts, model = "ZZZ")
hw.ZZZ

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = 12, level= 0)
hw.ZZZ.pred

# Accuracy comparison with baseline naive models
round(accuracy(hw.ZZZ.pred$fitted, sales.ts), 3)
round(accuracy((naive(sales.ts))$fitted, sales.ts), 3)
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)

# Plot HW predictions for original data, optimal smoothing parameters.
plot(hw.ZZZ.pred, 
     xlab = "Time", ylab = "Shampoo Sales", ylim = c(100, 700), bty = "l",
     xaxt = "n", xlim = c(1995,1999), 
     main = "Holt-Winter's Model with Automated Selection of Model Options", flty = 2) 
axis(1, at = seq(1995, 1998, 1), labels = format(seq(1995, 1998, 1)))
lines(hw.ZZZ.pred$fitted, col = "blue", lwd = 2)

# plot on the chart vertical lines and horizontal arrows
lines(c(1995, 1995), c(100, 700))
lines(c(1997.92,1997.92), c(100, 700))
text(1996.25,700, "Entire Data")
text(1998.5,700, "Future")
arrows(1995, 650, 1997.92, 650, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1997.90, 650, 2000, 650, code = 3, length = 0.1,
       lwd = 1, angle = 30)

##4 Regression Based Models 
## 4

#i Regression model with linear trend
Reg.lin <- tslm(sales.ts ~ trend)
summary(Reg.lin)

Reg.lin.pred <- forecast(Reg.lin, h = 12, level = 0)
Reg.lin.pred

#iiRegression model with quadratic trend
Reg.quad <- tslm(sales.ts ~ trend + I(trend^2))
summary(Reg.quad)

Reg.quad.pred <- forecast(Reg.quad, h = 12, level = 0)
Reg.quad.pred

#iii.	Regression model with seasonality 
Reg.sea <- tslm(sales.ts ~ season)
summary(Reg.sea)

Reg.sea.pred <- forecast(Reg.sea, h = 12, level = 0)
Reg.sea.pred


#iv.Regression model with linear trend and seasonality
Reg.lintrend.sea <- tslm(sales.ts ~ trend + season)
summary(Reg.lintrend.sea)

Reg.lintrend.sea.pred <- forecast(Reg.lintrend.sea, h = 12, level = 0)
Reg.lintrend.sea.pred


#v.Regression model with quadratic trend and seasonality
Reg.quadtrend.sea<- tslm(sales.ts ~ trend + I(trend^2) + season)
summary(Reg.quadtrend.sea)

Reg.quadtrend.sea.pred <- forecast(Reg.quadtrend.sea, h = 12, level = 0)
Reg.quadtrend.sea.pred


#Comparison

#Regression Model with Linear Trend
round(accuracy(Reg.lin.pred$fitted, sales.ts), 3)

#Regression Model with Quadratic Trend
round(accuracy(Reg.quad.pred$fitted, sales.ts), 3)

#Regression Model with Seasonality
round(accuracy(Reg.sea.pred$fitted, sales.ts), 3)

#Regression Model with Linear Trend and Seasonality
round(accuracy(Reg.lintrend.sea.pred$fitted, sales.ts), 3)

#Regression Model with Quadratic Trend and Seasonality
round(accuracy(Reg.quadtrend.sea.pred$fitted,sales.ts), 3)

#The lowest Mape and rmse is quadratic trend and seaonality
plot(Reg.quadtrend.sea.pred, 
     xlab = "Time", ylab = "Shampoo sales", ylim = c(100, 1100), bty = "l",
     xaxt = "n", xlim = c(1995, 1999), 
     main = "Regression Model with Quadractic  Trend and Seasonality ", lwd = 2, flty = 5) 
axis(1, at = seq(1995, 1999, 1), labels = format(seq(1995, 1999, 1)))
lines(Reg.quadtrend.sea.pred$fitted, col = "blue", lwd = 2)
lines(sales.ts, col = "black", lwd = 2, lty = 1)


lines(c(1995, 1995), c(100, 1100))
lines(c(1998, 1998), c(100, 1100))
text(1996.5, 1100, "Entire Data")
text(1998.5, 1100, "Future")
arrows(1995.06, 1000, 1997.94, 1000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1998.06, 1000, 1999, 1000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# ACF of Regession Residual 
Acf(Reg.quadtrend.sea$residuals, lag.max = 12, 
    main = "Autocorrelation for Shamppo Sales Regression Residuals")
# Two-Level Model's Forecast: 
# Regression with Quadratic Trend and Seasonality + MA Trailing (window width of 2)
# Regression model with quadratic trend and seasonality
summary(Reg.quadtrend.sea) # from the previous part 
# Forecast in 1998 
Reg.quadtrend.sea.pred

# Identify regression residuals
Reg.quadtrend.sea.res <- Reg.quadtrend.sea$residuals
Reg.quadtrend.sea.res

# apply a trailing MA (window width of 2) for these residuals using the rollmean() function
MA.trailing.res_2 <- rollmean(Reg.quadtrend.sea.res, k = 2, align = "right")
MA.trailing.res_2
# and forecast sales in 1998 
MA.trailing.res_2.pred <- forecast(MA.trailing.res_2, h = 12, level = 0)
MA.trailing.res_2.pred

# Combine the regression and trailing MA residuals’ forecast for 1998.
reg.combine.forecast<- Reg.quadtrend.sea.pred$mean + MA.trailing.res_2.pred$mean
reg.combine.forecast

# Create a table that contains regression forecast, trailing MA forecast for residuals, 
# and total (combined) forecast for sales in 1998 

table.Reg.MA.forecast <- data.frame('Regression'=Reg.quadtrend.sea.pred$mean , 
                                    'Trailing MA of Residuals'=MA.trailing.res_2.pred$mean, 
                                    'Total Forecast'=reg.combine.forecast)
table.Reg.MA.forecast



#Plot Two-Level Forecast: Regression with Quadratic Trend and Seasonlity + MA Trailing 
plot(sales.ts, 
     xlab = "Time", ylab = "Shampoo sales", ylim = c(100, 1100), bty = "l",
     xaxt = "n", xlim = c(1995,1999), lwd = 2,
     main = "Two-Level Forecast: Regression with Quadratic Trend and Seasonlity 
     + Trailing MA (window width of 2) for Residuals") 
axis(1, at = seq(1995, 1999, 1), labels = format(seq(1995, 1999, 1)))
lines(Reg.quadtrend.sea$fitted + MA.trailing.res_2, col = "blue", lwd = 2)
lines(reg.combine.forecast, col = "blue", lty = 5, lwd = 2)
legend(1995,1000, legend = c("Historical data of Shampoo Sales", 
                             "Two-Level Forecast for entire data", "Two-Level Forecast for 12 months in 1998"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1995, 1995), c(100, 1100))
lines(c(1998, 1998), c(100, 1100))
text(1996.5, 1100, "Entire Data")
text(1998.5, 1100, "Future")
arrows(1995.06, 1000, 1997.94, 1000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1998.06, 1000, 1999, 1000, code = 3, length = 0.1,
       lwd = 1, angle = 30)


# Two-Level Model's Forecast: 
# REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY + AR FOR RESIDUALS 

# Regression with Quadratic Trend and Seasonlity + AR(1) for residuals
# Regression model with quadratic trend and seasonality
summary(Reg.quadtrend.sea) # from the previous part 
# Forecast in 1998 
Reg.quadtrend.sea.pred

# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into the next 12 months .
residual.AR1 <- Arima(Reg.quadtrend.sea$residuals, order = c(1,0,0))
residual.AR1.pred <- forecast(residual.AR1, h = 12, level = 0)
# Use summary() to identify parameters of AR(1) model.
summary(residual.AR1)
# Use Acf() function to identify autocorrealtion for the residual of residuals 
# and plot autocorrelation for different lags 
Acf(residual.AR1$residuals, lag.max = 12, 
    main = "Autocorrelation for Shampoo Sales Residuals of Residuals for Entire Data Set
    AR(1)")
# Identify forecast for the next 12 periods as sum of quadratic trend and seasonal model
# and AR(1) model for residuals.
Reg.quadtrend.sea.AR1.pred <- Reg.quadtrend.sea.pred$mean + residual.AR1.pred$mean
Reg.quadtrend.sea.AR1.pred
# plot historical data, predictions for historical data, and forecast for next 12 months in 1998 .
plot(sales.ts, 
     xlab = "Time", ylab = "Shampoo Sales ", ylim = c(100, 1100), bty = "l",
     xaxt = "n", xlim = c(1995, 1999), lwd = 2,
     main = "Two-Level Forecast: Regression with Quadratic Trend and Seasonlity
     + AR(1) for Residuals") 
axis(1, at = seq(1995, 1999, 1), labels = format(seq(1995, 1999, 1)))
lines(Reg.quadtrend.sea$fitted + residual.AR1$fitted, col = "blue", lwd = 2)
lines(Reg.quadtrend.sea.AR1.pred, col = "blue", lty = 5, lwd = 2)
legend(1995,1000, legend = c("Historical data of Shampoo Sales", 
                             "Two-Level Forecast for entire data", "Two-Level Forecast for 12 months in 1998"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1995, 1995), c(100, 1100))
lines(c(1998, 1998), c(100, 1100))
text(1996.5, 1100, "Entire Data")
text(1998.5, 1100, "Future")
arrows(1995.06, 1000, 1997.94, 1000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1998.06, 1000, 1999, 1000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Regression with Quadratic Trend and Seasonlity + AR(2) for residuals

# Use Arima() function to fit AR(2) model for regression residuals.
# The AR model order of order = c(2,0,0) gives an AR(2) model.
# Use forecast() function to make prediction of residuals into the next 12 months .
residual.AR2 <- Arima(Reg.quadtrend.sea$residuals, order = c(2,0,0))
residual.AR2.pred <- forecast(residual.AR2, h = 12, level = 0)
# Use summary() to identify parameters of AR(2) model.
summary(residual.AR2)
# Use Acf() function to identify autocorrealtion for the residual of residuals 
# and plot autocorrelation for different lags 
Acf(residual.AR2$residuals, lag.max = 12, 
    main = "Autocorrelation for Shampoo Sales Residuals of Residuals for Entire Data Set
    AR(2)")
# Identify forecast for the next 12 periods as sum of quadratic trend and seasonal model
# and AR(2) model for residuals.
Reg.quadtrend.sea.AR2.pred <- Reg.quadtrend.sea.pred$mean + residual.AR2.pred$mean
Reg.quadtrend.sea.AR2.pred
# plot historical data, predictions for historical data, and forecast for next 12 months in 1998 .
plot(sales.ts, 
     xlab = "Time", ylab = "Shampoo Sales ", ylim = c(100, 1100), bty = "l",
     xaxt = "n", xlim = c(1995, 1999), lwd = 2,
     main = "Two-Level Forecast: Regression with Quadratic Trend and Seasonlity
     + AR(2) for Residuals") 
axis(1, at = seq(1995, 1999, 1), labels = format(seq(1995, 1999, 1)))
lines(Reg.quadtrend.sea$fitted + residual.AR2$fitted, col = "blue", lwd = 2)
lines(Reg.quadtrend.sea.AR2.pred, col = "blue", lty = 5, lwd = 2)
legend(1995,1000, legend = c("Historical data of Shampoo Sales", 
                             "Two-Level Forecast for entire data", "Two-Level Forecast for 12 months in 1998"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1995, 1995), c(100, 1100))
lines(c(1998, 1998), c(100, 1100))
text(1996.5, 1100, "Entire Data")
text(1998.5, 1100, "Future")
arrows(1995.06, 1000, 1997.94, 1000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1998.06, 1000, 1999, 1000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Regression with Quadratic Trend and Seasonlity + AR(3) for residuals

# Use Arima() function to fit AR(3) model for regression residuals.
# The AR model order of order = c(3,0,0) gives an AR(3) model.
# Use forecast() function to make prediction of residuals into the next 12 months .
residual.AR3 <- Arima(Reg.quadtrend.sea$residuals, order = c(3,0,0))
residual.AR3.pred <- forecast(residual.AR3, h = 12, level = 0)
# Use summary() to identify parameters of AR(2) model.
summary(residual.AR3)
# Use Acf() function to identify autocorrealtion for the residual of residuals 
# and plot autocorrelation for different lags 
Acf(residual.AR3$residuals, lag.max = 12, 
    main = "Autocorrelation for Shampoo Sales Residuals of Residuals for Entire Data Set
    AR(3)")

# Identify forecast for the next 12 periods as sum of quadratic trend and seasonal model
# and AR(3) model for residuals.
Reg.quadtrend.sea.AR3.pred <- Reg.quadtrend.sea.pred$mean + residual.AR3.pred$mean
Reg.quadtrend.sea.AR3.pred
# plot historical data, predictions for historical data, and forecast for next 12 months in 1998 .
plot(sales.ts, 
     xlab = "Time", ylab = "Shampoo Sales ", ylim = c(100, 1100), bty = "l",
     xaxt = "n", xlim = c(1995, 1999), lwd = 2,
     main = "Two-Level Forecast: Regression with Quadratic Trend and Seasonlity
     + AR(3) for Residuals") 
axis(1, at = seq(1995, 1999, 1), labels = format(seq(1995, 1999, 1)))
lines(Reg.quadtrend.sea$fitted + residual.AR3$fitted, col = "blue", lwd = 2)
lines(Reg.quadtrend.sea.AR3.pred, col = "blue", lty = 5, lwd = 2)
legend(1995,1000, legend = c("Historical data of Shampoo Sales", 
                             "Two-Level Forecast for entire data", "Two-Level Forecast for 12 months in 1998"), 
       col = c("black", "blue" , "blue"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(1995, 1995), c(100, 1100))
lines(c(1998, 1998), c(100, 1100))
text(1996.5, 1100, "Entire Data")
text(1998.5, 1100, "Future")
arrows(1995.06, 1000, 1997.94, 1000, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(1998.06, 1000, 1999, 1000, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Create a data table with quadratic trend and seasonal forecast for next 12 months,
# AR(3)model for residuals for for next 12 months, and combined two-level forecast for
# for next 12 months.
table.Reg.AR3.forecast <- data.frame("Reg.Forecast"= Reg.quadtrend.sea.pred$mean, 
                                    "ARIMA Forecast Residuals "=residual.AR3.pred$mean, 
                                    "Combined.Forecast"=Reg.quadtrend.sea.AR3.pred)
table.Reg.AR3.forecast 

# Accuracy of 4 two-level models: 
#Regression with quadratic trend and seasonality + MA Trailing (window width of 2)
round(accuracy(Reg.quadtrend.sea.pred$fitted+MA.trailing.res_2, sales.ts), 3)
# Regression with quadratic trend and seasonal model + AR(1) model for residuals
round(accuracy(Reg.quadtrend.sea$fitted + residual.AR1$fitted, sales.ts), 3)
# Regression with quadratic trend and seasonal model + AR(2) model for residuals
round(accuracy(Reg.quadtrend.sea$fitted + residual.AR2$fitted, sales.ts), 3)
# Regression with quadratic trend and seasonal model + AR(3) model for residuals
round(accuracy(Reg.quadtrend.sea$fitted + residual.AR3$fitted, sales.ts), 3)



