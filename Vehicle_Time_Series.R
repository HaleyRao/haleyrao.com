# load all relevant libraries
library(tseries)
library(forecast)
library(strucchange)
library(timeSeries)
library(vars)
library(tidyverse)
library(rugarch)

#Read in the data
vehicles <- read.csv("TOTALNSA.csv")

#Create a time series object for each data set, specifying the start and frequency
vehicles_ts <- ts(vehicles$TOTALNSA, start = c(1976, 1), frequency = 12)

# Plot the time series, ACF, and PACF for vehicles sold
tsdisplay(vehicles_ts, main="Number of Vehicles Sold in the U.S.")

# Take the seasonal and first difference of the data and then display the stationary series
vehicles_ts %>% diff(lag=12) %>% diff() %>% ggtsdisplay()

# Fit an ARIMA model that has non-seasonal AR and MA components, a seasonal AR component, and both differences
vehicle_model <- Arima(vehicles_ts, order = c(4,1,1), seasonal = c(2,1,0))

# Plot the residuals against the fitted values for the vehicle model
plot(vehicle_model$residuals ~ vehicle_model$fitted, main="Vehicle Model Residuals", 
     ylab="Residuals", xlab="Fitted Values")

# Plot the ACF and PACF of the vehicle model residuals
acf(vehicle_model$residuals, main="ACF of Vehicle Model Residuals")
pacf(vehicle_model$residuals, main="PACF of Vehicle Model Residuals")

# Compute and then plot the empirical fluctuation process based on recursive residuals and cumulative sums for the vehicle model
plot(efp(vehicle_model$residuals~1, type = "Rec-CUSUM"))

# Compute and then plot the recursive residuals for the vehicle model
plot(recresid(vehicle_model$residuals~1), pch=16,ylab="Recursive Residuals", main = "Vehicle Model Residuals")

# Perform a Ljung-Box test on the residuals to check for autocorrelation, and plot the residuals, their ACF plot, and a histogram to show their distribution
checkresiduals(vehicle_model)

# Forecast the vehicle model 12-steps ahead and plot it along with the time series from 2010 to 2021
forecast(vehicle_model, h=12)
plot(forecast(vehicle_model, h=12), main="12-step Ahead Forecast for Vehicles Sold", xlab="Year", ylab="Units Sold (in thousands)", xlim=c(2010, 2021))

# Split data into training and testing sets (the testing set is the last 12 observation of the data)
vehicles_ts_train <- window(vehicles_ts, end = c(2019, 1))
vehicles_ts_test <- window(vehicles_ts, start = c(2019, 2))

# Fit the vehicle model again with training data
# Calculate MAPE for the forecasts of the house_model on the testing data
vehicle_model_2 <- Arima(vehicles_ts_train, order = c(4,1,1), seasonal = c(2,1,0))
accuracy(forecast(vehicle_model_2, vehicles_ts_test))[5]

# Fit an ARIMA model with training data
# Calculate MAPE for the forecasts of the ARIMA model on the testing data
vehicle_arima_model <- auto.arima(vehicles_ts_train)
accuracy(forecast(vehicle_arima_model, vehicles_ts_test))[5]

# Fit a Holt-Winters model on the training data
# Calculate MAPE for the 12-step ahead forecasts for the Holt-Winters model
accuracy(forecast(HoltWinters(vehicles_ts_train), h=12))[5]

# Fit an ETS model on the training data
# Calculate MAPE for the 12-step ahead forecasts for the ETS model
accuracy(forecast(ets(vehicles_ts_train), h=12))[5]

# Take an average of the individual forecasts
# Calculate the MAPE of the average of the forecasts using the testing data
vehicle_combo <- (forecast(vehicle_model_2, vehicles_ts_test)$mean + forecast(vehicle_arima_model, vehicles_ts_test)$mean + forecast(HoltWinters(vehicles_ts_train), h=12)$mean + forecast(ets(vehicles_ts_train), h=12)$mean)/4

accuracy(vehicle_combo, vehicles_ts_test)[5]


