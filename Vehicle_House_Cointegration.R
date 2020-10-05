# Plot the time-series of vehicles and houses sold in the U.S. from 1976 to 2020
plot(vehicles_ts, ylab="Houses and Vehicles Sold (in thousands)", ylim=c(0,1800), xlab = "Year", main = "Houses and Vehicles Sold")
lines(houses_ts, col="red")
lines(vehicles_ts, col="blue")
legend(x = 1975, y = 500, legend = c("Vehicles Sold", "Houses Sold"),text.col=c("blue", "red"),bty="n")

# Plot each time-series individually to get a better look at the dynamics of the data
plot(vehicles_ts, ylab="Vehicles Sold (in thousands)", xlab = "Year", main = "Vehicles Sold")

plot(houses_ts, ylab="Houses Sold (in thousands)", xlab = "Year", main = "Houses Sold")


# Plot the cross-correlation function
ccf(houses_ts,vehicles_ts,ylab="Cross-Correlation Function", main = "Houses and Vehicles Sold CCF")

# Fit a VAR model to the data
# Combine the data into one data frame
y <- cbind(houses_ts, vehicles_ts)
y_tot <- data.frame(y)

# Find the optimal order for the VAR model by looking at various criteria like AIC and SC
# Choose VAR model of order 10, all criteria agree
VARselect(y_tot)$selection

# Fit the VAR model, show the summary of the model, and plot the model
y_model <- VAR(y_tot,p=10)
summary(y_model)
par(mfrow=c(2,2))
plot(y_model$varresult$houses_ts$fitted.values, ylab = "Units Sold (in thousands)", 
     main = "VAR House Fitted Values", type = 'l')
plot(y_model$varresult$vehicles_ts$fitted.values, ylab = "Units Sold (in thousands)", 
     main = "VAR Vehicle Fitted Values", type = 'l')
plot(y_model$varresult$houses_ts$residuals, ylab = "Residuals", 
     main = "VAR House Series Residuals", type = 'l')
plot(y_model$varresult$vehicles_ts$residuals, ylab = "Residuals", 
     main = "VAR Vehicle Series Residuals", type = 'l')

# Plot the impulse-response functions for both series
par(mfrow=c(1,2))
plot(irf(y_model))

# Perform the Granger-Causality Test twice to see if one series Granger-Causes the other
grangertest(houses_ts ~ vehicles_ts, order = 10)
grangertest(vehicles_ts ~ houses_ts, order = 10)

# Predict the next 12 values of each series using the VAR model
# Plot the predictions
var.predict = predict(object=y_model, n.ahead=12)
plot(var.predict, xlim = c(300,550))

# Fit the house ARMA model and a GARCH Model to the house data
p_model <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(4, 1), include.mean = TRUE),
  distribution.model = "sstd")

p_model_fit <- ugarchfit(spec= p_model, data=houses_ts)
p_model_fit

# Forecast
model_p_for <- ugarchforecast(p_model_fit, data = NULL, n.ahead = 12, n.roll = 0, out.sample = 0)
model_p_for
