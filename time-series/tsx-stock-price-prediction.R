library(forecast)
library(ggplot2)
library(dplyr)
library(reshape)
library(Cairo)

# Stocks Forecasting Project

setwd("C:/Users/artemior/Desktop/Stocks Forecasting")

# Read in data and split the data into a training set and testing set

stocks.data <- read.csv("stocks_2009_2016.csv", header = TRUE)
stocks.training <- stocks.data[1:132,]
stocks.testing <- stocks.data[133:142,]

# Create time series objects for both training and testing sets

stocks.ts.data <- ts(stocks.data$Adj.Close, frequency = 12, start=c(2005,1), end=c(2016,9))
stocks.ts.training <- ts(stocks.training$Adj.Close, frequency = 12, start=c(2005,1), end=c(2015,12))
stocks.ts.testing <- ts(stocks.testing$Adj.Close, frequency = 12, start=c(2016,1), end=c(2016,9))

# Apply STL method to training time-series object and full data

stocks.stl.training <- stl(stocks.ts.data, s.window = "periodic")

stocks.remainder.training <- as.data.frame(stocks.stl.training$time.series[,3])[1:132,]

stocks.df.training <- as.data.frame(cbind(Time = time(stocks.remainder.training), 
                                          Month = rep(1:12, 11), 
                                          Remainder = stocks.remainder.training))

# Plot boxplots to see if the data is stationary

stocks.stationary <- ggplot(stocks.df.training, aes(x = as.factor(Month), y = Remainder)) + 
  geom_boxplot(fill = "skyblue")

# Generate ARIMA model on the training set

stocks.arima.training <- auto.arima(stocks.remainder.training, stepwise = FALSE, approximation = FALSE)

# Get a summary of the arima model 
summary(stocks.arima.training)

# Test the lack of fit of a time series model using the Box-Ljung test. Null hypothesis is
# the model does not provide a good fit. Alternative hypothesis that the model does provide
# a good fit. In this case, 
Box.test(stocks.arima.training$residuals, lag = 21, type = "Ljung", fitdf = 5)

# Forecasting the next 9 months and compare model performance, create data set to visualize

stocks.predict <- predict(stocks.arima.training, n.ahead = 9, prediction.interval = T, level = 0.95)
stocks.ahead <- forecast(stocks.arima.training, h = 9, level = 0.95)

stocks.actual <- data.frame(Time = round(time(stocks.stl.training$time.series[,3]),3),
                            Actual = as.data.frame(stocks.stl.training$time.series)[,3])

stocks.fitted <- data.frame(Time = round(time(stocks.stl.training$time.series[,3])[1:132],3),
                            Fitted = stocks.arima.training$x - stocks.arima.training$residuals)

stocks.forecast <- data.frame(Time = round(time(stocks.stl.training$time.series[,3])[133:141],3),
                              Forecast = stocks.predict$pred,
                              Lower = as.data.frame(stocks.ahead)[,2],
                              Upper = as.data.frame(stocks.ahead)[,3])

stocks.plot.data <- merge(stocks.actual, stocks.fitted, by='Time', all = TRUE)
stocks.plot.data <- merge(stocks.plot.data, stocks.forecast, by = 'Time', all = TRUE)

stocks.plot.data$Fitted[is.na(stocks.plot.data$Fitted)] <- 0
stocks.plot.data$Forecast[is.na(stocks.plot.data$Forecast)] <- 0
stocks.plot.data$Fitted <- stocks.plot.data$Fitted + stocks.plot.data$Forecast

stocks.plot.data.melt <- melt(stocks.plot.data[, c("Time", "Actual", "Fitted")], id = "Time")

# Visualize the forecasted stock prices using ggplot2

stocks.plot <- ggplot(stocks.plot.data.melt, aes(x = Time, y = value), colour = variable) +
  geom_line(aes(colour = variable), lwd = 1.1) +
  geom_ribbon(data = stocks.plot.data, aes(x = Time, y = Fitted, ymin = Lower, ymax = Upper), fill = "#fdce10", alpha = 0.2) +
  geom_vline(xintercept = min(stocks.forecast$Time), lty = 2, lwd = 0.5) +
  ggtitle("Forecasting 2016 Monthly Seasonally Adjusted Stock Prices using ARIMA(5,0,0)") +
  scale_x_continuous(breaks = seq(2005, 2016, 1)) +
  scale_y_continuous(breaks = seq(-2000, 2000, 400)) +
  scale_colour_manual(values = c("#e14318", "#207394")) +
  xlab("Year") +
  ylab("Seasonally Adjusted Stock Price") +
  theme(legend.title = element_blank(),
        axis.ticks = element_line(colour = "grey80", size = 0.5),
        axis.ticks.length = unit(0.1, "in"),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        panel.background = element_rect(fill = "white", colour = "white", size = 1),
        panel.grid.major = element_line(colour = "grey90", size = 0.1),
        plot.title = element_text(colour = "black", size = "15", face = "bold"))

# Save the file in a Cairo-png which will enhance the graphic
ggsave(file="stockpriceforecast.png", type="cairo-png", h = 7, w = 10)
ggsave(file="stockpricestationary.png", type="cairo-png", h = 7, w = 10)

# Run CairoWin() in to see what the enhanced graphic will look like 

CairoWin()
