# New Homes Sales and Prices
# June 16, 2014
# Author: Ray
###############################################################################
# libraries (hopefully these are all the needed libraries
library(fImport)
library(forecast)
library(timeSeries)

# time series and recession dates
startDate <- as.Date('1960-01-01')
endDate <- as.Date('2015-01-01')
horizon <- 12
load(url("http://marriottschool.net/teacher/govfinance/recessions.RData"))
recessions.trim <- subset(recessions, Start >= startDate)

# get data from FRED
homeSales <- fredSeries("HSN1F", from = startDate)
homePrices <- fredSeries("MSPNHSUS", from = startDate)

# forecast home sales using exponential smoothing
sales.model <- ets(homeSales)
(sales.forecast <- forecast(sales.model, h = 12))
plot(sales.forecast)

# Data frame for ggplot
date <- as.Date(timeSequence(from = end(homeSales), by = 'month',
				length.out = (horizon + 1)))
forecastData <- data.frame(date[2:length(date)],
		ets.forecast$lower[, 2], ets.forecast$lower[, 1],
		ets.forecast$mean, ets.forecast$upper)
names(forecastData) <- c('date', 'lower95', 'lower80', 'forecast',
		'upper80', 'upper95')
plotData <- data.frame(as.Date(time(indicator)), series(indicator))
colnames(plotData) <- c("date", "indicator")

# plot for exponential smoothing forecasts
ggplot(plotData) +
		geom_rect(data = recessions.trim, aes(xmin = Peak, xmax = Trough, 
					ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
		geom_vline(xintercept = presidents, colour = 'lightblue', size = 0.50) +
		geom_ribbon(data = forecastData, fill = 'lightblue',
				aes(x = date, ymin = lower95, ymax = upper95)) +
		geom_ribbon(data = forecastData, fill = 'yellow',
				aes(x = date, ymin = lower80, ymax = upper80)) +
		geom_line(data = forecastData, aes(x = date, y = forecast), size = 1.0,
				colour = 'red') +
		geom_point(aes(x= date, y = indicator), size = 1.0, color = "red") +
		geom_smooth(aes(x= date, y = indicator), span = smoothConstant,
				size = 0.50, color = "darkblue", fill = "springgreen4") +
		scale_x_date("", lim = c(startDate, endDate)) +
		labs(title = paste(chart.title, "ETS Exponential Smoothing Model\n"),
				y = y.title) +
		theme(plot.title = element_text(size = 20))


# forecast home sales using exponential smoothing
price.model <- ets(homePrices)
(price.forecast <- forecast(price.model, h = 12))
plot(price.forecast)

# ggplot graph of smoothed data and forecast
plotData <- data.frame(date = as.Date(time(homeSales)),
		coredata(homeSales))
colnames(plotData) <- c("date", "indicator")
ggplot(data = plotData, aes(x = date, y = indicator)) +
		geom_point(colour = 'red', size = 1.5) +
		geom_smooth(method = 'loess', span = 0.14, size = 1.0, colour = 'blue') +
		labs(title = 'Private Single Unit Housing Starts\n', x = '',
				y = "Thousands of Units")