# Forecasting Script for FRED Data
# June 16, 2014
# Author: Ray Nelson
###############################################################################
library(fImport)
library(forecast)
library(timeSeries)

# time series and dates
FREDSeries <- 'HSN1F'
startDate <- as.Date('1960-01-01')
endDate <- as.Date('2015-01-01')
scaleFactor <- 1
horizon <- 12
smoothConstant <- 0.05
y.title <- 'Thousands of Units\n'
chart.title <- paste('Private Single Unit Housing Starts (', FREDSeries, ')\n',
	sep = '')

indicator <- fredSeries(FREDSeries, from = startDate) / scaleFactor
tsdisplay(indicator)

# load and subset recessions and presidents data for ggplot2
load(url("http://marriottschool.net/teacher/govfinance/recessions.RData"))
recessions <- subset(recessions, Start >= startDate)
presidents <- as.numeric(timeCalendar(y = seq(1961, 2015, 4), m = 1, d = 1))

# Exponential Smoothing
(ets.forecast <- ets(indicator, model = "ZZZ"))
(ets.forecast <- forecast(ets.forecast, h = horizon))
plot(ets.forecast)

# Data frame for ggplot
date <- as.Date(timeSequence(from = end(indicator), by = 'month',
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
	geom_rect(data = recessions, aes(xmin = Start, xmax = End, 
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

# save graph to file
ggsave(filename = paste(FREDSeries, '.pdf', sep = ''), width = 9, height = 7)
ggsave(filename = paste(FREDSeries, '.png', sep = ''),
		width = 8, height = 5, dpi = 100) # png format for powerpoint

# ARIMA Forecast
(arima.model <- auto.arima(indicator))
(arima.forecast <- forecast(arima.model, h = 12))
plot(arima.forecast)

# Data frame for ggplot
date <- as.Date(timeSequence(from = end(indicator), by = 'month',
				length.out = (horizon + 1)))
forecastData <- data.frame(date[2:length(date)],
		ets.forecast$lower[, 2], arima.forecast$lower[, 1],
		ets.forecast$mean, ets.forecast$upper)
names(forecastData) <- c('date', 'lower95', 'lower80', 'forecast',
		'upper80', 'upper95')
plotData <- data.frame(as.Date(time(indicator)), series(indicator))
colnames(plotData) <- c("date", "indicator")

# plot for exponential smoothing forecasts
ggplot(plotData) +
	geom_rect(data = recessions, aes(xmin = Start, xmax = End, 
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
	labs(title = paste(chart.title, "ARIMA Model\n"), y = y.title) +
	theme(plot.title = element_text(size = 20))

# save graph to file
ggsave(filename = paste(FREDSeries, '.pdf', sep = ''), width = 9, height = 7)
ggsave(filename = paste(FREDSeries, '.png', sep = ''),
		width = 8, height = 5, dpi = 100) # png format for powerpoint

# Cleanup
rm(FREDSeries, startDate, endDate, scaleFactor, horizon,
		chart.title, y.title, smoothConstant, ets.forecast,
		indicator, plotData, date,
		forecastData, recessions, presidents, arima.model, arima.forecast)