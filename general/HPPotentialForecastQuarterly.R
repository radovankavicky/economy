# Monthly FRED data with general trend
# Author: Ray Nelson
# June 26, 2014
###############################################################################
# load libraries
library("fImport")
library("mFilter")
library("timeSeries")
library("forecast")
library("ggplot2")
library("reshape2")

# Time Series Specifications

horizon <- 8
frequencyParameter <- 100000
startDate <- as.Date('1960-01-01')
endDate <- as.Date('2015-01-01')
series <- 'GDPC96'
scaleFactor <- 1000
y.title <- "Trillions of Dollars\n"
chart.title <- "Gross Domestic Product (GDPC96)\n"

# Retrieve and subset data from FRED
indicator <- fredSeries(series, from = startDate) / scaleFactor

indicator.hp <- hpfilter(indicator, freq = frequencyParameter, type = 'lambda')
hp.data <- data.frame(as.Date(rownames(indicator.hp$x)), 
		indicator.hp$x, indicator.hp$trend, indicator.hp$cycle)
names(hp.data) <- c("xTime", "indicator", "trend", "cycle")

# Extend the trend 
spline.model <- smooth.spline(hp.data$xTime, hp.data$trend)
futureTime <- seq(as.Date(max(time(indicator))), by = "3 months",
		length = (horizon + 1))
futureTime <- futureTime[2:(horizon + 1)]
xTime <- c(hp.data$xTime, futureTime)
predicted <- predict(spline.model, as.numeric(xTime))
predicted <- data.frame(xTime, smooth.trend = predicted$y)

# forecast next five years
indicator.ets = ets(as.timeSeries(indicator), model = "ZZZ")
forecast.ets = forecast(indicator.ets, h = horizon)
forecast.df <- data.frame(xTime = futureTime, forecast.ets)

# cleanup
rm(indicator, indicator.hp, spline.model, futureTime, xTime,
		indicator.ets, forecast.ets)

# Load and subset recessions dates and subset the data frame
load('recessions.RData')
recessions <- subset(recessions, Start >= startDate)

# Create the plot

g <- ggplot(predicted)
g <- g + geom_rect(data = recessions, aes(xmin = Start, xmax = End, 
				ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
		geom_line(aes(x = xTime, y = smooth.trend)) + 
		geom_ribbon(aes(x = xTime, ymin = trend, ymax = indicator),
				data = hp.data, fill = "lightskyblue3") +
		geom_line(aes(x = xTime, y = indicator), data = hp.data) +
		geom_ribbon(aes(x = xTime, ymin = Lo.95, ymax = Hi.95),
				data = forecast.df, fill = "lightskyblue1") +
		geom_ribbon(aes(x = xTime, ymin = Lo.80, ymax = Hi.80),
				data = forecast.df, fill = "lightgoldenrod") +
		geom_line(aes(x = xTime, y = smooth.trend), size = 1.0,
				color = "darkblue") +
		geom_line(aes(x = xTime, y = Point.Forecast), data = forecast.df,
				size = 1.0, colour = "darkred")	+
		scale_x_date("", lim = c(startDate, endDate)) +
		scale_y_continuous(y.title) +
		labs(title = chart.title) +
		theme(legend.position = 'none', plot.title = element_text(size = 20))
print(g)

# print the plot

ggsave(g, filename = "Indicator.pdf", width = 9, height = 7)

# Cleanup
rm(horizon, startDate, endDate, series, scaleFactor, y.title, chart.title,
		frequencyParameter)
rm(predicted, forecast.spline, recessions, hp.data, forecast.df, g)