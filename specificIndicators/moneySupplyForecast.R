# Required libraries

library(quantmod)
library(forecast)
library(mFilter)
library(timeSeries)
library(ggplot2)

# Time Series Specifications

horizon <- 24
startDate <- as.Date('1960-01-01')
endDate <- as.Date('2014-01-01')
series <- "M2SL"
scaleFactor <- 1000
y.title <- "Trillions of Dollars\n"
chart.title <- "M2 Money Supply (M2SL)\n"

# Retrieve and subset data from FRED

indicator <- getSymbols(series, src = 'FRED', auto.assign = FALSE)
indicator <- window(indicator, start = startDate) / scaleFactor

# Hoddrick Prescott for Trend and Cycle

indicator.hp <- hpfilter(indicator, freq = 100000000, type = 'lambda')
hp.data <- data.frame(as.Date(rownames(indicator.hp$x)), 
		indicator.hp$x, indicator.hp$trend, indicator.hp$cycle)
names(hp.data) <- c("xTime", "indicator", "trend", "cycle")

# Extend the trend 
spline.model <- smooth.spline(hp.data$xTime, hp.data$trend)
futureTime <- seq(as.Date(max(time(indicator))), by="months",
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
govFinance <- "http://marriottschool.net/teacher/govfinance/economy/indicators/"
con <- url(paste(govFinance, "recessions.RData", sep = ""))
load(con)
close(con)
recessions.trim <- subset(recessions.df, Peak >= startDate)
rm(govFinance, con, recessions.df) # cleanup

# Create the plot

g <- ggplot(predicted)
g <- g + geom_rect(data = recessions.trim, aes(xmin = Peak, xmax = Trough, 
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
		scale_x_date("", lim = c(startDate, endDate), major = "5 years") +
		scale_y_continuous(y.title) +
		opts(title = chart.title, plot.title = theme_text(size = 20),
				legend.position = "none")
print(g)

# print the plot

ggsave(g, filename = "Indicator.pdf", width = 9, height = 7)

rm(horizon, startDate, endDate, series, scaleFactor, y.title, chart.title)
rm(predicted, recessions.trim, hp.data, forecast.df, g)