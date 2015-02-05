# Nonfarm employment forecasting using the forecast package
# Author: Ray Nelson
# March 6, 2013
###############################################################################
# load libraries
library(fImport)

# Series IDs for employment data from FRED
# US Total NonFarm Employment (PAYEMS)
# Utah Total NonFarm Employment (UTNA)
# Utah Total Government Employment (UTGOVT)
# time series and dates
startDate <- as.Date('1959-01-01')
endDate <- as.Date('2013-04-01')
scaleFactor <- 1000
y.title <- "Nonfarm Employment\n"
chart.title <- "Nonfarm Payroll\n"

# get employment data from FRED and calculate the change or first difference
indicator <- fredSeries('PAYEMS', from = startDate) / scaleFactor
indicator.diff <- na.omit(diff(indicator, lag = 1))

# forecast payroll jobs using exponential smoothing
(model.ets <- ets(indicator, model = 'ZZN'))
(forecast.ets<- forecast(model.ets, h = 12))
plot(forecast.ets,
	main = "One Year Forecast for Total Nonfarm Employment")

# forecast of number of jobs gained or lost using exponential smoothing
(model.ets <- ets(indicator.diff, model = 'ZZN'))
(forecast.ets <- forecast(model.ets, h = 12))
plot(forecast.ets,
	main = "One Year Exponential Smoothing Forecast for Jobs Growth")
abline(h = 0, col = 'grey')

# forecast of number of jobs gained or lost using ARIMA
(model.arima <- auto.arima(as.ts(indicator.diff)))
(forecast.arima <- forecast(model.arima, h = 12))
plot(forecast.arima, main = 'One Year ARIMA Forecasts for Jobs Growth')
abline(h = 0, col = 'grey')

# forecast using a cubic spline
(forecast.spline <- splinef(ts(indicator.diff), h = 12))
summary(forecast.spline)
plot(forecast.spline, main = 'One Year Cubic Spline Forecasts for Jobs Growth')
abline(h = 0, col = 'grey', lwd = 1.0)

# Plot of job change
# Data for Recession Bars
load("recessions.RData")
recessions.trim <- subset(recessions.df, Peak >= startDate)

plotData <- data.frame(date = as.Date(time(indicator.diff)),
	series(indicator.diff))
names(plotData) <-  c('date', 'indicator')

ggplot(plotData) + 
	geom_hline(yintercept = 0, colour = 'grey75') +
	geom_rect(data = recessions.trim, aes(xmin = Peak, xmax = Trough, 
		 ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +	
	geom_point(aes(x = date, y = indicator), size = 1, colour = 'red') +
	geom_smooth(aes(x = date, y = indicator), method = 'loess', span = 0.12,
		size = 1.0, colour = 'blue') +
	scale_x_date("", lim = c(startDate, endDate)) +
	labs(title = chart.title, y = y.title) +
	theme(legend.position = 'none',
		plot.title = element_text(size = 18, face = 'bold'))

# print the plot

ggsave(filename = "employment.pdf", width = 9.5, height = 7.5)
ggsave(filename = "employment.png", width = 9.5, height = 7.5)

# cleanup
rm(startDate, endDate, scaleFactor, y.title, chart.title, indicator,
		indicator.diff, model.ets, forecast.ets, model.arima, forecast.arima,
		recessions.df, recessions.trim, plotData, p)