# Labor Market forecast and analysis
# June 9, 2014
# Author: Ray Nelson
###############################################################################
# Plot and data parameters
FREDSeries <- 'PAYEMS'
startDate <- as.Date('1960-01-01')
endDate <- as.Date('2015-01-01')
scaleFactor <- 1000
horizon <- 24
nobs_per_year <- 12

# Download data from FRED and construct a data frame for ggplot2
indicator <- fredSeries(FREDSeries, from = startDate) / scaleFactor
PlotData <- data.frame(as.Date(time(indicator)), series(indicator))
names(PlotData) <- c('date', 'indicator')

# Construct a forecast using exponential smoothing
indicator.ets <- ets(indicator, model = 'ZZN')
(indicator.forecast <- forecast(indicator.ets, h = horizon))

# Data frame for ggplot
date <- as.Date(timeSequence(from = end(indicator), by = 'month',
	length.out = (horizon + 1)))
forecastData <- data.frame(date[2:length(date)],
	indicator.forecast$lower[, 2], indicator.forecast$lower[, 1],
	indicator.forecast$mean, indicator.forecast$upper)
names(forecastData) <- c('date', 'lower95', 'lower80', 'forecast',
		'upper80', 'upper95')

# Data for business cycle
load('recessions.RData')
Recessions <- subset(recessions, Start >= startDate)

# Data for presidential administrations
presidents <- as.numeric(timeCalendar(y = seq(1961, 2015, 4), m = 1, d = 1))

ggplot(PlotData) +
	geom_rect(data = Recessions, aes(xmin = Start, xmax = End, 
		ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
	geom_vline(xintercept = presidents, colour = 'gray25', size = 0.75,
		linetype = 3) +
	geom_ribbon(data = forecastData, fill = 'lightgreen',
		aes(x = date, ymin = lower95, ymax = upper95)) +
	geom_ribbon(data = forecastData, fill = 'yellow',
		aes(x = date, ymin = lower80, ymax = upper80)) +
	geom_line(aes(x= date, y = indicator), size = 1.0, color = "black") +
	geom_line(data = forecastData, aes(x = date, y = forecast), size = 1.0,
		colour = 'darkblue') +
	scale_x_date("", lim = c(startDate, endDate)) +
	labs(title = 'Nonfarm Employment\n', y = 'Millions of Workers') +
	theme(plot.title = element_text(size = 20))

# Print the graph in pdf file format
ggsave("PAYEMS.pdf", width = 9, height = 7)
ggsave("PAYEMS.png", width = 7, height = 5, dpi = 100)

# Analysis of jobs creation
indicator <- diff(fredSeries(FREDSeries, from = startDate))
losses <- indicator
losses[indicator > 0] <- 0
gains <- indicator
gains[indicator < 0] <- 0
PlotData <- data.frame(as.Date(time(indicator)), series(indicator),
	series(gains), series(losses))
names(PlotData) <- c('date', 'indicator', 'gains', 'losses')

# Construct a forecast using exponential smoothing
indicator.ets <- ets(indicator, model = 'ZZN')
(indicator.forecast <- forecast(indicator.ets, h = horizon))

# Data frame for ggplot
date <- as.Date(timeSequence(from = end(indicator), by = 'month',
	length.out = (horizon + 1)))
forecastData <- data.frame(date[2:length(date)],
	indicator.forecast$lower[, 2], indicator.forecast$lower[, 1],
	indicator.forecast$mean, indicator.forecast$upper)
names(forecastData) <- c('date', 'lower95', 'lower80', 'forecast',
		'upper80', 'upper95')

ggplot(PlotData) +
	geom_rect(data = Recessions, aes(xmin = Start, xmax = End, 
		ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
	geom_vline(xintercept = presidents, colour = 'gray25', size = 0.75,
		linetype = 3) +
	geom_ribbon(data = forecastData, fill = 'lightgreen',
		aes(x = date, ymin = lower95, ymax = upper95)) +
	geom_ribbon(data = forecastData, fill = 'yellow',
		aes(x = date, ymin = lower80, ymax = upper80)) +
	geom_ribbon(aes(x = date, ymax = 0, ymin = losses), fill = 'indianred',
		alpha = 0.6) +
	geom_ribbon(aes(x = date, ymax = gains, ymin = 0), fill = 'dodgerblue',
		alpha = 0.6) +
	geom_smooth(aes(x = date, y = indicator), se = FALSE, colour = 'black',
		span = 0.05, size = 1.0) +
	geom_line(data = forecastData, aes(x = date, y = forecast), size = 1.0,
		colour = 'red') +
	scale_x_date("", lim = c(startDate, endDate)) +
	labs(title = 'Nonfarm Job Creation\n', y = 'Thousands of Workers') +
	theme(plot.title = element_text(size = 20))

# Print the graph in pdf file format
ggsave("newJobs.pdf", width = 9, height = 7)
ggsave("newJobs.png", width = 7, height = 5, dpi = 100)

# Cleanup
rm(FREDSeries, startDate, endDate, scaleFactor, nobs_per_year, horizon, 
	indicator, PlotData, indicator.ets, indicator.forecast, date,
	forecastData, Recessions, presidents, losses, gains)
