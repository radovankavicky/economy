# Simple Smooth of core PCE chain index from Fred
# Author: Ray Nelson
###############################################################################
library(quantmod)
library(timeSeries)

# time series and dates
series <- "PCEPILFE"
startDate <- as.Date('1960-01-01')
endDate <- as.Date('2012-01-01')
smoothConstant <- 0.10
y.title <- "Percentage\n"
chart.title <- "PCE Core Inflation Rate (PCEPILFE)\n"

# Download PCE from FRED and compute an annualized rate of change
plotData <- getSymbols(series, src='FRED', auto.assign = FALSE,
		return.class = 'timeSeries')
plotData <- (plotData/Lag(plotData) - 1) * 100 * 12
plotData.df <- data.frame(date = as.Date(time(plotData)), coredata(plotData))
colnames(plotData.df) <- c('date', 'indicator')
plotData.df <- subset(plotData.df, date >= startDate & !is.na(indicator))

# load and subset recessions data  for graphs
load('recessions.RData')
recessions.trim <- subset(recessions.df, Peak >= startDate)

# Plot the inflation rate and use a LOWESS smoother to isolate the pattern
g <- ggplot(plotData.df)
g + geom_rect(data = recessions.trim, aes(xmin = Peak, xmax = Trough, 
		ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
	geom_hline(yintercept = 2, colour = "grey65", alpha = 0.5) +
	geom_point(aes(x= date, y = indicator), size = 1.0, color = "red") +
	geom_smooth(aes(x= date, y = indicator), span = smoothConstant,
		size = 0.50, color = "darkblue", fill = "springgreen4") +
	scale_x_date("", lim = c(startDate, endDate)) +
	scale_y_continuous(y.title) +
	opts(title = chart.title, plot.title = theme_text(size = 20),
		legend.position = "none")

ggsave("PCEInflation.pdf", width = 9, height = 7)
ggsave("PCEInflation.png", width = 7, height = 5, dpi = 100)

# cleanup
rm(series, startDate, endDate, smoothConstant, y.title, chart.title,
	plotData, plotData.df, g, recessions.df, recessions.trim)