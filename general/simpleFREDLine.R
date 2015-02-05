# Simple Smooth of Data from FRED
# February 6, 2014
# Author: Ray Nelson
###############################################################################
# time series and dates
FREDSeries <- 'EXCSRESNS'
startDate <- as.Date('2000-01-01')
endDate <- as.Date('2015-01-01')
scaleFactor <- 1000
y.title <- "Trillions of Dollars\n"
chart.title <- paste('Excess Reserves of Depository Institutions  (',
		FREDSeries, ')\n', sep = '')
load("recessions.RData")

# Federal debt and deficit FRED
plotData <- fredSeries(FREDSeries, from = startDate) / scaleFactor
plotData <- data.frame(date = as.Date(time(plotData)), plotData)
colnames(plotData) <- c('date', 'indicator')
plotData <- subset(plotData, date >= startDate)

ggplot(plotData) +
	geom_rect(data = subset(recessions, Start >= startDate),
		aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf),
		fill = 'grey65', alpha = 0.4) +
	geom_line(aes(x= date, y = indicator), color = "darkblue", size = 1) +
	scale_x_date("", lim = c(startDate, endDate)) +
	labs(title = chart.title, y = y.title, x = "") +
	theme(legend.position = 'none',
		plot.title = element_text(size = 18)) +

# save graph to file
ggsave(filename = paste(FREDSeries, '.pdf', sep = ''), width = 9, height = 7)
ggsave(filename = paste(FREDSeries, '.png', sep = ''),
		width = 8, height = 5, dpi = 100) # png format for powerpoint

# Cleanup
rm(FREDSeries, startDate, endDate, scaleFactor, y.title, chart.title, plotData,
		recessions)