# Simple Smooth the US Unemployment Rate
# June 26, 2014
# Author: Ray Nelson
###############################################################################
library(fImport)

# time series and dates
FREDSeries <- 'UNRATE'
startDate <- as.Date('1980-01-01')
endDate <- as.Date('2015-01-01')
scaleFactor <- 1
smoothConstant <- 0.1
y.title <- 'Percentage\n'
chart.title <- paste('Unemployment Rate\n (',
	FREDSeries, ')\n', sep = '')

# Download data from FRED and create the data frame needed for the plot
plotData <- fredSeries(FREDSeries, from = startDate, to = endDate)
plotData.df <- data.frame(as.Date(time(plotData)), series(plotData)/scaleFactor)
colnames(plotData.df) <- c('date', 'indicator')

# load and subset recessions data  for graphs
load('recessions.RData')
recessions <- subset(recessions, Start >= startDate)

ggplot(plotData.df) +
	geom_rect(data = recessions, aes(xmin = Start, xmax = End, 
		ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
	# geom_hline(yintercept = 0, colour = 'grey65') +
	geom_point(aes(x = date, y = indicator), size = 1.25, color = "red") +
	geom_smooth(aes(x = date, y = indicator), span = smoothConstant,
		size = 0.75, color = "darkblue", fill = "springgreen4") +
	scale_x_date("", lim = c(startDate, endDate)) +
	scale_y_continuous(y.title) +
	labs(title = chart.title) +
	theme(plot.title = element_text(size = 20), legend.position = "none")

# save graph to file
ggsave(filename = paste(FREDSeries, '.pdf', sep = ''), width = 9, height = 7)
ggsave(filename = paste(FREDSeries, '.png', sep = ''),
	width = 8, height = 5, dpi = 100) # png format for powerpoint

# Cleanup
rm(FREDSeries, startDate, endDate, scaleFactor, smoothConstant, y.title,
	chart.title, plotData, plotData.df, recessions)