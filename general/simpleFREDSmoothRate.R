# Simple Smooth of Rate of Change Data from Fred
# Author: Ray Nelson
###############################################################################
# Load Libraries
library(ggplot2)
library(fImport)

# time series and dates
FREDSeries <- 'PCEPILFE'
startDate <- as.Date('1960-01-01')
endDate <- as.Date('2013-01-01')
scaleFactor <- 1
dataFrequency <- 12
smoothConstant <- 0.08
y.title <- "Annual Percentage\n"
chart.title <- paste('PCE Core Inflation Rate  (',
		FREDSeries, ')\n', sep = '')

# Download data
plotData <- fredSeries(FREDSeries, from = startDate)
plotData <- returns(plotData,  na.rm = TRUE, percentage = TRUE,
	method = "continuous") * dataFrequency
plotData.df <- data.frame(date = as.Date(time(plotData)),
	series(plotData)/scaleFactor)
colnames(plotData.df) <- c('date', 'indicator')

# load and subset recessions data  for graphs
load('recessions.RData')
recessions.trim <- subset(recessions.df, Peak >= startDate)

# Graph
ggplot(plotData.df) +
	geom_rect(data = recessions.trim, aes(xmin = Peak, xmax = Trough, 
					ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
	geom_hline(yintercept = 2, colour = 'grey65') +
	geom_point(aes(x= date, y = indicator), size = 1.25, color = "red") +
	geom_smooth(aes(x= date, y = indicator), span = smoothConstant,
			size = 0.90, color = "darkblue", fill = "springgreen4") +
	scale_x_date(lim = c(startDate, endDate)) +
	labs(title = chart.title, y = y.title, x = "") +
	theme(plot.title = element_text(size = 20), legend.position = "none")

# save graph to file
ggsave(filename = paste(FREDSeries, '.pdf', sep = ''), width = 9, height = 7)
ggsave(filename = paste(FREDSeries, '.png', sep = ''),
		width = 8, height = 5, dpi = 100) # png format for powerpoint

# Cleanup
rm(FREDSeries, startDate, endDate, scaleFactor, smoothConstant, y.title,
		chart.title, plotData, plotData.df, recessions.df, recessions.trim, 
		dataFrequency)