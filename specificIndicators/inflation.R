# Comparison of inflation rates
# June 9, 2014
# Author: Ray Nelson
###############################################################################
library("fImport")
library("forecast")
library("ggplot2")
library("reshape2")

# Year over year growth rate function
yoy <- function(tsData) {
	na.omit((tsData/lag(tsData, 12) - 1) * 100)
}

startDate <- as.Date('1960-01-01')
endDate <- as.Date('2016-01-01')

cpi <- yoy(fredSeries('CPILFESL', from = startDate))
pce <- yoy(fredSeries('PCEPILFE', from = startDate))
plotData <- cbind(cpi, pce)
plotData <- data.frame(as.Date(time(plotData)), series(plotData))
names(plotData) <- c('date','CPI', 'PCE')

# Scatterplot
ggplot(data = plotData, aes(x = CPI, y = PCE)) +
	geom_point(colour = 'red', size = 1.0, shape = 20) +
	geom_smooth(size = 1.0) +
	labs(title = 'Inflation Comparison by Price Index\n',
		x = 'Consumer Price Index YOY Growth Rate',
		y = 'Personal Consumption Expenditure Index YOY Growth Rate')

# Covert the data frame plotData into long format
plotData <- melt(plotData, measure.vars = c('CPI', 'PCE'))
names(plotData) <- c('date', 'index', 'rate') 

# Data for Recession Bars
load("recessions.RData")
recessions.trim <- subset(recessions, Start >= startDate)

# Time Series Comparison
ggplot(data = plotData) +
	geom_rect(data = recessions.trim, aes(xmin = Start, xmax = End, 
		ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
	geom_hline(yintercept = 2.0, colour = 'grey65', linetype = 'dashed') +
	geom_point(aes(x = date, y = rate), size = 1.0, color = "red") +
	geom_smooth(aes(x = date, y = rate), size = 0.75,
		color = "darkblue", size = 1.0, fill = 'springgreen4', span = 0.05) +
	scale_x_date("", lim = c(startDate, as.Date('2013-01-01'))) +
	facet_grid(index ~ .) +
	labs(title = 'Core Inflation Rate\n', y ='Percentage') +
	theme(plot.title = element_text(size = 18, face = 'bold'))
ggsave("inflation.png", width = 7, height = 5, dpi = 100)
ggsave("inflation.pdf", width = 9, height = 7)

# Cleanup
rm(yoy, startDate, endDate, cpi, pce, plotData, recessions.trim, recessions)