# Comparison of inflation rates
# October 11, 2012
# Author: Ray Nelson
###############################################################################
library(fImport)
library(ggplot2)

# Import FRED functions
source('FREDfunctions.R')

fromDate <- as.Date('1960-01-01')
span <- 0.15

ism <- fredSeries('NAPM', from = fromDate)
ind <- cca(fredSeries('INDPRO', from = fromDate), 12)
plotData <- cbind(ism, ind)
plotData <- data.frame(as.Date(time(plotData)), series(plotData))
names(plotData) <- c('date','ISM', 'IND')

# Data for business cycle
load(url('http://marriottschool.net/teacher/govfinance/recessions.RData'))
Recessions <- subset(recessions.df, Peak >=  fromDate)

# Time Series Comparison
ggplot(data = plotData) +
	geom_rect(data = Recessions, aes(xmin = Peak, xmax = Trough, 
		ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
	geom_hline(yintercept = 50.0, colour = 'grey65') +
	geom_point(aes(x = date, y = ISM), size = 1.0, color = "red") +
	geom_smooth(aes(x = date, y = ISM), size = 0.75,
		color = "darkblue", size = 1.0, fill = 'springgreen4', span = span) +
	scale_x_date("", lim = c(fromDate, as.Date('2013-01-01'))) +
	labs(title = 'US Manufacturing ISM Index\n', y ='Index')
ggsave("ismI.pdf", width = 9, height = 7)

ggplot(data = plotData) +
	geom_rect(data = Recessions, aes(xmin = Peak, xmax = Trough, 
		ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
	geom_hline(yintercept = 0.0, colour = 'grey65') +
	geom_point(aes(x = date, y = IND), size = 1.0, color = "red") +
	geom_smooth(aes(x = date, y = IND), size = 0.75,
		color = "darkblue", size = 1.0, fill = 'springgreen4', span = span) +
	scale_x_date("", lim = c(fromDate, as.Date('2013-01-01'))) +
	labs(title = 'US Industrial Production Index\n',
		y ='Month over Month Percentage Change')
ggsave("productionIndexI.pdf", width = 9, height = 7)