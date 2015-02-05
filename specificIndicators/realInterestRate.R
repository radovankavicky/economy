# Real Interest Rate Calculation
# Author: Ray Nelson
# May 14, 2012
###############################################################################
# Load Libraries
library(fImport)

# graph parameters
fromDate <- as.Date('1970-01-01')
endDate <- as.Date('2014-01-01')
smoothConstant <- 0.12
y.title <- "Annual Percentage\n"
chart.title <- 'Ex Post Real 30-Year Mortgage Interest Rate\n'

# time series and dates
priceIndex <- fredSeries('PCEPILFE', from = fromDate)
inflation <- returns(priceIndex, method = "continuous", percentage = TRUE) * 12
interestRate <- fredSeries('MORTG', from = fromDate)
plotData <- na.omit(cbind(interestRate, inflation))
names(plotData) <- c('Nominal', 'Inflation')
Real <- plotData$Nominal - plotData$Inflation
plotData <- cbind(plotData, Real = Real)

plotData <- data.frame(date = as.Date(time(plotData)), plotData$Real)
colnames(plotData) <- c('date', 'indicator')
plotData <- subset(plotData, date >= fromDate & !is.na(indicator))

# load and subset recessions data  for graphs
load('recessions.RData')
recessions.trim <- subset(recessions, Start >= fromDate)

# Graph
ggplot(plotData) +
	geom_rect(data = recessions.trim, aes(xmin = Start, xmax = End, 
		ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
	geom_hline(yintercept = 0, colour = 'grey65') +
	geom_point(aes(x= date, y = indicator), size = 1.25, color = "red") +
	geom_smooth(aes(x= date, y = indicator), span = smoothConstant,
		size = 1.0, color = "darkblue", fill = "springgreen4") +
	scale_x_date("", lim = c(fromDate, endDate)) +
	labs(title = chart.title, y = y.title)

# save graph to file
ggsave(filename = 'realInterestRate.pdf', width = 9, height = 7)
ggsave(filename = 'realInterestRate.png',
	width = 8, height = 5, dpi = 100) # png format for powerpoint

# Cleanup
rm(fromDate, endDate, smoothConstant, y.title, chart.title, priceIndex,
	inflation, interestRate, Real, plotData, recessions, recessions.trim)