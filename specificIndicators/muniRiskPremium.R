# Muni Risk Premium
# April 3, 2013
# Author: Ray Nelson
###############################################################################
#Libraries
library(timeSeries)
library(fImport)

# time series and dates
Treasury <- 'GS20'
Muni <- 'MSLB20'
startDate <- as.Date('1960-02-01')
endDate <- as.Date('2013-06-01')
smoothConstant <- 0.10
y.title <- "Percentage\n"
chart.title <- "Municipal Bond Rate minus Treasury Rate\n"

# Construct time series
interestRates <- cbind(fredSeries(Muni, from = startDate),
		fredSeries(Treasury, from = startDate))
premium <- timeSeries(interestRates[, 1] - interestRates[, 2],
		as.Date(time(interestRates)))
interestRates <- cbind(interestRates, premium)
names(interestRates) <- c('Muni', 'Treasury', 'RiskPremium')
date <- as.Date(time(interestRates))
interestRates <- as.data.frame(interestRates)
interestRates <- data.frame(date, interestRates)

# Melt data for ggplot2
plotData <- melt(interestRates, id.vars = 'date',
	measure.vars = c('Muni', 'Treasury', 'RiskPremium'),
	variable.name = 'interestRate')
colnames(plotData) <- c('date', 'interestRate', 'indicator')

# Data for Recession Bars
load("recessions.RData")
recessions.trim <- subset(recessions.df, Peak >= startDate)

p <- ggplot(plotData) +
		geom_rect(data = recessions.trim, aes(xmin = Peak, xmax = Trough, 
				ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
		geom_line(aes(x= date, y = indicator, colour = interestRate),
		size = 1.00) +
		scale_x_date("", lim = c(startDate, endDate)) +
		facet_grid(interestRate ~ .) +
		labs(title = chart.title, y = y.title) +
		theme(legend.position = 'none',
				plot.title = element_text(size = 18, face = 'bold'))
print(p)
ggsave(p, filename = "muni.pdf", width = 9, height = 7)

# Graph of risk premium
plotData <- interestRates[, c(1,4)]
names(plotData) <- c('date', 'indicator')
p <- ggplot(plotData) +
		geom_rect(data = recessions.trim, aes(xmin = Peak, xmax = Trough, 
				ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
		geom_hline(yintercept = 0, colour = 'grey65') +
		geom_line(aes(x= date, y = indicator), size = 1.00, colour = 'blue') +
		scale_x_date("", lim = c(startDate, endDate)) +
		labs(title = chart.title, y = y.title) +
		theme(legend.position = 'none',
				plot.title = element_text(size = 18, face = 'bold'))
print(p)
ggsave(p, filename = "muni2.pdf", width = 9, height = 7)

# Cleanup
rm(Muni, Treasury, startDate, endDate, smoothConstant, y.title, chart.title,
		interestRates, date, premium, plotData, recessions.df, recessions.trim,
		p)



