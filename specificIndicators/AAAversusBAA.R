# BAAcipal Risk Premium
# April 3, 2013
# Author: Ray Nelson
###############################################################################
#Libraries
library(timeSeries)
library(fImport)

# time series and dates
AAA <- 'AAA'
BAA <- 'BAA'
startDate <- as.Date('1960-02-01')
endDate <- as.Date('2013-06-01')
smoothConstant <- 0.10
y.title <- "Percentage\n"
chart.title <- "AAA versus BAA Corporate Bonds\n"

# Construct time series
interestRates <- cbind(fredSeries(BAA, from = startDate),
		fredSeries(AAA, from = startDate))
premium <- timeSeries(interestRates[, 1] - interestRates[, 2],
		as.Date(time(interestRates)))
interestRates <- cbind(interestRates, premium)
names(interestRates) <- c('BAA', 'AAA', 'RiskPremium')
date <- as.Date(time(interestRates))
interestRates <- as.data.frame(interestRates)
interestRates <- data.frame(date, interestRates)

# Melt data for ggplot2
plotData <- melt(interestRates, id.vars = 'date',
	measure.vars = c('BAA', 'AAA', 'RiskPremium'),
	variable.name = 'interestRate')
colnames(plotData) <- c('date', 'interestRate', 'indicator')

# Data for Recession Bars
load("recessions.RData")
recessions.trim <- subset(recessions.df, Peak >= startDate)

p <- ggplot(plotData) +
		geom_rect(data = recessions.trim, aes(xmin = Peak, xmax = Trough, 
				ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
		geom_point(aes(x= date, y = indicator), size = 1.25, color = "red") +
		geom_smooth(aes(x= date, y = indicator), span = smoothConstant,
			size = 0.75, color = "darkblue", fill = "springgreen4") +
		scale_x_date("", lim = c(startDate, endDate)) +
		facet_grid(interestRate ~ .) +
		labs(title = chart.title, y = y.title) +
		theme(legend.position = 'none',
				plot.title = element_text(size = 18, face = 'bold'))
print(p)
ggsave(p, filename = "AAAversusBAA.pdf", width = 9, height = 7)

# Graph of risk premium
plotData <- interestRates[, c(1,4)]
names(plotData) <- c('date', 'indicator')
p <- ggplot(plotData) +
		geom_rect(data = recessions.trim, aes(xmin = Peak, xmax = Trough, 
						ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
		geom_point(aes(x= date, y = indicator), size = 1.25, color = "red") +
		geom_smooth(aes(x= date, y = indicator), span = smoothConstant,
				size = 0.75, color = "darkblue", fill = "springgreen4") +
		scale_x_date("", lim = c(startDate, endDate)) +
		labs(title = chart.title, y = y.title) +
		theme(legend.position = 'none',
				plot.title = element_text(size = 18, face = 'bold'))
print(p)
ggsave(p, filename = "AAAversusBAA2.pdf", width = 9, height = 7)

# Cleanup
rm(BAA, AAA, startDate, endDate, y.title, chart.title,
		interestRates, date, premium, plotData, recessions.df, recessions.trim,
		p)