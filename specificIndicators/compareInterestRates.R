# Yield Curve Slope
# April 1, 2013
# Author: Ray Nelson
###############################################################################
#Libraries
library(timeSeries)
library(fImport)

# time series and dates
first <- 'GS10'
second <- 'GS1'
startDate <- as.Date('1960-01-01')
endDate <- as.Date('2013-06-01')
smoothConstant <- 0.10
y.title <- "Percentage\n"
chart.title <- "Yield Curve Slope: Ten Year minus One Year\n"

# Combine time series
plotData <- cbind(fredSeries(first, from = startDate),
		fredSeries(second, from = startDate))
slope <- timeSeries(plotData$GS10 - plotData$GS1,
		as.Date(time(plotData)))
plotData <- cbind(plotData, slope)
names(plotData) <- c('ten', 'one', 'slope')

# Federal debt and deficit FRED
plotData <- data.frame(date = as.Date(time(plotData)), plotData$slope)
colnames(plotData) <- c('date', 'indicator')
plotData <- subset(plotData, date >= startDate)

# Data for Recession Bars
load("recessions.RData")
recessions.trim <- subset(recessions.df, Peak >= startDate)

p <- ggplot(plotData) +
		geom_rect(data = recessions.trim, aes(xmin = Peak, xmax = Trough, 
				ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
		geom_hline(yintercept = 0, colour = 'grey65') +
		geom_point(aes(x= date, y = indicator), size = 1.25, color = "red") +
		geom_smooth(aes(x= date, y = indicator), span = smoothConstant,
				size = 0.75, color = "darkblue", fill = "springgreen4") +
		scale_x_date("", lim = c(startDate, endDate)) +
		labs(title = chart.title, y = y.title) +
		theme(legend.position = 'none',
				plot.title = element_text(size = 18, face = 'bold'))
print(p)

# save graph to file
ggsave(p, filename = "Indicator.pdf", width = 9, height = 7)

# Cleanup
rm(series, startDate, endDate, scaleFactor, y.title, chart.title, plotData,
		recessions.df, recessions.trim, p)



