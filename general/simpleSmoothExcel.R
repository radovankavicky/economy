# Simple Smooth of Data Initially in Excel
# Author: Ray Nelson
# March 11, 2013
###############################################################################
# time series and dates
startDate <- as.Date('1960-01-01')
endDate <- as.Date('2014-01-01')
scaleFactor <- 1
smoothConstant <- 0.08
y.title <- 'Millions of Employees\n'
chart.title <- 'NonFarm Employment\n'

ts.data <- read.table('clipboard', header = TRUE, sep = '\t')
charvec <- as.Date(ts.data$V1, '%m/%d/%Y')
indicator <- timeSeries(data = ts.data$V2 / scaleFactor, charvec = charvec)

# Create dataframe for ggplot
plotData <- data.frame(as.Date(time(indicator)), series(indicator))
colnames(plotData) <- c('date', 'indicator')
plotData <- subset(plotData, date >= startDate)

# load and subset recessions data  for graphs
load('recessions.RData')
recessions.trim <- subset(recessions.df, Peak >= startDate)

ggplot(plotData) +
	geom_rect(data = recessions.trim, aes(xmin = Peak, xmax = Trough, 
		ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
	geom_hline(yintercept = 0, colour = 'grey65') +
	geom_point(aes(x= date, y = indicator), size = 1.25, color = "red") +
	geom_smooth(aes(x= date, y = indicator), span = smoothConstant,
		size = 1, color = "darkblue", fill = "springgreen4") +
	scale_x_date("", lim = c(startDate, endDate)) +
	labs(title = chart.title, y = y.title) +
	theme(plot.title = element_text(size = 18, face = 'bold'))

# save graph to file
ggsave(filename ='realRate.pdf', width = 9, height = 7)
ggsave(filename = 'realRate.png', width = 8, height = 5, dpi = 100) # powerpoint
ggsave(filename = 'realRate.png', width = 7, height = 5, dpi = 100) # web

# Cleanup
rm(startDate, endDate, scaleFactor, smoothConstant, y.title,
	chart.title, plotData, recessions.df, recessions.trim)