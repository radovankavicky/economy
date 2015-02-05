# Time plot with recession bars
# February 6, 2014
###############################################################################
load("recessions.RData")
startDate <- as.Date("1959-01-01")
fredTicker <- "PAYEMS"

indicator <- fredSeries(fredTicker, from = startDate)

plotData <- data.frame(as.Date(time(indicator)), series(indicator))
colnames(plotData) <- c("Time", "Indicator")

ggplot(data = plotData) +
	geom_rect(data = recessions[recessions$Start >= startDate,],
		aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf),
		fill = 'grey65', alpha = 0.4) +
#	geom_line(aes(x = Time, y = Indicator))
	geom_point(aes(x = Time, y = Indicator)) +
	geom_smooth(aes(x = Time, y = Indicator)) 


