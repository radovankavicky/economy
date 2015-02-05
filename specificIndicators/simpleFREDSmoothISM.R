# Simple Smooth of Data from Fred
# Author: Ray Nelson
###############################################################################
library(ggplot2)
library(quantmod)

# time series and dates
series <- "NAPM"
startDate <- as.Date('1980-01-01')
endDate <- as.Date('2013-01-01')
scaleFactor <- 1
smoothConstant <- 0.05
y.title <- "Index Value\n"
chart.title <- "ISM Manufacturing: PMI Composite Index (NAPM)\n"

plotData <- getSymbols(series, src='FRED', auto.assign = FALSE)
plotData.df <- data.frame(date = time(plotData), coredata(plotData)/scaleFactor)
colnames(plotData.df) <- c('date', 'indicator')
plotData.df <- subset(plotData.df, date >= startDate)

# load and subset recessions data  for graphs
govFinance <- "http://marriottschool.net/teacher/govfinance/economy/indicators/"
con <- url(paste(govFinance, "recessions.RData", sep = ""))
load(con)
close(con)
recessions.trim <- subset(recessions.df, Peak >= startDate)
rm(govFinance, con, recessions.df)

ggplot(plotData.df) +
	geom_hline(yintercept = 50, colour = "grey75", size = 0.65) +
	geom_rect(data = recessions.trim, aes(xmin = Peak, xmax = Trough, 
		ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +
	geom_point(aes(x= date, y = indicator), size = 1.25, color = "red") +
	geom_smooth(aes(x= date, y = indicator), span = smoothConstant,
		size = 0.50, color = "darkblue", fill = "springgreen4") +
	scale_x_date("", lim = c(startDate, endDate)) +
	scale_y_continuous(y.title) +
	opts(title = chart.title, plot.title = theme_text(size = 20),
		legend.position = "none")

# save graph to file
ggsave(filename = "Indicator.pdf", width = 9, height = 7)

# Cleanup
rm(series, startDate, endDate, scaleFactor, smoothConstant, y.title, chart.title)
rm(plotData, plotData.df, recessions.trim)