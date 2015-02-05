# Conference Board Indicators
# June 9, 2014
# Author: Ray
###############################################################################
# time series and dates
startDate <- as.Date('1960-01-01')
endDate <- as.Date('2015-01-01')
load("recessions.RData")

# Bring in data from Bloomberg spreadsheet and create data frame
conference <- read.table('clipboard', header = TRUE, sep = '\t')
conference <- timeSeries(conference[, -1],
		charvec = as.Date(conference$Date, '%m/%d/%Y'))
plotData <- data.frame(Date = as.Date(time(conference)), series(conference))
plotData <- melt(plotData, id.vars = "Date")
names(plotData) <- c("date", "index", "indicator")

# Plot the Data
ggplot(plotData) +
		geom_rect(data = subset(recessions, Start >= startDate),
				aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf),
				fill = 'grey65', alpha = 0.4) +
		geom_hline(yintercept = 25, colour = alpha("white", 1.0)) +
		geom_line(aes(x= date, y = indicator, colour = index),
				size = 1.0) +
		scale_x_date("", lim = c(startDate, endDate)) +
		labs(title = "Conference Board Indicators\n",
				x = "", y = "Index Values") +
		theme(legend.position = "none", plot.title = element_text(size = 18)) +
		scale_colour_discrete(name = "legend") +
		facet_grid(index ~ .)

ggsave("conferenceBoard.pdf", width = 9, height = 7)

