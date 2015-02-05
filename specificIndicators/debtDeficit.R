# Get a time series indicator from FRED and graph it for the bulletin board
# February 6, 2014
# Author: Ray Nelson
###############################################################################
# time series and dates
startDate <- as.Date('1965-01-01')
endDate <- as.Date('2015-01-01')
scaleFactor <- 1000000
y.title <- "Trillions of Dollars\n"
chart.title <- "Government Debt and Deficit\n"
load("recessions.RData")

# Federal debt and deficit FRED
debt <- fredSeries("GFDEBTN", from = startDate) / scaleFactor
deficit <- -(debt - lag(debt,4))
debt <- window(debt, start = startDate, end = endDate)
deficit <- window(deficit, start = startDate, end = endDate)

# Convert wide data frame to long in preparation for plotting
plotData <- data.frame(as.Date(time(debt)), debt, deficit)
names(plotData) <- c("date", "Debt", "Deficit")
plotData <- melt(plotData, id.vars = "date")
names(plotData) <- c("date", "indicator", "value")

# Plot the data
ggplot(plotData) + 
	geom_rect(data = subset(recessions, Start >= startDate),
		aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf),
		fill = 'grey65', alpha = 0.4) +
	geom_hline(yintercept = 0, colour = "grey") +
	geom_line(aes(x = date, y = value, color = indicator), size = 1,) +
	facet_grid(facets = indicator ~ ., scale = "free_y") +
	scale_x_date("", lim = c(startDate, endDate)) +
	labs(title = 'US Government Debt and Deficit\n', y = y.title) +
	theme(legend.position = 'none',
		plot.title = element_text(size = 18, face = 'bold'))

# Save to file

ggsave(filename = "debtDeficit.pdf", width = 9.5, height = 7.5)
ggsave(filename = "debtDeficit.png", width = 7.0, height = 5.0, dpi = 100)

rm(startDate, endDate, scaleFactor, y.title, chart.title, debt, deficit,
		plotData)