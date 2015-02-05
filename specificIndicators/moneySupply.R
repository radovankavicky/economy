# Get a time series indicator from FRED and graph it for the bulletin board
# Author: Ray Nelson
###############################################################################
# time series and dates
startDate <- as.Date('1965-01-01')
endDate <- as.Date('2015-01-01')
scaleFactor <- 1000
y.title <- "Trillions of Dollars\n"
chart.title <- "M2 Money Supply\n"
load("recessions.RData")

# Money supply and monetary base from FRED and calculate the money multiplier
m2 <- fredSeries("M2SL", from = startDate)/scaleFactor
base <- fredSeries("AMBSL", from = startDate)/scaleFactor
base <- window(base, start = start(m2), end = end(m2))
multiplier <- m2/base

# Calculate the money multiplier and constur
plotData <- data.frame(as.Date(time(m2)), m2, base, multiplier)
names(plotData) <- c("date", "M2", "MonetaryBase", "MoneyMultiplier")
plotData <- melt(plotData, id.vars = "date")
names(plotData) <- c("date", "indicator", "value")

# Plot the data
ggplot(plotData) + 
	geom_rect(data = subset(recessions, Start >= startDate),
		aes(xmin = Start, xmax = End, ymin = -Inf, ymax = +Inf),
		fill = 'grey65', alpha = 0.4) +	
	geom_line(aes(x = date, y = value, color = indicator), size = 1) +
	facet_grid(facets = indicator ~ ., scale = "free_y") +
	scale_y_continuous(y.title) +
	labs(title = chart.title, x = '', y = y.title) +
	scale_x_date("", lim = c(startDate, endDate)) +
	theme(legend.position = 'none',
		plot.title = element_text(size = 18, face = 'bold'))

ggsave(filename = "moneySupply.pdf", width = 9.5, height = 7.5)
ggsave(filename = "moneySupply.png", width = 7, height = 5, dpi = 100)

# cleanup
rm(startDate, endDate, scaleFactor, y.title, chart.title, m2, base, multiplier,
		plotData, recessions)

