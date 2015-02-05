# Analyze US Unemployment, Nonag Employment, and Labor Market Participation
# August 2, 2013
# Author: Ray
###############################################################################
library(fImport)

# Series IDs for employment data from FRED
# US Total NonFarm Employment (PAYEMS)
# US Unemployment Rate (UNRATE)
# US Labor Market Participation (LRAC74TTUSM156S)

# time series and dates
startDate <- as.Date('1959-01-01')
endDate <- as.Date('2014-01-01')

# Get data from FRED
jobs <- fredSeries("PAYEMS", from = startDate) / 1000
newJobs <- diff(jobs)
unemploy <- fredSeries("UNRATE", from = startDate)
participation <- fredSeries("LRAC74TTUSM156S", from = startDate)
fredData <- cbind(jobs, newJobs, unemploy, participation)
colnames(fredData) <- c("jobs", "newJobs", "unemploy", "participation")

# Data for Recession Bars
load("recessions.RData")

# Plot of NonAg Employment
recessions.trim <- subset(recessions, Start >= startDate)
plotData <- data.frame(date = as.Date(time(fredData)),
		fredData$jobs)
names(plotData) <-  c('date', 'indicator')
ggplot(plotData) + 
	geom_hline(yintercept = max(plotData$indicator), colour = 'red') +
	geom_rect(data = recessions.trim, aes(xmin = Start, xmax = End, 
		ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +	
	geom_line(aes(x = date, y = indicator), size = 1, colour = 'blue') +
	scale_x_date("", lim = c(startDate, endDate)) +
	labs(title = "US NonAgricultural Employment\n", y = "Millions of Jobs") +
	theme(legend.position = 'none',
		plot.title = element_text(size = 18, face = 'bold'))

# Plot of New Jobs
recessions.trim <- subset(recessions, Start >= startDate)
plotData <- data.frame(date = as.Date(time(fredData)),
		fredData$newJobs * 1000)
names(plotData) <-  c('date', 'indicator')
plotData <- na.omit(plotData)
ggplot(plotData) + 
	geom_hline(yintercept = 0, colour = 'grey75') +
	geom_rect(data = recessions.trim, aes(xmin = Start, xmax = End, 
		ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +	
	geom_point(aes(x = date, y = indicator), size = 1, colour = 'red') +
	geom_smooth(aes(x = date, y = indicator), method = 'loess', span = 0.15,
		size = 1.0, colour = 'blue') +
	scale_x_date("", lim = c(startDate, endDate)) +
	labs(title = "New US NonAgricultural Jobs\n", y = "Thousands of Jobs") +
	theme(legend.position = 'none',
		plot.title = element_text(size = 18, face = 'bold'))

# Plot of Unemployment Rate
recessions.trim <- subset(recessions, Start >= startDate)
plotData <- data.frame(date = as.Date(time(fredData)),
		fredData$unemploy)
names(plotData) <-  c('date', 'indicator')
ggplot(plotData) + 
	geom_rect(data = recessions.trim, aes(xmin = Start, xmax = End, 
		ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +	
	geom_point(aes(x = date, y = indicator), size = 1, colour = 'red') +
	geom_smooth(aes(x = date, y = indicator), method = 'loess', span = 0.12,
		size = 1.0, colour = 'blue') +
	scale_x_date("", lim = c(startDate, endDate)) +
	labs(title = "US Unemployment Rate\n", y = "Percentage") +
	theme(legend.position = 'none',
		plot.title = element_text(size = 18, face = 'bold'))

# Plot of Participation Rate
startDate <- as.Date('1980-01-01')
recessions.trim <- subset(recessions, Start >= startDate)
plotData <- data.frame(date = as.Date(time(fredData)),
		fredData$participation)
names(plotData) <-  c('date', 'indicator')
ggplot(plotData) + 
	geom_rect(data = recessions.trim, aes(xmin = Start, xmax = End, 
		ymin = -Inf, ymax = +Inf), fill = 'grey65', alpha = 0.4) +	
	geom_point(aes(x = date, y = indicator), size = 1, colour = 'red') +
	geom_smooth(aes(x = date, y = indicator), method = 'loess', span = 0.12,
		size = 1.0, colour = 'blue') +
	scale_x_date("", lim = c(startDate, endDate)) +
	labs(title = "US Labor Market Participation Rate\n", y = "Percentage") +
	theme(legend.position = 'none',
		plot.title = element_text(size = 18, face = 'bold'))

# Cleanup
rm(startDate, endDate, jobs, newJobs, unemploy, participation, fredData,
	plotData, recessions.df, recessions.trim)

