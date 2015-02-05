# HousingStarts forecasting using the forecast package
# Author: Ray Nelson
# May 15, 2014
###############################################################################
# Series IDs for HousingStarts data from FRED
# Housing Starts (HOUST)

# get HousingStarts data from FRED
HousingStarts <- fredSeries("HOUST", from = "1960-01-01")

# forecast using exponential smoothing
(HousingStarts.ets <- ets(HousingStarts, model = 'ZZN'))
(HousingStarts.forecast <- forecast(HousingStarts.ets, h = 24))
plot(HousingStarts.forecast,
	main = "Two Year Forecast for Total Nonfarm HousingStarts")

# forecast of number of jobs gained or lost
(HousingStarts.ets <- ets(HousingStarts.diff, model = 'ZZN'))
(HousingStarts.forecast <- forecast(HousingStarts.ets, h = 12))
plot(HousingStarts.forecast,
	main = "Two Year Forecast for Jobs Growth")

# presentation plot
PlotData <- data.frame(date = as.Date(time(HousingStarts)),
	coredata(HousingStarts))
ggplot(data = PlotData, aes(x = date, y = HOUST)) +
	geom_point(colour = 'red', size = 1.5) +
	geom_smooth(method = 'loess', span = 0.14, size = 1.0, colour = 'blue') +
	labs(x = '', y = "Thousands of Units") +
	opts(title = 'Private Single Unit Housing Starts\n')

# cleanup
rm(HousingStarts,  HousingStarts.ets, HousingStarts.forecast, PlotData)