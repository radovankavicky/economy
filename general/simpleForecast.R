# Nonfarm employment forecasting using the forecast package
# Author: Ray Nelson
###############################################################################
# Required libraries
# install using install.packages("quantmod")
# install using install.packages("forecast")
# install using install.packages("timeSeries")

# function definition

forecastFRED <- function(series, horizon, scaleFactor) {
	# load libraries
	require(quantmod)
	require(forecast)
	require(timeSeries)
	# Retrieve data from FRED
	indicator <- getSymbols(series, src = 'FRED', auto.assign = FALSE) / scaleFactor
	# forecast next two years or 24 months
	indicator.ets = ets(as.timeSeries(indicator), model = "ZZZ")
	print(forecast(indicator.ets, h = horizon))
	plot(forecast(indicator.ets, h = horizon))
}

forecastFRED('PAYEMS', 24, 1000)