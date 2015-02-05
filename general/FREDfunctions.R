# FRED transformation functions
# October 11, 2012
# Author: Ray Nelson
###############################################################################
# Change
chg <- function(tsData) {
	na.omit(diff(tsData))
}

# Year over year change
ch1 <- function(tsData, n_obs_per_year) {
	na.omit(diff(tsData, lag = n_obs_per_year))
}

# Percentage change
pch <- function(tsData) {
	na.omit((tsData/lag(tsData, 1)-1) * 100)
}

# Year over year percentage change
pc1 <- function(tsData, n_obs_per_year) {
	na.omit((tsData/lag(tsData, n_obs_per_year)-1) * 100)
}

# Compounded annual change
pca <- function(tsData, n_obs_per_year) {
	na.omit(((tsData/lag(tsData, 1))^n_obs_per_year-1) * 100)
}

# Continously compounded percentage change
cch <- function(tsData) {
	na.omit((log(tsData) - log(lag(tsData, 1))) * 100)
}

# Continuously compounded annual change
cca <- function(tsData, n_obs_per_year) {
	na.omit((log(tsData) - log(lag(tsData, 1))) * n_obs_per_year * 100)
}

FRED <- function(symbol, fromDate, scaleFactor, transformation) {
	require(fImport)
	indicator <- fredSeries(symbol, from = fromDate) / scaleFactor
	if (transformation == 'level'){
		print('You are calculating the level.')
	}
	else if(transformation == 'change'){
		indicator <- diff(indicator)
		print('You are calculating the change in the level.')
	}
	else if (transformation == 'rate') {
		indicator <- returns(indicator, method = 'discrete') * 100
		print('You are calculating the percentage change.')
	}
	else {
		stop('Make sure that you choose level, change, or rate!')
	}
	indicator <- na.omit(indicator)
}