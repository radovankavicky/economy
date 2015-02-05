# Read in and compute coincident indicator data from Philadelphia Fed
# June 1, 2012
# Author: Ray Nelson
###############################################################################
library(gdata)
library(timeSeries)
# Remove the missing observations from the xls file
# Format the date as UK %Y-%m-%d
coincident <- read.xls('coincident.xls', sheet = 1, header = TRUE)
coincident$Date <- as.Date(coincident$Date)
coincident <- timeSeries(coincident[ , -1], coincident$Date)

# Calculate mom rate of increase
mom <- removeNA(log(coincident/lag(coincident)) * 100 * 12)
yoy <- removeNA((coincident/lag(coincident, 12)-1) * 100)
