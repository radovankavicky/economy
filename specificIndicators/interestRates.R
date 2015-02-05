# Multiple Interest Rates
# April 1, 2013
# Author: Ray Nelson
###############################################################################
# load libraries
library(fImport)

treasury <- fredSeries('GS20', from = '10/01/1993')
muni <- fredSeries('WSLB20', from = '10/01/1993')
by <- unique(timeLastNdayInMonth(time(muni)))
muni <- aggregate(muni, by, mean)

plot(muni)
