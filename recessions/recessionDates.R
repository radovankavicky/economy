# Read business cycle dates from file
# Author: Ray Nelson
###############################################################################

library(gdata)
setwd("economy")
businessCycle <- read.xls("recessDates.xlsx", header = TRUE,
		colClasses = c('factor', 'Date', 'Date', 'Date'))

save.image("recessionsII.RData")