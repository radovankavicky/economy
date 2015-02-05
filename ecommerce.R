# TODO: Add comment
# 
# Author: Ray
###############################################################################
library(fImport)
library(forecast)
ecommerce <- as.ts(fredSeries("ECOMPCTSA", from = as.Date("1999-10-1" )))
tsdisplay(ecommerce)
forecast(ets(ecommerce), h = 4)
diff(ecommerce)