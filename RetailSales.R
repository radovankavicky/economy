library("fImport")
library("forecast")

# Get data from FRED
retail <- fredSeries("RSAFSNA", from = "1992-01-01")
plot(retail)

# Classical decomposition forecast
retail.stl <- stl(retail, s.window = "periodic")
plot(retail.stl)
(retail.forecast <- forecast(retail.stl, h = 12))
plot(retail.forecast)

# Exponential smoothing forecast
(retail.ets <- ets(retail))
(retail.forecast <- forecast(retail.ets, h = 12))
plot(retail.forecast)