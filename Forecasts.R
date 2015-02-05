library(fImport)
library(forecast)
library(ggplot2)
retail <- fredSeries("RSAFSNA", from = "1992-01-01")
tsdisplay(retail)

plotData <- data.frame(as.Date(time(retail)), series(retail))
colnames(plotData) <- c("time", "retail")
ggplot(data = plotData, aes(x = time, y = retail)) +
  geom_smooth() +
  geom_line()

stl.model <- stl(retail, s.window = "periodic")
plot(stl.model)
(stl.forecast <- forecast(stl.model, h = 12))
plot(stl.forecast)

decompose.model <- decompose(as.ts(retail), type = "additive")
plot(decompose.model)

(retail.ets <- ets(retail, model = "ZZZ"))
(retail.ets.forecast <- forecast(retail.ets, h = 12))
plot(retail.ets.forecast)

gdp<- read.table("clipboard", sep = "\t", header = TRUE)
gdp.ts <- timeSeries(gdp$value, charvec = as.Date(gdp$date, "%m/%d/%Y"))
tsdisplay(gdp.ts)

(gdp.ets <- ets(gdp.ts))
(gdp.forecast <- forecast(gdp.ets, h = 4))
plot(gdp.forecast)

gdp.stl <- stl(gdp.ts, s.window = "periodic")
plot(gdp.stl)
(gdp.forecast <- forecast(gdp.stl))
plot(gdp.forecast)

(gdp.model <- auto.arima(gdp.ts))
(gdp.forecast <- forecast(gdp.model, h = 4))
plot(gdp.forecast)
