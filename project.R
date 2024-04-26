data=read.table("C:\\Users\\maina\\Desktop\\Project Work\\Project Demonstration 2019\\original.data.csv", header = TRUE, sep = ",")
attach(data)
library("TTR")
library("timeSeries")
data
View(data)
time.series.1=ts(data$Close,frequency = 12,start=c(1999,1))
time.series.2=ts(data$Open,frequency = 12,start=c(1999,1))
time.series.3=ts(data$High,frequency = 12,start=c(1999,1))
time.series.4=ts(data$Low,frequency = 12,start=c(1999,1))

##.....Data_Decomposition......##
t.decom.1=decompose(time.series.1)
t.decom.2=decompose(time.series.2)
t.decom.3=decompose(time.series.3)
t.decom.4=decompose(time.series.4)
summary(t.decom.1)

##.....Trend_Component_for_close.....##
plot(t.decom.1$trend,yaxt='n',ylab="Trend",main="The Trend component of the Time series(close)")
##.....Seasonal_Component_for_close.....##
plot(t.decom.1$seasonal,yaxt='n',ylab="seasonal",main="The seasonal component of the Time series(close)")
##.....Random_Component_for_close.....##
plot(t.decom.1$random,yaxt='n',ylab="random",main="The random component of the Time series(close)")

##.....Trend_Component_for_open.....##
plot(t.decom.2$trend,yaxt='n',ylab="Trend",main="The Trend component of the Time series(open)")
##.....Seasonal_Component_for_open.....##
plot(t.decom.2$seasonal,yaxt='n',ylab="seasonal",main="The seasonal component of the Time series(open)")
##.....Random_Component_for_open.....##
plot(t.decom.2$random,yaxt='n',ylab="random",main="The random component of the Time series(open)")

##.....Trend_Component_for_high.....##
plot(t.decom.3$trend,yaxt='n',ylab="Trend",main="The Trend component of the Time series(high)")
##.....Seasonal_Component_for_high.....##
plot(t.decom.3$seasonal,yaxt='n',ylab="seasonal",main="The seasonal component of the Time series(high)")
##.....Random_Component_for_high.....##
plot(t.decom.3$random,yaxt='n',ylab="random",main="The random component of the Time series(high)")

##.....Trend_Component_for_low.....##
plot(t.decom.4$trend,yaxt='n',ylab="Trend",main="The Trend component of the Time series(low)")
##.....Seasonal_Component_for_low.....##
plot(t.decom.4$seasonal,yaxt='n',ylab="seasonal",main="The seasonal component of the Time series(low)")
##.....Random_Component_for_low.....##
plot(t.decom.4$random,yaxt='n',ylab="random",main="The random component of the Time series(low)")


plot.ts(data$Close)

##.....Detrending.....##
adjusted.trend.1=time.series.1 - t.decom.1$trend
plot(adjusted.trend.1,yaxt='n',main="Detrending of the Time series (close)")
adjusted.trend.2=time.series.2 - t.decom.1$trend
plot(adjusted.trend.2,yaxt='n',main="Detrending of the Time series (open)")
adjusted.trend.3=time.series.3 - t.decom.1$trend
plot(adjusted.trend.3,yaxt='n',main="Detrending of the Time series (high)")
adjusted.trend.4=time.series.4 - t.decom.1$trend
plot(adjusted.trend.4,yaxt='n',main="Detrending of the Time series (low)")

##.....Deseasonalizing.....##
adjusted.seasonal.1=time.series.1 - t.decom.1$seasonal
plot(adjusted.seasonal.1,yaxt='n',main="Deseasonalized of the Time series (close)")
adjusted.seasonal.2=time.series.2 - t.decom.1$seasonal
plot(adjusted.seasonal.2,yaxt='n',main="Deseasonalized of the Time series (open)")
adjusted.seasonal.3=time.series.3 - t.decom.1$seasonal
plot(adjusted.seasonal.3,yaxt='n',main="Deseasonalized of the Time series (high)")
adjusted.seasonal.4=time.series.4 - t.decom.1$seasonal
plot(adjusted.seasonal.4,yaxt='n',main="Deseasonalized of the Time series (low)")

##.....Deseasonalizing_&_Detrending.....##
adjusted.seasonal.trend.1=time.series.1 - t.decom.1$seasonal-t.decom.1$trend
plot(adjusted.seasonal.trend.1,yaxt='n',main="Deseasonalized and detrended of the Time series (close)")

adjusted.seasonal.trend.2=time.series.2 - t.decom.2$seasonal-t.decom.2$trend
plot(adjusted.seasonal.trend.2,yaxt='n',main="Deseasonalized and detrended of the Time series (open)")

adjusted.seasonal.trend.3=time.series.3 - t.decom.3$seasonal-t.decom.3$trend
plot(adjusted.seasonal.trend.3,yaxt='n',main="Deseasonalized and detrended of the Time series (high)")

adjusted.seasonal.trend.4=time.series.4 - t.decom.4$seasonal-t.decom.4$trend
plot(adjusted.seasonal.trend.4,yaxt='n',main="Deseasonalized and detrended of the Time series (low)")

##.....exponential_smoothing.....##
timeseriesforecasts.1=HoltWinters(time.series.1, beta=FALSE, gamma=FALSE)
timeseriesforecasts.1

timeseriesforecasts.2=HoltWinters(time.series.2, beta=FALSE, gamma=FALSE)
timeseriesforecasts.2

timeseriesforecasts.3=HoltWinters(time.series.3, beta=FALSE, gamma=FALSE)
timeseriesforecasts.3

timeseriesforecasts.4=HoltWinters(time.series.4, beta=FALSE, gamma=FALSE)
timeseriesforecasts.4

##.....holt_winter.....##
library("forecast")

timeseries.holt.forecasts.1=forecast(time.series.1,h=8)
timeseries.holt.forecasts.1

timeseries.holt.forecasts.2=forecast(time.series.2,h=8)
timeseries.holt.forecasts.2

timeseries.holt.forecasts.3=forecast(time.series.3,h=8)
timeseries.holt.forecasts.3

timeseries.holt.forecasts.4=forecast(time.series.4,h=8)
timeseries.holt.forecasts.4


timeseries.holt.forecasts.5=HoltWinters(time.series.1,gamma=FALSE)
timeseries.holt.forecasts.5


