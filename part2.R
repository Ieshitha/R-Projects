library(readxl)  # read xlsx files

library(forecast)

library(ggfortify)

library(tseries)

library(nnfor)

ExchangeUSD <- read_excel("D:/Level 5/Semester 2/Machine Learning/CW/ExchangeUSD.xlsx")

View(ExchangeUSD)

names(ExchangeUSD)[names(ExchangeUSD) == "YYYY/MM/DD"] <- "Date"    #rename this column name YYY/MM/DD into Date

ExchangeUSD$Date <- as.Date(ExchangeUSD$Date)                       #Format the Date column

names(ExchangeUSD)[names(ExchangeUSD) == "USD/EUR"] <- "USD"       #rename column name USD/EUR into USD

is.ts(ExchangeUSD)

print(ExchangeUSD)


trainDataSet <- ExchangeUSD[1:400,][3]     # 1 to 400 training data sets only with the 3 rd column

View(trainDataSet)

testDataSet <- ExchangeUSD[401:500,][3]   #remaining 100 data for testing data sets only with the 3 rd column 

View(testDataSet)

#exchangeTimeSeries <- ts(trainDataSet, start=c(2011,10,13), end=c(2013,05,16), frequency=12)
exchangeTimeSeries <- ts(trainDataSet, start=c(as.Date("2011-10-13", "%Y-%m-%d")), end=c(ExchangeUSD$Date("2013-05-16", "%Y-%m-%d")), frequency=12)

print(exchangeTimeSeries)

is.ts(exchangeTimeSeries)

summary(exchangeTimeSeries)

plot.ts(exchangeTimeSeries)

exchangeTimeSeriesTest <- ts(testDataSet, start=c(as.Date("2013-05-17", "%Y-%m-%d")), end=c(as.Date("2013-10-09", "%Y-%m-%d")), frequency=12)

print(exchangeTimeSeriesTest)

is.ts(exchangeTimeSeriesTest)

exchangeTimeSeries

summary(exchangeTimeSeriesTest)

plot.ts(exchangeTimeSeriesTest)

exchange_train <- window(exchangeTimeSeries, start=c(as.Date("2011-10-13", "%Y-%m-%d")), end=c(as.Date("2013-05-16", "%Y-%m-%d")))

exchange_test <- window(exchangeTimeSeriesTest, start=c(as.Date("2013-05-17", "%Y-%m-%d")), end=c(as.Date("2013-10-09", "%Y-%m-%d")))


autoplot(exchange_train) +
  
  ylab("Exchange Rate") +
  
  ggtitle("Training dataset") +
  
  theme_minimal()


autoplot(exchange_test) +
  
  ylab("Exchange Rate") +
  
  ggtitle("Testing dataset") +
  
  theme_minimal()




fit <- mlp(exchange_train)   #fit the time series into the mlp model 

print(fit)

plot(fit)

fit2 <- mlp(exchange_train, hd = c(10,5))

plot(fit2)



frc <- forecast(fit2,h=36)

plot(frc)

library(forecast)


forecast(frc, 3)

plot(forecast(frc, 3))
accuracy(frc)














set.seed(12345)
library(neuralnet)
nn <- neuralnet(exchange_train, data = trainDataSet)



myts2 <- window(exchangeTimeSeries, start=c(2011, 10), end=c(2013,10))
print(myts2)

air.fit.nnetar <- nnetar(myts)
air.fcst.nnetar <- forecast(air.fit.nnetar, h = 35)



normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

data(lynx)

exchange_norm <- as.data.frame(lapply(lynx, normalize))
colnames(exchange_norm) <- "exchange"
summary(exchange_norm)   

#Convert again to time series data
exchange_norm <- ts(exchange_norm,frequency=1,start=c(2011,10), end=c(2013,10))

# subset the time series
exchange_train <- window(exchange_norm, start=c(2011,10), end=c(2013,04))
exchange_test <- window(exchange_norm, start=c(2013,05), end=c(2013,10))

## Fit model to the exchange_train data
fit <- nnetar(exchange_train, decay=0.5, maxit=150)
#Predict on exchange_test data
plot(forecast(fit,h=34))
lines(exchange_test)

library(nnfor)
fit1 <- mlp(exchangeTimeSeries)
print(fit1)
plot(fit1)
fit2 <- mlp(exchangeTimeSeries, hd = c(10,5))
plot(fit2)



