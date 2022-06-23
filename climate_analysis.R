library(astsa)
library(forecast)

train = read.csv('data/DailyDelhiClimateTrain.csv')
test = read.csv('data/DailyDelhiClimateTest.csv')

# Converting dates from characters to datetime objects
train$date <- as.Date.character(train$date, '%Y-%m-%d')
test$date <- as.Date.character(test$date, '%Y-%m-%d')

plot(train) # we can see that the meantemp, humidity and wind_speed 
            #can be modeled as time series 
train
# calculating the means
mean(train$meantemp)
mean(train$humidity)
mean(train$wind_speed)

# calculating the variances
var(train$meantemp)
var(train$humidity)
var(train$wind_speed)

plot.ts(train, plot.type = 'multiple') # the time series plot confirms our hipothesis

meantemp_series <- ts(train$meantemp, start = c(2013, 1) , frequency = 365)
humidity_series <- ts(train$humidity, start = c(2013, 1) , frequency = 365)
wind_series <- ts(train$wind_speed, start = c(2013, 1) , frequency = 365)
#meantemp_series

# Decomposing the series in its components
#decomposition = stl(meantemp_series, s.window = 'per')
# meantemp
plot(stl(meantemp_series, s.window = 'per')) # it clearly has seasonality and also a trend
# humidity
plot(stl(humidity_series, s.window = 'per')) # it clearly has seasonality and also a trend
# wind speed
plot(stl(wind_series, s.window = 'per')) # it clearly has seasonality and also a trend

# calculating the acfs
acf2(meantemp_series) # clearly not stationary
acf2(humidity_series) 
acf2(wind_series) 

# to check if it is necessary to differentiate
ndiffs(meantemp_series) # 1 difference
ndiffs(humidity_series) # 0 differences
ndiffs(wind_series) # 0 differences

# to check for the number of seasonal differences to use
nsdiffs(meantemp_series) # 1 difference
nsdiffs(humidity_series) # 1 difference
nsdiffs(wind_series) # 0 differences


# Calculating the first difference
fdiff_meantemp = diff(meantemp_series)
var(fdiff_meantemp)
mean(fdiff_meantemp)
plot.ts(cbind(meantemp_series,fdiff_meantemp), main="")


# calculating the acf
acf2(fdiff_meantemp)


x <- auto.arima(meantemp_series, lambda=0)
x
BoxCox.lambda(meantemp_series)
Box.test(res,lag=10, type='Ljung-Box') 

mod <- sarima(meantemp_series, 5,1,0, 0,1,0, 365)
mod

res = residuals(mod$fit)
res
mean(res)
var(res)

shapiro.test(res) # pvalue < 2.2e-16
ks.test(res, 'pnorm', mean(res), sd(res))

forecst <- sarima.for(meantemp_series, 30, 5,1,0, 0,1,0, 365)
forecst

