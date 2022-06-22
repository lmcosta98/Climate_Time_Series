library(astsa)

train = read.csv('data/DailyDelhiClimateTrain.csv')
test = read.csv('data/DailyDelhiClimateTest.csv')

# Converting dates from characters to datetime objects
train$date <- as.Date.character(train$date, '%Y-%m-%d')
test$date <- as.Date.character(test$date, '%Y-%m-%d')

plot(train) # we can see that the meantemp, humidity and wind_speed 
            #can be modeled as time series 

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


# Calculating the first differences
fdiff_meantemp = diff(meantemp_series)
var(fdiff_meantemp)
mean(fdiff_meantemp)
plot.ts(cbind(meantemp_series,fdiff_meantemp), main="")


fdiff_humidity = diff(humidity_series)
var(fdiff_humidity)
mean(fdiff_humidity)
plot.ts(cbind(humidity_series,fdiff_humidity), main="")


fdiff_wind = diff(wind_series)
var(fdiff_wind)
mean(fdiff_wind)
plot.ts(cbind(wind_series,fdiff_wind), main="")


# calculating the acfs
acf2(fdiff_meantemp)
acf2(fdiff_humidity)
acf2(fdiff_wind)



