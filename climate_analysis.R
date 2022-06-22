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

temp_series <- train[c(0:2)]

temp_series <- as.ts(temp_series, start=c(temp_series$date[0]), end=c(temp_series$date[-1]))

stl(temp_series, s.window = 'per')

