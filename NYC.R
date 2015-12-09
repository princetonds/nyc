# NYC.csv gives the average daily temperatures in Central
# Park, New York over 15 years (starting on 11/04/2000)

##############
### SET UP ###
##############

# read in the data
### use the path to which you saved your data!
mydata <- read.csv("NYC.csv",sep = ",", quote = "\"", dec = ".")

# check to make sure it is read in correctly
head(mydata)

# obtain dimension of data
dim(mydata)
temperature <- t(mydata)
dim(temperature)
TIME = 1:5477 # has to match the number of entries in "temperature"

# make a simple scatterplot of data to roughly visualize temperature fluctuations
plot(TIME, temperature, main="Average Daily Temperatures in Central
     Park 11/04/2000 - 2015",xlab="days from 11/04/2000", 
     ylab="Temperature in F",ylim=c(0, 100))

##################
### REGRESSION ###
##################

# At this point, it is rather clear that the pattern is sinusoidal.
# Therefore, we should try to fit our data to a function that is a sum of sines and cosines.
xc<-cos(2*pi*TIME/366) # 366 because we assume the fluctuation is yearly
xs<-sin(2*pi*TIME/366)

# lm is used to fit linear models. It can be used to carry out regression, single stratum 
# analysis of variance and analysis of covariance 
fit.lm <- lm(as.vector(temperature) ~ xc + xs)
summary(fit.lm)

# fitted is a generic function which extracts fitted values from objects returned by modeling
# functions.
fit <- fitted(fit.lm) 
summary(fit)

# Graph the prediction
lines(fit, col="red", lwd = 2)

# Graph of the Q-Q plot for standard residuals vs. theoretical quantiles for the regression
plot(fit.lm, which = 2)


##########################################
## Predict a temperature in the future! ##
##########################################

# Example of the "forecast" package:
# Fitting a model, generating forecasts, then plotting
# 
# IMPORTANT: if you do not have forecast package downloaded, uncomment the line below
# install.packages('forecast', dependencies = TRUE)
require(forecast)
ts.mydata <- ts(mydata) # a time series object
fit.arima <- auto.arima(ts.mydata)
fcast.arima <- forecast(fit.arima, h=120)
plot(fcast.arima)

#####################################
## Additional questions to explore ##
## (or come up with your own!)     ##
#####################################

# What are the average temperatures for each season? month?
# What is the expected temperature of <insert future date>?
# What dates are "outliers"? (And how would you define an outlier?)

## Challenge ##
# How would you account for global warming in your prediction?
# Related: What other type(s) of regression/what other method would you use for your prediction?

