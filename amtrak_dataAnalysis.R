library(forecast)
Amtrak_data <- read.csv("C:/Users/baner/Downloads/Amtrak_data.csv")

#create time series
ridership.ts <- ts(Amtrak_data$Ridership, start= c(1991,1), end= c(2004,3), freq=12) #load amtrak ridership data

# #plot time series
# plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300,2300), bty = "l") #plot time series
# 
# #zoom in to January 1997 to December 2000
# ridership.ts.zoom <- window(ridership.ts,start=c(1997,1), end=c(2000,12))

#plot zoomed time series
plot(ridership.ts.zoom, xlab = "Time", ylab = "Ridership", ylim = c(1300,2300), bty = "l") #plot zoomed in time series

#Create quadratic regression model
ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2)) #create a quadratic trend model
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300,2300), bty = "l") #plot time series
lines(ridership.lm$fitted.values,col="blue") #plot fitted values


ridership.lm$fitted.values

