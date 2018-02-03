
ridership.ts <- ts(Amtrak_data$Ridership, start= c(1991,1), end= c(2004,3), freq=12) #load amtrak ridership data

plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300,2300), bty = "l") #plot timke series

 nValid <- 36 #number of validation periods
 nTrain <- length(ridership.ts) - nValid #no. of training periods
 
 train.ts <- window(ridership.ts, start = c(1991,1), end = c(1991,nTrain)) #create training time series
 validation.ts <- window(ridership.ts, start = c(1991,nTrain + 1), end = c(1991,nTrain + nValid)) #create validation time series
 
 #Create Holt winters additive model
 ridership.hwa <- HoltWinters(train.ts,seasonal="additive") #build a Holt Winters additive model
 ridership.hwa.pred <- forecast(ridership.hwa, h = nValid, level = c(0.05,0.95)) #forecast into the future for nValid periods using ridership.hwa moedl
 #The level setting in forecast specifies if you want confidence inervals
 
 #check rediduals, which are the forecast errors in the training period
 ridership.hwa.pred$residuals

 #print forecast errors in validation period
 validation.ts - ridership.hwa.pred$mean 
 
 #create histogram of residuals to check normality
 hist(na.trim(ridership.hwa.pred$residuals), ylab = "Frequency", xlab = "Forecast Error", bty = "l", main = "")

 #Compute quantiles of error
 #quantile(na.trim(ridership.hwa.pred$residuals), probs = c(0.05,0.95))
 
 #Plot prediction intervals computed by R
 plot(ridership.hwa.pred,ylim = c(1300,2600), ylab = "Ridership", xlab = "Time", bty = "l")
 
 
 