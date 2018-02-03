library(forecast)
ridership.ts <- ts(Amtrak_data$Ridership, start= c(1991,1), end= c(2004,3), freq=12) #load amtrak ridership data

plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300,2300), bty = "l") #plot time series

 nValid <- 36 #number of validation periods
 nTrain <- length(ridership.ts) - nValid #no. of training periods
 
 train.ts <- window(ridership.ts, start = c(1991,1), end = c(1991,nTrain)) #create training time series
 validation.ts <- window(ridership.ts, start = c(1991,nTrain + 1), end = c(1991,nTrain + nValid)) #create validation time series
 
 #Create Holt Winters additive model
 ridership.hwa <- HoltWinters(train.ts,seasonal="additive") #build a Holt Winters additive model
 ridership.hwa.pred <- forecast(ridership.hwa, h = nValid, level = 0) #forecast into the future for nValid periods using ridership.hwa moedl
 #The level setting in forecast specifies if you want confidence inervals
 
 #Create Holt Winters multiplicative model
 ridership.hwm <- HoltWinters(train.ts,seasonal="multiplicative") #build a Holt Winters multiplicative model
 ridership.hwm.pred <- forecast(ridership.hwm, h = nValid, level = 0) #forecast into the future for nValid periods using ridership.hwm moedl
 
 #Create quadratic regression model
 ridership.lm <- tslm(train.ts ~ trend + I(trend^2)) #build a quadratic model to preduct ridership using 
 ridership.lm.pred <- forecast(ridership.lm, h = nValid, level = 0) #forecast into the future for nValid periods using ridership.lm moedl
 
 plot(ridership.lm.pred,ylim = c(1300,2600), ylab = "Ridership", xlab = "Time", bty = "l",
      xaxt = "n", xlim = c(1991,2006.25), main = "", flty =2)
 lines(ridership.lm$fitted, lwd = 2) #add fitted line to plot
 lines(ridership.hwa.pred$mean,col="green")
 lines(ridership.hwm.pred$mean,col="red")
 lines(validation.ts, col="brown") #add validation time series to plot
 
 #Compute accuracy of forecasts with respect to validation time series
 accuracy(ridership.hwa.pred$mean, validation.ts) #compute accuracy of Holt Winters additive model
 accuracy(ridership.hwm.pred$mean, validation.ts) #compute accuracy of Holt Winters multiplicative model 
 accuracy(ridership.lm.pred$mean, validation.ts) #compute accuracy of quadratic model 
 
 # #Rolling cross validation
  fhwa <- function(x, h){forecast(HoltWinters(x, seasonal="additive"), h=h)}
 #
 # #No rolling window
  e <- tsCV(ridership.ts, fhwa, h=1)
 
  #Rolling window error
 e.window <- tsCV(ridership.ts, fhwa, h=1, window = 60)
