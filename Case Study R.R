library(plyr)
library(forecast)
library(xts)     

StoreTrain <- read.csv("C:/Users/baner/Documents/Walmart/train.csv")
train = read.csv("C:/Users/baner/Documents/Walmart/train.csv")
Features<- read.csv("C:/Users/baner/Documents/Walmart/features.csv")

as.numeric(StoreTrain$IsHoliday == "TRUE")
StoreTrain$IsHoliday<-as.integer(as.logical(StoreTrain$IsHoliday))
Features$IsHoliday<-as.integer(as.logical(Features$IsHoliday))

#finaldataNonHoliday<-subset(Features, IsHoliday==FALSE)
#FeaturesTrainHoliday<-subset(Features, IsHoliday==TRUE)
#View(FeaturesTrainHoliday.xts)

#FeaturesTrainNonHoliday.xts<- xts( x= FeaturesTrainNonHoliday, order.by = as.Date(FeaturesTrainNonHoliday$Date))
myfulldata <- merge(Features, stores)
myfulldata[,5:9][is.na(myfulldata[,5:9])==TRUE]<- 0
View(myfulldata)


#na.fill(FeaturesTrainNonHoliday.xts,fill=0)  
#FeaturesTrainNonHoliday.xts$IsHoliday<-as.integer(as.logical(FeaturesTrainNonHoliday.xts$IsHoliday))
#View(FeaturesTrainNonHoliday.xts)

#na.fill(FeaturesTrainHoliday.xts,fill=1)  
#FeaturesTrainHoliday.xts$IsHoliday<-as.integer(as.logical(FeaturesTrainHoliday.xts$IsHoliday))
#View(FeaturesTrainHoliday.xts)


#View(myfulldata)
#cor(myfulldata)
finaldata <- merge(myfulldata , train)
View(finaldata)
length(finaldata$Date)
head(finaldata)
tail(finaldata)
as.Date(finaldata$Date)
range(as.integer(finaldata$Date))
count(unique(finaldata$Date))
View(unique(finaldata$Date))
View(count((is.na(finaldata))))
plot(finaldata$Date,finaldata$Weekly_Sales) # use to show the equal distribution of sales vs date/time

finaldata.xts<- xts( x= finaldata$Weekly_Sales, order.by = as.Date(finaldata$Date))

plot(finaldata.xts, xlab="Day", ylab="Sales")


finalDataTrain.xts <- window(finaldata.xts, start='2010-02-05', end='2012-04-06')
finalDataTest.xts <- window(finaldata.xts, start='2012-04-13', end='2012-10-26')
View(finalDataTrain.xts)
HWForcModel1 <- HoltWinters(finalDataTrain.xts)
xts
?HoltWinters()



View(finalDataTrain)

as.integer(finaldata$Date)

View()
       