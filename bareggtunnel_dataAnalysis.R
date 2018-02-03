#Load forecast and xts packages
library(forecast)
library(xts)

TunnelTraffic_data <- read_xls("C:/Users/baner/Downloads/TunnelTrafficdata.xls")

#create an xts object by day
traffic.xts.day <- xts(x = TunnelTraffic_data$`# Vehicles in tunnel`, order.by = TunnelTraffic_data$Day)
plot(traffic.xts.day, xlab = "Day", ylab = "Traffic", bty = "l", main = "") #plot xts series

#zoom into February (subset by date)
traffic.xts.day.zoom <- traffic.xts.day["20040201/20040229"]
plot(traffic.xts.day.zoom, xlab = "Day", ylab = "Traffic", bty = "l", main = "") #plot xts series

#Aggregate time series by week
traffic.xts.week.sum <- apply.weekly(traffic.xts.day,sum)
plot(traffic.xts.week.sum, xlab = "Week", ylab = "Traffic") #plot time series

#Average time series by week
traffic.xts.week.mean <- apply.weekly(traffic.xts,mean)
plot(traffic.xts.week.mean, xlab = "Week", ylab = "Traffic") #plot time series

#Aggregate time series by month
traffic.xts.month.sum <- apply.monthly(traffic.xts,sum)
plot(traffic.xts.month.sum, xlab = "Month", ylab = "Traffic") #plot time series

#create the time series at the graunalrity that you want
traffic.ts.week <- ts(traffic.xts.week.sum[2:(nrow(traffic.xts.week.sum) -1)],freq=4) #weekly time series excluding first and last values
