#Load forecast and xts packages
library(forecast)
library(xts)

#create a xts object by day
traffic.xts.day <- xts(x = TunnelTraffic_data_missing_values$`# Vehicles in tunnel`, order.by = TunnelTraffic_data_missing_values$Day)

plot(traffic.xts.day, xlab = "Day", ylab = "Traffic", bty = "l", main = "") #plot xts series

#carry forward most recent observation to fill in hole
traffic.xts.day.filled1 <- na.locf(traffic.xts.day)

#inerpolate adjacent values
traffic.xts.day.filled2 <- na.approx(traffic.xts.day)
