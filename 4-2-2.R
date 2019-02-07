# load libraries----
library(tidyverse)
library(data.table)
library(lubridate)
library(fpp)

# set wd  and read data ----
# set wd
setwd("C:\\Users\\ASUS\\Documents\\Ubiqum\\4.1")
input_file <- "household_power_consumption.txt"

# read data
data <- fread(input = input_file, sep = ";", dec = ".")

# Data preprocessing and choice of data ----
# make data numerical
data$Global_active_power <-
  as.numeric(data$Global_active_power, na.rm = TRUE)
data$Global_reactive_power <-
  as.numeric(data$Global_reactive_power, na.rm = TRUE)
data$Voltage <- as.numeric(data$Voltage, na.rm = TRUE)
data$Global_intensity <-
  as.numeric(data$Global_intensity, na.rm = TRUE)
data$Sub_metering_1 <- as.numeric(data$Sub_metering_1, na.rm = TRUE)
data$Sub_metering_2 <- as.numeric(data$Sub_metering_2, na.rm = TRUE)

# make everything into Watts/h
data$Global_active_power <- data$Global_active_power * 1000 / 60
data$Global_reactive_power <- data$Global_reactive_power * 1000 / 60

# 1h = 60 min -> min = h/60
# W/min -> W/(h/60) -> 60*W/h

# make dates in a way that I can read
data <-
  cbind(data, paste(data$Date, data$Time), stringsAsFactors = FALSE)
colnames(data)[10] <- "DateTime"
data$DateTime <-
  as.POSIXct(data$DateTime, format = "%d/%m/%Y %H:%M:%S", tz = "Europe/Paris")

# backup data
data_backup <- data
# write.csv(data_backup, "data_backup.csv")

# extract month
data <- cbind(data, month(data$DateTime), stringsAsFactors = FALSE)
colnames(data)[11] <- "Month"

# extract year
data <- cbind(data, year(data$DateTime), stringsAsFactors = FALSE)
colnames(data)[12] <- "Year"

# extract weekdays
data <- cbind(data, weekdays(data$DateTime), stringsAsFactors = FALSE)
colnames(data)[13] <- "Weekdays"
data$Weekdays <- as.factor(data$Weekdays)

# check the rest of the power and if it is significant
active_power <-
  data$Global_active_power - data$Sub_metering_1 - data$Sub_metering_2 -
  data$Sub_metering_3
data <- cbind(data, active_power)
colnames(data)[14] <- "rest_power"

# extract time
data <- cbind(data, hour(data$DateTime), stringsAsFactors = FALSE)
colnames(data)[15] <- "hour"

# extract the day
data <- cbind(data, day(data$DateTime), stringsAsFactors = FALSE)
colnames(data)[16] <- "day"

# extract date
data <- cbind(data, date(data$DateTime), stringsAsFactors = F)
colnames(data)[17] <- "Date"

# choose only what I need
data_c <-
  data[complete.cases(data), c(10, 15, 16, 13, 11, 12, 17, 3:9, 14)]

# Deconvoluting the data ----

# Granulating data into 15 min intervals
data_c$by15 = cut(data_c$DateTime, breaks = "15 min")

dat.summary <- data_c %>% group_by(by15) %>%
  summarise(
    avgGAP = sum(Global_active_power),
    avgGRP = sum(Global_reactive_power),
    avgSM1 = sum(Sub_metering_1),
    avgSM2 = sum(Sub_metering_2),
    avgSM3 = sum(Sub_metering_3),
    avgSM4 = sum(rest_power)
  )

plot.ts(dat.summary)

dat.mean <- data_c %>% group_by(by15) %>%
  summarise(
    avgGAP = mean(Global_active_power),
    avgGRP = mean(Global_reactive_power),
    avgSM1 = mean(Sub_metering_1),
    avgSM2 = mean(Sub_metering_2),
    avgSM3 = mean(Sub_metering_3),
    avgSM4 = mean(rest_power)
  )

plot.ts(dat.mean)

# by 30 min
data_c$by30 = cut(data_c$DateTime, breaks = "30 min")

dat.sum30 <- data_c %>% group_by(by30) %>%
  summarise(
    avgGAP = sum(Global_active_power),
    avgGRP = sum(Global_reactive_power),
    avgSM1 = sum(Sub_metering_1),
    avgSM2 = sum(Sub_metering_2),
    avgSM3 = sum(Sub_metering_3),
    avgSM4 = sum(rest_power)
  )

plot.ts(dat.sum30)

dat.mean30 <- data_c %>% group_by(by30) %>%
  summarise(
    avgGAP = mean(Global_active_power),
    avgGRP = mean(Global_reactive_power),
    avgSM1 = mean(Sub_metering_1),
    avgSM2 = mean(Sub_metering_2),
    avgSM3 = mean(Sub_metering_3),
    avgSM4 = mean(rest_power)
  )

plot.ts(dat.mean30)

# by 2 h
data_c$by2h = cut(data_c$DateTime, breaks = "2 hours")

dat.sum2h <- data_c %>% group_by(by2h) %>%
  summarise(
    avgGAP = sum(Global_active_power),
    avgGRP = sum(Global_reactive_power),
    avgSM1 = sum(Sub_metering_1),
    avgSM2 = sum(Sub_metering_2),
    avgSM3 = sum(Sub_metering_3),
    avgSM4 = sum(rest_power)
  )

plot.ts(dat.sum2h)

dat.mean2h <- data_c %>% group_by(by2h) %>%
  summarise(
    avgGAP = mean(Global_active_power),
    avgGRP = mean(Global_reactive_power),
    avgSM1 = mean(Sub_metering_1),
    avgSM2 = mean(Sub_metering_2),
    avgSM3 = mean(Sub_metering_3),
    avgSM4 = mean(rest_power)
  )

plot.ts(dat.mean2h)

plot.ts(data_c[, c(1, 8)], frequency = 12)

mydatats <- ts(data_c[, 8])
plot(mydatats, frequency = 12)

# by 10 days
data_c$by10d = cut(data_c$DateTime, breaks = "10 days")

dat.sum10d <- data_c %>% group_by(by10d) %>%
  summarise(
    avgGAP = sum(Global_active_power),
    avgGRP = sum(Global_reactive_power),
    avgSM1 = sum(Sub_metering_1),
    avgSM2 = sum(Sub_metering_2),
    avgSM3 = sum(Sub_metering_3),
    avgSM4 = sum(rest_power)
  )

plot.ts(dat.sum10d)

dat.mean10d <- data_c %>% group_by(by10d) %>%
  summarise(
    avgGAP = mean(Global_active_power),
    avgGRP = mean(Global_reactive_power),
    avgSM1 = mean(Sub_metering_1),
    avgSM2 = mean(Sub_metering_2),
    avgSM3 = mean(Sub_metering_3),
    avgSM4 = mean(rest_power)
  )

plot.ts(dat.mean10d)

dat.mean10dts <-
  ts(dat.mean10d, start = c(2006, 12), frequency = 36)
plot(dat.mean10dts)

mean10dGAP <-
  ts(dat.mean10d$avgGAP,
     start = c(2006, 12),
     frequency = 36)
plot(decompose(mean10dGAP))

mean10dSM1 <-
  ts(dat.mean10d$avgSM1,
     start = c(2006, 12),
     frequency = 36)
mean10dSM1dec <- decompose(mean10dSM1)
mean10dSM1adj <- mean10dSM1 - mean10dSM1dec$seasonal

plot(mean10dSM1dec$seasonal)
plot(decompose(mean10dSM1adj))
plot(mean10dSM1adj)

plot(decompose(mean10dGAP))

a <- mean10dGAP - mean10dGAPdec$seasonal
plot(decompose(a))

mean10dSM2 <-
  ts(dat.mean10d$avgSM2,
     start = c(2006, 12),
     frequency = 36)
mean10dSM2dec <- decompose(mean10dSM2)
mean10dSM2adj <- mean10dSM2 - mean10dSM2dec$seasonal

plot(mean10dSM2)
plot(mean10dSM2adj)

mean10dSM3 <-
  ts(dat.mean10d$avgSM3,
     start = c(2006, 12),
     frequency = 36)
mean10dSM3dec <- decompose(mean10dSM3)
mean10dSM3adj <- mean10dSM3 - mean10dSM3dec$seasonal

plot(mean10dSM3)
plot(mean10dSM3adj)

mean10dSM4 <-
  ts(dat.mean10d$avgSM4,
     start = c(2006, 12),
     frequency = 36)
mean10dSM4dec <- decompose(mean10dSM4)
mean10dSM4adj <- mean10dSM4 - mean10dSM4dec$seasonal

plot(mean10dSM4)
plot(mean10dSM4adj)

mean10dSM1adj_for <- HoltWinters(mean10dSM1adj, beta = F, gamma = F)
plot(forecast(mean10dSM1adj_for, h = 3))


armean10dGAP <- auto.arima(mean10dGAP)
autoplot(forecast(armean10dGAP)) + ylab("mean Global active power")
armean10dSM1 <- auto.arima(mean10dSM1)
autoplot(forecast(armean10dSM1)) + ylab("Kitchen")
armean10dSM2 <- auto.arima(mean10dSM2)
autoplot(forecast(armean10dSM2)) + ylab("Laundry Room")
armean10dSM3 <- auto.arima(mean10dSM3)
autoplot(forecast(armean10dSM3)) + ylab("Water heating & A/C")
armean10dSM4 <- auto.arima(mean10dSM4)
autoplot(forecast(armean10dSM4)) + ylab("Rest")

mean10dGAPdec <- decompose(mean10dGAP)

tsdisplay(mean10dGAP)
tsdisplay(mean10dGAP - mean10dGAPdec$seasonal)

hwmean10dGAP <- HoltWinters(mean10dGAP)
plot(mean10dGAP)
lines(hwmean10dGAP$seasonal, col = "red")
plot(hwmean10dGAP)

HWmean10dGAP <- HoltWinters(mean10dGAP, beta = FALSE, gamma = FALSE)
plot(HWmean10dGAP)

predict(HWmean10dGAP,
        n.ahead = 10,
        prediction.interval = T)

# by 5 days
data_c$by5d = cut(data_c$DateTime, breaks = "5 days")

dat.mean5d <- data_c %>% group_by(by5d) %>%
  summarise(
    avgGAP = mean(Global_active_power),
    avgGRP = mean(Global_reactive_power),
    avgSM1 = mean(Sub_metering_1),
    avgSM2 = mean(Sub_metering_2),
    avgSM3 = mean(Sub_metering_3),
    avgSM4 = mean(rest_power)
  )

mean5dGAP <-
  ts(dat.mean5d$avgGAP,
     start = c(2006, 12),
     frequency = 72)
plot(decompose(mean5dGAP))

mean5dSM1 <-
  ts(dat.mean5d$avgSM1,
     start = c(2006, 12),
     frequency = 72)
mean5dSM1dec <- decompose(mean5dSM1)
mean5dSM1adj <- mean5dSM1 - mean5dSM1dec$seasonal

plot(mean5dSM1dec$seasonal)
plot(decompose(mean5dSM1adj))
plot(mean5dSM1adj)

plot(decompose(mean5dGAP))
mean5dGAPdec <- decompose(mean5dGAP)

b <- mean5dGAP - mean5dGAPdec$seasonal
plot(decompose(b))

mean5dSM2 <-
  ts(dat.mean5d$avgSM2,
     start = c(2006, 12),
     frequency = 72)
mean5dSM2dec <- decompose(mean5dSM2)
mean5dSM2adj <- mean5dSM2 - mean5dSM2dec$seasonal

plot(mean5dSM2)
plot(mean5dSM2adj)

mean5dSM3 <-
  ts(dat.mean5d$avgSM3,
     start = c(2006, 12),
     frequency = 72)
mean5dSM3dec <- decompose(mean5dSM3)
mean5dSM3adj <- mean5dSM3 - mean5dSM3dec$seasonal

plot(mean5dSM3)
plot(mean5dSM3adj)

mean5dSM4 <-
  ts(dat.mean5d$avgSM4,
     start = c(2006, 12),
     frequency = 72)
mean5dSM4dec <- decompose(mean5dSM4)
mean5dSM4adj <- mean5dSM4 - mean5dSM4dec$seasonal

plot(mean5dSM4)
plot(mean5dSM4adj)

mean5dSM1adj_for <- HoltWinters(mean5dSM1adj, beta = F, gamma = F)
plot(forecast(mean5dSM1adj_for, h = 6))


# group by year, month, day
meanYearMonthDay <- data_c %>% group_by(Year, Month, day) %>%
  summarise(
    meanGAP = mean(Global_active_power),
    meanGRP = mean(Global_reactive_power),
    meanSM1 = mean(Sub_metering_1),
    meanSM2 = mean(Sub_metering_2),
    meanSM3 = mean(Sub_metering_3),
    meanSM4 = mean(rest_power)
  )

meanGAPts <- ts(
  meanYearMonthDay$meanGAP,
  start = c(2006, 12),
  end = c(2010, 11),
  frequency = 365
)
time(meanGAPts)

plot(meanGAPts)
plot(decompose(meanGAPts))
autoplot(decompose(meanGAPts))

# group by year, month
meanYearMonth <- data_c %>% group_by(Year, Month) %>%
  summarise(
    meanGAP = mean(Global_active_power),
    meanGRP = mean(Global_reactive_power),
    meanSM1 = mean(Sub_metering_1),
    meanSM2 = mean(Sub_metering_2),
    meanSM3 = mean(Sub_metering_3),
    meanSM4 = mean(rest_power)
  )

meanGAPYMts <-
  ts(meanYearMonth$meanGAP,
     start = c(2006, 12),
     frequency = 12)

plot(meanGAPYMts)
plot(decompose(meanGAPYMts))
autoplot(decompose(meanGAPYMts))

meanSM4YMts <-
  ts(meanYearMonth$meanSM4,
     start = c(2006, 12),
     frequency = 12)

plot(meanSM4YMts)
plot(decompose(meanSM4YMts))

SM4decomposed <- decompose(meanSM4YMts)
SM4adjusted <- meanSM4YMts - SM4decomposed$seasonal

plot(SM4adjusted)

SM4notrend <- meanSM4YMts - SM4decomposed$trend
plot(SM4notrend)

plot(SM4decomposed$seasonal)

plot(HoltWinters(meanSM4YMts, beta = F, gamma = F))
plot(HoltWinters(meanGAPYMts), beta = F, gamma = F)

plot(forecast(HoltWinters(
  meanGAPYMts, beta = F, gamma = F
), h = 12))
plot(forecast(HoltWinters(
  meanSM4YMts, beta = F, gamma = F
), h = 12))

plot(stlf(meanSM4YMts, method = "arima"))

etsSM4 <- ets(meanSM4YMts)
plot(meanSM4YMts)
lines(etsSM4$fitted, col = "red")

plot(decompose(meanSM4YMts))

meanSM1YMts <-
  ts(meanYearMonth$meanSM1,
     start = c(2006, 12),
     frequency = 12)
plot(HoltWinters(meanSM1YMts, beta = F, gamma = F))
plot(forecast(HoltWinters(
  meanSM1YMts, beta = F, gamma = F
)))
plot(decompose(meanSM1YMts))

meanSM2YMts <-
  ts(meanYearMonth$meanSM2,
     start = c(2006, 12),
     frequency = 12)
plot(HoltWinters(meanSM2YMts, beta = F, gamma = F))
plot(forecast(HoltWinters(meanSM2YMts), beta = F, gamma = F))
plot(decompose(meanSM2YMts))

meanSM3YMts <-
  ts(meanYearMonth$meanSM3,
     start = c(2006, 12),
     frequency = 12)
plot(HoltWinters(meanSM3YMts, beta = F, gamma = F))
plot(forecast(HoltWinters(
  meanSM3YMts, beta = F, gamma = F
)), h = 12)
plot(HoltWinters(meanSM3YMts, beta = F, gamma = F))
plot(decompose(meanSM3YMts))

HWGAP <- HoltWinters(meanGAPYMts, beta = NULL, gamma = NULL)
plot(forecast(HWGAP$fitted, h = 12))
plot(forecast(HWGAP, h = 12))

# preparing non seasonal TS for Year, month ----
# GAP
GAPts_dec <- decompose(meanGAPYMts)
nsGAPts <- meanGAPYMts - GAPts_dec$seasonal

# SM1
Kitchen_dec <- decompose(meanSM1YMts)
nsKitchen <- meanSM1YMts - Kitchen_dec$seasonal

# SM2
Laundry_dec <- decompose(meanSM2YMts)
nsLaundry <- meanSM2YMts - Laundry_dec$seasonal

# SM3
AC_dec <- decompose(meanSM3YMts)
nsAC <- meanSM3YMts - AC_dec$seasonal

# SM4
rest_dec <- decompose(meanSM4YMts)
nsRest <- meanSM4YMts - rest_dec$seasonal

# Holt Winters of all of them
nsGAPhw <- HoltWinters(nsGAPts, gamma = F)
nsKitchenhw <- HoltWinters(nsKitchen, gamma = F)
nsLaundryhw <- HoltWinters(nsLaundry, gamma = F)
nsAChw <- HoltWinters(nsAC, gamma = F)
nsResthw <- HoltWinters(nsRest, gamma = F)

# plot all of them
plot(meanGAPYMts, main = "Global active power",
     ylab = "mean Global active power")
plot(decompose(meanGAPYMts))
plot(nsGAPts, ylab = "non seasonal Global active power")

plot(decompose(meanSM1YMts))
autoplot(decompose(meanSM1YMts))

# check for the errors
accuracy(nsGAPfor)
accuracy(nsKitchenfor)
accuracy(nsLaundryfor)
accuracy(nsACfor)
accuracy(nsRestfor)

plot(nsGAPhw, main = "Holt-Winters filtering, Global active power")
plot(nsKitchenhw, main = "Holt-Winters filtering, Kitchen")
plot(nsLaundryhw, main = "Holt-Winters filtering, Laundry Room")
plot(nsAChw, main = "Holt-Winters filtering, Heating & A/C")
plot(nsResthw, main = "Holt-Winters filtering, Rest")

# forecasting the non seasonal series
nsGAPfor <- forecast(nsGAPhw, h = 12)
nsKitchenfor <- forecast(nsKitchenhw, h = 12)
nsLaundryfor <- forecast(nsLaundryhw, h = 12)
nsACfor <- forecast(nsAChw, h = 12)
nsRestfor <- forecast(nsResthw, h = 12)

# plot them
plot(nsGAPfor, ylab = "Global active power")
plot(nsKitchenfor, ylab = "Kitchen")
plot(nsLaundryfor, ylab = "Laundry Room")
plot(nsACfor, ylab = "Heating & A/C")
plot(nsRestfor, ylab = "Rest")

# plot with ggplot
autoplot(nsGAPfor)
autoplot(nsKitchenfor)
autoplot(nsLaundryfor)
autoplot(nsACfor)
autoplot(nsRestfor)

# plot only what you need
plot(nsGAPfor, xlim = c(2010, 2012), ylab = "Global active power")
plot(nsKitchenfor, xlim = c(2010, 2012), ylab = "Kitchen")
plot(nsLaundryfor, xlim = c(2010, 2012), ylab = "Laundry Room")
plot(nsACfor, xlim = c(2010, 2012), ylab = "Heating & A/C")
plot(nsRestfor, xlim = c(2010, 2012), ylab = "Rest")

# plot only what you need
autoplot(nsGAPfor) + xlim(2010, 2012) + ylab("")
autoplot(nsGAPfor) + xlim(2010, 2012) + ylab("non-seasonal Global active power")
plot(nsLaundryfor, xlim = c(2010, 2012))
plot(nsACfor, xlim = c(2010, 2012))
plot(nsRestfor, xlim = c(2010, 2012))

# preparing non seasonal TS for Year, month, day ----
meanYMD <- data_new %>% group_by(Year, Month, day) %>%
  summarise(
    meanGAP = mean(Global_active_power),
    meanGRP = mean(Global_reactive_power),
    meanSM1 = mean(Sub_metering_1),
    meanSM2 = mean(Sub_metering_2),
    meanSM3 = mean(Sub_metering_3),
    meanSM4 = mean(rest_power)
  )

# make them into ts
meanGAPYMDts <-
  ts(meanYMD$meanGAP,
     start = c(2007),
     frequency = 365)
meanSM1YMDts <-
  ts(meanYMD$meanSM1,
     start = c(2007),
     frequency = 365)
meanSM2YMDts <-
  ts(meanYMD$meanSM2,
     start = c(2007),
     frequency = 365)
meanSM3YMDts <-
  ts(meanYMD$meanSM3,
     start = c(2007),
     frequency = 365)
meanSM4YMDts <-
  ts(meanYMD$meanSM4,
     start = c(2007),
     frequency = 365)

# GAP
dGAPts_dec <- decompose(meanGAPYMDts)
nsdGAPts <- meanGAPYMDts - dGAPts_dec$seasonal

# SM1
dKitchen_dec <- decompose(meanSM1YMDts)
nsdKitchen <- meanSM1YMDts - dKitchen_dec$seasonal

# SM2
dLaundry_dec <- decompose(meanSM2YMDts)
nsdLaundry <- meanSM2YMDts - dLaundry_dec$seasonal

# SM3
dAC_dec <- decompose(meanSM3YMDts)
nsdAC <- meanSM3YMDts - dAC_dec$seasonal

# SM4
drest_dec <- decompose(meanSM4YMDts)
nsdRest <- meanSM4YMDts - drest_dec$seasonal

# Holt Winters of all of them
nsdGAPhw <- HoltWinters(nsdGAPts, gamma = F)
nsdKitchenhw <- HoltWinters(nsdKitchen, gamma = F)
nsdLaundryhw <- HoltWinters(nsdLaundry, gamma = F)
nsdAChw <- HoltWinters(nsdAC, gamma = F)
nsdResthw <- HoltWinters(nsdRest, gamma = F)

# plot all of them
plot(nsdGAPhw)
plot(nsdKitchenhw)
plot(nsdLaundryhw)
plot(nsdAChw)
plot(nsdResthw)

# forecasting the non seasonal series
nsdGAPfor <- forecast(nsdGAPhw, h = 30)
nsdKitchenfor <- forecast(nsdKitchenhw, h = 30)
nsdLaundryfor <- forecast(nsdLaundryhw, h = 30)
nsdACfor <- forecast(nsdAChw, h = 30)
nsdRestfor <- forecast(nsdResthw, h = 30)

# plot them
plot(nsdGAPfor)
plot(nsdKitchenfor)
plot(nsdLaundryfor)
plot(nsdACfor)
plot(nsdRestfor)

# plot with ggplot
autoplot(nsdGAPfor)
autoplot(nsdKitchenfor)
autoplot(nsdLaundryfor)
autoplot(nsdACfor)
autoplot(nsdRestfor)

# plot only what you need
plot(nsdGAPfor, xlim = c(2010.1, 2011.0))
plot(nsdKitchenfor, xlim = c(2010, 2011))
plot(nsdLaundryfor, xlim = c(2010, 2011))
plot(nsdACfor, xlim = c(2010, 2011))
plot(nsdRestfor, xlim = c(2010, 2011))

# plot only what you need
autoplot(nsdGAPfor) + xlim(2010, 2011) + ylab("")
autoplot(nsdGAPfor) + xlim(2010, 2012) + ylab("non-seasonal Global active power")
plot(nsdLaundryfor, xlim = c(2010, 2012))
plot(nsdACfor, xlim = c(2010, 2012))
plot(nsdRestfor, xlim = c(2010, 2012))

# Finding non seasonal time series
# January, April, July, October

# January
january <- data_c[which(Month == 1), ]

janmean <- january %>% group_by(Year, day) %>%
  summarise(
    meanGAP = mean(Global_active_power),
    meanGRP = mean(Global_reactive_power),
    Kitchen = mean(Sub_metering_1),
    LaundryRoom = mean(Sub_metering_2),
    Heater_AC = mean(Sub_metering_3),
    Other = mean(rest_power)
  )

ggplot(data = janmean, aes(x = day, y = meanGAP)) +
  facet_grid(Year ~ .) +
  geom_line() + geom_line(aes(y = Kitchen),                                               colour = "red") +
  geom_line(aes(y = LaundryRoom), colour = "green") +
  geom_line(aes(y = Heater_AC), colour = "orange") +
  geom_line(aes(y = Other), colour = "blue")

janGAPts <- ts

# Grouping by Year and day
# delete 2006
data_new <- data_c[Year != 2006, ]

YearDay <- data_new %>% group_by(Year, day) %>%
  summarise(
    meanGAP = mean(Global_active_power),
    meanGRP = mean(Global_reactive_power),
    Kitchen = mean(Sub_metering_1),
    LaundryRoom = mean(Sub_metering_2),
    Heater_AC = mean(Sub_metering_3),
    Other = mean(rest_power)
  )

YearDayGap <- ts(YearDay$meanGAP,
                 start = c(2006),
                 frequency = 30)
plot(YearDayGap)
plot(decompose(YearDayGap))

plot(HoltWinters(YearDayGap))

# Separate by hour of the day
YearHour <- data_new %>% group_by(Year, hour) %>%
  summarise(
    meanGAP = mean(Global_active_power),
    meanGRP = mean(Global_reactive_power),
    Kitchen = mean(Sub_metering_1),
    LaundryRoom = mean(Sub_metering_2),
    Heater_AC = mean(Sub_metering_3),
    Other = mean(rest_power)
  )

YearHourGap <- ts(YearHour$meanGAP,
                  start = c(2006),
                  frequency = 24)
plot(YearHourGap)
plot(decompose(YearHourGap))

plot(HoltWinters(YearHourGap, beta = F, gamma = F))
YHGhw <- HoltWinters(YearHourGap, beta = F)
plot(forecast(YHGhw, h = 24))

# Separate by weekday
YearWeekday <- data_new %>% group_by(Year, Weekdays) %>%
  summarise(
    meanGAP = mean(Global_active_power),
    meanGRP = mean(Global_reactive_power),
    Kitchen = mean(Sub_metering_1),
    LaundryRoom = mean(Sub_metering_2),
    Heater_AC = mean(Sub_metering_3),
    Other = mean(rest_power)
  )

YearWeekdayGap <-
  ts(YearWeekday$meanGAP,
     start = c(2006),
     frequency = 7)
plot(YearWeekdayGap)
plot(decompose(YearWeekdayGap))

plot(HoltWinters(YearWeekdayGap))
YWGhw <- HoltWinters(YearWeekdayGap)
plot(forecast(YWGhw, h = 7))
