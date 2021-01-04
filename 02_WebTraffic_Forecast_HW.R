library(googleAnalyticsR)
library(googleAuthR)

viewid<-xxxxxxxxx
start_date<-"2017-05-01"
end_date<-"2020-12-30"

df2 <- google_analytics_4(viewid, date_range = c(start_date, end_date),metrics = c("sessions"),dimensions = c("yearMonth"))
df2 <- google_analytics_4(viewid, date_range = c(start_date, end_date),metrics = c("sessions"),dimensions = c("yearMonth", "channelgrouping"))   #filter organic traffic

write.csv(df2, file="C:/Users/Desktop/datasets/sessions.csv", row.names=FALSE)  
#filter organic and import

sessions <- read.csv("C:/Users/Desktop/datasets/sessions.csv")
df<-sessions

head(df, n=10)
ga_ts <- ts(df$sessions, start = c(2017,05), end = c(2021,01), frequency = 12)

plot(ga_ts)
library(TSA)
library(tseries)
library(forecast)
kpss.test(ga_ts)

fcast<-hw(ga_ts, seasonal="additive")
plot(fcast)
