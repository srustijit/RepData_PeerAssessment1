setwd("C:/Users/Kanha/Desktop/Coursera/repdata_data_activity")

data<- read.csv("activity.csv")
data$date<- as.Date(data$date)


stepsbyday<- tapply(data$steps, data$date, sum, na.rm=TRUE)
library(ggplot2)

png('Plot1.png')
q1<-qplot(stepsbyday, xlab="No. of Steps Taken Each Day", ylab="Total Frequency", binwidth=500)
print(q1)
dev.off()

medianbyday<- median(stepsbyday)
meanbyday<- mean(stepsbyday)


avg<- tapply(data$steps, data$interval, mean, na.rm=TRUE)
png('Plot2.png')
plot(names(avg), avg, xlab="5-min interval", type="l", ylab="Average no. of steps")
dev.off()


maxavg<- max(avg)
maxinterval<- as.numeric(names(avg)[which(avg==max(avg))])


totalna<- sum(is.na(data$steps))


imputedata<- data


imputedata$steps[which(is.na(data$steps))]<- as.vector(avg[as.character(data[which(is.na(data$steps)),3])])


stepseachday<- tapply(imputedata$steps, imputedata$date, sum, na.rm=TRUE)
png('Plot3.png')
q2<-qplot(stepseachday, xlab="No. of Steps Taken Each Day", ylab="Total Frequency", binwidth=500)
print(q2)
dev.off()


medianEachDayImputed<- median(stepseachday)
meanEachDayImputed<- mean(stepseachday)


imputedata$dayType<- ifelse(as.POSIXlt(imputedata$date)$wday %in% c(0,6), "weekends","weekdays")


aggregateData<- aggregate(steps ~ interval + dayType, data=imputedata, mean)
png('Plot4.png')
g<-ggplot(aggregateData, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(dayType ~ .) +
  xlab("5-minute interval") + 
  ylab("avarage number of steps")
print(g)

dev.off()