library(ggplot2)

activity<- read.csv("C:/Users/200062/Downloads/repdata_data_activity/activity.csv")



date_of_activity<- as.Date.factor(activity$date,  "%m/%d/%Y" )

week_day<- weekdays(activity$date)
activity <-cbind(activity, week_day)
summary(activity)

#Question 1: What is mean total number of steps taken per day?
act.totalsteps<- with(activity, aggregate(steps, by = list(date), FUN = sum, na.rm = TRUE))

names(act.totalsteps)<- c("date", "steps")
#Create a histogram of the total number of steps taken per day
hist(act.totalsteps$steps, main = "The Total number of steps taken per day", xlab = "The Total steps taken per day", col = "pink", ylim = c(0,20), breaks = seq(0,25000, by=2500))

#Calculate the Mean and Median number of total steps taken per day:
mean(act.totalsteps$steps)

median(act.totalsteps$steps)



#Question 2: What is the average daily activity pattern?
##Create a Time series plot (type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

avg.daily.act<- aggregate(activity$steps, by= list(activity$interval), FUN = mean , na.rm = TRUE)

names(avg.daily.act)<-c("interval", "mean")

plot(avg.daily.act$interval, avg.daily.act$mean, type = "l", xlab = "Interval", ylab = "Average number of steps", main = "Average number of steps per interval")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

avg.daily.act[which.max(avg.daily.act$mean),]$interval



#Question 3: Imputing missing values
##There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity$steps))

##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

miss.values<- avg.daily.act$mean[match(activity$interval,avg.daily.act$interval)]

##Create a new dataset that is equal to the original dataset but with the missing data filled in.
activity.clean <- transform(activity, steps = ifelse(is.na(activity$steps), yes = miss.values, no = activity$steps))

total.miss.values<- aggregate(steps ~ date, activity.clean, sum)

names(total.miss.values)<- c("date", "daily.steps")
##Make a histogram of the total number of steps taken each day 
hist(total.miss.values$daily.steps, col = "lightblue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))

#Calculate & report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
  
mean(total.miss.values$daily.steps)

median(total.clean.steps$daily.steps)


#Question 4: Are there differences in activity patterns between weekdays and weekends?
#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
activity$datetype <- sapply(activity$date, function(x) {
  if (week_day(x) == "Saturday" | week_day(x) =="Sunday") 
  {y <- "weekday"} else 
  {y <- "weekend"}
  y
})

##Create a panel plot containing a time series plot (type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

activity.datetype <- aggregate(steps~interval+date, activity,mean, na.rm =TRUE)
ggplot(activity.datetype, aes(x = interval, y = steps, color = date))+ geom_line() + labs(title = "Average daily steps by date type", x = "Interval", y = "Average number of steps") + facet_wrap(~date, ncol = 1, nrow = 2) 



