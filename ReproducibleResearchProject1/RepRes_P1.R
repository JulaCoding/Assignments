library(readr)
activity <- read_csv("RepData_PeerAssessment1/activity.csv")
head(activity)
#Formating date
activity$date<- as.POSIXct(activity$date, "%Y%m%d")
days<- weekdays(activity$date)
#Adding column with weekdays to activity
activity<- cbind(activity,days)

#1)What is mean total number of steps taken per day
totalsteps<- with(activity, aggregate(steps, by = list(date), sum, na.rm=TRUE))
colnames(totalsteps)<- c("Date", "Steps")
#Making the plot 
library(ggplot2)
ggplot(totalsteps, aes(x = Steps)) +
  geom_histogram(fill = "#83CAFF", col = "black", binwidth = 1000) +
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")

#Calculate and report the mean and median total number of steps taken per day
mean(totalsteps$Steps)
median(totalsteps$Steps)

#2) What is the average daily activity pattern?
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
averageDailyActivity<- with(activity, aggregate(steps, by=list(activity$interval), FUN = mean, na.rm=TRUE))
colnames(averageDailyActivity)<- c("Interval", "Mean")
averageActivitydf <- data.frame(averageDailyActivity)
ggplot(averageDailyActivity, aes(x=Interval, y=Mean))+
  geom_line(col = "pink") +
  xlab("Interval") + 
  ylab("Average Number of Steps") + 
  ggtitle("Average Number of Steps Per Interval")
# Which 5-minute interval, on average across all the days in the dataset,contains the maximum number of steps?
averageDailyActivity[which.max(averageDailyActivity$Mean), ]$Interval

#3)Imputing missing values
#. Calculate and report the total number of missing values in the dataset
sum(is.na(activity$steps))
#Devise a strategy for filling in all of the missing values in the dataset.
imputedSteps <- averageDailyActivity$Mean[match(activity$interval, averageDailyActivity$Interval)]
 # Transforming steps in activity if they were missing values with the filled values from above.
activityImputed <- transform(activity, 
                             steps = ifelse(is.na(activity$steps), yes = imputedSteps, no = activity$steps))

 # Forming the new dataset with the imputed missing values.
totalActivityImputed <- aggregate(steps ~ date, activityImputed, sum)

 # Changing col names
names(totalActivityImputed) <- c("date", "dailySteps")
#plot
ggplot(totalActivityImputed, aes(x = dailySteps)) +
  geom_histogram(fill = "#83CAFF", col = "black", binwidth = 1000) +
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")
mean(totalActivityImputed$dailySteps)
median(totalActivityImputed$dailySteps)

#4) Are there differences in activity patterns between weekdays and weekends?
 #updating format of dates
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
 # function that recognises weekend over weekday
activity$dayType<- sapply(activity$date, function(x){
  if(weekdays(x)== "Saturday"| weekdays(x)== "Sunday")
  {y<- "Weekend"}
  else {y<-"Weekday"}
  y
})
activityByDay <-  aggregate(steps ~ interval + dayType, activity, mean, na.rm = TRUE)

dayPlot <-  ggplot(activityByDay, aes(x = interval , y = steps, color = dayType)) + 
  geom_line() + ggtitle("Average Daily Steps by Day Type") + 
  xlab("Interval") + 
  ylab("Average Number of Steps") +
  facet_wrap(~dayType, ncol = 1, nrow=2) +
  scale_color_discrete(name = "Day Type") 
print(dayPlot) 
