---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
activity<-read.csv("activity.csv")
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = "Total Steps Each Day",xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)


## What is mean total number of steps taken per day?

```r
mean(steps_by_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_by_day$steps)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

```r
#making histogram
steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
max_interval
```

```
## [1] 835
```
## Imputing missing values

```r
#finding the indices of missing values (NAs)
NA_index <- which(is.na(as.character(data$steps)))
complete_data <- data
#imputing missing values using the mean for that 5-minute interval
complete_data[NA_index, ]$steps<-unlist(lapply(NA_index, FUN=function(NA_index){
        steps_by_interval[data[NA_index,]$interval==steps_by_interval$interval,]$average_steps
}))
```

```
## Warning in `[<-.factor`(`*tmp*`, iseq, value = c(0L, 5L, 10L, 15L, 20L, : invalid
## factor level, NA generated
```

```r
#creating a data frame with the steps taken for each day
steps_each_day_complete <- aggregate(steps ~ date, data = complete_data, sum)
#adding column names to the created data frame
colnames(steps_each_day_complete) <- c("date", "steps")
#making the histogram
hist(as.numeric(steps_each_day_complete$steps), breaks = 20, col = "red", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)


## Are there differences in activity patterns between weekdays and weekends?

```r
#creating a factor variable "day "to store the day of the week:
complete_data$day <- as.factor(weekdays(as.Date(complete_data$date)))
#creating a logical variable "is_weekday" (weekday=TRUE, weekend = FALE) :
complete_data$is_weekday <- ifelse(!(complete_data$day %in% c("sabato","domenica")), TRUE, FALSE) 
#calculating the average number of steps for weekdays
weekdays_data <- complete_data[complete_data$is_weekday,]
steps_by_interval_weekdays <- aggregate(weekdays_data$steps, by=list(interval=weekdays_data$interval), FUN=mean)
#calculating the average number of steps for weekends
weekends_data <- complete_data[!complete_data$is_weekday,]
steps_by_interval_weekends <- aggregate(weekends_data$steps, by=list(interval=weekends_data$interval), FUN=mean)
#adding columns names
colnames(steps_by_interval_weekdays) <- c("interval", "average_steps")
colnames(steps_by_interval_weekends) <- c("interval", "average_steps")
#adding a column to indicate the day
steps_by_interval_weekdays$day <- "Weekday"
steps_by_interval_weekends$day <- "Weekend"
#merging the two together
week_data <- rbind(steps_by_interval_weekends, steps_by_interval_weekdays)
#converting the day variabke to a factor
week_data$day <- as.factor(week_data$day)
#Making the plot
library(lattice)
xyplot(average_steps ~  interval | day, data = week_data, layout = c(1,2), type ="l", ylab="Number of Steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)
