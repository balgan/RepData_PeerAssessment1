---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
  keep_md: true
---
  
## Loading and preprocessing the data

```r
df_activity <- read.csv("c://Coursera//activity.csv")
```

## What is mean total number of steps taken per day?
Sum the steps, create an histogram, calculate mean and median for steps by day

```r
steps_by_day <- aggregate(steps ~ date, df_activity, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps by day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
steps_day_mean <- mean(steps_by_day$steps)
steps_day_median <- median(steps_by_day$steps)
```
## What is the average daily activity pattern?

```r
steps_by_interval <- aggregate(steps ~ interval, df_activity, mean)
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Mean (number of steps) by interval")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
```


## Imputing missing values

```r
number_missing_values <- sum(!complete.cases(df_activity))
imputed_data <- transform(df_activity, steps = ifelse(is.na(df_activity$steps), steps_by_interval$steps[match(df_activity$interval, steps_by_interval$interval)], df_activity$steps))
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
step_by_day_imputed <- aggregate(steps ~ date, imputed_data, sum)
hist(step_by_day_imputed$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
rmean.i <- mean(step_by_day_imputed$steps)
rmedian.i <- median(step_by_day_imputed$steps)
mean_diff <- rmean.i - steps_day_mean
med_diff <- rmedian.i - steps_day_median
total_diff <- sum(steps_by_day_imputed$steps) - sum(steps_by_day$steps)
```

```
## Error in eval(expr, envir, enclos): object 'steps_by_day_imputed' not found
```



## Are there differences in activity patterns between weekdays and weekends?

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))
steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)
library(lattice)
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
