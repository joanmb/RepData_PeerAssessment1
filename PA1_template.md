---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
We unzip the file and transform the data into a format suitable for our analysis.

```r
unzip("activity.zip")
activity<-read.csv("./activity.csv")
activity$date<-as.Date(activity$date)
```

## What is mean total number of steps taken per day?  

1. The first we gonna do is to calculate the total number of steps taken per day:

```r
sum_steps_day<-tapply(activity$steps, activity$date, sum)
head(sum_steps_day)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##         NA        126      11352      12116      13294      15420
```

We can check that we haven't lost "any step" in the building of the last variable. This is due to if we have a NA value in the "steps" variable then we have NA values in all of the intervals of this day.

```r
sum(activity$steps, na.rm=TRUE) - sum(sum_steps_day, na.rm=TRUE) 
```

```
## [1] 0
```

2. Now we plot a histogram of the total number of steps taken each day

```r
par(mfrow=c(1,1))
hist(as.numeric(sum_steps_day),
        main="Total number of steps taken each day",
        xlab="Steps",
        ylab="Amount of Days",
        col="skyblue")
```

![](PA1_template_files/figure-html/histogram steps per day-1.png)<!-- -->

3. The last step is to calculate the mean and median of the total number of steps taken per day.

```r
mean(sum_steps_day, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(sum_steps_day, na.rm=TRUE)
```

```
## [1] 10765
```

Hence we have that the mean is 10766.19 and the median 10765.

## What is the average daily activity pattern?

We gonna calculate the mean of steps per day:

```r
mean_steps_interval<-tapply(activity$steps, activity$interval, mean, na.rm=TRUE)
head(mean_steps_interval)
```

```
##         0         5        10        15        20        25 
## 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396
```
Unlike the case of steps per day, now we have removed the NA values because in some days we have data available per interval and some days don't.  

Let's make the plot:

```r
plot(unique(activity$interval), mean_steps_interval,
        main="Average daily activity pattern",
        xlab="5-minute interval time of the day",
        ylab="Average of steps taken",
        col="tomato2",
        type="l")
```

![](PA1_template_files/figure-html/plot daily pattern-1.png)<!-- -->

## Imputing missing values

1. We start counting the total number of missing values in the dataset.

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
sum(is.na(activity$interval))
```

```
## [1] 0
```

```r
sum(is.na(activity$date))
```

```
## [1] 0
```
We have seen that we have NA values only in the *steps* variable, an amount of the 2304.



## Are there differences in activity patterns between weekdays and weekends?
