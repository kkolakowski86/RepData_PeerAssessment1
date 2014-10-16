---
title: "Reproducible Research: Peer Assessment 1"
output: 
html_document:
keep_md: true
---




## Loading and preprocessing the data

```r
unzip('activity.zip')
rawData <- read.csv('activity.csv')
```


## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

```r
grouped = aggregate (steps ~ date, data = rawData, FUN=sum)
barplot(
    height    = grouped$steps, 
    names.arg = grouped$date,
    col       = rainbow(400), 
    las       = 2
)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

2. Calculate and report the mean and median total number of steps taken per day


```r
grouped.mean   <- mean(grouped$steps)
grouped.median <- median(grouped$steps)
```
mean is 10766 and median is 10765

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
interval <- aggregate(steps ~ interval, data = rawData, FUN = mean)
plot(interval, type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
maxRow <- which.max(interval$steps)
maxInt <- interval$interval[maxRow]
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


Interval contains the maximum number of steps is #835


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)



```r
naElements <- rawData[!complete.cases(rawData),]
naElements <- nrow(naElements)
```

Total missing values - 2304

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
imputedData <- rawData;

getMeanByInterval <- function(int)
{
    round(interval[interval[,1] == int, 2])
}

for (element in 1:nrow(imputedData))
{ 
    if (is.na(imputedData[element,1]))
    { 
        imputedData[element,]$steps <- getMeanByInterval(imputedData[element,]$interval)
    }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
imputedByDate <- aggregate(steps ~ date, data = imputedData, FUN = sum)
barplot(
    imputedByDate$steps, 
    names.arg = imputedByDate$date, 
    col       = rainbow(400), 
    las       = 2
)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
mean(imputedByDate$steps)
```

```
## [1] 10765.64
```

```r
median(imputedByDate$steps)
```

```
## [1] 10762
```
## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.



```r
getDayType <- function(d)
{    
    strDay <- weekdays(as.Date(d));
    
    if (strDay %in% c("Saturday","Sunday"))
    {
        return("weekend")
    }
    else
    {
        return ("weekday")
    }
}

imputedData$dayType <- as.factor(sapply(imputedData$date, getDayType))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
plotByDayType <- function(type)
{
    dayTypePlot <- aggregate(
        steps   ~ interval, 
        data    = imputedData, 
        subset  = imputedData$dayType == type, 
        FUN     = mean
    )
    plot(dayTypePlot, type = "l", main = type)
}

par(mfrow = c(1, 2))

plotByDayType("weekend")
plotByDayType("weekday")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

