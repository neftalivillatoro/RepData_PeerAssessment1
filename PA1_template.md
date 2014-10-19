# Assignment 1 
========================================================

## Loading and preprocessing the data


```r
activity<-read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the            dataset.

Make a histogram of the total number of steps taken each day


```r
activity$date=as.Date(activity$date)
b=aggregate(activity$steps, by=list(activity$date), FUN=sum)
hist(b$x, breaks=10, main= "Total Number of Steps Taken Each Day", xlab="Steps")
```

![plot of chunk histogram](figure/histogram-1.png) 

Calculate and report the mean and median total number of steps taken per day


```r
mean(b$x, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(b$x, na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
dailyAverage=aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)

plot(dailyAverage, type="l", main="Average number of steps taken acrross all days by interval of 5-minute", xlab="Intervals", ylab="Average Number of Steps")
```

![plot of chunk averageDaily](figure/averageDaily-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Maximun steps in a day:


```r
max(dailyAverage$x)
```

```
## [1] 206.1698
```

Interval with the maximun steps in a day:


```r
dailyAverage$Group.1[which.max(dailyAverage$x)]
```

```
## [1] 835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Observation: I will use the mean of the 5-minute interval

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
newDataSet=activity
for (row in 1:length(newDataSet$steps)){
        if(is.na(newDataSet[row,1])){
                newDataSet[row,1]=dailyAverage[dailyAverage$Group.1==newDataSet[row,3],2]
        }
}
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.  


```r
newDataSet$date=as.Date(newDataSet$date)
b2=aggregate(newDataSet$steps, by=list(newDataSet$date), FUN=sum)
hist(b2$x, breaks=10, main= "Total Number of Steps Taken Each Day", xlab="Steps")
```

![plot of chunk histogram2](figure/histogram2-1.png) 


```r
mean(b2$x)
```

```
## [1] 10766.19
```

```r
median(b2$x)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment?

The mean remains the same, but the median changed. In this case, when we fill in the values NA, the median is iqual to the mean.

What is the impact of imputing missing data on the estimates of the total daily number of steps?

The impact of imputing missing data is low in this case

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
newDataSet$dayOfWeek=weekdays(newDataSet$date)
newDataSet$type=ifelse(newDataSet$dayOfWeek=="Saturday"|newDataSet$dayOfWeek=="Sunday","Weekend","Weekday")
newDataSet$type=as.factor(newDataSet$type)
newDataSet$interval=as.factor(newDataSet$interval)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.1
```

```r
r=aggregate(newDataSet$steps,list(as.factor(newDataSet$interval),as.factor(newDataSet$type)),mean)
p <- ggplot(r, aes(x = as.integer(Group.1), y=x)) + geom_line()
p + facet_grid(Group.2~.)+xlab("Interval")+ylab("Average Steps in Interval")
```

![plot of chunk panelPlot](figure/panelPlot-1.png) 


```r
library(ggplot2)
r=aggregate(newDataSet$steps,list(newDataSet$interval, newDataSet$type),mean)
p <- ggplot(r, aes(x = as.integer(Group.1), y=x)) + geom_line()
p + facet_grid(Group.2~.)+xlab("Interval")+ylab("Average Steps in Interval")
```

![plot of chunk panelPlot2](figure/panelPlot2-1.png) 
