---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## Loading and preprocessing the data
1. Load the data:

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.2
```

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.4.2
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
activity <- read.csv("activity.csv")

summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
2. Process/transform the data into a format suitable for the analysis:

```r
activity$date <- ymd(activity$date)
cleanactivity <- activity[!is.na(activity$steps),]
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day:

```r
daysteps <- aggregate(cleanactivity$steps,by = list(cleanactivity$date), FUN = sum, na.rm = TRUE)
```
2. Make a histogram of the total number of steps taken each day:

```r
qplot(daysteps$x, xlab = "Steps", main = "Total number of steps taken per day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="PA1_template_files/figure-html/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />
3. Calculate and report the mean and median of the total number of steps taken per day

```r
# Calculate Statistics
mean  <- aggregate(cleanactivity$steps,by = list(cleanactivity$date), FUN = mean, na.rm = TRUE)
median<- aggregate(cleanactivity$steps,by = list(cleanactivity$date), FUN = median, na.rm = FALSE)
# Assign Names to Data Frames
names(mean) <- c("Date","Mean")
names(median) <- c("Date","Median")
stats <- merge(mean,median,by = "Date")

print(stats)
```

```
##          Date       Mean Median
## 1  2012-10-02  0.4375000      0
## 2  2012-10-03 39.4166667      0
## 3  2012-10-04 42.0694444      0
## 4  2012-10-05 46.1597222      0
## 5  2012-10-06 53.5416667      0
## 6  2012-10-07 38.2465278      0
## 7  2012-10-09 44.4826389      0
## 8  2012-10-10 34.3750000      0
## 9  2012-10-11 35.7777778      0
## 10 2012-10-12 60.3541667      0
## 11 2012-10-13 43.1458333      0
## 12 2012-10-14 52.4236111      0
## 13 2012-10-15 35.2048611      0
## 14 2012-10-16 52.3750000      0
## 15 2012-10-17 46.7083333      0
## 16 2012-10-18 34.9166667      0
## 17 2012-10-19 41.0729167      0
## 18 2012-10-20 36.0937500      0
## 19 2012-10-21 30.6284722      0
## 20 2012-10-22 46.7361111      0
## 21 2012-10-23 30.9652778      0
## 22 2012-10-24 29.0104167      0
## 23 2012-10-25  8.6527778      0
## 24 2012-10-26 23.5347222      0
## 25 2012-10-27 35.1354167      0
## 26 2012-10-28 39.7847222      0
## 27 2012-10-29 17.4236111      0
## 28 2012-10-30 34.0937500      0
## 29 2012-10-31 53.5208333      0
## 30 2012-11-02 36.8055556      0
## 31 2012-11-03 36.7048611      0
## 32 2012-11-05 36.2465278      0
## 33 2012-11-06 28.9375000      0
## 34 2012-11-07 44.7326389      0
## 35 2012-11-08 11.1770833      0
## 36 2012-11-11 43.7777778      0
## 37 2012-11-12 37.3784722      0
## 38 2012-11-13 25.4722222      0
## 39 2012-11-15  0.1423611      0
## 40 2012-11-16 18.8923611      0
## 41 2012-11-17 49.7881944      0
## 42 2012-11-18 52.4652778      0
## 43 2012-11-19 30.6979167      0
## 44 2012-11-20 15.5277778      0
## 45 2012-11-21 44.3993056      0
## 46 2012-11-22 70.9270833      0
## 47 2012-11-23 73.5902778      0
## 48 2012-11-24 50.2708333      0
## 49 2012-11-25 41.0902778      0
## 50 2012-11-26 38.7569444      0
## 51 2012-11-27 47.3819444      0
## 52 2012-11-28 35.3576389      0
## 53 2012-11-29 24.4687500      0
```

## What is the average daily activity pattern?
1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
# Calculate Average Steps per 5-min interval
interval <- aggregate(cleanactivity$steps,by = list(cleanactivity$interval),FUN = mean)
# Assign Names to Data Frame
names(interval) <- c("Interval","Steps")
```

```r
plot(x = interval$Interval,y = interval$Steps, type = "l", col = "blue",
                                               xlab = "5-min Interval", ylab = "Average Steps",
                                               main = "Average Steps by 5-min Interval Accross all days")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# Obtaining 5-min Interval
interval$Interval[interval$Steps==max(interval$Steps)]
```

```
## [1] 835
```


## Imputing missing values
1. Calculate and report the total number of missing values in the dataset:

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset:

Several strategies migth be implemented for this mission, to simplify I will just substitute NA values by
0's assuming that no steps were taken that specific day/interval due to different reasons. Other strategies can be:

- Substituting by random value with normal distribution of corresponding 5-min interval.
- Substituting by random value with normal distribution of corresponding day.
- Substituting by mean value of corresponding 5-min interval.
- Substituting by mean value of corresponding day.
- ...

3. Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
fillactivity <- activity
fillactivity$steps[is.na(activity$steps)] <- 0
```
4. Make a histogram of the total number of steps taken each day:

```r
# Calculate Total Number of Steps
daysteps2 <- aggregate(fillactivity$steps,by = list(fillactivity$date), FUN = sum, na.rm = TRUE)
# Plot Histogram of Total number of Steps
qplot(daysteps2$x, xlab = "Steps", main = "Total number of steps taken per day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

<img src="PA1_template_files/figure-html/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />
5. Calculate and report the mean and median total number of steps taken per day:

```r
# Calculate Statistics
mean2  <- aggregate(fillactivity$steps,by = list(fillactivity$date), FUN = mean, na.rm = TRUE)
median2<- aggregate(fillactivity$steps,by = list(fillactivity$date), FUN = median, na.rm = FALSE)
# Assign Names to Data Frames
names(mean2) <- c("Date","Mean")
names(median2) <- c("Date","Median")
stats2 <- merge(mean2,median2,by = "Date")

print(stats2)
```

```
##          Date       Mean Median
## 1  2012-10-01  0.0000000      0
## 2  2012-10-02  0.4375000      0
## 3  2012-10-03 39.4166667      0
## 4  2012-10-04 42.0694444      0
## 5  2012-10-05 46.1597222      0
## 6  2012-10-06 53.5416667      0
## 7  2012-10-07 38.2465278      0
## 8  2012-10-08  0.0000000      0
## 9  2012-10-09 44.4826389      0
## 10 2012-10-10 34.3750000      0
## 11 2012-10-11 35.7777778      0
## 12 2012-10-12 60.3541667      0
## 13 2012-10-13 43.1458333      0
## 14 2012-10-14 52.4236111      0
## 15 2012-10-15 35.2048611      0
## 16 2012-10-16 52.3750000      0
## 17 2012-10-17 46.7083333      0
## 18 2012-10-18 34.9166667      0
## 19 2012-10-19 41.0729167      0
## 20 2012-10-20 36.0937500      0
## 21 2012-10-21 30.6284722      0
## 22 2012-10-22 46.7361111      0
## 23 2012-10-23 30.9652778      0
## 24 2012-10-24 29.0104167      0
## 25 2012-10-25  8.6527778      0
## 26 2012-10-26 23.5347222      0
## 27 2012-10-27 35.1354167      0
## 28 2012-10-28 39.7847222      0
## 29 2012-10-29 17.4236111      0
## 30 2012-10-30 34.0937500      0
## 31 2012-10-31 53.5208333      0
## 32 2012-11-01  0.0000000      0
## 33 2012-11-02 36.8055556      0
## 34 2012-11-03 36.7048611      0
## 35 2012-11-04  0.0000000      0
## 36 2012-11-05 36.2465278      0
## 37 2012-11-06 28.9375000      0
## 38 2012-11-07 44.7326389      0
## 39 2012-11-08 11.1770833      0
## 40 2012-11-09  0.0000000      0
## 41 2012-11-10  0.0000000      0
## 42 2012-11-11 43.7777778      0
## 43 2012-11-12 37.3784722      0
## 44 2012-11-13 25.4722222      0
## 45 2012-11-14  0.0000000      0
## 46 2012-11-15  0.1423611      0
## 47 2012-11-16 18.8923611      0
## 48 2012-11-17 49.7881944      0
## 49 2012-11-18 52.4652778      0
## 50 2012-11-19 30.6979167      0
## 51 2012-11-20 15.5277778      0
## 52 2012-11-21 44.3993056      0
## 53 2012-11-22 70.9270833      0
## 54 2012-11-23 73.5902778      0
## 55 2012-11-24 50.2708333      0
## 56 2012-11-25 41.0902778      0
## 57 2012-11-26 38.7569444      0
## 58 2012-11-27 47.3819444      0
## 59 2012-11-28 35.3576389      0
## 60 2012-11-29 24.4687500      0
## 61 2012-11-30  0.0000000      0
```
6. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# Merge both Data Frames by Date
diff_mean <- merge(mean,mean2,by="Date")
diff_mean$Difference <- diff_mean[,2] - diff_mean[,3]
diff_median <- merge(median,median2,by="Date")
diff_median$Difference <- diff_median[,2] - diff_median[,3]

print(diff_mean)
```

```
##          Date     Mean.x     Mean.y Difference
## 1  2012-10-02  0.4375000  0.4375000          0
## 2  2012-10-03 39.4166667 39.4166667          0
## 3  2012-10-04 42.0694444 42.0694444          0
## 4  2012-10-05 46.1597222 46.1597222          0
## 5  2012-10-06 53.5416667 53.5416667          0
## 6  2012-10-07 38.2465278 38.2465278          0
## 7  2012-10-09 44.4826389 44.4826389          0
## 8  2012-10-10 34.3750000 34.3750000          0
## 9  2012-10-11 35.7777778 35.7777778          0
## 10 2012-10-12 60.3541667 60.3541667          0
## 11 2012-10-13 43.1458333 43.1458333          0
## 12 2012-10-14 52.4236111 52.4236111          0
## 13 2012-10-15 35.2048611 35.2048611          0
## 14 2012-10-16 52.3750000 52.3750000          0
## 15 2012-10-17 46.7083333 46.7083333          0
## 16 2012-10-18 34.9166667 34.9166667          0
## 17 2012-10-19 41.0729167 41.0729167          0
## 18 2012-10-20 36.0937500 36.0937500          0
## 19 2012-10-21 30.6284722 30.6284722          0
## 20 2012-10-22 46.7361111 46.7361111          0
## 21 2012-10-23 30.9652778 30.9652778          0
## 22 2012-10-24 29.0104167 29.0104167          0
## 23 2012-10-25  8.6527778  8.6527778          0
## 24 2012-10-26 23.5347222 23.5347222          0
## 25 2012-10-27 35.1354167 35.1354167          0
## 26 2012-10-28 39.7847222 39.7847222          0
## 27 2012-10-29 17.4236111 17.4236111          0
## 28 2012-10-30 34.0937500 34.0937500          0
## 29 2012-10-31 53.5208333 53.5208333          0
## 30 2012-11-02 36.8055556 36.8055556          0
## 31 2012-11-03 36.7048611 36.7048611          0
## 32 2012-11-05 36.2465278 36.2465278          0
## 33 2012-11-06 28.9375000 28.9375000          0
## 34 2012-11-07 44.7326389 44.7326389          0
## 35 2012-11-08 11.1770833 11.1770833          0
## 36 2012-11-11 43.7777778 43.7777778          0
## 37 2012-11-12 37.3784722 37.3784722          0
## 38 2012-11-13 25.4722222 25.4722222          0
## 39 2012-11-15  0.1423611  0.1423611          0
## 40 2012-11-16 18.8923611 18.8923611          0
## 41 2012-11-17 49.7881944 49.7881944          0
## 42 2012-11-18 52.4652778 52.4652778          0
## 43 2012-11-19 30.6979167 30.6979167          0
## 44 2012-11-20 15.5277778 15.5277778          0
## 45 2012-11-21 44.3993056 44.3993056          0
## 46 2012-11-22 70.9270833 70.9270833          0
## 47 2012-11-23 73.5902778 73.5902778          0
## 48 2012-11-24 50.2708333 50.2708333          0
## 49 2012-11-25 41.0902778 41.0902778          0
## 50 2012-11-26 38.7569444 38.7569444          0
## 51 2012-11-27 47.3819444 47.3819444          0
## 52 2012-11-28 35.3576389 35.3576389          0
## 53 2012-11-29 24.4687500 24.4687500          0
```

```r
print(diff_median)
```

```
##          Date Median.x Median.y Difference
## 1  2012-10-02        0        0          0
## 2  2012-10-03        0        0          0
## 3  2012-10-04        0        0          0
## 4  2012-10-05        0        0          0
## 5  2012-10-06        0        0          0
## 6  2012-10-07        0        0          0
## 7  2012-10-09        0        0          0
## 8  2012-10-10        0        0          0
## 9  2012-10-11        0        0          0
## 10 2012-10-12        0        0          0
## 11 2012-10-13        0        0          0
## 12 2012-10-14        0        0          0
## 13 2012-10-15        0        0          0
## 14 2012-10-16        0        0          0
## 15 2012-10-17        0        0          0
## 16 2012-10-18        0        0          0
## 17 2012-10-19        0        0          0
## 18 2012-10-20        0        0          0
## 19 2012-10-21        0        0          0
## 20 2012-10-22        0        0          0
## 21 2012-10-23        0        0          0
## 22 2012-10-24        0        0          0
## 23 2012-10-25        0        0          0
## 24 2012-10-26        0        0          0
## 25 2012-10-27        0        0          0
## 26 2012-10-28        0        0          0
## 27 2012-10-29        0        0          0
## 28 2012-10-30        0        0          0
## 29 2012-10-31        0        0          0
## 30 2012-11-02        0        0          0
## 31 2012-11-03        0        0          0
## 32 2012-11-05        0        0          0
## 33 2012-11-06        0        0          0
## 34 2012-11-07        0        0          0
## 35 2012-11-08        0        0          0
## 36 2012-11-11        0        0          0
## 37 2012-11-12        0        0          0
## 38 2012-11-13        0        0          0
## 39 2012-11-15        0        0          0
## 40 2012-11-16        0        0          0
## 41 2012-11-17        0        0          0
## 42 2012-11-18        0        0          0
## 43 2012-11-19        0        0          0
## 44 2012-11-20        0        0          0
## 45 2012-11-21        0        0          0
## 46 2012-11-22        0        0          0
## 47 2012-11-23        0        0          0
## 48 2012-11-24        0        0          0
## 49 2012-11-25        0        0          0
## 50 2012-11-26        0        0          0
## 51 2012-11-27        0        0          0
## 52 2012-11-28        0        0          0
## 53 2012-11-29        0        0          0
```
As we can see from the output there is no difference by substitutting NA values.

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day:

```r
fillactivity$day <- ifelse(weekdays(as.Date(fillactivity$date,"%Y %m %d"))%in%c("sabado","domingo"),
                           "weekend","weekday")
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis):

```r
# Calculate Average Steps per 5-min Interval:
avgactivity <- aggregate(fillactivity$steps,by = list(fillactivity$interval,fillactivity$day),FUN = mean)
# Assign Names to Vector
names(avgactivity)<-c("Interval","Day","Steps")
# Plot Average Steps per 5-min Interval
g<-ggplot(data = avgactivity,aes(Interval,Steps))
g + geom_line() + facet_grid(avgactivity$Day~.)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />
