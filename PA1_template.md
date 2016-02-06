---
title: "PA1_template"
author: "W Wocher"
date: "February 6, 2016"
output: html_document
---

###Overview
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

###Data
The data for this assignment can be downloaded from the course web site:
[Activity Monitoring Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:
- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


##Session Information

```r
sessionInfo()
```

```
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] knitr_1.10.5
## 
## loaded via a namespace (and not attached):
## [1] httr_1.0.0    R6_2.1.0      magrittr_1.5  tools_3.2.0   stringi_0.5-5
## [6] stringr_1.0.0 evaluate_0.7
```

##Install Libraries

```r
library("ggplot2")
library("lattice")
```


##1.	Code for reading in the dataset and/or processing the data

Load Data

```r
setwd("D:/Coursera/05 Reproducible Research/Project 1")
data.original <- read.csv("activity.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
```

Make sure Date is defined as a Date.
Get Complete Data (remove NAs)

```r
data.original$date <- as.Date(data.original$date, "%Y-%m-%d")
data.complete <- data.original[complete.cases(data.original),]
```

##2.	Histogram of the total number of steps taken each day

```r
steps.total.df <- aggregate(steps ~ date, data.complete, sum)
hist(steps.total.df$steps, col=rainbow(5), main="Total Step Per Day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


##3.	Mean and median number of steps taken each day

```r
steps.total.mean <- mean(steps.total.df$steps)
steps.total.mean
```

```
## [1] 10766.19
```

```r
steps.total.median <- median(steps.total.df$steps)
steps.total.median
```

```
## [1] 10765
```

##4.	Time series plot of the average number of steps taken

```r
steps.interval <- unique(data.complete$interval)

steps.interval.mean <- with(data.complete, tapply(data.complete$steps, data.complete$interval, mean))

qplot(steps.interval, steps.interval.mean, geom="line", main="Mean/Average Steps Per Interval")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 


##5.	The 5-minute interval that, on average, contains the maximum number of steps

```r
steps.interval.mean.max <- which.max(steps.interval.mean)

steps.interval.max <- names(which.max(steps.interval.mean.max))
steps.interval.max
```

```
## [1] "835"
```


##6.	Code to describe and show a strategy for imputing missing data
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
data.incomplete <- data.original[!complete.cases(data.original),]

nrow(data.incomplete)
```

```
## [1] 2304
```

Fill in the missing data based on the mean for that day.

```r
data.incomplete.interval.value <- data.incomplete$interval

data.incomplete.steps.missing <- match(data.incomplete.interval.value, data.original$interval)

data.incomplete$steps <- steps.interval.mean[data.incomplete.steps.missing]
```

Merge the complete data and the incomplete data together.

```r
nrow(data.original)
```

```
## [1] 17568
```

```r
nrow(data.complete)
```

```
## [1] 15264
```

```r
nrow(data.incomplete)
```

```
## [1] 2304
```

```r
data.merged <- merge(data.complete, data.incomplete, all=TRUE)
nrow(data.merged)
```

```
## [1] 17568
```



##7.	Histogram of the total number of steps taken each day after missing values are imputed
Make a histogram of the total number of steps taken each day.

```r
data.merged.steps.total <- aggregate(steps ~ date, data.merged, sum)
hist(data.merged.steps.total$steps, col=rainbow(5), main="Total Step Per Day")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

Calculate and report the mean and median total number of steps taken per day. 

```r
data.merged.steps.total.mean <- mean(data.merged.steps.total$steps)
data.merged.steps.total.mean
```

```
## [1] 10766.19
```

```r
data.merged.steps.total.median <- median(data.merged.steps.total$steps)
data.merged.steps.total.median
```

```
## [1] 10766.19
```

Mean (Average) of Complete Data

```r
steps.total.mean
```

```
## [1] 10766.19
```
Mean (Average) of Imputed Data

```r
data.merged.steps.total.mean
```

```
## [1] 10766.19
```

Median of Complete Data

```r
steps.total.median
```

```
## [1] 10765
```
Median of Imputed Data

```r
data.merged.steps.total.median
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? 


What is the impact of imputing missing data on the estimates of the total daily number of steps?



##8.	Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Create a variable that defines the Week Name for each entry.

```r
data.merged$weekday <- as.character(weekdays(data.merged$date))
```

Based on the Week Name, determine the Day Type (Weekday or Weekend)

```r
data.merged$day.type <- gsub("Monday","Weekday", data.merged$weekday)
data.merged$day.type <- gsub("Tuesday","Weekday", data.merged$day.type)
data.merged$day.type <- gsub("Wednesday","Weekday", data.merged$day.type)
data.merged$day.type <- gsub("Thursday","Weekday", data.merged$day.type)
data.merged$day.type <- gsub("Friday","Weekday", data.merged$day.type)
data.merged$day.type <- gsub("Saturday","Weekend", data.merged$day.type)
data.merged$day.type <- gsub("Sunday","Weekend", data.merged$day.type)
```

Create a Weekday and Weekend data set

```r
steps.weekday <- unique(subset(data.merged, day.type == "Weekday"))
steps.weekend <- unique(subset(data.merged, day.type == "Weekend"))
```

Calculate the total for each

```r
steps.weekday.total.df <- aggregate(steps ~ interval, steps.weekday, sum)
steps.weekend.total.df <- aggregate(steps ~ interval, steps.weekend, sum)
```

Create the variable that specifies their day type

```r
steps.weekday.total.df$day.type <- "Weekday"
steps.weekend.total.df$day.type <- "Weekend"
```

Merge them together

```r
steps.merged <- merge(steps.weekday.total.df, steps.weekend.total.df, all=TRUE)
```

Plot the comparison between Weekday Steps and Weekend Steps

```r
xyplot(steps ~ interval | day.type, data = steps.merged, type = "l", layout = c(1,2), main="Weekday-Weekend Comparison", xlab="Interval", ylab="Steps")
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png) 



