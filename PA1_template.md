---
output: 
  html_document: 
    keep_md: yes
---
#My Reproducible Research Peer Assessment 1
#CM

##Loading and preprocessing the data


```r
getwd()
```

```
## [1] "C:/Users/Carolyn/Documents/R/Reproducible Research"
```

```r
setwd("C:/Users/Carolyn/Documents/R/Reproducible Research")
activity <- read.csv("activity.csv")

##Clean the data

activity$date <- as.Date(activity$date)
activity$interval <- as.factor(activity$interval)
```

##What is mean total number of steps taken per day.

##For this part of the assignment, you can ignore the missing values in the dataset.




```r
sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)
#Rename attributes
names(sum_data) <- c("date", "total")
```
##Make a histogram of the total number of steps take each day


```r
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="green", 
     xlab="Number of steps", 
     ylim=c(0, 20), 
     main="Histogram of the total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

##Calculate and report the mean and median total number of steps taken per day


```r
mean(sum_data$total)
```

```
## [1] 9354.23
```

```r
median(sum_data$total)
```

```
## [1] 10395
```

##What is the average daily activity pattern?

#Clear the workspace
rm(sum_data)

#Compute the mean of steps across intervals

```r
mean_data <- aggregate(activity$steps, 
                       by=list(activity$interval), 
                       FUN=mean, 
                       na.rm=TRUE)
names(mean_data) <- c("interval", "mean")
head(mean_data)
```

```
##   interval      mean
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```
#What is the average daily activity pattern?

#Make a time series plot


```r
plot(mean_data$interval, 
     mean_data$mean, 
     type="l", 
     col="yellow", 
     lwd=2, 
     xlab="Interval", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_pos <- which(mean_data$mean == max(mean_data$mean))
max_interval <- mean_data[max_pos, 1]
```

#Imputing missing values

#Clear workspace

```r
rm(max_interval)
#Boolean 
NA_count <- sum(is.na(activity$steps))
```

##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

#Find the NA's

```r
na_pos <- which(is.na(activity$steps))
mean_vec <- rep(mean(activity$steps, na.rm=TRUE), times=length(na_pos))
```

#Create a new dataset that is equal to the original dataset but with the missing data filled in.

#Replace the NA's by means

```r
activity[na_pos, "steps"] <- mean_vec
head(activity)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```
##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum)
names(sum_data) <- c("date", "total")
##Compute the histogram
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="green", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Hist of the total number of steps taken each day\n(NA replaced by mean value)")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
Are there differences in activity patterns between weekdays and weekends?

#Function converting weekend to weekday

```r
wkday <- function(dat_val) {
  wd <- weekdays(as.Date(dat_val, '%Y-%m-%d'))
  if  (!(wd == 'Saturday' || wd == 'Sunday')) {
    x <- 'Weekday'
  } 
  else {
    x <- 'Weekend'
  }
  x
}
#Factor weekend as weekday
activity$Day <- as.factor(sapply(activity$date, wkday))
```

#Compute the time series plot using ggplot2

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.0.5
```

```r
#aggregate the mean steps
report_activity <- aggregate(steps~interval+Day,activity,mean)
#plot the data
g<- ggplot(report_activity, aes(interval,steps,activity))
g <-g+ geom_line(stat = 'identity', aes(color='Day')) + facet_grid(Day~.)
g+ labs(x= '5 mins Interval', y = "Average of Steps") + ggtitle("The dataset with “weekday” and “weekend”")
```

```
## geom_path: Each group consists of only one observation. Do you need to adjust
## the group aesthetic?
## geom_path: Each group consists of only one observation. Do you need to adjust
## the group aesthetic?
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


