# Reproducible Research-Course Project
####This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

```r
# install.packages("lattice")
# install.packages("knitr")
library(knitr)
```

```
## Warning: package 'knitr' was built under R version 3.2.5
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.5
```

```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.2.5
```


###Loading and preprocessing the data


```r
activityData <- read.csv ("C:/Users/lsharifi/Desktop/Rot2/coursera/A5-W2-Courseproject/activity.csv", header = T, sep = ",", stringsAsFactors = F)
```

###convert the date column to the appropriate format:

```r
activityData$date <- as.Date(activityData$date, "%Y-%m-%d")
str(activityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
##1- What is mean total number of steps taken per day?
  
###Create a histogram of the total number of steps taken each day

```r
totalSteps <- aggregate(steps ~ date, data = activityData, sum, na.rm = TRUE)
str(totalSteps)
```

```
## 'data.frame':	53 obs. of  2 variables:
##  $ date : Date, format: "2012-10-02" "2012-10-03" ...
##  $ steps: int  126 11352 12116 13294 15420 11015 12811 9900 10304 17382 ...
```

```r
colnames(totalSteps)<-c("date","steps")
head(totalSteps)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

###Calculate and report the mean and median total number of steps taken per day


```r
mean_steps <- mean(totalSteps$steps,na.rm=TRUE)

median_steps <- median(totalSteps$steps,na.rm=TRUE)

ggplot(totalSteps, aes(x = steps)) + 
                      geom_histogram(fill = "blue", binwidth = 500) + 
                                                 labs(title="Histogram of Steps Taken per Day", 
                                                                 x = "Number of Steps per Day", y = "Number of times in a day") + theme_bw() 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
dev.copy(png,'C:/Users/lsharifi/Desktop/Rot2/coursera/A5-W2-Courseproject/plot1.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## png 
##   2
```
##2- What is the average daily activity pattern?

```r
steps_interval <- aggregate(steps ~ interval, data =activityData, mean, na.rm = TRUE)
plot(steps ~ interval, data = steps_interval, type = "l", xlab = "Time Intervals (5-minute)", 
     ylab = "Mean number of steps taken (all Days)", main = "Average number of steps Taken at 5 minute Intervals",  col = "green")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
png("C:/Users/lsharifi/Desktop/Rot2/coursera/A5-W2-Courseproject/plot2.png")
 
dev.off()
```

```
## png 
##   2
```

```r
max_interval <- steps_interval[which.max( steps_interval$steps),]
```
##3- Imputing missing values


###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
missing_values <- sum(is.na(steps_interval$steps))
mean(is.na(activityData$steps))
```

```
## [1] 0.1311475
```

```r
sum(is.na(activityData$steps))
```

```
## [1] 2304
```

```r
sum(is.na(steps_interval$steps))
```

```
## [1] 0
```

```r
newData<-activityData
  
  for (i in 1:nrow(newData)) {
    if (is.na(newData$steps[i])) {
      index <- newData$interval[i]
      value <- subset(steps_interval, interval==index)
      newData$steps[i] <- value$steps
    }
  }
head(newData)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

```r
#newAvg <- newData %>% group_by(date) %>% summarize(steps_interval = sum(steps, na.rm = T))

newAvg <- aggregate(steps ~ date, newData, mean)
total.steps.per.days <- aggregate(steps ~ date, data = newData, sum)


hist(total.steps.per.days$steps, col = "blue", xlab = "Total Number of Steps", 
     ylab = "Frequency", main = "Histogram of Total Number of Steps taken each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
NewwMean <- mean(total.steps.per.days$steps)
NewMedian <- median(total.steps.per.days$steps)
```
####The mean value is the same as the value before imputing missing data, but the median value has change.The mean value is the same as the value before imputing missing data since the mean value has been used for that particular 5-min interval.The median value is different, since the median index is now being changed after imputing missing values.

##4- Are there differences in activity patterns between weekdays and weekends?


```r
newData$day <- ifelse(as.POSIXlt(as.Date(newData$date))$wday %%6 == 0, "weekend", "weekday")
newData$day <- factor(newData$day, levels = c("weekday", "weekend"))

steps.interval= aggregate(steps ~ interval + day, newData, mean)

xyplot(steps ~ interval | factor(day), data = steps.interval, aspect = 1/2, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->














