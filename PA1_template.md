---
title: 'Reproducible Research: Peer Assessment 1'
author: "Stephane"
date: "04/10/2020"
output: 
  html_document: 
    keep_md: yes
---



## Loading and preprocessing the data



```r
file <- read.csv("activity.csv")

# set strings as factors to false: speed up the computational time
options(stringsAsFactors = FALSE)

# Create a parameter date_long unique along the rows 
data <- file %>%
        mutate(date = ymd(date)) %>%
        mutate(date_long = paste(date, interval))

data$steps <- as.numeric(data$steps)

summary(data)
```

```
##      steps             date               interval       date_long        
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Length:17568      
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Class :character  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Mode  :character  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                     
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                     
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0                     
##  NA's   :2304
```
Note that there are 2304 missing values

## What is mean total number of steps taken per day?


```r
# Number of steps taken each day
steps <- data %>%
         group_by(date) %>%
         summarise(total = sum(steps, na.rm = TRUE), .groups = "drop")

# histogram of the total number of steps each day
hist(steps$total, 5, main="Histogram of total number of steps taken each day", xlab = "Total number of steps")
```

![](PA1_template_files/figure-html/Histogram of steps per day-1.png)<!-- -->

## What is the average daily activity pattern?


```r
total_number_steps <- with(data, tapply(steps, date, sum))
summary(total_number_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```

Note the mean and median number of steps per day prior to the imputting

## Time series of the average number of steps per day


```r
dat <-  data %>%
        group_by(interval, .groups = "drop") %>%
        summarise(avg = mean(steps, na.rm = TRUE), .groups = "drop")

plot(dat$interval, dat$avg, type ='l', 
     xlab ="Time series of the average number of plots per day", 
     ylab = "Average steps")
```

![](PA1_template_files/figure-html/Time Series of mean steps per day-1.png)<!-- -->

```r
# 5' Interval with the maximum of steps per day in average accross all days of the set
dat$interval[which.max(dat$avg)]
```

```
## [1] 835
```


## Imputing missing values


```r
# strategy of filling NA values with a mean for that 5 mn interval
test <- data %>%
        group_by(interval) %>%
        summarise(avg = mean(steps, na.rm = TRUE), .groups = "drop") %>%
        merge(data) %>%
        mutate(steps = ifelse(is.na(steps), avg, steps)) %>%
        select( -date_long, - avg) %>%
        arrange(date)

sum(is.na(test$steps))
```

```
## [1] 0
```
This value above indicates there are no longer NA values in the test data frame


```r
# Number of steps taken each day after imputing
steps_im <- test %>%
            group_by(date) %>%
            summarise(total = sum(steps, na.rm = TRUE), .groups = "drop")

# histogram of the total number of steps each day
hist(steps_im$total, 5, 
     main="Histogram of total number of steps taken each day after imputing",
     xlab = "Total number of steps")
```

![](PA1_template_files/figure-html/Histogram of steps per day after imputing-1.png)<!-- -->

```r
# Comparaison with and without imputing
summary(total_number_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```

```r
total_number_steps_after_imputing <- with(test, tapply(steps, date, sum))
summary(total_number_steps_after_imputing)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```
With the startegy applied (filling NA values with a mean for that 5 mn interval), I do not note large differences

## Are there differences in activity patterns between weekdays and weekends?


```r
new_dt <- test
new_dt$days <- weekdays(test$date)

# find weekend features in the dataset
weekend_feature <- grep("samedi|dimanche", new_dt$days, ignore.case = TRUE)
week_feature <- grep("lundi|mardi|mercredi|jeudi|vendredi", new_dt$days, ignore.case = TRUE)

# subset data of the weekend
weekend_dt<-  new_dt[weekend_feature, ]
weekend_dt$weekday <- "weekend"

# subset data of the weekday
week_dt<-  new_dt[week_feature, ]
week_dt$weekday <- "semaine"

# Complete dt with distinction weekend and weekdays
complete_dt <- rbind(week_dt, weekend_dt)

# week
week_dt$interval <- as.numeric(week_dt$interval)
week <- week_dt %>%
        group_by(interval) %>%
        summarise(avg = mean(steps, na.rm = TRUE), .groups = "drop")

# weekend
weekend_dt$interval <- as.numeric(weekend_dt$interval)
weekend <- weekend_dt %>%
        group_by(interval) %>%
        summarise(avg = mean(steps, na.rm = TRUE), .groups = "drop")

# plot of the 5-minute interval and average number of steps
par(mfrow=c(2,1))

plot(week$interval, 
     week$avg, 
     xlab = " Interval", ylab = "Number of Steps",
     col = "black", lty="solid", type = "l", main ="Weekday")

plot(weekend$interval, 
     weekend$avg, 
     xlab = " Interval", ylab = "Number of Steps",
     col = "red", lty="solid", type = "l", main = "Weekend")
```

![](PA1_template_files/figure-html/Average number of steps within 5 mn interval-1.png)<!-- -->
