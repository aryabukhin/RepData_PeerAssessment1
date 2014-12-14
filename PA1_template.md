# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Unzip archive and read raw data from file. Overview the dataset.

```r
unzip('activity.zip', overwrite = TRUE)
raw_data <- read.csv('activity.csv', na.strings = 'NA', header = TRUE)
str(raw_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?

Calculate total steps per each days and draw a histogram of the total number of 
steps taken each day.

```r
steps_per_day <- aggregate(steps~date, data=raw_data, sum, na.rm = TRUE)
hist(steps_per_day$steps, main="Steps per day", xlab = "Total steps per day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Calculate mean and median.

```r
spd_mean <- mean(steps_per_day$steps)
spd_median <- median(steps_per_day$steps)
```

Mean of total number of steps taken per date is 10766.188679. 
And median is 10765.


## What is the average daily activity pattern?

Calculate average steps in 5-minute intervals across all days.

```r
asteps_5min <- aggregate(steps~interval, data=raw_data, mean, na.rm = TRUE)
```

Convert interval names to format _hh:mm_ and add this to data. Draw a plot.

```r
int_names <-sapply(sprintf("%04d", as.integer(asteps_5min$interval)), 
                   function(x){
                       paste(c(substr(x, 1, 2), substr(x, 3, 4)), collapse=':')

                   })

asteps_5min <- cbind(asteps_5min, 'intname' = int_names)

plot(asteps_5min$steps, type='l', main="Average numbers of steps taken", 
     xlab = "5-min intervals", ylab = "Average number of steps", col = "red",
     xaxt = "n")
lab_pos <- seq(from = 0, by = 50, length.out = nrow(asteps_5min)/50)
axis(1, at=lab_pos, labels = asteps_5min$intname[lab_pos + 1])
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Find interval with the maximum average number of steps.

```r
max_asteps_ind <- 
    (1:nrow(asteps_5min))[asteps_5min$steps == max(asteps_5min$steps)]
max_asteps_start <- asteps_5min$intname[max_asteps_ind]
max_asteps_end <- c(asteps_5min$intname, '00:00')[max_asteps_ind + 1]
```

Interval [08:35, 105) 
contains the maximum number of steps on average across all the days.

## Imputing missing values

Total number of missing values in the dataset is 2304.
Fill __NA__s with mean for this 5-minute interval. Replace __NA__ 
values


```r
filled_data <- raw_data
is_na_ind <- is.na(raw_data$steps)
filled_data$steps[is_na_ind] <- 
    asteps_5min$steps[filled_data$interval[is_na_ind]]
```

```
## Warning in filled_data$steps[is_na_ind] <-
## asteps_5min$steps[filled_data$interval[is_na_ind]]: число единиц для
## замены не является произведением длины замены
```

Draw a histogram. Calculate mean and median.

```r
filled_spd <- aggregate(steps~date, data = filled_data, sum, na.rm = TRUE)
hist(filled_spd$steps, main="Steps per day", xlab = "Total steps per day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
mean_fspd <- mean(filled_spd$steps)
median_fspd <- median(filled_spd$steps)
```

Mean of filled dataset is 9544.6371791.
Median is 10395.000000.

Mean and median is differ from the estimates from the first part. Imputing 
missing data enlarge number of observations, decrease mean and median.

## Are there differences in activity patterns between weekdays and weekends?
Set localce to C for English day names. Check first char of day name for S. If
it is TRUE it's weekend. Else weekday. Create new dataset __wddata__ 
with column __wdtype__.


```r
lct <- Sys.getlocale("LC_TIME"); 
Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

```r
wdtypes <- ifelse(
    substr(weekdays(as.Date(raw_data$date, '%Y-%m-%d')), 1, 1) == 'S', 
    'weekend', 
    'weekday')
wddata <- cbind(raw_data, wdtype=wdtypes)
Sys.setlocale("LC_TIME", lct)
```

```
## [1] "ru_RU.UTF-8"
```

Caclucalte average number of steps in 5 minute intervals. Plot graph with 
lattice package.


```r
wd5min <- aggregate(steps ~ interval + wdtype, data = wddata, sum, na.rm = TRUE)
library(lattice)
xyplot(steps ~ interval | wdtype, data = wd5min, layout=c(1, 2), type = "l")
```

![](./PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

Activity patterns between weekdays and weekends are differ.
