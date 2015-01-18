# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
### Load the data
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,"data.zip", method ="wget") ##download the zip file
data <- read.csv(unz("data.zip", filename="activity.csv")) ##unzip and read csv

### Preprocess data
data$date = as.Date(data$date, format = "%Y-%m-%d") ##set to date class
data$interval = as.factor(data$interval)
```


## What is mean total number of steps taken per day?
First the steps by date ar loadaded in the *stepsbydate* variable and then, plot a histogram of the data.


```r
stepsbydate <- aggregate(steps ~ date, data = data, FUN = sum, 
                        na.action = na.omit) ## sum data by date
hist(stepsbydate$steps, main = "Histogram of steps by date",
     xlab = "Amount of steps by date") ## Plot a histogram of resulting data
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
### CAlculate mean and median
mean = mean(stepsbydate$steps)
median = median(stepsbydate$steps)
```

The mean is 1.0766189\times 10^{4}, and the median is 10765.

## What is the average daily activity pattern?
The data is aggregated by day to plot average daily values. Note values are divided by number of days.


```r
stepsbytime <- aggregate(steps ~ interval, data = data, FUN = sum)

### Sum by date
stepsbytime$meansteps = stepsbytime$steps / nrow(stepsbydate)
plot(stepsbytime$interval, stepsbytime$meansteps, xlab = "Interval",
    ylab = "Steps averaged by day", main = "Daily Activity Pattern")
lines(stepsbytime$interval, stepsbytime$meansteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
maxint = which.max(stepsbytime$meansteps)
value = max(stepsbytime$meansteps)
```

The interval with the maximum number of steps is interval 104, which has 206.1698113 steps on average.


## Imputing missing values
First we report the number of missing values with the code: 

```r
missing = sum(is.na(data))
```
There are 2304 missing values.

Then we segregate the NA values from the data set, assign average interval values to the missing steps values and merge the data with the original data.

```r
library(plyr) ##for using mapvalues function
## mapvalues function is used to easily write the average steps values to each missing entry given the interval number for the entry.

NAs <- data[is.na(data),] ## Segregate NA values
NAs$steps <- mapvalues(NAs$interval, from = levels(data$interval), to = stepsbytime$meansteps) ## return average step values for given interval in "steps" column
NAs$steps = as.numeric(as.character(NAs$steps)) ##save as numeric values
dat2 = data ##create "dat2" variable to contain given and replaced missing vals.
dat2[is.na(data),1] = NAs$steps ##fill missing values
head(dat2)
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
As we can observe, missing values have been replaced with average interval values. We can now re-evaluate the data like previously done.


```r
stepsbydate <- aggregate(steps ~ date, data = dat2, FUN = sum, na.action = na.omit) ## sum by date
hist(stepsbydate$steps, main = "Histogram of steps by date", 
     xlab = "Amount of steps by date") ##Plot a histogram of the resulting data
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
## 
mean = mean(stepsbydate$steps)
median = median(stepsbydate$steps)
```

The mean is 1.0766189\times 10^{4} and the median is 1.0766189\times 10^{4}.


## Are there differences in activity patterns between weekdays and weekends?
First, the weekend/weekday factor is created by using weekdays() function.


```r
days <- weekdays(dat2$date)##Give name of day
wkend <- c("Saturday", "Sunday")##weekend days
wtest <- days %in% wkend##logical value of weekend or not
dat2$WE <- factor(wtest, labels = c("Weekday","Weekend")) ##Weekend factor var.
```

Then we plot the results.


```r
## Work with steps by date data to count number of weekdays and weekend days.
idays <- weekdays(stepsbydate$date) ##List of individual day names
n <- length(idays) ## Total number of days
w <- sum(idays %in% wkend) ## Number of weekend days
d <- n-w ## Number of weekdays

## Separate filled in data frame into weekdays (d) and weekend days(w).
datw <- dat2[wtest,] ## Weekend dataset
datd <- dat2[!wtest,] ## Weekday dataset

## Aggregate weekend data for plotting
wstepsbytime <- aggregate(steps ~ interval, data = datw, FUN = sum)
wstepsbytime$meansteps <- wstepsbytime$steps / w

## Aggregate weekday data for plotting
dstepsbytime <- aggregate(steps ~ interval, data = datd, FUN = sum) 
dstepsbytime$meansteps <- dstepsbytime$steps / d

## Generate plot
par(mfrow = c(2,1))
plot(wstepsbytime$interval, wstepsbytime$meansteps, type = "l", 
     xlab = "Interval", ylab = "Avg. steps", 
     main = "Weekend")
plot(dstepsbytime$interval, dstepsbytime$meansteps, type = "l", 
     xlab = "Interval", ylab = "Avg. steps", 
     main = "Weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
dev.off()
```

```
## null device 
##           1
```
