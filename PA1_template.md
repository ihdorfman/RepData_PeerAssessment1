Reproducible Research: Peer Assessment 1
================

### Isaac Dorfman

### September 18, 2017

Loading and preprocessing the data
----------------------------------

To insure maximum portability of code the importing and preprocessing of the
data handled as such:

``` r
tmp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",tmp)
activity <- read.csv(unz(tmp, "activity.csv"))
```

In the event that the URL for the data set changes in the future this portion
of the code is easily updated for such contingency. In the event that a
future user of this code already has the data set stored locally then the
read.csv function can be used by itself.

What is mean total number of steps taken per day?
-------------------------------------------------

At this step in the analysis it is permissable to ignore the copious numer of
NAs in the data set in order to get a better idea of what information we
actually have.

``` r
activity_mean <- mean(activity$steps, na.rm = TRUE)
activity_mean
```

    ## [1] 37.3826

``` r
activity_median <- median(activity$steps, na.rm = TRUE)
activity_median
```

    ## [1] 0

We can also examine a histogram of the data to see why the mean is likely
going to be more informative of further analysis than the median:

``` r
hist(activity$steps, col = "steelblue", xlab = "Steps Taken Per Day",
     main = "Histogram of the Number of Steps Taken Per Day")
```

![](figure/plot1-1.png)

From this we can see that for the days for which we have data that the
overwhelming majority of days saw no activity from the device.

What is the average daily activity pattern?
-------------------------------------------

To get a better impression of the daily pattern the first step is going to be
a simple (and somewhat messy) time series plot:

``` r
plot(activity$interval,activity$steps, type = "l", xlab = "Interval", 
     ylab = "Number of Steps", main = "Count of Steps by Interval")
```

![](figure/plot2-1.png)

We would also like to know which interval saw the maximum number of steps:

``` r
activity_max <- activity[which(activity[,"steps"]==max(activity$steps, na.rm = TRUE)),]
activity_max
```

    ##       steps       date interval
    ## 16492   806 2012-11-27      615

There are many different approaches to finding both the maximum value for a
given variable and the associated observation. This approach was selected
because it would allow us to see if the maximum value occurred more than once.
While it, as can be seen above, does not it is still a good idea to check for
such possibilities.

Imputing missing values
-----------------------

Before we can begin any imputation for missing values it would be helpful to
know how many missing values we are dealing with and what percent of the total
data set they represent.

``` r
activity_missing <- sum(!complete.cases(activity))
activity_missing
```

    ## [1] 2304

``` r
totals_obs <- nrow(activity)
percent_missing <- 100*(activity_missing/totals_obs)
percent_missing
```

    ## [1] 13.11475

It looks like a little over 13% of our data set is missing a value for the
'steps' variable. The next step is to come up with a reasonable estimate to
fill in those missing values. After some research on this topic the 'mice'
package was selected to assist with creating an imputed data set to fill in the
missing values.

``` r
require("mice")
```

    ## Loading required package: mice

``` r
md.pattern(activity)
```

    ##       date interval steps     
    ## 15264    1        1     1    0
    ##  2304    1        1     0    1
    ##          0        0  2304 2304

This verifies our earlier calculations for the number of missing values. Time
to impute their fill ins.

``` r
activity.imp <- mice(activity, m=1, method = "pmm", seed = 11235)
```

    ## 
    ##  iter imp variable
    ##   1   1  steps
    ##   2   1  steps
    ##   3   1  steps
    ##   4   1  steps
    ##   5   1  steps

``` r
activity_complete <- complete(activity.imp,1)
mean(activity_complete$steps)
```

    ## [1] 40.39356

``` r
median(activity_complete$steps)
```

    ## [1] 0

``` r
hist(activity_complete$steps, col = "paleturquoise1", xlab = "Steps Taken Per Day 
     (with imputed values included)",
     main = "Histogram of the Number of Steps Taken Per Day")
```

![](figure/plot3-1.png)

Even with the imputed values substituting for NAs the median is still 0 while
the imputed mean is around three steps per five minute interval higher than
the original mean.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

Since we are looking at activity levels it would be helpful to know if there
are predictible periods of higher or lower activity than normal or if there is
a predictible split in activity levels. To this end a comparison of weekdays
against the weekend.

``` r
activity_complete$date <- strptime(activity_complete$date, "%Y-%m-%d")
days <- c("Monday","Tuesday","Wednesday","Thursday", "Friday")
activity_complete$weekday <- factor((weekdays(activity_complete$date) %in% days), 
                           levels=c(FALSE, TRUE), 
                           labels=c('weekend', 'weekday'))
```

Now that we have the factor variables 'weekday' we can creat a plot to see if
there are any differences between weekdays and weekends.

``` r
require("lattice")
```

    ## Loading required package: lattice

``` r
xyplot(steps~interval | weekday, data = activity_complete, type="l", 
       col="lightcyan3", layout = c(1,2))
```

![](figure/plot4-1.png)
