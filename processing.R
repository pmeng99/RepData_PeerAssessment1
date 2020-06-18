library(ggplot2)
library(dplyr)
library(lattice)
library(stringr)

unzip("activity.zip")

##
## read in the raw data
##
act <- read.csv("activity.csv", header = TRUE, na.strings = "NA") %>% mutate(date = as.Date(as.character(date), "%Y-%m-%d")) 

##
## What is mean total number of steps taken per day?
##
total_by_date <- act %>% filter(!is.na(steps)) %>% group_by(date) %>% summarise(total = sum(steps))

ggplot(total_by_date) + 
    geom_histogram(aes(x = date, weight=total), col ="blue", binwidth = 1) + 
    labs(x = "Date", y = "Total Number of Steps", title = "Total Number of Steps per Day") 

data.frame(Daily.Total.Mean = mean(total_by_date$total),
           Daily.Total.Median = median(total_by_date$total), 
           Daily.Total.Std = sd(total_by_date$total))

##
## What is the average daily activity pattern?
##
average_by_interval <- act %>% 
  filter(!is.na(steps)) %>%
  group_by(interval) %>% 
  summarise(average = mean(steps)) %>% 
  mutate(tick = paste(str_pad(interval %/% 100, 2, pad = "0"), str_pad(interval %% 100, 2, pad = "0"), sep = ":"))

with(average_by_interval, 
     plot(interval, average, type = "l", xaxt='n', col = "blue", pch = 20, xlab = "Interval", ylab = "Average Number of Steps", main = "Average Number of Steps by Interval", xlim = range(interval)))
axis(1, at=average_by_interval$interval, labels=average_by_interval$tick)

average_by_interval %>% filter(average == max(average))

##
## Imputing missing values
##

##1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(act$steps))

## 2.Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
## or the mean for that 5-minute interval, etc.
## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
filled <- merge(act, average_by_interval, by = "interval") %>% 
    filter(is.na(steps)) %>% mutate(steps = average) %>% 
    select(-average, -tick) %>% rbind(filter(act, !is.na(steps)))

summary(filled)

## 4. Make a histogram of the total number of steps taken each day and 
## Calculate and report the mean and median total number of steps taken per day. 
## Do these values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates of the total daily number of steps?

total_by_date_filled <- filled %>% group_by(date) %>% summarise(total = sum(steps)) %>% mutate(filled = "NA Filled")
total_by_date_unfilled <- total_by_date %>% mutate(filled = "NOT Filled")

ggplot(rbind(total_by_date_filled, total_by_date_unfilled)) + 
  geom_histogram(aes(x = date, weight=total), col ="blue", binwidth = 1) +
  facet_grid(filled ~ .) +
  xlab("Date") + 
  ylab("Total Number of Steps") + 
  ggtitle("Total Number of Steps per Day") 

filled_vs_unfilled <- data.frame(Daily.Total.Mean = c(mean(total_by_date_filled$total), mean(total_by_date$total)),
                                 Daily.Total.Median = c(median(total_by_date_filled$total), median(total_by_date$total)),
                                 Daily.Total.Std = c(sd(total_by_date_filled$total), sd(total_by_date$total)))
row.names(filled_vs_unfilled) <- c("NA Filled", "NOT Filled")

print(filled_vs_unfilled)

## Are there differences in activity patterns between weekdays and weekends?

## 1. Create a new factor variable in the dataset with two levels - "weekday" 
## and "weekend" indicating whether a given date is a weekday or weekend day.
wdays <- factor(weekdays(filled$date) %in% c("Saturday", "Sunday"), labels = c("weekday", "weekend"))
filled <- filled %>% select(-date) %>% mutate(weekday = wdays)
head(filled)

## 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
## interval (x-axis) and the average number of steps taken, averaged across all weekday 
## days or weekend days (y-axis). See the README file in the GitHub repository to see 
## an example of what this plot should look like using simulated data.

filled <- filled %>% 
    group_by(weekday, interval) %>% 
    summarise(average = mean(steps)) %>%
    mutate(interval = as.POSIXct(
      paste("1970-01-01", 
          paste(str_pad(interval %/% 100, 2, pad = "0"), 
                str_pad(interval %% 100, 2, pad = "0"), 
                sep = ":"), 
          sep = " ")))

xyplot(average ~ interval | weekday, 
    type = "l", 
    data = filled, 
    layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of Steps", main = "Average Number of Steps by Interval",
    scales = list(x = list(at = seq(as.POSIXct(filled$interval[1]), by="2 hour", length=12), 
                            labels=format(seq(as.POSIXct(filled$interval[1]), by="2 hour", length=12), "%H:%M"))))
