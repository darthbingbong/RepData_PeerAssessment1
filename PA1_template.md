---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

library(dplyr)
library(lattice)

data <- read.csv("activity.csv")
data$date <- as.Date(data$date)

str(data)
summary(data)


## What is mean total number of steps taken per day?
library(dplyr)

daily_steps <- data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))

hist(daily_steps$total_steps,
     main = "Total Steps Per Day",
     xlab = "Total Steps",
     col = "lightblue")

mean(daily_steps$total_steps)
median(daily_steps$total_steps)

## What is the average daily activity pattern?
interval_avg <- data %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps, na.rm = TRUE))

plot(interval_avg$interval,
     interval_avg$avg_steps,
     type = "l",
     xlab = "Interval",
     ylab = "Average Steps")
     
interval_avg[which.max(interval_avg$avg_steps), ]

## Imputing missing values
sum(is.na(data$steps))

data_imputed <- data

for(i in 1:nrow(data_imputed)){
  if(is.na(data_imputed$steps[i])){
    interval_value <- data_imputed$interval[i]
    data_imputed$steps[i] <- interval_avg$avg_steps[
      interval_avg$interval == interval_value
    ]
  }
}

daily_steps_new <- data_imputed %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))
  
hist(daily_steps_new$total_steps,
     main = "Total Steps (Imputed)",
     xlab = "Total Steps",
     col = "lightgreen")  

mean(daily_steps_new$total_steps)
median(daily_steps_new$total_steps)

## Are there differences in activity patterns between weekdays and weekends?
data_imputed$day_type <- ifelse(
  weekdays(data_imputed$date) %in% c("Saturday","Sunday"),
  "weekend",
  "weekday"
)

data_imputed$day_type <- as.factor(data_imputed$day_type)
interval_daytype <- data_imputed %>%
  group_by(interval, day_type) %>%
  summarise(avg_steps = mean(steps))
  
library(lattice)

xyplot(avg_steps ~ interval | day_type,
       data = interval_daytype,
       type = "l",
       layout = c(1,2),
       xlab = "Interval",
       ylab = "Average Steps")  
