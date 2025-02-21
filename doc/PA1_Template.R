library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)
activity <- read_csv("Downloads/repdata_data_activity/activity.csv")
activity
View(activity)

# to create a new column to store the name of the days
activity$days <- wday(activity$date, label = T) 

# to show the number of NA in the column steps
activity %>% filter(is.na(steps)) %>% nrow()

# to show the mean of steps per day
activity %>% filter(!is.na(steps)) %>% group_by(days) %>% summarise(mean = mean(steps))

# to show the median of steps per day
activity %>% filter(!is.na(steps)) %>% group_by(days) %>% summarise(median = median(steps))

# to show the total number of steps taken per day
activity %>% filter(!is.na(steps)) %>% group_by(days) %>% summarise(Total = sum(steps))

# The histogram of the total steps per day
ggplot(activity %>% filter(!is.na(steps)), aes(x = steps)) +
    geom_histogram(binwidth = 100, fill = "skyblue", color = "black", alpha = 0.7) +
    facet_wrap(~ days) +
    labs(title = "Histogram of Steps per Day by Day of the Week",
         x = "Number of Steps", y = "Frequency") +
    theme_minimal()

# the time-series plot of the average numbers of steps taken per day
activity %>% filter(!is.na(steps)) %>% group_by(date) %>% 
    summarise(avg_steps = mean(steps)) %>%
    ggplot(aes(x = date, y = avg_steps)) + geom_line(color = "blue") +
    labs(title = "Average Steps per Day", x = "Date", y = "Average Steps") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# to impute the missing value with the average steps
activity_2 <- activity %>%
    mutate(steps = ifelse(is.na(steps),
                          mean(steps, na.rm = T), steps))

# Histogram after imputing the missing values in the Steps Column
ggplot(activity_2, aes(x = steps)) +
     geom_histogram(binwidth = 100, fill = "skyblue", color = "black", alpha = 0.7) +
     facet_wrap(~ days) +
     labs(title = "Histogram of Steps per Day by Day of the Week \n After imputing the missing value in the steps column",
          x = "Number of Steps", y = "Frequency") +
     theme_minimal()

# Panel plot comparing the average number of steps taken per 5-minute interval
# across weekdays and weekends
activity_2$daysType <- case_when(activity_2$days %in%
                                     c('Mon', 'Tue', 'Wed', 'Thu', 'Fri') ~ 'weekday',
                                 activity_2$days %in% c('Sat', 'Sun') ~ 'weekend')

activity_summary <- activity_2 %>%
     group_by(interval, daysType) %>%
     summarise(avg_steps = mean(steps, na.rm = TRUE))

ggplot(activity_summary, aes(x = interval, y = avg_steps)) +
     geom_line() +  # Add lines to show the trend
     facet_wrap(~ daysType, ncol = 1) +  # Separate panels for weekdays and weekends
     labs(title = "Average Steps per 5-Minute Interval (Weekdays vs Weekends)",
                       x = "5-Minute Interval", y = "Average Number of Steps") +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels