# Loading the tidyverse packages
library(tidyverse)
library(lubridate) # For working with dates
library(janitor)   # For cleaning column names

# Importing the main data files we need
daily_activity <- read_csv("dailyActivity_merged.csv")
sleep_day <- read_csv("sleepDay_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")

########################3

# Cleaning up column names for easier use
daily_activity <- clean_names(daily_activity)
sleep_day <- clean_names(sleep_day)
hourly_steps <- clean_names(hourly_steps)

# Converting date columns to the correct date format
daily_activity$activity_date <- mdy(daily_activity$activity_date)
sleep_day$sleep_day <- mdy_hms(sleep_day$sleep_day)

# Adding a day of the week column to see trends
daily_activity <- daily_activity %>%
  mutate(day_of_week = wday(activity_date, label = TRUE, abbr = FALSE))

# Mergeing the dataframes to analyze sleep and activity together
merged_data <- left_join(daily_activity, sleep_day, by = c("id" = "id", "activity_date" = "sleep_day"))

###################

# Group the data by day of the week and find the average steps and calories
daily_trends <- daily_activity %>%
  group_by(day_of_week) %>%
  summarise(
    avg_steps = mean(total_steps),
    avg_calories = mean(calories)
  )

# Plot average steps by day of the week
ggplot(data = daily_trends, aes(x = day_of_week, y = avg_steps, fill = day_of_week)) +
  geom_col() +
  labs(
    title = "Average Steps Per Day of the Week",
    x = "Day of the Week",
    y = "Average Steps"
  ) +
  theme_minimal()

###########################

# Creating a scatter plot of total steps vs. calories burned
ggplot(data = daily_activity, aes(x = total_steps, y = calories)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Steps and Calories Burned",
    x = "Total Steps",
    y = "Calories Burned"
  ) +
  theme_minimal()

##############

# Find the average minutes asleep
avg_sleep_minutes <- mean(sleep_day$total_minutes_asleep, na.rm = TRUE)

# Plot a histogram to show the distribution of sleep
ggplot(data = sleep_day, aes(x = total_minutes_asleep / 60)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(
    title = "Distribution of User Sleep Duration",
    x = "Hours Asleep",
    y = "Number of Records"
  ) +
  geom_vline(xintercept = 7, color = "red", linetype = "dashed", size = 1) +
  theme_minimal()


####################


