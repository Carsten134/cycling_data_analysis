## Requirements ################################################################
# R version: 4.3.1
library(tidyverse)
library(hms)

## Data import #################################################################
# NOTE: This will only work if you have the right working directory.
# If the error: "path does not exist" is thrown, use setwd("C:/your/dir") to fix
# this issue
raw_data <- read.csv2("./Data/raw_data.csv",
                      fileEncoding = "utf16")

# building datasets ------------------------------------------------------------
# how many cyclists per day
daily_cycling_data <- raw_data %>%
  group_by(date, counting_station) %>%
  reframe(date=date,
          month_id = month_id,
          weekend_yes_no = weekend_yes_no,
          cycling_count = sum(cycling_count),
          counting_station = counting_station) %>%
  unique()

# daily mean usage of cycling stations
time_cycling_data <- raw_data %>%
  group_by(time, counting_station) %>%
  reframe(time = as_hms(time),
          cycling_count = mean(cycling_count),
          counting_station = counting_station) %>%
  unique()

# monthly accumulated usage
monthly_cycling_data <- raw_data %>%
  group_by(month_id, counting_station) %>%
  reframe(month_id = month_id,
          month_spelled_out = month_spelled_out,
          cycling_count = sum(cycling_count)) %>%
  unique()
# checking for outliers --------------------------------------------------------
median_value_day <- median(daily_cycling_data$cycling_count)

outiliers_daily <- daily_cycling_data %>%
  filter(cycling_count > 3*median_value_day)

length(outiliers_daily$cycling_count)
# out: [1] 53

max(outiliers_daily$cycling_count)
# out: [1] 3305

## Descriptive Metrics #########################################################

summarized_daily_data <- daily_cycling_data %>%
  group_by(counting_station) %>%
  reframe(counting_station = counting_station,
            mean_cyclers_per_day = mean(cycling_count),
            var_cyclers_per_day = var(cycling_count)) %>%
  unique()

summarized_daily_data_weekend <- daily_cycling_data %>%
  filter(weekend_yes_no == 1) %>%
  group_by(counting_station) %>%
  reframe(counting_station = counting_station,
        mean_cyclers_on_w_e_per_day = mean(cycling_count),
        var_cyclers_on_w_e_per_day = var(cycling_count)) %>%
  unique()

summarized_daily_data_weekday <- daily_cycling_data %>%
  filter(weekend_yes_no == 0) %>%
  group_by(counting_station) %>%
  reframe(counting_station = counting_station,
          mean_cyclers_on_w_d_per_day = mean(cycling_count),
          var_cyclers_on_w_d_per_day = var(cycling_count)) %>%
  unique()

summarized_daily_data <- summarized_daily_data %>%
  inner_join(summarized_daily_data_weekday,
             by = "counting_station")

# comparing mean and variance between counting stations
# and filtered by weekend and weekday
summarized_daily_data <- summarized_daily_data %>%
  inner_join(summarized_daily_data_weekend,
             by = "counting_station")

# Storing for Latex-table
write.csv2(summarized_daily_data,
           "variance_and_mean_values.csv",
           fileEncoding="utf16")

## Descriptive plots ###########################################################
theme_set(theme_minimal())

# plotting the daily cycling data (still work in progress)
daily_cycling_data %>%
  filter(month_id == 2) %>%
  ggplot(aes(x = date,
             y = cycling_count,
             group = counting_station,
             color = counting_station,
             fill = counting_station)) +
  geom_area() +
  xlab("Datum") +
  ylab("Täglicher Fahrradverkehr")

# plotting monthly data

monthly_cycling_data %>%
  ggplot(aes(x = month_spelled_out,
             y = cycling_count,
             fill = counting_station)) + 
  geom_bar(stat = "identity") +
  xlab("Monat") +
  ylab("Fahrradverkehr")

# to better compare usage over month,
# because bar stacks the values
monthly_cycling_data %>%
  ggplot(aes(x = month_spelled_out,
             y = cycling_count,
             group = counting_station,
             color = counting_station)) + 
  geom_line(linewidth = 1) +
  xlab("Monat") +
  ylab("Fahrradverkehr")

# plotting daily usage

time_cycling_data %>%
  ggplot(aes(x = time,
             y = cycling_count,
             color = counting_station,
             group = counting_station)) +
  geom_line() +
  xlab("Uhrzeit") +
  ylab("Fahrradverkehr")

# plotting distributions after counting station

daily_cycling_data %>%
  ggplot(aes(x = cycling_count))+
  geom_density(color = "red",
               fill = "red",
               alpha = 0.5) +
  xlab("Fahrradverkehr") +
  ylab("Dichte")



raw_data %>%
  ggplot(aes(x = cycling_count,
             group = counting_station,
             color = counting_station,
             fill = counting_station))+
  geom_density(alpha = 0.5) +
  xlab("Fahrradverkehr") +
  ylab("Dichte")


