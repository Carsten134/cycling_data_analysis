## Requirements ################################################################
library(tidyverse)
library(hms)

## Data import #################################################################
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


monthly_cycling_data <- raw_data %>%
  group_by(month_id, counting_station) %>%
  reframe(month_id = month_id,
          month_spelled_out = month_spelled_out,
          cycling_count = sum(cycling_count)) %>%
  unique()
## Descriptive Metrics #########################################################

summarized_daily_data <- daily_cycling_data %>%
  group_by(counting_station) %>%
  reframe(counting_station = counting_station,
            mean_cyclers_per_day = mean(cycling_count),
            var_cyclers_per_day = var(cycling_count)) %>%
  unique()

summarized_daily_data

## Descriptive plots ###########################################################
theme_set(theme_minimal())

# plotting the daily cycling data (still work in progress)
daily_cycling_data %>% filter(month_id == 6) %>%
  ggplot(aes(x = date,
             y = cycling_count,
             group = counting_station,
             color = counting_station)) +
  geom_line(size = 1)

# plotting monthly data

monthly_cycling_data %>%
  ggplot(aes(x = month_spelled_out,
             y = cycling_count,
             fill = counting_station)) + 
  geom_bar(stat = "identity")

monthly_cycling_data %>%
  ggplot(aes(x = month_id,
             y = cycling_count,
             group = counting_station,
             color = counting_station)) + 
  geom_line(linewidth = 1)

# plotting daily usage

time_cycling_data %>%
  ggplot(aes(x = time,
             y = cycling_count,
             color = counting_station,
             group = counting_station)) +
  geom_line()


