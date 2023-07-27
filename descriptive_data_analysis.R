## Requirements ################################################################
library(tidyverse)
library(hms)

## Data import #################################################################
# BEFORE YOU START TO IMPORT THE DATA:
# MAKE SURE TO SET THE RIGHT WORKING-DIRECTORY with 

# setwd("C://your/directory/...")

cycling_data <- read.csv2("./Data/cyclist_counts.csv")
location_data <- read.csv2("./Data/locations_of_count.csv")

## Data cleaning ###############################################################
# checking for NA values

na_d_o <- is.na(cycling_data$fleher.deich.ost.stromaufwaerts)
na_d_w <- is.na(cycling_data$fleher.deich.west.stromabwaerts)
na_okb_n <- is.na(cycling_data$okb.nord)
na_okb_s <- is.na(cycling_data$okb.sued)

sum(na_d_o)
# 4 NA Values
sum(na_d_w)
# 4 NA Values
sum(na_okb_n)
# 4 NA Values
sum(na_okb_s)
# 4 NA Values

# looking at the concrete NA rows
cycling_data[na_d_o,]

# on the 28th of march in 2021 all counting stations produced NA values for one
# hour...interesting...

# Insertion can be found in the data processing part. It is easier to work with
# the raw data.

## Data processing #############################################################
# keymetrics -------------------------------------------------------------------
n <- length(cycling_data$date)

# preprocessing ----------------------------------------------------------------
# correcting false datatypes
cycling_data$date = as.Date(cycling_data$date, tryFormats = c("%d.%m.%Y"))

# building data set from scratch
raw_data <- data.frame(id = 1:(4*n))

# adding the date and time
raw_data$date <- rep(cycling_data$date, 4)
raw_data$time <- rep(cycling_data$time, 4)


# adding a weekend dummy variable, by repeating the first 7 days 52 times
# with regard to 96 time-intervals per day and
# adding the last day by hand
raw_data$weekend_yes_no <- rep(
  c(
    rep(
      c(rep(0, 96),
        rep(1, 96),
        rep(1, 96),
        rep(0, 96),
        rep(0, 96),
        rep(0, 96),
        rep(0, 96))
      ,52),
    rep(0, 96)
    ),
  4)

# adding the month spelled out by an inner join
raw_data$month_id <- rep(as.numeric(format(cycling_data$date, "%m")), 4)

number_to_month <- data.frame(id = c(1,2,3,4,5,6,7,8,9,10,11,12),
                              month_spelled_out = factor(c("Januar",
                                                            "Februar",
                                                            "März",
                                                            "April",
                                                            "Mai",
                                                            "Juni",
                                                            "Juli",
                                                            "August",
                                                            "September",
                                                            "Oktober",
                                                            "November",
                                                            "Dezember"),
                                                         levels = c("Januar",
                                                                    "Februar",
                                                                    "März",
                                                                    "April",
                                                                    "Mai",
                                                                    "Juni",
                                                                    "Juli",
                                                                    "August",
                                                                    "September",
                                                                    "Oktober",
                                                                    "November",
                                                                    "Dezember")))
raw_data <- raw_data %>%
  inner_join(number_to_month, by = c("month_id" = "id"))

# adding values from counting stastions
raw_data$cycling_count <- c(cycling_data$fleher.deich.ost.stromaufwaerts,
                            cycling_data$fleher.deich.west.stromabwaerts,
                            cycling_data$okb.nord,
                            cycling_data$okb.sued)

raw_data$counting_station <- c(rep("Fleher Deich ost stromaufwärts", n),
                               rep("Fleher Deich west stromabwärts", n),
                               rep("OKB Nord", n),
                               rep("OKB Süd", n))

# turning counting_station into factor variable
raw_data$counting_station <- as.factor(raw_data$counting_station)

# dealing with NA values -------------------------------------------------------

# because 16 insertions have to be made, it makes sense to define a function to
# make the code more readable

filtered_mean  <- function(month_id_f, time_f, counting_station_f) {
  # filtering data after given criteria
  data <- raw_data %>%
    filter(month_id == month_id_f,
           time == time_f,
           counting_station == counting_station_f)
  
  # return the mean of non NA values
  return(mean(na.omit(data$cycling_count)))
}

# get NA rows
na_data <- raw_data %>%
  filter(is.na(cycling_count))

# insert means in NA rows
for(i in 1:length(na_data$id)) {
  na_row <- na_data[i,]
  id <- na_row$id
  
  raw_data$cycling_count[id] <- filtered_mean(na_row$month_id,
                                              na_row$time,
                                              na_row$counting_station) 
}

# checking if it worked:
sum(is.na(raw_data$cycling_count))
# output: [1] 0

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


