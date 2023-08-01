## Requirements ################################################################
# R version: 4.3.1
library(tidyverse)
library(hms)

## Data import #################################################################
# NOTE: This will only work if you have the right working directory.
# If the error: "path does not exist" is thrown, use setwd("C:/your/dir") to fix
# this issue.
raw_data <- read.csv2("./Data/raw_data.csv",
                      fileEncoding = "utf16")

# building datasets ------------------------------------------------------------

# monthly accumulated usage
monthly_cycling_data <- raw_data %>%
  group_by(month_id, counting_station) %>%
  reframe(month_id = month_id,
          month_spelled_out = month_spelled_out,
          cycling_count = sum(cycling_count)) %>%
  unique()

## Contingency table ###########################################################

# Contingeny table -------------------------------------------------------------
# creating a contingency table
contingeny_table <- xtabs(cycling_count ~ month_id + counting_station,
                          data = monthly_cycling_data)

# Chisq Test -------------------------------------------------------------------
# applying the test
test <- chisq.test(contingeny_table)

test


