## Requirements ################################################################
# R version: 4.3.1
library(tidyverse)


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
contingency_table <- xtabs(cycling_count ~ month_id + counting_station,
                          data = monthly_cycling_data)



# Chisq Test -------------------------------------------------------------------
# applying the test
test <- chisq.test(contingency_table)



# CramersV ---------------------------------------------------------------------
# computing V manually
sqrt(test$statistic/(sum(monthly_cycling_data$cycling_count)*3))
# X-squared 
# 0.0638362

# Correlation pairs ------------------------------------------------------------

pair_correlation <- function(station_1, station_2) {
  # preventing lazy loading in R
  force(station_1)
  force(station_2)
  
  data <- monthly_cycling_data %>%
    filter(counting_station %in% c(station_1, station_2))
  
  if (length(data$cycling_count) == 0) {
    return(NA)
  }
  
  contingency_table <- xtabs(cycling_count ~ month_id + counting_station,
                             data = data)
  
  test <- chisq.test(contingency_table)
  
  return(sqrt(test$statistic/(sum(monthly_cycling_data$cycling_count)*3)))
}


# building the data-set
counting_stations <- factor(unique(monthly_cycling_data$counting_station))
counting_stations_first_col <- c(rep(counting_stations[1],4),
                                 rep(counting_stations[2],4),
                                 rep(counting_stations[3],4),
                                 rep(counting_stations[4],4))


pairs_cor_data <- data.frame(station_1 = counting_stations_first_col,
                             station_2 = rep(counting_stations, 4),
                             correlation = rep(0, 4*4))


for (i in 1:16) {
  pairs_cor_data$correlation[i] <- pair_correlation(pairs_cor_data$station_1[i],
                                                    pairs_cor_data$station_2[i])
}


# plotting dataframe with heatmap ----------------------------------------------
theme_set(theme_minimal())


pairs_cor_data %>%
  ggplot(aes(station_1, station_2,
             fill = correlation)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high="red") +
  xlab("Dauerzählstation") +
  ylab("Dauerzählstation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
