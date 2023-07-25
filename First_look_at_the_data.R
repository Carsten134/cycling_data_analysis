## Requirements ################################################################
library(tidyverse)

## Data import #################################################################
cycling_data <- read.csv2("./Data/cyclist_counts.csv")
location_data <- read.csv2("./Data/locations_of_count.csv")

## Data processing #############################################################

# how many cyclists per day
daily_cycling_data <- cycling_data %>%
  group_by(date) %>%
  reframe(date=as.Date.character(date,
                                 tryFormats = c("%d.%m.%Y")),
            deich_ost_strom_up_sum = sum(fleher.deich.ost.stromaufwaerts),
            deich_west_strom_down_sum = sum(fleher.deich.west.stromabwaerts),
            okb_north = sum(okb.nord),
            okb_south = sum(okb.sued)) %>%
  unique()



## Descriptive plots ###########################################################


# plotting the daily cycling data (still work in progress)
daily_cycling_data %>% ggplot(aes(x = date,
                                  y = deich_ost_strom_up_sum)) +
  geom_line(size = 1,
            color = "blue") +
  geom_line(aes(y = deich_west_strom_down_sum),
            size = 1,
            color = "red") +
  geom_line(size = 1,
            aes(y = okb_north),
            color= "green") +
  geom_line(size = 1,
              color = "orange",
              aes(y = okb_south))
