# Meta --------------------------------------------------------------------

## Title:  Combine ACS and Medicaid Expansion Data
## Author: Genevieve DeBell
## Date Created: 4/16/25
## Date Edited:  4/16/25


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl,
               scales, gganimate, cobalt, stargazer, haven, ggthemes,
             tidyr, here, acs)

install.packages("tidycensus")
library(tidycensus)


test_data <- get_acs(
  geography = "state",
  variables = "B19013_001",
  year = 2022
)

head(test_data)
colnames(final.insurance)
# Tidy --------------------------------------------------------------------
final.data <- final.insurance %>%
  left_join(kff.final, by="State") %>%
  mutate(expand_year = year(date_adopted),
         expand = (year>=expand_year & !is.na(expand_year))) %>%
  rename(expand_ever=expanded)

write_rds(final.data,'data/output/acs_medicaid.rds')

