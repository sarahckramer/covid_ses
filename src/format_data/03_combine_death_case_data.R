# ---------------------------------------------------------------------------------------------------------------------
# Add case data to mortality data frame(s), to act as offset
# ---------------------------------------------------------------------------------------------------------------------

# Load libraries:
library(tidyverse)

# Read in all data:
deaths_cdp_inc <- read_csv('data/formatted/weekly_covid_deaths_by_lk_INCIDENT_CDP.csv')
deaths_stand <- read_csv('data/formatted/STAND_weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')
deaths_agestrat <- read_csv('data/formatted/BYAGE_weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')

cases_cdp_inc <- read_csv('data/formatted/weekly_covid_cases_by_lk_INCIDENT_CDP.csv')
cases_stand <- read_csv('data/formatted/STAND_weekly_covid_cases_by_lk_CUMULATIVE_CDP.csv')
cases_agestrat <- read_csv('data/formatted/BYAGE_weekly_covid_cases_by_lk_CUMULATIVE_CDP.csv')

# Reduce case data frame to needed columns:
cases_cdp_inc <- cases_cdp_inc %>%
  select(Year:case_rate)
cases_stand <- cases_stand %>%
  select(date, lk, cases:case_rate)
cases_agestrat <- cases_agestrat %>%
  select(date, lk, age, cases:case_rate)

# Combine case data into death data frames:
deaths_cdp_inc <- deaths_cdp_inc %>%
  left_join(cases_cdp_inc, by = c('Year', 'Week', 'lk')) %>%
  select(Year:death_rate, cases:case_rate, pop)
deaths_stand <- deaths_stand %>%
  left_join(cases_stand, by = c('date', 'lk')) %>%
  select(ags2:Week, deaths:case_rate, pop)
deaths_agestrat <- deaths_agestrat %>%
  left_join(cases_agestrat, by = c('date', 'lk', 'age')) %>%
  select(ags2:Week, deaths:case_rate, pop)
rm(cases_cdp_inc, cases_stand, cases_agestrat)

# Check that case counts always greater than death counts:
deaths_cdp_inc %>% filter(deaths > cases) %>% nrow() # none
deaths_stand %>% filter(deaths > cases) %>% nrow() # none
deaths_agestrat %>% filter(deaths > cases) %>% nrow() # none

# Calculate case fatality rate:
deaths_cdp_inc <- deaths_cdp_inc %>%
  mutate(cfr = deaths / cases * 100,
         .after = death_rate)
deaths_stand <- deaths_stand %>%
  mutate(cfr = deaths / cases * 100,
         .after = death_rate)
deaths_agestrat <- deaths_agestrat %>%
  mutate(cfr = deaths / cases * 100,
         .after = death_rate)

# Write data to file:
write_csv(deaths_cdp_inc, file = 'data/formatted/weekly_covid_deaths_by_lk_INCIDENT_CDP.csv')
write_csv(deaths_stand, file = 'data/formatted/STAND_weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')
write_csv(deaths_agestrat, file = 'data/formatted/BYAGE_weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')

# Clean up:
rm(list = ls())
