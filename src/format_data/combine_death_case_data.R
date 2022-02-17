# ---------------------------------------------------------------------------------------------------------------------
# Add case data to mortality data frame(s), to act as offset
# ---------------------------------------------------------------------------------------------------------------------

# Load libraries:
library(tidyverse)

# Read in all data:
deaths_cdp <- read_csv('data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')
deaths_cdp_i <- read_csv('data/formatted/weekly_covid_deaths_by_lk_INCIDENT_CDP.csv')
deaths_stand <- read_csv('data/formatted/STAND_weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')

cases_cdp <- read_csv('data/formatted/weekly_covid_cases_by_lk_CUMULATIVE_CDP.csv')
cases_cdp_i <- read_csv('data/formatted/weekly_covid_cases_by_lk_INCIDENT_CDP.csv')
cases_stand <- read_csv('data/formatted/STAND_weekly_covid_cases_by_lk_CUMULATIVE_CDP.csv')

# Reduce case data frame to needed columns:
cases_cdp <- cases_cdp %>%
  select(date, lk, cases:case_rate)
cases_cdp_i <- cases_cdp_i %>%
  select(Year:case_rate)
cases_stand <- cases_stand %>%
  select(date, lk, cases:case_rate)

# Combine case data into death data frames:
deaths_cdp <- deaths_cdp %>%
  left_join(cases_cdp, by = c('date', 'lk')) %>%
  select(ags2:death_rate, cases:case_rate, pop)
deaths_cdp_i <- deaths_cdp_i %>%
  left_join(cases_cdp_i, by = c('Year', 'Week', 'lk')) %>%
  select(Year:death_rate, cases:case_rate, pop)
deaths_stand <- deaths_stand %>%
  left_join(cases_stand, by = c('date', 'lk')) %>%
  select(ags2:Week, deaths:case_rate, pop)
rm(cases_cdp, cases_cdp_i, cases_stand)

# Check that case counts always greater than death counts:
deaths_cdp %>% filter(deaths > cases) %>% nrow() # none
deaths_cdp_i %>% filter(deaths > cases) %>% nrow() # none
deaths_stand %>% filter(deaths > cases) %>% nrow() # none

# Calculate case fatality rate:
deaths_cdp <- deaths_cdp %>%
  mutate(cfr = deaths / cases * 100,
         .after = death_rate)
deaths_cdp_i <- deaths_cdp_i %>%
  mutate(cfr = deaths / cases * 100,
         .after = death_rate)
deaths_stand <- deaths_stand %>%
  mutate(cfr = deaths / cases * 100,
         .after = death_rate)

# Write data to file:
write_csv(deaths_cdp, file = 'data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')
write_csv(deaths_cdp_i, file = 'data/formatted/weekly_covid_deaths_by_lk_INCIDENT_CDP.csv')
write_csv(deaths_stand, file = 'data/formatted/STAND_weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')

# Clean up:
rm(list = ls())
