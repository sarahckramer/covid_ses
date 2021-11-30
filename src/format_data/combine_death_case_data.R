# ---------------------------------------------------------------------------------------------------------------------
# Add case data to mortality data frame(s), to act as offset
# ---------------------------------------------------------------------------------------------------------------------

# Load libraries:
library(tidyverse)

# Read in all data:
deaths_c <- read_csv('data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE.csv')
deaths_cdp <- read_csv('data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')
deaths_cdp_i <- read_csv('data/formatted/weekly_covid_deaths_by_lk_INCIDENT_CDP.csv')

cases_c <- read_csv('data/formatted/weekly_covid_cases_by_lk_CUMULATIVE.csv')
cases_cdp <- read_csv('data/formatted/weekly_covid_cases_by_lk_CUMULATIVE_CDP.csv')
cases_cdp_i <- read_csv('data/formatted/weekly_covid_cases_by_lk_INCIDENT_CDP.csv')

# Reduce case data frame to needed columns:
cases_c <- cases_c %>%
  select(date, lk, cases:case_rate)
cases_cdp <- cases_cdp %>%
  select(date, lk, cases:case_rate)
cases_cdp_i <- cases_cdp_i %>%
  select(Year:case_rate)

# Combine case data into death data frames:
deaths_c <- deaths_c %>%
  left_join(cases_c, by = c('date', 'lk')) %>%
  select(date:death_rate, cases:case_rate, pop)
deaths_cdp <- deaths_cdp %>%
  left_join(cases_cdp, by = c('date', 'lk')) %>%
  select(ags2:death_rate, cases:case_rate, pop)
deaths_cdp_i <- deaths_cdp_i %>%
  left_join(cases_cdp_i, by = c('Year', 'Week', 'lk')) %>%
  select(Year:death_rate, cases:case_rate, pop)

rm(cases_c, cases_cdp, cases_cdp_i)

# Check that case counts always greater than death counts:
deaths_c %>% filter(deaths > cases) %>% nrow() # none
deaths_cdp %>% filter(deaths > cases) %>% nrow() # none
deaths_cdp_i %>% filter(deaths > cases) %>% nrow() # none

# Calculate infection fatality rate:
deaths_c <- deaths_c %>%
  mutate(ifr = deaths / cases * 100,
         .after = death_rate)
deaths_cdp <- deaths_cdp %>%
  mutate(ifr = deaths / cases * 100,
         .after = death_rate)
deaths_cdp_i <- deaths_cdp_i %>%
  mutate(ifr = deaths / cases * 100,
         .after = death_rate)

# Write data to file:
write_csv(deaths_c, file = 'data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE.csv')
write_csv(deaths_cdp, file = 'data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')
write_csv(deaths_cdp_i, file = 'data/formatted/weekly_covid_deaths_by_lk_INCIDENT_CDP.csv')

# Clean up:
rm(list = ls())
