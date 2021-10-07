# ---------------------------------------------------------------------------------------------------------------------
# Add case data to mortality data frame(s), to act as offset
# ---------------------------------------------------------------------------------------------------------------------

# Load libraries:
library(tidyverse)

# Read in all data:
deaths_c <- read_csv('data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE.csv')
deaths_i <- read_csv('data/formatted/weekly_covid_deaths_by_lk_INCIDENT.csv')
deaths_cdp <- read_csv('data/formatted/weekly_covid_deaths_by_lk_CDP.csv')
cases_c <- read_csv('data/formatted/weekly_covid_cases_by_lk_CUMULATIVE.csv')
cases_i <- read_csv('data/formatted/weekly_covid_cases_by_lk_INCIDENT.csv')
cases_cdp <- read_csv('data/formatted/weekly_covid_cases_by_lk_CDP.csv')

deaths_c_mo <- read_csv('data/formatted/monthly_covid_deaths_by_lk_CUMULATIVE.csv')
deaths_i_mo <- read_csv('data/formatted/monthly_covid_deaths_by_lk_INCIDENT.csv')
deaths_cdp_mo <- read_csv('data/formatted/monthly_covid_deaths_by_lk_CDP.csv')
cases_c_mo <- read_csv('data/formatted/monthly_covid_cases_by_lk_CUMULATIVE.csv')
cases_i_mo <- read_csv('data/formatted/monthly_covid_cases_by_lk_INCIDENT.csv')
cases_cdp_mo <- read_csv('data/formatted/monthly_covid_cases_by_lk_CDP.csv')

# Reduce case data frame to needed columns:
cases_c <- cases_c %>%
  select(date, lk, cases:case_rate)
cases_i <- cases_i %>%
  select(date, lk, cases:case_rate)
cases_cdp <- cases_cdp %>%
  select(Year:case_rate)

cases_c_mo <- cases_c_mo %>%
  select(date, lk, cases:case_rate)
cases_i_mo <- cases_i_mo %>%
  select(date, lk, cases:case_rate)
cases_cdp_mo <- cases_cdp_mo %>%
  select(Year:case_rate)

# Combine case data into death data frames:
deaths_c <- deaths_c %>%
  left_join(cases_c, by = c('date', 'lk')) %>%
  select(date:death_rate, cases:case_rate, pop)
deaths_i <- deaths_i %>%
  left_join(cases_i, by = c('date', 'lk')) %>%
  select(date:death_rate, cases:case_rate, pop)
deaths_cdp <- deaths_cdp %>%
  left_join(cases_cdp, by = c('Year', 'Week', 'lk')) %>%
  select(Year:death_rate, cases:case_rate, pop)

deaths_c_mo <- deaths_c_mo %>%
  left_join(cases_c_mo, by = c('date', 'lk')) %>%
  select(date:death_rate, cases:case_rate, pop)
deaths_i_mo <- deaths_i_mo %>%
  left_join(cases_i_mo, by = c('date', 'lk')) %>%
  select(date:death_rate, cases:case_rate, pop)
deaths_cdp_mo <- deaths_cdp_mo %>%
  left_join(cases_cdp_mo, by = c('Year', 'Month', 'lk')) %>%
  select(Year:death_rate, cases:case_rate, pop)

rm(cases_c, cases_i, cases_cdp, cases_c_mo, cases_i_mo, cases_cdp_mo)

# Check that case counts always greater than death counts:
deaths_c %>% filter(deaths > cases) %>% nrow() # none

deaths_i %>% filter(deaths > cases) %>% nrow() # 176
deaths_i %>% filter(deaths > cases) %>% pull(Week) %>% table()
# Many appear to be in the summer, when data likely to be less reliable

deaths_cdp %>% filter(deaths > cases) %>% nrow() # none

deaths_c_mo %>% filter(deaths > cases) %>% nrow() # none

deaths_i_mo %>% filter(deaths > cases) %>% nrow() # 18
deaths_i_mo %>% filter(deaths > cases) %>% pull(Month) %>% table() # all May-July

deaths_cdp_mo %>% filter(deaths > cases) %>% nrow() # none

# Calculate infection fatality rate:
deaths_c <- deaths_c %>%
  mutate(ifr = deaths / cases * 100,
         .after = death_rate)
deaths_i <- deaths_i %>%
  mutate(ifr = deaths / cases * 100,
         .after = death_rate)
deaths_cdp <- deaths_cdp %>%
  mutate(ifr = deaths / cases * 100,
         .after = death_rate)

deaths_c_mo <- deaths_c_mo %>%
  mutate(ifr = deaths / cases * 100,
         .after = death_rate)
deaths_i_mo <- deaths_i_mo %>%
  mutate(ifr = deaths / cases * 100,
         .after = death_rate)
deaths_cdp_mo <- deaths_cdp_mo %>%
  mutate(ifr = deaths / cases * 100,
         .after = death_rate)

# Write data to file:
write_csv(deaths_c, file = 'data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE.csv')
write_csv(deaths_i, file = 'data/formatted/weekly_covid_deaths_by_lk_INCIDENT.csv')
write_csv(deaths_cdp, file = 'data/formatted/weekly_covid_deaths_by_lk_CDP.csv')
write_csv(deaths_c_mo, file = 'data/formatted/monthly_covid_deaths_by_lk_CUMULATIVE.csv')
write_csv(deaths_i_mo, file = 'data/formatted/monthly_covid_deaths_by_lk_INCIDENT.csv')
write_csv(deaths_cdp_mo, file = 'data/formatted/monthly_covid_deaths_by_lk_CDP.csv')

# Clean up:
rm(list = ls())
