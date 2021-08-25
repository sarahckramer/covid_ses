# ---------------------------------------------------------------------------------------------------------------------
# Add case data to mortality data frame(s), to act as offset
# ---------------------------------------------------------------------------------------------------------------------

# Load libraries:
library(tidyverse)

# Read in all data:
deaths_c <- read_csv('data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE.csv')
deaths_i <- read_csv('data/formatted/weekly_covid_deaths_by_lk_INCIDENT.csv')
cases_c <- read_csv('data/formatted/weekly_covid_cases_by_lk_CUMULATIVE.csv')
cases_i <- read_csv('data/formatted/weekly_covid_cases_by_lk_INCIDENT.csv')

# Reduce case data frame to needed columns:
cases_c <- cases_c %>%
  select(date, lk, cases:case_rate)
cases_i <- cases_i %>%
  select(date, lk, cases:case_rate)

# Combine case data into death data frames:
deaths_c <- deaths_c %>%
  left_join(cases_c, by = c('date', 'lk')) %>%
  select(date:death_rate, cases:case_rate, pop)
deaths_i <- deaths_i %>%
  left_join(cases_i, by = c('date', 'lk')) %>%
  select(date:death_rate, cases:case_rate, pop)

rm(cases_c, cases_i)

# Check that case counts always greater than death counts:
deaths_c %>% filter(deaths > cases) %>% dim() # none

deaths_i %>% filter(deaths > cases) %>% dim() # 122
deaths_i %>% filter(deaths > cases) %>% pull(Week) %>% table()
# All in first wave (63), or summer (between waves)
# Use the cases from the same week, or from the previous week or two? Many people won't die in the same week they become infected
# For now, focus on second wave and leave alone

# Write data to file:
write_csv(deaths_c, file = 'data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE.csv')
write_csv(deaths_i, file = 'data/formatted/weekly_covid_deaths_by_lk_INCIDENT.csv')

# Clean up:
rm(list = ls())
