# ---------------------------------------------------------------------------------------------------------------------
# Process COVID-19 case data at the Landkreis-level
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(testthat)

# Read in necessary functions:
source('src/functions/data_processing_fxns.R')

# ---------------------------------------------------------------------------------------------------------------------

# Load and format daily, cumulative data (crowdsourced)

# Read in data:
case_dat <- read_csv('data/raw/cases_rl_crowdsource_by_ags.csv')

# Check that summed data are correct:
expect_true(all(rowSums(case_dat[, 2:(dim(case_dat)[2] - 1)]) == case_dat[, dim(case_dat)[2]]))

# Remove country total:
case_dat <- case_dat %>%
  select(1:(dim(case_dat)[2] - 1))

# Are any dates missing?:
missing_dates <- check_for_missing_dates(case_dat, dat_source = 'crowdsource')

# Format data:
case_dat <- case_dat %>%
  rename(date = time_iso8601) %>%
  mutate(date = as.Date(format(date, '%Y-%m-%d'))) %>%
  # filter(date != '2021-03-01') %>%
  pivot_longer(!date, names_to = 'lk', values_to = 'cases') %>%
  mutate(lk = str_pad(lk, width = 5, side = 'left', pad = '0'))

# Check that all LKs have data for all available dates:
expect_equal(dim(case_dat)[1], length(unique(case_dat$date)) * length(unique(case_dat$lk)))

# ---------------------------------------------------------------------------------------------------------------------

# Load and format Corona Daten Plattform data

# Read in data:
cdp_dat <- read_csv('data/raw/cdp/infektionen.csv')

# Check for missing dates:
missing_dates <- check_for_missing_dates(cdp_dat, dat_source = 'cdp')

# Format data (incident):
cdp_dat_inc <- cdp_dat %>%
  filter(variable == 'kr_inf_md') %>%
  select(-c(`_id`, ags2, bundesland, kreis, variable)) %>%
  pivot_longer(-ags5, names_to = 'date', values_to = 'cases') %>%
  mutate(date = as.Date(str_sub(date, 2, 9), format = '%Y%m%d')) %>%
  rename('lk' = 'ags5')

# Format data (cumulative):
cdp_dat <- cdp_dat %>%
  filter(variable == 'kr_inf_md_kum') %>%
  select(-c(`_id`, kreis, variable)) %>%
  pivot_longer(-c(ags2, bundesland, ags5), names_to = 'date', values_to = 'cases') %>%
  mutate(date = as.Date(str_sub(date, 2, 9), format = '%Y%m%d')) %>%
  rename('lk' = 'ags5') %>%
  unique()

# Check that all LKs have data for all available dates:
expect_equal(dim(cdp_dat)[1], length(unique(cdp_dat$date)) * length(unique(cdp_dat$lk)))

# ---------------------------------------------------------------------------------------------------------------------

# Incorporate population data

# Get population data by Landkreis:
pop_dat <- read_csv2('data/raw/independent_vars/pop_counts_12411-0015.csv', col_names = FALSE, skip = 6, n_max = 476)
# Source: https://www-genesis.destatis.de/genesis/online

pop_dat <- pop_dat %>%
  select(-X2) %>%
  rename(lk = X1, pop = X3) %>%
  mutate(pop = as.numeric(pop)) %>%
  filter(!is.na(pop))

# Join with case data:
case_dat_new <- case_dat %>%
  left_join(pop_dat, by = 'lk')
expect_identical(dim(case_dat)[1], dim(case_dat_new)[1])

cdp_dat_new <- cdp_dat %>%
  left_join(pop_dat, by = 'lk')
expect_identical(dim(cdp_dat)[1], dim(cdp_dat_new)[1])

cdp_dat_inc_new <- cdp_dat_inc %>%
  left_join(pop_dat, by = 'lk')
expect_identical(dim(cdp_dat_inc)[1], dim(cdp_dat_inc_new)[1])

case_dat <- case_dat_new
cdp_dat <- cdp_dat_new
cdp_dat_inc <- cdp_dat_inc_new
rm(case_dat_new, cdp_dat_new, cdp_dat_inc_new, pop_dat)

# Calculate rates per 100,000 population:
case_dat <- case_dat %>%
  mutate(case_rate = cases / pop * 100000, .after = cases)
cdp_dat <- cdp_dat %>%
  mutate(case_rate = cases / pop * 100000, .after = cases)
cdp_dat_inc <- cdp_dat_inc %>%
  mutate(case_rate = cases / pop * 100000, .after = cases)

# Write data to file:
write_csv(case_dat, file = 'data/formatted/daily_covid_cases_by_lk_CUMULATIVE.csv')
write_csv(cdp_dat, file = 'data/formatted/daily_covid_cases_by_lk_CUMULATIVE_CDP.csv')
write_csv(cdp_dat_inc, file = 'data/formatted/daily_covid_cases_by_lk_INCIDENT_CDP.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Get weekly data (cumulative)

# Limit data to only rows at week ends:
week_ends <- unique(cdp_dat$date)[format(unique(cdp_dat$date), '%w') == '0']
# if (missing_dates %in% week_ends) {
#   print('Some week end dates missing!')
# }

case_dat_wk <- case_dat %>%
  filter(case_dat$date %in% week_ends)
cdp_dat_wk <- cdp_dat %>%
  filter(cdp_dat$date %in% week_ends)

# Add column for year and for week number:
case_dat_wk <- case_dat_wk %>%
  mutate(Year = format(date, '%Y'),
         Week = format(date, '%V'),
         .after = date) %>%
  mutate(Year = if_else(Week == 53, '2020', Year))
cdp_dat_wk <- cdp_dat_wk %>%
  mutate(Year = format(date, '%Y'),
         Week = format(date, '%V'),
         .after = date) %>%
  mutate(Year = if_else(Week == 53, '2020', Year))

# Write data to file:
write_csv(case_dat_wk, file = 'data/formatted/weekly_covid_cases_by_lk_CUMULATIVE.csv')
write_csv(cdp_dat_wk, file = 'data/formatted/weekly_covid_cases_by_lk_CUMULATIVE_CDP.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Get weekly data (incident CDP data)

# Add week and year numbers:
cdp_dat_inc_wk <- cdp_dat_inc %>%
  mutate(Week = format(date, '%V'),
         Year = format(date, '%Y'),
         Year = if_else(Week == 53, '2020', Year),
         year_week = paste(Year, Week, sep = '_'))

# Remove incomplete weeks:
dates_to_remove <- cdp_dat_inc_wk %>%
  group_by(lk, year_week) %>%
  summarise(len = length(cases)) %>%
  filter(len < 7,
         year_week != '2020_09') %>%
  ungroup() %>%
  select(-c(lk, len)) %>%
  unique()

cdp_dat_inc_wk <- cdp_dat_inc_wk %>%
  filter(!(year_week %in% dates_to_remove$year_week))

# Sum over each LK/week:
cdp_dat_inc_wk <- cdp_dat_inc_wk %>%
  group_by(lk, Year, Week, pop) %>%
  summarise(cases = sum(cases)) %>%
  mutate(case_rate = cases / pop * 100000) %>%
  ungroup() %>%
  select(Year:Week, lk, cases:case_rate, pop)

# Write data to file:
write_csv(cdp_dat_inc_wk, file = 'data/formatted/weekly_covid_cases_by_lk_INCIDENT_CDP.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Explore data for obvious patterns/issues

# Check where cumulative data not strictly increasing over time:
not_strictly_inc <- cdp_dat_wk %>%
  group_by(lk) %>%
  mutate(check = cummax(cases)) %>%
  filter(cases != check) %>%
  pull(lk) %>%
  unique()

# ---------------------------------------------------------------------------------------------------------------------

# Compare two data sources

# Join data sets:
cases_all <- case_dat_wk %>%
  left_join(cdp_dat_wk, by = c('lk', 'Year', 'Week'))
expect_true(all.equal(cases_all$pop.x, cases_all$pop.y))

# Format:
cases_all <- cases_all %>%
  rename('date' = 'date.x',
         'cases_cs' = 'cases.x',
         'cases_cdp' = 'cases.y') %>%
  select(date, lk, cases_cs, cases_cdp) %>%
  pivot_longer(cases_cs:cases_cdp,
               names_to = 'Source',
               values_to = 'cases')

# Plot:
p1 <- ggplot(data = cases_all, aes(x = date, y = cases, col = Source, lty = Source)) +
  geom_line() + facet_wrap(~ lk, scales = 'free_y') + theme_classic()
# print(p1)

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
