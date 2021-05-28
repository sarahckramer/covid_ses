# ---------------------------------------------------------------------------------------------------------------------
# Process COVID-19 mortality data at the Landkreis-level
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(testthat)

# Read in necessary functions:
source('src/data_processing_fxns.R')

# ---------------------------------------------------------------------------------------------------------------------

# Load and format daily, cumulative data

# Read in data:
mortality_dat <- read_csv('data/raw/deaths_rl_crowdsource_by_ags.csv')

# Check that summed data are correct:
expect_true(all(rowSums(mortality_dat[, 2:(dim(mortality_dat)[2] - 1)]) == mortality_dat[, dim(mortality_dat)[2]]))

# Remove country total:
mortality_dat <- mortality_dat %>%
  select(1:(dim(mortality_dat)[2] - 1))

# Are any dates missing?:
missing_dates <- check_for_missing_dates(mortality_dat)
# One date is missing (2020-03-28)

# Format data:
mortality_dat <- mortality_dat %>%
  rename(date = time_iso8601) %>%
  mutate(date = as.Date(format(date, '%Y-%m-%d'))) %>%
  filter(date != '2021-03-01') %>%
  pivot_longer(!date, names_to = 'lk', values_to = 'deaths') %>%
  mutate(lk = str_pad(lk, width = 5, side = 'left', pad = '0'))

# Check that all LKs have data for all available dates:
expect_equal(dim(mortality_dat)[1], length(unique(mortality_dat$date)) * length(unique(mortality_dat$lk)))

# ---------------------------------------------------------------------------------------------------------------------

# Incorporate population data

# Get population data by Landkreis:
pop_dat <- read_csv2('data/raw/pop_counts_12411-0015.csv', col_names = FALSE, skip = 6, n_max = 476)
# Source: https://www-genesis.destatis.de/genesis/online

pop_dat <- pop_dat %>%
  select(-X2) %>%
  rename(lk = X1, pop = X3) %>%
  mutate(pop = as.numeric(pop)) %>%
  filter(!is.na(pop))

# Join with mortality data:
mortality_dat_new <- mortality_dat %>%
  left_join(pop_dat, by = 'lk')
expect_identical(dim(mortality_dat)[1], dim(mortality_dat_new)[1])

mortality_dat <- mortality_dat_new
rm(mortality_dat_new, pop_dat)

# Calculate rates per 100,000 population:
mortality_dat <- mortality_dat %>%
  mutate(death_rate = deaths / pop * 100000, .after = deaths)

# # Write data to file:
# write_csv(mortality_dat, file = 'data/formatted/daily_covid_deaths_by_lk_CUMULATIVE.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Get weekly data

# Limit data to only rows at week ends:
week_ends <- unique(mortality_dat$date)[format(unique(mortality_dat$date), '%w') == '0']
if (missing_dates %in% week_ends) {
  print('Some week end dates missing!')
}

mortality_dat_wk <- mortality_dat %>%
  filter(mortality_dat$date %in% week_ends)

# Add column for year and for week number:
mortality_dat_wk <- mortality_dat_wk %>%
  mutate(Year = format(date, '%Y'),
         Week = format(date, '%V'),
         .after = date) %>%
  mutate(Year = if_else(Week == 53, '2020', Year))

# # Write data to file:
# write_csv(mortality_dat_wk, file = 'data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Convert to weekly deaths (rather than cumulative)
# Try two methods, check for equivalence

# METHOD 1: Subtract from previous week:
mortality_inc1 <- convert_to_incident(mortality_dat_wk)

# METHOD 2: Get daily deaths, then add over week:
mortality_inc2 <- convert_to_incident(mortality_dat)
mortality_inc2 <- mortality_inc2 %>%
  group_by(lk, Week) %>%
  summarise(deaths = sum(deaths))

# Compare the two estimates:
mortality_inc <- mortality_inc1 %>%
  left_join(mortality_inc2, by = c('Week', 'lk')) %>%
  rename(death_rate.x = death_rate) %>%
  mutate(death_rate.y = deaths.y / pop * 100000)
expect_true(all.equal(mortality_inc$deaths.x, mortality_inc$deaths.y))

# Estimates are the same - keep only one data frame:
mortality_inc <- mortality_inc1
rm(mortality_inc1, mortality_inc2)
print(length(mortality_inc$deaths[mortality_inc$deaths < 0])) # 17 negative values
# no clear geographic pattern to these, and most have abs. val. < 10

# Replace negatives with NAs:
mortality_inc <- mortality_inc %>%
  mutate(deaths = ifelse(deaths < 0, NA, deaths))

# Recalculate death rates:
mortality_inc <- mortality_inc %>%
  mutate(death_rate = deaths / pop * 100000)

# # Write data to file:
# write_csv(mortality_inc, file = 'data/formatted/weekly_covid_deaths_by_lk_INCIDENT.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Explore data for obvious patterns/issues

# Check for obvious weekend effect:
p1 <- ggplot(data = mortality_dat, aes(x = date, y = death_rate, group = lk)) +
  geom_line() + geom_vline(xintercept = week_ends) + theme_classic() +
  labs(x = 'Date', y = 'Cumulative Deaths (per 100,000) by Landkreis')
print(p1)
# There seems to be evidence of fewer deaths being reported over the weekend

# Check where cumulative data not strictly increasing over time:
not_strictly_inc <- mortality_dat_wk %>%
  group_by(lk) %>%
  mutate(check = cummax(deaths)) %>%
  filter(deaths != check) %>%
  pull(lk) %>%
  unique()
p2 <- ggplot(mortality_dat[mortality_dat$lk %in% not_strictly_inc, ], aes(x = date, y = deaths)) +
  geom_line() + facet_wrap(~ lk, scales = 'free_y') + theme_classic() +
  labs(x = 'Date', y = 'Cumulative Deaths')
print(p2)

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
