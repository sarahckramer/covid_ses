# ---------------------------------------------------------------------------------------------------------------------
# Process COVID-19 case data at the Landkreis-level
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
case_dat <- read_csv('data/raw/cases_rl_crowdsource_by_ags.csv')

# Check that summed data are correct:
expect_true(all(rowSums(case_dat[, 2:(dim(case_dat)[2] - 1)]) == case_dat[, dim(case_dat)[2]]))

# Remove country total:
case_dat <- case_dat %>%
  select(1:(dim(case_dat)[2] - 1))

# Are any dates missing?:
missing_dates <- check_for_missing_dates(case_dat)
# no dates missing

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

case_dat <- case_dat_new
rm(case_dat_new, pop_dat)

# Calculate rates per 100,000 population:
case_dat <- case_dat %>%
  mutate(case_rate = cases / pop * 100000, .after = cases)

# # Write data to file:
# write_csv(case_dat, file = 'data/formatted/daily_covid_cases_by_lk_CUMULATIVE.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Get weekly data

# Limit data to only rows at week ends:
week_ends <- unique(case_dat$date)[format(unique(case_dat$date), '%w') == '0']
# if (missing_dates %in% week_ends) {
#   print('Some week end dates missing!')
# }

case_dat_wk <- case_dat %>%
  filter(case_dat$date %in% week_ends)

# Add column for year and for week number:
case_dat_wk <- case_dat_wk %>%
  mutate(Year = format(date, '%Y'),
         Week = format(date, '%V'),
         .after = date) %>%
  mutate(Year = if_else(Week == 53, '2020', Year))

# # Write data to file:
# write_csv(case_dat_wk, file = 'data/formatted/weekly_covid_cases_by_lk_CUMULATIVE.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Convert to weekly cases (rather than cumulative)

# Subtract values from previous week:
case_inc <- case_dat_wk %>%
  select(date, lk, cases) %>%
  pivot_wider(names_from = lk, values_from = cases)

for (i in nrow(case_inc):2) {
  case_inc[i, 2:ncol(case_inc)] <- case_inc[i, 2:ncol(case_inc)] - case_inc[i - 1, 2:ncol(case_inc)]
}

case_inc <- case_inc %>%
  pivot_longer(!date, names_to = 'lk', values_to = 'cases') %>%
  left_join(case_dat_wk[, c('date', 'lk', 'pop')],
            by = c('date', 'lk')) %>%
  mutate(Year = format(date, '%Y'),
         Week = format(date, '%V'),
         .after = date) %>%
  mutate(Year = ifelse(Week == 53, '2020', Year)) %>%
  mutate(case_rate = cases / pop * 100000,
         .after = cases)

expect_equal(dim(case_dat_wk)[1], dim(case_inc)[1])

print(length(case_inc$cases[case_inc$cases < 0])) # 112 negative values
# mostly small values, but occasionally goes to -633 (at least 6 cases of going below -100)?

# Replace negatives with NAs:
case_inc <- case_inc %>%
  mutate(cases = ifelse(cases < 0, NA, cases))

# Recalculate case rates:
case_inc <- case_inc %>%
  mutate(case_rate = cases / pop * 100000)

# # Write data to file:
# write_csv(case_inc, file = 'data/formatted/weekly_covid_cases_by_lk_INCIDENT.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Explore data for obvious patterns/issues

# Check where cumulative data not strictly increasing over time:
not_strictly_inc <- case_dat_wk %>%
  group_by(lk) %>%
  mutate(check = cummax(cases)) %>%
  filter(cases != check) %>%
  pull(lk) %>%
  unique()
# only 88 - so sometimes occurs multiple times per LK
p1 <- ggplot(case_dat[case_dat$lk %in% not_strictly_inc, ], aes(x = date, y = cases)) +
  geom_line() + facet_wrap(~ lk, scales = 'free_y') + theme_classic() +
  labs(x = 'Date', y = 'Cumulative Cases')
print(p1)

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
