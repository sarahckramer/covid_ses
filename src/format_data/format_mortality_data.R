# ---------------------------------------------------------------------------------------------------------------------
# Process COVID-19 mortality data at the Landkreis-level
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(testthat)

# Read in necessary functions:
source('src/functions/data_processing_fxns.R')

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
  # filter(date != '2021-03-01') %>%
  pivot_longer(!date, names_to = 'lk', values_to = 'deaths') %>%
  mutate(lk = str_pad(lk, width = 5, side = 'left', pad = '0'))

# Check that all LKs have data for all available dates:
expect_equal(dim(mortality_dat)[1], length(unique(mortality_dat$date)) * length(unique(mortality_dat$lk)))

# ---------------------------------------------------------------------------------------------------------------------

# Load and format Corona Daten Plattform data

# Read in data:
cdp_dat <- read_csv('data/raw/cdp/todesfaelle.csv')

# Format data:
cdp_dat <- cdp_dat %>%
  filter(variable == 'kr_tod_md') %>%
  select(-c(`_id`, ags2, bundesland, kreis, variable)) %>%
  pivot_longer(-ags5, names_to = 'date', values_to = 'deaths') %>%
  mutate(date = as.Date(str_sub(date, 2, 9), format = '%Y%m%d')) %>%
  rename('lk' = 'ags5')

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

# Join with mortality data:
mortality_dat_new <- mortality_dat %>%
  left_join(pop_dat, by = 'lk')
expect_identical(dim(mortality_dat)[1], dim(mortality_dat_new)[1])

cdp_dat_new <- cdp_dat %>%
  left_join(pop_dat, by = 'lk')
expect_identical(dim(cdp_dat)[1], dim(cdp_dat_new)[1])

mortality_dat <- mortality_dat_new
cdp_dat <- cdp_dat_new
rm(mortality_dat_new, cdp_dat_new, pop_dat)

# Calculate rates per 100,000 population:
mortality_dat <- mortality_dat %>%
  mutate(death_rate = deaths / pop * 100000, .after = deaths)

# Write data to file:
write_csv(mortality_dat, file = 'data/formatted/daily_covid_deaths_by_lk_CUMULATIVE.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Get weekly data (crowdsourced data)

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

# Write data to file:
write_csv(mortality_dat_wk, file = 'data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Get weekly data (CDP data)

# Add week and year numbers:
cdp_dat_wk <- cdp_dat %>%
  mutate(Week = format(date, '%V'),
         Year = format(date, '%Y'),
         Year = if_else(Week == 53, '2020', Year),
         year_week = paste(Year, Week, sep = '_'))

# Remove incomplete weeks:
dates_to_remove <- cdp_dat_wk %>%
  group_by(lk, year_week) %>%
  summarise(len = length(deaths)) %>%
  filter(len < 7) %>%
  ungroup() %>%
  select(-c(lk, len)) %>%
  unique()

cdp_dat_wk <- cdp_dat_wk %>%
  filter(!(year_week %in% dates_to_remove$year_week))

# Sum over each LK/week:
cdp_dat_wk <- cdp_dat_wk %>%
  group_by(lk, Year, Week, pop) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(death_rate = deaths / pop * 100000) %>%
  ungroup() %>%
  select(Year:Week, lk, deaths:death_rate, pop)

# Write data to file:
write_csv(cdp_dat_wk, file = 'data/formatted/weekly_covid_deaths_by_lk_CDP.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Get monthly data (crowdsourced data)

# Limit data to only rows at month ends:
month_ends <- mortality_dat %>%
  mutate(year = format(date, '%Y'),
         month = format(date, '%m')) %>%
  group_by(year, month) %>%
  summarise(date = max(date)) %>%
  pull(date) %>%
  unname()
# remove last month if incomplete?
month_ends <- month_ends[1:(length(month_ends) - 1)]

mortality_dat_mo <- mortality_dat %>%
  filter(mortality_dat$date %in% month_ends)

# Add column for year and for month number:
mortality_dat_mo <- mortality_dat_mo %>%
  mutate(Year = format(date, '%Y'),
         Month = format(date, '%m'),
         .after = date)

# Write data to file:
write_csv(mortality_dat_mo, file = 'data/formatted/monthly_covid_deaths_by_lk_CUMULATIVE.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Get monthly data (CDP data)

# Add month and year numbers:
cdp_dat_mo <- cdp_dat %>%
  mutate(Month = format(date, '%m'),
         Year = format(date, '%Y'),
         year_month = paste(Year, Month, sep = '_'))

# Remove incomplete months:
if (format(max(cdp_dat_mo$date), '%m') == format((max(cdp_dat_mo$date) + 1), '%m')) {
  cdp_dat_mo <- cdp_dat_mo %>%
    filter(!(Year == format(max(cdp_dat_mo$date), '%Y') &
               Month == format(max(cdp_dat_mo$date), '%m')))
}

# Sum over each LK/month:
cdp_dat_mo <- cdp_dat_mo %>%
  group_by(lk, Year, Month, pop) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(death_rate = deaths / pop * 100000) %>%
  ungroup() %>%
  select(Year:Month, lk, deaths:death_rate, pop)

# Write data to file:
write_csv(cdp_dat_mo, file = 'data/formatted/monthly_covid_deaths_by_lk_CDP.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Convert to weekly deaths (rather than cumulative)
# Try two methods, check for equivalence

# METHOD 1: Subtract from previous week:
mortality_inc1 <- convert_to_incident(mortality_dat_wk)

# METHOD 2: Get daily deaths, then add over week:
mortality_inc2 <- convert_to_incident(mortality_dat)
mortality_inc2 <- mortality_inc2 %>%
  group_by(lk, Week, Year) %>%
  summarise(deaths = sum(deaths))

# Compare the two estimates:
mortality_inc <- mortality_inc1 %>%
  left_join(mortality_inc2, by = c('Week', 'Year', 'lk')) %>%
  rename(death_rate.x = death_rate) %>%
  mutate(death_rate.y = deaths.y / pop * 100000)
expect_true(all.equal(mortality_inc$deaths.x, mortality_inc$deaths.y))

# Estimates are the same - keep only one data frame:
mortality_inc <- mortality_inc1
rm(mortality_inc1, mortality_inc2)
print(length(mortality_inc$deaths[mortality_inc$deaths < 0])) # 54 negative values
# no clear geographic pattern to these, and most have abs. val. < 10

# Replace negatives with NAs:
mortality_inc <- mortality_inc %>%
  mutate(deaths = ifelse(deaths < 0, NA, deaths))

# Recalculate death rates:
mortality_inc <- mortality_inc %>%
  mutate(death_rate = deaths / pop * 100000)

# Write data to file:
write_csv(mortality_inc, file = 'data/formatted/weekly_covid_deaths_by_lk_INCIDENT.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Convert to monthly cases (rather than cumulative)

# Subtract values from previous month:
mortality_inc_mo <- mortality_dat_mo %>%
  select(date, lk, deaths) %>%
  pivot_wider(names_from = lk, values_from = deaths)

for (i in nrow(mortality_inc_mo):2) {
  mortality_inc_mo[i, 2:ncol(mortality_inc_mo)] <- mortality_inc_mo[i, 2:ncol(mortality_inc_mo)] - mortality_inc_mo[i - 1, 2:ncol(mortality_inc_mo)]
}

mortality_inc_mo <- mortality_inc_mo %>%
  pivot_longer(!date, names_to = 'lk', values_to = 'deaths') %>%
  left_join(mortality_dat_mo[, c('date', 'lk', 'pop')],
            by = c('date', 'lk')) %>%
  mutate(Year = format(date, '%Y'),
         Month = format(date, '%m'),
         .after = date) %>%
  mutate(death_rate = deaths / pop * 100000,
         .after = deaths)

expect_equal(dim(mortality_dat_mo)[1], dim(mortality_inc_mo)[1])
print(length(mortality_inc_mo$deaths[mortality_inc_mo$deaths < 0])) # 10 negative values
# typically relatively small - never less than -10

# Replace negatives with NAs:
mortality_inc_mo <- mortality_inc_mo %>%
  mutate(deaths = ifelse(deaths < 0, NA, deaths))

# Recalculate case rates:
mortality_inc_mo <- mortality_inc_mo %>%
  mutate(death_rate = deaths / pop * 100000)

# Write data to file:
write_csv(mortality_inc_mo, file = 'data/formatted/monthly_covid_deaths_by_lk_INCIDENT.csv')

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

not_strictly_inc <- mortality_dat_mo %>%
  group_by(lk) %>%
  mutate(check = cummax(deaths)) %>%
  filter(deaths != check) %>%
  pull(lk) %>%
  unique()
p3 <- ggplot(mortality_dat[mortality_dat$lk %in% not_strictly_inc, ], aes(x = date, y = deaths)) +
  geom_line() + facet_wrap(~ lk, scales = 'free_y') + theme_classic() +
  labs(x = 'Date', y = 'Cumulative Deaths')
print(p3)

# ---------------------------------------------------------------------------------------------------------------------

# Compare two data sources

# Join data sets:
mortality_all <- mortality_inc %>%
  left_join(cdp_dat_wk, by = c('lk', 'Year', 'Week'))
expect_true(all.equal(mortality_all$pop.x, mortality_all$pop.y))

# Format:
mortality_all <- mortality_all %>%
  select(date, lk, deaths.x, deaths.y) %>%
  rename('deaths_cs' = 'deaths.x',
         'deaths_cdp' = 'deaths.y') %>%
  pivot_longer(deaths_cs:deaths_cdp,
               names_to = 'Source',
               values_to = 'deaths')

# Plot:
p4 <- ggplot(data = mortality_all, aes(x = date, y = deaths, col = Source, lty = Source)) +
  geom_line() + facet_wrap(~ lk, scales = 'free_y') + theme_classic()
# print(p4)
# Less similar than case data, but still relatively close; however, it does seem like CDP data tend to rise/peak before
# the crowdsourced data, as expected if the reporting dates are when the case was initially reported, and not when the
# deaths itself was reported

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
