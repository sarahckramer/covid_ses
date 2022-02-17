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

# Load and format Corona Daten Plattform data

# Read in data:
cdp_dat <- read_csv('data/raw/cdp/infektionen.csv')

# Check for missing dates:
missing_dates <- check_for_missing_dates(cdp_dat)

# Limit to case data by age group:
cdp_dat <- cdp_dat %>%
  filter(variable %in% c('kr_inf_a0004', 'kr_inf_a0514', 'kr_inf_a1534', 'kr_inf_a3559', 'kr_inf_a6079', 'kr_inf_a80', 'kr_inf_99')) %>%
  select(-c(`_id`, kreis)) %>%
  pivot_longer(-c(ags2, bundesland, ags5, variable), names_to = 'date', values_to = 'cases') %>%
  mutate(date = as.Date(str_sub(date, 2, 9), format = '%Y%m%d')) %>%
  rename('lk' = 'ags5') %>%
  unique()

# Check that all age groups in all LKs have data for all available dates:
expect_equal(dim(cdp_dat)[1], length(unique(cdp_dat$date)) * length(unique(cdp_dat$lk)) * length(unique(cdp_dat$variable)))

# ---------------------------------------------------------------------------------------------------------------------

# Deal with cases with no age information

# Allocate cases with no age data proportionally:
cdp_list <- split(cdp_dat, cdp_dat$lk)
expect_true(length(cdp_list) == 401)

for (i in 1:401) {
  
  # To ensure that the final "long" tibble has the right dimensions:
  dim_orig <- nrow(cdp_list[[i]])
  
  # Widen and create columns with sum checks:
  cdp_temp <- cdp_list[[i]] %>%
    pivot_wider(names_from = variable, values_from = cases) %>%
    mutate(tot_w_age_info = kr_inf_a0004 + kr_inf_a0514 + kr_inf_a1534 + kr_inf_a3559 + kr_inf_a6079 + kr_inf_a80,
           check_sum = kr_inf_a0004 + kr_inf_a0514 + kr_inf_a1534 + kr_inf_a3559 + kr_inf_a6079 + kr_inf_a80 + kr_inf_99) %>%
    as.data.frame()
  
  # For each row, allocate un-aged data proportionally:
  for (j in 1:nrow(cdp_temp)) {
    
    if (cdp_temp[j, 'kr_inf_99'] > 0) {
      
      if (cdp_temp[j, 'tot_w_age_info'] > 0) {
        
        x_temp <- cdp_temp[j, ] %>% select(kr_inf_a0004:kr_inf_a80) %>% unlist()
        to_add <- cdp_temp[j, 'kr_inf_99']
        
        cdp_temp[j, names(x_temp)] <- reallocate_preserving_sum(x_temp, to_add)
        rm(x_temp, to_add)
        
      } else {
        print(cdp_temp[j, 'kr_inf_99'])
        # This happens very rarely - only 10 cases total
      }
      
    }
    
  }
  
  # Check that values sum to expected amounts:
  cdp_temp <- as_tibble(cdp_temp) %>%
    mutate(check_sum_POST = kr_inf_a0004 + kr_inf_a0514 + kr_inf_a1534 + kr_inf_a3559 + kr_inf_a6079 + kr_inf_a80)
  # expect_true(all(cdp_temp$check_sum == cdp_temp$check_sum_POST))
  expect_true(all.equal(cdp_temp %>% filter(tot_w_age_info > 0) %>% pull(check_sum),
                        cdp_temp %>% filter(tot_w_age_info > 0) %>% pull(check_sum_POST)))
  
  # Re-lengthen and check dimensions:
  cdp_temp <- cdp_temp %>%
    select(ags2:kr_inf_99) %>%
    pivot_longer(kr_inf_a0004:kr_inf_99,
                 names_to = 'variable',
                 values_to = 'cases')
  expect_true(nrow(cdp_temp) == dim_orig)
  rm(dim_orig)
  
  # Remove cases with no age info:
  cdp_temp <- cdp_temp %>%
    filter(variable != 'kr_inf_99')
  
  # Store in list:
  cdp_list[[i]] <- cdp_temp
  rm(cdp_temp)
  
}
rm(i, j)

# Compile list to tibble:
cdp_dat <- bind_rows(cdp_list)
rm(cdp_list)

# Check again that all age groups in all LKs have data for all available dates:
expect_equal(dim(cdp_dat)[1], length(unique(cdp_dat$date)) * length(unique(cdp_dat$lk)) * length(unique(cdp_dat$variable)))

# ---------------------------------------------------------------------------------------------------------------------

# Calculate and compare cumulative case counts

# Convert to cumulative:
cdp_dat <- cdp_dat %>%
  group_by(ags2, bundesland, lk, variable) %>%
  mutate(cases = cumsum(cases)) %>%
  ungroup()

# Check that the calculation worked:
expect_true(cdp_dat %>% group_by(lk, variable) %>% mutate(check = cummax(cases)) %>% filter(cases != check) %>% nrow() == 0)

# Reallocate 58 "missing" cases in Heinsberg:
x_temp <- cdp_dat %>%
  pivot_wider(names_from = 'variable',
              values_from = 'cases') %>%
  filter(lk == '05370',
         date == min(date)) %>%
  select(kr_inf_a0004:kr_inf_a80) %>%
  unlist()
to_add <- 58

vals_to_add <- reallocate_preserving_sum(x_temp, to_add) - x_temp
rm(x_temp, to_add)

vals_to_add <- vals_to_add %>%
  as_tibble() %>%
  bind_cols(variable = names(vals_to_add)) %>%
  rename('to_add' = 'value')
cdp_dat <- cdp_dat %>%
  left_join(vals_to_add, by = 'variable') %>%
  mutate(cases = if_else(lk == '05370', cases + to_add, cases)) %>%
  select(-to_add)
rm(vals_to_add)

# Compare sums to cumulative data in original data:
cdp_dat_check <- read_csv('data/formatted/daily_covid_cases_by_lk_CUMULATIVE_CDP.csv')
cdp_dat_sum_all_ages <- cdp_dat %>%
  group_by(ags2, bundesland, lk, date) %>%
  summarise(cases = sum(cases))

cdp_dat_check <- cdp_dat_check %>%
  left_join(cdp_dat_sum_all_ages, by = c('ags2', 'bundesland', 'lk', 'date'))
cdp_dat_check %>%
  select(-c(case_rate, pop)) %>%
  filter(cases.x != cases.y) %>%
  mutate(diff = cases.x - cases.y) %>%
  pull(diff) %>%
  summary()
# 1-4, with median 1.0 and mean 1.396; only different in 27190 / 283200 (9.60%) of data points
rm(cdp_dat_check, cdp_dat_sum_all_ages)

# ---------------------------------------------------------------------------------------------------------------------

# Get age-standardized case counts/rates

# Get population data:
pop_dat <- read_csv('data/raw/cdp/bevoelkerung.csv')

# How current?:
pop_dat %>%
  select(ags5, kr_ew_00u05:kr_ew_80, kr_ew_19) %>%
  mutate(check_tot = kr_ew_00u05 + kr_ew_05u15 + kr_ew_15u35 + kr_ew_35u60 + kr_ew_60u80 + kr_ew_80) %>%
  filter(check_tot != kr_ew_19)
# Data from 2019

# Format population data:
pop_dat <- pop_dat %>%
  select(ags5, kr_ew_00u05:kr_ew_80) %>%
  pivot_longer(-ags5,
               names_to = 'variable',
               values_to = 'pop') %>%
  rename('lk' = 'ags5') %>%
  mutate(age = str_sub(variable, 7, 8)) %>%
  select(-variable)

# Merge and calculate age-specific rates:
cdp_dat_new <- cdp_dat %>%
  mutate(age = str_sub(variable, 9, 10)) %>%
  left_join(pop_dat, by = c('lk', 'age'))
expect_true(nrow(cdp_dat) == nrow(cdp_dat_new))
cdp_dat <- cdp_dat_new
rm(cdp_dat_new)

cdp_dat <- cdp_dat %>%
  mutate(case_rate = cases / pop)

# Get standard age distribution (country-level):
pop_dat <- pop_dat %>%
  group_by(age) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  mutate(age_prop = pop / sum(pop)) %>%
  select(-pop)

# Get standardized case rates/counts for each LK's population:
cdp_dat <- cdp_dat %>%
  left_join(pop_dat, by = 'age') %>%
  mutate(cases_stand = case_rate * age_prop) %>%
  group_by(ags2, bundesland, lk, date) %>%
  summarise(case_rate_stand = sum(cases_stand),
            pop = sum(pop),
            cases = round(case_rate_stand * pop)) %>%
  select(ags2:date, pop, cases) %>%
  mutate(case_rate = cases / pop * 100000)

# Write data to file:
write_csv(cdp_dat, file = 'data/formatted/STAND_daily_covid_cases_by_lk_CUMULATIVE_CDP.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Convert to weekly

# Limit data to only rows at week ends:
week_ends <- unique(cdp_dat$date)[format(unique(cdp_dat$date), '%w') == '0']

cdp_dat_wk <- cdp_dat %>%
  filter(date %in% week_ends)

# Add column for year and for week number:
cdp_dat_wk <- cdp_dat_wk %>%
  mutate(Year = format(date, '%Y'),
         Week = format(date, '%V'),
         .after = date) %>%
  mutate(Year = if_else(Week == 53, '2020', Year),
         Year = if_else(Week == 52 & Year != '2020', '2021', Year))

# Write data to file:
write_csv(cdp_dat_wk, file = 'data/formatted/STAND_weekly_covid_cases_by_lk_CUMULATIVE_CDP.csv')

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

# Clean up:
rm(list = ls())
