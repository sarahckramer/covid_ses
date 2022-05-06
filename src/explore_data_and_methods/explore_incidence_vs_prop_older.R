# ---------------------------------------------------------------------------------------------------------------------
# Check whether incidence/age-standardized incidence associated with proportion older than 60/80
# ---------------------------------------------------------------------------------------------------------------------

# Load libraries:
library(tidyverse)

# Read in age-standardized incidence data:
dat_cumulative_stand <- read_csv('data/formatted/STAND_cumulative_cases_and_deaths.csv')
dat_cumulative_stand <- dat_cumulative_stand %>%
  filter(outcome %in% c('cases_wave1_rate', 'cases_wave2_rate', 'cases_wave3_rate',
                        'cases_wave4_rate', 'cases_wave5_rate', 'cases_wave1_1_rate',
                        'cases_wave1_2_rate')) %>%
  mutate(outcome = str_remove(outcome, '_rate'))

# Get cumulative incidence by wave:
dat_cumulative <- read_csv('data/formatted/weekly_covid_cases_by_lk_CUMULATIVE_CDP.csv')
dat_cumulative <- dat_cumulative %>%
  mutate(Week = as.numeric(Week)) %>%
  mutate(time = if_else(Year == 2020, Week, Week + 53),
         time = if_else(Year == 2022, time + 52, time),
         .before = Year) %>%
  filter(time %in% c(14, 20, 39, 61, 74, 84, 104, 110)) %>%
  select(time, lk, cases) %>%
  mutate(time = paste('wk', time, sep = '_')) %>%
  pivot_wider(names_from = time, values_from = cases) %>%
  mutate(cases_wave1 = wk_20,
         cases_wave2 = wk_61 - wk_39,
         cases_wave3 = wk_74 - wk_61,
         cases_wave4 = wk_104 - wk_84,
         cases_wave5 = wk_110 - wk_104,
         cases_wave1_1 = wk_14,
         cases_wave1_2 = wk_20 - wk_14) %>%
  select(lk, cases_wave1:cases_wave1_2) %>%
  pivot_longer(-lk, names_to = 'outcome', values_to = 'val') %>%
  left_join(dat_cumulative_stand %>% select(lk, pop) %>% unique(),
            by = 'lk') %>%
  mutate(val = val / pop * 10000)

# Check how similar raw vs. age-standardized incidence are:
par(mfrow = c(2, 4))
for (outcome in unique(dat_cumulative_stand$outcome)) {
  plot(dat_cumulative_stand$val[dat_cumulative_stand$outcome == outcome],
       dat_cumulative$val[dat_cumulative$outcome == outcome],
       pch = 20,
       main = outcome,
       xlab = 'Age-Standardized Incidence',
       ylab = 'Raw Incidence')
}
rm(outcome)

# Calculate proportion older than 60/80 by wave:
dat_prop <- read_csv('data/formatted/weekly_covid_cases_by_lk_by_age_CUMULATIVE_CDP.csv')
dat_prop <- dat_prop %>%
  mutate(Week = as.numeric(Week)) %>%
  mutate(time = if_else(Year == 2020, Week, Week + 53),
         time = if_else(Year == 2022, time + 52, time),
         .before = Year) %>%
  filter(time %in% c(14, 20, 39, 61, 74, 84, 104, 110)) %>%
  select(time, lk, variable, cases) %>%
  mutate(time = paste('wk', time, sep = '_')) %>%
  pivot_wider(names_from = time, values_from = cases) %>%
  mutate(cases_wave1 = wk_20,
         cases_wave2 = wk_61 - wk_39,
         cases_wave3 = wk_74 - wk_61,
         cases_wave4 = wk_104 - wk_84,
         cases_wave5 = wk_110 - wk_104,
         cases_wave1_1 = wk_14,
         cases_wave1_2 = wk_20 - wk_14) %>%
  select(lk:variable, cases_wave1:cases_wave1_2) %>%
  pivot_longer(-c(lk, variable), names_to = 'outcome', values_to = 'val')
dat_prop <- dat_prop %>%
  pivot_wider(names_from = variable, values_from = val) %>%
  mutate(prop_60plus = (kr_inf_a6079 + kr_inf_a80) /
           (kr_inf_a0004 + kr_inf_a0514 + kr_inf_a1534 + kr_inf_a3559 + kr_inf_a6079 + kr_inf_a80),
         prop_80plus = kr_inf_a80 /
           (kr_inf_a0004 + kr_inf_a0514 + kr_inf_a1534 + kr_inf_a3559 + kr_inf_a6079 + kr_inf_a80)) %>%
  select(lk:outcome, prop_60plus:prop_80plus)

# Calculate correlations:
for (outcome in unique(dat_cumulative_stand$outcome)) {
  
  dat_temp_cumulative_stand <- dat_cumulative_stand[dat_cumulative_stand$outcome == outcome, ]
  dat_temp_cumulative <- dat_cumulative[dat_cumulative$outcome == outcome, ]
  dat_temp_prop <- dat_prop[dat_prop$outcome == outcome, ]
  
  dat_temp_cumulative_stand <- dat_temp_cumulative_stand %>%
    left_join(dat_temp_prop, by = c('lk', 'outcome'))
  dat_temp_cumulative <- dat_temp_cumulative %>%
    left_join(dat_temp_prop, by = c('lk', 'outcome'))
  
  print(outcome)
  
  cor.test(dat_temp_cumulative_stand$val, dat_temp_cumulative_stand$prop_60plus, method = 'kendall') %>%
    print()
  cor.test(dat_temp_cumulative_stand$val, dat_temp_cumulative_stand$prop_80plus, method = 'kendall') %>%
    print()
  cor.test(dat_temp_cumulative$val, dat_temp_cumulative$prop_60plus, method = 'kendall') %>%
    print()
  cor.test(dat_temp_cumulative$val, dat_temp_cumulative$prop_80plus, method = 'kendall') %>%
    print()
  
  par(mfrow = c(2, 2))
  plot(dat_temp_cumulative_stand$val, dat_temp_cumulative_stand$prop_60plus, pch = 20,
       xlab = 'Age-Standardized Incidence', ylab = 'Prop. 60+')
  plot(dat_temp_cumulative_stand$val, dat_temp_cumulative_stand$prop_80plus, pch = 20,
       xlab = 'Age-Standardized Incidence', ylab = 'Prop. 80+')
  plot(dat_temp_cumulative$val, dat_temp_cumulative$prop_60plus, pch = 20,
       xlab = 'Raw Incidence', ylab = 'Prop. 60+')
  plot(dat_temp_cumulative$val, dat_temp_cumulative$prop_80plus, pch = 20,
       xlab = 'Raw Incidence', ylab = 'Prop. 80+')
  
  print('----------------------------------')
  
}
rm(outcome)

# Clean up:
rm(list = ls())
