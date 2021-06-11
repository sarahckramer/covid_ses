# ---------------------------------------------------------------------------------------------------------------------
# Code to format data on SES-related and other independent variables
# ---------------------------------------------------------------------------------------------------------------------

# Load libraries:
library(tidyverse)
library(testthat)

# ---------------------------------------------------------------------------------------------------------------------

# Age structure

# Read in age data:
age_dist <- read_csv2('data/raw/independent_vars/age_groups.csv', col_names = TRUE, skip = 6, n_max = 9684)
colnames(age_dist) <- c('lk', 'name', 'age', 'value', 'men', 'women')

# Format:
age_dist <- age_dist %>%
  select(lk:value) %>%
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value)) %>%
  mutate(lk = ifelse(lk == '02', '02000', lk),
         lk = ifelse(lk == '11', '11000', lk)) %>%
  filter(str_length(lk) == 5) %>%
  mutate(lk = factor(lk)) %>%
  filter(age %in% c('65 bis unter 75 Jahre',
                    '75 Jahre und mehr',
                    'Insgesamt'))

expect_true(length(levels(age_dist$lk)) == 401)

age_dist <- age_dist %>%
  pivot_wider(id_cols = lk, names_from = age, values_from = value) %>%
  mutate(prop65 = (`65 bis unter 75 Jahre` + `75 Jahre und mehr`) /
           Insgesamt) %>%
  select(lk, prop65)

# Write to file:
write_csv(age_dist, file = 'data/formatted/age_dist.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Data on immigration/naturalization/hospitals

# Read in migration/citizenship data:
mig_dat <- read_csv2('data/raw/independent_vars/inkar1_new.csv')
colnames(mig_dat) <- c('lk', 'name', 'agg', 'prop.aus', 'prop.asyl', 'cit.af', 'cit.am', 'cit.as', 'cit.aus', 'cit.eu',
                       'hosp.beds', 'doc.p.pop', 'cit.p.aus', 'cit.p.pop')

# Format:
mig_dat <- mig_dat %>%
  select(-agg) %>%
  filter(!is.na(lk))

mig_dat <- mig_dat %>%
  mutate(cit.af = (cit.af / 100) * (cit.p.pop / 1000) * 10000,
         cit.am = (cit.am / 100) * (cit.p.pop / 1000) * 10000,
         cit.as = (cit.as / 100) * (cit.p.pop / 1000) * 10000,
         cit.aus = (cit.aus / 100) * (cit.p.pop / 1000) * 10000,
         cit.eu = (cit.eu / 100) * (cit.p.pop / 1000) * 10000)
# Convert to new citizens from each continent per 10,000 population

# Write to file:
write_csv(mig_dat, file = 'data/formatted/mig_hosp_dat.csv')
