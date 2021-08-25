# ---------------------------------------------------------------------------------------------------------------------
# Code to read in and process/clean all covariate data
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)

# ---------------------------------------------------------------------------------------------------------------------

# # Population data
# 
# # Read in population data by Landkreis:
# pop_dat <- read_csv2('data/raw/independent_vars/pop_counts_12411-0015.csv', col_names = FALSE, skip = 6, n_max = 476)
# # Source: https://www-genesis.destatis.de/genesis/online
# 
# # Format:
# pop_dat <- pop_dat %>%
#   select(-X2) %>%
#   rename(lk = X1, pop = X3) %>%
#   mutate(pop = as.numeric(pop)) %>%
#   filter(!is.na(pop))
# 
# # This is also handled in the case/death data formatting code

# ---------------------------------------------------------------------------------------------------------------------

# INKAR data

# Read in downloaded data:
inkar_dat <- read_csv2('data/raw/independent_vars/inkar_data_2017.csv')
# most data from 2017
# 2016: Krankenhausbetten

# Remove years:
inkar_dat <- inkar_dat[-1, ]

# Keep only columns of interest:
inkar_dat <- inkar_dat %>%
  select(Kennziffer:Aggregat, `Einwohner 65 Jahre und älter`:Frauenanteil, Siedlungsdichte,
         Ausländeranteil, `Verhältnis der Beschäftigungsquote von Ausländern zu gesamt`,
         Wohnfläche, `Anteil Wohnungen in Mehrfamilienhäusern`,
         `Beschäftigte in personenbezogenen Dienstleistungsberufen`,
         Pflegeheimplätze, Krankenhausbetten, `Nahversorgung Apotheken Durchschnittsdistanz`)

# Convert variables to percentages of full/sub-population as needed:
# Beschäftigte in personenbezogenen Dienstleistungsberufen?

# Rename variables:
inkar_dat <- inkar_dat %>%
  rename('lk_code' = 'Kennziffer',
         'lk_name' = 'Raumeinheit',
         'lk_type' = 'Aggregat',
         'perc_65plus' = 'Einwohner 65 Jahre und älter',
         'perc_75plus' = 'Einwohner 75 Jahre und älter',
         'perc_85plus' = 'Einwohner 85 Jahre und älter',
         'perc_women' = 'Frauenanteil',
         'pop_dens' = 'Siedlungsdichte',
         'perc_imm' = 'Ausländeranteil',
         'employ_rate_ratio' = 'Verhältnis der Beschäftigungsquote von Ausländern zu gesamt',
         'living_area' = 'Wohnfläche',
         'perc_apt_multifamily' = 'Anteil Wohnungen in Mehrfamilienhäusern',
         'perc_service' = 'Beschäftigte in personenbezogenen Dienstleistungsberufen',
         'long_term_care_beds' = 'Pflegeheimplätze',
         'hosp_beds' = 'Krankenhausbetten',
         'avg_dist_pharm' = 'Nahversorgung Apotheken Durchschnittsdistanz')

# Reorganize variables to put health/control variables first:
inkar_dat <- inkar_dat %>%
  select(lk_code:lk_type, hosp_beds:avg_dist_pharm, perc_65plus:perc_women, long_term_care_beds,
         perc_imm:employ_rate_ratio, pop_dens, living_area:perc_service)

# Plot:
plot(inkar_dat[, 4:ncol(inkar_dat)], pch = 20)

# # Visualize similar types of data:
# inkar_demo <- inkar_dat[, c(6:9, 13)]
# inkar_housing <- inkar_dat[, 14:15]
# # inkar_work <- inkar_dat[, 16]
# inkar_imm <- inkar_dat[, 11:12]
# inkar_health <- inkar_dat[, c(4:5, 10)]
# 
# plot(inkar_demo, pch = 20)
# plot(inkar_housing, pch = 20)
# plot(inkar_imm, pch = 20)
# plot(inkar_health, pch = 20)
# 
# rm(inkar_demo, inkar_housing, inkar_imm, inkar_health)

# ---------------------------------------------------------------------------------------------------------------------

# # Format GENESIS data
# 
# # Read in individual data files:
# gen_dat_mig1 <- read_csv2('data/raw/independent_vars/AI-Z1-2011.csv', col_names = FALSE, skip = 5, n_max = 538)
# gen_dat_mig1 <- gen_dat_mig1[, c(2:3, 7)]
# names(gen_dat_mig1) <- c('lk_code', 'lk_name', 'perc_MHG')
# # 2011
# 
# gen_dat_mig2 <- read_csv2('data/raw/independent_vars/12111-07-01-4.csv', col_names = FALSE, skip = 11, n_max = 5380)
# names(gen_dat_mig2) <- c('lk_code', 'lk_name', 'age', 'pop_private', 'all_german', 'german_wo_MHG', 'german_w_MHG', 'other_citizenship')
# gen_dat_mig2 <- gen_dat_mig2 %>%
#   filter(age == 'Insgesamt') %>%
#   select(-c(lk_name, age, all_german))
# # 2011
# 
# gen_dat_mig3 <- read_csv2('data/raw/independent_vars/12711-03-02-4-B.csv', col_names = FALSE, skip = 9, n_max = 1467)
# gen_dat_mig3 <- gen_dat_mig3[, c(2, 4, 8)]
# names(gen_dat_mig3) <- c('lk_code', 'citizenship', 'migration_from_outside_DE')
# gen_dat_mig3 <- gen_dat_mig3 %>%
#   filter(citizenship != 'Deutsche' & citizenship != 'Insgesamt') %>%
#   select(-citizenship) %>%
#   mutate(lk_code = if_else(lk_code %in% c('02000', '11000'), str_sub(lk_code, 1, 2), lk_code))
# # 2019
# 
# gen_dat_household <- read_csv2('data/raw/independent_vars/AI-Z3-2011.csv', col_names = FALSE, skip = 5, n_max = 538)
# gen_dat_household <- gen_dat_household[, c(2, 4:5)]
# names(gen_dat_household) <- c('lk_code', 'avg_household_size', 'perc_1person_households')
# # 2011
# 
# gen_dat_essential <- read_csv2('data/raw/independent_vars/13312-01-05-4.csv', col_names = FALSE, skip = 7, n_max = 1614)
# gen_dat_essential <- gen_dat_essential[, c(1:2, 4, 11)]
# names(gen_dat_essential) <- c('year', 'lk_code', 'total_workers', 'workers_publicservice_education_health')
# gen_dat_essential <- gen_dat_essential %>%
#   filter(year == 2019) %>%
#   select(-year)
# # 2019
# 
# # Join data into one tibble:
# genesis_dat <- gen_dat_mig1 %>%
#   full_join(gen_dat_mig2, by = 'lk_code') %>%
#   full_join(gen_dat_mig3, by = 'lk_code') %>%
#   full_join(gen_dat_household, by = 'lk_code') %>%
#   full_join(gen_dat_essential, by = 'lk_code')
# 
# # Remove individual datasets:
# rm(gen_dat_mig1, gen_dat_mig2, gen_dat_mig3, gen_dat_household, gen_dat_essential)
# 
# # Reduce to LKs:
# genesis_dat <- genesis_dat %>%
#   filter(str_length(lk_code) == 5 | lk_code %in% c('02', '11')) %>%
#   mutate(lk_code = str_pad(lk_code, width = 5, side = 'right', pad = '0'))
# 
# # Join population data:
# genesis_dat <- genesis_dat %>%
#   right_join(pop_dat, by = c('lk_code' = 'lk'))
# 
# # Reformat data:
# genesis_dat <- genesis_dat %>%
#   mutate(across(c(perc_MHG, avg_household_size:workers_publicservice_education_health), ~ str_replace(.x, ',', '.'))) %>%
#   mutate(across(perc_MHG:workers_publicservice_education_health, as.numeric)) %>%
#   mutate(perc_wo_MHG = german_wo_MHG / pop_private * 100,
#          perc_w_MHG_German = german_w_MHG / pop_private * 100,
#          perc_other_cit = other_citizenship / pop_private * 100,
#          migration_from_outside_DE = migration_from_outside_DE / pop * 1000,
#          perc_workers_ServEduHealth = workers_publicservice_education_health / total_workers * 100) %>%
#   select(lk_code:perc_MHG, perc_w_MHG_German:perc_other_cit, migration_from_outside_DE:perc_1person_households, perc_workers_ServEduHealth)
# 
# # Data from 2011 missing values for 7 LKs, 6 from Mecklenburg-Vorpommern and 1 from Niedersachsen - these appear
# # to be newer Landkreise?

# ---------------------------------------------------------------------------------------------------------------------

# Deprivation index

# First, read in and format index data themselves:
di_dat <- read_csv('data/raw/independent_vars/deprivation_index_NEW.csv')
di_dat <- di_dat %>%
  filter(Jahr == 2017) %>%
  select(Kreiskennziffer, GISD_Score:TS_Arbeitswelt_adj) %>%
  mutate(Kreiskennziffer = str_pad(Kreiskennziffer, width = 5, side = 'left', pad = '0'))

# Then, get individual variables (and similar variables):
di_variables_dat <- read_csv2('data/raw/independent_vars/inkar_data_deprivationsindex_individual_variables.csv')
di_variables_dat <- di_variables_dat[-1, ]

di_variables_dat <- di_variables_dat %>%
  select(Kennziffer,
         `Schulabgänger ohne Abschluss`, `Anteil Beschäftigte mit akademischem Berufsabschluss`, `Anteil Beschäftigte ohne Berufsabschluss`,
         Schuldnerquote, Haushaltseinkommen, Steuerkraft,
         Arbeitslosigkeit, Bruttoverdienst, Beschäftigtenquote)

# Merge:
di_dat <- di_dat %>%
  left_join(di_variables_dat, by = c('Kreiskennziffer' = 'Kennziffer'))
rm(di_variables_dat)

# Rename variables:
di_dat <- di_dat %>%
  rename('lk_code' = 'Kreiskennziffer',
         'perc_no_degree' = `Schulabgänger ohne Abschluss`,
         'perc_workers_academic_degree' = `Anteil Beschäftigte mit akademischem Berufsabschluss`,
         'perc_workers_no_voc_degree' = `Anteil Beschäftigte ohne Berufsabschluss`,
         'perc_debt' = 'Schuldnerquote',
         'household_income' = 'Haushaltseinkommen',
         'tax_force' = 'Steuerkraft',
         'unemployment' = 'Arbeitslosigkeit',
         'gross_income' = 'Bruttoverdienst',
         'perc_SV_workers' = 'Beschäftigtenquote')

# Plot:
plot(di_dat[, 2:ncol(di_dat)], pch = 20)

# ---------------------------------------------------------------------------------------------------------------------

# Mobility data

# RKI data (from cell phone network):
mobility_rki_dat <- read_csv('data/raw/independent_vars/mobility_change_kreise_NEW.csv')

# Add years and reduce columns:
mobility_rki_dat <- mobility_rki_dat %>%
  select(NUTS3:week, mobility_change, mobility_change_weekly) %>%
  mutate(year = str_sub(date, 1, 4)) %>%
  mutate(year = if_else(week == 53, '2020', year)) %>%
  select(NUTS3:name, year, week, mobility_change:mobility_change_weekly)

# Remove incomplete weeks:
mobility_rki_dat <- mobility_rki_dat %>%
  filter(!(year == 2020 & week == 5) & !(year == 2021 & week == 33))
# note there are some weeks that don't have data for some days; this just removes partial weeks at beginning and end

# Check whether weekly change is a simple mean:
mobility_rki_dat %>%
  group_by(year, week, mobility_change_weekly) %>%
  summarise(mean = mean(mobility_change))
# No, they sometimes differ; use RKI's weekly counts

# Remove daily values:
mobility_rki_dat <- mobility_rki_dat %>%
  select(-mobility_change) %>%
  unique()

# # Plot:
# ggplot(data = mobility_rki_dat, aes(x = week, y = mobility_change_weekly, group = NUTS3)) + geom_line() +
#   facet_wrap(~year) + theme_classic()

# Convert NUTS3 codes to Landkreis codes:
convert_names <- read_csv2('data/raw/independent_vars/Referenz Gebietsstand 2017-1.csv')
convert_names <- convert_names[-1, ]
convert_names <- convert_names %>%
  select(krs17, NUTS3) %>%
  mutate(krs17 = str_sub(krs17, 1, -4)) %>%
  mutate(krs17 = str_pad(krs17, width = 5, side = 'left', pad = '0'))

convert_names <- convert_names %>%
  mutate(NUTS3 = if_else(NUTS3 == 'DEB16', 'DEB1C', NUTS3),
         NUTS3 = if_else(NUTS3 == 'DEB19', 'DEB1D', NUTS3))

mobility_rki_dat <- mobility_rki_dat %>%
  left_join(convert_names, by = 'NUTS3') %>%
  select(krs17, name:mobility_change_weekly) %>%
  rename(lk_code = 'krs17')

# Clean up:
rm(convert_names)

# ---------------------------------------------------------------------------------------------------------------------

# # Behavior tracker data
# 
# behav_dat <- read_csv('data/raw/independent_vars/yougov_tracker_germany.csv')
# 
# behav_dat %>%
#   select(RecordNo:qweek, gender:state, i12_health_1:i12_health_20, i12_health_21:i12_health_25, i12_health_26:i12_health_29, d1_health_1:d1_health_13, WCRV_4)
# # i12_health are public health measures; d1_health are health conditions
# 
# # Only available at Bundesland level, but could be useful

# ---------------------------------------------------------------------------------------------------------------------

# Stringency index

# Read in:
stringency_dat <- read_csv2('data/raw/independent_vars/stringency_index.csv')
contain_health_dat <- read_csv2('data/raw/independent_vars/containment_health_index.csv')

# Format:
stringency_dat <- stringency_dat %>%
  filter(country_code == 'DEU') %>%
  pivot_longer(cols = -c(country_code:country_name),
               names_to = 'date',
               values_to = 'stringency_index') %>%
  select(date:stringency_index)

contain_health_dat <- contain_health_dat %>%
  filter(country_code == 'DEU') %>%
  pivot_longer(cols = -c(country_code:country_name),
               names_to = 'date',
               values_to = 'containment_health_index') %>%
  select(date:containment_health_index)

# Join:
index_dat <- stringency_dat %>%
  left_join(contain_health_dat, by = 'date')
rm(stringency_dat, contain_health_dat)

# Convert to weekly:
index_dat <- index_dat %>%
  mutate(date = as.Date(date, format = '%d%b%Y')) %>%
  mutate(Year = format(date, '%Y'),
         Week = format(date, '%V')) %>%
  select(-date) %>%
  group_by(Year, Week) %>%
  summarise(stringency_index = mean(stringency_index),
            containment_health_index = mean(containment_health_index))
# make sure week 53 is correct; for months 3,5,10,12, need German names or change locale

# Only available at the country level

# ---------------------------------------------------------------------------------------------------------------------

# Merge datasets as needed

# SES/health indicators:
ses_dat <- inkar_dat %>%
  left_join(di_dat, by = 'lk_code')
rm(inkar_dat, di_dat)
# plot(ses_dat[, 4:ncol(ses_dat)], pch = 20)

# ---------------------------------------------------------------------------------------------------------------------

# Output formatted data

write_csv(ses_dat, file = 'data/formatted/independent_vars/ses_independent_variables.csv')
write_csv(mobility_rki_dat, file = 'data/formatted/independent_vars/mobility_dat_WEEKLY.csv')
write_csv(index_dat, file = 'data/formatted/independent_vars/stringency_and_containment_indices.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Clean up
rm(list = ls())

# ---------------------------------------------------------------------------------------------------------------------
