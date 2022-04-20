# ---------------------------------------------------------------------------------------------------------------------
# Code to read in and process/clean all covariate data
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(ISOweek)
library(testthat)
library(jsonlite)

# ---------------------------------------------------------------------------------------------------------------------

# INKAR data

# Read in downloaded data:
inkar_dat <- read_csv2('data/raw/independent_vars/inkar_data_2019.csv')
inkar_dat_old1 <- read_csv2('data/raw/independent_vars/inkar_data_2017.csv')
inkar_dat_old2 <- read_csv2('data/raw/independent_vars/inkar_dat_2017_UPDATES.csv')
# most data from 2019; percent in service and production jobs, distance to pharmacies from 2017

# Remove years:
inkar_dat <- inkar_dat[-1, ]
inkar_dat_old1 <- inkar_dat_old1[-1, ]
inkar_dat_old2 <- inkar_dat_old2[-1, ]

# Keep only columns of interest:
inkar_dat <- inkar_dat %>%
  select(Kennziffer:`Einwohner 65 Jahre und älter`, `Siedlungsdichte in km²`:Wohnfläche,
         Pflegeheimplätze:`Nahversorgung Apotheken Durchschnittsdistanz`)
inkar_dat_old1 <- inkar_dat_old1 %>%
  select(Kennziffer, `Beschäftigte in personenbezogenen Dienstleistungsberufen`)
inkar_dat_old2 <- inkar_dat_old2 %>%
  select(Kennziffer, `Beschäftigte in Produktionsberufen`)

# Calculate total percentage of population <18, 18-64:
inkar_dat <- inkar_dat %>%
  mutate(perc_lessthan18 = `Einwohner unter 6 Jahre` + `Einwohner von 6 bis unter 18 Jahren`,
         perc_18to64 = `Einwohner von 18 bis unter 25 Jahren` + `Einwohner von 25 bis unter 30 Jahren` +
           `Einwohner von 30 bis unter 50 Jahren` + `Einwohner von 50 bis unter 65 Jahren`) %>%
  select(Kennziffer:Aggregat, `Einwohner 65 Jahre und älter`:perc_18to64)

# Join tibbles:
inkar_dat <- inkar_dat %>%
  inner_join(inkar_dat_old1, by = 'Kennziffer') %>%
  inner_join(inkar_dat_old2, by = 'Kennziffer')
rm(inkar_dat_old1, inkar_dat_old2)

# Convert variables to percentages of full/sub-population as needed:
# Beschäftigte in personenbezogenen Dienstleistungsberufen, in Produktionsberufen
# As far as I can tell, not possible with available data

# Rename variables:
inkar_dat <- inkar_dat %>%
  rename('lk_code' = 'Kennziffer',
         'lk_name' = 'Raumeinheit',
         'lk_type' = 'Aggregat',
         'perc_65plus' = 'Einwohner 65 Jahre und älter',
         'pop_dens' = 'Siedlungsdichte in km²',
         'living_area' = 'Wohnfläche',
         'care_home_beds' = 'Pflegeheimplätze',
         'hosp_beds' = 'Krankenhausbetten',
         'avg_dist_pharm' = 'Nahversorgung Apotheken Durchschnittsdistanz',
         'perc_service' = 'Beschäftigte in personenbezogenen Dienstleistungsberufen',
         'perc_production' = 'Beschäftigte in Produktionsberufen')

# Reorganize variables to put health/control variables first:
inkar_dat <- inkar_dat %>%
  select(lk_code:lk_type, hosp_beds:avg_dist_pharm, perc_lessthan18:perc_18to64, perc_65plus, care_home_beds,
         pop_dens:living_area, perc_service:perc_production)

# Plot:
plot(inkar_dat[, 4:ncol(inkar_dat)], pch = 20)

# ---------------------------------------------------------------------------------------------------------------------

# Deprivation index

# Read in and format index data:
di_dat <- read_csv('data/raw/independent_vars/deprivation_index_NEW.csv')
di_dat <- di_dat %>%
  filter(Jahr == 2017) %>%
  select(Kreiskennziffer, GISD_Score:TS_Arbeitswelt_adj) %>%
  mutate(Kreiskennziffer = str_pad(Kreiskennziffer, width = 5, side = 'left', pad = '0')) %>%
  rename('lk_code' = 'Kreiskennziffer')

# Plot:
plot(di_dat[, 2:ncol(di_dat)], pch = 20)

# ---------------------------------------------------------------------------------------------------------------------

# COVID-19 vaccination rates
# https://github.com/DLR-SC/memilio/tree/main/pycode/memilio-epidata
# https://www.medrxiv.org/content/10.1101/2021.07.09.21260257v3

# Read in data:
vacc_dat <- fromJSON('data/raw/independent_vars/vacc/all_county_vacc_all_dates.json')

# Format:
vacc_dat <- vacc_dat %>%
  as_tibble() %>%
  select(-ID_State) %>%
  mutate(ID_County = as.character(ID_County),
         ID_County = str_pad(ID_County, width = 5, side = 'left', pad = '0'))

# Add population data and calculate vaccination rates:
pop_dat <- read_csv2('data/raw/independent_vars/pop_counts_12411-0015_NEW2020.csv', col_names = FALSE, skip = 6, n_max = 476) %>%
  select(-X2) %>%
  rename(lk = X1, pop = X3) %>%
  mutate(pop = as.numeric(pop)) %>%
  filter(!is.na(pop))

vacc_dat <- vacc_dat %>%
  left_join(pop_dat, by = c('ID_County' = 'lk')) %>%
  mutate(vacc1_rate = Vacc_partially / pop,
         vacc2_rate = Vacc_completed / pop,
         vacc3_rate = Vacc_refreshed / pop)
rm(pop_dat)

# Limit to the end of weeks:
week_ends <- unique(vacc_dat$Date)[format(unique(as.Date(vacc_dat$Date)), '%w') == '0']
vacc_dat <- vacc_dat %>%
  filter(Date %in% week_ends) %>%
  mutate(Date = as.Date(Date),
         Year = format(Date, '%Y'),
         Week = format(Date, '%V'),
         .after = Date)
rm(week_ends)

# Get vaccination rates at 2 weeks before the midpoint of each wave (weeks 66 and 92 / 13 and 39 of 2021):
vacc_dat <- vacc_dat %>%
  filter(Year == '2021' &
           (Week %in% c('13', '39')))

# Convert to wide format:
vacc_dat <- vacc_dat %>%
  mutate(year_week = paste(Year, Week, sep = '_')) %>%
  select(year_week, ID_County, vacc2_rate) %>%
  pivot_wider(id_cols = ID_County, names_from = year_week, values_from = vacc2_rate)# %>%
  # rename_with(~ paste0('vacc_', .x), .cols = !ID_County)
names(vacc_dat)[2:3] <- c('vacc_w3', 'vacc_w4')

# Plot:
plot(vacc_dat$vacc_w3, vacc_dat$vacc_w4, pch = 20)

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
write_csv(vacc_dat, file = 'data/formatted/independent_vars/vacc_dat.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Clean up
rm(list = ls())

# ---------------------------------------------------------------------------------------------------------------------
