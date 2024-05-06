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
inkar_dat <- read_csv2('data/raw/independent_vars/inkar_data_2020.csv')
inkar_dat_17 <- read_csv2('data/raw/independent_vars/inkar_dat_2017_UPDATES.csv')
# most data from 2020; care home beds from 2019; percent in service and production jobs from 2017

# Limit to 2020 data when available:
inkar_dat <- inkar_dat[, c(1:3, which(inkar_dat[1, ] == 2020), 31)]

# Remove years:
inkar_dat <- inkar_dat[-1, ]
inkar_dat_17 <- inkar_dat_17[-1, ]

# Keep only columns of interest:
inkar_dat_17 <- inkar_dat_17 %>%
  select(Kennziffer, `Beschäftigte in personenbezogenen Dienstleistungsberufen`, `Beschäftigte in Produktionsberufen`)

# Calculate total percentage of population <18, 18-64:
inkar_dat <- inkar_dat %>%
  mutate(perc_lessthan18 = `Einwohner unter 6 Jahre...4` + `Einwohner von 6 bis unter 18 Jahren...7`,
         perc_18to64 = `Einwohner von 18 bis unter 25 Jahren...10` + `Einwohner von 25 bis unter 30 Jahren...13` +
           `Einwohner von 30 bis unter 50 Jahren...16` + `Einwohner von 50 bis unter 65 Jahren...19`) %>%
  select(Kennziffer:Aggregat, `Einwohner 65 Jahre und älter...22`:perc_18to64)

# Rename variables:
inkar_dat <- inkar_dat %>%
  rename('lk_code' = 'Kennziffer',
         'lk_name' = 'Raumeinheit',
         'lk_type' = 'Aggregat',
         'perc_65plus' = 'Einwohner 65 Jahre und älter...22',
         'pop_dens' = 'Siedlungsdichte in km²...25',
         'living_area' = 'Wohnfläche...28',
         'hosp_beds' = 'Krankenhausbetten...33',
         'care_home_beds' = 'Pflegeheimplätze...31')
inkar_dat_17 <- inkar_dat_17 %>%
  rename('lk_code' = 'Kennziffer',
         'perc_service' = 'Beschäftigte in personenbezogenen Dienstleistungsberufen',
         'perc_production' = 'Beschäftigte in Produktionsberufen')

# Calculate population-weighted average of 2017 data for LKs that were later merged:
pop_dat <- read_csv('data/raw/cdp/bevoelkerung.csv') %>%
  filter(ags5 %in% c('16056', '16063')) %>%
  select(ags5, kr_ew_20)
inkar_dat_17_red <- inkar_dat_17 %>%
  filter(lk_code %in% c('16056', '16063')) %>%
  left_join(pop_dat, by = c('lk_code' = 'ags5')) %>%
  mutate(perc_service = perc_service * kr_ew_20,
         perc_production = perc_production * kr_ew_20) %>%
  summarise(perc_service = sum(perc_service),
            perc_production = sum(perc_production),
            kr_ew_20 = sum(kr_ew_20)) %>%
  mutate(perc_service = perc_service / kr_ew_20,
         perc_production = perc_production / kr_ew_20) %>%
  mutate(Kennziffer = '16063') %>%
  select(-kr_ew_20)
inkar_dat_17 <- inkar_dat_17 %>%
  mutate(perc_service = if_else(lk_code == '16063', inkar_dat_17_red$perc_service, perc_service),
         perc_production = if_else(lk_code == '16063', inkar_dat_17_red$perc_production, perc_production)) %>%
  filter(lk_code != '16056')
rm(inkar_dat_17_red, pop_dat)

# Join tibbles:
inkar_dat <- inkar_dat %>%
  inner_join(inkar_dat_17, by = 'lk_code')
rm(inkar_dat_17)

# Convert variables to percentages of full/sub-population as needed:
# Beschäftigte in personenbezogenen Dienstleistungsberufen, in Produktionsberufen
# As far as I can tell, not possible with available data

# Reorganize variables to put health/control variables first:
inkar_dat <- inkar_dat %>%
  select(lk_code:lk_type, hosp_beds, perc_lessthan18:perc_18to64, perc_65plus, care_home_beds,
         pop_dens:living_area, perc_service:perc_production)

# Plot:
plot(inkar_dat[, 4:ncol(inkar_dat)], pch = 20)

# ---------------------------------------------------------------------------------------------------------------------

# Deprivation index

# Read in and format index data:
di_dat <- read_csv('data/raw/independent_vars/deprivation_index_NEW.csv')
di_dat <- di_dat %>%
  filter(year == 2019) %>%
  select(kreis_id:gisd_score) %>%
  rename('lk_code' = 'kreis_id',
         'GISD_Score' = 'gisd_score')

# ---------------------------------------------------------------------------------------------------------------------

# COVID-19 vaccination rates
# https://github.com/DLR-SC/memilio/tree/main/pycode/memilio-epidata
# https://www.medrxiv.org/content/10.1101/2021.07.09.21260257v3

# Read in data:
vacc_dat <- fromJSON('data/raw/independent_vars/vacc/3_all_county_vacc_all_dates.json')
vacc_dat_regional <- fromJSON('data/raw/independent_vars/vacc/2_all_county_vacc_all_dates.json')

# Format:
vacc_dat <- vacc_dat %>%
  as_tibble() %>%
  select(-ID_State) %>%
  mutate(ID_County = as.character(ID_County),
         ID_County = str_pad(ID_County, width = 5, side = 'left', pad = '0'))
vacc_dat_regional <- vacc_dat_regional %>%
  as_tibble() %>%
  select(-ID_State) %>%
  mutate(ID_County = as.character(ID_County),
         ID_County = str_pad(ID_County, width = 5, side = 'left', pad = '0'))

# Add population data and calculate vaccination rates:
pop_dat <- read_csv('data/raw/cdp/bevoelkerung.csv') %>%
  select(ags5, kr_ew_20) %>%
  rename('lk' = 'ags5',
         'pop' = 'kr_ew_20')

vacc_dat <- vacc_dat %>%
  left_join(pop_dat, by = c('ID_County' = 'lk')) %>%
  mutate(vacc1_rate = Vacc_partially / pop,
         vacc2_rate = Vacc_completed / pop,
         vacc3_rate = Vacc_refreshed / pop)
vacc_dat_regional <- vacc_dat_regional %>%
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
vacc_dat_regional <- vacc_dat_regional %>%
  filter(Date %in% week_ends) %>%
  mutate(Date = as.Date(Date),
         Year = format(Date, '%Y'),
         Week = format(Date, '%V'),
         .after = Date)
rm(week_ends)

# Get vaccination rates at 2 weeks before the midpoint of each wave (weeks 66, 92, 113 / 13 and 39 of 2021, 8 of 2022):
# (And partial waves (weeks 63 and 70 for wave 3 / weeks 87 and 97 for wave 4 / weeks 107 and 118 for wave 5 - weeks 10 and 17 / 34 and 44 of 2021 / 2 and 13 of 2022))
# (And for summer 2 - week 77 / week 24 of 2021)
vacc_dat <- vacc_dat %>%
  filter((Year == '2021' &
            Week %in% c('13', '39',
                        '10', '17',
                        '34', '44',
                        '24')) |
           (Year == '2022' &
              Week %in% c('08', '02', '13')))
vacc_dat_regional <- vacc_dat_regional %>%
  filter((Year == '2021' &
            Week %in% c('13', '39',
                        '10', '17',
                        '34', '44',
                        '24')) |
           (Year == '2022' &
              Week %in% c('08', '02', '13')))

# Convert to wide format:
vacc_dat <- vacc_dat %>%
  mutate(year_week = paste(Year, Week, sep = '_')) %>%
  select(year_week, ID_County, vacc2_rate) %>%
  pivot_wider(id_cols = ID_County, names_from = year_week, values_from = vacc2_rate)# %>%
# rename_with(~ paste0('vacc_', .x), .cols = !ID_County)
names(vacc_dat)[2:11] <- c('vacc_w3_1', 'vacc_w3', 'vacc_w3_2', 'vacc_summer2', 'vacc_w4_1', 'vacc_w4', 'vacc_w4_2', 'vacc_w5_1', 'vacc_w5', 'vacc_w5_2')
vacc_dat <- vacc_dat %>%
  select(ID_County, vacc_w3, vacc_w4, vacc_w5, vacc_w3_1, vacc_w3_2, vacc_w4_1, vacc_w4_2, vacc_w5_1, vacc_w5_2, vacc_summer2)

vacc_dat_regional <- vacc_dat_regional %>%
  mutate(year_week = paste(Year, Week, sep = '_')) %>%
  select(year_week, ID_County, vacc2_rate) %>%
  pivot_wider(id_cols = ID_County, names_from = year_week, values_from = vacc2_rate)
names(vacc_dat_regional)[2:11] <- c('vacc_w3_1', 'vacc_w3', 'vacc_w3_2', 'vacc_summer2', 'vacc_w4_1', 'vacc_w4', 'vacc_w4_2', 'vacc_w5_1', 'vacc_w5', 'vacc_w5_2')
vacc_dat_regional <- vacc_dat_regional %>%
  select(ID_County, vacc_w3, vacc_w4, vacc_w5, vacc_w3_1, vacc_w3_2, vacc_w4_1, vacc_w4_2, vacc_w5_1, vacc_w5_2, vacc_summer2)

# Plot:
plot(vacc_dat[, 2:4], pch = 20)
plot(vacc_dat_regional[, 2:4], pch = 20)

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
write_csv(vacc_dat_regional, file = 'data/formatted/independent_vars/vacc_dat_REGIONAL.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Clean up
rm(list = ls())

# ---------------------------------------------------------------------------------------------------------------------
