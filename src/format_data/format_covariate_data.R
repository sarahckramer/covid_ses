# ---------------------------------------------------------------------------------------------------------------------
# Code to read in and process/clean all covariate data
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(ISOweek)

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
inkar_dat2 <- read_csv2('data/raw/independent_vars/inkar_dat_2017_UPDATES.csv')
# most data from 2017
# 2016: Krankenhausbetten

# Remove years:
inkar_dat <- inkar_dat[-1, ]
inkar_dat2 <- inkar_dat2[-1, ]

# Keep only columns of interest:
inkar_dat <- inkar_dat %>%
  select(Kennziffer:Aggregat, `Einwohner 65 Jahre und älter`:Frauenanteil, Siedlungsdichte,
         Ausländeranteil, `Verhältnis der Beschäftigungsquote von Ausländern zu gesamt`,
         Wohnfläche, `Anteil Wohnungen in Mehrfamilienhäusern`,
         `Beschäftigte in personenbezogenen Dienstleistungsberufen`,
         Pflegeheimplätze, Krankenhausbetten, `Nahversorgung Apotheken Durchschnittsdistanz`)
inkar_dat2 <- inkar_dat2 %>%
  select(Kennziffer, Einpendler:Auspendler, `Beschäftigte in Produktionsberufen`)

# Join tibbles:
inkar_dat <- inkar_dat %>%
  inner_join(inkar_dat2, by = 'Kennziffer')
rm(inkar_dat2)

# Convert variables to percentages of full/sub-population as needed:
# Beschäftigte in personenbezogenen Dienstleistungsberufen, in Produktionsberufen; Ein/Auspendler
# As far as I can tell, would only even be possible for Auspendler

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
         'care_home_beds' = 'Pflegeheimplätze',
         'hosp_beds' = 'Krankenhausbetten',
         'avg_dist_pharm' = 'Nahversorgung Apotheken Durchschnittsdistanz',
         'commuters_in' = 'Einpendler',
         'commuters_out' = 'Auspendler',
         'perc_production' = 'Beschäftigte in Produktionsberufen')

# Reorganize variables to put health/control variables first:
inkar_dat <- inkar_dat %>%
  select(lk_code:lk_type, hosp_beds:avg_dist_pharm, perc_65plus:perc_women, care_home_beds,
         perc_imm:employ_rate_ratio, pop_dens, living_area:perc_service, perc_production,
         commuters_in:commuters_out)

# Plot:
plot(inkar_dat[, 4:ncol(inkar_dat)], pch = 20)

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

# Check for issue in week 53 calculations:
mobility_rki_dat %>%
  select(NUTS3:week, mobility_change, mobility_change_weekly) %>%
  mutate(year = str_sub(date, 1, 4)) %>%
  mutate(year = if_else(week == 53, '2020', year)) %>%
  filter(week == 53)

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
  group_by(NUTS3, year, week, mobility_change_weekly) %>%
  summarise(mean_change_weekly = mean(mobility_change))
# No, they sometimes differ

# To avoid issue with week 53, use our calculations:
mobility_rki_dat <- mobility_rki_dat %>%
  group_by(NUTS3, year, week) %>%
  summarise(mobility_change_weekly = mean(mobility_change)) %>%
  ungroup()

# # Remove daily values:
# mobility_rki_dat <- mobility_rki_dat %>%
#   select(-mobility_change) %>%
#   unique()

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
  select(krs17, year:mobility_change_weekly) %>%
  rename(lk_code = 'krs17')

# Clean up:
rm(convert_names)

# ---------------------------------------------------------------------------------------------------------------------

# Measures against COVID-19

# Read in categories and subcategories:
policy_dat_ober <- read_csv('data/raw/cdp/kr_massnahmen_oberkategorien.csv')
policy_dat_unter <- read_csv('data/raw/cdp/kr_massnahmen_unterkategorien.csv')

# Format larger categories:
policy_dat_ober <- policy_dat_ober %>%
  select(-c(`_id`:bundesland, kreis)) %>%
  filter(m_code %in% c('M02a', 'M02b', 'M03', 'M14', 'M15', 'M16', 'M21')) %>%
  pivot_longer(-c(ags5:m_code), names_to = 'date', values_to = 'val') %>%
  mutate(date = as.Date(str_sub(date, 2, 9), format = '%Y%m%d'))

# Format subcategories:
policy_dat_unter <- policy_dat_unter %>%
  select(-c(`_id`:bundesland, kreis)) %>%
  filter(str_detect(code, '^M02|^M03|^M16|^M21')) %>%
  pivot_longer(-c(ags5:code), names_to = 'date', values_to = 'val') %>%
  mutate(date = as.Date(str_sub(date, 2, 9), format = '%Y%m%d'),
         val = ifelse(val == -99, NA, val))

# Split into categories and recode:
school_pol_dat <- policy_dat_unter %>%
  filter(str_detect(code, '^M02')) %>%
  filter(!str_detect(code, '^M02a_01|^M02b_01'))
expect_true(school_pol_dat %>%
              group_by(ags5, date, str_sub(code, 1, 7)) %>%
              summarise(val = all(is.na(val))) %>%
              filter(val) %>%
              nrow() == 0)
school_pol_dat <- school_pol_dat %>%
  group_by(ags5, date, str_sub(code, 1, 7)) %>%
  summarise(val = any(val > 0 & !is.na(val))) %>%
  rename('code' = `str_sub(code, 1, 7)`) %>%
  mutate(val = as.numeric(val))
school_pol_dat <- school_pol_dat %>%
  pivot_wider(names_from = 'code', values_from = 'val') %>%
  mutate(school_closures2 = ifelse(M02a_04 == 1, 3, NA),
         school_closures2 = if_else(M02a_03 == 1 & is.na(school_closures2), 2, school_closures2),
         school_closures2 = if_else(M02a_02 == 1 & is.na(school_closures2), 1, school_closures2),
         school_closures2 = if_else(is.na(school_closures2), 0, school_closures2),
         school_closures1 = ifelse(M02b_04 == 1, 3, NA),
         school_closures1 = if_else(M02b_03 == 1 & is.na(school_closures1), 2, school_closures1),
         school_closures1 = if_else(M02b_02 == 1 & is.na(school_closures1), 1, school_closures1),
         school_closures1 = if_else(is.na(school_closures1), 0, school_closures1)) %>%
  select(ags5:date, school_closures1, school_closures2) %>%
  pivot_longer(school_closures1:school_closures2,
               names_to = 'code',
               values_to = 'val')

kita_pol_dat <- policy_dat_unter %>%
  filter(str_detect(code, '^M03')) %>%
  filter(!str_detect(code, '^M03_01'))
expect_true(kita_pol_dat %>%
              group_by(ags5, date, str_sub(code, 1, 6)) %>%
              summarise(val = all(is.na(val))) %>%
              filter(val) %>%
              nrow() == 0)
kita_pol_dat <- kita_pol_dat %>%
  group_by(ags5, date, str_sub(code, 1, 6)) %>%
  summarise(val = any(val > 0 & !is.na(val))) %>%
  rename('code' = `str_sub(code, 1, 6)`) %>%
  mutate(val = as.numeric(val))
kita_pol_dat <- kita_pol_dat %>%
  pivot_wider(names_from = 'code', values_from = 'val') %>%
  mutate(kita_closures = ifelse(M03_06 == 1, 4, NA),
         kita_closures = if_else(M03_05 == 1 & is.na(kita_closures), 3, kita_closures),
         kita_closures = if_else(M03_03 == 1 & is.na(kita_closures), 2, kita_closures),
         kita_closures = if_else(M03_02 == 1 & is.na(kita_closures), 1, kita_closures),
         kita_closures = if_else(is.na(kita_closures), 0 , kita_closures)) %>%
  select(ags5:date, kita_closures) %>%
  pivot_longer(kita_closures,
               names_to = 'code',
               values_to = 'val')

mask_pol_dat <- policy_dat_unter %>%
  # filter(str_detect(code, '^M16_02|^M16_03|^M16_04|^M16_05|^M16_10')) %>%
  filter(str_detect(code, '^M16_03|^M16_04'))
expect_true(mask_pol_dat %>%
              group_by(ags5, date, str_sub(code, 1, 6)) %>%
              summarise(val = all(is.na(val))) %>%
              filter(val) %>%
              nrow() == 0)
mask_pol_dat <- mask_pol_dat %>%
  group_by(ags5, date, str_sub(code, 1, 6)) %>%
  summarise(val = any(val > 0 & !is.na(val))) %>%
  rename('code' = `str_sub(code, 1, 6)`) %>%
  mutate(val = as.numeric(val),
         code = if_else(code == 'M16_03', 'masks_transport', 'masks_shopping'))

# test_pol_dat <- policy_dat_unter %>%
#   filter(str_detect(code, '^M21')) %>%
#   filter(!str_detect(code, '^M21_01'))
# test_pol_dat <- test_pol_dat %>%
#   group_by(ags5, date, str_sub(code, 1, 6)) %>%
#   summarise(val = any(val[!is.na(val)] > 0)) %>%
#   rename('code' = `str_sub(code, 1, 6)`) %>%
#   mutate(val = as.numeric(val))

# Join into single data frame:
policy_dat <- school_pol_dat %>%
  bind_rows(kita_pol_dat) %>%
  bind_rows(mask_pol_dat)

# Clean up:
rm(policy_dat_ober, policy_dat_unter, school_pol_dat, kita_pol_dat, mask_pol_dat)

# Plot/explore:
# policy_dat %>% group_by(ags5, code) %>% summarise(weighted_avg = sum(val) / length(val)) %>%
#   ggplot(aes(x = ags5, y = weighted_avg)) + geom_point() + facet_wrap(~ code, scales = 'free_y') +
#   theme_classic()

# for (pol in unique(policy_dat$code)) {
#   dat_temp <- policy_dat %>%
#     filter(code == pol) %>%
#     mutate(bundesland = str_sub(ags5, 1, 2))
#   
#   p_temp <- ggplot(dat_temp, aes(x = date, y = val, group = ags5)) +
#     geom_line() + facet_wrap(~ bundesland) + theme_classic() +
#     labs(x = 'Date', y = 'Value', title = pol)
#   print(p_temp)
# }
# rm(dat_temp, p_temp, pol)

# Convert to wide format:
policy_dat <- policy_dat %>%
  pivot_wider(names_from = code, values_from = val)

# Convert to weekly values:
policy_dat <- policy_dat %>%
  mutate(Year = str_sub(date, 1, 4),
         Week = format(date, '%V')) %>%
  group_by(ags5, Year, Week) %>%
  summarise(school_closures1 = max(school_closures1),
            school_closures2 = max(school_closures2),
            kita_closures = max(kita_closures),
            masks_transport = max(masks_transport),
            masks_shopping = max(masks_shopping))
# For each week, choose the highest value each measure takes during that week for that LK

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

# COVID-19 vaccination rates

# # Read in data:
# vacc_dat <- read_csv('data/raw/cdp/impfdaten_regional.csv')
# 
# # Format:
# vacc_dat <- vacc_dat %>%
#   select(-c(`_id`:bundesland, kreis)) %>%
#   rename('date' = 'datum') %>%
#   mutate(Year = format(date, '%Y'),
#          Week = format(date, '%V'),
#          .after = date) %>%
#   mutate(Year = if_else(Week == '53', '2020', Year)) %>%
#   mutate(year_week = paste(Year, Week, sep = '_'),
#          .after = Week)
# 
# # Keep measures of interest:
# vacc_dat <- vacc_dat %>%
#   select(ags5:kr_erstimpf, kr_zweitimpf)
# 
# # Remove incomplete weeks:
# vacc_dat <- vacc_dat %>%
#   filter(year_week != '2021_39')
# # Several weeks/LKs with less than 7 observations, but removing these will definitely prevent us from calculating
# # the total vaccination coverage over time; assume these values represent no vaccinations
# # Note that these mostly happen near the beginning of the vaccination period
# 
# # Sum to get weekly values:
# vacc_dat <- vacc_dat %>%
#   group_by(ags5, Year, Week, year_week) %>%
#   summarise(first_dose = sum(kr_erstimpf),
#             second_dose = sum(kr_zweitimpf)) %>%
#   ungroup()
# 
# # Fill in missing LK/year/weeks with 0:
# vacc_dat <- vacc_dat %>%
#   expand(ags5, year_week) %>%
#   left_join(vacc_dat, by = c('ags5', 'year_week')) %>%
#   mutate(Year = str_sub(year_week, 1, 4),
#          Week = str_sub(year_week, 6, 7)) %>%
#   replace_na(list(first_dose = 0, second_dose = 0))
# # Does this make sense? Are there really LK where vaccination didn't start until week 14 of 2021?
# 
# expect_equal(nrow(vacc_dat), length(unique(vacc_dat$year_week)) * length(unique(vacc_dat$ags5)))
# 
# # Convert to cumulative vaccinations:
# vacc_dat <- vacc_dat %>%
#   mutate(date = ISOweek2date(paste0(Year, '-W', Week, '-1')))
# 
# vacc_cumulative1 <- vacc_dat %>%
#   select(!second_dose) %>%
#   pivot_wider(names_from = ags5, values_from = first_dose) %>%
#   arrange(date)
# vacc_cumulative2 <- vacc_dat %>%
#   select(!first_dose) %>%
#   pivot_wider(names_from = ags5, values_from = second_dose) %>%
#   arrange(date)
# 
# vacc_cumulative1_ORIG <- vacc_cumulative1
# vacc_cumulative2_ORIG <- vacc_cumulative2
# 
# for (i in 2:nrow(vacc_cumulative1)) {
#   vacc_cumulative1[i, 5:ncol(vacc_cumulative1)] <- vacc_cumulative1[i, 5:ncol(vacc_cumulative1)] + vacc_cumulative1[i - 1, 5:ncol(vacc_cumulative1)]
# }
# for (i in 2:nrow(vacc_cumulative2)) {
#   vacc_cumulative2[i, 5:ncol(vacc_cumulative2)] <- vacc_cumulative2[i, 5:ncol(vacc_cumulative2)] + vacc_cumulative2[i - 1, 5:ncol(vacc_cumulative2)]
# }
# 
# vacc_cumulative1_CHECK <- vacc_cumulative1
# for (i in nrow(vacc_cumulative1_CHECK):2) {
#   vacc_cumulative1_CHECK[i, 5:ncol(vacc_cumulative1_CHECK)] <- vacc_cumulative1_CHECK[i, 5:ncol(vacc_cumulative1_CHECK)] - vacc_cumulative1_CHECK[i - 1, 5:ncol(vacc_cumulative1_CHECK)]
# }
# vacc_cumulative2_CHECK <- vacc_cumulative2
# for (i in nrow(vacc_cumulative1_CHECK):2) {
#   vacc_cumulative2_CHECK[i, 5:ncol(vacc_cumulative2_CHECK)] <- vacc_cumulative2_CHECK[i, 5:ncol(vacc_cumulative2_CHECK)] - vacc_cumulative2_CHECK[i - 1, 5:ncol(vacc_cumulative2_CHECK)]
# }
# 
# expect_true(all.equal(vacc_cumulative1_ORIG, vacc_cumulative1_CHECK))
# expect_true(all.equal(vacc_cumulative2_ORIG, vacc_cumulative2_CHECK))
# 
# rm(vacc_cumulative1_ORIG, vacc_cumulative1_CHECK, vacc_cumulative2_ORIG, vacc_cumulative2_CHECK, i)
# 
# # Lengthen and rejoin:
# vacc_cumulative1 <- vacc_cumulative1 %>%
#   pivot_longer(-c(year_week, Year, Week, date),
#                names_to = 'ags5',
#                values_to = 'first_dose')
# vacc_cumulative2 <- vacc_cumulative2 %>%
#   pivot_longer(-c(year_week, Year, Week, date),
#                names_to = 'ags5',
#                values_to = 'second_dose')
# 
# vacc_dat <- vacc_cumulative1 %>%
#   inner_join(vacc_cumulative2)
# rm(vacc_cumulative1, vacc_cumulative2)
# 
# # Double-check values:
# vacc_dat %>% filter(date == '2021-07-19') %>% pull(first_dose) %>% sum() # 48463582
# vacc_dat %>% filter(date == '2021-07-19') %>% pull(second_dose) %>% sum() # 41150131
# # First doses are lower than reported in wikipedia, but second doses are higher
# 
# # Get population data and calculate % vaccinated:
# pop_dat <- read_csv2('data/raw/independent_vars/pop_counts_12411-0015.csv', col_names = FALSE, skip = 6, n_max = 476)
# pop_dat <- pop_dat %>%
#   select(-X2) %>%
#   rename(lk = X1, pop = X3) %>%
#   mutate(pop = as.numeric(pop)) %>%
#   filter(!is.na(pop))
# 
# vacc_dat <- vacc_dat %>%
#   rename('lk' = 'ags5') %>%
#   left_join(pop_dat, by = 'lk')
# rm(pop_dat)
# 
# vacc_dat <- vacc_dat %>%
#   mutate(prop1 = first_dose / pop * 100,
#          prop2 = second_dose / pop * 100,
#          .after = second_dose)
# # many exceed 100% - clearly people being vaccinated in BL where they don't live
# # although many of these tend to be cities
# 
# # Explore extent of variability by Bundesland:
# vacc_dat <- vacc_dat %>%
#   mutate(bl = str_sub(lk, 1, 2))
# 
# ggplot(data = vacc_dat, aes(x = date, y = prop1, group = lk, col = bl)) + geom_line() + theme_classic() + facet_wrap(~ bl)
# ggplot(data = vacc_dat, aes(x = date, y = prop2, group = lk, col = bl)) + geom_line() + theme_classic() + facet_wrap(~ bl)
# # can be quite a bit of variability between LK in a BL, esp. for 07 (RP), 08 (BW), 09 (BY), 10 (SL), 12 (BB), 16 (TH)

# Read in and format BL-level data:
vacc_dat_bl <- read_csv('data/raw/cdp/impfdaten.csv')

vacc_dat_bl <- vacc_dat_bl %>%
  select(ags2:datum, bl_erstimpf_quote, bl_zweitimpf_quote) %>%
  rename('bl' = 'ags2',
         'date' = 'datum',
         'prop_first_dose' = 'bl_erstimpf_quote',
         'prop_second_dose' = 'bl_zweitimpf_quote') %>%
  mutate(Year = format(date, '%Y'),
         Week = format(date, '%V'),
         .after = date) %>%
  mutate(Year = if_else(Week == '53', '2020', Year)) %>%
  mutate(year_week = paste(Year, Week, sep = '_'),
         .after = Week)

vacc_dat_bl <- vacc_dat_bl %>%
  filter(year_week != '2021_35')

vacc_dat_bl <- vacc_dat_bl %>%
  group_by(bl, bundesland, Year, Week, year_week) %>%
  summarise(prop_first_dose = max(prop_first_dose),
            prop_second_dose = max(prop_second_dose)) %>%
  # ungroup() %>% group_by(bl) %>% mutate(check = cummax(prop_first_dose)) %>% filter(prop_first_dose != check)
  # ungroup() %>% group_by(bl) %>% mutate(check = cummax(prop_second_dose)) %>% filter(prop_second_dose != check)
  ungroup()
# data are not strictly increasing in the original, but taking the highest value from each week yields monotonically increasing values

expect_equal(nrow(vacc_dat_bl), length(unique(vacc_dat_bl$year_week)) * 16)

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
write_csv(policy_dat, file = 'data/formatted/independent_vars/policy_dat_WEEKLY.csv')
write_csv(vacc_dat_bl, file = 'data/formatted/independent_vars/vacc_dat_WEEKLY_BL.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Clean up
rm(list = ls())

# ---------------------------------------------------------------------------------------------------------------------
