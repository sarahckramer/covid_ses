# ---------------------------------------------------------------------------------------------------------------------
# Code to read in and process/clean all covariate data
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(ISOweek)
library(testthat)
library(jsonlite)
library(gridExtra)

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
vacc_dat_regional_BYAGE <- fromJSON('data/raw/independent_vars/vacc/2_all_county_ageinf_vacc_all_dates.json')

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
vacc_dat_regional_BYAGE <- vacc_dat_regional_BYAGE %>%
  as_tibble() %>%
  select(-ID_State) %>%
  mutate(ID_County = as.character(ID_County),
         ID_County = str_pad(ID_County, width = 5, side = 'left', pad = '0'))

# Add population data and calculate vaccination rates:
pop_dat <- read_csv('data/raw/cdp/bevoelkerung.csv') %>%
  select(ags5, kr_ew_20) %>%
  rename('lk' = 'ags5',
         'pop' = 'kr_ew_20')

pop_dat_60plus <- read_csv('data/raw/cdp/bevoelkerung.csv') %>%
  select(ags5, kr_ew_00u05:kr_ew_80) %>%
  rename('lk' = 'ags5') %>%
  select(lk, kr_ew_60u80, kr_ew_80) %>%
  mutate(pop = kr_ew_60u80 + kr_ew_80) %>%
  select(lk, pop)

pop_dat_15thru59 <- read_csv('data/raw/cdp/bevoelkerung.csv') %>%
  select(ags5, kr_ew_00u05:kr_ew_80) %>%
  rename('lk' = 'ags5') %>%
  select(lk, kr_ew_15u35, kr_ew_35u60) %>%
  mutate(pop = kr_ew_15u35 + kr_ew_35u60) %>%
  select(lk, pop)

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

vacc_dat_regional_60plus <- vacc_dat_regional_BYAGE %>%
  filter(Age_RKI %in% c('60-79', '80-99')) %>%
  group_by(Date, ID_County, County) %>%
  summarise(Vacc_partially = sum(Vacc_partially),
            Vacc_completed = sum(Vacc_completed),
            Vacc_refreshed = sum(Vacc_refreshed)) %>%
  ungroup() %>%
  left_join(pop_dat_60plus, by = c('ID_County' = 'lk')) %>%
  mutate(vacc1_rate = Vacc_partially / pop,
         vacc2_rate = Vacc_completed / pop,
         vacc3_rate = Vacc_refreshed / pop)

vacc_dat_regional_15thru59 <- vacc_dat_regional_BYAGE %>%
  filter(Age_RKI %in% c('15-34', '35-59')) %>%
  group_by(Date, ID_County, County) %>%
  summarise(Vacc_partially = sum(Vacc_partially),
            Vacc_completed = sum(Vacc_completed),
            Vacc_refreshed = sum(Vacc_refreshed)) %>%
  ungroup() %>%
  left_join(pop_dat_15thru59, by = c('ID_County' = 'lk')) %>%
  mutate(vacc1_rate = Vacc_partially / pop,
         vacc2_rate = Vacc_completed / pop,
         vacc3_rate = Vacc_refreshed / pop)

rm(pop_dat, pop_dat_60plus, pop_dat_15thru59, vacc_dat_regional_BYAGE)

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
vacc_dat_regional_60plus <- vacc_dat_regional_60plus %>%
  filter(Date %in% week_ends) %>%
  mutate(Date = as.Date(Date),
         Year = format(Date, '%Y'),
         Week = format(Date, '%V'),
         .after = Date)
vacc_dat_regional_15thru59 <- vacc_dat_regional_15thru59 %>%
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
vacc_dat_regional_60plus <- vacc_dat_regional_60plus %>%
  filter((Year == '2021' &
            Week %in% c('13', '39',
                        '10', '17',
                        '34', '44',
                        '24')) |
           (Year == '2022' &
              Week %in% c('08', '02', '13')))
vacc_dat_regional_15thru59 <- vacc_dat_regional_15thru59 %>%
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

vacc_dat_regional_60plus <- vacc_dat_regional_60plus %>%
  mutate(year_week = paste(Year, Week, sep = '_')) %>%
  select(year_week, ID_County, vacc2_rate) %>%
  pivot_wider(id_cols = ID_County, names_from = year_week, values_from = vacc2_rate)
names(vacc_dat_regional_60plus)[2:11] <- c('vacc_w3_1', 'vacc_w3', 'vacc_w3_2', 'vacc_summer2', 'vacc_w4_1', 'vacc_w4', 'vacc_w4_2', 'vacc_w5_1', 'vacc_w5', 'vacc_w5_2')
vacc_dat_regional_60plus <- vacc_dat_regional_60plus %>%
  select(ID_County, vacc_w3, vacc_w4, vacc_w5, vacc_w3_1, vacc_w3_2, vacc_w4_1, vacc_w4_2, vacc_w5_1, vacc_w5_2, vacc_summer2)

vacc_dat_regional_15thru59 <- vacc_dat_regional_15thru59 %>%
  mutate(year_week = paste(Year, Week, sep = '_')) %>%
  select(year_week, ID_County, vacc2_rate) %>%
  pivot_wider(id_cols = ID_County, names_from = year_week, values_from = vacc2_rate)
names(vacc_dat_regional_15thru59)[2:11] <- c('vacc_w3_1', 'vacc_w3', 'vacc_w3_2', 'vacc_summer2', 'vacc_w4_1', 'vacc_w4', 'vacc_w4_2', 'vacc_w5_1', 'vacc_w5', 'vacc_w5_2')
vacc_dat_regional_15thru59 <- vacc_dat_regional_15thru59 %>%
  select(ID_County, vacc_w3, vacc_w4, vacc_w5, vacc_w3_1, vacc_w3_2, vacc_w4_1, vacc_w4_2, vacc_w5_1, vacc_w5_2, vacc_summer2)

# If coverage >100%, set to 1:
vacc_dat_regional_60plus <- vacc_dat_regional_60plus %>%
  mutate(vacc_w4 = if_else(vacc_w4 > 1, 1, vacc_w4),
         vacc_w5 = if_else(vacc_w5 > 1, 1, vacc_w5))
vacc_dat_regional_15thru59 <- vacc_dat_regional_15thru59 %>%
  mutate(vacc_w5 = if_else(vacc_w5 > 1, 1, vacc_w5))

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
write_csv(vacc_dat_regional_60plus, file = 'data/formatted/independent_vars/BYAGE_60plus_vacc_dat_REGIONAL.csv')
write_csv(vacc_dat_regional_15thru59, file = 'data/formatted/independent_vars/BYAGE_15thru59_vacc_dat_REGIONAL.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Plot timeline of government response/testing/vaccination:
stringency_dat <- read_csv('data/raw/revision1/OxCGRT_compact_national_v1.csv') %>%
  filter(CountryName == 'Germany') %>%
  mutate(Date = as.Date(paste0(str_sub(Date, 1, 4), '-', str_sub(Date, 5, 6), '-', str_sub(Date, 7, 8)), format = '%Y-%m-%d')) %>%
  select(Date:C4M_Flag, `C8EV_International travel controls`, `H2_Testing policy`, `H6M_Facial Coverings`:H7_Flag, contains('V2'), StringencyIndex_Average, ContainmentHealthIndex_Average) %>%
  filter(Date <= '2022-05-29') %>%
  filter(Date >= '2020-02-23')

test_dat <- read_csv('data/raw/revision1/SARS-CoV-2-PCR-Testungen_in_Deutschland.csv') %>%
  select(date, tests_total, tests_positive, tests_positive_ratio) %>%
  mutate(year = as.numeric(str_sub(date, 1, 4)),
         week = as.numeric(str_sub(date, 7, 8))) %>%
  mutate(time = week,
         time = if_else(year == 2021, time + 53, time),
         time = if_else(year == 2022, time + 53 + 52, time)) %>%
  filter(year < 2022 | (year == 2022 & week <= 30))
pop_dat <- read_csv('data/raw/cdp/bevoelkerung.csv') %>%
  select(ags5, kr_ew_20) %>%
  rename('lk' = 'ags5',
         'pop' = 'kr_ew_20')
test_dat <- test_dat %>%
  mutate(pop = sum(pop_dat$pop)) %>%
  mutate(tests_per_pop = tests_total / pop * 10000)

vacc_dat_rki <- read_csv('data/raw/revision1/Deutschland_Landkreise_COVID-19-Impfungen.csv') %>%
  group_by(Impfdatum, Impfschutz) %>%
  summarise(Anzahl = sum(Anzahl)) %>%
  ungroup() %>%
  group_by(Impfschutz) %>%
  mutate(tot_vacc = cumsum(Anzahl)) %>%
  select(-Anzahl) %>%
  ungroup() %>%
  pivot_wider(names_from = Impfschutz, names_prefix = 'vacc_', values_from = tot_vacc) %>%
  select(Impfdatum:vacc_2)
week_ends <- unique(vacc_dat_rki$Impfdatum)[format(unique(as.Date(vacc_dat_rki$Impfdatum)), '%w') == '0']
vacc_dat_rki <- vacc_dat_rki %>%
  filter(Impfdatum %in% week_ends) %>%
  mutate(Date = as.Date(Impfdatum),
         Year = format(Date, '%Y'),
         Week = format(Date, '%V'),
         .after = Date) %>%
  select(Date:Week, vacc_1:vacc_2) %>%
  mutate(Year = if_else(Week == 53, '2020', Year),
         Year = if_else(Week == 52 & Year == '2022', '2021', Year)) %>%
  mutate(year = as.numeric(Year),
         week = as.numeric(Week))
rm(week_ends)
vacc_dat_rki <- vacc_dat_rki %>%
  filter(Year %in% c('2020', '2021') | (Year == '2022' & as.numeric(Week) <= 21))
vacc_dat_rki <- vacc_dat_rki %>% mutate(pop = sum(pop_dat$pop))
vacc_dat_rki <- test_dat %>%
  select(year:time) %>%
  left_join(vacc_dat_rki, by = c('year', 'week'))

x_breaks <- c(9, 17 + 5/7, 26 + 3/7, 35 + 2/7, 44,
              52 + 5/7, 61 + 1/7, 69 + 6/7, 78 + 4/7, 87 + 3/7, 96 + 1/7,
              104 + 6/7, 113 + 2/7, 122)
x_labs <- c('Mar 2020', 'May 2020', 'Jul 2020', 'Sep 2020', 'Nov 2020',
            'Jan 2021', 'Mar 2021', 'May 2021', 'Jul 2021', 'Sep 2021', 'Nov 2021',
            'Jan 2022', 'Mar 2022', 'May 2022')

p_timeline1 <- ggplot(data = stringency_dat) +
  geom_rect(data = stringency_dat[1, ], aes(xmin = as.Date('2020-02-23'), xmax = as.Date('2020-05-17'), ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#e41a1c') +
  geom_rect(data = stringency_dat[1, ], aes(xmin = as.Date('2020-09-27'), xmax = as.Date('2021-02-28'), ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#4daf4a') +
  geom_rect(data = stringency_dat[1, ], aes(xmin = as.Date('2021-02-28'), xmax = as.Date('2021-05-30'), ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#377eb8') +
  geom_rect(data = stringency_dat[1, ], aes(xmin = as.Date('2021-08-08'), xmax = as.Date('2021-12-26'), ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#984ea3') +
  geom_rect(data = stringency_dat[1, ], aes(xmin = as.Date('2021-12-26'), xmax = as.Date('2022-05-29'), ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#f781bf') +
  geom_vline(xintercept = as.Date('2022-04-03'), lty = 2, lwd = 1.0, col = 'gray70') +
  geom_line(aes(x = Date, y = StringencyIndex_Average), lwd = 1.0) +
  geom_segment(x = as.Date('2020-03-22'), xend = as.Date('2020-04-20'), y = 92, yend = 92, lwd = 2.0) +
  geom_segment(x = as.Date('2020-11-02'), xend = as.Date('2020-12-13'), y = 92, yend = 92, lwd = 2.0) +
  geom_segment(x = as.Date('2020-11-14'), xend = as.Date('2020-11-17'), y = 92, yend = 92, lwd = 2.0, col = 'white') +
  geom_segment(x = as.Date('2020-11-28'), xend = as.Date('2020-12-01'), y = 92, yend = 92, lwd = 2.0, col = 'white') +
  geom_segment(x = as.Date('2020-12-16'), xend = as.Date('2021-03-03'), y = 92, yend = 92, lwd = 2.0) +
  geom_label(label = 'LD 1', x = as.Date('2020-03-15'), y = 92, size = 3, hjust = 1, inherit.aes = FALSE) +
  geom_label(label = 'LD Light', x = as.Date('2020-10-26'), y = 92, size = 3, hjust = 1, inherit.aes = FALSE) +
  geom_label(label = 'LD 2', x = as.Date('2021-03-10'), y = 92, size = 3, hjust = 0, inherit.aes = FALSE) +
  geom_label(label = 'End of most control measures', x = as.Date('2022-03-27'), y = 92, size = 3, hjust = 1, inherit.aes = FALSE) +
  # geom_vline(xintercept = as.Date('2022-04-03'), lty = 2, lwd = 1.0, col = 'gray70') +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 35, vjust = 0.62),
        plot.tag = element_text(size = 20),
        plot.tag.position = c(0.065, 0.95)) +
  scale_x_date(breaks = c(as.Date('2020-03-01'), as.Date('2020-05-01'), as.Date('2020-07-01'), as.Date('2020-09-01'), as.Date('2020-11-01'),
                          as.Date('2021-01-01'), as.Date('2021-03-01'), as.Date('2021-05-01'), as.Date('2021-07-01'), as.Date('2021-09-01'),
                          as.Date('2021-11-01'), as.Date('2022-01-01'), as.Date('2022-03-01'), as.Date('2022-05-01')),
               labels = x_labs) +
  scale_y_continuous(n.breaks = 6, limits = c(0, 100), expand = c(0, 0)) +
  labs(x = 'Date', y = 'Stringency Index', tag = 'A')

p_timeline2 <- ggplot(data = test_dat %>% filter(time <= 126)) +
  geom_rect(data = test_dat[1, ], aes(xmin = 9, xmax = 20, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#e41a1c') +
  geom_rect(data = test_dat[1, ], aes(xmin = 39, xmax = 61, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#4daf4a') +
  geom_rect(data = test_dat[1, ], aes(xmin = 61, xmax = 74, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#377eb8') +
  geom_rect(data = test_dat[1, ], aes(xmin = 84, xmax = 104, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#984ea3') +
  geom_rect(data = test_dat[1, ], aes(xmin = 104, xmax = 126, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#f781bf') +
  geom_line(aes(x = time, y = tests_per_pop), lwd = 1.0) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 35, vjust = 0.62),
        plot.tag = element_text(size = 20),
        plot.tag.position = c(0.065, 0.95)) +
  scale_x_continuous(breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(n.breaks = 10, limits = c(0, 320), expand = c(0, 0)) +
  labs(x = 'Date', y = '# of Tests per 10,000 Population', tag = 'B')

p_timeline3 <- ggplot(vacc_dat_rki %>% filter(time <= 126)) +
  geom_rect(data = vacc_dat_rki[1, ], aes(xmin = 9, xmax = 20, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#e41a1c') +
  geom_rect(data = vacc_dat_rki[1, ], aes(xmin = 39, xmax = 61, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#4daf4a') +
  geom_rect(data = vacc_dat_rki[1, ], aes(xmin = 61, xmax = 74, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#377eb8') +
  geom_rect(data = vacc_dat_rki[1, ], aes(xmin = 84, xmax = 104, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#984ea3') +
  geom_rect(data = vacc_dat_rki[1, ], aes(xmin = 104, xmax = 126, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#f781bf') +
  geom_vline(xintercept = 52, lty = 2, lwd = 1.0, col = 'gray70') +
  geom_vline(xintercept = 76, lty = 2, lwd = 1.0, col = 'gray70') +
  geom_vline(xintercept = 103, lty = 2, lwd = 1.0, col = 'gray70') +
  geom_point(data = vacc_dat_regional, x = 68, aes(y = vacc_w3 * 100), size = 1.5, alpha = 0.01) +
  geom_point(data = vacc_dat_regional, x = 94, aes(y = vacc_w4 * 100), size = 1.5, alpha = 0.01) +
  geom_point(data = vacc_dat_regional, x = 115, aes(y = vacc_w5 * 100), size = 1.5, alpha = 0.01) +
  geom_line(aes(x = time, y = vacc_2 / pop * 100), lwd = 1.0) +
  geom_label(label = 'First vaccines administered', x = 51.25, y = 90, size = 3, hjust = 1, inherit.aes = FALSE) +
  geom_label(label = 'End of prioritization', x = 75.25, y = 90, size = 3, hjust = 1, inherit.aes = FALSE) +
  geom_label(label = 'Vaccines available to all age 5+', x = 102.25, y = 90, size = 3, hjust = 1, inherit.aes = FALSE) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 35, vjust = 0.62),
        plot.tag = element_text(size = 20),
        plot.tag.position = c(0.065, 0.95)) +
  scale_x_continuous(breaks = x_breaks, labels = x_labs) +
  scale_y_continuous(n.breaks = 6, limits = c(0, 100), expand = c(0, 0)) +
  labs(x = 'Date', y = '% Fully Vaccinated', tag = 'C')

grid.arrange(p_timeline1, p_timeline2, p_timeline3, ncol = 1)

# ---------------------------------------------------------------------------------------------------------------------

# Clean up
rm(list = ls())

# ---------------------------------------------------------------------------------------------------------------------
