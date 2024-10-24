# ---------------------------------------------------------------------------------------------------------------------
# Code to read in and format cumulative case/death data, and add lat/long/SES data
# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data

# Should maps be removed from environment?:
if (!exists('keep_map')) {
  keep_map <- FALSE
}

# Load cumulative data:
dat_cumulative <- read_csv('data/formatted/STAND_cumulative_cases_and_deaths.csv')
dat_agestrat <- read_csv('data/formatted/BYAGE_cumulative_cases_and_deaths.csv')

# Format data based on desired analysis:
if (!exists('sens_age')) {
  
  # Check no zeros in case data:
  expect_true(dat_cumulative %>%
                filter(val == 0) %>%
                filter(str_detect(outcome, 'cases')) %>%
                nrow() == 0)
  
} else {
  
  if (sens_age == 'age_60plus') {
    
    # Limit to 60+ age group:
    dat_cumulative <- dat_agestrat %>%
      filter(age == '60') %>%
      select(-age)
    
    # Check no zeros in case data:
    expect_true(dat_cumulative %>%
                  filter(val == 0) %>%
                  filter(str_detect(outcome, 'cases') &
                           !str_detect(outcome, 'summer')) %>%
                  nrow() == 0)
    
    # If CFR is 100, set to NA:
    dat_cumulative <- dat_cumulative %>%
      mutate(val = if_else(val == 100 & str_detect(outcome, 'cfr'), NA, val))
    # occurs once for wave 1_2
    
  } else if (sens_age == 'age_15thru59') {
    
    # Limit to 15-59 age group:
    dat_cumulative <- dat_agestrat %>%
      filter(age == '15') %>%
      select(-age)
    
    # Check no zeros in case data:
    expect_true(dat_cumulative %>%
                  filter(val == 0) %>%
                  filter(str_detect(outcome, 'cases')) %>%
                  nrow() == 0)
    
  }
  
}
rm(dat_agestrat)

# Format:
dat_cumulative <- dat_cumulative %>%
  mutate(ags2 = factor(ags2),
         bundesland = factor(bundesland),
         lk = factor(lk)) %>%
  pivot_wider(names_from = outcome,
              values_from = val)

# Remove Landkreise (LK(s)) that were later merged:
dat_cumulative <- dat_cumulative %>%
  filter(!(lk %in% c('16056', '16063'))) # Eisenach and Wartburgkreis

# ---------------------------------------------------------------------------------------------------------------------

# Get map data and relevant coordinates

# Read in map data:
map_base <- st_read(dsn = 'data/raw/map/vg2500_12-31.gk3.shape/vg2500/VG2500_KRS.shp')
expect_true(all(unique(dat_cumulative$lk) %in% unique(map_base$ARS)))

# Get neighborhood info:
nb <- spdep::poly2nb(map_base, row.names = map_base$ARS)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

# Check whether centroids are within LK:
map_cent <- st_centroid(map_base)
centroid_not_contained <- map_cent %>%
  mutate(check = map_base$ARS[as.integer(st_intersects(geometry, map_base))]) %>%
  filter(ARS != check) %>%
  pull(ARS)

# If so, use centroids; otherwise, use "point_on_surface":
map_base[, c('long', 'lat')] <- st_centroid(map_base) %>% st_transform(., '+proj=longlat') %>% st_coordinates()
map_base[, c('long_ALT', 'lat_ALT')] <- st_point_on_surface(map_base) %>% st_transform(., '+proj=longlat') %>% st_coordinates()
# https://gis.stackexchange.com/questions/76498/how-is-st-pointonsurface-calculated

map_base_new <- map_base %>%
  mutate(long = if_else(ARS %in% centroid_not_contained, long_ALT, long),
         lat = if_else(ARS %in% centroid_not_contained, lat_ALT, lat)) %>%
  dplyr::select(-c(long_ALT:lat_ALT))
expect_true(length(which(map_base_new$long != map_base$long)) == length(centroid_not_contained))
expect_true(length(which(map_base_new$lat != map_base$lat)) == length(centroid_not_contained))

map_base <- map_base_new
rm(map_base_new, map_cent, centroid_not_contained)

# Check that replacement results in points within each LK:
map_base <- map_base %>%
  st_transform(., '+proj=longlat')
map_base %>%
  as_tibble() %>%
  dplyr::select(long:lat) %>%
  st_as_sf(coords = c('long', 'lat'), crs = st_crs(map_base)) %>%
  mutate(check = map_base$ARS[as.integer(st_intersects(geometry, map_base %>%
                                                         st_transform(., '+proj=longlat')))]) %>%
  pull(check) %>%
  all.equal(map_base$ARS) %>%
  expect_true()

# # Plot:
# ggplot() + geom_sf(data = map_base) + geom_point(data = map_base, aes(x = long, y = lat)) + theme_void()

# ---------------------------------------------------------------------------------------------------------------------

# Join map data to cases/deaths data

# Add lat/long to deaths/cases data frame:
dat_cumulative <- dat_cumulative %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = c('lk' = 'ARS')) %>%
  dplyr::select(-geometry)
if (!keep_map) {
  rm(map_base)
}

# ---------------------------------------------------------------------------------------------------------------------

# Read in and join vaccination data

# Load vaccination data:
vacc_dat <- read_csv('data/formatted/independent_vars/vacc_dat.csv')
# vacc_w3: Estimated rate of full vaccination 2 weeks before the midpoint of wave 3
# vacc_w4: Estimated rate of full vaccination 2 weeks before the midpoint of wave 4
# vacc_w3_1: Estimated rate of full vaccination 2 weeks before the midpoint of wave 3, part 1
# vacc_w3_2: Estimated rate of full vaccination 2 weeks before the midpoint of wave 3, part 2
# vacc_w4_1: Estimated rate of full vaccination 2 weeks before the midpoint of wave 4, part 1
# vacc_w4_2: Estimated rate of full vaccination 2 weeks before the midpoint of wave 4, part 2
# vacc_summer2: Estimated rate of full vaccination 2 weeks before the midpoint of summer 2021 (between waves 3 and 4)

vacc_dat_regional <- read_csv('data/formatted/independent_vars/vacc_dat_REGIONAL.csv')

# Also get vaccination data by age, if needed:
if (exists('sens_age')) {
  if (sens_age == 'age_60plus') {
    vacc_dat_regional <- read_csv('data/formatted/independent_vars/BYAGE_60plus_vacc_dat_REGIONAL.csv')
  } else if (sens_age == 'age_15thru59') {
    vacc_dat_regional <- read_csv('data/formatted/independent_vars/BYAGE_15thru59_vacc_dat_REGIONAL.csv')
  }
}

# Join with case/death data:
dat_cumulative <- dat_cumulative %>%
  left_join(vacc_dat, by = c('lk' = 'ID_County'))
rm(vacc_dat)

# Join regional vaccination data as well?:
names(vacc_dat_regional) <- paste(names(vacc_dat_regional), 'reg', sep = '_')
dat_cumulative <- dat_cumulative %>%
  left_join(vacc_dat_regional, by = c('lk' = 'ID_County_reg'))

rm(vacc_dat_regional)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and join predictor variables

# Load predictor data:
inkar_dat <- read_csv('data/formatted/independent_vars/ses_independent_variables.csv')
# hosp_beds: Number of hospital beds per 1000 population
# perc_lessthan18: Percentage of the population under the age of 18
# perc_18to64: Percentage of the population aged 18 through 64
# perc_65plus: Percentage of the population aged 65 or older
# care_home_beds: Number of spots in long-term care facilities per 10,000 population
# pop_dens: Population density (100's of people per square kilometer of settlement/transportation areas)
# living_area: Average amount of living space per person in square meters
# perc_service: Percentage of workers employed in person-related service jobs (“personenbezogene Dienstleistungsberufe”)
# perc_production: Percentage of workers employed in production-oriented jobs (“Produktionsberufe”)
# GISD_Score: German Index of Socioeconomic Deprivation score

# Join with case/death data:
dat_cumulative <- dat_cumulative %>%
  left_join(inkar_dat, by = c('lk' = 'lk_code')) %>%
  dplyr::select(-c(lk_name, lk_type))
rm(inkar_dat)

# ---------------------------------------------------------------------------------------------------------------------

# Confirm that data were loaded without issue:
print('Done.')
