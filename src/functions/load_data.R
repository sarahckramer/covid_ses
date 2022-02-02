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

# Check no zeros in case data:
expect_true(dat_cumulative %>%
              filter(val == 0) %>%
              filter(str_detect(outcome, 'cases')) %>%
              nrow() == 0)

# # Where do zeroes occur?:
# dat_cumulative %>%
#   filter(val == 0) %>%
#   pull(outcome) %>%
#   table()
# # No 0s in cumulative case counts; only from deaths and IFR in wave 1

# Format:
dat_cumulative <- dat_cumulative %>%
  mutate(ags2 = factor(ags2),
         bundesland = factor(bundesland),
         lk = factor(lk)) %>%
  pivot_wider(names_from = outcome,
              values_from = val)

# Get total cases/deaths in wave1+summer:
dat_cumulative <- dat_cumulative %>%
  mutate(cases_pre_rate = (cases_wave1 + cases_summer) / pop * 10000,
         deaths_pre_rate = (deaths_wave1 + deaths_summer) / pop * 10000)

# ---------------------------------------------------------------------------------------------------------------------

# Get map data and relevant coordinates

# Read in map data:
map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')

expect_true(all(unique(map_base$ARS) %in% unique(dat_cumulative$lk)))
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

# Read in and join predictor variables

# Load predictor data:
inkar_dat <- read_csv('data/formatted/independent_vars/ses_independent_variables.csv')

# Join with case/death data:
dat_cumulative <- dat_cumulative %>%
  left_join(inkar_dat, by = c('lk' = 'lk_code')) %>%
  dplyr::select(-c(lk_name, lk_type))
rm(inkar_dat)

# ---------------------------------------------------------------------------------------------------------------------

# Confirm that data were loaded without issue:
print('Done.')
