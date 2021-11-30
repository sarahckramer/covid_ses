# ---------------------------------------------------------------------------------------------------------------------
# Code to read in and format cumulative case/death data, and add lat/long/SES data
# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data

# Load cumulative data:
dat_cumulative <- read_csv('data/formatted/cumulative_cases_and_deaths.csv')

# Format:
dat_cumulative <- dat_cumulative %>%
  mutate(ags2 = factor(ags2),
         bundesland = factor(bundesland),
         lk = factor(lk))

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

# Get CENTROID coordinates:
map_base[, c('long', 'lat')] <- st_centroid(map_base) %>% st_transform(., '+proj=longlat') %>% st_coordinates()
# Note: eventually probably want to do population center and not centroid - centroids aren't necessarily within LK

# map_cent <- st_centroid(map_base)
# ggplot() + geom_sf(data = map_base) + geom_sf(data = map_cent) + theme_void()

# ---------------------------------------------------------------------------------------------------------------------

# Join map data to cases/deaths data

# Add lat/long to deaths/cases data frame:
dat_cumulative <- dat_cumulative %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = c('lk' = 'ARS')) %>%
  select(-geometry)
rm(map_base)

# ---------------------------------------------------------------------------------------------------------------------

# For mortality analyses, only want where cases > 0

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

# ---------------------------------------------------------------------------------------------------------------------

# Read in and join predictor variables

# Load predictor data:
inkar_dat <- read_csv('data/formatted/independent_vars/ses_independent_variables.csv')

# Join with case/death data:
dat_cumulative <- dat_cumulative %>%
  left_join(inkar_dat, by = c('lk' = 'lk_code')) %>%
  select(-c(lk_name, lk_type))
rm(inkar_dat)

# ---------------------------------------------------------------------------------------------------------------------
