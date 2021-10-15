# ---------------------------------------------------------------------------------------------------------------------
# Build GAMs exploring the role of individual predictors
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(ggeffects)
library(sf)
library(testthat)
library(spdep)
library(viridis)
library(gridExtra)
library(pomp)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data

# Read in weekly data:
dat_inc_wk <- read_csv('data/formatted/weekly_covid_deaths_by_lk_INCIDENT.csv')
dat_inc_wk_cdp <- read_csv('data/formatted/weekly_covid_deaths_by_lk_CDP.csv')

# Read in monthly data:
dat_inc_mo <- read_csv('data/formatted/monthly_covid_deaths_by_lk_INCIDENT.csv')
dat_inc_mo_cdp <- read_csv('data/formatted/monthly_covid_deaths_by_lk_CDP.csv')

# Format:
dat_inc_wk <- dat_inc_wk %>%
  mutate(time = if_else(Year == 2020, Week, Week + 53)) %>%
  mutate(deaths = ifelse(deaths > cases, NA, deaths)) %>%
  mutate(death_rate = deaths / pop * 100000,
         ifr = deaths / cases * 100)
dat_inc_wk_cdp <- dat_inc_wk_cdp %>%
  mutate(Week = as.numeric(Week)) %>%
  mutate(time = if_else(Year == 2020, Week, Week + 53)) %>%
  mutate(deaths = ifelse(deaths > cases, NA, deaths)) %>%
  mutate(death_rate = deaths / pop * 100000,
         ifr = deaths / cases * 100)

dat_inc_mo <- dat_inc_mo %>%
  mutate(Month = as.numeric(Month)) %>%
  mutate(time = if_else(Year == 2020, Month, Month + 12)) %>%
  mutate(deaths = ifelse(deaths > cases, NA, deaths)) %>%
  mutate(death_rate = deaths / pop * 100000,
         ifr = deaths / cases * 100)
dat_inc_mo_cdp <- dat_inc_mo_cdp %>%
  mutate(Month = as.numeric(Month)) %>%
  mutate(time = if_else(Year == 2020, Month, Month + 12)) %>%
  mutate(deaths = ifelse(deaths > cases, NA, deaths)) %>%
  mutate(death_rate = deaths / pop * 100000,
         ifr = deaths / cases * 100)

# Add column for Bundesland:
dat_inc_wk <- dat_inc_wk %>%
  mutate(bundesland = lk,
         bundesland = if_else(str_starts(lk, '01'), 'SchleswigHolstein', bundesland),
         bundesland = if_else(str_starts(lk, '02'), 'Hamburg', bundesland),
         bundesland = if_else(str_starts(lk, '03'), 'Niedersachsen', bundesland),
         bundesland = if_else(str_starts(lk, '04'), 'Bremen', bundesland),
         bundesland = if_else(str_starts(lk, '05'), 'NordrheinWestfalen', bundesland),
         bundesland = if_else(str_starts(lk, '06'), 'Hessen', bundesland),
         bundesland = if_else(str_starts(lk, '07'), 'RheinlandPfalz', bundesland),
         bundesland = if_else(str_starts(lk, '08'), 'BadenWuerttemberg', bundesland),
         bundesland = if_else(str_starts(lk, '09'), 'Bayern', bundesland),
         bundesland = if_else(str_starts(lk, '10'), 'Saarland', bundesland),
         bundesland = if_else(str_starts(lk, '11'), 'Berlin', bundesland),
         bundesland = if_else(str_starts(lk, '12'), 'Brandenburg', bundesland),
         bundesland = if_else(str_starts(lk, '13'), 'MecklenburgVorpommern', bundesland),
         bundesland = if_else(str_starts(lk, '14'), 'Sachsen', bundesland),
         bundesland = if_else(str_starts(lk, '15'), 'SachsenAnhalt', bundesland),
         bundesland = if_else(str_starts(lk, '16'), 'Thueringen', bundesland))
dat_inc_wk_cdp <- dat_inc_wk_cdp %>%
  mutate(bundesland = lk,
         bundesland = if_else(str_starts(lk, '01'), 'SchleswigHolstein', bundesland),
         bundesland = if_else(str_starts(lk, '02'), 'Hamburg', bundesland),
         bundesland = if_else(str_starts(lk, '03'), 'Niedersachsen', bundesland),
         bundesland = if_else(str_starts(lk, '04'), 'Bremen', bundesland),
         bundesland = if_else(str_starts(lk, '05'), 'NordrheinWestfalen', bundesland),
         bundesland = if_else(str_starts(lk, '06'), 'Hessen', bundesland),
         bundesland = if_else(str_starts(lk, '07'), 'RheinlandPfalz', bundesland),
         bundesland = if_else(str_starts(lk, '08'), 'BadenWuerttemberg', bundesland),
         bundesland = if_else(str_starts(lk, '09'), 'Bayern', bundesland),
         bundesland = if_else(str_starts(lk, '10'), 'Saarland', bundesland),
         bundesland = if_else(str_starts(lk, '11'), 'Berlin', bundesland),
         bundesland = if_else(str_starts(lk, '12'), 'Brandenburg', bundesland),
         bundesland = if_else(str_starts(lk, '13'), 'MecklenburgVorpommern', bundesland),
         bundesland = if_else(str_starts(lk, '14'), 'Sachsen', bundesland),
         bundesland = if_else(str_starts(lk, '15'), 'SachsenAnhalt', bundesland),
         bundesland = if_else(str_starts(lk, '16'), 'Thueringen', bundesland))

dat_inc_mo <- dat_inc_mo %>%
  mutate(bundesland = lk,
         bundesland = if_else(str_starts(lk, '01'), 'SchleswigHolstein', bundesland),
         bundesland = if_else(str_starts(lk, '02'), 'Hamburg', bundesland),
         bundesland = if_else(str_starts(lk, '03'), 'Niedersachsen', bundesland),
         bundesland = if_else(str_starts(lk, '04'), 'Bremen', bundesland),
         bundesland = if_else(str_starts(lk, '05'), 'NordrheinWestfalen', bundesland),
         bundesland = if_else(str_starts(lk, '06'), 'Hessen', bundesland),
         bundesland = if_else(str_starts(lk, '07'), 'RheinlandPfalz', bundesland),
         bundesland = if_else(str_starts(lk, '08'), 'BadenWuerttemberg', bundesland),
         bundesland = if_else(str_starts(lk, '09'), 'Bayern', bundesland),
         bundesland = if_else(str_starts(lk, '10'), 'Saarland', bundesland),
         bundesland = if_else(str_starts(lk, '11'), 'Berlin', bundesland),
         bundesland = if_else(str_starts(lk, '12'), 'Brandenburg', bundesland),
         bundesland = if_else(str_starts(lk, '13'), 'MecklenburgVorpommern', bundesland),
         bundesland = if_else(str_starts(lk, '14'), 'Sachsen', bundesland),
         bundesland = if_else(str_starts(lk, '15'), 'SachsenAnhalt', bundesland),
         bundesland = if_else(str_starts(lk, '16'), 'Thueringen', bundesland))
dat_inc_mo_cdp <- dat_inc_mo_cdp %>%
  mutate(bundesland = lk,
         bundesland = if_else(str_starts(lk, '01'), 'SchleswigHolstein', bundesland),
         bundesland = if_else(str_starts(lk, '02'), 'Hamburg', bundesland),
         bundesland = if_else(str_starts(lk, '03'), 'Niedersachsen', bundesland),
         bundesland = if_else(str_starts(lk, '04'), 'Bremen', bundesland),
         bundesland = if_else(str_starts(lk, '05'), 'NordrheinWestfalen', bundesland),
         bundesland = if_else(str_starts(lk, '06'), 'Hessen', bundesland),
         bundesland = if_else(str_starts(lk, '07'), 'RheinlandPfalz', bundesland),
         bundesland = if_else(str_starts(lk, '08'), 'BadenWuerttemberg', bundesland),
         bundesland = if_else(str_starts(lk, '09'), 'Bayern', bundesland),
         bundesland = if_else(str_starts(lk, '10'), 'Saarland', bundesland),
         bundesland = if_else(str_starts(lk, '11'), 'Berlin', bundesland),
         bundesland = if_else(str_starts(lk, '12'), 'Brandenburg', bundesland),
         bundesland = if_else(str_starts(lk, '13'), 'MecklenburgVorpommern', bundesland),
         bundesland = if_else(str_starts(lk, '14'), 'Sachsen', bundesland),
         bundesland = if_else(str_starts(lk, '15'), 'SachsenAnhalt', bundesland),
         bundesland = if_else(str_starts(lk, '16'), 'Thueringen', bundesland))

# Get Landkreise as factor:
dat_inc_wk <- dat_inc_wk %>%
  mutate(ARS = factor(lk),
         bundesland = factor(bundesland))
dat_inc_wk_cdp <- dat_inc_wk_cdp %>%
  mutate(ARS = factor(lk),
         bundesland = factor(bundesland))
dat_inc_mo <- dat_inc_mo %>%
  mutate(ARS = factor(lk),
         bundesland = factor(bundesland))
dat_inc_mo_cdp <- dat_inc_mo_cdp %>%
  mutate(ARS = factor(lk),
         bundesland = factor(bundesland))

# #Plot:
# p1 <- ggplot(data = dat_inc_wk, aes(x = time, y = case_rate, group = lk)) +
#   geom_line(alpha = 0.2) + theme_classic()
# p2 <- ggplot(data = dat_inc_wk, aes(x = time, y = ifr, group = lk)) +
#   geom_line(alpha = 0.2) + theme_classic()
# p3 <- ggplot(data = dat_inc_wk, aes(x = time, y = death_rate, group = lk)) +
#   geom_line(alpha = 0.2) + theme_classic()
# grid.arrange(p1, p2, p3, ncol = 1)
# 
# p1 <- ggplot(data = dat_inc_wk_cdp, aes(x = time, y = case_rate, group = lk)) +
#   geom_line(alpha = 0.2) + theme_classic()
# p2 <- ggplot(data = dat_inc_wk_cdp, aes(x = time, y = ifr, group = lk)) +
#   geom_line(alpha = 0.2) + theme_classic()
# p3 <- ggplot(data = dat_inc_wk_cdp, aes(x = time, y = death_rate, group = lk)) +
#   geom_line(alpha = 0.2) + theme_classic()
# grid.arrange(p1, p2, p3, ncol = 1)
# 
# p1 <- ggplot(data = dat_inc_mo, aes(x = time, y = case_rate, group = lk)) +
#   geom_line(alpha = 0.2) + theme_classic()
# p2 <- ggplot(data = dat_inc_mo, aes(x = time, y = ifr, group = lk)) +
#   geom_line(alpha = 0.2) + theme_classic()
# p3 <- ggplot(data = dat_inc_mo, aes(x = time, y = death_rate, group = lk)) +
#   geom_line(alpha = 0.2) + theme_classic()
# grid.arrange(p1, p2, p3, ncol = 1)
# 
# p1 <- ggplot(data = dat_inc_mo_cdp, aes(x = time, y = case_rate, group = lk)) +
#   geom_line(alpha = 0.2) + theme_classic()
# p2 <- ggplot(data = dat_inc_mo_cdp, aes(x = time, y = ifr, group = lk)) +
#   geom_line(alpha = 0.2) + theme_classic()
# p3 <- ggplot(data = dat_inc_mo_cdp, aes(x = time, y = death_rate, group = lk)) +
#   geom_line(alpha = 0.2) + theme_classic()
# grid.arrange(p1, p2, p3, ncol = 1)
# 
# rm(p1, p2, p3)

# ---------------------------------------------------------------------------------------------------------------------

# Get map data and relevant coordinates

# Read in map data:
map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')

expect_true(all(unique(map_base$ARS) %in% unique(dat_inc_wk$lk)))
expect_true(all(unique(dat_inc_wk$lk) %in% unique(map_base$ARS)))

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
dat_inc_wk <- dat_inc_wk %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)
dat_inc_wk_cdp <- dat_inc_wk_cdp %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)
dat_inc_mo <- dat_inc_mo %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)
dat_inc_mo_cdp <- dat_inc_mo_cdp %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)

# ---------------------------------------------------------------------------------------------------------------------

# For mortality analyses, only want where cases > 0

dat_inc_wk_fromCases <- dat_inc_wk %>%
  filter(cases > 0)
dat_inc_wk_cdp_fromCases <- dat_inc_wk_cdp %>%
  filter(cases > 0)
dat_inc_mo_fromCases <- dat_inc_mo %>%
  filter(cases > 0)
dat_inc_mo_cdp_fromCases <- dat_inc_mo_cdp %>%
  filter(cases > 0)

# ---------------------------------------------------------------------------------------------------------------------

# Organize data into lists

dat_wk <- list(dat_inc_wk, dat_inc_wk_fromCases, dat_inc_wk_cdp, dat_inc_wk_cdp_fromCases)
dat_mo <- list(dat_inc_mo, dat_inc_mo_fromCases, dat_inc_mo_cdp, dat_inc_mo_cdp_fromCases)
rm(dat_inc_wk, dat_inc_wk_fromCases, dat_inc_wk_cdp, dat_inc_wk_cdp_fromCases,
   dat_inc_mo, dat_inc_mo_fromCases, dat_inc_mo_cdp, dat_inc_mo_cdp_fromCases)

# ---------------------------------------------------------------------------------------------------------------------

# Limit to 2020 for now (to avoid influence of vaccination)

# Remove data from 2021:
dat_wk <- lapply(dat_wk, function(ix) {
  ix %>% filter(Year == 2020)
})
dat_mo <- lapply(dat_mo, function(ix) {
  ix %>% filter(Year == 2020)
})

# Get number of regions and weeks:
lapply(dat_wk, function(ix) {
  length(unique(ix$ARS))
}) %>% print()
lapply(dat_mo, function(ix) {
  length(unique(ix$ARS))
}) %>% print()

lapply(dat_wk, function(ix) {
  length(unique(ix$time))
}) %>% print()
# 41, 41, 44, 44
lapply(dat_mo, function(ix) {
  length(unique(ix$time))
}) %>% print()
# 10, 10, 10, 10

# ---------------------------------------------------------------------------------------------------------------------

# Read in and join predictor variables

# Load predictor data:
inkar_dat <- read_csv('data/formatted/independent_vars/ses_independent_variables.csv')
mobility_dat <- read_csv('data/formatted/independent_vars/mobility_dat_WEEKLY.csv')
policy_dat <- read_csv('data/formatted/independent_vars/policy_dat_WEEKLY.csv')
stringency_dat <- read_csv('data/formatted/independent_vars/stringency_and_containment_indices.csv')

# predictors: perc_65plus, perc_women, pop_dens, living_area, perc_service, perc_production, care_home_beds, hosp_beds, GISD_Score
# (commuters_in, commuters_out)
# (mobility? policy (masks)?)
# (stringency?)

# Join with incidence data:
dat_wk <- lapply(dat_wk, function(ix) {
  ix %>%
    left_join(inkar_dat, by = c('lk' = 'lk_code')) %>%
    left_join(mobility_dat, by = c('lk' = 'lk_code',
                                   'Week' = 'week',
                                   'Year' = 'year')) %>%
    left_join(policy_dat %>%
                mutate(Week = as.numeric(Week)), by = c('lk' = 'ags5',
                                                        'Week' = 'Week',
                                                        'Year' = 'Year')) %>%
    left_join(stringency_dat %>%
                mutate(Week = as.numeric(Week)),
              by = c('Week', 'Year'))
})

dat_mo <- lapply(dat_mo, function(ix) {
  ix %>%
    left_join(inkar_dat, by = c('lk' = 'lk_code'))
})

# ---------------------------------------------------------------------------------------------------------------------

# Formulate and fit models

# Without predictors:
n1a <- bake(file = 'results/fitted_models/n1a_monthly.rds',
            expr = {
              bam(cases ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Month, k = 10, bs = 'cr') +
                    ti(long, lat, Month, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(200, 10)) +
                    s(bundesland, bs = 're', k = 16) + offset(log(pop)),
                  data = dat_mo[[3]], family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
            }
)
n1b <- bake(file = 'results/fitted_models/n1b_monthly.rds',
            expr = {
              bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Month, k = 10, bs = 'cr') +
                    ti(long, lat, Month, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(200, 10)) +
                    s(bundesland, bs = 're', k = 16) + offset(log(cases)),
                  data = dat_mo[[2]], family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
            }
)
n1b_cdp <- bake(file = 'results/fitted_models/n1b_cdp_monthly.rds',
                expr = {
                  bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Month, k = 10, bs = 'cr') +
                        ti(long, lat, Month, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(200, 10)) +
                        s(bundesland, bs = 're', k = 16) + offset(log(cases)),
                      data = dat_mo[[4]], family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
                }
)

# Monthly:
n1b_cdp_1 <- bake(file = 'results/fitted_models/n1b_cdp_1.rds',
                  expr = {
                    bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Month, k = 10, bs = 'cr') +
                          ti(long, lat, Month, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(200, 10)) +
                          s(perc_65plus, k = 20) + s(perc_women, k = 20) + s(hosp_beds, k = 20) + s(care_home_beds, k = 20) + s(GISD_Score, k = 20) + s(pop_dens, k = 20) +
                          s(bundesland, bs = 're', k = 16) + offset(log(cases)),
                        data = dat_mo[[4]], family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
                  }
)
n1b_cdp_2 <- bake(file = 'results/fitted_models/n1b_cdp_2.rds',
                  expr = {
                    bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Month, k = 10, bs = 'cr') +
                          ti(long, lat, Month, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(200, 10)) +
                          s(perc_65plus, k = 20) + s(perc_women, k = 20) + s(hosp_beds, k = 20) + s(care_home_beds, k = 20) + s(GISD_Score, k = 20) + s(pop_dens, k = 20) +
                          ti(perc_65plus, Month) + ti(perc_women, Month) + ti(hosp_beds, Month) + ti(care_home_beds, Month) +
                          ti(GISD_Score, Month) + ti(pop_dens, Month) +
                          s(bundesland, bs = 're', k = 16) + offset(log(cases)),
                        data = dat_mo[[4]], family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
                  }
)

n1a_cdp_1 <- bake(file = 'results/fitted_models/n1a_cdp_1.rds',
                  expr = {
                    bam(cases ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Month, k = 10, bs = 'cr') +
                          ti(long, lat, Month, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(200, 10)) +
                          s(perc_65plus, k = 20) + s(perc_women, k = 20) + s(care_home_beds, k = 20) + s(GISD_Score, k = 20) +
                          s(pop_dens, k = 20) + s(living_area, k = 20) + s(perc_service, k = 20) + s(perc_production, k = 20) +
                          s(bundesland, bs = 're', k = 16) + offset(log(pop)),
                        data = dat_mo[[3]], family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
                  }
)
n1a_cdp_2 <- bake(file = 'results/fitted_models/n1a_cdp_2.rds',
                  expr = {
                    bam(cases ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Month, k = 10, bs = 'cr') +
                          ti(long, lat, Month, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(200, 10)) +
                          s(perc_65plus, k = 20) + s(perc_women, k = 20) + s(care_home_beds, k = 20) + s(GISD_Score, k = 20) +
                          s(pop_dens, k = 20) + s(living_area, k = 20) + s(perc_service, k = 20) + s(perc_production, k = 20) +
                          ti(perc_65plus, Month) + ti(perc_women, Month) + ti(care_home_beds, Month) +
                          ti(GISD_Score, Month) + ti(pop_dens, Month) + ti(living_area, Month) +
                          ti(perc_service, Month) + ti(perc_production, Month) +
                          s(bundesland, bs = 're', k = 16) + offset(log(pop)),
                        data = dat_mo[[3]], family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
                  }
)

# Weekly:
n1a_cdp_wk <- bake(file = 'results/fitted_models/n1a_cdp_wk.rds',
                   expr = {
                     bam(cases ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 44, bs = 'cr') +
                           ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(100, 20)) +
                           s(bundesland, bs = 're', k = 16) + offset(log(pop)),
                         data = dat_wk[[3]], family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
                   }
)
n1b_cdp_wk <- bake(file = 'results/fitted_models/n1b_cdp_wk.rds',
                   expr = {
                     bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 44, bs = 'cr') +
                           ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(100, 20)) +
                           s(bundesland, bs = 're', k = 16) + offset(log(cases)),
                         data = dat_wk[[4]], family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
                   }
)
n1b_cdp_wk_full <- bake(file = 'results/fitted_models/n1b_cdp_wk_full.rds',
                        expr = {
                          bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 44, bs = 'cr') +
                                ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(100, 20)) +
                                s(perc_65plus) + s(perc_women) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                                ti(perc_65plus, Week) + ti(perc_women, Week) + ti(hosp_beds, Week) + ti(care_home_beds, Week) +
                                ti(GISD_Score, Week) + ti(pop_dens, Week) +
                                s(bundesland, bs = 're', k = 16) + offset(log(cases)),
                              data = dat_wk[[4]], family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
                        }
)
n1b_cdp_wk_add <- bake(file = 'results/fitted_models/n1b_cdp_wk_fullplus.rds',
                       expr = {
                         bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 44, bs = 'cr') +
                               ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(100, 20)) +
                               s(perc_65plus) + s(perc_women) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                               ti(perc_65plus, Week) + ti(perc_women, Week) + ti(hosp_beds, Week) + ti(care_home_beds, Week) +
                               ti(GISD_Score, Week) + ti(pop_dens, Week) + s(stringency_index) +
                               s(bundesland, bs = 're', k = 16) + offset(log(cases)),
                             data = dat_wk[[4]], family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
                       }
)

dat_wk[[4]] <- dat_wk[[4]] %>%
  mutate(masks_shopping = factor(masks_shopping),
         school_closures2 = factor(school_closures2))
n1b_cdp_wk_add_2 <- bake(file = 'results/fitted_models/n1b_cdp_wk_fullplus2.rds',
                       expr = {
                         bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 44, bs = 'cr') +
                               ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(100, 20)) +
                               s(perc_65plus) + s(perc_women) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                               ti(perc_65plus, Week) + ti(perc_women, Week) + ti(hosp_beds, Week) + ti(care_home_beds, Week) +
                               ti(GISD_Score, Week) + ti(pop_dens, Week) + s(stringency_index) + masks_shopping + school_closures2 +
                               s(bundesland, bs = 're', k = 16) + offset(log(cases)),
                             data = dat_wk[[4]], family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
                       }
)

# ---------------------------------------------------------------------------------------------------------------------

# Run model checks

# List all models:
models_cases <- list(n1a, n1a_cdp_1, n1a_cdp_2, n1a_cdp_wk)
models_deaths_mo <- list(n1b, n1b_cdp, n1b_cdp_1, n1b_cdp_2)
models_deaths_wk <- list(n1b_cdp_wk, n1b_cdp_wk_full, n1b_cdp_wk_add, n1b_cdp_wk_add_2)
rm(n1a, n1a_cdp_1, n1a_cdp_2, n1a_cdp_wk, n1b, n1b_cdp, n1b_cdp_1, n1b_cdp_2,
   n1b_cdp_wk, n1b_cdp_wk_full, n1b_cdp_wk_add, n1b_cdp_wk_add_2)

# Loop through models and check fit/residuals:
for (i in 1:length(models_deaths_mo)) {
  mod <- models_deaths_mo[[i]]
  rsd <- residuals(mod, type = 'deviance')
  
  par(mfrow = c(2, 2))
  gam.check(mod, rep = 50)
  
  par(mfrow = c(1, 1))
  qqnorm(rsd)
  print(shapiro.test(rsd))
  # print(shapiro.test(sample(rsd, 5000, replace = FALSE)))
  
  plot(mod, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
  plot(mod, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE, residuals = TRUE, pch = 19)
  
  plot(fitted(mod), residuals(mod))
  
  try(plot(dat_mo[[4]]$time, rsd))
}

for (i in 1:length(models_cases)) {
  mod <- models_cases[[i]]
  rsd <- residuals(mod, type = 'deviance')
  
  par(mfrow = c(2, 2))
  gam.check(mod, rep = 50)
  
  par(mfrow = c(1, 1))
  qqnorm(rsd)
  
  if (length(rsd) > 5000) {
    print(shapiro.test(sample(rsd, 5000, replace = FALSE)))
  } else {
    print(shapiro.test(rsd))
  }
  
  plot(mod, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
  plot(mod, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE, residuals = TRUE, pch = 19)
  
  plot(fitted(mod), residuals(mod))
  
  try(plot(dat_mo[[3]]$time, rsd))
}

for (i in 1:length(models_deaths_wk)) {
  mod <- models_deaths_wk[[i]]
  rsd <- residuals(mod, type = 'deviance')
  
  par(mfrow = c(2, 2))
  gam.check(mod, rep = 50)
  
  par(mfrow = c(1, 1))
  qqnorm(rsd)
  print(shapiro.test(sample(rsd, 5000, replace = FALSE)))
  
  plot(mod, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
  plot(mod, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE, residuals = TRUE, pch = 19)
  
  plot(fitted(mod), residuals(mod))
  
  try(plot(dat_wk[[4]]$time, rsd))
}

# ---------------------------------------------------------------------------------------------------------------------

# Detailed check of residuals

# Focus here on monthly cases/deaths, as well as weekly deaths












# Residuals check:
# focus on: n1b_cdp, n1b_cdp_tweedie, n1b_cdp_1/n1b_cdp_2

plot(dat_mo[[4]]$time, residuals(mod))
plot(dat_mo[[4]]$bundesland, residuals(n1b_cdp))
plot(dat_mo[[4]]$lat, residuals(n1b_cdp))
plot(dat_mo[[4]]$long, residuals(n1b_cdp))

plot(dat_mo[[4]]$time, residuals(n1b_cdp_small_tweedie))
plot(dat_mo[[4]]$bundesland, residuals(n1b_cdp_small_tweedie))
plot(dat_mo[[4]]$lat, residuals(n1b_cdp_small_tweedie))
plot(dat_mo[[4]]$long, residuals(n1b_cdp_small_tweedie))

plot(dat_mo[[4]]$time, residuals(n1b_cdp_1))
plot(dat_mo[[4]]$bundesland, residuals(n1b_cdp_1))
plot(dat_mo[[4]]$lat, residuals(n1b_cdp_1))
plot(dat_mo[[4]]$long, residuals(n1b_cdp_1))

plot(dat_mo[[4]]$perc_65plus, residuals(n1b_cdp_1))
plot(dat_mo[[4]]$perc_women, residuals(n1b_cdp_1))
plot(dat_mo[[4]]$hosp_beds, residuals(n1b_cdp_1))
plot(dat_mo[[4]]$care_home_beds, residuals(n1b_cdp_1))
plot(dat_mo[[4]]$GISD_Score, residuals(n1b_cdp_1))
plot(dat_mo[[4]]$pop_dens, residuals(n1b_cdp_1))

rsd <- residuals(n1b_cdp, type = 'deviance')
dat_mo[[4]]$res <- rsd
a <- gam(rsd ~ s(lat) + s(long) + s(Month) - 1, data = dat_mo[[4]], select = TRUE)
a <- bam(rsd ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Month, k = 10, bs = 'cr') +
           ti(long, lat, Month, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(100, 10)) +
           s(bundesland, bs = 're', k = 16),
         data = dat_mo[[4]], select = TRUE, method = 'fREML', nthreads = 4)
# seems that there is still a pattern in the residuals over time, BL, and the time-space interaction
# but above, when we plot residuals vs. time, no pattern is observed

rsd <- residuals(n1b_cdp_1, type = 'deviance')
a <- bam(rsd ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Month, k = 10, bs = 'cr') +
           ti(long, lat, Month, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(100, 10)) +
           # s(perc_65plus) + s(perc_women) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
           s(bundesland, bs = 're', k = 16), # + offset(log(cases)),
         data = dat_mo[[4]], select = TRUE, method = 'fREML', nthreads = 4)
# also still a lot of patterns by predictors

p <- predict(n1b_cdp_small, type = 'response')
dat_mo[[4]]$pred_dat <- round(p)

a <- bam(pred_dat ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Month, k = 10, bs = 'cr') +
           ti(long, lat, Month, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(100, 10)) +
           s(bundesland, bs = 're', k = 16) + offset(log(cases)),
         data = dat_mo[[4]], family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

rsd <- residuals(a, type = 'deviance')
qqnorm(rsd) # these actually look pretty normal? what's happening in gam.check?

dat_mo[[4]]$res <- rsd
b <- bam(res ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Month, k = 10, bs = 'cr') +
           ti(long, lat, Month, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(100, 10)) +
           s(bundesland, bs = 're', k = 16),
         data = dat_mo[[4]], method = 'fREML', nthreads = 4, discrete = TRUE)


# DHARMa workflow:
library(Rcpp)
library(DHARMa)

# https://aosmith.rbind.io/2017/12/21/using-dharma-for-residual-checks-of-unsupported-models/
mus <- predict(n1b_cdp, type = 'response')
sim_n1b_cdp <- replicate(1000, rnbinom(n = nrow(dat_mo[[4]]),
                                       size = 8.497,
                                       mu = mus))
sim_res_n1b_cdp <- createDHARMa(simulatedResponse = sim_n1b_cdp,
                                observedResponse = dat_mo[[4]]$deaths,
                                fittedPredictedResponse = predict(n1b_cdp, type = 'response'),
                                integerResponse = TRUE)

plot(sim_res_n1b_cdp)
plotResiduals(sim_res_n1b_cdp, form = dat_mo[[4]]$Month)

testOutliers(sim_res_n1b_cdp, type = 'bootstrap')
# tests if there are more simulation outliers than expected

testDispersion(sim_res_n1b_cdp)
# tests if the simulated dispersion is equal to the observed dispersion

testZeroInflation(sim_res_n1b_cdp)

testQuantiles(sim_res_n1b_cdp)
# fits a quantile regression or residuals against a predictor (default predicted value), and tests if this conforms to the expected quantile

testUniformity(sim_res_n1b_cdp)
# tests if the overall distribution conforms to expectations

testTemporalAutocorrelation(sim_res_n1b_cdp)
testSpatialAutocorrelation(sim_res_n1b_cdp)




mus <- predict(n1a, type = 'response')
sim_n1a <- replicate(1000, rnbinom(n = nrow(dat_mo[[3]]),
                                   size = 16.147,
                                   mu = mus))
sim_res_n1a <- createDHARMa(simulatedResponse = sim_n1a,
                            observedResponse = dat_mo[[3]]$cases,
                            fittedPredictedResponse = predict(n1a, type = 'response'),
                            integerResponse = TRUE)

plot(sim_res_n1a)
plotResiduals(sim_res_n1a, form = dat_mo[[3]]$Month)

testOutliers(sim_res_n1a, type = 'bootstrap')
# tests if there are more simulation outliers than expected

testDispersion(sim_res_n1a)
# tests if the simulated dispersion is equal to the observed dispersion

testZeroInflation(sim_res_n1a)

testQuantiles(sim_res_n1a)
# fits a quantile regression or residuals against a predictor (default predicted value), and tests if this conforms to the expected quantile

testUniformity(sim_res_n1a)










# ---------------------------------------------------------------------------------------------------------------------
