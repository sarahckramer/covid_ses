# ---------------------------------------------------------------------------------------------------------------------
# Fit GAMs to partial (half) waves, rather than full waves, to see how relationships change over time
# Models labeled "a" are models of incidence; models labeled "b" are models of CFR
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(sf)
library(spdep)
library(testthat)
library(pomp)
library(Rcpp)
library(DHARMa)
library(gridExtra)
library(viridis)
library(psych)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
keep_map <- TRUE
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Load functions:
source('src/functions/assess_results_fxns.R')

# ---------------------------------------------------------------------------------------------------------------------

# Fit models
# By default, use same k-values for lat/long as for full waves, but adjust based on gam.check/DHARMa
# For all other parameters, check gam.check, and use same k's for both partial waves

# Wave 2:
n2_1a_full <- bake(file = 'results/fitted_models/SA/FULL_n2_1a_ml.rds',
                   expr = {
                     gam(cases_wave2_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) +
                           s(perc_service) + s(perc_production) +
                           s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)
n2_2a_full <- bake(file = 'results/fitted_models/SA/FULL_n2_2a_ml.rds',
                   expr = {
                     gam(cases_wave2_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 80) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) +
                           s(perc_service) + s(perc_production) +
                           s(cases_pre2_2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)

n2_1b_full <- bake(file = 'results/fitted_models/SA/FULL_n2_1b_ml.rds',
                   expr = {
                     gam(deaths_wave2_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                           s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave2_1_rate) +
                           s(cases_pre2_rate) + offset(log(cases_wave2_1)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)
n2_2b_full <- bake(file = 'results/fitted_models/SA/FULL_n2_2b_ml.rds',
                   expr = {
                     gam(deaths_wave2_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                           s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave2_2_rate) +
                           s(cases_pre2_2_rate) + offset(log(cases_wave2_2)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)

# Wave 3:
n3_1a_full <- bake(file = 'results/fitted_models/SA/FULL_n3_1a_ml.rds',
                   expr = {
                     gam(cases_wave3_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                           s(perc_service) + s(perc_production) +
                           s(cases_pre3_rate) + s(vacc_w3_1_reg) +
                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)
n3_2a_full <- bake(file = 'results/fitted_models/SA/FULL_n3_2a_ml.rds',
                   expr = {
                     gam(cases_wave3_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                           s(perc_service) + s(perc_production) +
                           s(cases_pre3_2_rate) + s(vacc_w3_2_reg) +
                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)

n3_1b_full <- bake(file = 'results/fitted_models/SA/FULL_n3_1b_ml.rds',
                   expr = {
                     gam(deaths_wave3_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                           s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_1_rate, k = 25) +
                           s(cases_pre3_rate) + s(vacc_w3_1_reg) + ti(pop_dens, GISD_Score) +
                           offset(log(cases_wave3_1)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)
n3_2b_full <- bake(file = 'results/fitted_models/SA/FULL_n3_2b_ml.rds',
                   expr = {
                     gam(deaths_wave3_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                           s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_2_rate, k = 25) +
                           s(cases_pre3_2_rate) + s(vacc_w3_2_reg) + ti(pop_dens, GISD_Score) +
                           offset(log(cases_wave3_2)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)

# Wave 4:
n4_1a_full <- bake(file = 'results/fitted_models/SA/FULL_n4_1a_ml.rds',
                   expr = {
                     gam(cases_wave4_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64) + s(perc_lessthan18, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                           s(perc_service) + s(perc_production) +
                           s(cases_pre4_rate) + s(vacc_w4_1_reg) +
                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)
n4_2a_full <- bake(file = 'results/fitted_models/SA/FULL_n4_2a_ml.rds',
                   expr = {
                     gam(cases_wave4_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64) + s(perc_lessthan18, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                           s(perc_service) + s(perc_production) +
                           s(cases_pre4_2_rate) + s(vacc_w4_2_reg) +
                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)

n4_1b_full <- bake(file = 'results/fitted_models/SA/FULL_n4_1b_ml.rds',
                   expr = {
                     gam(deaths_wave4_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                           s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_1_rate) +
                           s(cases_pre4_rate) + s(vacc_w4_1_reg) +
                           offset(log(cases_wave4_1)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)
n4_2b_full <- bake(file = 'results/fitted_models/SA/FULL_n4_2b_ml.rds',
                   expr = {
                     gam(deaths_wave4_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                           s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_2_rate) +
                           s(cases_pre4_2_rate) + s(vacc_w4_2_reg) +
                           offset(log(cases_wave4_2)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)

# Wave 5:
n5_1a_full <- bake(file = 'results/fitted_models/SA/FULL_n5_1a_ml.rds',
                   expr = {
                     gam(cases_wave5_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                           s(perc_service) + s(perc_production) +
                           s(cases_pre5_rate, k = 25) + s(vacc_w5_1_reg) +
                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)
n5_2a_full <- bake(file = 'results/fitted_models/SA/FULL_n5_2a_ml.rds',
                   expr = {
                     gam(cases_wave5_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                           s(perc_service) + s(perc_production) +
                           s(cases_pre5_2_rate, k = 25) + s(vacc_w5_2_reg) +
                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)

n5_1b_full <- bake(file = 'results/fitted_models/SA/FULL_n5_1b_ml.rds',
                   expr = {
                     gam(deaths_wave5_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                           s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave5_1_rate) +
                           s(cases_pre5_rate) + s(vacc_w5_1_reg) +
                           offset(log(cases_wave5_1)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)
n5_2b_full <- bake(file = 'results/fitted_models/SA/FULL_n5_2b_ml.rds',
                   expr = {
                     gam(deaths_wave5_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                           s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave5_2_rate) +
                           s(cases_pre5_2_rate) + s(vacc_w5_2_reg) +
                           offset(log(cases_wave5_2)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)

# Quick check of fits:
par(mfrow = c(2, 2))

gam.check(n2_1a_full, rep = 50)
gam.check(n2_2a_full, rep = 50)
gam.check(n2_1b_full, rep = 50)
gam.check(n2_2b_full, rep = 50)

gam.check(n3_1a_full, rep = 50)
gam.check(n3_2a_full, rep = 50)
gam.check(n3_1b_full, rep = 50)
gam.check(n3_2b_full, rep = 50)

gam.check(n4_1a_full, rep = 50)
gam.check(n4_2a_full, rep = 50)
gam.check(n4_1b_full, rep = 50)
gam.check(n4_2b_full, rep = 50)

gam.check(n5_1a_full, rep = 50)
gam.check(n5_2a_full, rep = 50)
gam.check(n5_1b_full, rep = 50)
gam.check(n5_2b_full, rep = 50)

# ---------------------------------------------------------------------------------------------------------------------

# Residual checks

# Check using DHARMa package:
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n2_1a_full, depend = 'none')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n2_2a_full, depend = 'none')

par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n2_1b_full, depend = 'none')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n2_2b_full, depend = 'none')

par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n3_1a_full, depend = 'none')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n3_2a_full, depend = 'none')

par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n3_1b_full, depend = 'none')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n3_2b_full, depend = 'none')

par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n4_1a_full, depend = 'none')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n4_2a_full, depend = 'none')

par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n4_1b_full, depend = 'none')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n4_2b_full, depend = 'none')

par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n5_1a_full, depend = 'none')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n5_2a_full, depend = 'none')

par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n5_1b_full, depend = 'none')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n5_2b_full, depend = 'none')

# Check for residual spatial autocorrelation:
dat_cumulative$resid_n2_1a <- residuals(n2_1a_full, type = 'deviance')
dat_cumulative$resid_n2_2a <- residuals(n2_2a_full, type = 'deviance')
dat_cumulative$resid_n2_1b <- residuals(n2_1b_full, type = 'deviance')
dat_cumulative$resid_n2_2b <- residuals(n2_2b_full, type = 'deviance')
dat_cumulative$resid_n3_1a <- residuals(n3_1a_full, type = 'deviance')
dat_cumulative$resid_n3_2a <- residuals(n3_2a_full, type = 'deviance')
dat_cumulative$resid_n3_1b <- residuals(n3_1b_full, type = 'deviance')
dat_cumulative$resid_n3_2b <- residuals(n3_2b_full, type = 'deviance')
dat_cumulative$resid_n4_1a <- residuals(n4_1a_full, type = 'deviance')
dat_cumulative$resid_n4_2a <- residuals(n4_2a_full, type = 'deviance')
dat_cumulative$resid_n4_1b <- residuals(n4_1b_full, type = 'deviance')
dat_cumulative$resid_n4_2b <- residuals(n4_2b_full, type = 'deviance')
dat_cumulative$resid_n5_1a <- residuals(n5_1a_full, type = 'deviance')
dat_cumulative$resid_n5_2a <- residuals(n5_2a_full, type = 'deviance')
dat_cumulative$resid_n5_1b <- residuals(n5_1b_full, type = 'deviance')
dat_cumulative$resid_n5_2b <- residuals(n5_2b_full, type = 'deviance')

map_base <- st_read(dsn = 'data/raw/map/vg2500_12-31.gk3.shape/vg2500/VG2500_KRS.shp')
map_base <- map_base %>%
  left_join(dat_cumulative %>%
              select(lk, resid_n2_1a:resid_n5_2b),
            by = c('ARS' = 'lk')) %>%
  drop_na()

nb <- spdep::poly2nb(map_base, row.names = map_base$ARS)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)
moran.mc(map_base$resid_n2_1a, lw, nsim = 999)
moran.mc(map_base$resid_n2_2a, lw, nsim = 999)
moran.mc(map_base$resid_n2_1b, lw, nsim = 999)
moran.mc(map_base$resid_n2_2b, lw, nsim = 999)

moran.mc(map_base$resid_n3_1a, lw, nsim = 999)
moran.mc(map_base$resid_n3_2a, lw, nsim = 999)
moran.mc(map_base$resid_n3_1b, lw, nsim = 999)
moran.mc(map_base$resid_n3_2b, lw, nsim = 999)

moran.mc(map_base$resid_n4_1a, lw, nsim = 999)
moran.mc(map_base$resid_n4_2a, lw, nsim = 999)
moran.mc(map_base$resid_n4_1b, lw, nsim = 999)
moran.mc(map_base$resid_n4_2b, lw, nsim = 999)

moran.mc(map_base$resid_n5_1a, lw, nsim = 999)
moran.mc(map_base$resid_n5_2a, lw, nsim = 999)
moran.mc(map_base$resid_n5_1b, lw, nsim = 999)
moran.mc(map_base$resid_n5_2b, lw, nsim = 999)

# ---------------------------------------------------------------------------------------------------------------------

# Read in full wave models:
n2a_full <- read_rds('results/fitted_models/FULL_n2a_ml.rds')
n2b_full <- read_rds('results/fitted_models/FULL_n2b_ml.rds')
n3a_full <- read_rds('results/fitted_models/FULL_n3a_ml.rds')
n3b_full <- read_rds('results/fitted_models/FULL_n3b_ml.rds')
n4a_full <- read_rds('results/fitted_models/FULL_n4a_ml.rds')
n4b_full <- read_rds('results/fitted_models/FULL_n4b_ml.rds')
n5a_full <- read_rds('results/fitted_models/FULL_n5a_ml.rds')
n5b_full <- read_rds('results/fitted_models/FULL_n5b_ml.rds')

# ---------------------------------------------------------------------------------------------------------------------

# Plot results

# Plot observed incidence/CFR by partial wave:
map_pan <- map_base %>%
  left_join(dat_cumulative %>%
              select(lk, cases_wave1_1_rate, cases_wave1_2_rate, cfr_wave1_1, cfr_wave1_2,
                     cases_wave2_1_rate, cases_wave2_2_rate, cfr_wave2_1, cfr_wave2_2,
                     cases_wave3_1_rate, cases_wave3_2_rate, cfr_wave3_1, cfr_wave3_2,
                     cases_wave4_1_rate, cases_wave4_2_rate, cfr_wave4_1, cfr_wave4_2,
                     cases_wave5_1_rate, cases_wave5_2_rate, cfr_wave5_1, cfr_wave5_2),
            by = c('ARS' = 'lk'))
map_bl <- st_read(dsn = 'data/raw/map/vg2500_12-31.gk3.shape/vg2500/VG2500_LAN.shp')[1:16, ]

p1_1a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave1_1_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_1_rate, map_pan$cases_wave1_2_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(20, 100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 1 (1/2)', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p1_2a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave1_2_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_1_rate, map_pan$cases_wave1_2_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(20, 100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 1 (2/2)', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
grid.arrange(p1_1a, p1_2a, nrow = 1)

p2_1a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave2_1_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave2_1_rate, map_pan$cases_wave2_2_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(20, 100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 2 (1/2)', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2_2a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave2_2_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave2_1_rate, map_pan$cases_wave2_2_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(20, 100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 2 (2/2)', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
grid.arrange(p2_1a, p2_2a, nrow = 1)

p3_1a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave3_1_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave3_1_rate, map_pan$cases_wave3_2_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(20, 100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 3 (1/2)', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p3_2a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave3_2_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave3_1_rate, map_pan$cases_wave3_2_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(20, 100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 3 (2/2)', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
grid.arrange(p3_1a, p3_2a, nrow = 1)

p4_1a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave4_1_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave4_1_rate, map_pan$cases_wave4_2_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(20, 100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 4 (1/2)', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p4_2a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave4_2_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave4_1_rate, map_pan$cases_wave4_2_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(20, 100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 4 (2/2)', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
grid.arrange(p4_1a, p4_2a, nrow = 1)

p5_1a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave5_1_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave5_1_rate, map_pan$cases_wave5_2_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(100, 500, 1250, 2300)) +
  theme_void() + labs(title = 'Wave 5 (1/2)', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p5_2a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave5_2_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave5_1_rate, map_pan$cases_wave5_2_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(100, 500, 1250, 2300)) +
  theme_void() + labs(title = 'Wave 5 (2/2)', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
grid.arrange(p5_1a, p5_2a, nrow = 1)

p1_1b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave1_1), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1_1, map_pan$cfr_wave1_2, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 2, 4, 8, 16)) +
  theme_void() + labs(title = 'Wave 1 (1/2)', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p1_2b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave1_2), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1_1, map_pan$cfr_wave1_2, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 2, 4, 8, 16)) +
  theme_void() + labs(title = 'Wave 1 (2/2)', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
grid.arrange(p1_1b, p1_2b, nrow = 1)

p2_1b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave2_1), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave2_1, map_pan$cfr_wave2_2, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 2, 4, 6, 8, 16)) +
  theme_void() + labs(title = 'Wave 2 (1/2)', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2_2b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave2_2), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave2_1, map_pan$cfr_wave2_2, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 2, 4, 6, 8, 16)) +
  theme_void() + labs(title = 'Wave 2 (2/2)', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
grid.arrange(p2_1b, p2_2b, nrow = 1)

p3_1b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave3_1), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave3_1, map_pan$cfr_wave3_2, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 2, 4, 8, 16)) +
  theme_void() + labs(title = 'Wave 3 (1/2)', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p3_2b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave3_2), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave3_1, map_pan$cfr_wave3_2, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 2, 4, 8, 16)) +
  theme_void() + labs(title = 'Wave 3 (2/2)', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
grid.arrange(p3_1b, p3_2b, nrow = 1)

p4_1b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave4_1), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave4_1, map_pan$cfr_wave4_2, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 2, 3, 4)) +
  theme_void() + labs(title = 'Wave 4 (1/2)', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p4_2b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave4_2), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave4_1, map_pan$cfr_wave4_2, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 2, 3, 4)) +
  theme_void() + labs(title = 'Wave 4 (2/2)', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
grid.arrange(p4_1b, p4_2b, nrow = 1)

p5_1b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave5_1), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave5_1, map_pan$cfr_wave5_2, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 0.1, 0.5, 1, 2)) +
  theme_void() + labs(title = 'Wave 5 (1/2)', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p5_2b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave5_2), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave5_1, map_pan$cfr_wave5_2, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 0.1, 0.5, 1, 2)) +
  theme_void() + labs(title = 'Wave 5 (2/2)', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
grid.arrange(p5_1b, p5_2b, nrow = 1)

# Significant clustering by Moran's I?:
map_pan <- map_pan %>%
  drop_na()
map_temp <- st_read(dsn = 'data/raw/map/vg2500_12-31.gk3.shape/vg2500/VG2500_KRS.shp') %>%
  filter(GEN != 'Eisenach', GEN != 'Wartburgkreis')

nb <- spdep::poly2nb(map_temp, row.names = map_temp$ARS)
attr(nb, 'region.id') <- map_temp$ARS
names(nb) <- attr(nb, 'region.id')

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)
moran.mc(map_pan$cases_wave1_1_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave1_2_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave2_1_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave2_2_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave3_1_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave3_2_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave4_1_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave4_2_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave5_1_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave5_2_rate, lw, nsim = 999)

moran.mc(map_pan$cfr_wave1_1, lw, nsim = 999)
moran.mc(map_pan$cfr_wave1_2, lw, nsim = 999)
moran.mc(map_pan$cfr_wave2_1, lw, nsim = 999)
moran.mc(map_pan$cfr_wave2_2, lw, nsim = 999)
moran.mc(map_pan$cfr_wave3_1, lw, nsim = 999)
moran.mc(map_pan$cfr_wave3_2, lw, nsim = 999)
moran.mc(map_pan$cfr_wave4_1, lw, nsim = 999)
moran.mc(map_pan$cfr_wave4_2, lw, nsim = 999)
moran.mc(map_pan$cfr_wave5_1, lw, nsim = 999)
moran.mc(map_pan$cfr_wave5_2, lw, nsim = 999)

# How consistent are patterns between partial waves?:
pairs.panels(dat_cumulative %>%
               select(cases_wave1_rate, cases_wave1_1_rate, cases_wave1_2_rate,
                      cases_wave2_rate, cases_wave2_1_rate, cases_wave2_2_rate,
                      cases_wave3_rate, cases_wave3_1_rate, cases_wave3_2_rate,
                      cases_wave4_rate, cases_wave4_1_rate, cases_wave4_2_rate,
                      cases_wave5_rate, cases_wave5_1_rate, cases_wave5_2_rate),
             smooth = FALSE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             digits = 2,
             method = 'kendall',
             pch = 20,
             cex = 1.15,
             stars = TRUE,
             hist.col = 'gray85',
             rug = FALSE,
             breaks = 20,
             cex.cor = 0.6)

pairs.panels(dat_cumulative %>%
               select(cfr_wave1, cfr_wave1_1, cfr_wave1_2,
                      cfr_wave2, cfr_wave2_1, cfr_wave2_2,
                      cfr_wave3, cfr_wave3_1, cfr_wave3_2,
                      cfr_wave4, cfr_wave4_1, cfr_wave4_2,
                      cfr_wave5, cfr_wave5_1, cfr_wave5_2),
             smooth = FALSE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             digits = 2,
             method = 'kendall',
             pch = 20,
             cex = 1.15,
             stars = TRUE,
             hist.col = 'gray85',
             rug = FALSE,
             breaks = 20,
             cex.cor = 0.6)

# Compare deviance explained to full wave models:
summary(n2a_full)
summary(n2_1a_full)
summary(n2_2a_full)

summary(n3a_full)
summary(n3_1a_full)
summary(n3_2a_full)

summary(n4a_full)
summary(n4_1a_full)
summary(n4_2a_full)

summary(n5a_full)
summary(n5_1a_full)
summary(n5_2a_full)

summary(n2b_full)
summary(n2_1b_full)
summary(n2_2b_full)

summary(n3b_full)
summary(n3_1b_full)
summary(n3_2b_full)

summary(n4b_full)
summary(n4_1b_full)
summary(n4_2b_full)

summary(n5b_full)
summary(n5_1b_full)
summary(n5_2b_full)

# Plot smooth relationships between GISD_Score and incidence/CFR for each pair of partial waves:
n1_1a_full <- read_rds('results/fitted_models/FULL_n1_1a_ml.rds')
n1_2a_full <- read_rds('results/fitted_models/FULL_n1_2a_ml.rds')
n1_1b_full <- read_rds('results/fitted_models/FULL_n1_1b_ml.rds')
n1_2b_full <- read_rds('results/fitted_models/FULL_n1_2b_ml.rds')

mod_list <- list(n1_1a_full, n1_2a_full, n2_1a_full, n2_2a_full, n3_1a_full, n3_2a_full, n4_1a_full, n4_2a_full, n5_1a_full, n5_2a_full)
names(mod_list) <- c('1_1', '1_2', '2_1', '2_2', '3_1', '3_2', '4_1', '4_2', '5_1', '5_2')

pred_a_GISD_Score <- get_marginal_prediction(dat_cumulative, 'GISD_Score', 'incidence', mod_list,
                                             standardize = TRUE, partial_waves = TRUE)

plot_a <- plot_marginal_prediction(pred_a_GISD_Score %>% filter(!str_detect(wave, 'Wave 1')),
                                   'GISD_Score', 'Cases / 10000 Pop', single_plot = FALSE)
plot_a <- plot_a + labs(x = 'GISD', y = 'Relative Change (Incidence)', tag = 'A') + theme(plot.tag = element_text(size = 20), plot.tag.position = c(0.005, 0.98))

mod_list <- list(n1_1b_full, n1_2b_full, n2_1b_full, n2_2b_full, n3_1b_full, n3_2b_full, n4_1b_full, n4_2b_full, n5_1b_full, n5_2b_full)
names(mod_list) <- c('1_1', '1_2', '2_1', '2_2', '3_1', '3_2', '4_1', '4_2', '5_1', '5_2')

pred_b_GISD_Score <- get_marginal_prediction(dat_cumulative, 'GISD_Score', 'cfr', mod_list,
                                             standardize = TRUE, partial_waves = TRUE)

plot_b <- plot_marginal_prediction(pred_b_GISD_Score %>% filter(!str_detect(wave, 'Wave 1')),
                                   'GISD_Score', 'CFR', single_plot = FALSE)
plot_b <- plot_b + labs(x = 'GISD', tag = 'B') + theme(plot.tag = element_text(size = 20), plot.tag.position = c(0.005, 0.98))

figS8 <- arrangeGrob(plot_a, plot_b, nrow = 2)
plot(figS8)
# ggsave('results/FigureS8.svg', figS8, width = 13.125, height = 8.5)

# Plot interactions with GISD_Score, if present:
mod_list <- list(n1_1b_full, n1_2b_full, n2_1b_full, n2_2b_full, n3_1b_full, n3_2b_full, n4_1b_full, n4_2b_full, n5_1b_full, n5_2b_full)
names(mod_list) <- c('1_1', '1_2', '2_1', '2_2', '3_1', '3_2', '4_1', '4_2', '5_1', '5_2')

pred_popdens_GISD <- get_marginal_prediction(dat_cumulative, c('pop_dens', 'GISD_Score'), 'cfr',
                                             mod_list, standardize = TRUE, partial_waves = TRUE)

plot_popdens_GISD <- plot_marginal_prediction(pred_popdens_GISD, c('pop_dens', 'GISD_Score'),
                                              'CFR', single_plot = FALSE,
                                              which_waves = c('3_1', '3_2'))

grid.arrange(plot_popdens_GISD[[2]][[1]], plot_popdens_GISD[[2]][[2]], ncol = 2)

# Plot smooth relationships between incidence and CFR for each pair of partial waves:
mod_list <- list(n1_1b_full, n1_2b_full, n2_1b_full, n2_2b_full, n3_1b_full, n3_2b_full, n4_1b_full, n4_2b_full, n5_1b_full, n5_2b_full)
names(mod_list) <- c('1_1', '1_2', '2_1', '2_2', '3_1', '3_2', '4_1', '4_2', '5_1', '5_2')

pred_b_cases_rate <- get_marginal_prediction(dat_cumulative, 'cases_rate', 'cfr', mod_list,
                                             standardize = TRUE, partial_waves = TRUE)

plot_b_cases_rate1 <- plot_marginal_prediction(pred_b_cases_rate %>% filter(str_detect(wave, 'Wave 1')),
                                               'cases_rate', 'CFR', single_plot = TRUE)
plot_b_cases_rate2 <- plot_marginal_prediction(pred_b_cases_rate %>% filter(str_detect(wave, 'Wave 2')),
                                               'cases_rate', 'CFR', single_plot = TRUE)
plot_b_cases_rate3 <- plot_marginal_prediction(pred_b_cases_rate %>% filter(str_detect(wave, 'Wave 3')),
                                               'cases_rate', 'CFR', single_plot = TRUE)
plot_b_cases_rate4 <- plot_marginal_prediction(pred_b_cases_rate %>% filter(str_detect(wave, 'Wave 4')),
                                               'cases_rate', 'CFR', single_plot = TRUE)
plot_b_cases_rate5 <- plot_marginal_prediction(pred_b_cases_rate %>% filter(str_detect(wave, 'Wave 5')),
                                               'cases_rate', 'CFR', single_plot = TRUE)

grid.arrange(plot_b_cases_rate1, plot_b_cases_rate2, plot_b_cases_rate3, plot_b_cases_rate4, plot_b_cases_rate5, ncol = 2)
