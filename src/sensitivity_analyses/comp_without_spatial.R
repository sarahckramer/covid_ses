# ---------------------------------------------------------------------------------------------------------------------
# Fit GAMs without accounting for spatial autocorrelation, to see how estimates and deviance explained are affected
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

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Load functions:
source('src/functions/assess_results_fxns.R')

# ---------------------------------------------------------------------------------------------------------------------

# Fit models

# Wave 1:
n1a_full_NS <- bake(file = 'results/fitted_models/SA/FULL_n1a_NOSPATIAL_ml.rds',
                 expr = {
                   gam(cases_wave1 ~ s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n1b_full_NS <- bake(file = 'results/fitted_models/SA/FULL_n1b_NOSPATIAL_ml.rds',
                 expr = {
                   gam(deaths_wave1 ~ s(ags2, bs = 're', k = 16) +
                         s(hosp_beds, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave1_rate) +
                         offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Wave 2:
n2a_full_NS <- bake(file = 'results/fitted_models/SA/FULL_n2a_NOSPATIAL_ml.rds',
                 expr = {
                   gam(cases_wave2 ~ s(ags2, bs = 're', k = 16) +
                         s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         ti(perc_18to64, pop_dens, k = 8) + ti(perc_lessthan18, pop_dens) +
                         s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n2b_full_NS <- bake(file = 'results/fitted_models/SA/FULL_n2b_NOSPATIAL_ml.rds',
                 expr = {
                   gam(deaths_wave2 ~ s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) + s(cases_wave2_rate, k = 25) +
                         s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Wave 3:
n3a_full_NS <- bake(file = 'results/fitted_models/SA/FULL_n3a_NOSPATIAL_ml.rds',
                 expr = {
                   gam(cases_wave3 ~ s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         ti(perc_18to64, GISD_Score) + ti(perc_lessthan18, GISD_Score) +
                         s(cases_pre3_rate) + s(vacc_w3_reg) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n3b_full_NS <- bake(file = 'results/fitted_models/SA/FULL_n3b_NOSPATIAL_ml.rds',
                 expr = {
                   gam(deaths_wave3 ~ s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_rate) +
                         s(cases_pre3_rate) + s(vacc_w3_reg) +
                         offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Wave 4:
n4a_full_NS <- bake(file = 'results/fitted_models/SA/FULL_n4a_NOSPATIAL_ml.rds',
                 expr = {
                   gam(cases_wave4 ~ s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         ti(perc_lessthan18, GISD_Score) +
                         s(cases_pre4_rate) + s(vacc_w4_reg) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n4b_full_NS <- bake(file = 'results/fitted_models/SA/FULL_n4b_NOSPATIAL_ml.rds',
                 expr = {
                   gam(deaths_wave4 ~ s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_rate) +
                         s(cases_pre4_rate) + s(vacc_w4_reg) +
                         offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Quick check of fits:
par(mfrow = c(2, 2))
gam.check(n1a_full_NS, rep = 50)
gam.check(n1b_full_NS, rep = 50)
gam.check(n2a_full_NS, rep = 50)
gam.check(n2b_full_NS, rep = 50)
gam.check(n3a_full_NS, rep = 50)
gam.check(n3b_full_NS, rep = 50)
gam.check(n4a_full_NS, rep = 50)
gam.check(n4b_full_NS, rep = 50)

# ---------------------------------------------------------------------------------------------------------------------

# Residual checks

# Check using DHARMa package:
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n1a_full_NS, depend = 'none')
check_dharma(dat_cumulative, n2a_full_NS, depend = 'none')
check_dharma(dat_cumulative, n3a_full_NS, depend = 'none')
check_dharma(dat_cumulative, n4a_full_NS, depend = 'none')
check_dharma(dat_cumulative, n1b_full_NS, depend = 'none')
check_dharma(dat_cumulative, n2b_full_NS, depend = 'none')
check_dharma(dat_cumulative, n3b_full_NS, depend = 'none')
check_dharma(dat_cumulative, n4b_full_NS, depend = 'none')

# Check for residual spatial autocorrelation:
dat_cumulative$resid_n1a <- residuals(n1a_full_NS, type = 'deviance')
dat_cumulative$resid_n1b <- residuals(n1b_full_NS, type = 'deviance')
dat_cumulative$resid_n2a <- residuals(n2a_full_NS, type = 'deviance')
dat_cumulative$resid_n2b <- residuals(n2b_full_NS, type = 'deviance')
dat_cumulative$resid_n3a <- residuals(n3a_full_NS, type = 'deviance')
dat_cumulative$resid_n3b <- residuals(n3b_full_NS, type = 'deviance')
dat_cumulative$resid_n4a <- residuals(n4a_full_NS, type = 'deviance')
dat_cumulative$resid_n4b <- residuals(n4b_full_NS, type = 'deviance')

map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')
map_base <- map_base %>%
  left_join(dat_cumulative %>%
              select(lk, resid_n1a, resid_n1b, resid_n2a, resid_n2b,
                     resid_n3a, resid_n3b, resid_n4a, resid_n4b),
            by = c('ARS' = 'lk')) %>%
  drop_na()

nb <- spdep::poly2nb(map_base, row.names = map_base$ARS)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)
moran.mc(map_base$resid_n1a, lw, nsim = 999)
moran.mc(map_base$resid_n1b, lw, nsim = 999)
moran.mc(map_base$resid_n2a, lw, nsim = 999)
moran.mc(map_base$resid_n2b, lw, nsim = 999)
moran.mc(map_base$resid_n3a, lw, nsim = 999)
moran.mc(map_base$resid_n3b, lw, nsim = 999)
moran.mc(map_base$resid_n4a, lw, nsim = 999)
moran.mc(map_base$resid_n4b, lw, nsim = 999)

# ---------------------------------------------------------------------------------------------------------------------

# Read in models with lat/long included:
n1a_full <- read_rds('results/fitted_models/FULL_n1a_ml.rds')
n1b_full <- read_rds('results/fitted_models/FULL_n1b_ml.rds')
n2a_full <- read_rds('results/fitted_models/FULL_n2a_ml.rds')
n2b_full <- read_rds('results/fitted_models/FULL_n2b_ml.rds')
n3a_full <- read_rds('results/fitted_models/FULL_n3a_ml.rds')
n3b_full <- read_rds('results/fitted_models/FULL_n3b_ml.rds')
n4a_full <- read_rds('results/fitted_models/FULL_n4a_ml.rds')
n4b_full <- read_rds('results/fitted_models/FULL_n4b_ml.rds')

# ---------------------------------------------------------------------------------------------------------------------

# Plot results

# Compare deviance explained:
summary(n1a_full)
summary(n1a_full_NS)

summary(n2a_full)
summary(n2a_full_NS)

summary(n3a_full)
summary(n3a_full_NS)

summary(n4a_full)
summary(n4a_full_NS)

summary(n1b_full)
summary(n1b_full_NS)

summary(n2b_full)
summary(n2b_full_NS)

summary(n3b_full)
summary(n3b_full_NS)

summary(n4b_full)
summary(n4b_full_NS)

# Plot smooth relationships between GISD_Score and incidence/CFR:
mod_list <- list(n1a_full_NS, n2a_full_NS, n3a_full_NS, n4a_full_NS)
names(mod_list) <- c('1', '2', '3', '4')

pred_GISD_Score <- get_marginal_prediction(dat_cumulative, 'GISD_Score', 'incidence', mod_list, standardize = TRUE)
plot_a_GISD_Score <- plot_marginal_prediction(pred_GISD_Score, 'GISD_Score', 'Cases / 10000 Pop', single_plot = TRUE)
print(plot_a_GISD_Score)

mod_list <- list(n1b_full_NS, n2b_full_NS, n3b_full_NS, n4b_full_NS)
names(mod_list) <- c('1', '2', '3', '4')

pred_GISD_Score <- get_marginal_prediction(dat_cumulative, 'GISD_Score', 'cfr', mod_list, standardize = TRUE)
plot_b_GISD_Score <- plot_marginal_prediction(pred_GISD_Score, 'GISD_Score', 'CFR', single_plot = TRUE)
print(plot_b_GISD_Score)

# Plot interactions with GISD_Score:
mod_list <- list(n1a_full_NS, n2a_full_NS, n3a_full_NS, n4a_full_NS)
names(mod_list) <- c('1', '2', '3', '4')

pred_GISD_Score_perc18to64 <- get_marginal_prediction(dat_cumulative, c('GISD_Score', 'perc_18to64'),
                                                      'incidence', mod_list, standardize = TRUE)
pred_GISD_Score_perclessthan18 <- get_marginal_prediction(dat_cumulative, c('GISD_Score', 'perc_lessthan18'),
                                                          'incidence', mod_list, standardize = TRUE)

plot_a_GISD_Score_perc18to64 <- plot_marginal_prediction(pred_GISD_Score_perc18to64, c('GISD_Score', 'perc_18to64'),
                                                         'Cases / 10000 Pop', single_plot = FALSE,
                                                         which_waves = 3)
plot_a_GISD_Score_perclessthan18 <- plot_marginal_prediction(pred_GISD_Score_perclessthan18, c('GISD_Score', 'perc_lessthan18'),
                                                         'Cases / 10000 Pop', single_plot = FALSE,
                                                         which_waves = c(3, 4))

grid.arrange(plot_a_GISD_Score_perc18to64[[1]], plot_a_GISD_Score_perclessthan18[[1]][[1]])
print(plot_a_GISD_Score_perclessthan18[[1]][[2]])

# Plot smooth relationships between incidence and CFR:
mod_list <- list(n1b_full_NS, n2b_full_NS, n3b_full_NS, n4b_full_NS)
names(mod_list) <- c('1', '2', '3', '4')

pred_cases_rate <- get_marginal_prediction(dat_cumulative, 'cases_rate', 'cfr', mod_list, standardize = TRUE)
plot_b_cases_rate <- plot_marginal_prediction(pred_cases_rate, 'cases_rate', 'CFR', single_plot = FALSE)
print(plot_b_cases_rate)
