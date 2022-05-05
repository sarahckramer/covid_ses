# ---------------------------------------------------------------------------------------------------------------------
# Fit GAMs using regional averages of vaccination rates, rather than estimated LK-specific rates
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

# Wave 3:
n3a_full <- bake(file = 'results/fitted_models/SA/FULL_n3a_vacc_reg_ml.rds',
                 expr = {
                   gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         ti(perc_18to64, GISD_Score) + ti(perc_lessthan18, GISD_Score) +
                         s(cases_pre3_rate) + s(vacc_w3_reg) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n3b_full <- bake(file = 'results/fitted_models/SA/FULL_n3b_vacc_reg_ml.rds',
                 expr = {
                   gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_rate) +
                         s(cases_pre3_rate) + s(vacc_w3_reg) +
                         offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Wave 4:
n4a_full <- bake(file = 'results/fitted_models/SA/FULL_n4a_vacc_reg_ml.rds',
                 expr = {
                   gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         ti(perc_lessthan18, GISD_Score) +
                         s(cases_pre4_rate) + s(vacc_w4_reg) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n4b_full <- bake(file = 'results/fitted_models/SA/FULL_n4b_vacc_reg_ml.rds',
                 expr = {
                   gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_rate) +
                         s(cases_pre4_rate) + s(vacc_w4_reg) +
                         offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Quick check of fits:
par(mfrow = c(2, 2))
gam.check(n3a_full, rep = 50)
gam.check(n3b_full, rep = 50)
gam.check(n4a_full, rep = 50)
gam.check(n4b_full, rep = 50)

# ---------------------------------------------------------------------------------------------------------------------

# Residual checks

# Check using DHARMa package:
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n3a_full, depend = 'none')
check_dharma(dat_cumulative, n3b_full, depend = 'none')
check_dharma(dat_cumulative, n4a_full, depend = 'none')
check_dharma(dat_cumulative, n4b_full, depend = 'none')

# Check for residual spatial autocorrelation:
dat_cumulative$resid_n3a <- residuals(n3a_full, type = 'deviance')
dat_cumulative$resid_n3b <- residuals(n3b_full, type = 'deviance')
dat_cumulative$resid_n4a <- residuals(n4a_full, type = 'deviance')
dat_cumulative$resid_n4b <- residuals(n4b_full, type = 'deviance')

map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')
map_base <- map_base %>%
  left_join(dat_cumulative %>%
              select(lk, resid_n3a, resid_n3b, resid_n4a, resid_n4b),
            by = c('ARS' = 'lk')) %>%
  drop_na()

nb <- spdep::poly2nb(map_base, row.names = map_base$ARS)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)
moran.mc(map_base$resid_n3a, lw, nsim = 999)
moran.mc(map_base$resid_n3b, lw, nsim = 999)
moran.mc(map_base$resid_n4a, lw, nsim = 999)
moran.mc(map_base$resid_n4b, lw, nsim = 999)

# ---------------------------------------------------------------------------------------------------------------------

# Plot smooth relationships between GISD_Score and incidence/CFR:
mod_list <- list(n3a_full, n4a_full)
names(mod_list) <- c('3', '4')

pred_GISD_Score <- get_marginal_prediction(dat_cumulative, 'GISD_Score', 'incidence', mod_list, standardize = TRUE)
plot_a_GISD_Score <- plot_marginal_prediction(pred_GISD_Score, 'GISD_Score', 'Cases / 10000 Pop', single_plot = TRUE)
print(plot_a_GISD_Score)

mod_list <- list(n3b_full, n4b_full)
names(mod_list) <- c('3', '4')

pred_GISD_Score <- get_marginal_prediction(dat_cumulative, 'GISD_Score', 'cfr', mod_list, standardize = TRUE)
plot_b_GISD_Score <- plot_marginal_prediction(pred_GISD_Score, 'GISD_Score', 'CFR', single_plot = TRUE)
print(plot_b_GISD_Score)

# Plot interactions with GISD_Score:
mod_list <- list(n3a_full, n4a_full)
names(mod_list) <- c('3', '4')

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
mod_list <- list(n3b_full, n4b_full)
names(mod_list) <- c('3', '4')

pred_cases_rate <- get_marginal_prediction(dat_cumulative, 'cases_rate', 'cfr', mod_list, standardize = TRUE)
plot_b_cases_rate <- plot_marginal_prediction(pred_cases_rate, 'cases_rate', 'CFR', single_plot = FALSE)
print(plot_b_cases_rate)

# Plot smooth relationships with vaccination rates:
mod_list <- list(n3a_full, n4a_full)
names(mod_list) <- c('3', '4')

pred_vacc <- get_marginal_prediction(dat_cumulative, 'vacc_reg', 'incidence', mod_list, standardize = TRUE)
plot_a_vacc <- plot_marginal_prediction(pred_vacc, 'vacc', 'Cases / 10000 Pop', single_plot = FALSE)
print(plot_a_vacc)

mod_list <- list(n3b_full, n4b_full)
names(mod_list) <- c('3', '4')

pred_vacc <- get_marginal_prediction(dat_cumulative, 'vacc_reg', 'cfr', mod_list, standardize = TRUE)
plot_b_vacc <- plot_marginal_prediction(pred_vacc, 'vacc', 'CFR', single_plot = FALSE)
print(plot_b_vacc)
