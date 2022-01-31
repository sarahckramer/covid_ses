# ---------------------------------------------------------------------------------------------------------------------
# Compare full models against other model specifications to see if improvements are possible
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(ggeffects)
library(sf)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Load full models:
n1a_full <- read_rds('results/fitted_models/FULL_n1a_ml.rds')
n1b_full <- read_rds('results/fitted_models/FULL_n1b_ml.rds')
n2a_full <- read_rds('results/fitted_models/FULL_n2a_ml.rds')
n2b_full <- read_rds('results/fitted_models/FULL_n2b_ml.rds')

# ---------------------------------------------------------------------------------------------------------------------

# Explore potential model improvements

# Should living_area be included?:
n1a_comp <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n2a_comp <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')

n1a_comp_alt <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                      s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(living_area, k = 25) +
                      s(perc_service) + s(perc_production) +
                      offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n2a_comp_alt <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                      s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(living_area) +
                      s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                      offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')

AIC(n1a_full, n1a_comp)
BIC(n1a_full, n1a_comp)

AIC(n2a_full, n2a_comp)
BIC(n2a_full, n2a_comp)

AIC(n1a_full, n1a_comp_alt)
BIC(n1a_full, n1a_comp_alt)

AIC(n2a_full, n2a_comp_alt)
BIC(n2a_full, n2a_comp_alt)

# Try using MRF:
dat_cumulative$ARS <- factor(dat_cumulative$lk)
n1a_mrf <- gam(cases_wave1 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 50) + s(ags2, bs = 're', k = 16) +
                 s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(living_area, k = 25) +
                 s(perc_service) + s(perc_production) +
                 offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n1b_mrf <- gam(deaths_wave1 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 60) + s(ags2, bs = 're', k = 16) +
                 s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                 offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')

n2a_mrf <- gam(cases_wave2 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 70) + s(ags2, bs = 're', k = 16) +
                 s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) + s(living_area) +
                 s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                 offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n2b_mrf <- gam(deaths_wave2 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 50) + s(ags2, bs = 're', k = 16) +
                 s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) +
                 s(cases_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')

AIC(n1a_full, n1a_mrf)
BIC(n1a_full, n1a_mrf)

AIC(n1b_full, n1b_mrf)
BIC(n1b_full, n1b_mrf)

AIC(n2a_full, n2a_mrf)
BIC(n2a_full, n2a_mrf)

AIC(n2b_full, n2b_mrf)
BIC(n2b_full, n2b_mrf)

# Compare with poisson/zero-inflated:
n1a_pois <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(living_area, k = 25) +
                  s(perc_service) + s(perc_production) +
                  offset(log(pop)), data = dat_cumulative, family = 'poisson', method = 'ML')
n1b_pois <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  offset(log(cases_wave1)), data = dat_cumulative, family = 'poisson', method = 'ML')

n2a_pois <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) + s(living_area) +
                  s(perc_service) + s(perc_production) +
                  s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'poisson', method = 'ML')
n2b_pois <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) +
                  s(cases_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'poisson', method = 'ML')

n1b_zip <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                 s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                 offset(log(cases_wave1)), data = dat_cumulative, family = 'ziP', method = 'ML')

BIC(n1a_full, n1a_pois)
BIC(n1b_full, n1b_pois, n1b_zip)

BIC(n2a_full, n2a_pois)
BIC(n2b_full, n2b_pois)

# Change random effects:
n1a_fixed <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) +
                   s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(living_area, k = 25) +
                   s(perc_service) + s(perc_production) +
                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n1b_fixed <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) +
                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                   offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')

n2a_fixed <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) +
                   s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) + s(living_area) +
                   s(perc_service) + s(perc_production) +
                   s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n2b_fixed <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) +
                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) +
                   s(cases_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')

AIC(n1a_full, n1a_fixed)
BIC(n1a_full, n1a_fixed)

AIC(n1b_full, n1b_fixed)
BIC(n1b_full, n1b_fixed)

AIC(n2a_full, n2a_fixed)
BIC(n2a_full, n2a_fixed)

AIC(n2b_full, n2b_fixed)
BIC(n2b_full, n2b_fixed)

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
