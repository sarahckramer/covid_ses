# ---------------------------------------------------------------------------------------------------------------------
# Fit GAMs with no, single, or all SES predictors and store results
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(ggeffects)
library(sf)
library(testthat)
library(pomp)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Formulate and fit models (lat/long only)

# Without predictors:
n1a <- bake(file = 'results/fitted_models/null_n1a_ml.rds',
            expr = {
              gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)
n1b <- bake(file = 'results/fitted_models/null_n1b_ml.rds',
            expr = {
              gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                    offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)

n2a <- bake(file = 'results/fitted_models/null_n2a_ml.rds',
            expr = {
              gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)
n2b <- bake(file = 'results/fitted_models/null_n2b_ml.rds',
            expr = {
              gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                    offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)

n2a_adj <- bake(file = 'results/fitted_models/null_n2a_adj_ml.rds',
            expr = {
              gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                    s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)
n2b_adj <- bake(file = 'results/fitted_models/null_n2b_adj_ml.rds',
            expr = {
              gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                    s(cases_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)

par(mfrow = c(2, 2))
gam.check(n1a, rep = 50)
gam.check(n1b, rep = 50)
gam.check(n2a, rep = 50)
gam.check(n2b, rep = 50)
gam.check(n2a_adj, rep = 50)
gam.check(n2b_adj, rep = 50)

# ---------------------------------------------------------------------------------------------------------------------

# Fit models with all predictors

# Wave 1:
n1a_full <- bake(file = 'results/fitted_models/FULL_n1a_ml.rds',
                 expr = {
                   gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(living_area, k = 25) +
                         s(perc_service) + s(perc_production) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n1b_full <- bake(file = 'results/fitted_models/FULL_n1b_ml.rds',
                 expr = {
                   gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Wave 2:
n2a_full <- bake(file = 'results/fitted_models/FULL_n2a_ml.rds',
                 expr = {
                   gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) + s(living_area) +
                         s(perc_service) + s(perc_production) +
                         s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n2b_full <- bake(file = 'results/fitted_models/FULL_n2b_ml.rds',
                 expr = {
                   gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) +
                         s(cases_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Quick check of fits:
par(mfrow = c(2, 2))
gam.check(n1a_full, rep = 50)
gam.check(n1b_full, rep = 50)
gam.check(n2a_full, rep = 50)
gam.check(n2b_full, rep = 50)

# ---------------------------------------------------------------------------------------------------------------------

# Formulate and fit "univariable" models (for comparison)

# Wave 1:
# Cases:
n1a_perc_18to64 <- bake(file = 'results/fitted_models/uni/n1a_perc_18to64_ml.rds',
                        expr = {
                          gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                                s(perc_18to64) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                        }
)
n1a_care_home_beds <- bake(file = 'results/fitted_models/uni/n1a_care_home_beds_ml.rds',
                           expr = {
                             gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                                   s(care_home_beds) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                           }
)
n1a_GISD <- bake(file = 'results/fitted_models/uni/n1a_GISD_ml.rds',
                 expr = {
                   gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                         s(GISD_Score) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n1a_pop_dens <- bake(file = 'results/fitted_models/uni/n1a_pop_dens_ml.rds',
                     expr = {
                       gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                             s(pop_dens) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                     }
)
n1a_living_area <- bake(file = 'results/fitted_models/uni/n1a_living_area_ml.rds',
                        expr = {
                          gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                                s(living_area, k = 25) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                        }
)
n1a_perc_serv <- bake(file = 'results/fitted_models/uni/n1a_perc_serv_ml.rds',
                      expr = {
                        gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                              s(perc_service) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                      }
)
n1a_perc_prod <- bake(file = 'results/fitted_models/uni/n1a_perc_prod_ml.rds',
                      expr = {
                        gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                              s(perc_production) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                      }
)

# Mortality:
n1b_hosp_beds <- bake(file = 'results/fitted_models/uni/n1b_hosp_beds_ml.rds',
                      expr = {
                        gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                              s(hosp_beds) + offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
                      }
)
n1b_care_home_beds <- bake(file = 'results/fitted_models/uni/n1b_care_home_beds_ml.rds',
                           expr = {
                             gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                                   s(care_home_beds) + offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
                           }
)
n1b_GISD <- bake(file = 'results/fitted_models/uni/n1b_GISD_ml.rds',
                 expr = {
                   gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                         s(GISD_Score) + offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Wave 2:
# Cases:
n2a_perc_18to64 <- bake(file = 'results/fitted_models/uni/n2a_perc_18to64_ml.rds',
                        expr = {
                          gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                                s(perc_18to64) + s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                        }
)
n2a_care_home_beds <- bake(file = 'results/fitted_models/uni/n2a_care_home_beds_ml.rds',
                           expr = {
                             gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                                   s(care_home_beds, k = 25) + s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                           }
)
n2a_GISD <- bake(file = 'results/fitted_models/uni/n2a_GISD_ml.rds',
                 expr = {
                   gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                         s(GISD_Score) + s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n2a_pop_dens <- bake(file = 'results/fitted_models/uni/n2a_pop_dens_ml.rds',
                     expr = {
                       gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                             s(pop_dens) + s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                     }
)
n2a_living_area <- bake(file = 'results/fitted_models/uni/n2a_living_area_ml.rds',
                        expr = {
                          gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                                s(living_area) + s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                        }
)
n2a_perc_serv <- bake(file = 'results/fitted_models/uni/n2a_perc_serv_ml.rds',
                      expr = {
                        gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                              s(perc_service) + s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                      }
)
n2a_perc_prod <- bake(file = 'results/fitted_models/uni/n2a_perc_prod_ml.rds',
                      expr = {
                        gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                              s(perc_production) + s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                      }
)

# Mortality:
n2b_hosp_beds <- bake(file = 'results/fitted_models/uni/n2b_hosp_beds_ml.rds',
                      expr = {
                        gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                              s(hosp_beds) + s(cases_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
                      }
)
n2b_care_home_beds <- bake(file = 'results/fitted_models/uni/n2b_care_home_beds_ml.rds',
                           expr = {
                             gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                                   s(care_home_beds) + s(cases_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
                           }
)
n2b_GISD <- bake(file = 'results/fitted_models/uni/n2b_GISD_ml.rds',
                 expr = {
                   gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                         s(GISD_Score, k = 25) + s(cases_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
