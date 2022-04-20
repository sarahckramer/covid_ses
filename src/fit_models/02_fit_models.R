# ---------------------------------------------------------------------------------------------------------------------
# Fit GAMs with no, single, or all SES predictors and store results
# Models labeled "a" are models of incidence; models labeled "b" are models of CFR
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
              gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                    offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)

n2a <- bake(file = 'results/fitted_models/null_n2a_ml.rds',
            expr = {
              gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)
n2b <- bake(file = 'results/fitted_models/null_n2b_ml.rds',
            expr = {
              gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                    offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)

n3a <- bake(file = 'results/fitted_models/null_n3a_ml.rds',
            expr = {
              gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)
n3b <- bake(file = 'results/fitted_models/null_n3b_ml.rds',
            expr = {
              gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                    offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)

n4a <- bake(file = 'results/fitted_models/null_n4a_ml.rds',
            expr = {
              gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 35) + s(ags2, bs = 're', k = 16) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)
n4b <- bake(file = 'results/fitted_models/null_n4b_ml.rds',
            expr = {
              gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                    offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)

n1b_adj <- bake(file = 'results/fitted_models/null_n1b_adj_ml.rds',
                expr = {
                  gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                        s(cases_wave1_rate) +
                        offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)
n2a_adj <- bake(file = 'results/fitted_models/null_n2a_adj_ml.rds',
                expr = {
                  gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                        s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)
n2b_adj <- bake(file = 'results/fitted_models/null_n2b_adj_ml.rds',
                expr = {
                  gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                        s(cases_pre2_rate) + s(cases_wave2_rate, k = 25) + offset(log(cases_wave2)),
                      data = dat_cumulative, family = 'nb', method = 'ML')
                }
)

n3a_adj <- bake(file = 'results/fitted_models/null_n3a_adj_ml.rds',
                expr = {
                  gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                        s(cases_pre3_rate) + s(vacc_w3) +
                        offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)
n3b_adj <- bake(file = 'results/fitted_models/null_n3b_adj_ml.rds',
                expr = {
                  gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                        s(cases_pre3_rate) + s(vacc_w3) + s(cases_wave3_rate) +
                        offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)

n4a_adj <- bake(file = 'results/fitted_models/null_n4a_adj_ml.rds',
                expr = {
                  gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 35) + s(ags2, bs = 're', k = 16) +
                        s(cases_pre4_rate) + s(vacc_w4) +
                        offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)
n4b_adj <- bake(file = 'results/fitted_models/null_n4b_adj_ml.rds',
                expr = {
                  gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                        s(cases_pre4_rate) + s(vacc_w4) + s(cases_wave4_rate) +
                        offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)

par(mfrow = c(2, 2))
gam.check(n1a, rep = 50)
gam.check(n1b, rep = 50)
gam.check(n2a, rep = 50)
gam.check(n2b, rep = 50)
gam.check(n3a, rep = 50)
gam.check(n3b, rep = 50)
gam.check(n4a, rep = 50)
gam.check(n4b, rep = 50)
gam.check(n2a_adj, rep = 50)
gam.check(n2b_adj, rep = 50)
gam.check(n3a_adj, rep = 50)
gam.check(n3b_adj, rep = 50)
gam.check(n4a_adj, rep = 50)
gam.check(n4b_adj, rep = 50)

# ---------------------------------------------------------------------------------------------------------------------

# Fit models with all predictors

# Wave 1:
n1a_full <- bake(file = 'results/fitted_models/FULL_n1a_ml.rds',
                 expr = {
                   gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n1b_full <- bake(file = 'results/fitted_models/FULL_n1b_ml.rds',
                 expr = {
                   gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave1_rate) +
                         offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Wave 2:
n2a_full <- bake(file = 'results/fitted_models/FULL_n2a_ml.rds',
                 expr = {
                   gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         ti(perc_18to64, pop_dens, k = 8) + ti(perc_lessthan18, pop_dens) +
                         s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n2b_full <- bake(file = 'results/fitted_models/FULL_n2b_ml.rds',
                 expr = {
                   gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) + s(cases_wave2_rate, k = 25) +
                         s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Wave 3:
n3a_full <- bake(file = 'results/fitted_models/FULL_n3a_ml.rds',
                 expr = {
                   gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         ti(perc_18to64, GISD_Score) + ti(perc_lessthan18, GISD_Score) +
                         s(cases_pre3_rate) + s(vacc_w3) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n3b_full <- bake(file = 'results/fitted_models/FULL_n3b_ml.rds',
                 expr = {
                   gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_rate) +
                         s(cases_pre3_rate) + s(vacc_w3) +
                         offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Wave 4:
n4a_full <- bake(file = 'results/fitted_models/FULL_n4a_ml.rds',
                 expr = {
                   gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 35) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         ti(perc_lessthan18, GISD_Score) +
                         s(cases_pre4_rate) + s(vacc_w4) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n4b_full <- bake(file = 'results/fitted_models/FULL_n4b_ml.rds',
                 expr = {
                   gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_rate) +
                         s(cases_pre4_rate) + s(vacc_w4) +
                         offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Quick check of fits:
par(mfrow = c(2, 2))
gam.check(n1a_full, rep = 50)
gam.check(n1b_full, rep = 50)
gam.check(n2a_full, rep = 50)
gam.check(n2b_full, rep = 50)
gam.check(n3a_full, rep = 50)
gam.check(n3b_full, rep = 50)
gam.check(n4a_full, rep = 50)
gam.check(n4b_full, rep = 50)

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
