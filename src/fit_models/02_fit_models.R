# ---------------------------------------------------------------------------------------------------------------------
# Fit GAMs with no, single, or all SES predictors and store results
# Models labeled "a" are models of incidence; models labeled "b" are models of CFR
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(sf)
library(testthat)
library(pomp)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Formulate and fit models (lat/long only)

# Without predictors:
n1_1a <- bake(file = 'results/fitted_models/null_n1_1a_ml.rds',
              expr = {
                gam(cases_wave1_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 45) + s(ags2, bs = 're', k = 16) +
                      offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
              }
)
n1_2a <- bake(file = 'results/fitted_models/null_n1_2a_ml.rds',
              expr = {
                gam(cases_wave1_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 80) + s(ags2, bs = 're', k = 16) +
                      offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
              }
)

n1_1b <- bake(file = 'results/fitted_models/null_n1_1b_ml.rds',
            expr = {
              gam(deaths_wave1_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                    offset(log(cases_wave1_1)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)
n1_2b <- bake(file = 'results/fitted_models/null_n1_2b_ml.rds',
            expr = {
              gam(deaths_wave1_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                    offset(log(cases_wave1_2)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)

n2a <- bake(file = 'results/fitted_models/null_n2a_ml.rds',
            expr = {
              gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
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
              gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 80) + s(ags2, bs = 're', k = 16) +
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
              gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)
n4b <- bake(file = 'results/fitted_models/null_n4b_ml.rds',
            expr = {
              gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                    offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)

n5a <- bake(file = 'results/fitted_models/null_n5a_ml.rds',
            expr = {
              gam(cases_wave5 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)
n5b <- bake(file = 'results/fitted_models/null_n5b_ml.rds',
            expr = {
              gam(deaths_wave5 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                    offset(log(cases_wave5)), data = dat_cumulative, family = 'nb', method = 'ML')
            }
)

n1_2a_adj <- bake(file = 'results/fitted_models/null_n1_2a_adj_ml.rds',
                   expr = {
                     gam(cases_wave1_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 80) + s(ags2, bs = 're', k = 16) +
                           s(cases_wave1_1_rate) +
                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)
n1_1b_adj <- bake(file = 'results/fitted_models/null_n1_1b_adj_ml.rds',
                expr = {
                  gam(deaths_wave1_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                        s(cases_wave1_1_rate) +
                        offset(log(cases_wave1_1)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)
n1_2b_adj <- bake(file = 'results/fitted_models/null_n1_2b_adj_ml.rds',
                expr = {
                  gam(deaths_wave1_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                        s(cases_wave1_1_rate) + s(cases_wave1_2_rate) +
                        offset(log(cases_wave1_2)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)
n2a_adj <- bake(file = 'results/fitted_models/null_n2a_adj_ml.rds',
                expr = {
                  gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                        s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)
n2b_adj <- bake(file = 'results/fitted_models/null_n2b_adj_ml.rds',
                expr = {
                  gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                        s(cases_pre2_rate) + s(cases_wave2_rate) + offset(log(cases_wave2)),
                      data = dat_cumulative, family = 'nb', method = 'ML')
                }
)

n3a_adj <- bake(file = 'results/fitted_models/null_n3a_adj_ml.rds',
                expr = {
                  gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 80) + s(ags2, bs = 're', k = 16) +
                        s(cases_pre3_rate) + s(vacc_w3_reg) +
                        offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)
n3b_adj <- bake(file = 'results/fitted_models/null_n3b_adj_ml.rds',
                expr = {
                  gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                        s(cases_pre3_rate) + s(vacc_w3_reg) + s(cases_wave3_rate, k = 25) +
                        offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)

n4a_adj <- bake(file = 'results/fitted_models/null_n4a_adj_ml.rds',
                expr = {
                  gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                        s(cases_pre4_rate) + s(vacc_w4_reg) +
                        offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)
n4b_adj <- bake(file = 'results/fitted_models/null_n4b_adj_ml.rds',
                expr = {
                  gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                        s(cases_pre4_rate) + s(vacc_w4_reg) + s(cases_wave4_rate) +
                        offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)

n5a_adj <- bake(file = 'results/fitted_models/null_n5a_adj_ml.rds',
                expr = {
                  gam(cases_wave5 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                        s(cases_pre5_rate, k = 25) + s(vacc_w5_reg) +
                        offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)
n5b_adj <- bake(file = 'results/fitted_models/null_n5b_adj_ml.rds',
                expr = {
                  gam(deaths_wave5 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                        s(cases_pre5_rate) + s(vacc_w5_reg) + s(cases_wave5_rate) +
                        offset(log(cases_wave5)), data = dat_cumulative, family = 'nb', method = 'ML')
                }
)

par(mfrow = c(2, 2))
gam.check(n1_1a, rep = 50)
gam.check(n1_2a, rep = 50)
gam.check(n1_1b, rep = 50)
gam.check(n1_2b, rep = 50)
gam.check(n2a, rep = 50)
gam.check(n2b, rep = 50)
gam.check(n3a, rep = 50)
gam.check(n3b, rep = 50)
gam.check(n4a, rep = 50)
gam.check(n4b, rep = 50)
gam.check(n5a, rep = 50)
gam.check(n5b, rep = 50)
gam.check(n1_2a_adj, rep = 50)
gam.check(n1_1b_adj, rep = 50)
gam.check(n1_2b_adj, rep = 50)
gam.check(n2a_adj, rep = 50)
gam.check(n2b_adj, rep = 50)
gam.check(n3a_adj, rep = 50)
gam.check(n3b_adj, rep = 50)
gam.check(n4a_adj, rep = 50)
gam.check(n4b_adj, rep = 50)
gam.check(n5a_adj, rep = 50)
gam.check(n5b_adj, rep = 50)

# ---------------------------------------------------------------------------------------------------------------------

# Fit models with all predictors

# Wave 1:
n1_1a_full <- bake(file = 'results/fitted_models/FULL_n1_1a_ml.rds',
                   expr = {
                     gam(cases_wave1_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 45) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                           s(perc_service) + s(perc_production) +
                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)
n1_2a_full <- bake(file = 'results/fitted_models/FULL_n1_2a_ml.rds',
                   expr = {
                     gam(cases_wave1_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 80) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) +
                           s(perc_service) + s(perc_production) + s(cases_wave1_1_rate) +
                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)

n1_1b_full <- bake(file = 'results/fitted_models/FULL_n1_1b_ml.rds',
                 expr = {
                   gam(deaths_wave1_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave1_1_rate) +
                         offset(log(cases_wave1_1)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n1_2b_full <- bake(file = 'results/fitted_models/FULL_n1_2b_ml.rds',
                   expr = {
                     gam(deaths_wave1_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                           s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                           s(cases_wave1_1_rate) + s(cases_wave1_2_rate) +
                           offset(log(cases_wave1_2)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)

# Wave 2:
n2a_full <- bake(file = 'results/fitted_models/FULL_n2a_ml.rds',
                 expr = {
                   gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n2b_full <- bake(file = 'results/fitted_models/FULL_n2b_ml.rds',
                 expr = {
                   gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave2_rate) +
                         s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Wave 3:
n3a_full <- bake(file = 'results/fitted_models/FULL_n3a_ml.rds',
                 expr = {
                   gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 80) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         s(cases_pre3_rate) + s(vacc_w3_reg) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n3b_full <- bake(file = 'results/fitted_models/FULL_n3b_ml.rds',
                 expr = {
                   gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_rate, k = 25) +
                         s(cases_pre3_rate) + s(vacc_w3_reg) + ti(pop_dens, GISD_Score) +
                         offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Wave 4:
n4a_full <- bake(file = 'results/fitted_models/FULL_n4a_ml.rds',
                 expr = {
                   gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         s(cases_pre4_rate) + s(vacc_w4_reg) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n4b_full <- bake(file = 'results/fitted_models/FULL_n4b_ml.rds',
                 expr = {
                   gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_rate) +
                         s(cases_pre4_rate) + s(vacc_w4_reg) +
                         offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Wave 5:
n5a_full <- bake(file = 'results/fitted_models/FULL_n5a_ml.rds',
                 expr = {
                   gam(cases_wave5 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         s(cases_pre5_rate, k = 25) + s(vacc_w5_reg) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)
n5b_full <- bake(file = 'results/fitted_models/FULL_n5b_ml.rds',
                 expr = {
                   gam(deaths_wave5 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave5_rate) +
                         s(cases_pre5_rate) + s(vacc_w5_reg) +
                         offset(log(cases_wave5)), data = dat_cumulative, family = 'nb', method = 'ML')
                 }
)

# Quick check of fits:
par(mfrow = c(2, 2))
gam.check(n1_1a_full, rep = 50)
gam.check(n1_2a_full, rep = 50)
gam.check(n1_1b_full, rep = 50)
gam.check(n1_2b_full, rep = 50)
gam.check(n2a_full, rep = 50)
gam.check(n2b_full, rep = 50)
gam.check(n3a_full, rep = 50)
gam.check(n3b_full, rep = 50)
gam.check(n4a_full, rep = 50)
gam.check(n4b_full, rep = 50)
gam.check(n5a_full, rep = 50)
gam.check(n5b_full, rep = 50)

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
