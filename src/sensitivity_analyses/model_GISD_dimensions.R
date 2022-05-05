# ---------------------------------------------------------------------------------------------------------------------
# Fit GAMs using three individual "dimensions" of GISD (education, work, and income) rather than overall index
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
library(psych)
library(gridExtra)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Load functions:
source('src/functions/assess_results_fxns.R')

# ---------------------------------------------------------------------------------------------------------------------

# Fit models
# By default, use same k-values for lat/long as in main analysis, but adjust based on gam.check/DHARMa
# For all other parameters, check gam.check to determine whether k's are high enough
# Fit first without interactions, then check whether interactions improve fit

# Wave 1:
n1a_full_bildung <- bake(file = 'results/fitted_models/SA/FULL_n1a_bildung_ml.rds',
                         expr = {
                           gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                                 s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) +
                                 s(perc_service) + s(perc_production) +
                                 offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                         }
)
n1a_full_einkommen <- bake(file = 'results/fitted_models/SA/FULL_n1a_einkommen_ml.rds',
                           expr = {
                             gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) +
                                   s(perc_service) + s(perc_production) +
                                   ti(perc_18to64, TS_Einkommen_adj) + ti(pop_dens, TS_Einkommen_adj) +
                                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                           }
)
n1a_full_arbeitswelt <- bake(file = 'results/fitted_models/SA/FULL_n1a_arbeitswelt_ml.rds',
                             expr = {
                               gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                                     s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) +
                                     s(perc_service) + s(perc_production) +
                                     offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                             }
)

n1b_full_bildung <- bake(file = 'results/fitted_models/SA/FULL_n1b_bildung_ml.rds',
                         expr = {
                           gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                                 s(hosp_beds, k = 25) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) + s(cases_wave1_rate) +
                                 offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
                         }
)
n1b_full_einkommen <- bake(file = 'results/fitted_models/SA/FULL_n1b_einkommen_ml.rds',
                           expr = {
                             gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                                   s(hosp_beds, k = 25) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) + s(cases_wave1_rate) +
                                   # ti(cases_wave1_rate, hosp_beds) +
                                   offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
                           }
)
n1b_full_arbeitswelt <- bake(file = 'results/fitted_models/SA/FULL_n1b_arbeitswelt_ml.rds',
                             expr = {
                               gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                                     s(hosp_beds, k = 25) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) + s(cases_wave1_rate) +
                                     # ti(cases_wave1_rate, hosp_beds) +
                                     offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
                             }
)

# Wave 2:
n2a_full_bildung <- bake(file = 'results/fitted_models/SA/FULL_n2a_bildung_ml.rds',
                         expr = {
                           gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                                 s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) +
                                 s(perc_service) + s(perc_production) +
                                 ti(perc_18to64, TS_Bildung_adj) + ti(perc_lessthan18, pop_dens) +
                                 s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                         }
)
n2a_full_einkommen <- bake(file = 'results/fitted_models/SA/FULL_n2a_einkommen_ml.rds',
                           expr = {
                             gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                                   s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) +
                                   s(perc_service) + s(perc_production) +
                                   ti(perc_18to64, TS_Einkommen_adj) + ti(pop_dens, TS_Einkommen_adj) + ti(perc_lessthan18, pop_dens) +
                                   s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                           }
)
n2a_full_arbeitswelt <- bake(file = 'results/fitted_models/SA/FULL_n2a_arbeitswelt_ml.rds',
                             expr = {
                               gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                                     s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) +
                                     s(perc_service) + s(perc_production) +
                                     ti(perc_18to64, TS_Arbeitswelt_adj) + ti(perc_lessthan18, pop_dens) +
                                     s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                             }
)

n2b_full_bildung <- bake(file = 'results/fitted_models/SA/FULL_n2b_bildung_ml.rds',
                         expr = {
                           gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                                 s(hosp_beds) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) + s(cases_wave2_rate) +
                                 s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
                         }
)
n2b_full_einkommen <- bake(file = 'results/fitted_models/SA/FULL_n2b_einkommen_ml.rds',
                           expr = {
                             gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                                   s(hosp_beds) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) + s(cases_wave2_rate) +
                                   s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
                           }
)
n2b_full_arbeitswelt <- bake(file = 'results/fitted_models/SA/FULL_n2b_arbeitswelt_ml.rds',
                             expr = {
                               gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                                     s(hosp_beds) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) + s(cases_wave2_rate) +
                                     s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
                             }
)

# Wave 3:
n3a_full_bildung <- bake(file = 'results/fitted_models/SA/FULL_n3a_bildung_ml.rds',
                         expr = {
                           gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                                 s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens, k = 25) +
                                 s(perc_service) + s(perc_production, k = 25) +
                                 s(cases_pre3_rate) + s(vacc_w3) +
                                 offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                         }
)
n3a_full_einkommen <- bake(file = 'results/fitted_models/SA/FULL_n3a_einkommen_ml.rds',
                           expr = {
                             gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Einkommen_adj, k = 25) + s(pop_dens, k = 25) +
                                   s(perc_service) + s(perc_production, k = 25) +
                                   ti(perc_18to64, TS_Einkommen_adj) + ti(perc_lessthan18, TS_Einkommen_adj) +
                                   s(cases_pre3_rate) + s(vacc_w3) +
                                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                           }
)
n3a_full_arbeitswelt <- bake(file = 'results/fitted_models/SA/FULL_n3a_arbeitswelt_ml.rds',
                             expr = {
                               gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                                     s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens, k = 25) +
                                     s(perc_service) + s(perc_production, k = 25) +
                                     ti(pop_dens, TS_Arbeitswelt_adj) + ti(perc_lessthan18, TS_Arbeitswelt_adj) +
                                     s(cases_pre3_rate) + s(vacc_w3) +
                                     offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                             }
)

n3b_full_bildung <- bake(file = 'results/fitted_models/SA/FULL_n3b_bildung_ml.rds',
                         expr = {
                           gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                                 s(hosp_beds) + s(care_home_beds) + s(TS_Bildung_adj, k = 25) + s(pop_dens) + s(cases_wave3_rate) +
                                 s(cases_pre3_rate) + s(vacc_w3, k = 25) +
                                 offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
                         }
)
n3b_full_einkommen <- bake(file = 'results/fitted_models/SA/FULL_n3b_einkommen_ml.rds',
                           expr = {
                             gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                                   s(hosp_beds) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) + s(cases_wave3_rate) +
                                   s(cases_pre3_rate) + s(vacc_w3, k = 25) +
                                   offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
                           }
)
n3b_full_arbeitswelt <- bake(file = 'results/fitted_models/SA/FULL_n3b_arbeitswelt_ml.rds',
                             expr = {
                               gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                                     s(hosp_beds) + s(care_home_beds) + s(TS_Arbeitswelt_adj, k = 25) + s(pop_dens) + s(cases_wave3_rate) +
                                     s(cases_pre3_rate) + s(vacc_w3, k = 25) +
                                     offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
                             }
)

# Wave 4:
n4a_full_bildung <- bake(file = 'results/fitted_models/SA/FULL_n4a_bildung_ml.rds',
                         expr = {
                           gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                                 s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) +
                                 s(perc_service) + s(perc_production) +
                                 ti(perc_18to64, pop_dens) + ti(pop_dens, TS_Bildung_adj) +
                                 ti(perc_lessthan18, TS_Bildung_adj) +
                                 s(cases_pre4_rate) + s(vacc_w4) +
                                 offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                         }
)
n4a_full_einkommen <- bake(file = 'results/fitted_models/SA/FULL_n4a_einkommen_ml.rds',
                           expr = {
                             gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) +
                                   s(perc_service) + s(perc_production) +
                                   s(cases_pre4_rate) + s(vacc_w4) +
                                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                           }
)
n4a_full_arbeitswelt <- bake(file = 'results/fitted_models/SA/FULL_n4a_arbeitswelt_ml.rds',
                             expr = {
                               gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                                     s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) +
                                     s(perc_service) + s(perc_production) +
                                     s(cases_pre4_rate) + s(vacc_w4) +
                                     offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                             }
)

n4b_full_bildung <- bake(file = 'results/fitted_models/SA/FULL_n4b_bildung_ml.rds',
                         expr = {
                           gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                                 s(hosp_beds) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) + s(cases_wave4_rate) +
                                 s(cases_pre4_rate) + s(vacc_w4) +
                                 offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
                         }
)
n4b_full_einkommen <- bake(file = 'results/fitted_models/SA/FULL_n4b_einkommen_ml.rds',
                           expr = {
                             gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                                   s(hosp_beds) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) + s(cases_wave4_rate) +
                                   s(cases_pre4_rate) + s(vacc_w4) +
                                   offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
                           }
)
n4b_full_arbeitswelt <- bake(file = 'results/fitted_models/SA/FULL_n4b_arbeitswelt_ml.rds',
                             expr = {
                               gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                                     s(hosp_beds) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) + s(cases_wave4_rate) +
                                     s(cases_pre4_rate) + s(vacc_w4) +
                                     offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
                             }
)

# Quick check of fits:
par(mfrow = c(2, 2))
gam.check(n1a_full_bildung, rep = 50)
gam.check(n1a_full_einkommen, rep = 50)
gam.check(n1a_full_arbeitswelt, rep = 50)

gam.check(n1b_full_bildung, rep = 50)
gam.check(n1b_full_einkommen, rep = 50)
gam.check(n1b_full_arbeitswelt, rep = 50)

gam.check(n2a_full_bildung, rep = 50)
gam.check(n2a_full_einkommen, rep = 50)
gam.check(n2a_full_arbeitswelt, rep = 50)

gam.check(n2b_full_bildung, rep = 50)
gam.check(n2b_full_einkommen, rep = 50)
gam.check(n2b_full_arbeitswelt, rep = 50)

gam.check(n3a_full_bildung, rep = 50)
gam.check(n3a_full_einkommen, rep = 50)
gam.check(n3a_full_arbeitswelt, rep = 50)

gam.check(n3b_full_bildung, rep = 50)
gam.check(n3b_full_einkommen, rep = 50)
gam.check(n3b_full_arbeitswelt, rep = 50)

gam.check(n4a_full_bildung, rep = 50)
gam.check(n4a_full_einkommen, rep = 50)
gam.check(n4a_full_arbeitswelt, rep = 50)

gam.check(n4b_full_bildung, rep = 50)
gam.check(n4b_full_einkommen, rep = 50)
gam.check(n4b_full_arbeitswelt, rep = 50)

# ---------------------------------------------------------------------------------------------------------------------

# # Check for inclusion of interactions
# 
# # Initial check:
# n1a_comp_bildung <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
#                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) +
#                           s(perc_service) + s(perc_production) +
#                           ti(perc_18to64, pop_dens) + ti(perc_18to64, TS_Bildung_adj) + ti(pop_dens, TS_Bildung_adj) +
#                           ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, TS_Bildung_adj) +
#                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n1a_comp_einkommen <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
#                             s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) +
#                             s(perc_service) + s(perc_production) +
#                             ti(perc_18to64, pop_dens) + ti(perc_18to64, TS_Einkommen_adj) + ti(pop_dens, TS_Einkommen_adj) +
#                             ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, TS_Einkommen_adj) +
#                             offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n1a_comp_arbeitswelt <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
#                               s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) +
#                               s(perc_service) + s(perc_production) +
#                               ti(perc_18to64, pop_dens) + ti(perc_18to64, TS_Arbeitswelt_adj) + ti(pop_dens, TS_Arbeitswelt_adj) +
#                               ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, TS_Arbeitswelt_adj) +
#                               offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# 
# n1b_comp_bildung <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                           s(hosp_beds) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) + s(cases_wave1_rate) +
#                           ti(pop_dens, TS_Bildung_adj) + ti(cases_wave1_rate, hosp_beds) +
#                           offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n1b_comp_einkommen <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                             s(hosp_beds) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) + s(cases_wave1_rate) +
#                             ti(pop_dens, TS_Einkommen_adj) + ti(cases_wave1_rate, hosp_beds) +
#                             offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n1b_comp_arbeitswelt <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                               s(hosp_beds) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) + s(cases_wave1_rate) +
#                               ti(pop_dens, TS_Arbeitswelt_adj) + ti(cases_wave1_rate, hosp_beds) +
#                               offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# 
# # Wave 2:
# n2a_comp_bildung <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
#                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) +
#                           s(perc_service) + s(perc_production) +
#                           ti(perc_18to64, pop_dens) + ti(perc_18to64, TS_Bildung_adj) + ti(pop_dens, TS_Bildung_adj) +
#                           ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, TS_Bildung_adj) +
#                           s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n2a_comp_einkommen <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
#                             s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) +
#                             s(perc_service) + s(perc_production) +
#                             ti(perc_18to64, pop_dens) + ti(perc_18to64, TS_Einkommen_adj) + ti(pop_dens, TS_Einkommen_adj) +
#                             ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, TS_Einkommen_adj) +
#                             s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n2a_comp_arbeitswelt <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
#                               s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) +
#                               s(perc_service) + s(perc_production) +
#                               ti(perc_18to64, pop_dens) + ti(perc_18to64, TS_Arbeitswelt_adj) + ti(pop_dens, TS_Arbeitswelt_adj) +
#                               ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, TS_Arbeitswelt_adj) +
#                               s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# 
# n2b_comp_bildung <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                           s(hosp_beds) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) + s(cases_wave2_rate) +
#                           ti(pop_dens, TS_Bildung_adj) + ti(cases_wave2_rate, hosp_beds) +
#                           s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n2b_comp_einkommen <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                             s(hosp_beds) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) + s(cases_wave2_rate) +
#                             ti(pop_dens, TS_Einkommen_adj) + ti(cases_wave2_rate, hosp_beds) +
#                             s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n2b_comp_arbeitswelt <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                               s(hosp_beds) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) + s(cases_wave2_rate) +
#                               ti(pop_dens, TS_Arbeitswelt_adj) + ti(cases_wave2_rate, hosp_beds) +
#                               s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# 
# # Wave 3:
# n3a_comp_bildung <- gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
#                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) +
#                           s(perc_service) + s(perc_production) +
#                           ti(perc_18to64, pop_dens) + ti(perc_18to64, TS_Bildung_adj) + ti(pop_dens, TS_Bildung_adj) +
#                           ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, TS_Bildung_adj) +
#                           s(cases_pre3_rate) + s(vacc_w3) +
#                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n3a_comp_einkommen <- gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
#                             s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) +
#                             s(perc_service) + s(perc_production) +
#                             ti(perc_18to64, pop_dens) + ti(perc_18to64, TS_Einkommen_adj) + ti(pop_dens, TS_Einkommen_adj) +
#                             ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, TS_Einkommen_adj) +
#                             s(cases_pre3_rate) + s(vacc_w3) +
#                             offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n3a_comp_arbeitswelt <- gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
#                               s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) +
#                               s(perc_service) + s(perc_production) +
#                               ti(perc_18to64, pop_dens) + ti(perc_18to64, TS_Arbeitswelt_adj) + ti(pop_dens, TS_Arbeitswelt_adj) +
#                               ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, TS_Arbeitswelt_adj) +
#                               s(cases_pre3_rate) + s(vacc_w3) +
#                               offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# 
# n3b_comp_bildung <- gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
#                           s(hosp_beds) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) + s(cases_wave3_rate) +
#                           ti(pop_dens, TS_Bildung_adj) + ti(cases_wave3_rate, hosp_beds) +
#                           s(cases_pre3_rate) + s(vacc_w3) +
#                           offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n3b_comp_einkommen <- gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
#                             s(hosp_beds) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) + s(cases_wave3_rate) +
#                             ti(pop_dens, TS_Einkommen_adj) + ti(cases_wave3_rate, hosp_beds) +
#                             s(cases_pre3_rate) + s(vacc_w3) +
#                             offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n3b_comp_arbeitswelt <- gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
#                               s(hosp_beds) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) + s(cases_wave3_rate) +
#                               ti(pop_dens, TS_Arbeitswelt_adj) + ti(cases_wave3_rate, hosp_beds) +
#                               s(cases_pre3_rate) + s(vacc_w3) +
#                               offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# 
# # Wave 4:
# n4a_comp_bildung <- gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 35) + s(ags2, bs = 're', k = 16) +
#                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) +
#                           s(perc_service) + s(perc_production) +
#                           ti(perc_18to64, pop_dens) + ti(perc_18to64, TS_Bildung_adj) + ti(pop_dens, TS_Bildung_adj) +
#                           ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, TS_Bildung_adj) +
#                           s(cases_pre4_rate) + s(vacc_w4) +
#                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n4a_comp_einkommen <- gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 35) + s(ags2, bs = 're', k = 16) +
#                             s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) +
#                             s(perc_service) + s(perc_production) +
#                             ti(perc_18to64, pop_dens) + ti(perc_18to64, TS_Einkommen_adj) + ti(pop_dens, TS_Einkommen_adj) +
#                             ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, TS_Einkommen_adj) +
#                             s(cases_pre4_rate) + s(vacc_w4) +
#                             offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n4a_comp_arbeitswelt <- gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 35) + s(ags2, bs = 're', k = 16) +
#                               s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) +
#                               s(perc_service) + s(perc_production) +
#                               ti(perc_18to64, pop_dens) + ti(perc_18to64, TS_Arbeitswelt_adj) + ti(pop_dens, TS_Arbeitswelt_adj) +
#                               ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, TS_Arbeitswelt_adj) +
#                               s(cases_pre4_rate) + s(vacc_w4) +
#                               offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# 
# n4b_comp_bildung <- gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                           s(hosp_beds) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) + s(cases_wave4_rate) +
#                           ti(pop_dens, TS_Bildung_adj) + ti(cases_wave4_rate, hosp_beds) +
#                           s(cases_pre4_rate) + s(vacc_w4) +
#                           offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n4b_comp_einkommen <- gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                             s(hosp_beds) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) + s(cases_wave4_rate) +
#                             ti(pop_dens, TS_Einkommen_adj) + ti(cases_wave4_rate, hosp_beds) +
#                             s(cases_pre4_rate) + s(vacc_w4) +
#                             offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n4b_comp_arbeitswelt <- gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                               s(hosp_beds) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) + s(cases_wave4_rate) +
#                               ti(pop_dens, TS_Arbeitswelt_adj) + ti(cases_wave4_rate, hosp_beds) +
#                               s(cases_pre4_rate) + s(vacc_w4) +
#                               offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# 
# # Confirm improved model fit:
# n1a_comp_bildung <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
#                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) +
#                           s(perc_service) + s(perc_production) +
#                           ti(perc_18to64, TS_Bildung_adj) +
#                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n1a_comp_einkommen <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
#                             s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) +
#                             s(perc_service) + s(perc_production) +
#                             ti(perc_18to64, TS_Einkommen_adj) + ti(pop_dens, TS_Einkommen_adj) +
#                             offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# n1b_comp_bildung <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                           s(hosp_beds) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) + s(cases_wave1_rate) +
#                           ti(cases_wave1_rate, hosp_beds) +
#                           offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
# n1b_comp_einkommen <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                             s(hosp_beds) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) + s(cases_wave1_rate) +
#                             ti(cases_wave1_rate, hosp_beds) +
#                             offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
# n1b_comp_arbeitswelt <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                               s(hosp_beds) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) + s(cases_wave1_rate) +
#                               ti(cases_wave1_rate, hosp_beds) +
#                               offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# n2a_comp_bildung <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
#                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) +
#                           s(perc_service) + s(perc_production) +
#                           ti(perc_18to64, TS_Bildung_adj) + ti(perc_lessthan18, pop_dens) +
#                           s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n2a_comp_einkommen <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
#                             s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) +
#                             s(perc_service) + s(perc_production) +
#                             ti(perc_18to64, TS_Einkommen_adj) + ti(pop_dens, TS_Einkommen_adj) + ti(perc_lessthan18, pop_dens) +
#                             s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n2a_comp_arbeitswelt <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
#                               s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) +
#                               s(perc_service) + s(perc_production) +
#                               ti(perc_18to64, TS_Arbeitswelt_adj) + ti(perc_lessthan18, pop_dens) +
#                               s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# n2b_comp_arbeitswelt <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                               s(hosp_beds) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) + s(cases_wave2_rate) +
#                               ti(cases_wave2_rate, hosp_beds) +
#                               s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# n3a_comp_bildung <- gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
#                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) +
#                           s(perc_service) + s(perc_production) +
#                           ti(perc_lessthan18, TS_Bildung_adj) +
#                           s(cases_pre3_rate) + s(vacc_w3) +
#                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n3a_comp_einkommen <- gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
#                             s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) +
#                             s(perc_service) + s(perc_production) +
#                             ti(perc_18to64, TS_Einkommen_adj) + ti(perc_lessthan18, TS_Einkommen_adj) +
#                             s(cases_pre3_rate) + s(vacc_w3) +
#                             offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n3a_comp_arbeitswelt <- gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
#                               s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) +
#                               s(perc_service) + s(perc_production) +
#                               ti(pop_dens, TS_Arbeitswelt_adj) + ti(perc_lessthan18, TS_Arbeitswelt_adj) +
#                               s(cases_pre3_rate) + s(vacc_w3) +
#                               offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# n3b_comp_bildung <- gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
#                           s(hosp_beds) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) + s(cases_wave3_rate) +
#                           ti(pop_dens, TS_Bildung_adj) +
#                           s(cases_pre3_rate) + s(vacc_w3) +
#                           offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
# n3b_comp_einkommen <- gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
#                             s(hosp_beds) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) + s(cases_wave3_rate) +
#                             ti(pop_dens, TS_Einkommen_adj) +
#                             s(cases_pre3_rate) + s(vacc_w3) +
#                             offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
# n3b_comp_arbeitswelt <- gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
#                               s(hosp_beds) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) + s(cases_wave3_rate) +
#                               ti(pop_dens, TS_Arbeitswelt_adj) +
#                               s(cases_pre3_rate) + s(vacc_w3) +
#                               offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# n4a_comp_bildung <- gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 35) + s(ags2, bs = 're', k = 16) +
#                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Bildung_adj) + s(pop_dens) +
#                           s(perc_service) + s(perc_production) +
#                           ti(perc_18to64, pop_dens) + ti(pop_dens, TS_Bildung_adj) +
#                           ti(perc_lessthan18, TS_Bildung_adj) +
#                           s(cases_pre4_rate) + s(vacc_w4) +
#                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n4a_comp_einkommen <- gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 35) + s(ags2, bs = 're', k = 16) +
#                             s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) +
#                             s(perc_service) + s(perc_production) +
#                             ti(perc_18to64, pop_dens) +
#                             ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, TS_Einkommen_adj) +
#                             s(cases_pre4_rate) + s(vacc_w4) +
#                             offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n4a_comp_arbeitswelt <- gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 35) + s(ags2, bs = 're', k = 16) +
#                               s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(TS_Arbeitswelt_adj) + s(pop_dens) +
#                               s(perc_service) + s(perc_production) +
#                               ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, TS_Arbeitswelt_adj) +
#                               s(cases_pre4_rate) + s(vacc_w4) +
#                               offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# n4b_comp_einkommen <- gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                             s(hosp_beds) + s(care_home_beds) + s(TS_Einkommen_adj) + s(pop_dens) + s(cases_wave4_rate) +
#                             ti(pop_dens, TS_Einkommen_adj) +
#                             s(cases_pre4_rate) + s(vacc_w4) +
#                             offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# anova(n1a_full_bildung, n1a_comp_bildung, test = 'Chisq')
# anova(n1a_full_einkommen, n1a_comp_einkommen, test = 'Chisq') # sig
# 
# anova(n1b_full_bildung, n1b_comp_bildung, test = 'Chisq')
# anova(n1b_full_einkommen, n1b_comp_einkommen, test = 'Chisq') # sig
# anova(n1b_full_arbeitswelt, n1b_comp_arbeitswelt, test = 'Chisq') # sig
# 
# anova(n2a_full_bildung, n2a_comp_bildung, test = 'Chisq') # sig
# anova(n2a_full_einkommen, n2a_comp_einkommen, test = 'Chisq') # sig
# anova(n2a_full_arbeitswelt, n2a_comp_arbeitswelt, test = 'Chisq') # sig
# 
# anova(n2b_full_arbeitswelt, n2b_comp_arbeitswelt, test = 'Chisq')
# 
# anova(n3a_full_bildung, n3a_comp_bildung, test = 'Chisq')
# anova(n3a_full_einkommen, n3a_comp_einkommen, test = 'Chisq') # sig
# anova(n3a_full_arbeitswelt, n3a_comp_arbeitswelt, test = 'Chisq') # sig
# 
# anova(n3b_full_bildung, n3b_comp_bildung, test = 'Chisq')
# anova(n3b_full_einkommen, n3b_comp_einkommen, test = 'Chisq')
# anova(n3b_full_arbeitswelt, n3b_comp_arbeitswelt, test = 'Chisq')
# 
# anova(n4a_full_bildung, n4a_comp_bildung, test = 'Chisq') # sig
# anova(n4a_full_einkommen, n4a_comp_einkommen, test = 'Chisq')
# anova(n4a_full_arbeitswelt, n4a_comp_arbeitswelt, test = 'Chisq')
# 
# anova(n4b_full_einkommen, n4b_comp_einkommen, test = 'Chisq')

# ---------------------------------------------------------------------------------------------------------------------

# Residual checks

# Check using DHARMa package:
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n1a_full_bildung, depend = 'none')
check_dharma(dat_cumulative, n1a_full_einkommen, depend = 'none')
check_dharma(dat_cumulative, n1a_full_arbeitswelt, depend = 'none')

check_dharma(dat_cumulative, n1b_full_bildung, depend = 'none')
check_dharma(dat_cumulative, n1b_full_einkommen, depend = 'none')
check_dharma(dat_cumulative, n1b_full_arbeitswelt, depend = 'none')

par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n2a_full_bildung, depend = 'none')
check_dharma(dat_cumulative, n2a_full_einkommen, depend = 'none')
check_dharma(dat_cumulative, n2a_full_arbeitswelt, depend = 'none')

check_dharma(dat_cumulative, n2b_full_bildung, depend = 'none')
check_dharma(dat_cumulative, n2b_full_einkommen, depend = 'none')
check_dharma(dat_cumulative, n2b_full_arbeitswelt, depend = 'none')

par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n3a_full_bildung, depend = 'none')
check_dharma(dat_cumulative, n3a_full_einkommen, depend = 'none')
check_dharma(dat_cumulative, n3a_full_arbeitswelt, depend = 'none')

check_dharma(dat_cumulative, n3b_full_bildung, depend = 'none')
check_dharma(dat_cumulative, n3b_full_einkommen, depend = 'none')
check_dharma(dat_cumulative, n3b_full_arbeitswelt, depend = 'none')

par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n4a_full_bildung, depend = 'none')
check_dharma(dat_cumulative, n4a_full_einkommen, depend = 'none')
check_dharma(dat_cumulative, n4a_full_arbeitswelt, depend = 'none')

check_dharma(dat_cumulative, n4b_full_bildung, depend = 'none')
check_dharma(dat_cumulative, n4b_full_einkommen, depend = 'none')
check_dharma(dat_cumulative, n4b_full_arbeitswelt, depend = 'none')

# Check for residual spatial autocorrelation:
dat_cumulative$resid_n1a_bildung <- residuals(n1a_full_bildung, type = 'deviance')
dat_cumulative$resid_n1a_einkommen <- residuals(n1a_full_einkommen, type = 'deviance')
dat_cumulative$resid_n1a_arbeitswelt <- residuals(n1a_full_arbeitswelt, type = 'deviance')
dat_cumulative$resid_n1b_bildung <- residuals(n1b_full_bildung, type = 'deviance')
dat_cumulative$resid_n1b_einkommen <- residuals(n1b_full_einkommen, type = 'deviance')
dat_cumulative$resid_n1b_arbeitswelt <- residuals(n1b_full_arbeitswelt, type = 'deviance')

dat_cumulative$resid_n2a_bildung <- residuals(n2a_full_bildung, type = 'deviance')
dat_cumulative$resid_n2a_einkommen <- residuals(n2a_full_einkommen, type = 'deviance')
dat_cumulative$resid_n2a_arbeitswelt <- residuals(n2a_full_arbeitswelt, type = 'deviance')
dat_cumulative$resid_n2b_bildung <- residuals(n2b_full_bildung, type = 'deviance')
dat_cumulative$resid_n2b_einkommen <- residuals(n2b_full_einkommen, type = 'deviance')
dat_cumulative$resid_n2b_arbeitswelt <- residuals(n2b_full_arbeitswelt, type = 'deviance')

dat_cumulative$resid_n3a_bildung <- residuals(n3a_full_bildung, type = 'deviance')
dat_cumulative$resid_n3a_einkommen <- residuals(n3a_full_einkommen, type = 'deviance')
dat_cumulative$resid_n3a_arbeitswelt <- residuals(n3a_full_arbeitswelt, type = 'deviance')
dat_cumulative$resid_n3b_bildung <- residuals(n3b_full_bildung, type = 'deviance')
dat_cumulative$resid_n3b_einkommen <- residuals(n3b_full_einkommen, type = 'deviance')
dat_cumulative$resid_n3b_arbeitswelt <- residuals(n3b_full_arbeitswelt, type = 'deviance')

dat_cumulative$resid_n4a_bildung <- residuals(n4a_full_bildung, type = 'deviance')
dat_cumulative$resid_n4a_einkommen <- residuals(n4a_full_einkommen, type = 'deviance')
dat_cumulative$resid_n4a_arbeitswelt <- residuals(n4a_full_arbeitswelt, type = 'deviance')
dat_cumulative$resid_n4b_bildung <- residuals(n4b_full_bildung, type = 'deviance')
dat_cumulative$resid_n4b_einkommen <- residuals(n4b_full_einkommen, type = 'deviance')
dat_cumulative$resid_n4b_arbeitswelt <- residuals(n4b_full_arbeitswelt, type = 'deviance')

map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')
map_base <- map_base %>%
  left_join(dat_cumulative %>%
              select(lk, resid_n1a_bildung:resid_n4b_arbeitswelt),
            by = c('ARS' = 'lk')) %>%
  drop_na()

nb <- spdep::poly2nb(map_base, row.names = map_base$ARS)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)
moran.mc(map_base$resid_n1a_bildung, lw, nsim = 999)
moran.mc(map_base$resid_n1a_einkommen, lw, nsim = 999)
moran.mc(map_base$resid_n1a_arbeitswelt, lw, nsim = 999)
moran.mc(map_base$resid_n1b_bildung, lw, nsim = 999)
moran.mc(map_base$resid_n1b_einkommen, lw, nsim = 999)
moran.mc(map_base$resid_n1b_arbeitswelt, lw, nsim = 999)

moran.mc(map_base$resid_n2a_bildung, lw, nsim = 999)
moran.mc(map_base$resid_n2a_einkommen, lw, nsim = 999)
moran.mc(map_base$resid_n2a_arbeitswelt, lw, nsim = 999)
moran.mc(map_base$resid_n2b_bildung, lw, nsim = 999)
moran.mc(map_base$resid_n2b_einkommen, lw, nsim = 999)
moran.mc(map_base$resid_n2b_arbeitswelt, lw, nsim = 999)

moran.mc(map_base$resid_n3a_bildung, lw, nsim = 999)
moran.mc(map_base$resid_n3a_einkommen, lw, nsim = 999)
moran.mc(map_base$resid_n3a_arbeitswelt, lw, nsim = 999)
moran.mc(map_base$resid_n3b_bildung, lw, nsim = 999)
moran.mc(map_base$resid_n3b_einkommen, lw, nsim = 999)
moran.mc(map_base$resid_n3b_arbeitswelt, lw, nsim = 999)

moran.mc(map_base$resid_n4a_bildung, lw, nsim = 999)
moran.mc(map_base$resid_n4a_einkommen, lw, nsim = 999)
moran.mc(map_base$resid_n4a_arbeitswelt, lw, nsim = 999)
moran.mc(map_base$resid_n4b_bildung, lw, nsim = 999)
moran.mc(map_base$resid_n4b_einkommen, lw, nsim = 999)
moran.mc(map_base$resid_n4b_arbeitswelt, lw, nsim = 999)

# ---------------------------------------------------------------------------------------------------------------------

# Read in models of full GISD score:
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

# Plot relationships between 3 dimensions and other predictors:
dat_ses <- dat_cumulative %>%
  select(perc_18to64, perc_lessthan18, hosp_beds, care_home_beds, pop_dens, TS_Bildung_adj,
         TS_Einkommen_adj, TS_Arbeitswelt_adj, perc_service, perc_production, vacc_w3, vacc_w4)

pairs.panels(dat_ses,
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
rm(dat_ses)

# Compare deviance explained by GISD_Score vs. 3 dimensions:
summary(n1a_full)
summary(n1a_full_bildung)
summary(n1a_full_einkommen)
summary(n1a_full_arbeitswelt) # better?

summary(n2a_full)
summary(n2a_full_bildung) # better?
summary(n2a_full_einkommen)
summary(n2a_full_arbeitswelt)

summary(n3a_full)
summary(n3a_full_bildung)
summary(n3a_full_einkommen)
summary(n3a_full_arbeitswelt)

summary(n4a_full)
summary(n4a_full_bildung) # slightly better?
summary(n4a_full_einkommen)
summary(n4a_full_arbeitswelt)

# Plot smooth relationships between each of the 3 dimensions and incidence/CFR for all waves:
mod_list <- list(n1a_full_bildung, n2a_full_bildung, n3a_full_bildung, n4a_full_bildung)
names(mod_list) <- c('1', '2', '3', '4')

pred_a_bildung <- get_marginal_prediction(dat_cumulative, pred_var = 'TS_Bildung_adj',
                                          outcome_measure = 'incidence', mod_list, standardize = TRUE)
plot_a_bildung <- plot_marginal_prediction(pred_a_bildung, 'TS_Bildung_adj', 'Cases / 10000 Pop',
                                           single_plot = TRUE)

mod_list <- list(n1a_full_einkommen, n2a_full_einkommen, n3a_full_einkommen, n4a_full_einkommen)
names(mod_list) <- c('1', '2', '3', '4')

pred_a_einkommen <- get_marginal_prediction(dat_cumulative, pred_var = 'TS_Einkommen_adj',
                                            outcome_measure = 'incidence', mod_list, standardize = TRUE)
plot_a_einkommen <- plot_marginal_prediction(pred_a_einkommen, 'TS_Einkommen_adj', 'Cases / 10000 Pop',
                                             single_plot = TRUE)

mod_list <- list(n1a_full_arbeitswelt, n2a_full_arbeitswelt, n3a_full_arbeitswelt, n4a_full_arbeitswelt)
names(mod_list) <- c('1', '2', '3', '4')

pred_a_arbeitswelt <- get_marginal_prediction(dat_cumulative, pred_var = 'TS_Arbeitswelt_adj',
                                              outcome_measure = 'incidence', mod_list, standardize = TRUE)
plot_a_arbeitswelt <- plot_marginal_prediction(pred_a_arbeitswelt, 'TS_Arbeitswelt_adj', 'Cases / 10000 Pop',
                                               single_plot = TRUE)

grid.arrange(plot_a_bildung, plot_a_einkommen, plot_a_arbeitswelt, nrow = 1)

mod_list <- list(n1b_full_bildung, n2b_full_bildung, n3b_full_bildung, n4b_full_bildung)
names(mod_list) <- c('1', '2', '3', '4')

pred_b_bildung <- get_marginal_prediction(dat_cumulative, pred_var = 'TS_Bildung_adj',
                                          outcome_measure = 'cfr', mod_list, standardize = TRUE)
plot_b_bildung <- plot_marginal_prediction(pred_b_bildung, 'TS_Bildung_adj', 'CFR',
                                           single_plot = TRUE)

mod_list <- list(n1b_full_einkommen, n2b_full_einkommen, n3b_full_einkommen, n4b_full_einkommen)
names(mod_list) <- c('1', '2', '3', '4')

pred_b_einkommen <- get_marginal_prediction(dat_cumulative, pred_var = 'TS_Einkommen_adj',
                                            outcome_measure = 'cfr', mod_list, standardize = TRUE)
plot_b_einkommen <- plot_marginal_prediction(pred_b_einkommen, 'TS_Einkommen_adj', 'CFR',
                                             single_plot = TRUE)

mod_list <- list(n1b_full_arbeitswelt, n2b_full_arbeitswelt, n3b_full_arbeitswelt, n4b_full_arbeitswelt)
names(mod_list) <- c('1', '2', '3', '4')

pred_b_arbeitswelt <- get_marginal_prediction(dat_cumulative, pred_var = 'TS_Arbeitswelt_adj',
                                              outcome_measure = 'cfr', mod_list, standardize = TRUE)
plot_b_arbeitswelt <- plot_marginal_prediction(pred_b_arbeitswelt, 'TS_Arbeitswelt_adj', 'CFR',
                                               single_plot = TRUE)

grid.arrange(plot_b_bildung, plot_b_einkommen, plot_b_arbeitswelt, nrow = 1)

# Plot interactions where present:
mod_list <- list(n1a_full_bildung, n2a_full_bildung, n3a_full_bildung, n4a_full_bildung)
names(mod_list) <- c('1', '2', '3', '4')

pred_a_bildung_perc18to64 <- get_marginal_prediction(dat_cumulative, pred_var = c('TS_Bildung_adj', 'perc_18to64'),
                                                     outcome_measure = 'incidence', mod_list, standardize = TRUE)
pred_a_bildung_perclessthan18 <- get_marginal_prediction(dat_cumulative, pred_var = c('TS_Bildung_adj', 'perc_lessthan18'),
                                                         outcome_measure = 'incidence', mod_list, standardize = TRUE)
pred_a_bildung_popdens <- get_marginal_prediction(dat_cumulative, pred_var = c('TS_Bildung_adj', 'pop_dens'),
                                                  outcome_measure = 'incidence', mod_list, standardize = TRUE)

plot_a_bildung_perc18to64 <- plot_marginal_prediction(pred_a_bildung_perc18to64, c('TS_Bildung_adj', 'perc_18to64'),
                                                      'Cases / 10000 Pop', single_plot = FALSE, which_waves = 2)
plot_a_bildung_perclessthan18 <- plot_marginal_prediction(pred_a_bildung_perclessthan18, c('TS_Bildung_adj', 'perc_lessthan18'),
                                                          'Cases / 10000 Pop', single_plot = FALSE, which_waves = 4)
plot_a_bildung_popdens <- plot_marginal_prediction(pred_a_bildung_popdens, c('TS_Bildung_adj', 'pop_dens'),
                                                   'Cases / 10000 Pop', single_plot = FALSE, which_waves = 4)

mod_list <- list(n1a_full_einkommen, n2a_full_einkommen, n3a_full_einkommen, n4a_full_einkommen)
names(mod_list) <- c('1', '2', '3', '4')

pred_a_einkommen_perc18to64 <- get_marginal_prediction(dat_cumulative, pred_var = c('TS_Einkommen_adj', 'perc_18to64'),
                                                       outcome_measure = 'incidence', mod_list, standardize = TRUE)
pred_a_einkommen_perclessthan18 <- get_marginal_prediction(dat_cumulative, pred_var = c('TS_Einkommen_adj', 'perc_lessthan18'),
                                                           outcome_measure = 'incidence', mod_list, standardize = TRUE)
pred_a_einkommen_popdens <- get_marginal_prediction(dat_cumulative, pred_var = c('TS_Einkommen_adj', 'pop_dens'),
                                                    outcome_measure = 'incidence', mod_list, standardize = TRUE)

plot_a_einkommen_perc18to64 <- plot_marginal_prediction(pred_a_einkommen_perc18to64, c('TS_Einkommen_adj', 'perc_18to64'),
                                                        'Cases / 10000 Pop', single_plot = FALSE, which_waves = c(1, 2, 3))
plot_a_einkommen_perclessthan18 <- plot_marginal_prediction(pred_a_einkommen_perclessthan18, c('TS_Einkommen_adj', 'perc_lessthan18'),
                                                            'Cases / 10000 Pop', single_plot = FALSE, which_waves = 3)
plot_a_einkommen_popdens <- plot_marginal_prediction(pred_a_einkommen_popdens, c('TS_Einkommen_adj', 'pop_dens'),
                                                     'Cases / 10000 Pop', single_plot = FALSE, which_waves = c(1, 2))

mod_list <- list(n1a_full_arbeitswelt, n2a_full_arbeitswelt, n3a_full_arbeitswelt, n4a_full_arbeitswelt)
names(mod_list) <- c('1', '2', '3', '4')

pred_a_arbeitswelt_perc18to64 <- get_marginal_prediction(dat_cumulative, pred_var = c('TS_Arbeitswelt_adj', 'perc_18to64'),
                                                         outcome_measure = 'incidence', mod_list, standardize = TRUE)
pred_a_arbeitswelt_perclessthan18 <- get_marginal_prediction(dat_cumulative, pred_var = c('TS_Arbeitswelt_adj', 'perc_lessthan18'),
                                                             outcome_measure = 'incidence', mod_list, standardize = TRUE)
pred_a_arbeitswelt_popdens <- get_marginal_prediction(dat_cumulative, pred_var = c('TS_Arbeitswelt_adj', 'pop_dens'),
                                                      outcome_measure = 'incidence', mod_list, standardize = TRUE)

plot_a_arbeitswelt_perc18to64 <- plot_marginal_prediction(pred_a_arbeitswelt_perc18to64, c('TS_Arbeitswelt_adj', 'perc_18to64'),
                                                          'Cases / 10000 Pop', single_plot = FALSE, which_waves = 2)
plot_a_arbeitswelt_perclessthan18 <- plot_marginal_prediction(pred_a_arbeitswelt_perclessthan18, c('TS_Arbeitswelt_adj', 'perc_lessthan18'),
                                                              'Cases / 10000 Pop', single_plot = FALSE, which_waves = 3)
plot_a_arbeitswelt_popdens <- plot_marginal_prediction(pred_a_arbeitswelt_popdens, c('TS_Arbeitswelt_adj', 'pop_dens'),
                                                       'Cases / 10000 Pop', single_plot = FALSE, which_waves = 3)

grid.arrange(plot_a_einkommen_perc18to64[[1]][[1]],
             plot_a_einkommen_popdens[[1]][[1]]) # wave 1
grid.arrange(plot_a_bildung_perc18to64[[1]],
             plot_a_einkommen_perc18to64[[1]][[2]],
             plot_a_einkommen_popdens[[1]][[2]],
             plot_a_arbeitswelt_perc18to64[[1]]) # wave 2
grid.arrange(plot_a_einkommen_perc18to64[[1]][[3]],
             plot_a_einkommen_perclessthan18[[1]],
             plot_a_arbeitswelt_perclessthan18[[1]],
             plot_a_arbeitswelt_popdens[[1]]) # wave 3
grid.arrange(plot_a_bildung_perclessthan18[[1]],
             plot_a_bildung_popdens[[1]]) # wave 4
