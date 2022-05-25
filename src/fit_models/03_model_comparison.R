# ---------------------------------------------------------------------------------------------------------------------
# Compare full models against other model specifications to see if improvements are possible
# Models labeled "a" are models of incidence; models labeled "b" are models of CFR
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(ggeffects)
library(testthat)
library(sf)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Load full models:
n1_1a_full <- read_rds('results/fitted_models/FULL_n1_1a_ml.rds')
n1_2a_full <- read_rds('results/fitted_models/FULL_n1_2a_ml.rds')
n1b_full <- read_rds('results/fitted_models/FULL_n1b_ml.rds')
n2a_full <- read_rds('results/fitted_models/FULL_n2a_ml.rds')
n2b_full <- read_rds('results/fitted_models/FULL_n2b_ml.rds')
n3a_full <- read_rds('results/fitted_models/FULL_n3a_ml.rds')
n3b_full <- read_rds('results/fitted_models/FULL_n3b_ml.rds')
n4a_full <- read_rds('results/fitted_models/FULL_n4a_ml.rds')
n4b_full <- read_rds('results/fitted_models/FULL_n4b_ml.rds')

# ---------------------------------------------------------------------------------------------------------------------

# Explore potential model improvements

# # Should both pop_dens AND living_area be included?:
# n1a_comp <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                   s(perc_service) + s(perc_production) +
#                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n2a_comp <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) +
#                   s(perc_service) + s(perc_production) + s(cases_pre_rate) +
#                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# n1a_comp_alt <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
#                       s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(living_area, k = 25) +
#                       s(perc_service) + s(perc_production) +
#                       offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n2a_comp_alt <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
#                       s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(living_area) +
#                       s(perc_service) + s(perc_production) + s(cases_pre_rate) +
#                       offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# anova(n1a_full, n1a_comp, test = 'Chisq')
# anova(n1a_full, n1a_comp_alt, test = 'Chisq')
# 
# anova(n2a_full, n2a_comp, test = 'Chisq')
# anova(n2a_full, n2a_comp_alt, test = 'Chisq')
# 
# rm(n1a_comp, n2a_comp, n1a_comp_alt, n2a_comp_alt)

# # Try including alternative age group information:
# n1a_comp <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                   s(perc_service) + s(perc_production) +
#                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n2a_comp <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) +
#                   s(perc_service) + s(perc_production) + s(cases_pre_rate) +
#                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# anova(n1a_full, n1a_comp, test = 'Chisq')
# anova(n2a_full, n2a_comp, test = 'Chisq')
# 
# rm(n1a_comp, n2a_comp)

# # Consider interactions:
# n1_1a_comp <- gam(cases_wave1_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 45) + s(ags2, bs = 're', k = 16) +
#                     s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                     s(perc_service) + s(perc_production) +
#                     ti(perc_18to64, pop_dens) + ti(perc_18to64, GISD_Score) + ti(pop_dens, GISD_Score) +
#                     ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, GISD_Score) +
#                     offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n1_2a_comp <- gam(cases_wave1_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
#                     s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                     s(perc_service) + s(perc_production) + s(cases_wave1_1_rate) +
#                     ti(perc_18to64, pop_dens) + ti(perc_18to64, GISD_Score) + ti(pop_dens, GISD_Score) +
#                     ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, GISD_Score) +
#                     offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# # n1a_comp <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
# #                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
# #                   s(perc_service) + s(perc_production) +
# #                   ti(perc_18to64, pop_dens) + ti(perc_18to64, GISD_Score) + ti(pop_dens, GISD_Score) +
# #                   ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, GISD_Score) +
# #                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n1b_comp <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
#                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + ti(pop_dens, GISD_Score) +
#                   offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# n2a_comp <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) +
#                   s(perc_service) + s(perc_production) + s(cases_pre2_rate) +
#                   ti(perc_18to64, pop_dens) + ti(perc_18to64, GISD_Score) + ti(pop_dens, GISD_Score) +
#                   ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, GISD_Score) +
#                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n2b_comp <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
#                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) + ti(pop_dens, GISD_Score) +
#                   s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# n3a_comp <- gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens, k = 25) +
#                   s(perc_service) + s(perc_production) +
#                   ti(perc_18to64, pop_dens) + ti(perc_18to64, GISD_Score) + ti(pop_dens, GISD_Score) +
#                   ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, GISD_Score) +
#                   s(cases_pre3_rate) + s(vacc_w3_reg) +
#                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n3b_comp <- gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
#                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + ti(pop_dens, GISD_Score) +
#                   s(cases_pre3_rate) + s(vacc_w3_reg) +
#                   offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# n4a_comp <- gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                   s(perc_service, k = 25) + s(perc_production) +
#                   ti(perc_18to64, pop_dens) + ti(perc_18to64, GISD_Score) + ti(pop_dens, GISD_Score) +
#                   ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, GISD_Score) +
#                   s(cases_pre4_rate) + s(vacc_w4_reg) +
#                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n4b_comp <- gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + ti(pop_dens, GISD_Score) +
#                   s(cases_pre4_rate) + s(vacc_w4_reg) +
#                   offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# par(mfrow = c(2, 2))
# gam.check(n1a_comp, rep = 50)
# gam.check(n2a_comp, rep = 50)
# gam.check(n3a_comp, rep = 50)
# gam.check(n4a_comp, rep = 50)
# 
# anova(n1_1a_full, n1_1a_comp, test = 'Chisq')
# anova(n1_2a_full, n1_2a_comp, test = 'Chisq')
# anova(n1a_full, n1a_comp, test = 'Chisq')
# anova(n1b_full, n1b_comp, test = 'Chisq')
# anova(n2a_full, n2a_comp, test = 'Chisq')
# anova(n2b_full, n2b_comp, test = 'Chisq')
# anova(n3a_full, n3a_comp, test = 'Chisq')
# anova(n3b_full, n3b_comp, test = 'Chisq')
# anova(n4a_full, n4a_comp, test = 'Chisq')
# anova(n4b_full, n4b_comp, test = 'Chisq')
# 
# n2a_comp <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) +
#                   s(perc_service) + s(perc_production) + s(cases_pre2_rate) +
#                   ti(perc_18to64, pop_dens) + ti(perc_18to64, GISD_Score) + ti(pop_dens, GISD_Score) +
#                   ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, GISD_Score) +
#                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n3a_comp <- gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens, k = 25) +
#                   s(perc_service) + s(perc_production) +
#                   ti(perc_18to64, pop_dens) + ti(perc_18to64, GISD_Score) + ti(pop_dens, GISD_Score) +
#                   ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, GISD_Score) +
#                   s(cases_pre3_rate) + s(vacc_w3_reg) +
#                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# n4a_comp <- gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                   s(perc_service, k = 25) + s(perc_production) +
#                   ti(perc_18to64, pop_dens) + ti(perc_18to64, GISD_Score) + ti(pop_dens, GISD_Score) +
#                   ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, GISD_Score) +
#                   s(cases_pre4_rate) + s(vacc_w4_reg) +
#                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML', select = TRUE)
# 
# n2a_comp <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) +
#                   s(perc_service) + s(perc_production) + s(cases_pre2_rate) +
#                   ti(perc_18to64, pop_dens) + ti(perc_lessthan18, pop_dens) +
#                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n3a_comp <- gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens, k = 25) +
#                   s(perc_service) + s(perc_production) +
#                   ti(perc_18to64, GISD_Score) + ti(perc_lessthan18, GISD_Score) +
#                   s(cases_pre3_rate) + s(vacc_w3_reg) +
#                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n4a_comp <- gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                   s(perc_service, k = 25) + s(perc_production) +
#                   ti(perc_lessthan18, GISD_Score) +
#                   s(cases_pre4_rate) + s(vacc_w4_reg) +
#                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# anova(n2a_full, n2a_comp, test = 'Chisq')
# anova(n3a_full, n3a_comp, test = 'Chisq')
# anova(n4a_full, n4a_comp, test = 'Chisq')

# # Include incidence as predictor for CFR models?:
# n1b_comp <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
#                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave1_rate) +
#                   offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
# n2b_comp <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
#                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) + s(cases_wave2_rate) +
#                   s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
# n3b_comp <- gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
#                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_rate) +
#                   s(cases_pre3_rate) + s(vacc_w3_reg) +
#                   offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
# n4b_comp <- gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_rate) +
#                   s(cases_pre4_rate) + s(vacc_w4_reg) +
#                   offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# anova(n1b_full, n1b_comp, test = 'Chisq')
# anova(n2b_full, n2b_comp, test = 'Chisq')
# anova(n3b_full, n3b_comp, test = 'Chisq')
# anova(n4b_full, n4b_comp, test = 'Chisq')
# 
# # And related interactions:
# n1b_comp <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                   s(hosp_beds, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave1_rate) +
#                   ti(cases_wave1_rate, hosp_beds) +
#                   offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
# n2b_comp <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) + s(cases_wave2_rate, k = 25) +
#                   ti(cases_wave2_rate, hosp_beds) +
#                   s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
# n3b_comp <- gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
#                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_rate) +
#                   ti(cases_wave3_rate, hosp_beds) +
#                   s(cases_pre3_rate) + s(vacc_w3_reg) +
#                   offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
# n4b_comp <- gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_rate) +
#                   ti(cases_wave4_rate, hosp_beds) +
#                   s(cases_pre4_rate) + s(vacc_w4_reg) +
#                   offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# anova(n1b_full, n1b_comp, test = 'Chisq')
# anova(n2b_full, n2b_comp, test = 'Chisq')
# anova(n3b_full, n3b_comp, test = 'Chisq')
# anova(n4b_full, n4b_comp, test = 'Chisq')

# Try Tweedie distribution:
n1_1a_comp <- gam(cases_wave1_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 45) + s(ags2, bs = 're', k = 16) +
                    s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                    s(perc_service) + s(perc_production) +
                    offset(log(pop)), data = dat_cumulative, family = 'tw', method = 'ML')
n1_2a_comp <- gam(cases_wave1_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                    s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                    s(perc_service) + s(perc_production) + s(cases_wave1_1_rate) +
                    offset(log(pop)), data = dat_cumulative, family = 'tw', method = 'ML')
# n1a_comp <-  gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
#                    s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                    s(perc_service) + s(perc_production) +
#                    offset(log(pop)), data = dat_cumulative, family = 'tw', method = 'ML')
n1b_comp <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave1_rate) +
                  offset(log(cases_wave1)), data = dat_cumulative, family = 'tw', method = 'ML')
n2a_comp <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_18to64, pop_dens, k = 8) + ti(perc_lessthan18, pop_dens) +
                  s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'tw', method = 'ML')
n2b_comp <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) + s(cases_wave2_rate, k = 25) +
                  s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'tw', method = 'ML')

n3a_comp <- gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_18to64, GISD_Score) + ti(perc_lessthan18, GISD_Score) +
                  s(cases_pre3_rate) + s(vacc_w3_reg) +
                  offset(log(pop)), data = dat_cumulative, family = 'tw', method = 'ML')
n3b_comp <- gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_rate) +
                  s(cases_pre3_rate) + s(vacc_w3_reg) +
                  offset(log(cases_wave3)), data = dat_cumulative, family = 'tw', method = 'ML')
n4a_comp <- gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_lessthan18, GISD_Score) +
                  s(cases_pre4_rate) + s(vacc_w4_reg) +
                  offset(log(pop)), data = dat_cumulative, family = 'tw', method = 'ML')
n4b_comp <- gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_rate) +
                  s(cases_pre4_rate) + s(vacc_w4_reg) +
                  offset(log(cases_wave4)), data = dat_cumulative, family = 'tw', method = 'ML')

AIC(n1_1a_full, n1_1a_comp)
AIC(n1_2a_full, n1_2a_comp)
AIC(n1b_full, n1b_comp)
AIC(n2a_full, n2a_comp)
AIC(n2b_full, n2b_comp)
AIC(n3a_full, n3a_comp)
AIC(n3b_full, n3b_comp)
AIC(n4a_full, n4a_comp)
AIC(n4b_full, n4b_comp)

# Try different spatial smooth types (s, sos, gp):
n1_1a_comp <- gam(cases_wave1_1 ~ s(long, lat, k = 45) + s(ags2, bs = 're', k = 16) +
                    s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                    s(perc_service) + s(perc_production) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n1_2a_comp <- gam(cases_wave1_2 ~ s(long, lat, k = 70) + s(ags2, bs = 're', k = 16) +
                    s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                    s(perc_service) + s(perc_production) + s(cases_wave1_1_rate) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n1a_comp <-  gam(cases_wave1 ~ s(long, lat, k = 60) + s(ags2, bs = 're', k = 16) +
#                    s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                    s(perc_service) + s(perc_production) +
#                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n1b_comp <- gam(deaths_wave1 ~ s(long, lat, k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave1_rate) +
                  offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
n2a_comp <- gam(cases_wave2 ~ s(long, lat, k = 65) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_18to64, pop_dens, k = 8) + ti(perc_lessthan18, pop_dens) +
                  s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n2b_comp <- gam(deaths_wave2 ~ s(long, lat, k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) + s(cases_wave2_rate, k = 25) +
                  s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')

n3a_comp <- gam(cases_wave3 ~ s(long, lat, k = 65) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_18to64, GISD_Score) + ti(perc_lessthan18, GISD_Score) +
                  s(cases_pre3_rate) + s(vacc_w3_reg) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n3b_comp <- gam(deaths_wave3 ~ s(long, lat, k = 50) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_rate) +
                  s(cases_pre3_rate) + s(vacc_w3_reg) +
                  offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
n4a_comp <- gam(cases_wave4 ~ s(long, lat, k = 40) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_lessthan18, GISD_Score) +
                  s(cases_pre4_rate) + s(vacc_w4_reg) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n4b_comp <- gam(deaths_wave4 ~ s(long, lat, k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_rate) +
                  s(cases_pre4_rate) + s(vacc_w4_reg) +
                  offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')

AIC(n1_1a_full, n1_1a_comp)
AIC(n1_2a_full, n1_2a_comp)
AIC(n1b_full, n1b_comp)
AIC(n2a_full, n2a_comp)
AIC(n2b_full, n2b_comp)
AIC(n3a_full, n3a_comp)
AIC(n3b_full, n3b_comp)
AIC(n4a_full, n4a_comp)
AIC(n4b_full, n4b_comp)

n1_1a_comp <- gam(cases_wave1_1 ~ s(long, lat, bs = 'sos', k = 45) + s(ags2, bs = 're', k = 16) +
                    s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                    s(perc_service) + s(perc_production) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n1_2a_comp <- gam(cases_wave1_2 ~ s(long, lat, bs = 'sos', k = 70) + s(ags2, bs = 're', k = 16) +
                    s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                    s(perc_service) + s(perc_production) + s(cases_wave1_1_rate) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n1a_comp <-  gam(cases_wave1 ~ s(long, lat, bs = 'sos', k = 60) + s(ags2, bs = 're', k = 16) +
#                    s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                    s(perc_service) + s(perc_production) +
#                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n1b_comp <- gam(deaths_wave1 ~ s(long, lat, bs = 'sos', k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave1_rate) +
                  offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
n2a_comp <- gam(cases_wave2 ~ s(long, lat, bs = 'sos', k = 65) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_18to64, pop_dens, k = 8) + ti(perc_lessthan18, pop_dens) +
                  s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n2b_comp <- gam(deaths_wave2 ~ s(long, lat, bs = 'sos', k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) + s(cases_wave2_rate, k = 25) +
                  s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')

n3a_comp <- gam(cases_wave3 ~ s(long, lat, bs = 'sos', k = 65) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_18to64, GISD_Score) + ti(perc_lessthan18, GISD_Score) +
                  s(cases_pre3_rate) + s(vacc_w3_reg) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n3b_comp <- gam(deaths_wave3 ~ s(long, lat, bs = 'sos', k = 50) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_rate) +
                  s(cases_pre3_rate) + s(vacc_w3_reg) +
                  offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
n4a_comp <- gam(cases_wave4 ~ s(long, lat, bs = 'sos', k = 40) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_lessthan18, GISD_Score) +
                  s(cases_pre4_rate) + s(vacc_w4_reg) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n4b_comp <- gam(deaths_wave4 ~ s(long, lat, bs = 'sos', k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_rate) +
                  s(cases_pre4_rate) + s(vacc_w4_reg) +
                  offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')

AIC(n1_1a_full, n1_1a_comp)
AIC(n1_2a_full, n1_2a_comp)
AIC(n1b_full, n1b_comp)
AIC(n2a_full, n2a_comp)
AIC(n2b_full, n2b_comp)
AIC(n3a_full, n3a_comp)
AIC(n3b_full, n3b_comp)
AIC(n4a_full, n4a_comp)
AIC(n4b_full, n4b_comp)

n1_1a_comp <- gam(cases_wave1_1 ~ s(long, lat, bs = 'gp', k = 45) + s(ags2, bs = 're', k = 16) +
                    s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                    s(perc_service) + s(perc_production) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n1_2a_comp <- gam(cases_wave1_2 ~ s(long, lat, bs = 'gp', k = 70) + s(ags2, bs = 're', k = 16) +
                    s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                    s(perc_service) + s(perc_production) + s(cases_wave1_1_rate) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n1a_comp <-  gam(cases_wave1 ~ s(long, lat, bs = 'gp', k = 60) + s(ags2, bs = 're', k = 16) +
#                    s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                    s(perc_service) + s(perc_production) +
#                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n1b_comp <- gam(deaths_wave1 ~ s(long, lat, bs = 'gp', k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave1_rate) +
                  offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
n2a_comp <- gam(cases_wave2 ~ s(long, lat, bs = 'gp', k = 65) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_18to64, pop_dens, k = 8) + ti(perc_lessthan18, pop_dens) +
                  s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n2b_comp <- gam(deaths_wave2 ~ s(long, lat, bs = 'gp', k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) + s(cases_wave2_rate, k = 25) +
                  s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')

n3a_comp <- gam(cases_wave3 ~ s(long, lat, bs = 'gp', k = 65) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_18to64, GISD_Score) + ti(perc_lessthan18, GISD_Score) +
                  s(cases_pre3_rate) + s(vacc_w3_reg) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n3b_comp <- gam(deaths_wave3 ~ s(long, lat, bs = 'gp', k = 50) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_rate) +
                  s(cases_pre3_rate) + s(vacc_w3_reg) +
                  offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
n4a_comp <- gam(cases_wave4 ~ s(long, lat, bs = 'gp', k = 40) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_lessthan18, GISD_Score) +
                  s(cases_pre4_rate) + s(vacc_w4_reg) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n4b_comp <- gam(deaths_wave4 ~ s(long, lat, bs = 'gp', k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_rate) +
                  s(cases_pre4_rate) + s(vacc_w4_reg) +
                  offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')

AIC(n1_1a_full, n1_1a_comp)
AIC(n1_2a_full, n1_2a_comp)
AIC(n1b_full, n1b_comp)
AIC(n2a_full, n2a_comp)
AIC(n2b_full, n2b_comp)
AIC(n3a_full, n3a_comp)
AIC(n3b_full, n3b_comp)
AIC(n4a_full, n4a_comp)
AIC(n4b_full, n4b_comp)

rm(n1_1a_comp, n1_2a_comp, n1b_comp, n2a_comp, n2b_comp, n3a_comp, n3b_comp, n4a_comp, n4b_comp)

# Try using MRF:
dat_cumulative$ARS <- factor(dat_cumulative$lk)

map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp') %>%
  filter(GEN != 'Eisenach', GEN != 'Wartburgkreis')

nb <- spdep::poly2nb(map_base, row.names = map_base$ARS)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

n1_1a_mrf <- gam(cases_wave1_1 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 45) + s(ags2, bs = 're', k = 16) +
                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                   s(perc_service) + s(perc_production) +
                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n1_2a_mrf <- gam(cases_wave1_2 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 70) + s(ags2, bs = 're', k = 16) +
                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                   s(perc_service) + s(perc_production) + s(cases_wave1_1_rate) +
                   offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n1a_mrf <- gam(cases_wave1 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 60) + s(ags2, bs = 're', k = 16) +
#                  s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                  s(perc_service) + s(perc_production) +
#                  offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n1b_mrf <- gam(deaths_wave1 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 40) + s(ags2, bs = 're', k = 16) +
                 s(hosp_beds, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave1_rate) +
                 offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
n2a_mrf <- gam(cases_wave2 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 65) + s(ags2, bs = 're', k = 16) +
                 s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                 s(perc_service) + s(perc_production) +
                 ti(perc_18to64, pop_dens, k = 8) + ti(perc_lessthan18, pop_dens) +
                 s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n2b_mrf <- gam(deaths_wave2 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 40) + s(ags2, bs = 're', k = 16) +
                 s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) + s(cases_wave2_rate, k = 25) +
                 s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
n3a_mrf <- gam(cases_wave3 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 65) + s(ags2, bs = 're', k = 16) +
                 s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                 s(perc_service) + s(perc_production) +
                 ti(perc_18to64, GISD_Score) + ti(perc_lessthan18, GISD_Score) +
                 s(cases_pre3_rate) + s(vacc_w3_reg) +
                 offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n3b_mrf <- gam(deaths_wave3 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 50) + s(ags2, bs = 're', k = 16) +
                 s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_rate) +
                 s(cases_pre3_rate) + s(vacc_w3_reg) +
                 offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
n4a_mrf <- gam(cases_wave4 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 40) + s(ags2, bs = 're', k = 16) +
                 s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                 s(perc_service) + s(perc_production) +
                 ti(perc_lessthan18, GISD_Score) +
                 s(cases_pre4_rate) + s(vacc_w4_reg) +
                 offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
n4b_mrf <- gam(deaths_wave4 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 40) + s(ags2, bs = 're', k = 16) +
                 s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_rate) +
                 s(cases_pre4_rate) + s(vacc_w4_reg) +
                 offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')

AIC(n1_1a_full, n1_1a_mrf)
BIC(n1_1a_full, n1_1a_mrf)

AIC(n1_2a_full, n1_2a_mrf)
BIC(n1_2a_full, n1_2a_mrf)

AIC(n1b_full, n1b_mrf)
BIC(n1b_full, n1b_mrf)

AIC(n2a_full, n2a_mrf)
BIC(n2a_full, n2a_mrf)

AIC(n2b_full, n2b_mrf)
BIC(n2b_full, n2b_mrf)

AIC(n3a_full, n3a_mrf)
BIC(n3a_full, n3a_mrf)

AIC(n3b_full, n3b_mrf)
BIC(n3b_full, n3b_mrf)

AIC(n4a_full, n4a_mrf)
BIC(n4a_full, n4a_mrf)

AIC(n4b_full, n4b_mrf)
BIC(n4b_full, n4b_mrf)

rm(n1_1a_mrf, n1_2a_mrf, n1b_mrf, n2a_mrf, n2b_mrf, n3a_mrf, n3b_mrf, n4a_mrf, n4b_mrf)

# Compare with poisson/zero-inflated:
n1_1a_pois <- gam(cases_wave1_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 45) + s(ags2, bs = 're', k = 16) +
                    s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                    s(perc_service) + s(perc_production) +
                    offset(log(pop)), data = dat_cumulative, family = 'poisson', method = 'ML')
n1_2a_pois <- gam(cases_wave1_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                    s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                    s(perc_service) + s(perc_production) + s(cases_wave1_1_rate) +
                    offset(log(pop)), data = dat_cumulative, family = 'poisson', method = 'ML')
# n1a_pois <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                   s(perc_service) + s(perc_production) +
#                   offset(log(pop)), data = dat_cumulative, family = 'poisson', method = 'ML')
n1b_pois <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave1_rate) +
                  offset(log(cases_wave1)), data = dat_cumulative, family = 'poisson', method = 'ML')
n2a_pois <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_18to64, pop_dens, k = 8) + ti(perc_lessthan18, pop_dens) +
                  s(cases_pre2_rate) + offset(log(pop)), data = dat_cumulative, family = 'poisson', method = 'ML')
n2b_pois <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) + s(cases_wave2_rate, k = 25) +
                  s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'poisson', method = 'ML')
n3a_pois <- gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 65) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_18to64, GISD_Score) + ti(perc_lessthan18, GISD_Score) +
                  s(cases_pre3_rate) + s(vacc_w3_reg) +
                  offset(log(pop)), data = dat_cumulative, family = 'poisson', method = 'ML')
n3b_pois <- gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_rate) +
                  s(cases_pre3_rate) + s(vacc_w3_reg) +
                  offset(log(cases_wave3)), data = dat_cumulative, family = 'poisson', method = 'ML')
n4a_pois <- gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  ti(perc_lessthan18, GISD_Score) +
                  s(cases_pre4_rate) + s(vacc_w4_reg) +
                  offset(log(pop)), data = dat_cumulative, family = 'poisson', method = 'ML')
n4b_pois <- gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_rate) +
                  s(cases_pre4_rate) + s(vacc_w4_reg) +
                  offset(log(cases_wave4)), data = dat_cumulative, family = 'poisson', method = 'ML')

n1b_zip <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                 s(hosp_beds, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave1_rate) +
                 offset(log(cases_wave1)), data = dat_cumulative, family = 'ziP', method = 'ML')

BIC(n1_1a_full, n1_1a_pois)
BIC(n1_2a_full, n1_2a_pois)
BIC(n1b_full, n1b_pois, n1b_zip)

BIC(n2a_full, n2a_pois)
BIC(n2b_full, n2b_pois)

BIC(n3a_full, n3a_pois)
BIC(n3b_full, n3b_pois)

BIC(n4a_full, n4a_pois)
BIC(n4b_full, n4b_pois)

rm(n1_1a_pois, n1_2a_pois, n1b_pois, n2a_pois, n2b_pois, n3a_pois, n3b_pois, n4a_pois, n4b_pois, n1b_zip)

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
