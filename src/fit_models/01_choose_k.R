# ---------------------------------------------------------------------------------------------------------------------
# Determine optimal values of k for the spatial effect
# Models labeled "a" are models of incidence; models labeled "b" are models of CFR
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(testthat)
library(sf)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Determine best range of values for k for lat/long

# Loop through values and fit models:
n1_1a_mods = n1_2a_mods = n1b_mods = n2a_mods = n2b_mods = n3a_mods = n3b_mods = n4a_mods = n4b_mods = n5a_mods = n5b_mods = vector('list', length = length(seq(10, 150, by = 10)))
for (i in 1:length(seq(10, 150, by = 10))) {
  k_val <- seq(10, 150, by = 10)[i]
  
  n1_1a_mods[[i]] <- gam(cases_wave1_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                           s(perc_service) + s(perc_production) +
                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
  n1_2a_mods[[i]] <- gam(cases_wave1_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) +
                           s(perc_service) + s(perc_production) + s(cases_wave1_1_rate) +
                           offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
  n1b_mods[[i]] <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave1_rate) +
                         offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')
  
  n2a_mods[[i]] <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) + #s(living_area) +
                         s(perc_service) + s(perc_production) + s(cases_pre2_rate) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
  n2b_mods[[i]] <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave2_rate) +
                         s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')
  
  n3a_mods[[i]] <- gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         s(cases_pre3_rate) + s(vacc_w3_reg) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
  n3b_mods[[i]] <- gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_rate, k = 25) +
                         s(cases_pre3_rate) + s(vacc_w3_reg) + ti(pop_dens, GISD_Score) +
                         offset(log(cases_wave3)), data = dat_cumulative, family = 'nb', method = 'ML')
  
  n4a_mods[[i]] <- gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         s(cases_pre4_rate) + s(vacc_w4_reg) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
  n4b_mods[[i]] <- gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_rate) +
                         s(cases_pre4_rate) + s(vacc_w4_reg) +
                         offset(log(cases_wave4)), data = dat_cumulative, family = 'nb', method = 'ML')
  
  n5a_mods[[i]] <- gam(cases_wave5 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) + s(cases_pre5_rate, k = 25) + s(vacc_w5_reg) +
                         offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
  n5b_mods[[i]] <- gam(deaths_wave5 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave5_rate) +
                         s(cases_pre5_rate) + s(vacc_w5_reg) +
                         offset(log(cases_wave5)), data = dat_cumulative, family = 'nb', method = 'ML')
}

# Evaluate AICs and BICs:
n1_1a_aics <- lapply(n1_1a_mods, AIC) %>% unlist()
n1_2a_aics <- lapply(n1_2a_mods, AIC) %>% unlist()
n1b_aics <- lapply(n1b_mods, AIC) %>% unlist()
n2a_aics <- lapply(n2a_mods, AIC) %>% unlist()
n2b_aics <- lapply(n2b_mods, AIC) %>% unlist()
n3a_aics <- lapply(n3a_mods, AIC) %>% unlist()
n3b_aics <- lapply(n3b_mods, AIC) %>% unlist()
n4a_aics <- lapply(n4a_mods, AIC) %>% unlist()
n4b_aics <- lapply(n4b_mods, AIC) %>% unlist()
n5a_aics <- lapply(n5a_mods, AIC) %>% unlist()
n5b_aics <- lapply(n5b_mods, AIC) %>% unlist()

n1_1a_bics <- lapply(n1_1a_mods, BIC) %>% unlist()
n1_2a_bics <- lapply(n1_2a_mods, BIC) %>% unlist()
n1b_bics <- lapply(n1b_mods, BIC) %>% unlist()
n2a_bics <- lapply(n2a_mods, BIC) %>% unlist()
n2b_bics <- lapply(n2b_mods, BIC) %>% unlist()
n3a_bics <- lapply(n3a_mods, BIC) %>% unlist()
n3b_bics <- lapply(n3b_mods, BIC) %>% unlist()
n4a_bics <- lapply(n4a_mods, BIC) %>% unlist()
n4b_bics <- lapply(n4b_mods, BIC) %>% unlist()
n5a_bics <- lapply(n5a_mods, BIC) %>% unlist()
n5b_bics <- lapply(n5b_mods, BIC) %>% unlist()

# Plot:
par(mfrow = c(2, 1))
plot(seq(10, 150, by = 10), n1_1a_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n1_1a_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

plot(seq(10, 150, by = 10), n1_2a_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n1_2a_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

plot(seq(10, 150, by = 10), n1b_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n1b_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

plot(seq(10, 150, by = 10), n2a_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n2a_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

plot(seq(10, 150, by = 10), n2b_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n2b_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

plot(seq(10, 150, by = 10), n3a_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n3a_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

plot(seq(10, 150, by = 10), n3b_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n3b_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

plot(seq(10, 150, by = 10), n4a_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n4a_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

plot(seq(10, 150, by = 10), n4b_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n4b_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

plot(seq(10, 150, by = 10), n5a_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n5a_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

plot(seq(10, 150, by = 10), n5b_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n5b_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')
