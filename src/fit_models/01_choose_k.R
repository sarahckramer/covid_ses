# ---------------------------------------------------------------------------------------------------------------------
# Determine optimal values of k for the spatial effect
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
n1a_mods = n1b_mods = n2a_mods = n2b_mods = vector('list', length = length(seq(10, 150, by = 10)))
for (i in 1:length(seq(10, 150, by = 10))) {
  k_val <- seq(10, 150, by = 10)[i]
  n1a_temp <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                    s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + #s(living_area, k = 25) +
                    s(perc_service) + s(perc_production) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
  n1b_temp <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                    s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + #s(pop_dens) +
                    offset(log(cases_wave1)), data = dat_cumulative, family = 'nb', method = 'ML')

  n2a_temp <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                    s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) + #s(living_area) +
                    s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
  n2b_temp <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
                    s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + #s(pop_dens) +
                    s(cases_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb', method = 'ML')

  n1a_mods[[i]] <- n1a_temp
  n1b_mods[[i]] <- n1b_temp
  n2a_mods[[i]] <- n2a_temp
  n2b_mods[[i]] <- n2b_temp
}

# Evaluate AICs and BICs:
n1a_aics <- lapply(n1a_mods, AIC) %>% unlist()
n1b_aics <- lapply(n1b_mods, AIC) %>% unlist()
n2a_aics <- lapply(n2a_mods, AIC) %>% unlist()
n2b_aics <- lapply(n2b_mods, AIC) %>% unlist()

n1a_bics <- lapply(n1a_mods, BIC) %>% unlist()
n1b_bics <- lapply(n1b_mods, BIC) %>% unlist()
n2a_bics <- lapply(n2a_mods, BIC) %>% unlist()
n2b_bics <- lapply(n2b_mods, BIC) %>% unlist()

# Plot:
par(mfrow = c(2, 1))
plot(seq(10, 150, by = 10), n1a_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n1a_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

plot(seq(10, 150, by = 10), n1b_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n1b_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

plot(seq(10, 150, by = 10), n2a_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n2a_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

plot(seq(10, 150, by = 10), n2b_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
plot(seq(10, 150, by = 10), n2b_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')
