# ---------------------------------------------------------------------------------------------------------------------
# Run and assess univariable GAMs exploring the role of various predictors
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(ggeffects)
library(sf)
library(testthat)
library(spdep)
library(viridis)
library(gridExtra)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Formulate and fit "univariable" models

# Wave 1:
n1a_care_home_beds <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                            s(care_home_beds) + offset(log(pop)), data = dat_cumulative, family = 'nb')
n1a_GISD <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                  s(GISD_Score) + offset(log(pop)), data = dat_cumulative, family = 'nb')
n1a_pop_dens <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                      s(pop_dens) + offset(log(pop)), data = dat_cumulative, family = 'nb')
n1a_living_area <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                         s(living_area, k = 15) + offset(log(pop)), data = dat_cumulative, family = 'nb')
n1a_perc_serv <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                       s(perc_service) + offset(log(pop)), data = dat_cumulative, family = 'nb')
n1a_perc_prod <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                       s(perc_production) + offset(log(pop)), data = dat_cumulative, family = 'nb')

n1b_hosp_beds <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                       s(hosp_beds) + offset(log(cases_wave1)), data = dat_cumulative, family = 'nb')
n1b_care_home_beds <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                            s(care_home_beds) + offset(log(cases_wave1)), data = dat_cumulative, family = 'nb')
n1b_GISD <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                  s(GISD_Score) + offset(log(cases_wave1)), data = dat_cumulative, family = 'nb')

# Wave 2:
n2a_care_home_beds <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                            s(care_home_beds, k = 15) + s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb')
n2a_GISD <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                  s(GISD_Score) + s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb')
n2a_pop_dens <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                      s(pop_dens) + s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb')
n2a_living_area <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                         s(living_area) + s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb')
n2a_perc_serv <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                       s(perc_service) + s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb')
n2a_perc_prod <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                       s(perc_production) + s(cases_pre_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb')

n2b_hosp_beds <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                       s(hosp_beds) + s(deaths_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb')
n2b_care_home_beds <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                            s(care_home_beds) + s(deaths_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb')
n2b_GISD <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                  s(GISD_Score) + s(deaths_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb')

# Assess fit and plot significant variables:
mod_list1 <- list(n1a_hosp_beds, n1a_care_home_beds, n1a_GISD, n1a_pop_dens, n1a_living_area, n1a_perc_serv, n1a_perc_prod,
                  n1b_hosp_beds, n1b_care_home_beds, n1b_GISD)
mod_list2 <- list(n2a_hosp_beds, n2a_care_home_beds, n2a_GISD, n2a_pop_dens, n2a_living_area, n2a_perc_serv, n2a_perc_prod,
                  n2b_hosp_beds, n2b_care_home_beds, n2b_GISD)

par(mfrow = c(2, 2))
for (mod in mod_list1) {
  gam.check(mod, rep = 50)
}
for (mod in mod_list2) {
  gam.check(mod, rep = 50)
}

for (mod in mod_list1) {
  print(summary(mod))
}
for (mod in mod_list2) {
  print(summary(mod))
}

plot(n1a_GISD, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(n1b_care_home_beds, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)

plot(n2a_care_home_beds, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(n2a_GISD, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
# plot(n2a_pop_dens, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE) # p=0.0993
plot(n2a_living_area, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
# plot(n2a_perc_serv, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE) # p=0.0592

plot(n2b_care_home_beds, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(n2b_GISD, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)

# ---------------------------------------------------------------------------------------------------------------------
