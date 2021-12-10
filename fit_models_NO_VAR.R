# ---------------------------------------------------------------------------------------------------------------------
# Run and assess GAMs with no SES predictors to explore spatial patterns
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

# Formulate and fit models (lat/long only)

# Without predictors:
n1a <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
             offset(log(pop)), data = dat_cumulative, family = 'nb')
n1b <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
             offset(log(pop)), data = dat_cumulative, family = 'nb')
n1c <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
             offset(log(cases_wave1)), data = dat_cumulative, family = 'nb')

n2a <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
             offset(log(pop)), data = dat_cumulative, family = 'nb')
n2b <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
             offset(log(pop)), data = dat_cumulative, family = 'nb')
n2c <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
             offset(log(cases_wave2)), data = dat_cumulative, family = 'nb')

par(mfrow = c(2, 2))
gam.check(n1a, rep = 50)
gam.check(n1b, rep = 50)
gam.check(n1c, rep = 50)
gam.check(n2a, rep = 50)
gam.check(n2b, rep = 50)
gam.check(n2c, rep = 50)

# ---------------------------------------------------------------------------------------------------------------------
