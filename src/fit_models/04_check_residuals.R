# ---------------------------------------------------------------------------------------------------------------------
# Check model residuals to determine quality of fits
# Models labeled "a" are models of incidence; models labeled "b" are models of CFR
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(sf)
library(spdep)
library(testthat)
library(Rcpp)
library(DHARMa)

# Load necessary functions:
source('src/functions/assess_results_fxns.R')

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Assess residuals

# Load models:
n1_1a_full <- read_rds('results/fitted_models/FULL_n1_1a_ml.rds')
n1_2a_full <- read_rds('results/fitted_models/FULL_n1_2a_ml.rds')
# n1a_full <- read_rds('results/fitted_models/FULL_n1a_ml.rds')
n1b_full <- read_rds('results/fitted_models/FULL_n1b_ml.rds')
n2a_full <- read_rds('results/fitted_models/FULL_n2a_ml.rds')
n2b_full <- read_rds('results/fitted_models/FULL_n2b_ml.rds')
n3a_full <- read_rds('results/fitted_models/FULL_n3a_ml.rds')
n3b_full <- read_rds('results/fitted_models/FULL_n3b_ml.rds')
n4a_full <- read_rds('results/fitted_models/FULL_n4a_ml.rds')
n4b_full <- read_rds('results/fitted_models/FULL_n4b_ml.rds')
n5a_full <- read_rds('results/fitted_models/FULL_n5a_ml.rds')
n5b_full <- read_rds('results/fitted_models/FULL_n5b_ml.rds')

# Add fitted values to data frame:
dat_cumulative$fitted_n1_1a <- fitted(n1_1a_full)
dat_cumulative$fitted_n1_2a <- fitted(n1_2a_full)
dat_cumulative$fitted_n1b <- fitted(n1b_full)
dat_cumulative$fitted_n2a <- fitted(n2a_full)
dat_cumulative$fitted_n2b <- fitted(n2b_full)
dat_cumulative$fitted_n3a <- fitted(n3a_full)
dat_cumulative$fitted_n3b <- fitted(n3b_full)
dat_cumulative$fitted_n4a <- fitted(n4a_full)
dat_cumulative$fitted_n4b <- fitted(n4b_full)
dat_cumulative$fitted_n5a <- fitted(n5a_full)
dat_cumulative$fitted_n5b <- fitted(n5b_full)

# Plot observed vs. fitted values:
par(mfrow = c(2, 2))
plot(dat_cumulative$cases_wave1_1, fitted(n1_1a_full),
     xlab = 'Observed Values', ylab = 'Fitted Values',
     main = 'Cases per Pop (Wave 1_1)', pch = 20)
lines(dat_cumulative$cases_wave1_1, dat_cumulative$cases_wave1_1,
      col = 'gray80')
plot(dat_cumulative$cases_wave1_2, fitted(n1_2a_full),
     xlab = 'Observed Values', ylab = 'Fitted Values',
     main = 'Cases per Pop (Wave 1_2)', pch = 20)
lines(dat_cumulative$cases_wave1_2, dat_cumulative$cases_wave1_2,
      col = 'gray80')
# plot(dat_cumulative$cases_wave1, fitted(n1a_full),
#      xlab = 'Observed Values', ylab = 'Fitted Values',
#      main = 'Cases per Pop (Wave 1)', pch = 20)
# lines(dat_cumulative$cases_wave1, dat_cumulative$cases_wave1,
#       col = 'gray80')
plot(dat_cumulative$deaths_wave1, fitted(n1b_full),
     xlab = 'Observed Values', ylab = 'Fitted Values',
     main = 'Deaths per Case (Wave 1)', pch = 20)
lines(dat_cumulative$deaths_wave1, dat_cumulative$deaths_wave1,
      col = 'gray80')
plot(dat_cumulative$cases_wave2, fitted(n2a_full),
     xlab = 'Observed Values', ylab = 'Fitted Values',
     main = 'Cases per Pop (Wave 2)', pch = 20)
lines(dat_cumulative$cases_wave2, dat_cumulative$cases_wave2,
      col = 'gray80')
plot(dat_cumulative$deaths_wave2, fitted(n2b_full),
     xlab = 'Observed Values', ylab = 'Fitted Values',
     main = 'Deaths per Case (Wave 2)', pch = 20)
lines(dat_cumulative$deaths_wave2, dat_cumulative$deaths_wave2,
      col = 'gray80')
plot(dat_cumulative$cases_wave3, fitted(n3a_full),
     xlab = 'Observed Values', ylab = 'Fitted Values',
     main = 'Cases per Pop (Wave 3)', pch = 20)
lines(dat_cumulative$cases_wave3, dat_cumulative$cases_wave3,
      col = 'gray80')
plot(dat_cumulative$deaths_wave3, fitted(n3b_full),
     xlab = 'Observed Values', ylab = 'Fitted Values',
     main = 'Deaths per Case (Wave 3)', pch = 20)
lines(dat_cumulative$deaths_wave3, dat_cumulative$deaths_wave3,
      col = 'gray80')
plot(dat_cumulative$cases_wave4, fitted(n4a_full),
     xlab = 'Observed Values', ylab = 'Fitted Values',
     main = 'Cases per Pop (Wave 4)', pch = 20)
lines(dat_cumulative$cases_wave4, dat_cumulative$cases_wave4,
      col = 'gray80')
plot(dat_cumulative$deaths_wave4, fitted(n4b_full),
     xlab = 'Observed Values', ylab = 'Fitted Values',
     main = 'Deaths per Case (Wave 4)', pch = 20)
lines(dat_cumulative$deaths_wave4, dat_cumulative$deaths_wave4,
      col = 'gray80')
plot(dat_cumulative$cases_wave5, fitted(n5a_full),
     xlab = 'Observed Values', ylab = 'Fitted Values',
     main = 'Cases per Pop (Wave 5)', pch = 20)
lines(dat_cumulative$cases_wave5, dat_cumulative$cases_wave5,
      col = 'gray80')
plot(dat_cumulative$deaths_wave5, fitted(n5b_full),
     xlab = 'Observed Values', ylab = 'Fitted Values',
     main = 'Deaths per Case (Wave 5)', pch = 20)
lines(dat_cumulative$deaths_wave5, dat_cumulative$deaths_wave5,
      col = 'gray80')

# Plot residuals vs. fitted values:
par(mfrow = c(2, 2))
plot(log(fitted(n1_1a_full)), residuals(n1_1a_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Cases per Pop (Wave 1_1)', pch = 20)#, col = dat_cumulative$bundesland)
lines(smooth.spline(log(fitted(n1_1a_full)), residuals(n1_1a_full, type = 'deviance')))
plot(log(fitted(n1_2a_full)), residuals(n1_2a_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Cases per Pop (Wave 1_2)', pch = 20)#, col = dat_cumulative$bundesland)
lines(smooth.spline(log(fitted(n1_2a_full)), residuals(n1_2a_full, type = 'deviance')))
# plot(log(fitted(n1a_full)), residuals(n1a_full, type = 'deviance'),
#      xlab = 'Fitted Values', ylab = 'Deviance Residuals',
#      main = 'Cases per Pop (Wave 1)', pch = 20)#, col = dat_cumulative$bundesland)
# lines(smooth.spline(log(fitted(n1a_full)), residuals(n1a_full, type = 'deviance')))
plot(log(fitted(n1b_full)), residuals(n1b_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Deaths per Case (Wave 1)', pch = 20)#, col = dat_cumulative$bundesland)
lines(smooth.spline(log(fitted(n1b_full)), residuals(n1b_full, type = 'deviance')))
plot(log(fitted(n2a_full)), residuals(n2a_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Cases per Pop (Wave 2)', pch = 20)#, col = dat_cumulative$bundesland)
lines(smooth.spline(log(fitted(n2a_full)), residuals(n2a_full, type = 'deviance')))
plot(log(fitted(n2b_full)), residuals(n2b_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Deaths per Case (Wave 2)', pch = 20)#, col = dat_cumulative$bundesland)
lines(smooth.spline(log(fitted(n2b_full)), residuals(n2b_full, type = 'deviance')))
plot(log(fitted(n3a_full)), residuals(n3a_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Cases per Pop (Wave 3)', pch = 20)#, col = dat_cumulative$bundesland)
lines(smooth.spline(log(fitted(n3a_full)), residuals(n3a_full, type = 'deviance')))
plot(log(fitted(n3b_full)), residuals(n3b_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Deaths per Case (Wave 3)', pch = 20)#, col = dat_cumulative$bundesland)
lines(smooth.spline(log(fitted(n3b_full)), residuals(n3b_full, type = 'deviance')))
plot(log(fitted(n4a_full)), residuals(n4a_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Cases per Pop (Wave 4)', pch = 20)#, col = dat_cumulative$bundesland)
lines(smooth.spline(log(fitted(n4a_full)), residuals(n4a_full, type = 'deviance')))
plot(log(fitted(n4b_full)), residuals(n4b_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Deaths per Case (Wave 4)', pch = 20)#, col = dat_cumulative$bundesland)
lines(smooth.spline(log(fitted(n4b_full)), residuals(n4b_full, type = 'deviance')))
plot(log(fitted(n5a_full)), residuals(n5a_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Cases per Pop (Wave 5)', pch = 20)#, col = dat_cumulative$bundesland)
lines(smooth.spline(log(fitted(n5a_full)), residuals(n5a_full, type = 'deviance')))
plot(log(fitted(n5b_full)), residuals(n5b_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Deaths per Case (Wave 5)', pch = 20)#, col = dat_cumulative$bundesland)
lines(smooth.spline(log(fitted(n5b_full)), residuals(n5b_full, type = 'deviance')))

# Check using DHARMa package:
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
# https://aosmith.rbind.io/2017/12/21/using-dharma-for-residual-checks-of-unsupported-models/
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n1_1a_full, depend = 'cases')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n1_2a_full, depend = 'cases')
# par(mfrow = c(2, 2))
# check_dharma(dat_cumulative, n1a_full, depend = 'cases')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n1b_full, depend = 'deaths')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n2a_full, depend = 'cases')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n2b_full, depend = 'deaths')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n3a_full, depend = 'cases')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n3b_full, depend = 'deaths')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n4a_full, depend = 'cases')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n4b_full, depend = 'deaths')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n5a_full, depend = 'cases')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n5b_full, depend = 'deaths')

# Calculate Moran's I for residuals:
dat_cumulative$resid_n1_1a <- residuals(n1_1a_full, type = 'deviance')
dat_cumulative$resid_n1_2a <- residuals(n1_2a_full, type = 'deviance')
# dat_cumulative$resid_n1a <- residuals(n1a_full, type = 'deviance')
dat_cumulative$resid_n1b <- residuals(n1b_full, type = 'deviance')
dat_cumulative$resid_n2a <- residuals(n2a_full, type = 'deviance')
dat_cumulative$resid_n2b <- residuals(n2b_full, type = 'deviance')
dat_cumulative$resid_n3a <- residuals(n3a_full, type = 'deviance')
dat_cumulative$resid_n3b <- residuals(n3b_full, type = 'deviance')
dat_cumulative$resid_n4a <- residuals(n4a_full, type = 'deviance')
dat_cumulative$resid_n4b <- residuals(n4b_full, type = 'deviance')
dat_cumulative$resid_n5a <- residuals(n5a_full, type = 'deviance')
dat_cumulative$resid_n5b <- residuals(n5b_full, type = 'deviance')

map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')
map_base <- map_base %>%
  left_join(dat_cumulative %>%
              select(lk, resid_n1_1a, resid_n1_2a, resid_n1b, resid_n2a, resid_n2b,
                     resid_n3a, resid_n3b, resid_n4a, resid_n4b, resid_n5a, resid_n5b),
            by = c('ARS' = 'lk')) %>%
  drop_na()

nb <- spdep::poly2nb(map_base, row.names = map_base$ARS)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)

moran.mc(map_base$resid_n1_1a, lw, nsim = 999)
moran.mc(map_base$resid_n1_2a, lw, nsim = 999)
# moran.mc(map_base$resid_n1a, lw, nsim = 999)
moran.mc(map_base$resid_n1b, lw, nsim = 999)
moran.mc(map_base$resid_n2a, lw, nsim = 999)
moran.mc(map_base$resid_n2b, lw, nsim = 999)
moran.mc(map_base$resid_n3a, lw, nsim = 999)
moran.mc(map_base$resid_n3b, lw, nsim = 999)
moran.mc(map_base$resid_n4a, lw, nsim = 999)
moran.mc(map_base$resid_n4b, lw, nsim = 999)
moran.mc(map_base$resid_n5a, lw, nsim = 999)
moran.mc(map_base$resid_n5b, lw, nsim = 999)
# none are significant, except n5b

# Clean up:
rm(list = ls())

# ---------------------------------------------------------------------------------------------------------------------
