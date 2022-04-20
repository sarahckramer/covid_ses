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
n1a_full <- read_rds('results/fitted_models/FULL_n1a_ml.rds')
n1b_full <- read_rds('results/fitted_models/FULL_n1b_ml.rds')
n2a_full <- read_rds('results/fitted_models/FULL_n2a_ml.rds')
n2b_full <- read_rds('results/fitted_models/FULL_n2b_ml.rds')
n3a_full <- read_rds('results/fitted_models/FULL_n3a_ml.rds')
n3b_full <- read_rds('results/fitted_models/FULL_n3b_ml.rds')
n4a_full <- read_rds('results/fitted_models/FULL_n4a_ml.rds')
n4b_full <- read_rds('results/fitted_models/FULL_n4b_ml.rds')

# Add fitted values to data frame:
dat_cumulative$fitted_n1a <- fitted(n1a_full)
dat_cumulative$fitted_n1b <- fitted(n1b_full)
dat_cumulative$fitted_n2a <- fitted(n2a_full)
dat_cumulative$fitted_n2b <- fitted(n2b_full)
dat_cumulative$fitted_n3a <- fitted(n3a_full)
dat_cumulative$fitted_n3b <- fitted(n3b_full)
dat_cumulative$fitted_n4a <- fitted(n4a_full)
dat_cumulative$fitted_n4b <- fitted(n4b_full)

# Plot observed vs. fitted values:
par(mfrow = c(2, 2))
plot(dat_cumulative$cases_wave1, fitted(n1a_full),
     xlab = 'Observed Values', ylab = 'Fitted Values',
     main = 'Cases per Pop (Wave 1)', pch = 20)
lines(dat_cumulative$cases_wave1, dat_cumulative$cases_wave1,
      col = 'gray80')
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

# Plot residuals vs. fitted values:
plot(log(fitted(n1a_full)), residuals(n1a_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Cases per Pop (Wave 1)', pch = 20)#, col = dat_cumulative$bundesland)
lines(smooth.spline(log(fitted(n1a_full)), residuals(n1a_full, type = 'deviance')))
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

# Check using DHARMa package:
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
# https://aosmith.rbind.io/2017/12/21/using-dharma-for-residual-checks-of-unsupported-models/
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n1a_full, depend = 'cases')
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

# Compare model-generated data to observed data:
n1a_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 7.985, mu = predict(n1a_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$cases_wave1) %>%
  bind_cols(pop = dat_cumulative$pop) %>%
  pivot_longer(V1:V5, names_to = 'id') %>%
  mutate(obs = obs / pop * 10000,
         value = value / pop * 10000)
n1b_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 6.336, mu = predict(n1b_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$deaths_wave1) %>%
  bind_cols(pop = dat_cumulative$pop) %>%
  pivot_longer(V1:V5, names_to = 'id') %>%
  mutate(obs = obs / pop * 10000,
         value = value / pop * 10000)
n2a_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 34.594, mu = predict(n2a_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$cases_wave2) %>%
  bind_cols(pop = dat_cumulative$pop) %>%
  pivot_longer(V1:V5, names_to = 'id') %>%
  mutate(obs = obs / pop * 10000,
         value = value / pop * 10000)
n2b_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 17.448, mu = predict(n2b_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$deaths_wave2) %>%
  bind_cols(pop = dat_cumulative$pop) %>%
  pivot_longer(V1:V5, names_to = 'id') %>%
  mutate(obs = obs / pop * 10000,
         value = value / pop * 10000)
n3a_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 40.901, mu = predict(n3a_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$cases_wave3) %>%
  bind_cols(pop = dat_cumulative$pop) %>%
  pivot_longer(V1:V5, names_to = 'id') %>%
  mutate(obs = obs / pop * 10000,
         value = value / pop * 10000)
n3b_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 19.356, mu = predict(n3b_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$deaths_wave3) %>%
  bind_cols(pop = dat_cumulative$pop) %>%
  pivot_longer(V1:V5, names_to = 'id') %>%
  mutate(obs = obs / pop * 10000,
         value = value / pop * 10000)
n4a_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 51.511, mu = predict(n4a_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$cases_wave4) %>%
  bind_cols(pop = dat_cumulative$pop) %>%
  pivot_longer(V1:V5, names_to = 'id') %>%
  mutate(obs = obs / pop * 10000,
         value = value / pop * 10000)
n4b_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 15.68, mu = predict(n4b_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$deaths_wave4) %>%
  bind_cols(pop = dat_cumulative$pop) %>%
  pivot_longer(V1:V5, names_to = 'id') %>%
  mutate(obs = obs / pop * 10000,
         value = value / pop * 10000)

p1a <- ggplot(data = n1a_comp) +
  geom_histogram(aes(x = obs), alpha = 0.5, binwidth = 5) +
  geom_histogram(aes(x = value), alpha = 0.5, fill = 'coral', binwidth = 5) +
  facet_wrap(~ id) + theme_classic()
p1b <- ggplot(data = n1b_comp) +
  geom_histogram(aes(x = obs), alpha = 0.5, binwidth = 1) +
  geom_histogram(aes(x = value), alpha = 0.5, fill = 'coral', binwidth = 1) +
  facet_wrap(~ id) + theme_classic()
p2a <- ggplot(data = n2a_comp) +
  geom_histogram(aes(x = obs), alpha = 0.5, binwidth = 15) +
  geom_histogram(aes(x = value), alpha = 0.5, fill = 'coral', binwidth = 15) +
  facet_wrap(~ id) + theme_classic()
p2b <- ggplot(data = n2b_comp) +
  geom_histogram(aes(x = obs), alpha = 0.5, binwidth = 1) +
  geom_histogram(aes(x = value), alpha = 0.5, fill = 'coral', binwidth = 1) +
  facet_wrap(~ id) + theme_classic()
p3a <- ggplot(data = n3a_comp) +
  geom_histogram(aes(x = obs), alpha = 0.5, binwidth = 10) +
  geom_histogram(aes(x = value), alpha = 0.5, fill = 'coral', binwidth = 10) +
  facet_wrap(~ id) + theme_classic()
p3b <- ggplot(data = n3b_comp) +
  geom_histogram(aes(x = obs), alpha = 0.5, binwidth = 1) +
  geom_histogram(aes(x = value), alpha = 0.5, fill = 'coral', binwidth = 1) +
  facet_wrap(~ id) + theme_classic()
p4a <- ggplot(data = n4a_comp) +
  geom_histogram(aes(x = obs), alpha = 0.5, binwidth = 25) +
  geom_histogram(aes(x = value), alpha = 0.5, fill = 'coral', binwidth = 25) +
  facet_wrap(~ id) + theme_classic()
p4b <- ggplot(data = n4b_comp) +
  geom_histogram(aes(x = obs), alpha = 0.5, binwidth = 1) +
  geom_histogram(aes(x = value), alpha = 0.5, fill = 'coral', binwidth = 1) +
  facet_wrap(~ id) + theme_classic()

print(p1a)
print(p1b)
print(p2a)
print(p2b)
print(p3a)
print(p3b)
print(p4a)
print(p4b)

# Calculate Moran's I for residuals:
dat_cumulative$resid_n1a <- residuals(n1a_full, type = 'deviance')
dat_cumulative$resid_n1b <- residuals(n1b_full, type = 'deviance')
dat_cumulative$resid_n2a <- residuals(n2a_full, type = 'deviance')
dat_cumulative$resid_n2b <- residuals(n2b_full, type = 'deviance')
dat_cumulative$resid_n3a <- residuals(n3a_full, type = 'deviance')
dat_cumulative$resid_n3b <- residuals(n3b_full, type = 'deviance')
dat_cumulative$resid_n4a <- residuals(n4a_full, type = 'deviance')
dat_cumulative$resid_n4b <- residuals(n4b_full, type = 'deviance')

map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')
map_base <- map_base %>%
  left_join(dat_cumulative %>%
              select(lk, resid_n1a, resid_n1b, resid_n2a, resid_n2b,
                     resid_n3a, resid_n3b, resid_n4a, resid_n4b),
            by = c('ARS' = 'lk')) %>%
  drop_na()

nb <- spdep::poly2nb(map_base, row.names = map_base$ARS)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)
moran.mc(map_base$resid_n1a, lw, nsim = 999)
moran.mc(map_base$resid_n1b, lw, nsim = 999)
moran.mc(map_base$resid_n2a, lw, nsim = 999)
moran.mc(map_base$resid_n2b, lw, nsim = 999)
moran.mc(map_base$resid_n3a, lw, nsim = 999)
moran.mc(map_base$resid_n3b, lw, nsim = 999)
moran.mc(map_base$resid_n4a, lw, nsim = 999)
moran.mc(map_base$resid_n4b, lw, nsim = 999)
# none are significant

# Clean up:
rm(list = ls())

# ---------------------------------------------------------------------------------------------------------------------
