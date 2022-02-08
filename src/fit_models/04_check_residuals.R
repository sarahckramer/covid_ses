# ---------------------------------------------------------------------------------------------------------------------
# Check model residuals to determine quality of fits
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

n1a <- read_rds('results/fitted_models/null_n1a_ml.rds')
n1b <- read_rds('results/fitted_models/null_n1b_ml.rds')
n2a <- read_rds('results/fitted_models/null_n2a_ml.rds')
n2b <- read_rds('results/fitted_models/null_n2b_ml.rds')

# Add fitted values to data frame:
dat_cumulative$fitted_n1a <- fitted(n1a_full)
dat_cumulative$fitted_n1b <- fitted(n1b_full)
dat_cumulative$fitted_n2a <- fitted(n2a_full)
dat_cumulative$fitted_n2b <- fitted(n2b_full)

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

# par(mfrow = c(2, 2))
# check_dharma(dat_cumulative, n1a, depend = 'none')
# par(mfrow = c(2, 2))
# check_dharma(dat_cumulative, n1b, depend = 'none')
# par(mfrow = c(2, 2))
# check_dharma(dat_cumulative, n2a, depend = 'none')
# par(mfrow = c(2, 2))
# check_dharma(dat_cumulative, n2b, depend = 'none')

# Compare model-generated data to observed data:
n1a_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 7.905, mu = predict(n1a_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$cases_wave1) %>%
  bind_cols(pop = dat_cumulative$pop) %>%
  pivot_longer(V1:V5, names_to = 'id') %>%
  mutate(obs = obs / pop * 10000,
         value = value / pop * 10000)
n1b_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 4.944, mu = predict(n1b_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$deaths_wave1) %>%
  bind_cols(pop = dat_cumulative$pop) %>%
  pivot_longer(V1:V5, names_to = 'id') %>%
  mutate(obs = obs / pop * 10000,
         value = value / pop * 10000)
n2a_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 34.826, mu = predict(n2a_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$cases_wave2) %>%
  bind_cols(pop = dat_cumulative$pop) %>%
  pivot_longer(V1:V5, names_to = 'id') %>%
  mutate(obs = obs / pop * 10000,
         value = value / pop * 10000)
n2b_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 16.756, mu = predict(n2b_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$deaths_wave2) %>%
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

print(p1a)
print(p1b)
print(p2a)
print(p2b)

# Calculate Moran's I for residuals:
dat_cumulative$resid_n1a <- residuals(n1a_full, type = 'deviance')
dat_cumulative$resid_n1b <- residuals(n1b_full, type = 'deviance')
dat_cumulative$resid_n2a <- residuals(n2a_full, type = 'deviance')
dat_cumulative$resid_n2b <- residuals(n2b_full, type = 'deviance')

map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')
map_base <- map_base %>%
  left_join(dat_cumulative %>%
              select(lk, resid_n1a, resid_n1b, resid_n2a, resid_n2b),
            by = c('ARS' = 'lk'))

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)
moran.mc(map_base$resid_n1a, lw, nsim = 999)
moran.mc(map_base$resid_n1b, lw, nsim = 999)
moran.mc(map_base$resid_n2a, lw, nsim = 999)
moran.mc(map_base$resid_n2b, lw, nsim = 999)
# none are significant

# Clean up:
rm(list = ls())

# ---------------------------------------------------------------------------------------------------------------------
