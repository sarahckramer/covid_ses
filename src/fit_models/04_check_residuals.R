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
library(gridExtra)
library(viridis)

# Load necessary functions:
source('src/functions/assess_results.R')

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Assess residuals

# Load models:
n1a_full <- read_rds('results/fitted_models/FULL_n1a.rds')
n1b_full <- read_rds('results/fitted_models/FULL_n1b.rds')
n2a_full <- read_rds('results/fitted_models/FULL_n2a.rds')
n2b_full <- read_rds('results/fitted_models/FULL_n2b.rds')

n1a <- read_rds('results/fitted_models/null_n1a.rds')
n1b <- read_rds('results/fitted_models/null_n1b.rds')
n2a <- read_rds('results/fitted_models/null_n2a.rds')
n2b <- read_rds('results/fitted_models/null_n2b.rds')

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

for (bl in unique(dat_cumulative$bundesland)) {
  print(bl)
  dat_temp <- dat_cumulative %>% filter(bundesland == bl)
  
  plot(dat_temp$cases_wave1, dat_temp$fitted_n1a,
       xlab = 'Observed Values', ylab = 'Fitted Values',
       main = paste0(bl, ' Cases per Pop (Wave 1)'), pch = 20)
  lines(dat_cumulative$cases_wave1, dat_cumulative$cases_wave1,
        col = 'gray80')
  plot(dat_temp$deaths_wave1, dat_temp$fitted_n1b,
       xlab = 'Observed Values', ylab = 'Fitted Values',
       main = 'Deaths per Case (Wave 1)', pch = 20)
  lines(dat_cumulative$deaths_wave1, dat_cumulative$deaths_wave1,
        col = 'gray80')
  plot(dat_temp$cases_wave2, dat_temp$fitted_n2a,
       xlab = 'Observed Values', ylab = 'Fitted Values',
       main = 'Cases per Pop (Wave 2)', pch = 20)
  lines(dat_cumulative$cases_wave2, dat_cumulative$cases_wave2,
        col = 'gray80')
  plot(dat_temp$deaths_wave2, dat_temp$fitted_n2b,
       xlab = 'Observed Values', ylab = 'Fitted Values',
       main = 'Deaths per Case (Wave 2)', pch = 20)
  lines(dat_cumulative$deaths_wave2, dat_cumulative$deaths_wave2,
        col = 'gray80')
}

for (predictor in c('GISD_Score', 'perc_18to64', 'perc_service', 'perc_production', 'care_home_beds', 'hosp_beds',
                    'pop_dens', 'living_area', 'lat', 'long')) {
  dat_temp <- dat_cumulative %>% rename(pred_temp = all_of(predictor))
  
  p1 <- ggplot(data = dat_temp, aes(x = cases_wave1, y = fitted_n1a, col = pred_temp)) +
    geom_point() + theme_classic() + scale_color_viridis() + labs(title = predictor)
  p2 <- ggplot(data = dat_temp, aes(x = deaths_wave1, y = fitted_n1b, col = pred_temp)) +
    geom_point() + theme_classic() + scale_color_viridis() + labs(title = predictor)
  p3 <- ggplot(data = dat_temp, aes(x = cases_wave2, y = fitted_n2a, col = pred_temp)) +
    geom_point() + theme_classic() + scale_color_viridis() + labs(title = predictor)
  p4 <- ggplot(data = dat_temp, aes(x = deaths_wave2, y = fitted_n2b, col = pred_temp)) +
    geom_point() + theme_classic() + scale_color_viridis() + labs(title = predictor)
  
  grid.arrange(p1, p2, p3, p4, ncol = 2)
}

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
par(mfrow = c(1, 1))
check_dharma(dat_cumulative, n1a_full, depend = 'cases')
check_dharma(dat_cumulative, n1b_full, depend = 'deaths')
check_dharma(dat_cumulative, n2a_full, depend = 'cases')
check_dharma(dat_cumulative, n2b_full, depend = 'deaths')

check_dharma(dat_cumulative, n1a, depend = 'none')
check_dharma(dat_cumulative, n1b, depend = 'none')
check_dharma(dat_cumulative, n2a, depend = 'none')
check_dharma(dat_cumulative, n2b, depend = 'none')

# Check using rootograms:
library(countreg)
root_n1a <- rootogram(n1a_full, style = 'hanging', plot = FALSE)
root_n1b <- rootogram(n1b_full, style = 'hanging', plot = FALSE)
root_n2a <- rootogram(n2a_full, style = 'hanging', plot = FALSE)
root_n2b <- rootogram(n2b_full, style = 'hanging', plot = FALSE)

autoplot(root_n1a)
autoplot(root_n1b)
autoplot(root_n2a)
autoplot(root_n2b)

detach('package:countreg')
detach('package:MASS')

# Calculate dispersion parameters for Pearson residuals:
sum(residuals(n1a_full, type = 'pearson') ** 2) / df.residual(n1a_full)
sum(residuals(n1b_full, type = 'pearson') ** 2) / df.residual(n1b_full)
sum(residuals(n2a_full, type = 'pearson') ** 2) / df.residual(n2a_full)
sum(residuals(n2b_full, type = 'pearson') ** 2) / df.residual(n2b_full)

# Compare model-generated data to observed data:
n1a_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 7.533, mu = predict(n1a_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$cases_wave1) %>%
  bind_cols(pop = dat_cumulative$pop) %>%
  pivot_longer(V1:V5, names_to = 'id') %>%
  mutate(obs = obs / pop * 10000,
         value = value / pop * 10000)
n1b_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 5.089, mu = predict(n1b_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$deaths_wave1) %>%
  bind_cols(pop = dat_cumulative$pop) %>%
  pivot_longer(V1:V5, names_to = 'id') %>%
  mutate(obs = obs / pop * 10000,
         value = value / pop * 10000)
n2a_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 32.791, mu = predict(n2a_full, type = 'response'))) %>%
  as_tibble() %>%
  bind_cols(obs = dat_cumulative$cases_wave2) %>%
  bind_cols(pop = dat_cumulative$pop) %>%
  pivot_longer(V1:V5, names_to = 'id') %>%
  mutate(obs = obs / pop * 10000,
         value = value / pop * 10000)
n2b_comp <- replicate(5, rnbinom(n = nrow(dat_cumulative), size = 16.567, mu = predict(n2b_full, type = 'response'))) %>%
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

# Plot residuals and calculate Moran's I:
dat_cumulative$resid_n1a <- residuals(n1a_full, type = 'deviance')
dat_cumulative$resid_n1b <- residuals(n1b_full, type = 'deviance')
dat_cumulative$resid_n2a <- residuals(n2a_full, type = 'deviance')
dat_cumulative$resid_n2b <- residuals(n2b_full, type = 'deviance')

map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')
map_bl <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_lan.shp')

map_base <- map_base %>%
  left_join(dat_cumulative %>%
              select(lk, resid_n1a, resid_n1b, resid_n2a, resid_n2b),
            by = c('ARS' = 'lk'))

p1a <- ggplot(data = map_base) + geom_sf(aes(fill = resid_n1a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Cases (Wave 1)', fill = 'Deviance Resid.') +
  theme(legend.position = 'bottom')
p2a <- ggplot(data = map_base) + geom_sf(aes(fill = resid_n2a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Cases (Wave 2)', fill = 'Deviance Resid.') +
  theme(legend.position = 'bottom')
p1b <- ggplot(data = map_base) + geom_sf(aes(fill = resid_n1b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Deaths (Wave 1)', fill = 'Deviance Resid.') +
  theme(legend.position = 'bottom')
p2b <- ggplot(data = map_base) + geom_sf(aes(fill = resid_n2b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Deaths (Wave 2)', fill = 'Deviance Resid.') +
  theme(legend.position = 'bottom')
grid.arrange(p1a, p1b, p2a, p2b, ncol = 2)

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)
moran.mc(map_base$resid_n1a, lw, nsim = 999)
moran.mc(map_base$resid_n1b, lw, nsim = 999)
moran.mc(map_base$resid_n2a, lw, nsim = 999)
moran.mc(map_base$resid_n2b, lw, nsim = 999)
# none are significant

# Clean up:
rm(list = ls())

# ---------------------------------------------------------------------------------------------------------------------
