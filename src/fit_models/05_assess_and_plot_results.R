# Setup

# Load libraries:
library(tidyverse)
# library(mgcv)
library(ggeffects)
library(sf)
library(spdep)
library(testthat)
library(gridExtra)
library(viridis)
library(psych)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
keep_map <- TRUE
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Load models

# Full models:
n1a_full <- read_rds('results/fitted_models/FULL_n1a.rds')
n1b_full <- read_rds('results/fitted_models/FULL_n1b.rds')
n2a_full <- read_rds('results/fitted_models/FULL_n2a.rds')
n2b_full <- read_rds('results/fitted_models/FULL_n2b.rds')

# Null models:
n1a <- read_rds('results/fitted_models/null_n1a.rds')
n1b <- read_rds('results/fitted_models/null_n1b.rds')
n2a <- read_rds('results/fitted_models/null_n2a.rds')
n2b <- read_rds('results/fitted_models/null_n2b.rds')

# "Univariate" models:










# ---------------------------------------------------------------------------------------------------------------------

### Plot total cumulative case/death rates by wave, by LK ###

# Join case/death info to map data:
map_pan <- map_base %>%
  left_join(dat_cumulative %>%
              select(lk, cases_wave1_rate, ifr_wave1, cases_wave2_rate, ifr_wave2),
            by = c('ARS' = 'lk'))

# Get map of Bundeslaender:
map_bl <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_lan.shp')

# Plot case/death rates:
p1a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave1_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate))) +
  theme_void() + labs(title = 'Wave 1', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom')
p2a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave2_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate))) +
  theme_void() + labs(title = 'Wave 2', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom')

p1b <- ggplot(data = map_pan) + geom_sf(aes(fill = ifr_wave1), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  scale_fill_viridis(limits = c(0, max(map_pan$ifr_wave1, map_pan$ifr_wave2))) +
  theme_void() + labs(title = 'Wave 1', fill = '% IFR') +
  theme(legend.position = 'bottom')
p2b <- ggplot(data = map_pan) + geom_sf(aes(fill = ifr_wave2), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  scale_fill_viridis(limits = c(0, max(map_pan$ifr_wave1, map_pan$ifr_wave2))) +
  theme_void() + labs(title = 'Wave 2', fill = '% IFR') +
  theme(legend.position = 'bottom')
# 
grid.arrange(p1a, p1b, p2a, p2b, ncol = 2)

# Significant clustering by Moran's I?:
# https://keen-swartz-3146c4.netlify.app/spatautocorr.html
lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)
moran.mc(map_pan$cases_wave1_rate, lw, nsim = 999) # 0.64069
moran.mc(map_pan$cases_wave2_rate, lw, nsim = 999) # 0.69316
moran.mc(map_pan$ifr_wave1, lw, nsim = 999) # 0.11773
moran.mc(map_pan$ifr_wave2, lw, nsim = 999) # 0.18589

# Plot boxplot of case/death rates by wave and BL:

p1a <- ggplot(data = dat_cumulative, aes(x = ags2, y = cases_wave1_rate, group = ags2)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  scale_y_continuous(trans = 'log', breaks = c(5, 10, 25, 50, 100, 150)) +
  labs(x = 'Bundesland', y = 'Cases per 10,000 Pop', title = 'Wave 1')
p2a <- ggplot(data = dat_cumulative, aes(x = ags2, y = cases_wave2_rate, group = ags2)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  scale_y_continuous(trans = 'log', breaks = c(100, 200, 300, 400, 500, 600)) +
  labs(x = 'Bundesland', y = 'Cases per 10,000 Pop', title = 'Wave 2')
grid.arrange(p1a, p2a, ncol = 1)

p1b <- ggplot(data = dat_cumulative, aes(x = ags2, y = ifr_wave1, group = ags2)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  # scale_y_continuous(trans = 'log', breaks = c(5, 10, 15)) +
  labs(x = 'Bundesland', y = 'IFR (%)', title = 'Wave 1')
p2b <- ggplot(data = dat_cumulative, aes(x = ags2, y = ifr_wave2, group = ags2)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  # scale_y_continuous(trans = 'log', breaks = c(2, 4, 6)) +
  labs(x = 'Bundesland', y = 'IFR (%)', title = 'Wave 2')
grid.arrange(p1b, p2b, ncol = 1)

# How consistent are patterns from one wave to the next?:
cor.test(dat_cumulative$cases_wave1_rate, dat_cumulative$cases_wave2_rate, method = 'kendall') # sig; tau = 0.132
cor.test(dat_cumulative$ifr_wave1, dat_cumulative$ifr_wave2, method = 'kendall') # not sig; tau = -0.027

# ---------------------------------------------------------------------------------------------------------------------

### Observed spatial patterns (from model with no predictors) ###
# Note: since controlling for first wave's cases doesn't actually improve fit for models of second wave, just use n2a
# and n2b, not n2a_adj and n2b_adj

# Plot trends by lat and long:
n1a_lat <- ggpredict(n1a, 'lat')
n1a_long <- ggpredict(n1a, 'long')
plot(n1a_lat); plot(n1a_long)

n2a_lat <- ggpredict(n2a, 'lat')
n2a_long <- ggpredict(n2a, 'long')
plot(n2a_lat); plot(n2a_long)

n1b_lat <- ggpredict(n1b, 'lat')
n1b_long <- ggpredict(n1b, 'long')
plot(n1b_lat); plot(n1b_long)

n2b_lat <- ggpredict(n2b, 'lat')
n2b_long <- ggpredict(n2b, 'long')
plot(n2b_lat); plot(n2b_long)

rm(n1a_lat, n1a_long, n2a_lat, n2a_long,
   n1b_lat, n1b_long, n2b_lat, n2b_long)

# Plot overall spatial pattern:
spatial_trend_NULL <- dat_cumulative %>%
  select(lk, long, lat) %>%
  unique() %>%
  mutate(pop = 10000,
         cases_wave1 = 100,
         cases_wave2 = 100,
         ags2 = '01')

spatial_trend_NULL <- spatial_trend_NULL %>%
  mutate(fitted_n1a = predict(n1a, spatial_trend_NULL, type = 'response'),
         fitted_n2a = predict(n2a, spatial_trend_NULL, type = 'response'),
         fitted_n1b = predict(n1b, spatial_trend_NULL, type = 'response'),
         fitted_n2b = predict(n2b, spatial_trend_NULL, type = 'response'))

map_fitted_NULL <- map_pan %>%
  left_join(spatial_trend_NULL %>%
              select(lk, fitted_n1a:fitted_n2b),
            by = c('ARS' = 'lk'))
rm(spatial_trend_NULL)

p1a <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n1a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 1', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom')
p2a <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n2a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 2', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom')

p1b <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n1b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 1', fill = 'IFR (%)') +
  theme(legend.position = 'bottom')
p2b <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n2b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 2', fill = 'IFR (%)') +
  theme(legend.position = 'bottom')

grid.arrange(p1a, p1b, p2a, p2b, ncol = 2)

# Check significance, % deviance explained:
summary(n1a) # 72.9%
summary(n2a) # 73.2%
summary(n1b) # 14.2%; barely sig
summary(n2b) # 28.5 %

# ---------------------------------------------------------------------------------------------------------------------

### Plot relationships between SES variables (and show corr coefficients) ###

dat_ses <- dat_cumulative %>%
  select(perc_18to64, hosp_beds, care_home_beds, pop_dens, living_area,
         GISD_Score, perc_service, perc_production)

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

# ---------------------------------------------------------------------------------------------------------------------

### Show correlations between predictors and cases/deaths (rates) ###
# Note that of course this won't deal well with non-monotonic relationships

# Plot:
dat_corr <- dat_cumulative %>%
  select(cases_wave1_rate, cases_wave2_rate, ifr_wave1, ifr_wave2,
         perc_18to64, hosp_beds, care_home_beds, pop_dens, living_area,
         GISD_Score, perc_service, perc_production) %>%
  pivot_longer(perc_18to64:perc_production, names_to = 'var', values_to = 'val') %>%
  pivot_longer(cases_wave1_rate:ifr_wave2, names_to = 'outcome', values_to = 'obs') %>%
  mutate(var = factor(var, levels = c('perc_18to64', 'hosp_beds', 'care_home_beds',
                                      'pop_dens', 'living_area', 'GISD_Score',
                                      'perc_service', 'perc_production')),
         outcome = factor(outcome, levels = c('cases_wave1_rate', 'cases_wave2_rate',
                                              'ifr_wave1', 'ifr_wave2')))

p_corr <- ggplot(data = dat_corr) + geom_point(aes(x = val, y = obs)) +
  geom_smooth(aes(x = val, y = obs), method = 'gam') +
  facet_grid(outcome ~ var, scales = 'free') + theme_classic() +
  labs(x = 'Covariate Value', y = 'Outcome Value')
print(p_corr)

# No need to calculate correlation coefficients; b/c of spatial autocorrelation, p-values won't be reliable

# ---------------------------------------------------------------------------------------------------------------------
