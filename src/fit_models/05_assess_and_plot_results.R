# Setup

# Load libraries:
library(tidyverse)
# library(mgcv)
# library(ggeffects)
library(sf)
library(spdep)
library(testthat)
library(gridExtra)
library(viridis)

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
