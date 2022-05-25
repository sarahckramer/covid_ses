# ---------------------------------------------------------------------------------------------------------------------
# Assess and plot associations between predictors and outcomes
# Models labeled "a" are models of incidence; models labeled "b" are models of CFR
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(sf)
library(spdep)
library(testthat)
library(ggeffects)
library(gridExtra)
library(viridis)
library(psych)

# Load necessary functions:
source('src/functions/assess_results_fxns.R')

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
keep_map <- TRUE
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Load models

# Full models:
n1a_full <- read_rds('results/fitted_models/FULL_n1a_ml.rds')
n1b_full <- read_rds('results/fitted_models/FULL_n1b_ml.rds')
n2a_full <- read_rds('results/fitted_models/FULL_n2a_ml.rds')
n2b_full <- read_rds('results/fitted_models/FULL_n2b_ml.rds')
n3a_full <- read_rds('results/fitted_models/FULL_n3a_ml.rds')
n3b_full <- read_rds('results/fitted_models/FULL_n3b_ml.rds')
n4a_full <- read_rds('results/fitted_models/FULL_n4a_ml.rds')
n4b_full <- read_rds('results/fitted_models/FULL_n4b_ml.rds')

# Null models:
n1a <- read_rds('results/fitted_models/null_n1a_ml.rds')
n1b <- read_rds('results/fitted_models/null_n1b_ml.rds')
n2a <- read_rds('results/fitted_models/null_n2a_ml.rds')
n2b <- read_rds('results/fitted_models/null_n2b_ml.rds')
n3a <- read_rds('results/fitted_models/null_n3a_ml.rds')
n3b <- read_rds('results/fitted_models/null_n3b_ml.rds')
n4a <- read_rds('results/fitted_models/null_n4a_ml.rds')
n4b <- read_rds('results/fitted_models/null_n4b_ml.rds')

n2a_adj <- read_rds('results/fitted_models/null_n2a_adj_ml.rds')
n2b_adj <- read_rds('results/fitted_models/null_n2b_adj_ml.rds')
n3a_adj <- read_rds('results/fitted_models/null_n3a_adj_ml.rds')
n3b_adj <- read_rds('results/fitted_models/null_n3b_adj_ml.rds')
n4a_adj <- read_rds('results/fitted_models/null_n4a_adj_ml.rds')
n4b_adj <- read_rds('results/fitted_models/null_n4b_adj_ml.rds')

# ---------------------------------------------------------------------------------------------------------------------

### Plot total cumulative case/death rates by wave, by LK ###

# Join case/death info to map data:
map_pan <- map_base %>%
  left_join(dat_cumulative %>%
              select(lk, cases_wave1_rate, cfr_wave1, cases_wave2_rate, cfr_wave2,
                     cases_wave3_rate, cfr_wave3, cases_wave4_rate, cfr_wave4),
            by = c('ARS' = 'lk'))

# Get map of Bundeslaender:
map_bl <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_lan.shp')

# Plot case/death rates:
p1a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave1_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate, map_pan$cases_wave3_rate, map_pan$cases_wave4_rate, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate, map_pan$cases_wave3_rate, map_pan$cases_wave4_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 1', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave2_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate, map_pan$cases_wave3_rate, map_pan$cases_wave4_rate, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate, map_pan$cases_wave3_rate, map_pan$cases_wave4_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 2', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p3a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave3_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate, map_pan$cases_wave3_rate, map_pan$cases_wave4_rate, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate, map_pan$cases_wave3_rate, map_pan$cases_wave4_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 3', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p4a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave4_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate, map_pan$cases_wave3_rate, map_pan$cases_wave4_rate, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate, map_pan$cases_wave3_rate, map_pan$cases_wave4_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 4', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

p1b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave1), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 2, 4, 8, 16)) +
  theme_void() + labs(title = 'Wave 1', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave2), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 2, 4, 8, 16)) +
  theme_void() + labs(title = 'Wave 2', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p3b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave3), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 2, 4, 8, 16)) +
  theme_void() + labs(title = 'Wave 3', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p4b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave4), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 2, 4, 8, 16)) +
  theme_void() + labs(title = 'Wave 4', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

grid.arrange(p1a, p2a, p3a, p4a, p1b, p2b, p3b, p4b, ncol = 4)

# Significant clustering by Moran's I?:
# https://keen-swartz-3146c4.netlify.app/spatautocorr.html
map_pan <- map_pan %>%
  drop_na()
map_temp <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp') %>%
  filter(GEN != 'Eisenach', GEN != 'Wartburgkreis')

nb <- spdep::poly2nb(map_temp, row.names = map_temp$ARS)
attr(nb, 'region.id') <- map_temp$ARS
names(nb) <- attr(nb, 'region.id')

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)
moran.mc(map_pan$cases_wave1_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave2_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave3_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave4_rate, lw, nsim = 999)
moran.mc(map_pan$cfr_wave1, lw, nsim = 999)
moran.mc(map_pan$cfr_wave2, lw, nsim = 999)
moran.mc(map_pan$cfr_wave3, lw, nsim = 999)
moran.mc(map_pan$cfr_wave4, lw, nsim = 999)

# Plot boxplot of case/death rates by wave and BL:
p1a <- ggplot(data = dat_cumulative, aes(x = ags2, y = cases_wave1_rate, group = ags2)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  # theme(plot.title = element_text(size = 20), axis.title = element_text(size = 16),
  #       axis.text = element_text(size = 12)) +
  scale_x_discrete(labels = c('SH', 'HH', 'NI', 'HB', 'NW', 'HE', 'RP', 'BW', 'BY', 'SL',
                              'BE', 'BB', 'MV', 'SN', 'ST', 'TH')) +
  scale_y_continuous(trans = 'log', breaks = c(5, 10, 25, 50, 100, 150)) +
  labs(x = 'Bundesland', y = 'Cases / 10000 Pop', title = 'Wave 1')
p2a <- ggplot(data = dat_cumulative, aes(x = ags2, y = cases_wave2_rate, group = ags2)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  # theme(plot.title = element_text(size = 20), axis.title = element_text(size = 16),
  #       axis.text = element_text(size = 12)) +
  scale_x_discrete(labels = c('SH', 'HH', 'NI', 'HB', 'NW', 'HE', 'RP', 'BW', 'BY', 'SL',
                              'BE', 'BB', 'MV', 'SN', 'ST', 'TH')) +
  scale_y_continuous(trans = 'log', breaks = c(100, 200, 300, 400, 500, 600)) +
  labs(x = 'Bundesland', y = 'Cases / 10000 Pop', title = 'Wave 2')
p3a <- ggplot(data = dat_cumulative, aes(x = ags2, y = cases_wave3_rate, group = ags2)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  scale_x_discrete(labels = c('SH', 'HH', 'NI', 'HB', 'NW', 'HE', 'RP', 'BW', 'BY', 'SL',
                              'BE', 'BB', 'MV', 'SN', 'ST', 'TH')) +
  scale_y_continuous(trans = 'log', breaks = c(50, 100, 200, 300, 400, 500)) +
  labs(x = 'Bundesland', y = 'Cases / 10000 Pop', title = 'Wave 3')
p4a <- ggplot(data = dat_cumulative, aes(x = ags2, y = cases_wave4_rate, group = ags2)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  scale_x_discrete(labels = c('SH', 'HH', 'NI', 'HB', 'NW', 'HE', 'RP', 'BW', 'BY', 'SL',
                              'BE', 'BB', 'MV', 'SN', 'ST', 'TH')) +
  scale_y_continuous(trans = 'log', breaks = c(100, 200, 300, 400, 500, 750, 1000, 1250)) +
  labs(x = 'Bundesland', y = 'Cases / 10000 Pop', title = 'Wave 4')
grid.arrange(p1a, p2a, p3a, p4a, ncol = 1)

p1b <- ggplot(data = dat_cumulative, aes(x = ags2, y = cfr_wave1, group = ags2)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  scale_x_discrete(labels = c('SH', 'HH', 'NI', 'HB', 'NW', 'HE', 'RP', 'BW', 'BY', 'SL',
                              'BE', 'BB', 'MV', 'SN', 'ST', 'TH')) +
  # scale_y_continuous(trans = 'log', breaks = c(5, 10, 15)) +
  labs(x = 'Bundesland', y = 'CFR (%)', title = 'Wave 1')
p2b <- ggplot(data = dat_cumulative, aes(x = ags2, y = cfr_wave2, group = ags2)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  scale_x_discrete(labels = c('SH', 'HH', 'NI', 'HB', 'NW', 'HE', 'RP', 'BW', 'BY', 'SL',
                              'BE', 'BB', 'MV', 'SN', 'ST', 'TH')) +
  # scale_y_continuous(trans = 'log', breaks = c(2, 4, 6)) +
  labs(x = 'Bundesland', y = 'CFR (%)', title = 'Wave 2')
p3b <- ggplot(data = dat_cumulative, aes(x = ags2, y = cfr_wave3, group = ags2)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  scale_x_discrete(labels = c('SH', 'HH', 'NI', 'HB', 'NW', 'HE', 'RP', 'BW', 'BY', 'SL',
                              'BE', 'BB', 'MV', 'SN', 'ST', 'TH')) +
  labs(x = 'Bundesland', y = 'CFR (%)', title = 'Wave 3')
p4b <- ggplot(data = dat_cumulative, aes(x = ags2, y = cfr_wave4, group = ags2)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  scale_x_discrete(labels = c('SH', 'HH', 'NI', 'HB', 'NW', 'HE', 'RP', 'BW', 'BY', 'SL',
                              'BE', 'BB', 'MV', 'SN', 'ST', 'TH')) +
  labs(x = 'Bundesland', y = 'CFR (%)', title = 'Wave 4')
grid.arrange(p1b, p2b, p3b, p4b, ncol = 1)

# How consistent are patterns from one wave to the next?:
pairs.panels(dat_cumulative %>%
               select(cases_wave1_rate, cases_wave2_rate, cases_wave3_rate, cases_wave4_rate),
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
pairs.panels(dat_cumulative %>%
               select(cfr_wave1, cfr_wave2, cfr_wave3, cfr_wave4),
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

# ---------------------------------------------------------------------------------------------------------------------

### Observed spatial patterns (from model with no predictors) ###

# Check whether controlling for first wave's incidence (and vaccination) improves model fit:
anova(n2a, n2a_adj, test = 'Chisq')
anova(n2b, n2b_adj, test = 'Chisq')
anova(n3a, n3a_adj, test = 'Chisq')
anova(n3b, n3b_adj, test = 'Chisq')
anova(n4a, n4a_adj, test = 'Chisq')
anova(n4b, n4b_adj, test = 'Chisq')

# Plot overall spatial pattern:
spatial_trend_NULL <- dat_cumulative %>%
  select(lk, long, lat) %>%
  unique() %>%
  mutate(pop = 10000,
         cases_wave1 = 100,
         cases_wave2 = 100,
         cases_wave3 = 100,
         cases_wave4 = 100,
         cases_wave1_rate = mean(dat_cumulative$cases_wave1_rate),
         cases_wave2_rate = mean(dat_cumulative$cases_wave2_rate),
         cases_wave3_rate = mean(dat_cumulative$cases_wave3_rate),
         cases_wave4_rate = mean(dat_cumulative$cases_wave4_rate),
         cases_pre2_rate = mean(dat_cumulative$cases_pre2_rate),
         cases_pre3_rate = mean(dat_cumulative$cases_pre3_rate),
         cases_pre4_rate = mean(dat_cumulative$cases_pre4_rate),
         vacc_w3_reg = mean(dat_cumulative$vacc_w3_reg),
         vacc_w4_reg = mean(dat_cumulative$vacc_w4_reg),
         ags2 = '01')

spatial_trend_NULL <- spatial_trend_NULL %>%
  mutate(fitted_n1a = predict(n1a, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n2a = predict(n2a, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n3a = predict(n3a, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n4a = predict(n4a, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n1b = predict(n1b, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n2b = predict(n2b, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n3b = predict(n3b, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n4b = predict(n4b, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n2a_adj = predict(n2a_adj, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n2b_adj = predict(n2b_adj, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n3a_adj = predict(n3a_adj, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n3b_adj = predict(n3b_adj, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n4a_adj = predict(n4a_adj, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n4b_adj = predict(n4b_adj, spatial_trend_NULL, type = 'response'))#, exclude = 's(ags2)'))

map_fitted_NULL <- map_pan %>%
  left_join(spatial_trend_NULL %>%
              select(lk, fitted_n1a:fitted_n4b_adj),
            by = c('ARS' = 'lk'))
rm(spatial_trend_NULL)

p1a <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n1a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 1', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2a <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n2a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(breaks = c(250, 500)) +
  theme_void() + labs(title = 'Wave 2', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p3a <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n3a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(breaks = c(150, 250)) +
  theme_void() + labs(title = 'Wave 3', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p4a <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n4a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(breaks = c(250, 500, 800)) +
  theme_void() + labs(title = 'Wave 4', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

p1b <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n1b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 1', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2b <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n2b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 2', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p3b <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n3b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 3', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p4b <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n4b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 4', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

grid.arrange(p1a, p2a, p3a, p4a, p1b, p2b, p3b, p4b, ncol = 4)

# Check % deviance explained:
summary(n1a)
summary(n2a_adj)
summary(n3a_adj)
summary(n4a_adj)
summary(n1b)
summary(n2b_adj)
summary(n3b_adj)
summary(n4b_adj)

# ---------------------------------------------------------------------------------------------------------------------

### Plot relationships between SES variables (and show corr coefficients) ###

dat_ses <- dat_cumulative %>%
  select(perc_18to64, perc_lessthan18, hosp_beds, care_home_beds, pop_dens, #living_area,
         GISD_Score, perc_service, perc_production, vacc_w3_reg, vacc_w4_reg)

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

# Plot:
dat_corr <- dat_cumulative %>%
  select(cases_wave1_rate, cases_wave2_rate, cases_wave3_rate, cases_wave4_rate,
         cfr_wave1, cfr_wave2, cfr_wave3, cfr_wave4,
         perc_18to64, perc_lessthan18, hosp_beds, care_home_beds, pop_dens, #living_area,
         GISD_Score, perc_service, perc_production) %>%
  pivot_longer(perc_18to64:perc_production, names_to = 'var', values_to = 'val') %>%
  pivot_longer(cases_wave1_rate:cfr_wave4, names_to = 'outcome', values_to = 'obs') %>%
  mutate(var = factor(var, levels = c('perc_18to64', 'perc_lessthan18', 'hosp_beds', 'care_home_beds',
                                      'pop_dens', 'GISD_Score', 'perc_service',
                                      'perc_production')),
         outcome = factor(outcome, levels = c('cases_wave1_rate', 'cases_wave2_rate',
                                              'cases_wave3_rate', 'cases_wave4_rate',
                                              'cfr_wave1', 'cfr_wave2', 'cfr_wave3', 'cfr_wave4')))

p_corr <- ggplot(data = dat_corr) + geom_point(aes(x = val, y = obs)) +
  geom_smooth(aes(x = val, y = obs), method = 'gam') +
  facet_grid(outcome ~ var, scales = 'free') + theme_classic() +
  labs(x = 'Covariate Value', y = 'Outcome Value')
print(p_corr)

# ---------------------------------------------------------------------------------------------------------------------

### Full models ###

# Determine percent deviance explained:
summary(n1a_full)
summary(n1b_full)
summary(n2a_full)
summary(n2b_full)
summary(n3a_full)
summary(n3b_full)
summary(n4a_full)
summary(n4b_full)

# Compare deviance explained/model fit to null models:
anova(n1a, n1a_full, test = 'Chisq')
anova(n2a_adj, n2a_full, test = 'Chisq')
anova(n3a_adj, n3a_full, test = 'Chisq')
anova(n4a_adj, n4a_full, test = 'Chisq')

anova(n1b, n1b_full, test = 'Chisq')
anova(n2b_adj, n2b_full, test = 'Chisq')
anova(n3b_adj, n3b_full, test = 'Chisq')
anova(n4b_adj, n4b_full, test = 'Chisq')

# Plot smooth relationships between all predictors and outcomes (for incidence):
mod_list <- list(n1a_full, n2a_full, n3a_full, n4a_full)
names(mod_list) <- c('1', '2', '3', '4')

pred_perc_18to64 <- get_marginal_prediction(dat_cumulative, 'perc_18to64', 'incidence', mod_list, standardize = TRUE)
pred_perc_lessthan18 <- get_marginal_prediction(dat_cumulative, 'perc_lessthan18', 'incidence', mod_list, standardize = TRUE)
pred_care_home_beds <- get_marginal_prediction(dat_cumulative, 'care_home_beds', 'incidence', mod_list, standardize = TRUE)
pred_GISD_Score <- get_marginal_prediction(dat_cumulative, 'GISD_Score', 'incidence', mod_list, standardize = TRUE)
pred_pop_dens <- get_marginal_prediction(dat_cumulative, 'pop_dens', 'incidence', mod_list, standardize = TRUE)
pred_perc_service <- get_marginal_prediction(dat_cumulative, 'perc_service', 'incidence', mod_list, standardize = TRUE)
pred_perc_production <- get_marginal_prediction(dat_cumulative, 'perc_production', 'incidence', mod_list, standardize = TRUE)

plot_a_18to64 <- plot_marginal_prediction(pred_perc_18to64, 'perc_18to64', 'Cases / 10000 Pop', single_plot = TRUE)
plot_a_lessthan18 <- plot_marginal_prediction(pred_perc_lessthan18, 'perc_lessthan18', 'Cases / 10000 Pop', single_plot = TRUE)
plot_a_care_home_beds <- plot_marginal_prediction(pred_care_home_beds, 'care_home_beds', 'Cases / 10000 Pop', single_plot = TRUE)
plot_a_GISD_Score <- plot_marginal_prediction(pred_GISD_Score, 'GISD_Score', 'Cases / 10000 Pop', single_plot = TRUE)
plot_a_pop_dens <- plot_marginal_prediction(pred_pop_dens, 'pop_dens', 'Cases / 10000 Pop', single_plot = TRUE)
plot_a_service <- plot_marginal_prediction(pred_perc_service, 'perc_service', 'Cases / 10000 Pop', single_plot = TRUE)
plot_a_production <- plot_marginal_prediction(pred_perc_production, 'perc_production', 'Cases / 10000 Pop', single_plot = TRUE)

grid.arrange(plot_a_GISD_Score, plot_a_service, plot_a_production,
             plot_a_18to64, plot_a_lessthan18, plot_a_pop_dens, plot_a_care_home_beds, nrow = 2)

pred_18to64_popdens <- get_marginal_prediction(dat_cumulative, c('perc_18to64', 'pop_dens'), 'incidence', mod_list, standardize = TRUE)
pred_lessthan18_popdens <- get_marginal_prediction(dat_cumulative, c('perc_lessthan18', 'pop_dens'), 'incidence', mod_list, standardize = TRUE)
pred_18to64_GISD <- get_marginal_prediction(dat_cumulative, c('perc_18to64', 'GISD_Score'), 'incidence', mod_list, standardize = TRUE)
pred_lessthan18_GISD <- get_marginal_prediction(dat_cumulative, c('perc_lessthan18', 'GISD_Score'), 'incidence', mod_list, standardize = TRUE)

plot_18to64_popdens <- plot_marginal_prediction(pred_18to64_popdens, c('perc_18to64', 'pop_dens'), 'Cases / 10000 Pop', single_plot = FALSE, which_waves = 2)
plot_lessthan18_popdens <- plot_marginal_prediction(pred_lessthan18_popdens, c('perc_lessthan18', 'pop_dens'), 'Cases / 10000 Pop', single_plot = FALSE, which_waves = 2)
plot_18to64_GISD <- plot_marginal_prediction(pred_18to64_GISD, c('perc_18to64', 'GISD_Score'), 'Cases / 10000 Pop', single_plot = FALSE, which_waves = 3)
plot_lessthan18_GISD <- plot_marginal_prediction(pred_lessthan18_GISD, c('perc_lessthan18', 'GISD_Score'), 'Cases / 10000 Pop', single_plot = FALSE, which_waves = c(3, 4))

do.call('grid.arrange', c(plot_18to64_popdens, plot_lessthan18_popdens,
                          plot_18to64_GISD, plot_lessthan18_GISD[[1]],
                          plot_lessthan18_GISD[[2]], ncol = 4))

mod_list <- list(n2a_full, n3a_full, n4a_full)
names(mod_list) <- c('2', '3', '4')
pred_cases_pre <- get_marginal_prediction(dat_cumulative, 'cases_pre', 'incidence', mod_list, standardize = TRUE)

mod_list <- list(n3a_full, n4a_full)
names(mod_list) <- c('3', '4')
pred_vacc <- get_marginal_prediction(dat_cumulative, 'vacc_reg', 'incidence', mod_list, standardize = TRUE)

plot_a_cases_pre <- plot_marginal_prediction(pred_cases_pre, 'cases_pre', 'Cases / 10000 Pop', single_plot = TRUE)
plot_a_vacc <- plot_marginal_prediction(pred_vacc, 'vacc', 'Cases / 10000 Pop', single_plot = FALSE)

print(plot_a_cases_pre)
print(plot_a_vacc)

# Plot smooth relationships between all predictors and outcomes (for CFR):
mod_list <- list(n1b_full, n2b_full, n3b_full, n4b_full)
names(mod_list) <- c('1', '2', '3', '4')

pred_GISD_Score <- get_marginal_prediction(dat_cumulative, 'GISD_Score', 'cfr', mod_list, standardize = TRUE)
pred_hosp_beds <- get_marginal_prediction(dat_cumulative, 'hosp_beds', 'cfr', mod_list, standardize = TRUE)
pred_care_home_beds <- get_marginal_prediction(dat_cumulative, 'care_home_beds', 'cfr', mod_list, standardize = TRUE)

plot_b_GISD_Score <- plot_marginal_prediction(pred_GISD_Score, 'GISD_Score', 'CFR', single_plot = TRUE)
plot_b_hosp_beds <- plot_marginal_prediction(pred_hosp_beds, 'hosp_beds', 'CFR', single_plot = TRUE)
plot_b_care_home_beds <- plot_marginal_prediction(pred_care_home_beds, 'care_home_beds', 'CFR', single_plot = TRUE)

grid.arrange(plot_b_GISD_Score, plot_b_hosp_beds, plot_b_care_home_beds, nrow = 1)

# plot_b_GISD_Score <- plot_marginal_prediction(pred_GISD_Score, 'GISD_Score', 'CFR', single_plot = FALSE)
# plot_b_hosp_beds <- plot_marginal_prediction(pred_hosp_beds, 'hosp_beds', 'CFR', single_plot = FALSE)
# plot_b_care_home_beds <- plot_marginal_prediction(pred_care_home_beds, 'care_home_beds', 'CFR', single_plot = FALSE)
# 
# grid.arrange(plot_b_hosp_beds, plot_b_care_home_beds, plot_b_GISD_Score, ncol = 1)

pred_cases_rate <- get_marginal_prediction(dat_cumulative, 'cases_rate', 'cfr', mod_list, standardize = TRUE)

mod_list <- list(n2b_full, n3b_full, n4b_full)
names(mod_list) <- c('2', '3', '4')
pred_cases_pre <- get_marginal_prediction(dat_cumulative, 'cases_pre', 'cfr', mod_list, standardize = TRUE)

mod_list <- list(n3b_full, n4b_full)
names(mod_list) <- c('3', '4')
pred_vacc <- get_marginal_prediction(dat_cumulative, 'vacc_reg', 'cfr', mod_list, standardize = TRUE)

plot_b_cases_rate <- plot_marginal_prediction(pred_cases_rate, 'cases_rate', 'CFR', single_plot = FALSE)
plot_b_cases_pre <- plot_marginal_prediction(pred_cases_pre, 'cases_pre', 'CFR', single_plot = TRUE)
plot_b_vacc <- plot_marginal_prediction(pred_vacc, 'vacc', 'CFR', single_plot = FALSE)

print(plot_b_cases_rate)
print(plot_b_cases_pre)
print(plot_b_vacc)

# Plot spatial effect (after controlling for variables):
spatial_trend_FULL <- dat_cumulative %>%
  select(lk, long, lat) %>%
  unique() %>%
  mutate(pop = 10000,
         cases_wave1 = 100,
         cases_wave2 = 100,
         cases_wave3 = 100,
         cases_wave4 = 100,
         cases_pre2_rate = mean(dat_cumulative$cases_pre2_rate),
         cases_pre3_rate = mean(dat_cumulative$cases_pre3_rate),
         cases_pre4_rate = mean(dat_cumulative$cases_pre4_rate),
         cases_wave1_rate = mean(dat_cumulative$cases_wave1_rate),
         cases_wave2_rate = mean(dat_cumulative$cases_wave2_rate),
         cases_wave3_rate = mean(dat_cumulative$cases_wave3_rate),
         cases_wave4_rate = mean(dat_cumulative$cases_wave4_rate),
         ags2 = '01',
         cases_pre2_rate = mean(dat_cumulative$cases_pre2_rate),
         cases_pre3_rate = mean(dat_cumulative$cases_pre3_rate),
         cases_pre4_rate = mean(dat_cumulative$cases_pre4_rate),
         perc_18to64 = mean(dat_cumulative$perc_18to64),
         perc_lessthan18 = mean(dat_cumulative$perc_lessthan18),
         hosp_beds = mean(dat_cumulative$hosp_beds),
         care_home_beds = mean(dat_cumulative$care_home_beds),
         GISD_Score = mean(dat_cumulative$GISD_Score),
         pop_dens = mean(dat_cumulative$pop_dens),
         living_area = mean(dat_cumulative$living_area),
         perc_service = mean(dat_cumulative$perc_service),
         perc_production = mean(dat_cumulative$perc_production),
         vacc_w3_reg = mean(dat_cumulative$vacc_w3_reg),
         vacc_w4_reg = mean(dat_cumulative$vacc_w4_reg))

spatial_trend_FULL <- spatial_trend_FULL %>%
  mutate(fitted_n1a = predict(n1a_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n2a = predict(n2a_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n3a = predict(n3a_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n4a = predict(n4a_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n1b = predict(n1b_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n2b = predict(n2b_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n3b = predict(n3b_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n4b = predict(n4b_full, spatial_trend_FULL, type = 'response'))#, exclude = 's(ags2)'))

map_fitted_FULL <- map_pan %>%
  left_join(spatial_trend_FULL %>%
              select(lk, fitted_n1a:fitted_n4b),
            by = c('ARS' = 'lk'))
rm(spatial_trend_FULL)

p1a <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n1a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 1', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2a <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n2a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(breaks = c(100, 300, 500)) +
  theme_void() + labs(title = 'Wave 2', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p3a <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n3a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 3', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p4a <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n4a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(breaks = c(100, 300, 500)) +
  theme_void() + labs(title = 'Wave 4', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

p1b <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n1b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(breaks = c(5.590035, 5.59008)) +
  theme_void() + labs(title = 'Wave 1', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2b <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n2b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 2', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p3b <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n3b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 3', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p4b <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n4b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(breaks = c(0.5, 0.75)) +
  theme_void() + labs(title = 'Wave 4', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

grid.arrange(p1a, p2a, p3a, p4a, p1b, p2b, p3b, p4b, ncol = 4)

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
