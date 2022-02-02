# ---------------------------------------------------------------------------------------------------------------------
# Assess and plot associations between predictors and outcomes
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
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

# Null models:
n1a <- read_rds('results/fitted_models/null_n1a_ml.rds')
n1b <- read_rds('results/fitted_models/null_n1b_ml.rds')
n2a <- read_rds('results/fitted_models/null_n2a_ml.rds')
n2b <- read_rds('results/fitted_models/null_n2b_ml.rds')

n2a_adj <- read_rds('results/fitted_models/null_n2a_adj_ml.rds')
n2b_adj <- read_rds('results/fitted_models/null_n2b_adj_ml.rds')

# "Univariate" models:
n1a_perc_18to64 <- read_rds('results/fitted_models/uni/n1a_perc_18to64_ml.rds')
n1a_care_home_beds <- read_rds('results/fitted_models/uni/n1a_care_home_beds_ml.rds')
n1a_GISD_Score <- read_rds('results/fitted_models/uni/n1a_GISD_ml.rds')
n1a_pop_dens <- read_rds('results/fitted_models/uni/n1a_pop_dens_ml.rds')
# n1a_living_area <- read_rds('results/fitted_models/uni/n1a_living_area_ml.rds')
n1a_perc_service <- read_rds('results/fitted_models/uni/n1a_perc_serv_ml.rds')
n1a_perc_production <- read_rds('results/fitted_models/uni/n1a_perc_prod_ml.rds')

n1a_uni_list <- list(n1a_perc_18to64, n1a_care_home_beds, n1a_GISD_Score, n1a_pop_dens,
                     n1a_perc_service, n1a_perc_production)
names(n1a_uni_list) <- c('perc_18to64', 'care_home_beds', 'GISD_Score', 'pop_dens',
                         'perc_service', 'perc_production')

n2a_perc_18to64 <- read_rds('results/fitted_models/uni/n2a_perc_18to64_ml.rds')
n2a_care_home_beds <- read_rds('results/fitted_models/uni/n2a_care_home_beds_ml.rds')
n2a_GISD_Score <- read_rds('results/fitted_models/uni/n2a_GISD_ml.rds')
n2a_pop_dens <- read_rds('results/fitted_models/uni/n2a_pop_dens_ml.rds')
# n2a_living_area <- read_rds('results/fitted_models/uni/n2a_living_area_ml.rds')
n2a_perc_service <- read_rds('results/fitted_models/uni/n2a_perc_serv_ml.rds')
n2a_perc_production <- read_rds('results/fitted_models/uni/n2a_perc_prod_ml.rds')

n2a_uni_list <- list(n2a_perc_18to64, n2a_care_home_beds, n2a_GISD_Score, n2a_pop_dens,
                     n2a_perc_service, n2a_perc_production)
names(n2a_uni_list) <- c('perc_18to64', 'care_home_beds', 'GISD_Score', 'pop_dens',
                         'perc_service', 'perc_production')

n1b_hosp_beds <- read_rds('results/fitted_models/uni/n1b_hosp_beds_ml.rds')
n1b_care_home_beds <- read_rds('results/fitted_models/uni/n1b_care_home_beds_ml.rds')
n1b_GISD_Score <- read_rds('results/fitted_models/uni/n1b_GISD_ml.rds')

n1b_uni_list <- list(n1b_hosp_beds, n1b_care_home_beds, n1b_GISD_Score)
names(n1b_uni_list) <- c('hosp_beds', 'care_home_beds', 'GISD_Score')

n2b_hosp_beds <- read_rds('results/fitted_models/uni/n2b_hosp_beds_ml.rds')
n2b_care_home_beds <- read_rds('results/fitted_models/uni/n2b_care_home_beds_ml.rds')
n2b_GISD_Score <- read_rds('results/fitted_models/uni/n2b_GISD_ml.rds')

n2b_uni_list <- list(n2b_hosp_beds, n2b_care_home_beds, n2b_GISD_Score)
names(n2b_uni_list) <- c('hosp_beds', 'care_home_beds', 'GISD_Score')

rm(n1a_perc_18to64, n1a_care_home_beds, n1a_GISD_Score, n1a_pop_dens, n1a_perc_service, n1a_perc_production,
   n2a_perc_18to64, n2a_care_home_beds, n2a_GISD_Score, n2a_pop_dens, n2a_perc_service, n2a_perc_production,
   n1b_hosp_beds, n1b_care_home_beds, n1b_GISD_Score, n2b_hosp_beds, n2b_care_home_beds, n2b_GISD_Score)

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
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave2_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate))) +
  theme_void() + labs(title = 'Wave 2', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

p1b <- ggplot(data = map_pan) + geom_sf(aes(fill = ifr_wave1), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  scale_fill_viridis(limits = c(0, max(map_pan$ifr_wave1, map_pan$ifr_wave2))) +
  theme_void() + labs(title = 'Wave 1', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2b <- ggplot(data = map_pan) + geom_sf(aes(fill = ifr_wave2), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  scale_fill_viridis(limits = c(0, max(map_pan$ifr_wave1, map_pan$ifr_wave2))) +
  theme_void() + labs(title = 'Wave 2', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

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
grid.arrange(p1a, p2a, ncol = 1)

p1b <- ggplot(data = dat_cumulative, aes(x = ags2, y = ifr_wave1, group = ags2)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  scale_x_discrete(labels = c('SH', 'HH', 'NI', 'HB', 'NW', 'HE', 'RP', 'BW', 'BY', 'SL',
                              'BE', 'BB', 'MV', 'SN', 'ST', 'TH')) +
  # scale_y_continuous(trans = 'log', breaks = c(5, 10, 15)) +
  labs(x = 'Bundesland', y = 'CFR (%)', title = 'Wave 1')
p2b <- ggplot(data = dat_cumulative, aes(x = ags2, y = ifr_wave2, group = ags2)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  scale_x_discrete(labels = c('SH', 'HH', 'NI', 'HB', 'NW', 'HE', 'RP', 'BW', 'BY', 'SL',
                              'BE', 'BB', 'MV', 'SN', 'ST', 'TH')) +
  # scale_y_continuous(trans = 'log', breaks = c(2, 4, 6)) +
  labs(x = 'Bundesland', y = 'CFR (%)', title = 'Wave 2')
grid.arrange(p1b, p2b, ncol = 1)

# How consistent are patterns from one wave to the next?:
cor.test(dat_cumulative$cases_wave1_rate, dat_cumulative$cases_wave2_rate, method = 'kendall') # sig; tau = 0.132
cor.test(dat_cumulative$ifr_wave1, dat_cumulative$ifr_wave2, method = 'kendall') # not sig; tau = -0.027

# ---------------------------------------------------------------------------------------------------------------------

### Observed spatial patterns (from model with no predictors) ###

# Check whether controlling for first wave's incidence improves model fit:
anova(n2a, n2a_adj, test = 'Chisq')
anova(n2b, n2b_adj, test = 'Chisq')
# No; just use n2a and n2b, not n2a_adj and n2b_adj

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
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2a <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n2a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(breaks = c(250, 500)) +
  theme_void() + labs(title = 'Wave 2', fill = 'Cases / 10000 Pop') +
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

grid.arrange(p1a, p1b, p2a, p2b, ncol = 2)

# Check significance, % deviance explained:
summary(n1a) # 73.5%
summary(n2a) # 73.1%
summary(n1b) # 15.5%
summary(n2b) # 28.6% 

# ---------------------------------------------------------------------------------------------------------------------

### Plot relationships between SES variables (and show corr coefficients) ###

dat_ses <- dat_cumulative %>%
  select(perc_18to64, hosp_beds, care_home_beds, pop_dens, #living_area,
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
         perc_18to64, hosp_beds, care_home_beds, pop_dens, #living_area,
         GISD_Score, perc_service, perc_production) %>%
  pivot_longer(perc_18to64:perc_production, names_to = 'var', values_to = 'val') %>%
  pivot_longer(cases_wave1_rate:ifr_wave2, names_to = 'outcome', values_to = 'obs') %>%
  mutate(var = factor(var, levels = c('perc_18to64', 'hosp_beds', 'care_home_beds',
                                      'pop_dens', 'GISD_Score', 'perc_service',
                                      'perc_production')),
         outcome = factor(outcome, levels = c('cases_wave1_rate', 'cases_wave2_rate',
                                              'ifr_wave1', 'ifr_wave2')))

p_corr <- ggplot(data = dat_corr) + geom_point(aes(x = val, y = obs)) +
  geom_smooth(aes(x = val, y = obs), method = 'gam') +
  facet_grid(outcome ~ var, scales = 'free') + theme_classic() +
  labs(x = 'Covariate Value', y = 'Outcome Value')
print(p_corr)
# No need to calculate correlation coefficients; b/c of spatial autocorrelation, p-values won't be reliable

# ---------------------------------------------------------------------------------------------------------------------

### 'Univariate' models ###

# Determine significant predictors:
for (mod in n1a_uni_list) {
  print(summary(mod))
} # GISD_Score (p = 0.000281); perc_18to64 almost (p = 0.0812), perc_production almost (p = 0.09205)

for (mod in n2a_uni_list) {
  print(summary(mod))
} # perc_18to64 (p = 0.0443), care_home_beds (p = 0.0215), GISD_Score (p < 2e-16), perc_service (p = 0.0213), perc_production (p = 0.0235)

for (mod in n1b_uni_list) {
  print(summary(mod))
} # none

for (mod in n2b_uni_list) {
  print(summary(mod))
} # care_home_beds (p = 0.000732); GISD_Score (p = 0.0406)

# Plot relationships:
n1a_pred_GISD <- get_marginal_prediction(dat_cumulative, 'cases_wave1_rate', 'GISD_Score', n1a_uni_list[[3]]) # 1.783x
p_n1a_GISD <- plot_marginal_prediction(n1a_pred_GISD, 'GISD_Score', 'Cases / 10000 Pop')

plot(p_n1a_GISD) # sig

n1b_pred_hosp_beds <- get_marginal_prediction(dat_cumulative, 'ifr_wave1', 'hosp_beds', n1b_uni_list[[1]]) # 1.280x
p_n1b_hosp_beds <- plot_marginal_prediction(n1b_pred_hosp_beds, 'hosp_beds', 'CFR')
n1b_pred_care_home_beds <- get_marginal_prediction(dat_cumulative, 'ifr_wave1', 'care_home_beds', n1b_uni_list[[2]]) # 1.188x
p_n1b_care_home_beds <- plot_marginal_prediction(n1b_pred_care_home_beds, 'care_home_beds', 'CFR')

grid.arrange(p_n1b_hosp_beds, p_n1b_care_home_beds, ncol = 2) # not sig

n2a_pred_perc_18to64 <- get_marginal_prediction(dat_cumulative, 'cases_wave2_rate', 'perc_18to64', n2a_uni_list[[1]]) # 1.269x
p_n2a_perc_18to64 <- plot_marginal_prediction(n2a_pred_perc_18to64, 'perc_18to64', 'Cases / 10000 Pop')
n2a_pred_care_home_beds <- get_marginal_prediction(dat_cumulative, 'cases_wave2_rate', 'care_home_beds', n2a_uni_list[[2]]) # 1.417x
p_n2a_care_home_beds <- plot_marginal_prediction(n2a_pred_care_home_beds, 'care_home_beds', 'Cases / 10000 Pop')
n2a_pred_GISD_Score <- get_marginal_prediction(dat_cumulative, 'cases_wave2_rate', 'GISD_Score', n2a_uni_list[[3]]) # 1.890x
p_n2a_GISD_Score <- plot_marginal_prediction(n2a_pred_GISD_Score, 'GISD_Score', 'Cases / 10000 Pop')
n2a_pred_perc_service <- get_marginal_prediction(dat_cumulative, 'cases_wave2_rate', 'perc_service', n2a_uni_list[[5]]) # 1.230x
p_n2a_perc_service <- plot_marginal_prediction(n2a_pred_perc_service, 'perc_service', 'Cases / 10000 Pop')
n2a_pred_perc_production <- get_marginal_prediction(dat_cumulative, 'cases_wave2_rate', 'perc_production', n2a_uni_list[[6]]) # 1.179x
p_n2a_perc_production <- plot_marginal_prediction(n2a_pred_perc_production, 'perc_production', 'Cases / 10000 Pop')

grid.arrange(p_n2a_perc_18to64, p_n2a_care_home_beds, p_n2a_GISD_Score, p_n2a_perc_service, p_n2a_perc_production,
             ncol = 3) # sig

n2a_pred_pop_dens <- get_marginal_prediction(dat_cumulative, 'cases_wave2_rate', 'pop_dens', n2a_uni_list[[4]]) # 1.110x
p_n2a_pop_dens <- plot_marginal_prediction(n2a_pred_pop_dens, 'pop_dens', 'Cases / 10000 Pop')

plot(p_n2a_pop_dens) # not sig

n2b_pred_care_home_beds <- get_marginal_prediction(dat_cumulative, 'ifr_wave2', 'care_home_beds', n2b_uni_list[[2]]) # 1.441x
p_n2b_care_home_beds <- plot_marginal_prediction(n2b_pred_care_home_beds, 'care_home_beds', 'CFR')
n2b_pred_GISD_Score <- get_marginal_prediction(dat_cumulative, 'ifr_wave2', 'GISD_Score', n2b_uni_list[[3]]) # 1.252x
p_n2b_GISD_Score <- plot_marginal_prediction(n2b_pred_GISD_Score, 'GISD_Score', 'CFR')

grid.arrange(p_n2b_care_home_beds, p_n2b_GISD_Score, ncol = 2)

# Compare deviance explained/model fit to full models:
anova(n1a, n1a_full, test = 'Chisq') # deviance explained: 73.5% vs. 74.7%
anova(n1a_full, n1a_uni_list[[3]], test = 'Chisq')
# full model improves on null and univariate

anova(n2a, n2a_full, test = 'Chisq') # deviance explained: 73.1% vs. 78.4%
anova(n2a_full, n2a_uni_list[[2]], test = 'Chisq')
anova(n2a_full, n2a_uni_list[[3]], test = 'Chisq')
anova(n2a_full, n2a_uni_list[[4]], test = 'Chisq')
# full model improves on null and all univariate

anova(n1b, n1b_full, test = 'Chisq') # deviance explained: 15.5% vs. 16.1%
# anova(n1b_full, n1b_uni_list[[1]], test = 'Chisq')
# anova(n1b_full, n1b_uni_list[[2]], test = 'Chisq')
# full model does not improve on null model

anova(n2b, n2b_full, test = 'Chisq') # deviance explained: 28.6% vs. 31.8%
anova(n2b_full, n2b_uni_list[[2]], test = 'Chisq')
# full model improves on null but not univariate model

# ---------------------------------------------------------------------------------------------------------------------

### Full models ###

# Determine significant predictors (and differences from "univariate" above):
summary(n1a_full) # GISD_Score (0.000513); spatial, BL
summary(n1b_full) # hosp_beds (0.0131), care_home_beds (0.0260); spatial
summary(n2a_full) # care_home_beds (0.0206), GISD_Score (< 2e-16), pop_dens (3.80e-05); spatial, BL
summary(n2b_full) # care_home_beds (0.00116); spatial
# wave 1 rates not sig for n2a_full or n2b_full

# Plot smooth relationships between significant predictors and outcomes:
n1a_pred_GISD <- get_marginal_prediction(dat_cumulative, 'cases_wave1_rate', 'GISD_Score', n1a_full) # 1.767x
p_n1a_GISD <- plot_marginal_prediction(n1a_pred_GISD, 'GISD_Score', 'Cases / 10000 Pop')

plot(p_n1a_GISD)

n1b_pred_hosp_beds <- get_marginal_prediction(dat_cumulative, 'ifr_wave1', 'hosp_beds', n1b_full) # 1.981
p_n1b_hosp_beds <- plot_marginal_prediction(n1b_pred_hosp_beds, 'hosp_beds', 'CFR')
n1b_pred_care_home_beds <- get_marginal_prediction(dat_cumulative, 'ifr_wave1', 'care_home_beds', n1b_full) # 1.754
p_n1b_care_home_beds <- plot_marginal_prediction(n1b_pred_care_home_beds, 'care_home_beds', 'CFR')

# plot(p_n1b_hosp_beds)
# plot(p_n1b_care_home_beds)
grid.arrange(p_n1b_hosp_beds, p_n1b_care_home_beds, ncol = 2)

# n2a_pred_perc_18to64 <- get_marginal_prediction(dat_cumulative, 'cases_wave2_rate', 'perc_18to64', n2a_full) # 1.173
# p_n2a_perc_18to64 <- plot_marginal_prediction(n2a_pred_perc_18to64, 'perc_18to64', 'Cases / 10000 Pop')
n2a_pred_care_home_beds <- get_marginal_prediction(dat_cumulative, 'cases_wave2_rate', 'care_home_beds', n2a_full) # 1.452
p_n2a_care_home_beds <- plot_marginal_prediction(n2a_pred_care_home_beds, 'care_home_beds', 'Cases / 10000 Pop')
n2a_pred_GISD_Score <- get_marginal_prediction(dat_cumulative, 'cases_wave2_rate', 'GISD_Score', n2a_full) # 1.889
p_n2a_GISD_Score <- plot_marginal_prediction(n2a_pred_GISD_Score, 'GISD_Score', 'Cases / 10000 Pop')
n2a_pred_pop_dens <- get_marginal_prediction(dat_cumulative, 'cases_wave2_rate', 'pop_dens', n2a_full) # 1.478
p_n2a_pop_dens <- plot_marginal_prediction(n2a_pred_pop_dens, 'pop_dens', 'Cases / 10000 Pop')

grid.arrange(p_n2a_care_home_beds, p_n2a_GISD_Score, p_n2a_pop_dens, ncol = 2)

n2b_pred_care_home_beds <- get_marginal_prediction(dat_cumulative, 'ifr_wave2', 'care_home_beds', n2b_full) # 1.499
p_n2b_care_home_beds <- plot_marginal_prediction(n2b_pred_care_home_beds, 'care_home_beds', 'CFR')

plot(p_n2b_care_home_beds)

# Plot marginal effects of SES (even where not significant):
n1b_pred_GISD_Score <- get_marginal_prediction(dat_cumulative, 'ifr_wave1', 'GISD_Score', n1b_full) # 1.312
p_n1b_GISD_Score <- plot_marginal_prediction(n1b_pred_GISD_Score, 'GISD_Score', 'CFR')
n2b_pred_GISD_Score <- get_marginal_prediction(dat_cumulative, 'ifr_wave2', 'GISD_Score', n2b_full) # 1.170
p_n2b_GISD_Score <- plot_marginal_prediction(n2b_pred_GISD_Score, 'GISD_Score', 'CFR')

grid.arrange(p_n1a_GISD, p_n1b_GISD_Score, p_n2a_GISD_Score, p_n2b_GISD_Score, ncol = 2)

n1a_pred_perc_service <- get_marginal_prediction(dat_cumulative, 'cases_wave1_rate', 'perc_service', n1a_full) # 1.115
p_n1a_perc_service <- plot_marginal_prediction(n1a_pred_perc_service, 'perc_service', 'Cases / 10000 Pop')
n1a_pred_perc_production <- get_marginal_prediction(dat_cumulative, 'cases_wave1_rate', 'perc_production', n1a_full) # 1.435
p_n1a_perc_production <- plot_marginal_prediction(n1a_pred_perc_production, 'perc_production', 'Cases / 10000 Pop')

n2a_pred_perc_service <- get_marginal_prediction(dat_cumulative, 'cases_wave2_rate', 'perc_service', n2a_full) # 1.156
p_n2a_perc_service <- plot_marginal_prediction(n2a_pred_perc_service, 'perc_service', 'Cases / 10000 Pop')
n2a_pred_perc_production <- get_marginal_prediction(dat_cumulative, 'cases_wave2_rate', 'perc_production', n2a_full) # 1.201
p_n2a_perc_production <- plot_marginal_prediction(n2a_pred_perc_production, 'perc_production', 'Cases / 10000 Pop')

grid.arrange(p_n1a_perc_service, p_n1a_perc_production, p_n2a_perc_service, p_n2a_perc_production, ncol = 2)

# How well do models explain the data?:
summary(n1a_full) # 74.7%
summary(n1a) # 73.5%

summary(n1b_full) # 16.1%
summary(n1b) # 15.5%

summary(n2a_full) # 78.4%
summary(n2a) # 73.1%

summary(n2b_full) # 31.8%
summary(n2b) # 28.6%

# Plot spatial effect (after controlling for variables):
spatial_trend_FULL <- dat_cumulative %>%
  select(lk, long, lat) %>%
  unique() %>%
  mutate(pop = 10000,
         cases_wave1 = 100,
         cases_wave2 = 100,
         ags2 = '01',
         cases_pre_rate = mean(dat_cumulative$cases_pre_rate),
         perc_18to64 = mean(dat_cumulative$perc_18to64),
         hosp_beds = mean(dat_cumulative$hosp_beds),
         care_home_beds = mean(dat_cumulative$care_home_beds),
         GISD_Score = mean(dat_cumulative$GISD_Score),
         pop_dens = mean(dat_cumulative$pop_dens),
         living_area = mean(dat_cumulative$living_area),
         perc_service = mean(dat_cumulative$perc_service),
         perc_production = mean(dat_cumulative$perc_production))

spatial_trend_FULL <- spatial_trend_FULL %>%
  mutate(fitted_n1a = predict(n1a_full, spatial_trend_FULL, type = 'response'),
         fitted_n2a = predict(n2a_full, spatial_trend_FULL, type = 'response'),
         fitted_n1b = predict(n1b_full, spatial_trend_FULL, type = 'response'),
         fitted_n2b = predict(n2b_full, spatial_trend_FULL, type = 'response'))

map_fitted_FULL <- map_pan %>%
  left_join(spatial_trend_FULL %>%
              select(lk, fitted_n1a:fitted_n2b),
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

p1b <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n1b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 1', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2b <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n2b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis() +
  theme_void() + labs(title = 'Wave 2', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

grid.arrange(p1a, p1b, p2a, p2b, ncol = 2)

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
