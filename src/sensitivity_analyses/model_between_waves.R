# ---------------------------------------------------------------------------------------------------------------------
# Fit GAMs to periods between waves (summer 2020 and 2021)
# Models labeled "a" are models of incidence; models labeled "b" are models of CFR
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(sf)
library(spdep)
library(testthat)
library(pomp)
library(Rcpp)
library(DHARMa)
library(gridExtra)
library(viridis)
library(psych)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
keep_map <- TRUE
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Load functions:
source('src/functions/assess_results_fxns.R')

# ---------------------------------------------------------------------------------------------------------------------

# Determine k-values for lat/long

# # Loop through values and fit models:
# n1a_summer_mods = n1b_summer_mods = n2a_summer_mods = n2b_summer_mods = vector('list', length = length(seq(10, 100, by = 10)))
# for (i in 1:length(seq(10, 100, by = 10))) {
#   k_val <- seq(10, 100, by = 10)[i]
#   
#   n1a_summer_mods[[i]] <- gam(cases_summer1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
#                                 s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                                 s(perc_service) + s(perc_production) +
#                                 s(cases_wave1_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
#   n1b_summer_mods[[i]] <- gam(deaths_summer1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
#                                 s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_summer1_rate) +
#                                 s(cases_wave1_rate) +
#                                 offset(log(cases_summer1)), data = dat_cumulative, family = 'nb', method = 'ML')
#   
#   n2a_summer_mods[[i]] <- gam(cases_summer2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
#                                 s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                                 s(perc_service) + s(perc_production) +
#                                 s(cases_pre_summer2_rate) + s(vacc_summer2_reg) +
#                                 offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
#   n2b_summer_mods[[i]] <- gam(deaths_summer2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
#                                 s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_summer2_rate) +
#                                 s(cases_pre_summer2_rate) + s(vacc_summer2_reg) +
#                                 offset(log(cases_summer2)), data = dat_cumulative, family = 'nb', method = 'ML')
#   
# }
# 
# # Evaluate AICs and BICs:
# n1a_summer_aics <- lapply(n1a_summer_mods, AIC) %>% unlist()
# n1b_summer_aics <- lapply(n1b_summer_mods, AIC) %>% unlist()
# n2a_summer_aics <- lapply(n2a_summer_mods, AIC) %>% unlist()
# n2b_summer_aics <- lapply(n2b_summer_mods, AIC) %>% unlist()
# 
# n1a_summer_bics <- lapply(n1a_summer_mods, BIC) %>% unlist()
# n1b_summer_bics <- lapply(n1b_summer_mods, BIC) %>% unlist()
# n2a_summer_bics <- lapply(n2a_summer_mods, BIC) %>% unlist()
# n2b_summer_bics <- lapply(n2b_summer_mods, BIC) %>% unlist()
# 
# # Plot:
# par(mfrow = c(2, 1))
# plot(seq(10, 100, by = 10), n1a_summer_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
# plot(seq(10, 100, by = 10), n1a_summer_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')
# 
# plot(seq(10, 100, by = 10), n1b_summer_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
# plot(seq(10, 100, by = 10), n1b_summer_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')
# 
# plot(seq(10, 100, by = 10), n2a_summer_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
# plot(seq(10, 100, by = 10), n2a_summer_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')
# 
# plot(seq(10, 100, by = 10), n2b_summer_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
# plot(seq(10, 100, by = 10), n2b_summer_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

# ---------------------------------------------------------------------------------------------------------------------

# Fit models
# Use k-values found to be best by AIC/DHARMa/checks for spatial autocorrelation
# For all other parameters, check gam.check to determine whether k's are high enough
# Fit first without interactions, then check whether interactions improve fit

# Summer 2020:
# Note: cases_wave1_rate is functionally the same as cases_pre_summer1_rate
n1a_summer <- bake(file = 'results/fitted_models/SA/FULL_n1a_summer_ml.rds',
                   expr = {
                     gam(cases_summer1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                           s(perc_service) + s(perc_production) +
                           s(cases_wave1_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)
n1b_summer <- bake(file = 'results/fitted_models/SA/FULL_n1b_summer_ml.rds',
                   expr = {
                     gam(deaths_summer1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                           s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens, k = 25) + s(cases_summer1_rate) +
                           s(cases_wave1_rate) +
                           offset(log(cases_summer1)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)

# Summer 2021:
n2a_summer <- bake(file = 'results/fitted_models/SA/FULL_n2a_summer_ml.rds',
                   expr = {
                     gam(cases_summer2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 55) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64) + s(perc_lessthan18, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                           s(perc_service) + s(perc_production) +
                           s(cases_pre_summer2_rate) + s(vacc_summer2_reg) +
                           offset(log(pop)), data = dat_cumulative, family = 'tw', method = 'ML')
                   }
)
n2b_summer <- bake(file = 'results/fitted_models/SA/FULL_n2b_summer_ml.rds',
                   expr = {
                     gam(deaths_summer2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                           s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_summer2_rate) +
                           s(cases_pre_summer2_rate) + s(vacc_summer2_reg) + #ti(cases_summer2_rate, hosp_beds) +
                           offset(log(cases_summer2)), data = dat_cumulative, family = 'nb', method = 'ML')
                   }
)

# Quick check of fits:
par(mfrow = c(2, 2))
gam.check(n1a_summer, rep = 50)
gam.check(n1b_summer, rep = 50)
gam.check(n2a_summer, rep = 50)
gam.check(n2b_summer, rep = 50)

# ---------------------------------------------------------------------------------------------------------------------

# # Check for inclusion of interactions
#
# # Fit with all possible interactions:
# n1a_comp <- gam(cases_summer1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                   s(perc_service) + s(perc_production) +
#                   ti(perc_18to64, pop_dens) + ti(perc_18to64, GISD_Score) + ti(pop_dens, GISD_Score) +
#                   ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, GISD_Score) +
#                   s(cases_wave1_rate) + offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# n1b_comp <- gam(deaths_summer1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
#                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens, k = 25) + s(cases_summer1_rate) +
#                   s(cases_wave1_rate) + ti(pop_dens, GISD_Score) + ti(cases_summer1_rate, hosp_beds) +
#                   offset(log(cases_summer1)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# n2a_comp <- gam(cases_summer2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 55) + s(ags2, bs = 're', k = 16) +
#                   s(perc_18to64) + s(perc_lessthan18, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                   s(perc_service) + s(perc_production) +
#                   s(cases_pre_summer2_rate) + s(vacc_summer2_reg) +
#                   ti(perc_18to64, pop_dens) + ti(perc_18to64, GISD_Score) + ti(pop_dens, GISD_Score) +
#                   ti(perc_lessthan18, pop_dens) + ti(perc_lessthan18, GISD_Score) +
#                   offset(log(pop)), data = dat_cumulative, family = 'tw', method = 'ML')
# n2b_comp <- gam(deaths_summer2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
#                   s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_summer2_rate) +
#                   ti(pop_dens, GISD_Score) + ti(cases_summer2_rate, hosp_beds) +
#                   s(cases_pre_summer2_rate) + s(vacc_summer2_reg) + #ti(cases_summer2_rate, hosp_beds) +
#                   offset(log(cases_summer2)), data = dat_cumulative, family = 'nb', method = 'ML')
# 
# AIC(n1a_summer, n1a_comp)
# AIC(n2a_summer, n2a_comp)
# AIC(n1b_summer, n1b_comp)
# AIC(n2b_summer, n2b_comp)
# 
# BIC(n1a_summer, n1a_comp)
# BIC(n2a_summer, n2a_comp)
# BIC(n1b_summer, n1b_comp)
# BIC(n2b_summer, n2b_comp)

# ---------------------------------------------------------------------------------------------------------------------

# Residual checks

# Check using DHARMa package:
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n1a_summer, depend = 'none')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n1b_summer, depend = 'none')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n2a_summer, depend = 'none')
par(mfrow = c(2, 2))
check_dharma(dat_cumulative, n2b_summer, depend = 'none')

# Check for residual spatial autocorrelation:
dat_cumulative$resid_n1a_summer <- residuals(n1a_summer, type = 'deviance')
dat_cumulative$resid_n1b_summer <- residuals(n1b_summer, type = 'deviance')
dat_cumulative$resid_n2a_summer <- residuals(n2a_summer, type = 'deviance')
dat_cumulative$resid_n2b_summer <- residuals(n2b_summer, type = 'deviance')

map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')
map_base <- map_base %>%
  left_join(dat_cumulative %>%
              select(lk, resid_n1a_summer:resid_n2b_summer),
            by = c('ARS' = 'lk')) %>%
  drop_na()

nb <- spdep::poly2nb(map_base, row.names = map_base$ARS)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)
moran.mc(map_base$resid_n1a_summer, lw, nsim = 999)
moran.mc(map_base$resid_n1b_summer, lw, nsim = 999)
moran.mc(map_base$resid_n2a_summer, lw, nsim = 999)
moran.mc(map_base$resid_n2b_summer, lw, nsim = 999)

# ---------------------------------------------------------------------------------------------------------------------

# Plot results

# Plot observed incidence/CFR for between-wave periods:
map_pan <- map_base %>%
  left_join(dat_cumulative %>%
              select(lk, cases_summer1_rate, cases_summer2_rate, cfr_summer1, cfr_summer2),
            by = c('ARS' = 'lk'))
map_bl <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_lan.shp')

p_summer1a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_summer1_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_summer1_rate, map_pan$cases_summer2_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(5, 25, 50)) +
  theme_void() + labs(title = 'Summer 2020', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p_summer2a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_summer2_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_summer1_rate, map_pan$cases_summer2_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(5, 25, 50)) +
  theme_void() + labs(title = 'Summer 2021', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

p_summer1b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_summer1), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_summer1, map_pan$cfr_summer2, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 2, 4, 8)) +
  theme_void() + labs(title = 'Summer 2020', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p_summer2b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_summer2), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_summer1, map_pan$cfr_summer2, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 2, 4, 8)) +
  theme_void() + labs(title = 'Summer 2021', fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

grid.arrange(p_summer1a, p_summer2a, p_summer1b, p_summer2b, ncol = 2)

# Significant clustering by Moran's I?:
map_pan <- map_pan %>%
  drop_na()
map_temp <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp') %>%
  filter(GEN != 'Eisenach', GEN != 'Wartburgkreis')

nb <- spdep::poly2nb(map_temp, row.names = map_temp$ARS)
attr(nb, 'region.id') <- map_temp$ARS
names(nb) <- attr(nb, 'region.id')

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)
moran.mc(map_pan$cases_summer1_rate, lw, nsim = 999)
moran.mc(map_pan$cases_summer2_rate, lw, nsim = 999)
moran.mc(map_pan$cfr_summer1, lw, nsim = 999)
moran.mc(map_pan$cfr_summer2, lw, nsim = 999)

# How similar are patterns to those during waves?:
pairs.panels(dat_cumulative %>%
               select(cases_wave1_rate, cases_wave2_rate, cases_wave3_rate, cases_wave4_rate, cases_wave5_rate,
                      cases_summer1_rate, cases_summer2_rate),
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
               select(cfr_wave1, cfr_wave2, cfr_wave3, cfr_wave4, cfr_wave5,
                      cfr_summer1, cfr_summer2),
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

# How much deviance explained?:
summary(n1a_summer)
summary(n2a_summer)

summary(n1b_summer)
summary(n2b_summer)

# Plot smooth relationships between GISD_Score and incidence/CFR:
mod_list <- list(n1a_summer, n2a_summer)
names(mod_list) <- c('summer1', 'summer2')

pred_a_GISD_Score <- get_marginal_prediction(dat_cumulative, 'GISD_Score', 'incidence', mod_list,
                                             standardize = TRUE, partial_waves = FALSE,
                                             between_waves = TRUE)
plot_a_GISD_Score <- plot_marginal_prediction(pred_a_GISD_Score, 'GISD_Score', 'Cases / 10000 Pop',
                                              single_plot = TRUE)
print(plot_a_GISD_Score)

mod_list <- list(n1b_summer, n2b_summer)
names(mod_list) <- c('summer1', 'summer2')

pred_b_GISD_Score <- get_marginal_prediction(dat_cumulative, 'GISD_Score', 'cfr', mod_list,
                                             standardize = TRUE, partial_waves = FALSE,
                                             between_waves = TRUE)
plot_b_GISD_Score <- plot_marginal_prediction(pred_b_GISD_Score, 'GISD_Score', 'CFR',
                                              single_plot = TRUE)
print(plot_b_GISD_Score)

# Plot smooth relationships between incidence and CFR:
pred_b_cases_rate <- get_marginal_prediction(dat_cumulative, 'cases_rate', 'cfr', mod_list,
                                             standardize = TRUE, partial_waves = FALSE,
                                             between_waves = TRUE)
plot_b_cases_rate <- plot_marginal_prediction(pred_b_cases_rate, 'cases_rate', 'CFR',
                                              single_plot = FALSE)
print(plot_b_cases_rate)
