# ---------------------------------------------------------------------------------------------------------------------
# Fit GAMs to individual age groups, as sensitivity analysis
# Models labeled "a" are models of incidence; models labeled "b" are models of CFR
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(sf)
library(testthat)
library(pomp)
library(spdep)
library(Rcpp)
library(DHARMa)
library(psych)
library(patchwork)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format age-specific data

# For age 60+:
sens_age <- 'age_60plus'
source('src/functions/load_data.R')
dat_60plus <- dat_cumulative

# For age 15-59:
sens_age <- 'age_15thru59'
source('src/functions/load_data.R')
dat_15thru59 <- dat_cumulative
rm(dat_cumulative)

# ---------------------------------------------------------------------------------------------------------------------

# Fit models with all predictors (60+ age group)

# Wave 1:
n1_1a_full <- bake(file = 'results/fitted_models/SA_age/FULL_n1_1a_ml_ELDERLY.rds',
                   expr = {
                     gam(cases_wave1_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 45) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                           s(perc_service) + s(perc_production) +
                           offset(log(pop)), data = dat_60plus, family = 'nb', method = 'ML')
                   }
)
n1_2a_full <- bake(file = 'results/fitted_models/SA_age/FULL_n1_2a_ml_ELDERLY.rds',
                   expr = {
                     gam(cases_wave1_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 80) + s(ags2, bs = 're', k = 16) +
                           s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) +
                           s(perc_service) + s(perc_production) + s(cases_wave1_1_rate) +
                           offset(log(pop)), data = dat_60plus, family = 'nb', method = 'ML')
                   }
)

# Wave 2:
n2a_full <- bake(file = 'results/fitted_models/SA_age/FULL_n2a_ml_ELDERLY.rds',
                 expr = {
                   gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64, k = 25) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score, k = 25) + s(pop_dens) +
                         s(perc_service) + s(perc_production, k = 25) +
                         s(cases_pre2_rate) + offset(log(pop)), data = dat_60plus, family = 'nb', method = 'ML')
                 }
)

# Wave 3:
n3a_full <- bake(file = 'results/fitted_models/SA_age/FULL_n3a_ml_ELDERLY.rds',
                 expr = {
                   gam(cases_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         s(cases_pre3_rate) + s(vacc_w3_reg) +
                         offset(log(pop)), data = dat_60plus, family = 'nb', method = 'ML')
                 }
)

# Wave 4:
n4a_full <- bake(file = 'results/fitted_models/SA_age/FULL_n4a_ml_ELDERLY.rds',
                 expr = {
                   gam(cases_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18, k = 25) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         s(cases_pre4_rate) + s(vacc_w4_reg) +
                         offset(log(pop)), data = dat_60plus, family = 'nb', method = 'ML')
                 }
)

# Wave 5:
n5a_full <- bake(file = 'results/fitted_models/SA_age/FULL_n5a_ml_ELDERLY.rds',
                 expr = {
                   gam(cases_wave5 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                         s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                         s(perc_service) + s(perc_production) +
                         s(cases_pre5_rate, k = 25) + s(vacc_w5_reg) +
                         offset(log(pop)), data = dat_60plus, family = 'nb', method = 'ML')
                 }
)

# ---------------------------------------------------------------------------------------------------------------------

# Fit models with all predictors (15-59 age group)

# Wave 1:
n1_1b_full <- bake(file = 'results/fitted_models/SA_age/FULL_n1_1b_ml_ADULT.rds',
                   expr = {
                     gam(deaths_wave1_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                           s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave1_1_rate) +
                           offset(log(cases_wave1_1)), data = dat_15thru59, family = 'nb', method = 'ML')
                   }
)
n1_2b_full <- bake(file = 'results/fitted_models/SA_age/FULL_n1_2b_ml_ADULT.rds',
                   expr = {
                     gam(deaths_wave1_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                           s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                           s(cases_wave1_1_rate) + s(cases_wave1_2_rate) +
                           offset(log(cases_wave1_2)), data = dat_15thru59, family = 'nb', method = 'ML')
                   }
)

# Wave 2:
n2b_full <- bake(file = 'results/fitted_models/SA_age/FULL_n2b_ml_ADULT.rds',
                 expr = {
                   gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave2_rate) +
                         s(cases_pre2_rate) + offset(log(cases_wave2)), data = dat_15thru59, family = 'nb', method = 'ML')
                 }
)

# Wave 3:
n3b_full <- bake(file = 'results/fitted_models/SA_age/FULL_n3b_ml_ADULT.rds',
                 expr = {
                   gam(deaths_wave3 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave3_rate, k = 25) +
                         s(cases_pre3_rate) + s(vacc_w3_reg) + ti(pop_dens, GISD_Score) +
                         offset(log(cases_wave3)), data = dat_15thru59, family = 'nb', method = 'ML')
                 }
)

# Wave 4:
n4b_full <- bake(file = 'results/fitted_models/SA_age/FULL_n4b_ml_ADULT.rds',
                 expr = {
                   gam(deaths_wave4 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 40) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave4_rate) +
                         s(cases_pre4_rate) + s(vacc_w4_reg) +
                         offset(log(cases_wave4)), data = dat_15thru59, family = 'nb', method = 'ML')
                 }
)

# Wave 5:
n5b_full <- bake(file = 'results/fitted_models/SA_age/FULL_n5b_ml_ADULT.rds',
                 expr = {
                   gam(deaths_wave5 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 30) + s(ags2, bs = 're', k = 16) +
                         s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(cases_wave5_rate) +
                         s(cases_pre5_rate) + s(vacc_w5_reg) +
                         offset(log(cases_wave5)), data = dat_15thru59, family = 'nb', method = 'ML')
                 }
)

# ---------------------------------------------------------------------------------------------------------------------

# Assess residuals

# Load necessary functions:
source('src/functions/assess_results_fxns.R')

# Check using DHARMa package:
par(mfrow = c(2, 2))
check_dharma(dat_60plus, n1_1a_full, depend = 'cases')
par(mfrow = c(2, 2))
check_dharma(dat_60plus, n1_2a_full, depend = 'cases')
par(mfrow = c(2, 2))
check_dharma(dat_60plus, n2a_full, depend = 'cases')
par(mfrow = c(2, 2))
check_dharma(dat_60plus, n3a_full, depend = 'cases')
par(mfrow = c(2, 2))
check_dharma(dat_60plus, n4a_full, depend = 'cases')
par(mfrow = c(2, 2))
check_dharma(dat_60plus, n5a_full, depend = 'cases')

par(mfrow = c(2, 2))
check_dharma(dat_15thru59, n1_1b_full, depend = 'deaths')
par(mfrow = c(2, 2))
check_dharma(dat_15thru59, n1_2b_full, depend = 'deaths')
par(mfrow = c(2, 2))
check_dharma(dat_15thru59, n2b_full, depend = 'deaths')
par(mfrow = c(2, 2))
check_dharma(dat_15thru59, n3b_full, depend = 'deaths')
par(mfrow = c(2, 2))
check_dharma(dat_15thru59, n4b_full, depend = 'deaths')
par(mfrow = c(2, 2))
check_dharma(dat_15thru59, n5b_full, depend = 'deaths')

# Calculate Moran's I for residuals:
dat_60plus$resid_n1_1a <- residuals(n1_1a_full, type = 'deviance')
dat_60plus$resid_n1_2a <- residuals(n1_2a_full, type = 'deviance')
dat_60plus$resid_n2a <- residuals(n2a_full, type = 'deviance')
dat_60plus$resid_n3a <- residuals(n3a_full, type = 'deviance')
dat_60plus$resid_n4a <- residuals(n4a_full, type = 'deviance')
dat_60plus$resid_n5a <- residuals(n5a_full, type = 'deviance')

dat_15thru59$resid_n1_1b <- residuals(n1_1b_full, type = 'deviance')
dat_15thru59$resid_n1_2b <- residuals(n1_2b_full, type = 'deviance')
dat_15thru59$resid_n2b <- residuals(n2b_full, type = 'deviance')
dat_15thru59$resid_n3b <- residuals(n3b_full, type = 'deviance')
dat_15thru59$resid_n4b <- residuals(n4b_full, type = 'deviance')
dat_15thru59$resid_n5b <- residuals(n5b_full, type = 'deviance')

map_base <- st_read(dsn = 'data/raw/map/vg2500_12-31.gk3.shape/vg2500/VG2500_KRS.shp')
map_base <- map_base %>%
  left_join(dat_60plus %>%
              select(lk, resid_n1_1a, resid_n1_2a, resid_n2a, resid_n3a, resid_n4a, resid_n5a),
            by = c('ARS' = 'lk')) %>%
  left_join(dat_15thru59 %>%
              select(lk, resid_n1_1b, resid_n1_2b, resid_n2b, resid_n3b, resid_n4b, resid_n5b),
            by = c('ARS' = 'lk')) %>%
  drop_na()

nb <- spdep::poly2nb(map_base, row.names = map_base$ARS)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)

moran.mc(map_base$resid_n1_1a, lw, nsim = 999)
moran.mc(map_base$resid_n1_2a, lw, nsim = 999)
moran.mc(map_base$resid_n1_1b, lw, nsim = 999)
moran.mc(map_base$resid_n1_2b, lw, nsim = 999)
moran.mc(map_base$resid_n2a, lw, nsim = 999)
moran.mc(map_base$resid_n2b, lw, nsim = 999)
moran.mc(map_base$resid_n3a, lw, nsim = 999)
moran.mc(map_base$resid_n3b, lw, nsim = 999)
moran.mc(map_base$resid_n4a, lw, nsim = 999)
moran.mc(map_base$resid_n4b, lw, nsim = 999)
moran.mc(map_base$resid_n5a, lw, nsim = 999)
moran.mc(map_base$resid_n5b, lw, nsim = 999)

# ---------------------------------------------------------------------------------------------------------------------

# Get descriptive statistics and compare to age-standardized population

# Get range of incidence/CFR:
dat_60plus %>% select(cases_wave1_1_rate, cases_wave1_2_rate, cases_wave2_rate, cases_wave3_rate, cases_wave4_rate, cases_wave5_rate) %>% summary()
dat_15thru59 %>% select(cfr_wave1_1, cfr_wave1_2, cfr_wave2:cfr_wave5) %>% summary()

# Assess spatial clustering in the data:
map_base <- map_base %>%
  left_join(dat_60plus %>%
              select(lk, cases_wave1_rate, cases_wave1_1_rate, cases_wave1_2_rate, cases_wave2_rate, cases_wave3_rate, cases_wave4_rate, cases_wave5_rate),
            by = c('ARS' = 'lk')) %>%
  left_join(dat_15thru59 %>%
              select(lk, cfr_wave1, cfr_wave1_1, cfr_wave1_2, cfr_wave2, cfr_wave3, cfr_wave4, cfr_wave5),
            by = c('ARS' = 'lk')) %>%
  drop_na()

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)
moran.mc(map_base$cases_wave1_rate, lw, nsim = 999)
moran.mc(map_base$cases_wave1_1_rate, lw, nsim = 999)
moran.mc(map_base$cases_wave1_2_rate, lw, nsim = 999)
moran.mc(map_base$cases_wave2_rate, lw, nsim = 999)
moran.mc(map_base$cases_wave3_rate, lw, nsim = 999)
moran.mc(map_base$cases_wave4_rate, lw, nsim = 999)
moran.mc(map_base$cases_wave5_rate, lw, nsim = 999)
moran.mc(map_base$cfr_wave1, lw, nsim = 999)
moran.mc(map_base$cfr_wave1_1, lw, nsim = 999)
moran.mc(map_base$cfr_wave1_2, lw, nsim = 999)
moran.mc(map_base$cfr_wave2, lw, nsim = 999)
moran.mc(map_base$cfr_wave3, lw, nsim = 999)
moran.mc(map_base$cfr_wave4, lw, nsim = 999)
moran.mc(map_base$cfr_wave5, lw, nsim = 999)

# Plot consistency in spatial patterns over time:
pairs.panels(dat_60plus %>%
               select(cases_wave1_rate, cases_wave1_1_rate, cases_wave1_2_rate,
                      cases_wave2_rate, cases_wave3_rate, cases_wave4_rate,
                      cases_wave5_rate),
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
pairs.panels(dat_15thru59 %>%
               select(cfr_wave1, cfr_wave1_1, cfr_wave1_2, cfr_wave2,
                      cfr_wave3, cfr_wave4, cfr_wave5),
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

# Plot relationships between SES variables and outcomes

# Incidence / 60+:
mod_list <- list(n1_1a_full, n1_2a_full, n2a_full, n3a_full, n4a_full, n5a_full)
names(mod_list) <- c('1_1', '1_2', '2', '3', '4', '5')

pred_perc_18to64 <- get_marginal_prediction(dat_60plus, 'perc_18to64', 'incidence', mod_list, standardize = TRUE)
pred_perc_lessthan18 <- get_marginal_prediction(dat_60plus, 'perc_lessthan18', 'incidence', mod_list, standardize = TRUE)
pred_care_home_beds <- get_marginal_prediction(dat_60plus, 'care_home_beds', 'incidence', mod_list, standardize = TRUE)
pred_GISD_Score <- get_marginal_prediction(dat_60plus, 'GISD_Score', 'incidence', mod_list, standardize = TRUE)
pred_pop_dens <- get_marginal_prediction(dat_60plus, 'pop_dens', 'incidence', mod_list, standardize = TRUE)
pred_perc_service <- get_marginal_prediction(dat_60plus, 'perc_service', 'incidence', mod_list, standardize = TRUE)
pred_perc_production <- get_marginal_prediction(dat_60plus, 'perc_production', 'incidence', mod_list, standardize = TRUE)

plot_a_18to64 <- plot_marginal_prediction(pred_perc_18to64, 'perc_18to64', 'Incidence', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'))
plot_a_lessthan18 <- plot_marginal_prediction(pred_perc_lessthan18, 'perc_lessthan18', 'Incidence', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'))
plot_a_care_home_beds <- plot_marginal_prediction(pred_care_home_beds, 'care_home_beds', 'Incidence', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'))
plot_a_GISD_Score <- plot_marginal_prediction(pred_GISD_Score, 'GISD_Score', 'Incidence', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'))
plot_a_pop_dens <- plot_marginal_prediction(pred_pop_dens, 'pop_dens', 'Incidence', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'))
plot_a_service <- plot_marginal_prediction(pred_perc_service, 'perc_service', 'Incidence', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'))
plot_a_production <- plot_marginal_prediction(pred_perc_production, 'perc_production', 'Incidence', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'))

plot_a_GISD_Score <- plot_a_GISD_Score + labs(x = 'GISD', tag = 'A') + theme(plot.tag = element_text(size = 20), plot.tag.position = c(0.005, 0.98))
plot_a_service <- plot_a_service + labs(x = '% Service', tag = 'B') + theme(plot.tag = element_text(size = 20), plot.tag.position = c(0.005, 0.98))
plot_a_production <- plot_a_production + labs(x = '% Production', tag = 'C') + theme(plot.tag = element_text(size = 20), plot.tag.position = c(0.005, 0.98))
plot_a_18to64 <- plot_a_18to64 + labs(x = '% Aged 18-64', tag = 'D') + theme(plot.tag = element_text(size = 20), plot.tag.position = c(0.005, 0.98))
plot_a_lessthan18 <- plot_a_lessthan18 + labs(x = '% Aged <18', tag = 'E') + theme(plot.tag = element_text(size = 20), plot.tag.position = c(0.005, 0.98))
plot_a_pop_dens <- plot_a_pop_dens + labs(x = 'Population Density (100s / km2)', tag = 'F') + theme(plot.tag = element_text(size = 20), plot.tag.position = c(0.005, 0.98))
plot_a_care_home_beds <- plot_a_care_home_beds + labs(x = 'Care Home Beds per 10000 Pop', tag = 'G') + theme(plot.tag = element_text(size = 20), plot.tag.position = c(0.005, 0.98))

fig9s <- plot_a_GISD_Score + plot_a_service + plot_a_production + plot_a_18to64 + plot_a_lessthan18 +
  plot_a_pop_dens + plot_a_care_home_beds + plot_layout(nrow = 2)
# ggsave('results/FigureS9.svg', fig9s, width = 22.5, height = 7.8)

# CFRs / 15-59:
mod_list <- list(n1_1b_full, n1_2b_full, n2b_full, n3b_full, n4b_full, n5b_full)
names(mod_list) <- c('1_1', '1_2', '2', '3', '4', '5')

pred_GISD_Score <- get_marginal_prediction(dat_15thru59, 'GISD_Score', 'cfr', mod_list, standardize = TRUE)
pred_hosp_beds <- get_marginal_prediction(dat_15thru59, 'hosp_beds', 'cfr', mod_list, standardize = TRUE)
pred_care_home_beds <- get_marginal_prediction(dat_15thru59, 'care_home_beds', 'cfr', mod_list, standardize = TRUE)

plot_b_GISD_Score <- plot_marginal_prediction(pred_GISD_Score, 'GISD_Score', 'CFR', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'))
plot_b_hosp_beds <- plot_marginal_prediction(pred_hosp_beds, 'hosp_beds', 'CFR', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'))
plot_b_care_home_beds <- plot_marginal_prediction(pred_care_home_beds, 'care_home_beds', 'CFR', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'))

plot_b_GISD_Score <- plot_b_GISD_Score + labs(x = 'GISD', tag = 'A') + theme(plot.tag = element_text(size = 20), plot.tag.position = c(0.005, 0.98))
plot_b_hosp_beds <- plot_b_hosp_beds + labs(x = 'Hospital Beds per 1000 Pop', tag = 'B') + theme(plot.tag = element_text(size = 20), plot.tag.position = c(0.005, 0.98))
plot_b_care_home_beds <- plot_b_care_home_beds + labs(x = 'Care Home Beds per 10000 Pop', tag = 'C') + theme(plot.tag = element_text(size = 20), plot.tag.position = c(0.005, 0.98))

fig10s <- plot_b_GISD_Score + plot_b_hosp_beds + plot_b_care_home_beds + plot_layout(nrow = 1)
# ggsave('results/FigureS10.svg', fig10s, width = 16.875, height = 4.0)

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
