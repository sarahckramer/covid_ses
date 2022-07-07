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
library(patchwork)
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
n1_1a_full <- read_rds('results/fitted_models/FULL_n1_1a_ml.rds')
n1_2a_full <- read_rds('results/fitted_models/FULL_n1_2a_ml.rds')
n1b_full <- read_rds('results/fitted_models/FULL_n1b_ml.rds')
n2a_full <- read_rds('results/fitted_models/FULL_n2a_ml.rds')
n2b_full <- read_rds('results/fitted_models/FULL_n2b_ml.rds')
n3a_full <- read_rds('results/fitted_models/FULL_n3a_ml.rds')
n3b_full <- read_rds('results/fitted_models/FULL_n3b_ml.rds')
n4a_full <- read_rds('results/fitted_models/FULL_n4a_ml.rds')
n4b_full <- read_rds('results/fitted_models/FULL_n4b_ml.rds')

# Null models:
n1_1a <- read_rds('results/fitted_models/null_n1_1a_ml.rds')
n1_2a <- read_rds('results/fitted_models/null_n1_2a_ml.rds')
n1b <- read_rds('results/fitted_models/null_n1b_ml.rds')
n2a <- read_rds('results/fitted_models/null_n2a_ml.rds')
n2b <- read_rds('results/fitted_models/null_n2b_ml.rds')
n3a <- read_rds('results/fitted_models/null_n3a_ml.rds')
n3b <- read_rds('results/fitted_models/null_n3b_ml.rds')
n4a <- read_rds('results/fitted_models/null_n4a_ml.rds')
n4b <- read_rds('results/fitted_models/null_n4b_ml.rds')

n1_2a_adj <- read_rds('results/fitted_models/null_n1_2a_adj_ml.rds')
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
                     cases_wave3_rate, cfr_wave3, cases_wave4_rate, cfr_wave4,
                     cases_wave1_1_rate, cases_wave1_2_rate),
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
  theme_void() + labs(title = 'Wave 1', fill = '', tag = 'A') +#, fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 22, hjust = 0.5),
        legend.title = element_text(size = 12), legend.text = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'), legend.key.height = unit(0.7, 'cm'),
        plot.tag = element_text(size = 26), plot.tag.position = c(0.08, 0.96))
p2a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave2_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate, map_pan$cases_wave3_rate, map_pan$cases_wave4_rate, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate, map_pan$cases_wave3_rate, map_pan$cases_wave4_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 2', fill = '') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 22, hjust = 0.5),
        legend.title = element_text(size = 12), legend.text = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'), legend.key.height = unit(0.7, 'cm'))
p3a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave3_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate, map_pan$cases_wave3_rate, map_pan$cases_wave4_rate, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate, map_pan$cases_wave3_rate, map_pan$cases_wave4_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 3', fill = '') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 22, hjust = 0.5),
        legend.title = element_text(size = 12), legend.text = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'), legend.key.height = unit(0.7, 'cm'))
p4a <- ggplot(data = map_pan) + geom_sf(aes(fill = cases_wave4_rate), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate, map_pan$cases_wave3_rate, map_pan$cases_wave4_rate, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cases_wave1_rate, map_pan$cases_wave2_rate, map_pan$cases_wave3_rate, map_pan$cases_wave4_rate, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(100, 500, 1250)) +
  theme_void() + labs(title = 'Wave 4', fill = '') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 22, hjust = 0.5),
        legend.title = element_text(size = 12), legend.text = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'), legend.key.height = unit(0.7, 'cm'))

p1b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave1), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 4, 8, 16)) +
  theme_void() + labs(title = 'Wave 1', fill = '', tag = 'B') +# fill = '% CFR') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 22, hjust = 0.5),
        legend.title = element_text(size = 12), legend.text = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'), legend.key.height = unit(0.7, 'cm'),
        plot.tag = element_text(size = 26), plot.tag.position = c(0.08, 0.96))
p2b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave2), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 4, 8, 16)) +
  theme_void() + labs(title = 'Wave 2', fill = '') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 22, hjust = 0.5),
        legend.title = element_text(size = 12), legend.text = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'), legend.key.height = unit(0.7, 'cm'))
p3b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave3), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 4, 8, 16)) +
  theme_void() + labs(title = 'Wave 3', fill = '') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 22, hjust = 0.5),
        legend.title = element_text(size = 12), legend.text = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'), legend.key.height = unit(0.7, 'cm'))
p4b <- ggplot(data = map_pan) + geom_sf(aes(fill = cfr_wave4), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  # scale_fill_viridis() +
  # scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE))) +
  scale_fill_viridis(limits = c(0, max(map_pan$cfr_wave1, map_pan$cfr_wave2, map_pan$cfr_wave3, map_pan$cfr_wave4, na.rm = TRUE)),
                     trans = 'sqrt', na.value = 'gray80', breaks = c(0, 1, 4, 8, 16)) +
  theme_void() + labs(title = 'Wave 4', fill = '') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 22, hjust = 0.5),
        legend.title = element_text(size = 14), legend.text = element_text(size = 16),
        legend.key.width = unit(1.2, 'cm'), legend.key.height = unit(0.7, 'cm'))

# grid.arrange(p1a, p2a, p3a, p4a, p1b, p2b, p3b, p4b, ncol = 4)

fig1 <- (p1a + p2a + p3a + p4a + plot_layout(nrow = 1)) / (p1b + p2b + p3b + p4b + plot_layout(nrow = 1))
ggsave('results/Figure1.svg', fig1, width = 12, height = 10.5)

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
moran.mc(map_pan$cases_wave1_1_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave1_2_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave2_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave3_rate, lw, nsim = 999)
moran.mc(map_pan$cases_wave4_rate, lw, nsim = 999)
moran.mc(map_pan$cfr_wave1, lw, nsim = 999)
moran.mc(map_pan$cfr_wave2, lw, nsim = 999)
moran.mc(map_pan$cfr_wave3, lw, nsim = 999)
moran.mc(map_pan$cfr_wave4, lw, nsim = 999)

# How consistent are patterns from one wave to the next?:
pairs.panels(dat_cumulative %>%
               select(cases_wave1_rate, cases_wave1_1_rate, cases_wave1_2_rate,
                      cases_wave2_rate, cases_wave3_rate, cases_wave4_rate),
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

### Assess models with no predictors ###

# Check whether controlling for incidence in past 26 weeks (and vaccination) improves model fit:
anova(n1_2a, n1_2a_adj, test = 'Chisq')
anova(n2a, n2a_adj, test = 'Chisq')
anova(n2b, n2b_adj, test = 'Chisq')
anova(n3a, n3a_adj, test = 'Chisq')
anova(n3b, n3b_adj, test = 'Chisq')
anova(n4a, n4a_adj, test = 'Chisq')
anova(n4b, n4b_adj, test = 'Chisq')

# Check % deviance explained:
summary(n1_1a)
summary(n1_2a_adj)
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

### Full models ###

# Determine percent deviance explained:
summary(n1_1a_full)
summary(n1_2a_full)
summary(n1b_full)
summary(n2a_full)
summary(n2b_full)
summary(n3a_full)
summary(n3b_full)
summary(n4a_full)
summary(n4b_full)

# Compare deviance explained/model fit to null models:
anova(n1_1a, n1_1a_full, test = 'Chisq')
anova(n1_2a_adj, n1_2a_full, test = 'Chisq')
anova(n2a_adj, n2a_full, test = 'Chisq')
anova(n3a_adj, n3a_full, test = 'Chisq')
anova(n4a_adj, n4a_full, test = 'Chisq')

anova(n1b, n1b_full, test = 'Chisq')
anova(n2b_adj, n2b_full, test = 'Chisq')
anova(n3b_adj, n3b_full, test = 'Chisq')
anova(n4b_adj, n4b_full, test = 'Chisq')

# Plot smooth relationships between all predictors and outcomes (for incidence):
mod_list <- list(n1_1a_full, n1_2a_full, n2a_full, n3a_full, n4a_full)
names(mod_list) <- c('1_1', '1_2', '2', '3', '4')

pred_perc_18to64 <- get_marginal_prediction(dat_cumulative, 'perc_18to64', 'incidence', mod_list, standardize = TRUE)
pred_perc_lessthan18 <- get_marginal_prediction(dat_cumulative, 'perc_lessthan18', 'incidence', mod_list, standardize = TRUE)
pred_care_home_beds <- get_marginal_prediction(dat_cumulative, 'care_home_beds', 'incidence', mod_list, standardize = TRUE)
pred_GISD_Score <- get_marginal_prediction(dat_cumulative, 'GISD_Score', 'incidence', mod_list, standardize = TRUE)
pred_pop_dens <- get_marginal_prediction(dat_cumulative, 'pop_dens', 'incidence', mod_list, standardize = TRUE)
pred_perc_service <- get_marginal_prediction(dat_cumulative, 'perc_service', 'incidence', mod_list, standardize = TRUE)
pred_perc_production <- get_marginal_prediction(dat_cumulative, 'perc_production', 'incidence', mod_list, standardize = TRUE)

plot_a_18to64 <- plot_marginal_prediction(pred_perc_18to64, 'perc_18to64', 'Incidence', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#377eb8', '#4daf4a', '#984ea3'))
plot_a_lessthan18 <- plot_marginal_prediction(pred_perc_lessthan18, 'perc_lessthan18', 'Incidence', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#377eb8', '#4daf4a', '#984ea3'))
plot_a_care_home_beds <- plot_marginal_prediction(pred_care_home_beds, 'care_home_beds', 'Incidence', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#377eb8', '#4daf4a', '#984ea3'))
plot_a_GISD_Score <- plot_marginal_prediction(pred_GISD_Score, 'GISD_Score', 'Incidence', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#377eb8', '#4daf4a', '#984ea3'))
plot_a_pop_dens <- plot_marginal_prediction(pred_pop_dens, 'pop_dens', 'Incidence', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#377eb8', '#4daf4a', '#984ea3'))
plot_a_service <- plot_marginal_prediction(pred_perc_service, 'perc_service', 'Incidence', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#377eb8', '#4daf4a', '#984ea3'))
plot_a_production <- plot_marginal_prediction(pred_perc_production, 'perc_production', 'Incidence', single_plot = TRUE, color_vals = c('#e41a1c', '#ff7f00', '#377eb8', '#4daf4a', '#984ea3'))

# grid.arrange(plot_a_GISD_Score, plot_a_service, plot_a_production,
#              plot_a_18to64, plot_a_lessthan18, plot_a_pop_dens, plot_a_care_home_beds, nrow = 2)

plot_a_GISD_Score <- plot_a_GISD_Score + labs(x = 'GISD', tag = 'A') + theme(plot.tag = element_text(size = 26), plot.tag.position = c(0.02, 0.98))
plot_a_service <- plot_a_service + labs(x = '% Service', tag = 'B') + theme(plot.tag = element_text(size = 26), plot.tag.position = c(0.02, 0.98))
plot_a_production <- plot_a_production + labs(x = '% Production', tag = 'C') + theme(plot.tag = element_text(size = 26), plot.tag.position = c(0.02, 0.98))
plot_a_18to64 <- plot_a_18to64 + labs(x = '% Aged 18-64', tag = 'D') + theme(plot.tag = element_text(size = 26), plot.tag.position = c(0.02, 0.98))
plot_a_lessthan18 <- plot_a_lessthan18 + labs(x = '% Aged <18', tag = 'E') + theme(plot.tag = element_text(size = 26), plot.tag.position = c(0.02, 0.98))
plot_a_pop_dens <- plot_a_pop_dens + labs(x = 'Population Density (100s / km2)', tag = 'F') + theme(plot.tag = element_text(size = 26), plot.tag.position = c(0.02, 0.98))
plot_a_care_home_beds <- plot_a_care_home_beds + labs(x = 'Care Home Beds per 10000 Pop', tag = 'G') + theme(plot.tag = element_text(size = 26), plot.tag.position = c(0.02, 0.98))

fig2 <- plot_a_GISD_Score + plot_a_service + plot_a_production + plot_a_18to64 + plot_a_lessthan18 +
  plot_a_pop_dens + plot_a_care_home_beds + plot_layout(nrow = 2)
ggsave('results/Figure2.svg', fig2, width = 20, height = 8)

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

mod_list <- list(n1_2a_full, n2a_full, n3a_full, n4a_full)
names(mod_list) <- c('1_2', '2', '3', '4')
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

# grid.arrange(plot_b_GISD_Score, plot_b_hosp_beds, plot_b_care_home_beds, nrow = 1)

plot_b_GISD_Score <- plot_b_GISD_Score + labs(x = 'GISD', tag = 'A') + theme(plot.tag = element_text(size = 26), plot.tag.position = c(0.02, 0.98))
plot_b_hosp_beds <- plot_b_hosp_beds + labs(x = 'Hospital Beds per 1000 Pop', tag = 'B') + theme(plot.tag = element_text(size = 26), plot.tag.position = c(0.02, 0.98))
plot_b_care_home_beds <- plot_b_care_home_beds + labs(x = 'Care Home Beds per 10000 Pop', tag = 'C') + theme(plot.tag = element_text(size = 26), plot.tag.position = c(0.02, 0.98))

fig3 <- plot_b_GISD_Score + plot_b_hosp_beds + plot_b_care_home_beds + plot_layout(nrow = 1)
ggsave('results/Figure3.svg', fig3, width = 15, height = 4)

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
plot_b_cases_rate <- plot_b_cases_rate <- plot_b_cases_rate + labs(x = 'Incidence per 10000 Pop')

plot_b_cases_pre <- plot_marginal_prediction(pred_cases_pre, 'cases_pre', 'CFR', single_plot = TRUE)
plot_b_vacc <- plot_marginal_prediction(pred_vacc, 'vacc', 'CFR', single_plot = FALSE)

print(plot_b_cases_rate)
print(plot_b_cases_pre)
print(plot_b_vacc)

ggsave('results/Figure4.svg', plot_b_cases_rate, width = 10.5, height = 4.25)

# ---------------------------------------------------------------------------------------------------------------------

### Explore underlying spatial patterns in fitted models ###

# Plot overall spatial pattern (no predictors):
spatial_trend_NULL <- dat_cumulative %>%
  select(lk, long, lat) %>%
  unique() %>%
  mutate(pop = 10000,
         cases_wave1 = 100,
         cases_wave2 = 100,
         cases_wave3 = 100,
         cases_wave4 = 100,
         cases_wave1_rate = mean(dat_cumulative$cases_wave1_rate),
         cases_wave1_1_rate = mean(dat_cumulative$cases_wave1_1_rate),
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
  mutate(fitted_n1_1a = predict(n1_1a, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n1_2a = predict(n1_2a, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n2a = predict(n2a, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n3a = predict(n3a, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n4a = predict(n4a, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n1b = predict(n1b, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n2b = predict(n2b, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n3b = predict(n3b, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n4b = predict(n4b, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n1_2a_adj = predict(n1_2a_adj, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n2a_adj = predict(n2a_adj, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n2b_adj = predict(n2b_adj, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n3a_adj = predict(n3a_adj, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n3b_adj = predict(n3b_adj, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n4a_adj = predict(n4a_adj, spatial_trend_NULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n4b_adj = predict(n4b_adj, spatial_trend_NULL, type = 'response'))#, exclude = 's(ags2)'))

map_fitted_NULL <- map_pan %>%
  left_join(spatial_trend_NULL %>%
              select(lk, fitted_n1_1a:fitted_n4b_adj),
            by = c('ARS' = 'lk'))
rm(spatial_trend_NULL)

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
         cases_wave1_1_rate = mean(dat_cumulative$cases_wave1_1_rate),
         cases_wave2_rate = mean(dat_cumulative$cases_wave2_rate),
         cases_wave3_rate = mean(dat_cumulative$cases_wave3_rate),
         cases_wave4_rate = mean(dat_cumulative$cases_wave4_rate),
         ags2 = '01',
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
  mutate(fitted_n1_1a = predict(n1_1a_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n1_2a = predict(n1_2a_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n2a = predict(n2a_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n3a = predict(n3a_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n4a = predict(n4a_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n1b = predict(n1b_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n2b = predict(n2b_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n3b = predict(n3b_full, spatial_trend_FULL, type = 'response'),#, exclude = 's(ags2)'),
         fitted_n4b = predict(n4b_full, spatial_trend_FULL, type = 'response'))#, exclude = 's(ags2)'))

map_fitted_FULL <- map_pan %>%
  left_join(spatial_trend_FULL %>%
              select(lk, fitted_n1_1a:fitted_n4b),
            by = c('ARS' = 'lk'))
rm(spatial_trend_FULL)

p1_1a <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n1_1a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(min(c(map_fitted_NULL$fitted_n1_1a, map_fitted_FULL$fitted_n1_1a)),
                                max(c(map_fitted_NULL$fitted_n1_1a, map_fitted_FULL$fitted_n1_1a)))) +
  theme_void() + labs(title = 'Wave 1_1', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p1_2a <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n1_2a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(min(c(map_fitted_NULL$fitted_n1_2a, map_fitted_FULL$fitted_n1_2a)),
                                max(c(map_fitted_NULL$fitted_n1_2a, map_fitted_FULL$fitted_n1_2a)))) +
  theme_void() + labs(title = 'Wave 1_2', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2a <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n2a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(breaks = c(250, 500),
                     limits = c(min(c(map_fitted_NULL$fitted_n2a, map_fitted_FULL$fitted_n2a)),
                                max(c(map_fitted_NULL$fitted_n2a, map_fitted_FULL$fitted_n2a)))) +
  theme_void() + labs(title = 'Wave 2', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p3a <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n3a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(breaks = c(150, 250),
                     limits = c(min(c(map_fitted_NULL$fitted_n3a, map_fitted_FULL$fitted_n3a)),
                                max(c(map_fitted_NULL$fitted_n3a, map_fitted_FULL$fitted_n3a)))) +
  theme_void() + labs(title = 'Wave 3', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p4a <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n4a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(breaks = c(250, 500, 800),
                     limits = c(min(c(map_fitted_NULL$fitted_n4a, map_fitted_FULL$fitted_n4a)),
                                max(c(map_fitted_NULL$fitted_n4a, map_fitted_FULL$fitted_n4a)))) +
  theme_void() + labs(title = 'Wave 4', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

p1b <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n1b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(min(c(map_fitted_NULL$fitted_n1b, map_fitted_FULL$fitted_n1b)),
                                max(c(map_fitted_NULL$fitted_n1b, map_fitted_FULL$fitted_n1b)))) +
  theme_void() + labs(title = 'Wave 1', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2b <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n2b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(min(c(map_fitted_NULL$fitted_n2b, map_fitted_FULL$fitted_n2b)),
                                max(c(map_fitted_NULL$fitted_n2b, map_fitted_FULL$fitted_n2b)))) +
  theme_void() + labs(title = 'Wave 2', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p3b <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n3b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(min(c(map_fitted_NULL$fitted_n3b, map_fitted_FULL$fitted_n3b)),
                                max(c(map_fitted_NULL$fitted_n3b, map_fitted_FULL$fitted_n3b)))) +
  theme_void() + labs(title = 'Wave 3', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p4b <- ggplot(map_fitted_NULL) + geom_sf(aes(fill = fitted_n4b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(min(c(map_fitted_NULL$fitted_n4b, map_fitted_FULL$fitted_n4b)),
                                max(c(map_fitted_NULL$fitted_n4b, map_fitted_FULL$fitted_n4b)))) +
  theme_void() + labs(title = 'Wave 4', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

layout_mat <- rbind(c(1, 2, 3, 3, 4, 4, 5, 5),
                    c(6, 6, 7, 7, 8, 8, 9, 9))
grid.arrange(p1_1a, p1_2a, p2a, p3a, p4a, p1b, p2b, p3b, p4b, layout_matrix = layout_mat)#ncol = 4)

p1_1a_full <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n1_1a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(min(c(map_fitted_NULL$fitted_n1_1a, map_fitted_FULL$fitted_n1_1a)),
                                max(c(map_fitted_NULL$fitted_n1_1a, map_fitted_FULL$fitted_n1_1a)))) +
  theme_void() + labs(title = 'Wave 1_1', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p1_2a_full <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n1_2a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(min(c(map_fitted_NULL$fitted_n1_2a, map_fitted_FULL$fitted_n1_2a)),
                                max(c(map_fitted_NULL$fitted_n1_2a, map_fitted_FULL$fitted_n1_2a)))) +
  theme_void() + labs(title = 'Wave 1_2', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2a_full <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n2a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(breaks = c(100, 300, 500),
                     limits = c(min(c(map_fitted_NULL$fitted_n2a, map_fitted_FULL$fitted_n2a)),
                                max(c(map_fitted_NULL$fitted_n2a, map_fitted_FULL$fitted_n2a)))) +
  theme_void() + labs(title = 'Wave 2', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p3a_full <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n3a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(breaks = c(150, 250),
                     limits = c(min(c(map_fitted_NULL$fitted_n3a, map_fitted_FULL$fitted_n3a)),
                                max(c(map_fitted_NULL$fitted_n3a, map_fitted_FULL$fitted_n3a)))) +
  theme_void() + labs(title = 'Wave 3', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p4a_full <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n4a), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(breaks = c(250, 500, 800),
                     limits = c(min(c(map_fitted_NULL$fitted_n4a, map_fitted_FULL$fitted_n4a)),
                                max(c(map_fitted_NULL$fitted_n4a, map_fitted_FULL$fitted_n4a)))) +
  theme_void() + labs(title = 'Wave 4', fill = 'Cases / 10000 Pop') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

p1b_full <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n1b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(min(c(map_fitted_NULL$fitted_n1b, map_fitted_FULL$fitted_n1b)),
                                max(c(map_fitted_NULL$fitted_n1b, map_fitted_FULL$fitted_n1b)))) +
  theme_void() + labs(title = 'Wave 1', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p2b_full <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n2b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(min(c(map_fitted_NULL$fitted_n2b, map_fitted_FULL$fitted_n2b)),
                                max(c(map_fitted_NULL$fitted_n2b, map_fitted_FULL$fitted_n2b)))) +
  theme_void() + labs(title = 'Wave 2', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p3b_full <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n3b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(min(c(map_fitted_NULL$fitted_n3b, map_fitted_FULL$fitted_n3b)),
                                max(c(map_fitted_NULL$fitted_n3b, map_fitted_FULL$fitted_n3b)))) +
  theme_void() + labs(title = 'Wave 3', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))
p4b_full <- ggplot(map_fitted_FULL) + geom_sf(aes(fill = fitted_n4b), col = 'black') +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  scale_fill_viridis(limits = c(min(c(map_fitted_NULL$fitted_n4b, map_fitted_FULL$fitted_n4b)),
                                max(c(map_fitted_NULL$fitted_n4b, map_fitted_FULL$fitted_n4b)))) +
  theme_void() + labs(title = 'Wave 4', fill = 'CFR (%)') +
  theme(legend.position = 'bottom', plot.title = element_text(size = 20),
        legend.title = element_text(size = 12), legend.text = element_text(size = 12))

grid.arrange(p1_1a_full, p1_2a_full, p2a_full, p3a_full, p4a_full,
             p1b_full, p2b_full, p3b_full, p4b_full, layout_matrix = layout_mat)#ncol = 4)

# Also plot side-by-side with spatial patterns unadjusted for demographic/socioeconomic/healthcare predictors, for each wave:
grid.arrange(p1_1a, p1_1a_full, nrow = 1)
grid.arrange(p1_2a, p1_2a_full, nrow = 1)
grid.arrange(p2a, p2a_full, nrow = 1)
grid.arrange(p3a, p3a_full, nrow = 1)
grid.arrange(p4a, p4a_full, nrow = 1)

grid.arrange(p1b, p1b_full, nrow = 1)
grid.arrange(p2b, p2b_full, nrow = 1)
grid.arrange(p3b, p3b_full, nrow = 1)
grid.arrange(p4b, p4b_full, nrow = 1)

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
