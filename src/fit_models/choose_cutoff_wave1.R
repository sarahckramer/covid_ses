# ---------------------------------------------------------------------------------------------------------------------
# Determine a good cutoff for two parts of wave 1
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(sf)
library(testthat)
library(gridExtra)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format cumulative and covariate data:
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Load functions:
source('src/functions/assess_results_fxns.R')

# ---------------------------------------------------------------------------------------------------------------------

# Fit GAMs to partial waves with all possible cutoff weeks and compare fit

# Read in cumulative weekly data:
dat_inc_wk <- read_csv('data/formatted/STAND_weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')

# Add time column:
dat_inc_wk <- dat_inc_wk %>%
  mutate(Week = as.numeric(Week)) %>%
  mutate(time = if_else(Year == 2020, Week, Week + 53),
         time = if_else(Year == 2022, time + 52, time),
         .before = ags2)

# First wave runs from beginning of data (week 9) through week 20
# Loop through cutoff points 9-19 and fit GAMs:
plot_list <- vector('list', length = length(10:18))#length(9:19))
for (wk in 10:18) {#9:19) {
  
  # Print current cutoff week to show progress:
  print(wk)
  
  # Calculate total cases/incidence for each partial wave:
  dat_cases_temp <- dat_inc_wk %>%
    filter(time %in% c(wk, 20)) %>%
    select(time:lk, pop, cases) %>%
    mutate(time = if_else(time == 20, 'wk_20', 'wk_cutoff')) %>%
    pivot_wider(names_from = time, values_from = cases) %>%
    mutate(cases_wave1_1 = wk_cutoff,
           cases_wave1_2 = wk_20 - wk_cutoff) %>%
    select(ags2:pop, cases_wave1_1:cases_wave1_2) %>%
    mutate(cases_wave1_1_rate = cases_wave1_1 / pop * 10000,
           cases_wave1_2_rate = cases_wave1_2 / pop * 10000)
  
  # Join with relevant predictors:
  dat_cases_temp <- dat_cases_temp %>%
    inner_join(dat_cumulative %>%
                 select(lk, long:lat, hosp_beds:TS_Arbeitswelt_adj),
               by = 'lk') %>%
    mutate(ags2 = factor(ags2))
  
  # Fit models:
  n1_1a_full <- gam(cases_wave1_1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                      s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) +
                      s(perc_service) + s(perc_production) +
                      offset(log(pop)), data = dat_cases_temp, family = 'nb', method = 'ML')
  n1_2a_full <- gam(cases_wave1_2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                      s(perc_18to64) + s(perc_lessthan18) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) +
                      s(perc_service) + s(perc_production) + s(cases_wave1_1_rate) +
                      offset(log(pop)), data = dat_cases_temp, family = 'nb', method = 'ML')
  
  # Assess model fit:
  print(paste(summary(n1_1a_full)$dev.expl, summary(n1_2a_full)$dev.expl, summary(n1_1a_full)$dev.expl + summary(n1_2a_full)$dev.expl, sep = '  '))
  
  mod_list <- list(n1_1a_full, n1_2a_full)
  names(mod_list) <- c('1_1', '1_2')
  
  pred_a_GISD_Score <- get_marginal_prediction(dat_cumulative, 'GISD_Score', 'incidence', mod_list,
                                               standardize = TRUE, partial_waves = TRUE)
  
  p_temp <- ggplot(data = pred_a_GISD_Score, aes(group = wave)) +
    geom_ribbon(aes(x = GISD_Score, ymin = lower, ymax = upper, fill = wave), alpha = 0.1) +
    geom_line(aes(x = GISD_Score, y = fitted, col = wave)) +
    theme_classic() +
    scale_color_brewer(palette = 'Set1') + scale_fill_brewer(palette = 'Set1') +
    labs(title = paste0('Cutoff: Week ', wk), x = 'Predicted Change in Incidence', y = 'Cases / 10000 Pop', fill = '', col = '')
  plot_list[[wk - 9]] <- p_temp
  
}

# Plot relationships for all cutoff weeks:
do.call('grid.arrange', c(plot_list, nrow = 3))
