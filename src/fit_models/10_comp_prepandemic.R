# ---------------------------------------------------------------------------------------------------------------------
# Fit GAMs of pre-pandemic all-cause mortality, to determine how associations compare to main findings
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(sf)
library(spdep)
library(testthat)
library(DHARMa)
library(gridExtra)
library(viridis)

# ---------------------------------------------------------------------------------------------------------------------

# Functions

# Functions to plot results (main analysis): 
source('src/functions/assess_results_fxns.R')

# Functions to plot results (pre-pandemic analysis):
get_marginal_prediction_pre <- function(dat, pred_var, mod, standardize = FALSE) {
  
  # Get outcome variable:
  outcome_var <- 'deaths'
  
  # Get lat/long of LK with nearest to mean value of outcome rate:
  set_long_lat <- dat %>%
    mutate(dist = abs(deaths - mean(deaths, na.rm = TRUE))) %>%
    filter(dist == min(dist, na.rm = TRUE)) %>%
    select(long, lat)
  
  if (nrow(set_long_lat) > 1) {
    set_long <- set_long_lat$long[1]
    set_lat <- set_long_lat$lat[1]
  } else {
    set_long <- set_long_lat$long
    set_lat <- set_long_lat$lat
  }
  
  # Prepare data frame for prediction:
  pred_data <- with(dat,
                    expand_grid(var = seq(min(dat[, pred_var]),
                                          max(dat[, pred_var]),
                                          length.out = 1000))) %>%
    mutate(pop = 10000,
           ags2 = '01',
           long = set_long,
           lat = set_lat,
           perc_18to64 = mean(dat$perc_18to64),
           perc_lessthan18 = mean(dat$perc_lessthan18),
           hosp_beds = mean(dat$hosp_beds),
           care_home_beds = mean(dat$care_home_beds),
           GISD_Score = mean(dat$GISD_Score),
           pop_dens = mean(dat$pop_dens),
           living_area = mean(dat$living_area),
           perc_service = mean(dat$perc_service),
           perc_production = mean(dat$perc_production)) %>%
    select(-all_of(pred_var))
  
  # Give correct name to pred_var column:
  expect_true(names(pred_data)[1] == 'var')
  names(pred_data)[1] <- pred_var
  
  # Get predictions and standard errors (link scale):
  pred_data_new <- pred_data %>%
    bind_cols(as.data.frame(predict(mod, pred_data, type = 'link',
                                    se.fit = TRUE, exclude = 's(ags2)')))
  
  # Get inverse of link function:
  ilink <- family(mod)$linkinv
  
  # Limit to columns of interest:
  pred_data_new <- pred_data_new %>%
    select(all_of(pred_var), fit:se.fit)
  
  # Transform predictions to get predicted counts and 95% CIs:
  pred_data_new <- pred_data_new %>%
    mutate(fitted = ilink(fit),
           lower = ilink(fit - (2 * se.fit)),
           upper = ilink(fit + (2 * se.fit))) %>%
    select(-c(fit:se.fit))
  
  # How many x larger is largest predicted value than smallest?:
  print(max(pred_data_new$fitted) / min(pred_data_new$fitted))
  
  # Optional: Divide by mean so that plotting can occur on the same scale for all years
  if (standardize) {
    
    pred_data_new <- pred_data_new %>%
      mutate(lower = lower / mean(fitted),
             upper = upper / mean(fitted),
             fitted = fitted / mean(fitted))
    
  }
  
  # Return predictions:
  return(pred_data_new)
  
}

plot_marginal_prediction_pre <- function(pred_res, pred_var, standardize = TRUE) {
  
  if (standardize) {
    outcome_lab <- 'Relative Change in Mortality'
  } else {
    outcome_lab <- 'Predicted Mortality'
  }
  
  pred_res <- pred_res %>%
    rename('var' = all_of(pred_var))
  
  color_vals <- viridis(n = 6, option = 'G')[1:5]
  
  p_temp <- ggplot(data = pred_res %>% mutate(year = as.character(year)),
                   aes(group = year)) +
    geom_ribbon(aes(x = var, ymin = lower, ymax = upper, fill = year), alpha = 0.1) +
    geom_line(aes(x = var, y = fitted, col = year)) +
    theme_classic() +
    theme(legend.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14)) +
    # scale_color_viridis(discrete = TRUE, option = 'G') +
    # scale_fill_viridis(discrete = TRUE, option = 'G') +
    scale_color_manual(values = color_vals) +
    scale_fill_manual(values = color_vals) +
    labs(x = pred_var, y = outcome_lab, fill = '', col = '')
  p_temp
  
  return(p_temp)
  
}

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data

# Read in predictor data:
source('src/functions/load_data.R')

# Read in mortality data:
inkar_dat <- read_csv2('data/raw/revision1/excess_deaths/inkar_LK_death_rate_by_year.csv')

# Format:
names(inkar_dat)[4:11] <- paste0('Y', 2015:2022)
inkar_dat <- inkar_dat[-1, ]

inkar_dat <- inkar_dat %>%
  select(-Aggregat) %>%
  pivot_longer(Y2015:Y2022, names_to = 'year', values_to = 'mortality') %>%
  mutate(year = as.numeric(str_sub(year, 2, 5)))

# Get population counts:
pop_dat <- read_csv2('data/raw/revision1/inkar_LK_pop_sizes.csv')
names(pop_dat)[4:9] <- paste0('Y', 2015:2020)
pop_dat <- pop_dat[-1, ]

pop_dat <- pop_dat %>%
  select(-c(Raumeinheit, Aggregat)) %>%
  pivot_longer(Y2015:Y2020, names_to = 'year', values_to = 'pop') %>%
  mutate(year = as.numeric(str_sub(year, 2, 5)))

# Combine and convert to total number of deaths
inkar_dat <- inkar_dat %>%
  right_join(pop_dat, by = c('Kennziffer', 'year')) %>%
  mutate(deaths = mortality * (pop / 1000))

# Join predictor information:
inkar_dat <- inkar_dat %>%
  rename('lk' = 'Kennziffer') %>%
  left_join(dat_cumulative %>% select(lk, ags2, long, lat, hosp_beds:GISD_Score),
            by = 'lk')

# Drop NAs:
inkar_dat <- inkar_dat %>% drop_na()

# ---------------------------------------------------------------------------------------------------------------------

# Fit models

# Fit:
n15 <- gam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
             s(perc_18to64) + s(perc_lessthan18) + s(perc_service) + s(perc_production) +
             s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
             offset(log(pop)), data = inkar_dat %>% filter(year == 2015), family = 'nb', method = 'ML')
n16 <- gam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
             s(perc_18to64) + s(perc_lessthan18) + s(perc_service) + s(perc_production) +
             s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
             offset(log(pop)), data = inkar_dat %>% filter(year == 2016), family = 'nb', method = 'ML')
n17 <- gam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
             s(perc_18to64) + s(perc_lessthan18) + s(perc_service) + s(perc_production) +
             s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
             offset(log(pop)), data = inkar_dat %>% filter(year == 2017), family = 'nb', method = 'ML')
n18 <- gam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
             s(perc_18to64) + s(perc_lessthan18) + s(perc_service) + s(perc_production) +
             s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
             offset(log(pop)), data = inkar_dat %>% filter(year == 2018), family = 'nb', method = 'ML')
n19 <- gam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
             s(perc_18to64) + s(perc_lessthan18) + s(perc_service) + s(perc_production) +
             s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
             offset(log(pop)), data = inkar_dat %>% filter(year == 2019), family = 'nb', method = 'ML')

# Check with DHARMa:
par(mfrow = c(1, 3))
for (mod in list(n15, n16, n17, n18, n19)) {
  
  simulateResiduals(mod) %>% testQuantiles() %>% print()
  simulateResiduals(mod) %>% testDispersion() %>% print()
  simulateResiduals(mod) %>% testUniformity() %>% print()
  
}

# Check Moran's I:
resid_dat <- bind_rows(inkar_dat %>% filter(year == 2015) %>% mutate(resid = residuals(n15, type = 'deviance')),
                       inkar_dat %>% filter(year == 2016) %>% mutate(resid = residuals(n16, type = 'deviance')),
                       inkar_dat %>% filter(year == 2017) %>% mutate(resid = residuals(n17, type = 'deviance')),
                       inkar_dat %>% filter(year == 2018) %>% mutate(resid = residuals(n18, type = 'deviance')),
                       inkar_dat %>% filter(year == 2019) %>% mutate(resid = residuals(n19, type = 'deviance'))) %>%
  select(lk, year, resid) %>% pivot_wider(names_from = 'year', names_prefix = 'y', values_from = 'resid')

map_base <- st_read(dsn = 'data/raw/map/vg2500_12-31.gk3.shape/vg2500/VG2500_KRS.shp')
map_base <- map_base %>%
  left_join(resid_dat,
            by = c('ARS' = 'lk')) %>%
  drop_na()

nb <- spdep::poly2nb(map_base, row.names = map_base$ARS)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

lw <- nb2listw(nb, style = 'W', zero.policy = FALSE)

moran.mc(map_base$y2015, lw, nsim = 999)
moran.mc(map_base$y2016, lw, nsim = 999)
moran.mc(map_base$y2017, lw, nsim = 999)
moran.mc(map_base$y2018, lw, nsim = 999)
moran.mc(map_base$y2019, lw, nsim = 999)

# ---------------------------------------------------------------------------------------------------------------------

# Plot and compare to main findings

# Get and plot results for association between GISD and pre-pandemic, all-cause mortality:
pred_GISD_n15 <- get_marginal_prediction_pre(inkar_dat %>% filter(year == 2015), 'GISD_Score', n15, standardize = TRUE) %>% mutate(year = 2015)
pred_GISD_n16 <- get_marginal_prediction_pre(inkar_dat %>% filter(year == 2016), 'GISD_Score', n16, standardize = TRUE) %>% mutate(year = 2016)
pred_GISD_n17 <- get_marginal_prediction_pre(inkar_dat %>% filter(year == 2017), 'GISD_Score', n17, standardize = TRUE) %>% mutate(year = 2017)
pred_GISD_n18 <- get_marginal_prediction_pre(inkar_dat %>% filter(year == 2018), 'GISD_Score', n18, standardize = TRUE) %>% mutate(year = 2018)
pred_GISD_n19 <- get_marginal_prediction_pre(inkar_dat %>% filter(year == 2019), 'GISD_Score', n19, standardize = TRUE) %>% mutate(year = 2019)

pred_GISD <- bind_rows(pred_GISD_n15, pred_GISD_n16, pred_GISD_n17, pred_GISD_n18, pred_GISD_n19)
p_GISD <- plot_marginal_prediction_pre(pred_GISD, 'GISD_Score', standardize = TRUE) + labs(x = 'GISD')
# ggsave('results/FigureS12.svg', p_GISD, width = 6.0, height = 4.0)

# Plot results alongside main text findings:
n1_1a_full <- read_rds('results/fitted_models/FULL_n1_1a_ml.rds')
n1_2a_full <- read_rds('results/fitted_models/FULL_n1_2a_ml.rds')
n1_1b_full <- read_rds('results/fitted_models/FULL_n1_1b_ml.rds')
n1_2b_full <- read_rds('results/fitted_models/FULL_n1_2b_ml.rds')
n2a_full <- read_rds('results/fitted_models/FULL_n2a_ml.rds')
n2b_full <- read_rds('results/fitted_models/FULL_n2b_ml.rds')
n3a_full <- read_rds('results/fitted_models/FULL_n3a_ml.rds')
n3b_full <- read_rds('results/fitted_models/FULL_n3b_ml.rds')
n4a_full <- read_rds('results/fitted_models/FULL_n4a_ml.rds')
n4b_full <- read_rds('results/fitted_models/FULL_n4b_ml.rds')
n5a_full <- read_rds('results/fitted_models/FULL_n5a_ml.rds')
n5b_full <- read_rds('results/fitted_models/FULL_n5b_ml.rds')

mod_list <- list(n1_1a_full, n1_2a_full, n2a_full, n3a_full, n4a_full, n5a_full)
names(mod_list) <- c('1_1', '1_2', '2', '3', '4', '5')
pred_GISD_Score_inc <- get_marginal_prediction(dat_cumulative, 'GISD_Score', 'incidence', mod_list, standardize = TRUE)

mod_list <- list(n1_1b_full, n1_2b_full, n2b_full, n3b_full, n4b_full, n5b_full)
names(mod_list) <- c('1_1', '1_2', '2', '3', '4', '5')
pred_GISD_Score_cfr <- get_marginal_prediction(dat_cumulative, 'GISD_Score', 'cfr', mod_list, standardize = TRUE)

p_comp_inc <- ggplot() +
  geom_ribbon(data = pred_GISD_Score_inc, aes(x = GISD_Score, ymin = lower, ymax = upper, group = wave, fill = wave), alpha = 0.1) +
  geom_line(data = pred_GISD_Score_inc, aes(x = GISD_Score, y = fitted, group = wave, col = wave), lty = 2) +
  geom_ribbon(data = pred_GISD, aes(x = GISD_Score, ymin = lower, ymax = upper, group = year), alpha = 0.1) +
  geom_line(data = pred_GISD, aes(x = GISD_Score, y = fitted, group = year)) +
  theme_classic() +
  scale_color_manual(values = c('#e41a1c', '#ff7f00', '#4daf4a', '#377eb8', '#984ea3', '#f781bf')) +
  scale_fill_manual(values = c('#e41a1c', '#ff7f00', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'))
p_comp_cfr <- ggplot() +
  geom_ribbon(data = pred_GISD_Score_cfr, aes(x = GISD_Score, ymin = lower, ymax = upper, group = wave, fill = wave), alpha = 0.1) +
  geom_line(data = pred_GISD_Score_cfr, aes(x = GISD_Score, y = fitted, group = wave, col = wave), lty = 2) +
  geom_ribbon(data = pred_GISD, aes(x = GISD_Score, ymin = lower, ymax = upper, group = year), alpha = 0.1) +
  geom_line(data = pred_GISD, aes(x = GISD_Score, y = fitted, group = year)) +
  theme_classic() +
  scale_color_manual(values = c('#e41a1c', '#ff7f00', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'))
grid.arrange(p_comp_inc, p_comp_cfr, nrow = 1)
