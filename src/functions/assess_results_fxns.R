# ---------------------------------------------------------------------------------------------------------------------
# Functions to process and assess model results
# ---------------------------------------------------------------------------------------------------------------------


check_dharma <- function(dat, mod, depend) {
  # Function to check residuals from negative binomial GAM using various tests
  # param dat: Cumulative data (tibble)
  # param mod: Fitted model (gam)
  # param depend: Is fitted model fit to cases or deaths?
  
  sim_full <- simulateResiduals(mod)
  
  # plot(sim_full)
  testQuantiles(sim_full) %>% print()
  # testOutliers(sim_full, type = 'bootstrap') %>% print()
  testDispersion(sim_full) %>% print()
  # testZeroInflation(sim_full) %>% print()
  testUniformity(sim_full) %>% print()
  testSpatialAutocorrelation(sim_full, x = dat$long, y = dat$lat) %>% print()
  
  plotResiduals(sim_full, dat$lat, main = 'lat')
  plotResiduals(sim_full, dat$long, main = 'long')

  if (depend == 'cases') {
    plotResiduals(sim_full, dat$perc_18to64, main = 'perc_18to64')
    plotResiduals(sim_full, dat$care_home_beds, main = 'care_home_beds')
    plotResiduals(sim_full, dat$GISD_Score, main = 'GISD_Score')
    plotResiduals(sim_full, dat$pop_dens, main = 'pop_dens')
    # plotResiduals(sim_full, dat$living_area, main = 'living_area')
    plotResiduals(sim_full, dat$perc_service, main = 'perc_service')
    plotResiduals(sim_full, dat$perc_production, main = 'perc_production')
  } else if (depend == 'deaths') {
    plotResiduals(sim_full, dat$care_home_beds, main = 'care_home_beds')
    plotResiduals(sim_full, dat$hosp_beds, main = 'hosp_beds')
    plotResiduals(sim_full, dat$GISD_Score, main = 'GISD_Score')
    plotResiduals(sim_full, dat$pop_dens, main = 'pop_dens')
  }
  
}


get_marginal_prediction <- function(dat, outcome_var, pred_var, mod) {
  # Function to get marginal predictions from a GAM
  # param dat: Data frame containing information on predictors and outcomes
  # param outcome_var: The name of the column holding information on the RATE of the outcome
  # param pred_var: The name of the predictor for which marginal predictions are wanted
  # param mod: The fitted GAM used to make the predictions
  # returns: A tibble containing marginal predictions and 95% CIs
  
  # See code from Christensen et al. (2014) doi: 10.1098/rspb.2019.2269
  
  # Get lat/long of LK with nearest to mean value of outcome rate:
  set_long_lat <- dat %>%
    rename('outcome' = all_of(outcome_var)) %>%
    mutate(dist = abs(outcome - mean(outcome))) %>%
    filter(dist == min(dist)) %>%
    select(long, lat)
  set_long <- set_long_lat$long
  set_lat <- set_long_lat$lat
  
  # Prepare data frame for prediction:
  pred_data <- with(dat,
                    expand_grid(var = seq(min(dat_cumulative[, pred_var]),
                                          max(dat_cumulative[, pred_var]),
                                          length.out = 1000))) %>%
    mutate(pop = 10000,
           cases_wave1 = 100,
           cases_wave2 = 100,
           cases_pre_rate = mean(dat_cumulative$cases_pre_rate),
           ags2 = '01',
           long = set_long,
           lat = set_lat,
           perc_18to64 = mean(dat_cumulative$perc_18to64),
           hosp_beds = mean(dat_cumulative$hosp_beds),
           care_home_beds = mean(dat_cumulative$care_home_beds),
           GISD_Score = mean(dat_cumulative$GISD_Score),
           pop_dens = mean(dat_cumulative$pop_dens),
           living_area = mean(dat_cumulative$living_area),
           perc_service = mean(dat_cumulative$perc_service),
           perc_production = mean(dat_cumulative$perc_production)) %>%
    select(-all_of(pred_var))
  
  # Give correct name to pred_var column:
  expect_true(names(pred_data)[1] == 'var')
  names(pred_data)[1] <- pred_var
  
  # Get predictions and standard errors (link scale):
  pred_data <- pred_data %>%
    bind_cols(as.data.frame(predict(mod, pred_data, type = 'link', se.fit = TRUE)))
  
  # Limit to columns of interest:
  pred_data <- pred_data %>%
    select(all_of(pred_var), fit:se.fit)
  
  # Get inverse of link function:
  ilink <- family(mod)$linkinv
  
  # Transform predictions to get predicted counts and 95% CIs:
  pred_data <- pred_data %>%
    mutate(fitted = ilink(fit),
           lower = ilink(fit - (2 * se.fit)),
           upper = ilink(fit + (2 * se.fit))) %>%
    select(-c(fit:se.fit))
  
  # How many x larger is largest predicted value than smallest?:
  print(max(pred_data$fitted) / min(pred_data$fitted))
  
  # Return predictions:
  return(pred_data)
}


plot_marginal_prediction <- function(pred_dat, pred_var, outcome_lab) {
  # Function to plot marginal predictions
  # param pred_dat: Output from get_marginal_prediction
  # param pred_var: The predictor of interest
  # param outcome_lab: String for labeling the y-axis
  # returns: A plot of the marginal prediction, with 95% CI
  
  dat_temp <- pred_dat %>%
    rename('var' = pred_var)
  
  p_temp <- ggplot(data = dat_temp) + 
    geom_ribbon(aes(x = var, ymin = lower, ymax = upper), fill = 'gray90') +
    geom_line(aes(x = var, y = fitted)) +
    theme_classic() +
    labs(x = pred_var, y = paste(outcome_lab, '(Predicted)', sep = ' '))
  
  return(p_temp)
}
