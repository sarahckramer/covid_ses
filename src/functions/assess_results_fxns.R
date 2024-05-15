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
  testSpatialAutocorrelation(sim_full, x = dat$long, y = dat$lat, plot = FALSE) %>% print()
  
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

get_marginal_prediction <- function(dat, pred_var, outcome_measure, mod_list, standardize = FALSE,
                                    partial_waves = FALSE, between_waves = FALSE) {
  # Function to get marginal predictions from a GAM
  # param dat: Data frame containing information on predictors and outcomes
  # param pred_var: The name of the predictor(s) for which marginal predictions are wanted
  # param outcome_var: The name of the column holding information on the RATE of the outcome
  # param mod_list: A named list of fitted GAMs used to make the predictions
  # param standardize: Boolean; should predictions be standardized to be on same scale for all waves?
  # param partial_waves: Boolean; are models fitted to partial (rather than full) waves?
  # param between_waves: Boolean: are models fitted to data between waves (rather than data during waves)?
  # returns: A tibble containing marginal predictions and 95% CIs
  
  # See code from Christensen et al. (2014) doi: 10.1098/rspb.2019.2269
  
  res_list <- vector('list', length(mod_list))
  pred_var_orig <- pred_var
  
  # Loop through all waves and get predictions:
  for (i in 1:length(mod_list)) {
    
    # Get wave number:
    wave <- as.numeric(names(mod_list)[i])
    if (is.na(wave)) {
      wave <- names(mod_list)[i]
    }
    
    # If we are looking at effect of incidence, prior incidence, or vaccination, need to change pred_var with wave:
    if (length(pred_var_orig) > 1) {
      
      if (pred_var_orig[1] == 'cases_rate') {
        pred_var[1] <- paste0('cases_summer', i, '_rate')
      }
      
    } else {
      
      if (pred_var_orig == 'cases_pre') {
        if (is.numeric(wave)) {
          pred_var <- paste0(pred_var_orig, wave, '_rate')
        } else {
          pred_var <- 'cases_wave1_1_rate'
        }
        
      } else if (pred_var_orig == 'vacc') {
        pred_var <- paste0(pred_var_orig, '_w', wave)
      } else if (pred_var_orig == 'vacc_reg') {
        pred_var <- paste0('vacc_w', wave, '_reg')
      } else if (pred_var_orig == 'cases_rate') {
        if (between_waves) {
          pred_var <- paste0('cases_summer', i, '_rate')
        } else {
          pred_var <- paste0('cases_wave', wave, '_rate')
        }
      }
      
    }
    
    # Get outcome variable:
    if (partial_waves) {
      
      if (outcome_measure == 'incidence') {
        outcome_var <- c('cases_wave1_1_rate', 'cases_wave1_2_rate', 'cases_wave2_1_rate', 'cases_wave2_2_rate',
                         'cases_wave3_1_rate', 'cases_wave3_2_rate', 'cases_wave4_1_rate', 'cases_wave4_2_rate')[i]
      } else if (outcome_measure == 'cfr') {
        outcome_var <- c('cfr_wave1_1', 'cfr_wave1_2', 'cfr_wave2_1', 'cfr_wave2_2',
                         'cfr_wave3_1', 'cfr_wave3_2', 'cfr_wave4_1', 'cfr_wave4_2')[i]
      } else {
        stop('Unrecognized outcome measure.')
      }
      
    } else if (between_waves) {
      
      if (outcome_measure == 'incidence') {
        outcome_var <- c('cases_summer1_rate', 'cases_summer2_rate')[i]
      } else if (outcome_measure == 'cfr') {
        outcome_var <- c('cfr_summer1', 'cfr_summer2')[i]
      } else {
        stop('Unrecognized outcome measure.')
      }
      
    } else {
      
      if (is.numeric(wave)) {
        if (outcome_measure == 'incidence') {
          outcome_var <- c('cases_wave1_rate', 'cases_wave2_rate', 'cases_wave3_rate', 'cases_wave4_rate', 'cases_wave5_rate')[wave]
        } else if (outcome_measure == 'cfr') {
          outcome_var <- c('cfr_wave1', 'cfr_wave2', 'cfr_wave3', 'cfr_wave4', 'cfr_wave5')[wave]
        } else {
          stop('Unrecognized outcome measure.')
        }
      } else {
        if (outcome_measure == 'incidence') {
          if (wave == '1_1') {
            outcome_var <- 'cases_wave1_1_rate'
          } else if (wave == '1_2') {
            outcome_var <- 'cases_wave1_2_rate'
          } else {
            stop('Unrecognized partial wave.')
          }
        } else {
          stop('Unrecognized outcome measure.')
        }
      }
      
    }
    
    # Get lat/long of LK with nearest to mean value of outcome rate:
    set_long_lat <- dat %>%
      rename('outcome' = all_of(outcome_var)) %>%
      mutate(dist = abs(outcome - mean(outcome))) %>%
      filter(dist == min(dist)) %>%
      select(long, lat)
    
    if (nrow(set_long_lat) > 1) {
      set_long <- set_long_lat$long[1]
      set_lat <- set_long_lat$lat[1]
    } else {
      set_long <- set_long_lat$long
      set_lat <- set_long_lat$lat
    }
    
    # Get data frame to be used for prediction:
    if (length(pred_var) > 1) {
      
      # Prepare data frame for prediction:
      pred_data <- with(dat,
                        expand_grid(var1 = seq(min(dat[, pred_var[1]]),
                                               max(dat[, pred_var[1]]),
                                               length.out = 100),
                                    var2 = seq(min(dat[, pred_var[2]]),
                                               max(dat[, pred_var[2]]),
                                               length.out = 100))) %>%
        mutate(pop = 10000,
               cases_wave1 = 100,
               cases_wave2 = 100,
               cases_wave3 = 100,
               cases_wave4 = 100,
               cases_wave5 = 100,
               cases_wave1_1 = 100,
               cases_wave1_2 = 100,
               cases_wave2_1 = 100,
               cases_wave2_2 = 100,
               cases_wave3_1 = 100,
               cases_wave3_2 = 100,
               cases_wave4_1 = 100,
               cases_wave4_2 = 100,
               cases_wave5_1 = 100,
               cases_wave5_2 = 100,
               cases_summer1 = 100,
               cases_summer2 = 100,
               cases_pre2_rate = mean(dat$cases_pre2_rate),
               cases_pre3_rate = mean(dat$cases_pre3_rate),
               cases_pre4_rate = mean(dat$cases_pre4_rate),
               cases_pre5_rate = mean(dat$cases_pre5_rate),
               cases_pre2_2_rate = mean(dat$cases_pre2_2_rate),
               cases_pre3_2_rate = mean(dat$cases_pre3_2_rate),
               cases_pre4_2_rate = mean(dat$cases_pre4_2_rate),
               cases_pre5_2_rate = mean(dat$cases_pre5_2_rate),
               cases_pre_summer2_rate = mean(dat$cases_pre_summer2_rate),
               cases_wave1_rate = mean(dat$cases_wave1_rate),
               cases_wave2_rate = mean(dat$cases_wave2_rate),
               cases_wave3_rate = mean(dat$cases_wave3_rate),
               cases_wave4_rate = mean(dat$cases_wave4_rate),
               cases_wave5_rate = mean(dat$cases_wave5_rate),
               cases_wave1_1_rate = mean(dat$cases_wave1_1_rate),
               cases_wave1_2_rate = mean(dat$cases_wave1_2_rate),
               cases_wave2_1_rate = mean(dat$cases_wave2_1_rate),
               cases_wave2_2_rate = mean(dat$cases_wave2_2_rate),
               cases_wave3_1_rate = mean(dat$cases_wave3_1_rate),
               cases_wave3_2_rate = mean(dat$cases_wave3_2_rate),
               cases_wave4_1_rate = mean(dat$cases_wave4_1_rate),
               cases_wave4_2_rate = mean(dat$cases_wave4_2_rate),
               cases_wave5_1_rate = mean(dat$cases_wave5_1_rate),
               cases_wave5_2_rate = mean(dat$cases_wave5_2_rate),
               cases_summer1_rate = mean(dat$cases_summer1_rate),
               cases_summer2_rate = mean(dat$cases_summer2_rate),
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
               perc_production = mean(dat$perc_production),
               vacc_w3 = mean(dat$vacc_w3),
               vacc_w4 = mean(dat$vacc_w4),
               vacc_w5 = mean(dat$vacc_w5),
               vacc_w3_1 = mean(dat$vacc_w3_1),
               vacc_w3_2 = mean(dat$vacc_w3_2),
               vacc_w4_1 = mean(dat$vacc_w4_1),
               vacc_w4_2 = mean(dat$vacc_w4_2),
               vacc_w5_1 = mean(dat$vacc_w5_1),
               vacc_w5_2 = mean(dat$vacc_w5_2),
               vacc_summer2 = mean(dat$vacc_summer2),
               vacc_w3_reg = mean(dat$vacc_w3_reg),
               vacc_w4_reg = mean(dat$vacc_w4_reg),
               vacc_w5_reg = mean(dat$vacc_w5_reg),
               vacc_w3_1_reg = mean(dat$vacc_w3_1_reg),
               vacc_w3_2_reg = mean(dat$vacc_w3_2_reg),
               vacc_w4_1_reg = mean(dat$vacc_w4_1_reg),
               vacc_w4_2_reg = mean(dat$vacc_w4_2_reg),
               vacc_w5_1_reg = mean(dat$vacc_w5_1_reg),
               vacc_w5_2_reg = mean(dat$vacc_w5_2_reg),
               vacc_summer2_reg = mean(dat$vacc_summer2_reg)) %>%
        select(-all_of(pred_var))
      
      # Give correct name to pred_var column:
      expect_true(names(pred_data)[1] == 'var1')
      expect_true(names(pred_data)[2] == 'var2')
      names(pred_data)[1] <- pred_var[1]
      names(pred_data)[2] <- pred_var[2]
      
    } else {
      
      # Prepare data frame for prediction:
      pred_data <- with(dat,
                        expand_grid(var = seq(min(dat[, pred_var]),
                                              max(dat[, pred_var]),
                                              length.out = 1000))) %>%
        mutate(pop = 10000,
               cases_wave1 = 100,
               cases_wave2 = 100,
               cases_wave3 = 100,
               cases_wave4 = 100,
               cases_wave5 = 100,
               cases_wave1_1 = 100,
               cases_wave1_2 = 100,
               cases_wave2_1 = 100,
               cases_wave2_2 = 100,
               cases_wave3_1 = 100,
               cases_wave3_2 = 100,
               cases_wave4_1 = 100,
               cases_wave4_2 = 100,
               cases_wave5_1 = 100,
               cases_wave5_2 = 100,
               cases_summer1 = 100,
               cases_summer2 = 100,
               cases_pre2_rate = mean(dat$cases_pre2_rate),
               cases_pre3_rate = mean(dat$cases_pre3_rate),
               cases_pre4_rate = mean(dat$cases_pre4_rate),
               cases_pre5_rate = mean(dat$cases_pre5_rate),
               cases_pre2_2_rate = mean(dat$cases_pre2_2_rate),
               cases_pre3_2_rate = mean(dat$cases_pre3_2_rate),
               cases_pre4_2_rate = mean(dat$cases_pre4_2_rate),
               cases_pre5_2_rate = mean(dat$cases_pre5_2_rate),
               cases_pre_summer2_rate = mean(dat$cases_pre_summer2_rate),
               cases_wave1_rate = mean(dat$cases_wave1_rate),
               cases_wave2_rate = mean(dat$cases_wave2_rate),
               cases_wave3_rate = mean(dat$cases_wave3_rate),
               cases_wave4_rate = mean(dat$cases_wave4_rate),
               cases_wave5_rate = mean(dat$cases_wave5_rate),
               cases_wave1_1_rate = mean(dat$cases_wave1_1_rate),
               cases_wave1_2_rate = mean(dat$cases_wave1_2_rate),
               cases_wave2_1_rate = mean(dat$cases_wave2_1_rate),
               cases_wave2_2_rate = mean(dat$cases_wave2_2_rate),
               cases_wave3_1_rate = mean(dat$cases_wave3_1_rate),
               cases_wave3_2_rate = mean(dat$cases_wave3_2_rate),
               cases_wave4_1_rate = mean(dat$cases_wave4_1_rate),
               cases_wave4_2_rate = mean(dat$cases_wave4_2_rate),
               cases_wave5_1_rate = mean(dat$cases_wave5_1_rate),
               cases_wave5_2_rate = mean(dat$cases_wave5_2_rate),
               cases_summer1_rate = mean(dat$cases_summer1_rate),
               cases_summer2_rate = mean(dat$cases_summer2_rate),
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
               perc_production = mean(dat$perc_production),
               vacc_w3 = mean(dat$vacc_w3),
               vacc_w4 = mean(dat$vacc_w4),
               vacc_w5 = mean(dat$vacc_w5),
               vacc_w3_1 = mean(dat$vacc_w3_1),
               vacc_w3_2 = mean(dat$vacc_w3_2),
               vacc_w4_1 = mean(dat$vacc_w4_1),
               vacc_w4_2 = mean(dat$vacc_w4_2),
               vacc_w5_1 = mean(dat$vacc_w5_1),
               vacc_w5_2 = mean(dat$vacc_w5_2),
               vacc_summer2 = mean(dat$vacc_summer2),
               vacc_w3_reg = mean(dat$vacc_w3_reg),
               vacc_w4_reg = mean(dat$vacc_w4_reg),
               vacc_w5_reg = mean(dat$vacc_w5_reg),
               vacc_w3_1_reg = mean(dat$vacc_w3_1_reg),
               vacc_w3_2_reg = mean(dat$vacc_w3_2_reg),
               vacc_w4_1_reg = mean(dat$vacc_w4_1_reg),
               vacc_w4_2_reg = mean(dat$vacc_w4_2_reg),
               vacc_w5_1_reg = mean(dat$vacc_w5_1_reg),
               vacc_w5_2_reg = mean(dat$vacc_w5_2_reg),
               vacc_summer2_reg = mean(dat$vacc_summer2_reg)) %>%
        select(-all_of(pred_var))
      
      # Give correct name to pred_var column:
      expect_true(names(pred_data)[1] == 'var')
      names(pred_data)[1] <- pred_var
      
    }
    
    # Get predictions and standard errors (link scale):
    pred_data_new <- pred_data %>%
      bind_cols(as.data.frame(predict(mod_list[[i]], pred_data, type = 'link',
                                      se.fit = TRUE)))#, exclude = 's(ags2)')))
    
    # Get inverse of link function:
    ilink <- family(mod_list[[i]])$linkinv
    
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
    
    # Add column with wave number:
    pred_data_new <- pred_data_new %>%
      mutate(wave = paste0('Wave ', wave))
    
    # Unify column names if looking at incidence, prior incidence, or vaccination:
    if (length(pred_var_orig) > 1) {
      
      if (pred_var_orig[1] == 'cases_rate') {
        names(pred_data_new)[1] <- 'cases_rate'
      }
      
    } else {
      
      if (pred_var_orig == 'cases_pre') {
        names(pred_data_new)[1] <- 'cases_pre'
      } else if (pred_var_orig == 'vacc' | pred_var_orig == 'vacc_reg') {
        names(pred_data_new)[1] <- 'vacc'
      } else if (pred_var_orig == 'cases_rate') {
        names(pred_data_new)[1] <- 'cases_rate'
      }
      
    }
    
    # Store results in list:
    res_list[[i]] <- pred_data_new
    
  }
  
  # Optional: Divide by mean so that plotting can occur on the same scale for all waves
  if (standardize) {
    res_list <- lapply(res_list, function(ix) {
      ix <- ix %>%
        mutate(lower = lower / mean(fitted),
               upper = upper / mean(fitted),
               fitted = fitted / mean(fitted))
      ix
    })
  }
  
  # Compile into single data frame:
  pred_res <- bind_rows(res_list)
  
  # Return predictions:
  return(pred_res)
}


plot_marginal_prediction <- function(pred_res, pred_var, outcome_lab, single_plot = TRUE, standardize = TRUE, which_waves = NULL, color_vals = NULL) {
  # Function to plot marginal predictions
  # param pred_res: Output from get_marginal_prediction
  # param pred_var: The predictor(s) of interest
  # param outcome_lab: String for labeling the y-axis
  # param single_plot: Boolean; should all waves be plotted on the same plot, or should faceting be used?
  # param standardize: Boolean; were predictions standardized to be on same scale for all waves?
  # param which_waves: If only certain waves should be plotted, specify them here; otherwise, set to NULL
  # returns: A plot of the marginal prediction, with 95% CI
  
  if (standardize) {
    outcome_lab <- paste0('Relative Change (', outcome_lab, ')')
  } else {
    outcome_lab <- paste(outcome_lab, '(Predicted)', sep = ' ')
  }
  
  if (length(pred_var) > 1) {
    
    dat_temp <- pred_res %>%
      rename('var1' = pred_var[1],
             'var2' = pred_var[2])
    
    dat_temp1 <- dat_temp %>%
      mutate(diff_median = abs(var2 - median(var2))) %>%
      filter((var2 == min(var2)) | (var2 == max(var2)) |
               (diff_median == min(diff_median)))# & var2 <= median(var2))) %>%
    if (length(unique(dat_temp1$var2)) > 3) {
      dat_temp1 <- dat_temp1 %>%
        filter(!(diff_median == min(diff_median) & var2 > median(var2))) %>%
        select(-diff_median) %>%
        mutate(var2 = factor(var2, levels = c(min(var2), median(var2), max(var2))))
    } else {
      dat_temp1 <- dat_temp1 %>%
        select(-diff_median) %>%
        mutate(var2 = factor(var2, levels = c(min(var2), median(var2), max(var2))))
    }
    levels(dat_temp1$var2) <- levels(dat_temp1$var2) %>% as.numeric() %>% round(2)
    
    dat_temp2 <- dat_temp %>%
      mutate(diff_median = abs(var1 - median(var1))) %>%
      filter((var1 == min(var1)) | (var1 == max(var1)) |
               (diff_median == min(diff_median)))# & var1 <= median(var1))) %>%
    if (length(unique(dat_temp2$var1)) > 3) {
      dat_temp2 <- dat_temp2 %>%
        filter(!(diff_median == min(diff_median) & var1 > median(var1))) %>%
        select(-diff_median) %>%
        mutate(var1 = factor(var1, levels = c(min(var1), median(var1), max(var1))))
    } else {
      dat_temp2 <- dat_temp2 %>%
        select(-diff_median) %>%
        mutate(var1 = factor(var1, levels = c(min(var1), median(var1), max(var1))))
    }
    levels(dat_temp2$var1) <- levels(dat_temp2$var1) %>% as.numeric() %>% round(2)
    
    p_temp1 <- ggplot(data = dat_temp1, aes(group = var2)) +
      geom_ribbon(aes(x = var1, ymin = lower, ymax = upper, fill = var2), alpha = 0.1) +
      geom_line(aes(x = var1, y = fitted, col = var2)) +
      facet_wrap(~ wave, nrow = 1) +
      theme_classic() +
      scale_color_brewer(palette = 'Set2') +
      scale_fill_brewer(palette = 'Set2') +
      labs(x = pred_var[1], y = outcome_lab,
           col = pred_var[2], fill = pred_var[2])
    p_temp2 <- ggplot(data = dat_temp2, aes(group = var1)) +
      geom_ribbon(aes(x = var2, ymin = lower, ymax = upper, fill = var1), alpha = 0.1) +
      geom_line(aes(x = var2, y = fitted, col = var1)) +
      facet_wrap(~ wave, nrow = 1) +
      theme_classic() +
      scale_color_brewer(palette = 'Set2') +
      scale_fill_brewer(palette = 'Set2') +
      labs(x = pred_var[2], y = outcome_lab,
           col = pred_var[1], fill = pred_var[1])
    
    if (!is.null(which_waves)) {
      
      if (length(which_waves) > 1) {
        p_temp1 = p_temp2 = vector('list', length(which_waves))
      }
      
      for (w in which_waves) {
        dat_temp1_w <- dat_temp1 %>%
          filter(str_detect(wave, paste0('Wave ', as.character(w))))
        dat_temp2_w <- dat_temp2 %>%
          filter(str_detect(wave, paste0('Wave ', as.character(w))))
        
        p_temp1_w <- ggplot(data = dat_temp1_w, aes(group = var2)) +
          geom_ribbon(aes(x = var1, ymin = lower, ymax = upper, fill = var2), alpha = 0.1) +
          geom_line(aes(x = var1, y = fitted, col = var2)) +
          theme_classic() +
          scale_color_brewer(palette = 'Set2') +
          scale_fill_brewer(palette = 'Set2') +
          labs(x = pred_var[1], y = outcome_lab,
               col = pred_var[2], fill = pred_var[2])
        p_temp2_w <- ggplot(data = dat_temp2_w, aes(group = var1)) +
          geom_ribbon(aes(x = var2, ymin = lower, ymax = upper, fill = var1), alpha = 0.1) +
          geom_line(aes(x = var2, y = fitted, col = var1)) +
          theme_classic() +
          scale_color_brewer(palette = 'Set2') +
          scale_fill_brewer(palette = 'Set2') +
          labs(x = pred_var[2], y = outcome_lab,
               col = pred_var[1], fill = pred_var[1])
        
        if (length(which_waves) > 1) {
          p_temp1[[which(which_waves == w)]] <- p_temp1_w
          p_temp2[[which(which_waves == w)]] <- p_temp2_w
        } else {
          p_temp1 <- p_temp1_w
          p_temp2 <- p_temp2_w
        }
        
      }
      
    }
    
    if (pred_var[1] == 'cases_rate') {
      return(p_temp1) # second plot not supported for variables whose values change each wave
    } else {
      return(list(p_temp1, p_temp2))
    }
    
  } else {
    
    dat_temp <- pred_res %>%
      rename('var' = pred_var)
    
    if (is.null(color_vals)) {
      color_vals <- c('#e41a1c', '#4daf4a', '#377eb8', '#984ea3', '#f781bf')
    }
    
    if (single_plot) {
      p_temp <- ggplot(data = dat_temp, aes(group = wave)) +
        geom_ribbon(aes(x = var, ymin = lower, ymax = upper, fill = wave), alpha = 0.1) +
        geom_line(aes(x = var, y = fitted, col = wave)) +
        theme_classic() +
        theme(legend.text = element_text(size = 14),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 14)) +
        # scale_color_viridis(discrete = TRUE) +
        # scale_fill_viridis(discrete = TRUE) +
        # scale_color_brewer(palette = 'Set1') +
        # scale_fill_brewer(palette = 'Set1') +
        scale_color_manual(values = color_vals) +
        scale_fill_manual(values = color_vals) +
        labs(x = pred_var, y = outcome_lab, fill = '', col = '')
      p_temp
    } else {
      p_temp <- ggplot(data = dat_temp, aes(group = wave)) +
        geom_ribbon(aes(x = var, ymin = lower, ymax = upper), fill = 'gray90') +
        geom_line(aes(x = var, y = fitted)) +
        facet_wrap(~ wave, nrow = 1, scales = 'free_x') +
        theme_classic() +
        theme(strip.text = element_text(size = 14),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 14)) +
        labs(x = pred_var, y = outcome_lab, fill = '', col = '')
      
    }
    
    return(p_temp)
    
  }
  
}
