# ---------------------------------------------------------------------------------------------------------------------
# Functions to process and assess model results
# ---------------------------------------------------------------------------------------------------------------------


check_dharma <- function(dat, obs, mod, disp) {
  # Function to check residuals from negative binomial GAM using various tests
  # param dat: Cumulative data (tibble)
  # param obs: Observed values of the dependent variable
  # param mod: Fitted model (gam)
  # param disp: Dispersion parameter from fitted model
  
  mus <- predict(mod, type = 'response')
  sim_resp <- replicate(1000, rnbinom(n = nrow(dat),
                                      size = disp,
                                      mu = mus))
  sim_full <- createDHARMa(simulatedResponse = sim_resp,
                           observedResponse = obs,
                           fittedPredictedResponse = predict(mod, type = 'response'),
                           integerResponse = TRUE)
  
  plot(sim_full)
  testOutliers(sim_full, type = 'bootstrap') %>% print()
  testDispersion(sim_full) %>% print()
  testZeroInflation(sim_full) %>% print()
  testUniformity(sim_full) %>% print()
  testSpatialAutocorrelation(sim_full, x = dat$long, y = dat$lat) %>% print()
}
