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
    plotResiduals(sim_full, dat$living_area, main = 'living_area')
    plotResiduals(sim_full, dat$perc_service, main = 'perc_service')
    plotResiduals(sim_full, dat$perc_production, main = 'perc_production')
  } else if (depend == 'deaths') {
    plotResiduals(sim_full, dat$care_home_beds, main = 'care_home_beds')
    plotResiduals(sim_full, dat$hosp_beds, main = 'hosp_beds')
    plotResiduals(sim_full, dat$GISD_Score, main = 'GISD_Score')
    plotResiduals(sim_full, dat$pop_dens, main = 'pop_dens')
  }
  
}
