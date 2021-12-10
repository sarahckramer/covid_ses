# ---------------------------------------------------------------------------------------------------------------------
# Run and assess multivariable GAMs exploring the role of various predictors
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(ggeffects)
library(sf)
library(testthat)
library(spdep)
library(viridis)
library(gridExtra)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data:
source('src/functions/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Fit models with chosen k values

# Wave 1:
n1a_full <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(living_area, k = 25) +
                  s(perc_service) + s(perc_production) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb')
n1b_full <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(living_area) +
                  s(perc_service) + s(perc_production) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb')
n1c_full <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + #s(pop_dens) +
                  offset(log(cases_wave1)), data = dat_cumulative, family = 'nb')

# Wave 2:
n2a_full <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) + s(living_area) +
                  s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb')
n2b_full <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(living_area) +
                  s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb')
n2c_full <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + #s(pop_dens) +
                  s(cases_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb')

# Quick check of fits:
par(mfrow = c(2, 2))
gam.check(n1a_full, rep = 50)
gam.check(n1b_full, rep = 50)
gam.check(n1c_full, rep = 50)
gam.check(n2a_full, rep = 50)
gam.check(n2b_full, rep = 50)
gam.check(n2c_full, rep = 50)

# ---------------------------------------------------------------------------------------------------------------------

# Explore potential model improvements

# Should living_area be included?:
n1a_comp <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb')
n1b_comp <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb')

n2a_comp <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb')
n2b_comp <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) + s(deaths_pre_rate) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb')

n1a_comp_alt <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                      s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(living_area, k = 25) +
                      s(perc_service) + s(perc_production) +
                      offset(log(pop)), data = dat_cumulative, family = 'nb')
n1b_comp_alt <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                      s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(living_area) +
                      s(perc_service) + s(perc_production) +
                      offset(log(pop)), data = dat_cumulative, family = 'nb')

n2a_comp_alt <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                      s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(living_area) +
                      s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                      offset(log(pop)), data = dat_cumulative, family = 'nb')
n2b_comp_alt <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                      s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(living_area) +
                      s(perc_service) + s(perc_production) + s(deaths_pre_rate) +
                      offset(log(pop)), data = dat_cumulative, family = 'nb')

par(mfrow = c(2, 2))
gam.check(n1a_comp, rep = 50)
gam.check(n1b_comp, rep = 50)
gam.check(n2a_comp, rep = 50)
gam.check(n2b_comp, rep = 50)

AIC(n1a_full, n1a_comp)
BIC(n1a_full, n1a_comp)
AIC(n1b_full, n1b_comp)
BIC(n1b_full, n1b_comp)

AIC(n2a_full, n2a_comp)
BIC(n2a_full, n2a_comp)
AIC(n2b_full, n2b_comp)
BIC(n2b_full, n2b_comp)

AIC(n1a_comp, n1a_comp_alt)
BIC(n1a_comp, n1a_comp_alt)
AIC(n1b_comp, n1b_comp_alt)
BIC(n1b_comp, n1b_comp_alt)

AIC(n2a_comp, n2a_comp_alt)
BIC(n2a_comp, n2a_comp_alt)
AIC(n2b_comp, n2b_comp_alt)
BIC(n2b_comp, n2b_comp_alt)

# Try using MRF:
dat_cumulative$ARS <- factor(dat_cumulative$lk)
n2a_mrf <- gam(cases_wave2 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 100) + s(ags2, bs = 're', k = 16) +
                 s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) + s(living_area) +
                 s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                 offset(log(pop)), data = dat_cumulative, family = 'nb')
n2b_mrf <- gam(deaths_wave2 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 100) + s(ags2, bs = 're', k = 16) +
                 s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(living_area) +
                 s(perc_service) + s(perc_production) + s(deaths_pre_rate) +
                 offset(log(pop)), data = dat_cumulative, family = 'nb')
n2c_mrf <- gam(deaths_wave2 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 100) + s(ags2, bs = 're', k = 16) +
                 s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + #s(pop_dens) +
                 s(deaths_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb')

AIC(n2a_full, n2a_mrf)
BIC(n2a_full, n2a_mrf)

AIC(n2b_full, n2b_mrf)
BIC(n2b_full, n2b_mrf)

AIC(n2c_full, n2c_mrf)
BIC(n2c_full, n2c_mrf)

# Compare with poisson/zero-inflated:
n1a_pois <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(living_area, k = 25) +
                  s(perc_service) + s(perc_production) +
                  offset(log(pop)), data = dat_cumulative, family = 'poisson')
n1b_pois <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(living_area) + s(perc_service) + s(perc_production) +
                  offset(log(pop)), data = dat_cumulative, family = 'poisson')
n1c_pois <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) +# s(pop_dens) +
                  offset(log(cases_wave1)), data = dat_cumulative, family = 'poisson')

n2a_pois <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) + s(living_area) +
                  s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                  offset(log(pop)), data = dat_cumulative, family = 'poisson')
n2b_pois <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(living_area) + s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                  offset(log(pop)), data = dat_cumulative, family = 'poisson')
n2c_pois <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) +# s(pop_dens) +
                  s(cases_pre_rate) +
                  offset(log(cases_wave2)), data = dat_cumulative, family = 'poisson')

n1b_zip <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                 s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                 s(living_area) + s(perc_service) + s(perc_production) +
                 offset(log(pop)), data = dat_cumulative, family = 'ziP')
n1c_zip <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 100) + s(ags2, bs = 're', k = 16) +
                 s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) +# s(pop_dens) +
                 offset(log(cases_wave1)), data = dat_cumulative, family = 'ziP')

par(mfrow = c(2, 2))
gam.check(n2a_pois, rep = 50)
gam.check(n2b_pois, rep = 50)
gam.check(n2c_pois, rep = 50)
gam.check(n2a_zip, rep = 50)
gam.check(n2b_zip, rep = 50)
gam.check(n2c_zip, rep = 50)

BIC(n1a_full, n1a_pois)
BIC(n1b_full, n1b_pois, n1b_zip)
BIC(n1c_full, n1c_pois, n1c_zip)

BIC(n2a_full, n2a_pois)
BIC(n2b_full, n2b_pois)
BIC(n2c_full, n2c_pois)

# ---------------------------------------------------------------------------------------------------------------------

# Explore results

# Determine significant predictors:
summary(n1a_full)
summary(n1b_full)
summary(n1c_full)
summary(n2a_full)
summary(n2b_full)
summary(n2c_full)

# Visualize impact of different variables:
plot(n1a_full, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(n1b_full, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(n1c_full, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(n2a_full, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(n2b_full, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(n2c_full, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)

# ---------------------------------------------------------------------------------------------------------------------

# Run model checks

# List all models:
models_list <- list(n1a_full, n1b_full, n1c_full, n2a_full, n2b_full, n2c_full)

# Loop through models and check fit/residuals:
for (i in 1:length(models_list)) {
  mod <- models_list[[i]]
  rsd <- residuals(mod, type = 'deviance')
  
  par(mfrow = c(2, 2))
  gam.check(mod, rep = 50)
  
  par(mfrow = c(1, 1))
  qqnorm(rsd)
  print(shapiro.test(rsd))
  
  plot(mod, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
  plot(mod, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE, residuals = TRUE, pch = 19)
  
  plot(fitted(mod), residuals(mod), pch = 20)
}

# ---------------------------------------------------------------------------------------------------------------------

# Detailed check of residuals

# Plot residuals vs. fitted values:
par(mfrow = c(3, 2))
plot(fitted(n1a_full), residuals(n1a_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Cases per Pop (Wave 1)')
plot(fitted(n1b_full), residuals(n1b_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Deaths per Pop (Wave 1)')
plot(fitted(n1c_full), residuals(n1c_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'IFR (Wave 1)')
plot(fitted(n2a_full), residuals(n2a_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Cases per Pop (Wave 2)')
plot(fitted(n2b_full), residuals(n2b_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Deaths per Pop (Wave 2)')
plot(fitted(n2c_full), residuals(n2c_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'IFR (Wave 2)')

# Plot residuals vs. predictors:
rsd <- residuals(n1a_full, type = 'deviance')

par(mfrow = c(3, 3))
plot(dat_cumulative$perc_18to64, rsd, pch = 20)
plot(dat_cumulative$care_home_beds, rsd, pch = 20)
plot(dat_cumulative$GISD_Score, rsd, pch = 20)
plot(dat_cumulative$pop_dens, rsd, pch = 20)
plot(dat_cumulative$living_area, rsd, pch = 20)
plot(dat_cumulative$perc_service, rsd, pch = 20)
plot(dat_cumulative$perc_production, rsd, pch = 20)

rsd <- residuals(n1b_full, type = 'deviance')

par(mfrow = c(3, 3))
plot(dat_cumulative$perc_18to64, rsd, pch = 20)
plot(dat_cumulative$hosp_beds, rsd, pch = 20)
plot(dat_cumulative$care_home_beds, rsd, pch = 20)
plot(dat_cumulative$GISD_Score, rsd, pch = 20)
plot(dat_cumulative$pop_dens, rsd, pch = 20)
plot(dat_cumulative$living_area, rsd, pch = 20)
plot(dat_cumulative$perc_service, rsd, pch = 20)
plot(dat_cumulative$perc_production, rsd, pch = 20)

rsd <- residuals(n1c_full, type = 'deviance')

par(mfrow = c(2, 3))
plot(dat_cumulative$perc_18to64, rsd, pch = 20)
plot(dat_cumulative$hosp_beds, rsd, pch = 20)
plot(dat_cumulative$care_home_beds, rsd, pch = 20)
plot(dat_cumulative$GISD_Score, rsd, pch = 20)
plot(dat_cumulative$pop_dens, rsd, pch = 20)

rsd <- residuals(n2a_full, type = 'deviance')

par(mfrow = c(3, 3))
plot(dat_cumulative$perc_18to64, rsd, pch = 20)
plot(dat_cumulative$care_home_beds, rsd, pch = 20)
plot(dat_cumulative$GISD_Score, rsd, pch = 20)
plot(dat_cumulative$pop_dens, rsd, pch = 20)
plot(dat_cumulative$living_area, rsd, pch = 20)
plot(dat_cumulative$perc_service, rsd, pch = 20)
plot(dat_cumulative$perc_production, rsd, pch = 20)

rsd <- residuals(n2b_full, type = 'deviance')

par(mfrow = c(3, 3))
plot(dat_cumulative$perc_18to64, rsd, pch = 20)
plot(dat_cumulative$hosp_beds, rsd, pch = 20)
plot(dat_cumulative$care_home_beds, rsd, pch = 20)
plot(dat_cumulative$GISD_Score, rsd, pch = 20)
plot(dat_cumulative$pop_dens, rsd, pch = 20)
plot(dat_cumulative$living_area, rsd, pch = 20)
plot(dat_cumulative$perc_service, rsd, pch = 20)
plot(dat_cumulative$perc_production, rsd, pch = 20)

rsd <- residuals(n2c_full, type = 'deviance')

par(mfrow = c(2, 3))
plot(dat_cumulative$perc_18to64, rsd, pch = 20)
plot(dat_cumulative$hosp_beds, rsd, pch = 20)
plot(dat_cumulative$care_home_beds, rsd, pch = 20)
plot(dat_cumulative$GISD_Score, rsd, pch = 20)
plot(dat_cumulative$pop_dens, rsd, pch = 20)

# # Fit model of residuals vs. covariates:
# rsd <- residuals(n2b_full, type = 'deviance')
# dat_cumulative$rsd <- rsd
# gam_resid <- gam(rsd ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 300) + s(ags2, bs = 're', k = 16) +
#                    # s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens),
#                    s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
#                    s(living_area) + s(perc_service) + s(perc_production) + s(deaths_wave1_rate),
#                  data = dat_cumulative)
# summary(gam_resid)
# par(mfrow = c(2, 2))
# gam.check(gam_resid, rep = 50)

# DHARMa workflow:
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
# https://aosmith.rbind.io/2017/12/21/using-dharma-for-residual-checks-of-unsupported-models/
library(Rcpp)
library(DHARMa)

par(mfrow = c(1, 1))

mus <- predict(n1a_full, type = 'response')
sim_n1a_full <- replicate(1000, rnbinom(n = nrow(dat_cumulative),
                                        size = 8.335,#8.489,
                                        mu = mus))
sim_res_n1a_full <- createDHARMa(simulatedResponse = sim_n1a_full,
                                 observedResponse = dat_cumulative$cases_wave1,
                                 fittedPredictedResponse = predict(n1a_full, type = 'response'),
                                 integerResponse = TRUE)

plot(sim_res_n1a_full)
testOutliers(sim_res_n1a_full, type = 'bootstrap') # tests if there are more simulation outliers than expected
testDispersion(sim_res_n1a_full) # tests if the simulated dispersion is equal to the observed dispersion
testZeroInflation(sim_res_n1a_full)
testUniformity(sim_res_n1a_full) # tests if the overall distribution conforms to expectations
testSpatialAutocorrelation(sim_res_n1a_full, x = dat_cumulative$long, y = dat_cumulative$lat)

###

mus <- predict(n1b_full, type = 'response')
sim_n1b_full <- replicate(1000, rnbinom(n = nrow(dat_cumulative),
                                        size = 2.056,#1.996,
                                        mu = mus))
sim_res_n1b_full <- createDHARMa(simulatedResponse = sim_n1b_full,
                                 observedResponse = dat_cumulative$deaths_wave1,
                                 fittedPredictedResponse = predict(n1b_full, type = 'response'),
                                 integerResponse = TRUE)

plot(sim_res_n1b_full)
testOutliers(sim_res_n1b_full, type = 'bootstrap')
testDispersion(sim_res_n1b_full)
testZeroInflation(sim_res_n1b_full)
testUniformity(sim_res_n1b_full)
testSpatialAutocorrelation(sim_res_n1b_full, x = dat_cumulative$long, y = dat_cumulative$lat)

###

mus <- predict(n1c_full, type = 'response')
sim_n1c_full <- replicate(1000, rnbinom(n = nrow(dat_cumulative),
                                        size = 4.865,#4.717,
                                        mu = mus))
sim_res_n1c_full <- createDHARMa(simulatedResponse = sim_n1c_full,
                                 observedResponse = dat_cumulative$deaths_wave1,
                                 fittedPredictedResponse = predict(n1c_full, type = 'response'),
                                 integerResponse = TRUE)

plot(sim_res_n1c_full)
testOutliers(sim_res_n1c_full, type = 'bootstrap')
testDispersion(sim_res_n1c_full)
testZeroInflation(sim_res_n1c_full)
testUniformity(sim_res_n1c_full)
testSpatialAutocorrelation(sim_res_n1c_full, x = dat_cumulative$long, y = dat_cumulative$lat)

###

mus <- predict(n2a_full, type = 'response')
sim_n2a_full <- replicate(1000, rnbinom(n = nrow(dat_cumulative),
                                        size = 35.833,#35.308,
                                        mu = mus))
sim_res_n2a_full <- createDHARMa(simulatedResponse = sim_n2a_full,
                                 observedResponse = dat_cumulative$cases_wave2,
                                 fittedPredictedResponse = predict(n2a_full, type = 'response'),
                                 integerResponse = TRUE)

plot(sim_res_n2a_full)
testOutliers(sim_res_n2a_full, type = 'bootstrap')
testDispersion(sim_res_n2a_full)
testZeroInflation(sim_res_n2a_full)
testUniformity(sim_res_n2a_full)
testSpatialAutocorrelation(sim_res_n2a_full, x = dat_cumulative$long, y = dat_cumulative$lat)

###

mus <- predict(n2b_full, type = 'response')
sim_n2b_full <- replicate(1000, rnbinom(n = nrow(dat_cumulative),
                                        size = 8.698,#8.562,
                                        mu = mus))
sim_res_n2b_full <- createDHARMa(simulatedResponse = sim_n2b_full,
                                 observedResponse = dat_cumulative$deaths_wave2,
                                 fittedPredictedResponse = predict(n2b_full, type = 'response'),
                                 integerResponse = TRUE)

plot(sim_res_n2b_full)
testOutliers(sim_res_n2b_full, type = 'bootstrap')
testDispersion(sim_res_n2b_full)
testZeroInflation(sim_res_n2b_full)
testUniformity(sim_res_n2b_full)
testSpatialAutocorrelation(sim_res_n2b_full, x = dat_cumulative$long, y = dat_cumulative$lat)

###

mus <- predict(n2c_full, type = 'response')
sim_n2c_full <- replicate(1000, rnbinom(n = nrow(dat_cumulative),
                                        size = 16.734,#16.433,
                                        mu = mus))
sim_res_n2c_full <- createDHARMa(simulatedResponse = sim_n2c_full,
                                 observedResponse = dat_cumulative$deaths_wave2,
                                 fittedPredictedResponse = predict(n2c_full, type = 'response'),
                                 integerResponse = TRUE)

plot(sim_res_n2c_full)
# plotResiduals(sim_res_n2c_full, form = dat_cumulative$GISD_Score)
testOutliers(sim_res_n2c_full, type = 'bootstrap')
testDispersion(sim_res_n2c_full)
testZeroInflation(sim_res_n2c_full)
testUniformity(sim_res_n2c_full)
testSpatialAutocorrelation(sim_res_n2c_full, x = dat_cumulative$long, y = dat_cumulative$lat)

# ---------------------------------------------------------------------------------------------------------------------
