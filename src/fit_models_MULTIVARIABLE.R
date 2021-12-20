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

# # Determine best range of values for k for lat/long
# 
# # Loop through values and fit models:
# n1a_mods = n1c_mods = n2a_mods = n2c_mods = vector('list', length = length(seq(10, 150, by = 10)))
# for (i in 1:length(seq(10, 150, by = 10))) {
#   k_val <- seq(10, 150, by = 10)[i]
#   n1a_temp <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
#                     s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(living_area, k = 25) +
#                     s(perc_service) + s(perc_production) +
#                     offset(log(pop)), data = dat_cumulative, family = 'nb')
#   n1c_temp <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
#                     s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + #s(pop_dens) +
#                     offset(log(cases_wave1)), data = dat_cumulative, family = 'nb')
#   
#   n2a_temp <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
#                     s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) + s(living_area) +
#                     s(perc_service) + s(perc_production) + s(cases_pre_rate) +
#                     offset(log(pop)), data = dat_cumulative, family = 'nb')
#   n2c_temp <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = k_val) + s(ags2, bs = 're', k = 16) +
#                     s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + #s(pop_dens) +
#                     s(cases_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb')
#   
#   n1a_mods[[i]] <- n1a_temp
#   n1c_mods[[i]] <- n1c_temp
#   n2a_mods[[i]] <- n2a_temp
#   n2c_mods[[i]] <- n2c_temp
# }
# 
# # Evaluate AICs and BICs:
# n1a_aics <- lapply(n1a_mods, AIC) %>% unlist()
# n1c_aics <- lapply(n1c_mods, AIC) %>% unlist()
# n2a_aics <- lapply(n2a_mods, AIC) %>% unlist()
# n2c_aics <- lapply(n2c_mods, AIC) %>% unlist()
# 
# n1a_bics <- lapply(n1a_mods, BIC) %>% unlist()
# n1c_bics <- lapply(n1c_mods, BIC) %>% unlist()
# n2a_bics <- lapply(n2a_mods, BIC) %>% unlist()
# n2c_bics <- lapply(n2c_mods, BIC) %>% unlist()
# 
# # Plot:
# par(mfrow = c(2, 1))
# plot(seq(10, 150, by = 10), n1a_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
# plot(seq(10, 150, by = 10), n1a_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')
# 
# plot(seq(10, 150, by = 10), n1c_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
# plot(seq(10, 150, by = 10), n1c_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')
# 
# plot(seq(10, 150, by = 10), n2a_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
# plot(seq(10, 150, by = 10), n2a_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')
# 
# plot(seq(10, 150, by = 10), n2c_aics, pch = 20, type = 'b', xlab = 'k', ylab = 'AIC')
# plot(seq(10, 150, by = 10), n2c_bics, pch = 20, type = 'b', xlab = 'k', ylab = 'BIC')

# ---------------------------------------------------------------------------------------------------------------------

# Fit models with chosen k values

# Wave 1:
n1a_full <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(living_area, k = 25) +
                  s(perc_service) + s(perc_production) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb')
n1b_full <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  offset(log(cases_wave1)), data = dat_cumulative, family = 'nb')

# Wave 2:
n2a_full <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) + s(living_area) +
                  s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb')
n2b_full <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                  s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(cases_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb')

# Quick check of fits:
par(mfrow = c(2, 2))
gam.check(n1a_full, rep = 50)
gam.check(n1b_full, rep = 50)
gam.check(n2a_full, rep = 50)
gam.check(n2b_full, rep = 50)

# ---------------------------------------------------------------------------------------------------------------------

# Explore potential model improvements

# Should living_area be included?:
n1a_comp <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb')
n2a_comp <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) +
                  s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                  offset(log(pop)), data = dat_cumulative, family = 'nb')

n1a_comp_alt <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                      s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(living_area, k = 25) +
                      s(perc_service) + s(perc_production) +
                      offset(log(pop)), data = dat_cumulative, family = 'nb')
n2a_comp_alt <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                      s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(living_area) +
                      s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                      offset(log(pop)), data = dat_cumulative, family = 'nb')

par(mfrow = c(2, 2))
gam.check(n1a_comp, rep = 50)
gam.check(n2a_comp, rep = 50)

AIC(n1a_full, n1a_comp)
BIC(n1a_full, n1a_comp)

AIC(n2a_full, n2a_comp)
BIC(n2a_full, n2a_comp)

AIC(n1a_comp, n1a_comp_alt)
BIC(n1a_comp, n1a_comp_alt)

AIC(n2a_comp, n2a_comp_alt)
BIC(n2a_comp, n2a_comp_alt)

# Try using MRF:
dat_cumulative$ARS <- factor(dat_cumulative$lk)
n1a_mrf <- gam(cases_wave1 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 50) + s(ags2, bs = 're', k = 16) +
                 s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(living_area, k = 25) +
                 s(perc_service) + s(perc_production) +
                 offset(log(pop)), data = dat_cumulative, family = 'nb')
n1b_mrf <- gam(deaths_wave1 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 60) + s(ags2, bs = 're', k = 16) +
                 s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                 offset(log(cases_wave1)), data = dat_cumulative, family = 'nb')

n2a_mrf <- gam(cases_wave2 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 70) + s(ags2, bs = 're', k = 16) +
                 s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) + s(living_area) +
                 s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                 offset(log(pop)), data = dat_cumulative, family = 'nb')
n2b_mrf <- gam(deaths_wave2 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 50) + s(ags2, bs = 're', k = 16) +
                 s(hosp_beds) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) +
                 s(deaths_pre_rate) + offset(log(cases_wave2)), data = dat_cumulative, family = 'nb')

AIC(n2a_full, n2a_mrf)
BIC(n2a_full, n2a_mrf)

AIC(n2b_full, n2b_mrf)
BIC(n2b_full, n2b_mrf)

# Compare with poisson/zero-inflated:
n1a_pois <- gam(cases_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds) + s(GISD_Score) + s(pop_dens) + s(living_area, k = 25) +
                  s(perc_service) + s(perc_production) +
                  offset(log(pop)), data = dat_cumulative, family = 'poisson')
n1b_pois <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) +# s(pop_dens) +
                  offset(log(cases_wave1)), data = dat_cumulative, family = 'poisson')

n2a_pois <- gam(cases_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 70) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(care_home_beds, k = 25) + s(GISD_Score) + s(pop_dens) + s(living_area) +
                  s(perc_service) + s(perc_production) + s(cases_pre_rate) +
                  offset(log(pop)), data = dat_cumulative, family = 'poisson')
n2b_pois <- gam(deaths_wave2 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 50) + s(ags2, bs = 're', k = 16) +
                  s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) +# s(pop_dens) +
                  s(cases_pre_rate) +
                  offset(log(cases_wave2)), data = dat_cumulative, family = 'poisson')

n1b_zip <- gam(deaths_wave1 ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 60) + s(ags2, bs = 're', k = 16) +
                 s(perc_18to64) + s(hosp_beds) + s(care_home_beds) + s(GISD_Score) +# s(pop_dens) +
                 offset(log(cases_wave1)), data = dat_cumulative, family = 'ziP')

BIC(n1a_full, n1a_pois)
BIC(n1b_full, n1b_pois, n1b_zip)

BIC(n2a_full, n2a_pois)
BIC(n2b_full, n2b_pois)

# ---------------------------------------------------------------------------------------------------------------------

# Explore results

# Determine significant predictors:
summary(n1a_full)
summary(n1b_full)
summary(n2a_full)
summary(n2b_full)

# Visualize impact of different variables:
plot(n1a_full, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(n1b_full, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(n2a_full, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)
plot(n2b_full, pages = 1, scheme = 2, shade = TRUE, scale = 0, seWithMean = TRUE)

# Plot marginal effects of significant variables:
n1a_pred_GISD <- ggpredict(n1a_full, 'GISD_Score')

n1b_pred_hosp <- ggpredict(n1b_full, 'hosp_beds')
n1b_pred_chb <- ggpredict(n1b_full, 'care_home_beds')

n2a_pred_18to64 <- ggpredict(n2a_full, 'perc_18to64')
n2a_pred_GISD <- ggpredict(n2a_full, 'GISD_Score')
n2a_pred_popdens <- ggpredict(n2a_full, 'pop_dens')
n2a_pred_living <- ggpredict(n2a_full, 'living_area')

n2b_pred_chb <- ggpredict(n2b_full, 'care_home_beds')

plot(n1a_pred_GISD)
plot(n1b_pred_hosp)
plot(n1b_pred_chb)
plot(n2a_pred_18to64)
plot(n2a_pred_GISD)
plot(n2a_pred_popdens)
plot(n2a_pred_living)
plot(n2b_pred_chb)

# Plot marginal effects of SES (even where not significant):
n1a_pred_service <- ggpredict(n1a_full, 'perc_service')
n1a_pred_production <- ggpredict(n1a_full, 'perc_production')

n1b_pred_GISD <- ggpredict(n1b_full, 'GISD_Score')

n2a_pred_service <- ggpredict(n2a_full, 'perc_service')
n2a_pred_production <- ggpredict(n2a_full, 'perc_production')

n2b_pred_GISD <- ggpredict(n2b_full, 'GISD_Score')

plot(n1a_pred_service)
plot(n1a_pred_production)
plot(n1b_pred_GISD)
plot(n2a_pred_service)
plot(n2a_pred_production)
plot(n2b_pred_GISD)

# For second wave, plot impact of attack rate before wave 2:
n2a_pred_wave1 <- ggpredict(n2a_full, 'cases_pre_rate')
n2b_pred_wave1 <- ggpredict(n2b_full, 'cases_pre_rate')

plot(n2a_pred_wave1)
plot(n2b_pred_wave1)

# Plot BL-level effect:
n1a_pred_BL <- ggpredict(n1a_full, 'ags2') # sig
n1b_pred_BL <- ggpredict(n1b_full, 'ags2') # not sig
n2a_pred_BL <- ggpredict(n2a_full, 'ags2') # sig
n2b_pred_BL <- ggpredict(n2b_full, 'ags2') # not sig

plot(n1a_pred_BL)
plot(n1b_pred_BL)
plot(n2a_pred_BL)
plot(n2b_pred_BL)

# Plot spatial effect (after controlling for variables):
pdata <- with(dat_cumulative,
              expand.grid(pop = 100000,
                          cases_wave1 = 1000,
                          cases_wave2 = 1000,
                          ags2 = '01',
                          long = seq(min(long), max(long), length = 100),
                          lat = seq(min(lat), max(lat), length = 100),
                          perc_18to64 = median(perc_18to64),
                          hosp_beds = median(hosp_beds),
                          care_home_beds = median(care_home_beds),
                          GISD_Score = median(GISD_Score),
                          pop_dens = median(pop_dens),
                          living_area = median(living_area),
                          perc_service = median(perc_service),
                          perc_production = median(perc_production),
                          cases_pre_rate = median(cases_pre_rate)))

n1a_fit <- predict(n1a_full, pdata)
n1b_fit <- predict(n1b_full, pdata)
n2a_fit <- predict(n2a_full, pdata)
n2b_fit <- predict(n2b_full, pdata)

ind <- exclude.too.far(pdata$long, pdata$lat, dat_cumulative$long, dat_cumulative$lat, dist = 0.1)
n1a_fit[ind] <- NA
n1b_fit[ind] <- NA
n2a_fit[ind] <- NA
n2b_fit[ind] <- NA

n1a_pred <- cbind(pdata, fitted = n1a_fit)
n1b_pred <- cbind(pdata, fitted = n1b_fit)
n2a_pred <- cbind(pdata, fitted = n2a_fit)
n2b_pred <- cbind(pdata, fitted = n2b_fit)

p1a <- ggplot(n1a_pred, aes(x = long, y = lat)) + geom_raster(aes(fill = fitted)) +
  scale_fill_viridis(na.value = 'transparent') + coord_quickmap() + theme_void()
p1b <- ggplot(n1b_pred, aes(x = long, y = lat)) + geom_raster(aes(fill = fitted)) +
  scale_fill_viridis(na.value = 'transparent') + coord_quickmap() + theme_void()
p2a <- ggplot(n2a_pred, aes(x = long, y = lat)) + geom_raster(aes(fill = fitted)) +
  scale_fill_viridis(na.value = 'transparent') + coord_quickmap() + theme_void()
p2b <- ggplot(n2b_pred, aes(x = long, y = lat)) + geom_raster(aes(fill = fitted)) +
  scale_fill_viridis(na.value = 'transparent') + coord_quickmap() + theme_void()
grid.arrange(p1a, p1b, p2a, p2b, ncol = 2)

# ---------------------------------------------------------------------------------------------------------------------

# Run model checks

# List all models:
models_list <- list(n1a_full, n1b_full, n2a_full, n2b_full)

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
par(mfrow = c(2, 2))
plot(fitted(n1a_full), residuals(n1a_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Cases per Pop (Wave 1)')
plot(fitted(n1b_full), residuals(n1b_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Deaths per Pop (Wave 1)')
plot(fitted(n2a_full), residuals(n2a_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Cases per Pop (Wave 2)')
plot(fitted(n2b_full), residuals(n2b_full, type = 'deviance'),
     xlab = 'Fitted Values', ylab = 'Deviance Residuals',
     main = 'Deaths per Pop (Wave 2)')

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

par(mfrow = c(2, 2))
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

par(mfrow = c(2, 2))
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
                                        size = 7.533,
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
                                        size = 4.877,
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

mus <- predict(n2a_full, type = 'response')
sim_n2a_full <- replicate(1000, rnbinom(n = nrow(dat_cumulative),
                                        size = 32.791,
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
                                        size = 16.569,
                                        mu = mus))
sim_res_n2b_full <- createDHARMa(simulatedResponse = sim_n2b_full,
                                 observedResponse = dat_cumulative$deaths_wave2,
                                 fittedPredictedResponse = predict(n2b_full, type = 'response'),
                                 integerResponse = TRUE)

plot(sim_res_n2b_full)
# plotResiduals(sim_res_n2b_full, form = dat_cumulative$GISD_Score)
testOutliers(sim_res_n2b_full, type = 'bootstrap')
testDispersion(sim_res_n2b_full)
testZeroInflation(sim_res_n2b_full)
testUniformity(sim_res_n2b_full)
testSpatialAutocorrelation(sim_res_n2b_full, x = dat_cumulative$long, y = dat_cumulative$lat)

# ---------------------------------------------------------------------------------------------------------------------
