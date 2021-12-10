# ---------------------------------------------------------------------------------------------------------------------
# Build GAMs exploring the role of individual predictors
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
