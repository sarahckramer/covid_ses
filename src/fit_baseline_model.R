# ---------------------------------------------------------------------------------------------------------------------
# Build GAM for a single Bundesland
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
library(pomp)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data
source('src/load_data.R')

# ---------------------------------------------------------------------------------------------------------------------

# Get map data and relevant coordinates

# Read in map data:
map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')

expect_true(all(unique(map_base$ARS) %in% unique(dat_inc$lk)))
expect_true(all(unique(dat_inc$lk) %in% unique(map_base$ARS)))

# Get neighborhood info:
nb <- spdep::poly2nb(map_base, row.names = map_base$ARS)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

# Get CENTROID coordinates:
map_base[, c('long', 'lat')] <- st_centroid(map_base) %>% st_transform(., '+proj=longlat') %>% st_coordinates()
# Note: eventually probably want to do population center and not centroid - centroids aren't necessarily within LK

# map_cent <- st_centroid(map_base)
# ggplot() + geom_sf(data = map_base) + geom_sf(data = map_cent) + theme_void()

# ---------------------------------------------------------------------------------------------------------------------

# Begin to fit model with no predictors and using CENTROID lat/long

# Add lat/long to deaths/cases data frame:
dat_inc <- dat_inc %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)

# Get number of regions and weeks:
print(length(unique(dat_inc$ARS)))
print(length(unique(dat_inc$Week)))

# Need Bundesland as factor:
dat_inc <- dat_inc %>%
  mutate(bundesland = factor(bundesland))

# Get specific data frame for deaths/cases analysis, where no cases == 0:
dat_inc_fromCases <- dat_inc %>%
  filter(cases > 0)

# First, fit without interaction:
n1a <- bam(cases ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 62) + s(bundesland, bs = 're', k = 16) + offset(log(pop)),
           data = dat_inc, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
n1b <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 62) + s(bundesland, bs = 're', k = 16) + offset(log(cases)),
           data = dat_inc_fromCases, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
# n1c <- bam(deaths ~ s(long, lat, bs = 'ds', m = 2, k = 401) + s(Week, k = 62) + s(bundesland, bs = 're', k = 16) + offset(log(pop)),
#            data = dat_inc, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

plot(n1a, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(n1b, pages = 1, scheme = 2, shade = TRUE, scale = 0)
# plot(n1c, pages = 1, scheme = 2, shade = TRUE, scale = 0)

par(mfrow = c(2, 2))
gam.check(n1a)
gam.check(n1b)
# gam.check(n1c)

summary(n1a)
summary(n1b)
# summary(n1c)

n1a.pred <- ggpredict(n1a)
n1b.pred <- ggpredict(n1b)
# n1c.pred <- ggpredict(n1c)

plot(n1a.pred$lat)
plot(n1b.pred$lat)
# plot(n1c.pred$lat)

plot(n1a.pred$long)
plot(n1b.pred$long)
# plot(n1c.pred$long)

plot(n1a.pred$Week)
plot(n1b.pred$Week)
# plot(n1c.pred$Week)

plot(n1a.pred$bundesland)
plot(n1b.pred$bundesland)
# plot(n1c.pred$bundesland)

# # Check that better than poisson:
# n1a.pois <- bam(cases ~ s(long, lat, bs = 'ds', m = 2, k = 401) + s(Week, k = 62) + s(bundesland, bs = 're', k = 16) + offset(log(pop)),
#                 data = dat_inc, family = 'poisson', method = 'fREML', nthreads = 4, discrete = TRUE)
# n1b.pois <- bam(deaths ~ s(long, lat, bs = 'ds', m = 2, k = 401) + s(Week, k = 62) + s(bundesland, bs = 're', k = 16) + offset(log(cases)),
#                 data = dat_inc_fromCases, family = 'poisson', method = 'fREML', nthreads = 4, discrete = TRUE)
# 
# AIC(n1a, n1a.pois)
# BIC(n1a, n1a.pois)
# AIC(n1b, n1b.pois)
# BIC(n1b, n1b.pois)
# # NB consistently better
# 
# rm(n1a.pois, n1b.pois)
# 
# # Check zero-inflated (note that mgcv does not yet allow for zero-inflated NB, so may need to use another package if this is necessary):
# n1b.zip <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 62) + s(bundesland, bs = 're', k = 16) + offset(log(cases)),
#                data = dat_inc_fromCases, family = 'ziP', method = 'fREML', nthreads = 4, discrete = TRUE)
# 
# AIC(n1b, n1b.zip)
# BIC(n1b, n1b.zip)
# NB still looks better than zero-inflated, but might eventually want to check zero-inflated NB
#
# rm(n1b.zip)

# Now include interaction between space and time:
tic <- Sys.time()
n2a <- bake(file = 'results/fitted_models/n2a_401_62_200_20_NEW.rds',
            expr = {
              bam(cases ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 62) +
                    ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(200, 20)) +
                    s(bundesland, bs = 're', k = 16) + offset(log(pop)),
                  data = dat_inc, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
            }
)
toc <- Sys.time()
print(toc - tic)

tic <- Sys.time()
n2b <- bake(file = 'results/fitted_models/n2b_401_62_200_20_NEW.rds',
            expr = {
              bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 62) + 
                    ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(200, 20)) +
                    s(bundesland, bs = 're', k = 16) + offset(log(cases)),
                  data = dat_inc_fromCases, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
            }
)
toc <- Sys.time()
print(toc - tic)
# with ~100/20, <10 min; with 200/20, 1.35 hours (!!)

par(mfrow = c(2, 2))
gam.check(n2a)
gam.check(n2b)

plot(n2a, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(n2b, pages = 1, scheme = 2, shade = TRUE, scale = 0)

# summary(n2a)
summary(n2b)

n2a.pred <- ggpredict(n2a)
n2b.pred <- ggpredict(n2b)

plot(n2a.pred$lat)
plot(n2b.pred$lat)

plot(n2a.pred$long)
plot(n2b.pred$long)

plot(n2a.pred$Week)
plot(n2b.pred$Week)

plot(n2a.pred$bundesland)
plot(n2b.pred$bundesland)

# ---------------------------------------------------------------------------------------------------------------------
# Plot predictions
pdata <- with(dat_inc,
              expand.grid(cases = 100,
                          # bundesland = unique(dat_inc$bundesland),
                          bundesland = 'Bayern',
                          Week = seq(min(Week), max(Week), by = 4),
                          long = seq(min(long), max(long), length = 100),
                          lat = seq(min(lat), max(lat), length = 100)))

# n1b.fit <- predict(n1b, pdata)
n2b.fit <- predict(n2b, pdata)
# or add in observed case data and plot out predicted deaths / cases

ind <- exclude.too.far(pdata$long, pdata$lat, dat_inc$long, dat_inc$lat, dist = 0.1)
n1b.fit[ind] <- NA
n2b.fit[ind] <- NA

n1b.pred <- cbind(pdata, fitted = n1b.fit)
n2b.pred <- cbind(pdata, fitted = n2b.fit)

p3 <- ggplot(n1b.pred, aes(x = long, y = lat)) + geom_raster(aes(fill = fitted)) +
  facet_wrap(~ Week, ncol = 8) +
  scale_fill_viridis(na.value = 'transparent') +
  coord_quickmap() + theme_void()

p4 <- ggplot(n2b.pred, aes(x = long, y = lat)) + geom_raster(aes(fill = fitted)) +
  facet_wrap(~ Week, ncol = 8) +
  scale_fill_viridis(na.value = 'transparent') +
  coord_quickmap() + theme_void()

print(p3)
print(p4)
grid.arrange(p3, p4, ncol = 1)
# Of course, these also show the overall change week to week

# If we want to map just the relative intensity in a given week, we need to standardize somehow:
n1b.pred.stand <- n1b.pred %>%
  drop_na() %>%
  group_by(Week) %>%
  mutate(fitted = fitted - mean(fitted))
n2b.pred.stand <- n2b.pred %>%
  drop_na() %>%
  group_by(Week) %>%
  mutate(fitted = fitted - mean(fitted))

p3 <- ggplot(n1b.pred.stand, aes(x = long, y = lat)) + geom_raster(aes(fill = fitted)) +
  facet_wrap(~ Week, ncol = 8) +
  scale_fill_viridis(na.value = 'transparent') +
  coord_quickmap() + theme_void()

p4 <- ggplot(n2b.pred.stand, aes(x = long, y = lat)) + geom_raster(aes(fill = fitted)) +
  facet_wrap(~ Week, ncol = 8) +
  scale_fill_viridis(na.value = 'transparent') +
  coord_quickmap() + theme_void()

print(p3)
print(p4)
grid.arrange(p3, p4, ncol = 1)

# ---------------------------------------------------------------------------------------------------------------------

# explore different k values (for 1 and 2)
# no need to do random effect for BL once interaction between space and time included? (actually, seems to still play a role...)
# how to do model building with GAMs, since would need to adjust k as more predictors added/removed? (check French paper?)

# ---------------------------------------------------------------------------------------------------------------------

# Try with MRF instead

# Reload data:
source('src/load_data.R')

# Need ARS and Bundesland as factor:
dat_inc <- dat_inc %>%
  mutate(ARS = factor(ARS),
         bundesland = factor(bundesland))

# Get specific data frame for deaths/cases analysis, where no cases == 0:
dat_inc_fromCases <- dat_inc %>%
  filter(cases > 0)

# Fit w/o interaction:
m1b <- bam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 401) + s(Week, k = 62) + s(bundesland, bs = 're', k = 16) + offset(log(cases)),
           data = dat_inc_fromCases, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

par(mfrow = c(2, 2))
gam.check(m1b)

plot(m1b, pages = 1, scheme = 2, shade = TRUE, scale = 0)

# Fit w/ interaction:
tic <- Sys.time()
m2b <- bake(file = 'results/fitted_models/m2b_401_62_200_20.rds',
            expr = {
              bam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 401) + s(Week, k = 62) + 
                    te(Week, ARS, bs = c('fs', 'mrf'), xt = list(Week = NULL, ARS = list(nb = nb)), k = c(20, 200)) +
                    s(bundesland, bs = 're', k = 16) + offset(log(cases)),
                  data = dat_inc_fromCases, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
            }
)
toc <- Sys.time()
print(toc - tic)
# 1.44 hours; comparable to model using lat/long

par(mfrow = c(2, 2))
gam.check(m2b)

plot(m2b, pages = 1, scheme = 2, shade = TRUE, scale = 0)

# Plot predictions:
pdata <- with(dat_inc_fromCases,
              expand.grid(cases = 100,
                          bundesland = 'Bayern',
                          ARS = ARS,
                          Week = seq(min(Week), max(Week), by = 8)))

m1.fit <- predict(m1b, pdata)
m2.fit <- predict(m2b, pdata)

m1.pred <- cbind(pdata, fitted = m1.fit)
m2.pred <- cbind(pdata, fitted = m2.fit)

map.m1pred <- map_base %>%
  left_join(m1.pred, by = 'ARS')
map.m2pred <- map_base %>%
  left_join(m2.pred, by = 'ARS')

p5 <- ggplot(map.m1pred) + geom_sf(aes(fill = fitted)) + facet_wrap(~ Week, ncol = 4) +
  scale_fill_viridis(na.value = 'transparent') + theme_void()
p6 <- ggplot(map.m2pred) + geom_sf(aes(fill = fitted)) + facet_wrap(~ Week, ncol = 4) +
  scale_fill_viridis(na.value = 'transparent') + theme_void()
grid.arrange(p5, p6, ncol = 1)

# ---------------------------------------------------------------------------------------------------------------------

# Compare lat/long vs. MRF?

plot(m1b, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(n1b, pages = 1, scheme = 2, shade = TRUE, scale = 0)

plot(m2b, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(n2b, pages = 1, scheme = 2, shade = TRUE, scale = 0)
# estimates for week look similar in model 1, but once interaction is included, estimates seem to have much wider
# confidence intervals when using MRF than when using lat/long

AIC(m1b, n1b, m2b, n2b)
BIC(m1b, n1b, m2b, n2b)
# AIC consistently better for MRF, but also higher df; BIC lower for lat/long
