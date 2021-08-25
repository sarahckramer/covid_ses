# ---------------------------------------------------------------------------------------------------------------------
# Explore lat/long size of regions, possibility of changing weights over time, etc.
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(mgcv)
library(ggeffects)
library(sf)
library(testthat)
library(spdep)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data
source('src/functions/load_data.R')

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

# Explore coordinate ranges for each LK

# Get min and max coordinates:
calc_boxes <- map_base %>%
  st_transform(., '+proj=longlat') %>%
  group_by(ARS) %>% nest()# %>%
# mutate(bbox = map(data, st_bbox))
# https://stackoverflow.com/questions/54696440/create-polygons-representing-bounding-boxes-for-subgroups-using-sf#54697086

# Get ranges:
box_list <- lapply(calc_boxes$data, st_bbox)
box_ranges <- lapply(box_list, function(ix) {
  xrange <- ix$xmax - ix$xmin
  yrange <- ix$ymax - ix$ymin
  return(setNames(c(xrange, yrange),
                  nm = c('xrange', 'yrange')))
}) %>%
  bind_rows()

# Assess:
summary(box_ranges)
# Latitude ranges from 0.06 to 0.94 degrees; longitude from 0.096 to 2.11 degrees

hist(box_ranges$yrange, xlab = 'Latitude Range')
hist(box_ranges$xrange, xlab = 'Longitude Range')

# ---------------------------------------------------------------------------------------------------------------------

# Can models be fit where lat/long are allowed to change over time? (Potentially relevant for travel data)

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

# First, fit with constant lat/long:
n1b <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 62) + s(bundesland, bs = 're', k = 16) + offset(log(cases)),
           data = dat_inc_fromCases, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

plot(n1b, pages = 1, scheme = 2, shade = TRUE, scale = 0)

par(mfrow = c(2, 2))
gam.check(n1b)

# Now try allowing them to change:
dat_inc_fromCases_NOISE <- dat_inc_fromCases %>%
  mutate(long = long + rnorm(dim(dat_inc_fromCases)[1], mean = 0, sd = 0.5),
         lat = lat + rnorm(dim(dat_inc_fromCases)[1], mean = 0, sd = 0.5))

n1b.noise <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 62) + s(bundesland, bs = 're', k = 16) + offset(log(cases)),
                 data = dat_inc_fromCases_NOISE, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

plot(n1b.noise, pages = 1, scheme = 2, shade = TRUE, scale = 0)

par(mfrow = c(2, 2))
gam.check(n1b.noise)
# possible

n1b.pred <- ggpredict(n1b)
n1b.pred.noise <- ggpredict(n1b.noise, 'Week')
# can't do ggpredict on lat or long anymore? Why is this?; Maybe interaction doesn't make sense anymore

plot(n1b.pred$Week)
plot(n1b.pred.noise)

n1b.pred <- ggpredict(n1b, c('Week', 'lat'))
n1b.pred.noise <- ggpredict(n1b.noise, c('Week', 'lat'))
plot(n1b.pred)
plot(n1b.pred.noise)

# ---------------------------------------------------------------------------------------------------------------------

# MRF weights based on travel?

# Reload and format data:
source('src/functions/load_data.R')

# Need ARS and Bundesland as factor:
dat_inc <- dat_inc %>%
  mutate(ARS = factor(ARS),
         bundesland = factor(bundesland))

# Get specific data frame for deaths/cases analysis, where no cases == 0:
dat_inc_fromCases <- dat_inc %>%
  filter(cases > 0)

# Fit w/ default weights:
m1b <- bam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 401) + s(Week, k = 62) + s(bundesland, bs = 're', k = 16) + offset(log(cases)),
           data = dat_inc_fromCases, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

plot(m1b, pages = 1, scheme = 2, shade = TRUE, scale = 0)

# Now try with differing weights by area:
p_mat <- matrix(0, nrow = 401, ncol = 401)
rownames(p_mat) = colnames(p_mat) = map_base$ARS
diag(p_mat) <- card(nb)
for (i in 1:96) {
  for (j in nb[i][[1]]) {
    p_mat[i, j] <- -0.5
    # p_mat[j, i] <- -1
  }
}

m2b <- bam(deaths ~ s(ARS, bs = 'mrf', xt = list(penalty = p_mat), k = 401) + s(Week, k = 62) + s(bundesland, bs = 're', k = 16) + offset(log(cases)),
           data = dat_inc_fromCases, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

for (i in 1:96) {
  for (j in nb[i][[1]]) {
    p_mat[i, j] <- 0.5
  }
}

m3b <- bam(deaths ~ s(ARS, bs = 'mrf', xt = list(penalty = p_mat), k = 401) + s(Week, k = 62) + s(bundesland, bs = 're', k = 16) + offset(log(cases)),
           data = dat_inc_fromCases, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

for (i in 1:96) {
  for (j in nb[i][[1]]) {
    p_mat[i, j] <- 1
  }
}

m4b <- bam(deaths ~ s(ARS, bs = 'mrf', xt = list(penalty = p_mat), k = 401) + s(Week, k = 62) + s(bundesland, bs = 're', k = 16) + offset(log(cases)),
           data = dat_inc_fromCases, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
# seems like values have to be between -1 and 1

pdata <- with(dat_inc_fromCases,
              expand.grid(cases = 100,
                          bundesland = 'Bayern',
                          ARS = ARS,
                          Week = 56))

m1.fit <- predict(m1b, pdata)
m2.fit <- predict(m2b, pdata)
m3.fit <- predict(m3b, pdata)
m4.fit <- predict(m4b, pdata)

m1.pred <- cbind(pdata, fitted = m1.fit)
m2.pred <- cbind(pdata, fitted = m2.fit)
m3.pred <- cbind(pdata, fitted = m3.fit)
m4.pred <- cbind(pdata, fitted = m4.fit)

map.m1pred <- map_base %>%
  left_join(m1.pred, by = 'ARS')
map.m2pred <- map_base %>%
  left_join(m2.pred, by = 'ARS')
map.m3pred <- map_base %>%
  left_join(m3.pred, by = 'ARS')
map.m4pred <- map_base %>%
  left_join(m4.pred, by = 'ARS')

p1 <- ggplot(map.m1pred) + geom_sf(aes(fill = fitted)) +
  scale_fill_viridis(na.value = 'transparent') + theme_void()
p2 <- ggplot(map.m2pred) + geom_sf(aes(fill = fitted)) +
  scale_fill_viridis(na.value = 'transparent') + theme_void()
p3 <- ggplot(map.m3pred) + geom_sf(aes(fill = fitted)) +
  scale_fill_viridis(na.value = 'transparent') + theme_void()
p4 <- ggplot(map.m4pred) + geom_sf(aes(fill = fitted)) +
  scale_fill_viridis(na.value = 'transparent') + theme_void()
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Negative values indicate that regions should be MORE similar; both results using positive values appear similar
# Larger negative values lead to more smoothing - so if we used travel, more travel would lead to stronger negative
# values

# It doesn't look like there is a way to allow the penalty matrix to change over time, though

# ---------------------------------------------------------------------------------------------------------------------
