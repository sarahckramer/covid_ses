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
library(viridis)
library(gridExtra)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data
keep_map <- TRUE
source('src/functions/load_data.R')

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

# MRF weights based on travel?

# Need ARS and Bundesland as factor:
dat_cumulative <- dat_cumulative %>%
  mutate(ARS = factor(lk),
         bundesland = factor(bundesland))

# Fit w/ default weights:
n2a <- gam(cases_wave2 ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 300) + s(ags2, bs = 're', k = 16) +
                    offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')

# Now try with differing weights by area:
p_mat <- matrix(0, nrow = 401, ncol = 401)
rownames(p_mat) = colnames(p_mat) = map_base$ARS
diag(p_mat) <- card(nb)
for (i in 1:401) {
  for (j in nb[i][[1]]) {
    p_mat[i, j] <- -0.5
  }
}
n2a_1 <- gam(cases_wave2 ~ s(ARS, bs = 'mrf', xt = list(penalty = p_mat), k = 300) + s(ags2, bs = 're', k = 16) +
             offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')

for (i in 1:401) {
  for (j in nb[i][[1]]) {
    p_mat[i, j] <- 0.5
  }
}
n2a_2 <- gam(cases_wave2 ~ s(ARS, bs = 'mrf', xt = list(penalty = p_mat), k = 300) + s(ags2, bs = 're', k = 16) +
               offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')

for (i in 1:401) {
  for (j in nb[i][[1]]) {
    p_mat[i, j] <- 1
  }
}
n2a_3 <- gam(cases_wave2 ~ s(ARS, bs = 'mrf', xt = list(penalty = p_mat), k = 300) + s(ags2, bs = 're', k = 16) +
               offset(log(pop)), data = dat_cumulative, family = 'nb', method = 'ML')
# seems like values have to be between -1 and 1

# Plot predictions from each model:
pdata <- with(dat_cumulative,
              expand.grid(pop = 10000,
                          ags2 = '09',
                          ARS = ARS))

m1.fit <- predict(n2a, pdata)
m2.fit <- predict(n2a_1, pdata)
m3.fit <- predict(n2a_2, pdata)
m4.fit <- predict(n2a_3, pdata)

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
