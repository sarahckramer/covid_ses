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

# Choose Bundesland of interest:
bl_code <- '09' # Saxony=14; Bavaria=09; BadenWuerttemberg=08

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data

# Read in cumulative and incident data:
dat_inc <- read_csv('data/formatted/weekly_covid_deaths_by_lk_INCIDENT.csv')
dat_cum <- read_csv('data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE.csv')

# Limit to BL of interest:
dat_inc <- dat_inc %>%
  filter(str_starts(lk, bl_code))
dat_cum <- dat_cum %>%
  filter(str_starts(lk, bl_code))

# Format:
dat_inc <- dat_inc %>%
  mutate(Week = if_else(Year == 2020, Week, Week + 53)) %>%
  # mutate(time = Week - min(Week) + 1) %>%
  filter(deaths <= cases) %>%
  drop_na()
dat_cum <- dat_cum %>%
  mutate(Week = if_else(Year == 2020, Week, Week + 53)) %>%
  filter(deaths <= cases) %>%
  drop_na()

# p1 <- ggplot(data = dat_inc, aes(x = Week, y = death_rate, group = lk)) +
#   geom_line() + theme_classic() + facet_wrap(~ lk)
# print(p1)

# Get Landkreise as factor:
dat_inc <- dat_inc %>%
  mutate(ARS = factor(lk))
dat_cum <- dat_cum %>%
  mutate(ARS = factor(lk))

# Get second wave only:
dat_inc <- dat_inc %>%
  filter(Week >= 40)
dat_cum <- dat_cum %>%
  filter(Week >= 40) %>%
  left_join(dat_cum[dat_cum$Week == 39, c('lk', 'deaths', 'cases')],
            by = 'lk') %>%
  mutate(deaths = deaths.x - deaths.y,
         cases = cases.x - cases.y) %>%
  select(date:lk, deaths, cases, pop:ARS) %>%
  mutate(death_rate = deaths / pop * 100000,
         .after = deaths) %>%
  mutate(case_rate = cases / pop * 100000,
         .after = cases)

p2 <- ggplot(data = dat_inc, aes(x = Week, y = death_rate, group = lk)) +
  geom_line() + theme_classic() + facet_wrap(~ lk)
print(p2)

# ---------------------------------------------------------------------------------------------------------------------

# Add covariates
age_dist <- read_csv('data/formatted/age_dist.csv')

dat_inc <- dat_inc %>%
  left_join(age_dist, by = 'lk')
dat_cum <- dat_cum %>%
  left_join(age_dist, by = 'lk')

mig_dat <- read_csv('data/formatted/mig_hosp_dat.csv')

dat_inc <- dat_inc %>%
  left_join(mig_dat, by = 'lk')
dat_cum <- dat_cum %>%
  left_join(mig_dat, by = 'lk')

# ---------------------------------------------------------------------------------------------------------------------

# Plot covariates against incidence/mortality/each other

dat_cum %>%
  filter(Week == 61) %>%
  select(death_rate, case_rate, pop, prop65, prop.aus:cit.p.pop) %>%
  pairs()
# honestly no clear associations with outcomes here

# ---------------------------------------------------------------------------------------------------------------------

# # Map covariates
# dat_covar <- map_base %>%
#   left_join(dat_cum %>% pivot_longer(c(pop, prop65, prop.aus:cit.p.pop), names_to = 'var'), by = 'ARS') %>%
#   select(ARS, var:value) %>%
#   unique() %>%
#   mutate(var = factor(var))
# 
# for (ix in levels(dat_covar$var)) {
#   p <- ggplot(data = dat_covar[dat_covar$var == ix, ]) + geom_sf(aes(fill = value)) +
#     theme_void() + scale_fill_viridis() + labs(title = ix)
#   print(p)
# }

# ---------------------------------------------------------------------------------------------------------------------

# Fit spatial models at 3 timepoints

# Read in map data:
map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')
map_base <- map_base %>%
  filter(str_starts(ARS, bl_code))

expect_true(all(unique(map_base$ARS) %in% unique(dat_inc$lk)))
expect_true(all(unique(dat_inc$lk) %in% unique(map_base$ARS)))

# Get neighborhood info:
nb <- spdep::poly2nb(map_base, row.names = map_base$ARS)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

# map_base$neighbors <- card(nb)
# ggplot(map_base) + geom_sf() + theme_void()

# Limit data to week of interest:
dat_cum3 <- dat_cum %>%
  filter(Week == 61)

# Begin to fit models:
m1 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)) + offset(log(cases)), data = dat_cum3, family = 'poisson', method = 'REML')
# m2 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 50) + offset(log(cases)), data = dat_cum3, family = poisson())
# m3 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 10) + offset(log(cases)), data = dat_cum3, family = poisson())
# m4 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 5) + offset(log(cases)), data = dat_cum3, family = poisson())
# m1 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)), data = dat_cum3, family = poisson(), offset = log(cases))

m2 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 5) + offset(log(cases)), data = dat_cum3, family = 'nb', method = 'REML')
# Poisson appears to fit better, but NB might have more normal residuals? - NB also using way fewer df though!
# cmx; smooth$S; R; edf; edf1; Vp; Ve; coefficients; residuals; weights; working.weights; linear.predictors; y; rv; boundary; db.drho; dw.drho; sp; gcv.ubre

# m3 <- gam(deaths ~ s(ARS, bs = 're') + offset(log(cases)), data = dat_cum3, family = poisson())
# # "Modell hat mehr Koeffizienten als Daten" - cannot seem to fit this type of random effect unless there are multiple data points per LK?

par(mfrow = c(2, 2))
gam.check(m2)

# interpretation; method?; using ggeffects with discrete model?
# Test for spatial autocorrelation in the residuals to assure model fit?
# Some way to pull out which regions, holding all other covariates constant, still have unusually high/low incidence/mortality after controlling for variables?
# How to deal with outliers?



m3 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 15) + s(prop65) + s(doc.p.pop) + s(prop.aus) + offset(log(cases)), data = dat_cum3, family = 'nb', method = 'REML')
# a <- ggpredict(m3, terms = 'prop65')

m4 <- gam(deaths ~ s(prop65) + s(doc.p.pop) + s(prop.aus) + offset(log(cases)), data = dat_cum3, family = 'nb', method = 'REML')
a <- ggpredict(m4)
plot(a)

summary(m3)
summary(m4)

par(mfrow = c(2, 2))
gam.check(m3)
gam.check(m4)





# # Test penalty matrix construction:
# p_mat <- matrix(0, nrow = 96, ncol = 96)
# rownames(p_mat) = colnames(p_mat) = map_base$ARS
# diag(p_mat) <- card(nb)
# for (i in 1:96) {
#   for (j in nb[i][[1]]) {
#     p_mat[i, j] <- -1
#     # p_mat[j, i] <- -1
#   }
# }
# m5 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(penalty = p_mat), k =85) + offset(log(cases)), data = dat_cum3, family = 'nb', method = 'REML')
# # same results as m2; can edit penalty matrix to contain values other than 0 and -1

# Plot model fit:
dat_cum3 <- dat_cum3 %>%
  mutate(fit = predict(m1, type = 'response'),
         fit2 = predict(m2, type = 'response'),
         fit3 = predict(m3, type = 'response'),
         fit4 = predict(m4, type = 'response'),
         # coef = coefficients(m1),
         pred = m2$linear.predictors,
         res = residuals(m2))
map_fit <- map_base %>%
  left_join(dat_cum3, by = 'ARS')

diff_lim <- max(c(abs(min((map_fit$deaths - map_fit$fit2) / map_fit$cases)),
                  abs(max((map_fit$deaths - map_fit$fit2) / map_fit$cases))))

p.dat <- ggplot(map_fit) + geom_sf(aes(fill = deaths / cases)) + theme_void() + scale_fill_viridis()
p.fit.pois <- ggplot(map_fit) + geom_sf(aes(fill = fit / cases)) + theme_void() + scale_fill_viridis()
p.fit.nb <- ggplot(map_fit) + geom_sf(aes(fill = fit2 / cases)) + theme_void() + scale_fill_viridis()
p.fit.nb.covar <- ggplot(map_fit) + geom_sf(aes(fill = fit3 / cases)) + theme_void() + scale_fill_viridis()
p.fit.covar.only <- ggplot(map_fit) + geom_sf(aes(fill = fit4 / cases)) + theme_void() + scale_fill_viridis()
p.diff.obs.exp.nb <- ggplot(map_fit) + geom_sf(aes(fill = deaths/cases - fit2 / cases)) + theme_void() +
  scale_fill_distiller(palette = 'RdBu', limits = c(-diff_lim, diff_lim))
# # # ggplot(map_fit) + geom_sf(aes(fill = coef)) + theme_void() + scale_fill_viridis()
# ggplot(map_fit) + geom_sf(aes(fill = pred)) + theme_void() + scale_fill_viridis()
p.res.nb <- ggplot(map_fit) + geom_sf(aes(fill = res)) + theme_void() + scale_fill_distiller(palette = 'RdBu')

grid.arrange(p.dat, p.fit.pois, p.fit.nb, p.fit.nb.covar, p.fit.covar.only, ncol = 2)
grid.arrange(p.diff.obs.exp.nb, p.res.nb, ncol = 2)






# Try using lat/long instead of polygons:
map_cent <- st_centroid(map_base)
ggplot() + geom_sf(data = map_base) + geom_sf(data = map_cent) + theme_void()
# Note: eventually probably want to do population center and not centroid - centroids aren't necessarily within LK

map_base[, c('long', 'lat')] <- st_centroid(map_base) %>% st_transform(., '+proj=longlat') %>% st_coordinates()

dat_cum3 <- dat_cum3 %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)

n1 <- gam(deaths ~ s(long, lat, bs = 'gp', k = 65, m = 2) + offset(log(cases)), data = dat_cum3, family = 'poisson', method = 'REML') # or m=3
# plot(n1, pages = 1, scheme = 2)
# vis.gam(n1, view = c('long', 'lat'), plot.type = 'contour')
par(mfrow = c(2, 2))
gam.check(n1)

n2 <- gam(deaths ~ s(long, lat, bs = 'gp', k = 65, m = 2) + offset(log(cases)), data = dat_cum3, family = 'nb', method = 'REML')
gam.check(n2)

n2.pred <- ggpredict(n2)
plot(n2.pred$lat)
plot(n2.pred$long)

n2.pred <- ggpredict(n2, terms = c('long', 'lat'))
plot(n2.pred)

n3 <- gam(deaths ~ s(long, lat, bs = 'ds', k = 65, m = 2) + s(prop65) + s(doc.p.pop) + s(prop.aus) + offset(log(cases)), data = dat_cum3, family = 'nb', method = 'REML')
n4 <- gam(deaths ~ s(prop65) + s(doc.p.pop) + s(prop.aus) + offset(log(cases)), data = dat_cum3, family = 'nb', method = 'REML')

n3.pred <- ggpredict(n3)
n4.pred <- ggpredict(n4)

plot(n3.pred$lat)
plot(n3.pred$long)

plot(n3.pred$prop65)
plot(n4.pred$prop65)

plot(n3.pred$doc.p.pop)
plot(n4.pred$doc.p.pop)

plot(n3.pred$prop.aus)
plot(n4.pred$prop.aus)

n3.pred <- ggpredict(n3, terms = c('lat', 'long'))
plot(n3.pred)

par(mfrow = c(2, 2))
gam.check(n3)
gam.check(n4)

summary(n3)
summary(n4)

# Plot predictions:
dat_cum3 <- dat_cum3 %>%
  mutate(fit = predict(n1, type = 'response'),
         fit2 = predict(n2, type = 'response'),
         fit3 = predict(n3, type = 'response'),
         fit4 = predict(n4, type = 'response'))
map_fit <- map_base %>%
  left_join(dat_cum3, by = 'ARS')

p.dat <- ggplot(map_fit) + geom_sf(aes(fill = deaths / cases)) + theme_void() + scale_fill_viridis()
p.fit.pois <- ggplot(map_fit) + geom_sf(aes(fill = fit / cases)) + theme_void() + scale_fill_viridis()
p.fit.nb <- ggplot(map_fit) + geom_sf(aes(fill = fit2 / cases)) + theme_void() + scale_fill_viridis()
p.fit.nb.covar <- ggplot(map_fit) + geom_sf(aes(fill = fit3 / cases)) + theme_void() + scale_fill_viridis()
p.fit.covar.only <- ggplot(map_fit) + geom_sf(aes(fill = fit4 / cases)) + theme_void() + scale_fill_viridis()

grid.arrange(p.dat, p.fit.pois, p.fit.nb, p.fit.nb.covar, p.fit.covar.only, ncol = 2)






# Try including time as a covariate:
# Include interactions between time/space, and time/covars
dat_inc <- dat_inc %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)

n1 <- gam(deaths ~ s(long, lat, bs = 'ds', m = 2, k = 96) + s(Week, k = 35) + offset(log(cases)), data = dat_inc, family = 'nb', method = 'REML')

par(mfrow = c(2, 2))
gam.check(n1)

summary(n1)

n1.pred <- ggpredict(n1)
plot(n1.pred$lat)
plot(n1.pred$long)
plot(n1.pred$Week)

plot(n1, pages = 1, scheme = 2, shade = TRUE, scale = 0)

n2 <- bam(deaths ~ s(long, lat, bs = 'ds', m = 2, k = 96) + s(Week, k = 35) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(26, 35)) +
            offset(log(cases)),
          data = dat_inc, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

par(mfrow = c(2, 2))
gam.check(n2)
# m? add non-spatial random effect as well?

# explore where outliers are:
dat_inc$res1 <- n1$residuals
dat_inc$res2 <- n2$residuals

which(abs(dat_inc$res2) > 2.5)

outliers <- dat_inc %>%
  filter(res2 > 3 | res2 < -4)
# no patterns by LK or week
# one point where deaths somehow greater than cases - remove
# others just seem to be particularly high rates (and one 0)

# continue summarizing model:
summary(n2)

n2.pred <- ggpredict(n2)
plot(n2.pred$lat)
plot(n2.pred$long)
plot(n2.pred$Week)
plot(n2, pages = 1, scheme = 2, shade = TRUE, scale = 0)

pdata <- with(dat_inc,
              expand.grid(cases = 100,
                          Week = seq(min(Week), max(Week), by = 1),
                          long = seq(min(long), max(long), length = 100),
                          lat = seq(min(lat), max(lat), length = 100)))
n2.fit <- predict(n2, pdata)
# or add in observed case data and plot out predicted deaths / cases

ind <- exclude.too.far(pdata$long, pdata$lat, dat_inc$long, dat_inc$lat, dist = 0.1)
n2.fit[ind] <- NA

n2.pred <- cbind(pdata, fitted = n2.fit)

p3 <- ggplot(n2.pred, aes(x = long, y = lat)) + geom_raster(aes(fill = fitted)) +
  facet_wrap(~ Week, ncol = 10) +
  scale_fill_viridis(na.value = 'transparent') +
  coord_quickmap() + theme_void()
print(p3)





# Try including predictors:
n3 <- bam(deaths ~ s(long, lat, bs = 'ds', m = 2, k = 96) + s(Week, k = 35) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(26, 35)) +
            s(prop65, k = 75) + s(doc.p.pop, k = 62) + s(prop.aus, k = 62) + offset(log(cases)),
          data = dat_inc, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
n4 <- bam(deaths ~ s(Week, k = 35) + s(prop65, k = 75) + s(doc.p.pop, k = 62) + s(prop.aus, k = 62) + offset(log(cases)),
          data = dat_inc, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

par(mfrow = c(2, 2))
gam.check(n3)
gam.check(n4)

summary(n3)
summary(n4)

n3.pred <- ggpredict(n3)
n4.pred <- ggpredict(n4)

plot(n3.pred$lat)
plot(n3.pred$long)

plot(n3.pred$prop65)
plot(n4.pred$prop65)

plot(n3.pred$doc.p.pop)
plot(n4.pred$doc.p.pop)

plot(n3.pred$prop.aus)
plot(n4.pred$prop.aus)

plot(n3, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(n4, pages = 1, scheme = 2, shade = TRUE, scale = 0)




# And interactions between time and predictors?:
n5 <- bam(deaths ~ s(long, lat, bs = 'ds', m = 2, k = 96) + s(Week, k = 35) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(26, 35)) +
            s(prop65, k = 75) + s(doc.p.pop, k = 62) + s(prop.aus, k = 62) + ti(Week, prop.aus) +
            offset(log(cases)),
          data = dat_inc, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

par(mfrow = c(2, 2))
gam.check(n5)

plot(n5, pages = 1, scheme = 2, shade = TRUE, scale = 0)
n5.pred <- ggpredict(n5, terms = c('Week', 'prop.aus'))

plot(n5.pred)

summary(n5)




# Try with MRF instead?:
dat_inc <- dat_inc %>%
  mutate(ARS = factor(ARS))

m1 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 96) + s(Week, k = 35) + offset(log(cases)), data = dat_inc, family = 'nb', method = 'REML')

m2 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 96) + s(Week, k = 35) +
            te(Week, ARS, bs = c("fs", "mrf"), xt = list(Week = NULL, ARS = list(nb = nb))) +
            offset(log(cases)),
          data = dat_inc, family = 'nb', method = 'REML')
# m2 <- gam(deaths ~ te(Week, ARS, bs = c("fs", "mrf"), xt = list(Week = NULL, ARS = list(nb = nb))) + offset(log(cases)), data = dat_inc, family = 'nb', method = 'REML')

par(mfrow = c(2, 2))
gam.check(m2)

m3 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 96) + s(Week, k = 35) +
            te(Week, ARS, bs = c("fs", "mrf"), xt = list(Week = NULL, ARS = list(nb = nb))) +
            s(prop65, k = 75) + s(doc.p.pop, k = 62) + s(prop.aus, k = 62) + offset(log(cases)),
          data = dat_inc, family = 'nb', method = 'REML')
m4 <- gam(deaths ~ s(Week, k = 35) + s(prop65, k = 75) + s(doc.p.pop, k = 62) + s(prop.aus, k = 62) + offset(log(cases)),
          data = dat_inc, family = 'nb', method = 'REML')
m5 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb), k = 96) + s(Week, k = 35) +
            te(Week, ARS, bs = c("fs", "mrf"), xt = list(Week = NULL, ARS = list(nb = nb))) +
            s(prop65, k = 75) + s(doc.p.pop, k = 62) + s(prop.aus, k = 62) + ti(Week, prop.aus) +
            offset(log(cases)),
          data = dat_inc, family = 'nb', method = 'REML')

par(mfrow = c(2, 2))
gam.check(m3)
gam.check(m4)
gam.check(m5)

summary(m3)
summary(m4)
summary(m5)

plot(m3, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(m4, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(m5, pages = 1, scheme = 2, shade = TRUE, scale = 0)
# broadly, these are consistent with the results above using lat/long

pdata <- with(dat_inc,
              expand.grid(cases = 100,
                          ARS = ARS,
                          Week = seq(min(Week), max(Week), by = 1)))
m2.fit <- predict(m2, pdata)
m2.pred <- cbind(pdata, fitted = m2.fit)

map.m2pred <- map_base %>%
  left_join(m2.pred, by = 'ARS')

p4 <- ggplot(map.m2pred) + geom_sf(aes(fill = fitted)) + facet_wrap(~ Week, ncol = 10) +
  scale_fill_viridis(na.value = 'transparent') + theme_void()
print(p4)
































