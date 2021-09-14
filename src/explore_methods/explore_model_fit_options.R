# ---------------------------------------------------------------------------------------------------------------------
# 
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
source('src/functions/load_data.R')

# Calculate death rate as function of cases:
dat_inc <- dat_inc %>%
  mutate(death_rate_per_case = deaths / cases)

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

# ---------------------------------------------------------------------------------------------------------------------

# Limit data to a specific BL (for testing):
bl <- 'Brandenburg'
dat_inc <- dat_inc %>% filter(bundesland == bl)
# BadenWuerttemberg, Bayern, Berlin + Brandenburg, Hessen, MecklenburgVorpommern, Niedersachen + Bremen,
# NordrheinWestfalen, RheinlandPfalz, Saarland, Sachsen, SachsenAnhalt, SchleswigHolstein + Hamburg,
# Thueringen

# Separate data into df for cases and for deaths:
dat_cases <- dat_inc %>%
  select(date:lk, cases:ARS)
dat_deaths <- dat_inc

# Get case data for two weeks prior to deaths:
dat_deaths_lagged <- dat_deaths %>%
  mutate(Week = Week + 2) %>%
  select(Week:lk, cases) %>%
  rename('cases_lagged' = 'cases')
dat_deaths <- dat_deaths %>%
  left_join(dat_deaths_lagged, by = c('Week', 'lk')) %>%
  filter(Week <= 74)
rm(dat_deaths_lagged)

# Separate the data into two waves (leave out third for now - can download updated data and use this later):
dat_cases %>%
  group_by(Week) %>%
  summarise(med = mean(case_rate)) %>%
  plot()
dat_deaths %>%
  group_by(Week) %>%
  summarise(med = mean(death_rate_per_case)) %>%
  plot()

dat_cases_wave1 <- dat_cases %>%
  filter(Week >= 13 & Week <= 20)
dat_cases_wave2 <- dat_cases %>%
  filter(Week >= 40 & Week <= 59)
dat_deaths_wave1 <- dat_deaths %>%
  filter(Week >= 13 & Week <= 30) # 22
dat_deaths_wave2 <- dat_deaths %>%
  filter(Week >= 44 & Week <= 66) # 42-66

# Ideally, want to model deaths as a function of cases from ~2 weeks back; with first wave, data aren't available;
# but could be SA with second wave:
dat_deaths <- dat_deaths %>% select(-cases_lagged)
dat_deaths_wave1 <- dat_deaths_wave1 %>% select(-cases_lagged)

dat_deaths_wave2_lagged <- dat_deaths_wave2 %>%
  select(-c(cases, case_rate, death_rate)) %>%
  mutate(case_rate = cases_lagged / pop * 100000,
         death_rate = deaths / pop * 100000,
         death_rate_per_case = deaths / cases_lagged) %>%
  select(date:deaths, death_rate:death_rate_per_case, cases_lagged:case_rate, pop:ARS)
dat_deaths_wave2 <- dat_deaths_wave2 %>% select(-cases_lagged)

# Remove from death data where cases = 0:
dat_deaths <- dat_deaths %>%
  filter(cases > 0)
dat_deaths_wave1 <- dat_deaths_wave1 %>%
  filter(cases > 0)
dat_deaths_wave2 <- dat_deaths_wave2 %>%
  filter(cases > 0)
dat_deaths_wave2_lagged <- dat_deaths_wave2_lagged %>%
  filter(cases_lagged > 0)

# # Get data for single LK:
# dat_list_lk <- list(dat_cases, dat_cases_wave1, dat_cases_wave2,
#                     dat_deaths, dat_deaths_wave1, dat_deaths_wave2, dat_deaths_wave2_lagged)
# ggplot(data = dat_cases, aes(x = Week, y = case_rate)) + geom_line() + facet_wrap(~ lk) + theme_classic()
# ggplot(data = dat_deaths, aes(x = Week, y = death_rate_per_case)) + geom_line() + facet_wrap(~ lk) + theme_classic()
# # try 05112, 05966
# 
# dat_list_lk1 <- lapply(dat_list_lk, function(ix) {
#   ix <- ix %>% filter(lk == '05112')
# })
# dat_list_lk2 <- lapply(dat_list_lk, function(ix) {
#   ix <- ix %>% filter(lk == '05966')
# })

# Get cumulative deaths across first and second parts of first and second waves:
dat_cum <- read_csv('data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE.csv')

dat_cum <- dat_cum %>%
  mutate(Week = if_else(Year == 2020, Week, Week + 53)) %>%
  mutate(deaths = ifelse(deaths > cases, NA, deaths)) %>%
  mutate(death_rate = deaths / pop * 100000) %>%
  drop_na()

dat_cum <- dat_cum %>%
  mutate(bundesland = lk,
         bundesland = if_else(str_starts(lk, '01'), 'SchleswigHolstein', bundesland),
         bundesland = if_else(str_starts(lk, '02'), 'Hamburg', bundesland),
         bundesland = if_else(str_starts(lk, '03'), 'Niedersachsen', bundesland),
         bundesland = if_else(str_starts(lk, '04'), 'Bremen', bundesland),
         bundesland = if_else(str_starts(lk, '05'), 'NordrheinWestfalen', bundesland),
         bundesland = if_else(str_starts(lk, '06'), 'Hessen', bundesland),
         bundesland = if_else(str_starts(lk, '07'), 'RheinlandPfalz', bundesland),
         bundesland = if_else(str_starts(lk, '08'), 'BadenWuerttemberg', bundesland),
         bundesland = if_else(str_starts(lk, '09'), 'Bayern', bundesland),
         bundesland = if_else(str_starts(lk, '10'), 'Saarland', bundesland),
         bundesland = if_else(str_starts(lk, '11'), 'Berlin', bundesland),
         bundesland = if_else(str_starts(lk, '12'), 'Brandenburg', bundesland),
         bundesland = if_else(str_starts(lk, '13'), 'MecklenburgVorpommern', bundesland),
         bundesland = if_else(str_starts(lk, '14'), 'Sachsen', bundesland),
         bundesland = if_else(str_starts(lk, '15'), 'SachsenAnhalt', bundesland),
         bundesland = if_else(str_starts(lk, '16'), 'Thueringen', bundesland))

dat_cum <- dat_cum %>%
  mutate(ARS = factor(lk))

# wave1: 13-21, 22-30; wave2: 44-58, 59-66
dat_deaths_wave1_cum1 <- dat_cum %>%
  filter(Week == 21, bundesland == bl)
dat_deaths_wave1_cum2 <- dat_cum %>%
  filter(Week == 30, bundesland == bl) %>%
  inner_join(dat_deaths_wave1_cum1 %>% select(lk, deaths, cases), by = 'lk') %>%
  mutate(deaths = deaths.x - deaths.y,
         cases = cases.x - cases.y,
         deaths_past = deaths.y,
         cases_past = cases.y) %>%
  select(-c(deaths.x, cases.x, deaths.y, cases.y))

dat_deaths_wave2_cum1 <- dat_cum %>%
  filter(Week == 58, bundesland == bl) %>%
  inner_join(dat_cum %>% filter(Week == 43, bundesland == bl) %>% select(lk, deaths, cases), by = 'lk') %>%
  mutate(deaths = deaths.x - deaths.y,
         cases = cases.x - cases.y,
         deaths_past = deaths.y,
         cases_past = cases.y) %>%
  select(-c(deaths.x, cases.x, deaths.y, cases.y))
dat_deaths_wave2_cum2 <- dat_cum %>%
  filter(Week == 66, bundesland == bl) %>%
  inner_join(dat_deaths_wave2_cum1 %>% select(lk, deaths, cases), by = 'lk') %>%
  mutate(deaths = deaths.x - deaths.y,
         cases = cases.x - cases.y,
         deaths_past = deaths.y,
         cases_past = cases.y) %>%
  select(-c(deaths.x, cases.x, deaths.y, cases.y))

# ---------------------------------------------------------------------------------------------------------------------

# For single BL, try modeling time, space, and interaction for individual waves
# dat_deaths_wave1, dat_deaths_wave2, dat_deaths_wave2_lagged, (dat_cases_wave1, dat_cases_wave2)
dat_deaths_wave1 <- dat_deaths_wave1 %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)
dat_deaths_wave2 <- dat_deaths_wave2 %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)
dat_deaths_wave2_lagged <- dat_deaths_wave2_lagged %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)

dat_deaths %>% pull(lk) %>% unique() %>% length()

a1 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 53) + s(Week, k = 18) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(53, 18)) +
            offset(log(cases)),
          data = dat_deaths_wave1, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
a2 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 53) + s(Week, k = 23) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(53, 23)) +
            offset(log(cases)),
          data = dat_deaths_wave2, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
a3 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 53) + s(Week, k = 23) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(53, 23)) +
            offset(log(cases_lagged)),
          data = dat_deaths_wave2_lagged, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

par(mfrow = c(2, 2))
gam.check(a1, rep = 50)
gam.check(a2, rep = 50)
gam.check(a3, rep = 50)

# These actually do look much better, although gam.check saying k is too low for interaction (and sometimes for space)

# Do qqplots change if we lower k for the interaction?
a1 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 18) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(200, 10)) +
            offset(log(cases)),
          data = dat_deaths_wave1, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
a2 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 23) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(200, 15)) +
            offset(log(cases)),
          data = dat_deaths_wave2, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
a3 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 23) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(200, 15)) +
            offset(log(cases_lagged)),
          data = dat_deaths_wave2_lagged, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

par(mfrow = c(2, 2))
gam.check(a1, rep = 50)
gam.check(a2, rep = 50)
gam.check(a3, rep = 50)
# This may have made them a bit worse; would adding covariates help at all?
# Note also that not lagging the cases seems to work better, although this may be only for this specific BL;
# depending on how late cases are typically reported, a one-week lag may be better

plot(a1, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(a2, pages = 1, scheme = 2, shade = TRUE, scale = 0)

# Explore patterns in temporal autocorrelation of residuals:
acf(a1$residuals)
pacf(a1$residuals)

acf(a2$residuals)
pacf(a2$residuals)

acf(a3$residuals)
pacf(a3$residuals)
# But this doesn't break them down by LK, right?

dat_deaths_wave1$resid <- a1$residuals
dat_deaths_wave2$resid <- a2$residuals
dat_deaths_wave2_lagged$resid <- a3$residuals

list_wave1 <- split(dat_deaths_wave1, dat_deaths_wave1$lk)
par(mfrow = c(6, 6))
acfs_wave1 <- lapply(list_wave1, function(ix) {
  acf(ix$resid, lag.max = 10, plot = TRUE)
})
acfs_wave1 <- dat_deaths_wave1 %>%
  group_by(lk) %>%
  nest() %>%
  mutate(acfs = purrr::map(data, ~ acf(.x$resid, plot = FALSE)),
         acfs = purrr::map(acfs, ~ drop(.x$acf))) %>%
  unnest(acfs) %>%
  mutate(lag = seq(0, n() - 1))
# https://stackoverflow.com/questions/37325517/acf-by-group-in-r

list_wave2 <- split(dat_deaths_wave2, dat_deaths_wave2$lk)
par(mfrow = c(6, 6))
acfs_wave2 <- lapply(list_wave2, function(ix) {
  acf(ix$resid, lag.max = 10, plot = TRUE)
})
acfs_wave2 <- dat_deaths_wave2 %>%
  group_by(lk) %>%
  nest() %>%
  mutate(acfs = purrr::map(data, ~ acf(.x$resid, plot = FALSE)),
         acfs = purrr::map(acfs, ~ drop(.x$acf))) %>%
  unnest(acfs) %>%
  mutate(lag = seq(0, n() - 1))

# Here there is evidence of residual autocorrelation for some LK (although rare and slight)
# First explore what can be done with gamm for individual BL, then can try to apply to whole country
# But honestly doesn't look a ton worse with the full time series - could something else be causing the issues with the residuals?
# This issue doesn't occur when we use cumulative rates, so it must be something about including multiple timepoint; but that
# doesn't mean that temporal autocorrelation is the issue
# Unless even this little amount in some LK is itself an issue - can lead to the temporal component being overfit

dat_deaths <- dat_deaths %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)

# a4 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 53) + s(Week, k = 62) +
#             ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(25, 62)) +
#             offset(log(cases)),
#           data = dat_deaths, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

dat_deaths_red <- dat_deaths %>% filter(Week <= 66)
a5 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 53) + s(Week, k = 54) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(25, 54)) +
            offset(log(cases)),
          data = dat_deaths_red, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

par(mfrow = c(2, 2))
# gam.check(a4, rep = 50)
gam.check(a5, rep = 50)

# acf(a4$residuals)
# pacf(a4$residuals)
acf(a5$residuals)
pacf(a5$residuals)

# Here the problem with the residuals is worse again, but there doesn't seem to be substantial temporal autocorrelation

# How do these findings change with different BL?

# okay: BadenWuerttemberg, NordrheinWestfalen, 
# issues: Bayern, Brandenburg wave 2? (but small)





# ---------------------------------------------------------------------------------------------------------------------

# Try temporal model (for individual waves and whole time series) with a single LK
# dat_list_lk1, dat_list_lk2



# ---------------------------------------------------------------------------------------------------------------------

# For single BL, try 1) controlling for cases/deaths in the previous week, and 2) gamm
# Both for individual waves, and for entire time series
# dat_deaths_wave1, dat_deaths_wave2, dat_deaths_wave2_lagged, dat_deaths, (dat_cases_wave1, dat_cases_wave2, dat_cases)

dat_deaths_red <- dat_deaths_red %>%
  left_join(dat_deaths_red %>% select(Week, lk, deaths) %>% mutate(Week = Week + 1),
            by = c('Week', 'lk')) %>%
  rename('deaths' = 'deaths.x',
         'deaths_past' = 'deaths.y')

d1 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 53) + s(Week, k = 53) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(25, 53)) +
            s(deaths_past) + offset(log(cases)),
          data = dat_deaths_red, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

par(mfrow = c(2, 2))
gam.check(d1, rep = 50)

plot(d1, pages = 1, scheme = 2, shade = TRUE, scale = 0)

acf(d1$residuals)
pacf(d1$residuals)

# This actually appears to make the residuals look worse

d2_full <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 54) +
                  ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(100, 25)) +
                  offset(log(cases)),
                data = dat_deaths_red, family = 'nb')
d2_1 <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 18) +
               ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(100, 10)) +
               offset(log(cases)),
             data = dat_deaths_wave1, family = 'nb', correlation = corARMA(form = ~1|ARS, p = 1)) # or form = ~1|ARS?
d2_2 <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 23) +
               ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(100, 15)) +
               offset(log(cases)),
             data = dat_deaths_wave2, family = 'nb')




a1 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 18) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(200, 10)) +
            offset(log(cases)),
          data = dat_deaths_wave1, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
a2 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 23) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(200, 15)) +
            offset(log(cases)),
          data = dat_deaths_wave2, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)



# ---------------------------------------------------------------------------------------------------------------------

# Fit to monthly data (individual waves and full time series)
# 




# ---------------------------------------------------------------------------------------------------------------------

# Fit to cumulative rates over half-waves (no time component)
# dat_deaths_wave1_cum1, dat_deaths_wave1_cum2, dat_deaths_wave2_cum1, dat_deaths_wave2_cum2
dat_deaths_wave1_cum1 <- dat_deaths_wave1_cum1 %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)
dat_deaths_wave1_cum2 <- dat_deaths_wave1_cum2 %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)

dat_deaths_wave2_cum1 <- dat_deaths_wave2_cum1 %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)
dat_deaths_wave2_cum2 <- dat_deaths_wave2_cum2 %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)


n1_1 <- gam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 53) + offset(log(cases)),
            data = dat_deaths_wave1_cum1, family = 'nb')
n1_2 <- gam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 53) + offset(log(cases)),
            data = dat_deaths_wave1_cum2, family = 'nb')

n2_1 <- gam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 53) + offset(log(cases)),
            data = dat_deaths_wave2_cum1, family = 'nb')
n2_2 <- gam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 53) + offset(log(cases)),
            data = dat_deaths_wave2_cum2, family = 'nb')

par(mfrow = c(2, 2))
gam.check(n1_1, rep = 50)
gam.check(n1_2, rep = 50)
gam.check(n2_1, rep = 50)
gam.check(n2_2, rep = 50)

# n1_1.pred <- ggpredict(n1_1)
# plot(n1_1.pred$lat)
# plot(n1_1.pred$long)
# 
# plot(n1_1, pages = 1, scheme = 2, shade = TRUE, scale = 0)

pdata <- with(dat_deaths_wave1_cum1,
              expand.grid(cases = 100,
                          long = seq(min(long), max(long), length = 100),
                          lat = seq(min(lat), max(lat), length = 100)))

n1_1.fit <- predict(n1_1, pdata)
n1_2.fit <- predict(n1_2, pdata)
n2_1.fit <- predict(n2_1, pdata)
n2_2.fit <- predict(n2_2, pdata)

ind <- exclude.too.far(pdata$long, pdata$lat, dat_deaths_wave1_cum1$long, dat_deaths_wave1_cum1$lat, dist = 0.1)
n1_1.fit[ind] <- NA
n1_2.fit[ind] <- NA
n2_1.fit[ind] <- NA
n2_2.fit[ind] <- NA

n1_1.pred <- cbind(pdata, fitted = n1_1.fit)
n1_2.pred <- cbind(pdata, fitted = n1_2.fit)
n2_1.pred <- cbind(pdata, fitted = n2_1.fit)
n2_2.pred <- cbind(pdata, fitted = n2_2.fit)

p1 <- ggplot(n1_1.pred, aes(x = long, y = lat)) + geom_raster(aes(fill = fitted)) +
  scale_fill_viridis(na.value = 'transparent') + coord_quickmap() + theme_void()
p2 <- ggplot(n1_2.pred, aes(x = long, y = lat)) + geom_raster(aes(fill = fitted)) +
  scale_fill_viridis(na.value = 'transparent') + coord_quickmap() + theme_void()
p3 <- ggplot(n2_1.pred, aes(x = long, y = lat)) + geom_raster(aes(fill = fitted)) +
  scale_fill_viridis(na.value = 'transparent') + coord_quickmap() + theme_void()
p4 <- ggplot(n2_2.pred, aes(x = long, y = lat)) + geom_raster(aes(fill = fitted)) +
  scale_fill_viridis(na.value = 'transparent') + coord_quickmap() + theme_void()
grid.arrange(p1, p2, p3, p4, ncol = 2)

# ---------------------------------------------------------------------------------------------------------------------

# Does adding covariates help?
# dat_deaths_wave1, dat_deaths_wave2





# ---------------------------------------------------------------------------------------------------------------------

# Explore model residuals with dharma package




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
















