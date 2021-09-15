# ---------------------------------------------------------------------------------------------------------------------
# 
#
# https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
# https://jroy042.github.io/nonlinear/week4.html
# https://m-clark.github.io/generalized-additive-models/appendix.html#time-and-space
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
bl <- 'RheinlandPfalz'
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

# Remove if NO DEATHS through entire wave/pandemic?:
to_remove <- dat_deaths_wave1 %>% group_by(lk) %>% summarise(tot_deaths = sum(deaths)) %>% filter(tot_deaths == 0) %>% pull(lk)
dat_deaths_wave1 <- dat_deaths_wave1 %>% filter(!(lk %in% to_remove))
# But this doesn't really happen in second wave/full pandemic...


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

dat_deaths <- dat_deaths %>%
  left_join(map_base[, c('ARS', 'long', 'lat')],
            by = 'ARS') %>%
  select(-geometry)
dat_deaths_red <- dat_deaths %>% filter(Week <= 66)

dat_deaths %>% pull(lk) %>% unique() %>% length()
lk_num <- dat_deaths %>% pull(lk) %>% unique() %>% length()

a1 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 18) +
            # ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(53, 18)) +
            offset(log(cases)),
          data = dat_deaths_wave1, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
a2 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 23) +
            # ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(53, 23)) +
            offset(log(cases)),
          data = dat_deaths_wave2, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
a3 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 23) +
            # ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(53, 23)) +
            offset(log(cases_lagged)),
          data = dat_deaths_wave2_lagged, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
a4 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 54) +
            # ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(25, 54)) +
            offset(log(cases)),
          data = dat_deaths_red, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

par(mfrow = c(2, 2))
# gam.check(a1, rep = 50)
gam.check(a2, rep = 50)
gam.check(a3, rep = 50)
# gam.check(a4, rep = 50)

# With no interaction, residuals show issues, particularly with longer time series

b1 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 18) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(floor(lk_num / 3), 18)) +
            offset(log(cases)),
          data = dat_deaths_wave1, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
b2 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 23) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(floor(lk_num / 3), 23)) +
            offset(log(cases)),
          data = dat_deaths_wave2, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
b4 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 54) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(floor(lk_num / 3), 54)) +
            offset(log(cases)),
          data = dat_deaths_red, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

# par(mfrow = c(2, 2))
# gam.check(b1, rep = 50)
# gam.check(b2, rep = 50)
# gam.check(b4, rep = 50)

d1 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 18) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(lk_num, 18)) +
            offset(log(cases)),
          data = dat_deaths_wave1, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
d2 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 23) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(lk_num, 23)) +
            offset(log(cases)),
          data = dat_deaths_wave2, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
# d4 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 54) +
#             ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(lk_num, 54)) +
#             offset(log(cases)),
#           data = dat_deaths_red, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

par(mfrow = c(2, 2))
gam.check(a1, rep = 50)
gam.check(b1, rep = 50)
gam.check(d1, rep = 50)
gam.check(a2, rep = 50)
gam.check(b2, rep = 50)
gam.check(d2, rep = 50)
gam.check(a4, rep = 50)
gam.check(b4, rep = 50)
# gam.check(d4, rep = 50)
# Even including the maximum values of k does not fix the residuals
# And in fact, increasing k doesn't much impact fit of time smooth either

# Note also that not lagging the cases seems to work better, although this may be only for this specific BL;
# depending on how late cases are typically reported, a one-week lag may be better

plot(a1, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(a2, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(a3, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(a4, pages = 1, scheme = 2, shade = TRUE, scale = 0)

# Explore patterns in temporal autocorrelation of residuals:
dat_deaths_wave1$resid <- d1$residuals
dat_deaths_wave2$resid <- d2$residuals
# dat_deaths_wave2_lagged$resid <- a3$residuals
# dat_deaths_red$resid <- d4$residuals

list_wave1 <- split(dat_deaths_wave1, dat_deaths_wave1$lk)
par(mfrow = c(4, 6))
acfs_wave1 <- lapply(list_wave1, function(ix) {
  acf(ix$resid, lag.max = 10, plot = TRUE)
})
# acfs_wave1 <- dat_deaths_wave1 %>%
#   group_by(lk) %>%
#   nest() %>%
#   mutate(acfs = purrr::map(data, ~ acf(.x$resid, plot = FALSE)),
#          acfs = purrr::map(acfs, ~ drop(.x$acf))) %>%
#   unnest(acfs) %>%
#   mutate(lag = seq(0, n() - 1))
# # https://stackoverflow.com/questions/37325517/acf-by-group-in-r

list_wave2 <- split(dat_deaths_wave2, dat_deaths_wave2$lk)
par(mfrow = c(4, 6))
acfs_wave2 <- lapply(list_wave2, function(ix) {
  acf(ix$resid, lag.max = 10, plot = TRUE, main = unique(ix$lk))
})

list_waves12 <- split(dat_deaths_red, dat_deaths_red$lk)
par(mfrow = c(4, 6))
acfs_waves12 <- lapply(list_waves12, function(ix) {
  acf(ix$resid, lag.max = 10, plot = TRUE)
})

# Even with maximum k on interaction, evidence of residual temporal autocorrelation for second wave
# and for both waves together
# We know from below that trying to also use gamm to model the temporal autocorrelation tends to reduce
# the edf for the spatial effect
# But note that residuals don't look wonderful even for d1, where there is no noticeable autocorrelation

# Explore data for LK where autocorrelation still seems to be an issue:
ggplot(data = dat_deaths_wave2, aes(x = Week, y = deaths, group = lk)) + geom_line() +
  geom_line(data = dat_deaths_wave2 %>% filter(lk %in% c('07141', '07319', '07337')),
            aes(x = Week, y = deaths), col = 'coral', lwd = 1) +
  theme_classic()
# c('07141', '07319', '07337')
c('07235', '07135', '07315', '07312')
# These don't look unusual in any way; but autocorrelation also not super strong
# I guess they all show periods of time where there is the same (low) number of deaths every week

dat_deaths_wave2 %>% group_by(lk) %>% summarise(tot_deaths = sum(deaths)) %>% print(n = 36)
dat_deaths_wave2 %>% group_by(lk) %>% summarise(med_res = median(resid)) %>% print(n = 36)
# No clear patterns

ggplot(data = dat_deaths_wave2, aes(x = Week, y = resid, group = lk)) + geom_line() +
  geom_line(data = dat_deaths_wave2 %>% filter(lk %in% c('07141', '07319', '07337')),
            aes(x = Week, y = resid), col = 'coral', lwd = 1) +
  theme_classic()
# These also don't seem to be the LK with super high residuals

dat_deaths_wave2 %>% filter(resid > 3)
ggplot(data = dat_deaths_wave2, aes(x = Week, y = deaths/cases, group = lk)) + geom_line() +
  geom_line(data = dat_deaths_wave2 %>% filter(lk %in% c('07235', '07135', '07315', '07312')),
            aes(x = Week, y = deaths/cases), col = 'coral', lwd = 1) +
  theme_classic()# + facet_wrap(~ lk)
# These residuals seem to happen when there is a spike in the death count - is noisiness to some extent the issue?
# But these LK don't always look way noisier than the others

# If noise is an issue, using monthly data could help


# ---------------------------------------------------------------------------------------------------------------------

# How do these findings change with different BL?

# issues/autocorrelation: NordrheinWestfalen


# okay: BadenWuerttemberg, NordrheinWestfalen, 
# issues: Bayern, Brandenburg wave 2? (but small)





# ---------------------------------------------------------------------------------------------------------------------

# For single BL, try 1) controlling for cases/deaths in the previous week, and 2) gamm
# Both for individual waves, and for entire time series
# dat_deaths_wave1, dat_deaths_wave2, dat_deaths_wave2_lagged, dat_deaths, (dat_cases_wave1, dat_cases_wave2, dat_cases)

# dat_deaths_red <- dat_deaths_red %>%
#   left_join(dat_deaths_red %>% select(Week, lk, deaths) %>% mutate(Week = Week + 1),
#             by = c('Week', 'lk')) %>%
#   rename('deaths' = 'deaths.x',
#          'deaths_past' = 'deaths.y')
# 
# d1 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 53) + s(Week, k = 53) +
#             # ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(25, 53)) +
#             s(deaths_past) + offset(log(cases)),
#           data = dat_deaths_red, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
# 
# par(mfrow = c(2, 2))
# gam.check(d1, rep = 50)
# 
# plot(d1, pages = 1, scheme = 2, shade = TRUE, scale = 0)
# 
# # This actually appears to make the residuals look worse

a5 <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, bs = 'cr', k = 54) +
             # ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(5, 5)) +
             offset(log(cases)),
           data = dat_deaths_red, family = 'nb')
a1 <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 18) +
             offset(log(cases)),
           data = dat_deaths_wave1, family = 'nb')
a2 <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 23) +
             offset(log(cases)),
           data = dat_deaths_wave2, family = 'nb')

d_full <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, bs = 'cr', k = 54) +
                 # ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'cr'), m = list(c(1.0, 0.5), NA), k = c(5, 5)) +
                 offset(log(cases)),
               data = dat_deaths_red, family = 'nb', correlation = corARMA(form = ~Week|ARS, p = 1))
d1 <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 18) +
             offset(log(cases)),
           data = dat_deaths_wave1, family = 'nb', correlation = corARMA(form = ~Week|ARS, p = 1)) # form = ~1|ARS or ~Week|ARS?
d2 <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 23) +
             offset(log(cases)),
           data = dat_deaths_wave2, family = 'nb', correlation = corARMA(form = ~Week|ARS, p = 1))

# d_full_2 <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 54) +
#                  offset(log(cases)),
#                data = dat_deaths_red, family = 'nb', correlation = corARMA(form = ~1|ARS, p = 2))
d1_2 <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 18) +
               offset(log(cases)),
             data = dat_deaths_wave1, family = 'nb', correlation = corARMA(form = ~1|ARS, p = 2)) # form = ~1|ARS or ~Week|ARS?
d2_2 <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 23) +
               offset(log(cases)),
             data = dat_deaths_wave2, family = 'nb', correlation = corARMA(form = ~1|ARS, p = 2))


d_full_3 <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 54) +
                   offset(log(cases)),
                 data = dat_deaths_red, family = 'nb', correlation = corARMA(form = ~Week|ARS, p = 3))
# d1_3 <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 18) +
#              offset(log(cases)),
#            data = dat_deaths_wave1, family = 'nb', correlation = corARMA(form = ~1|ARS, p = 3)) # form = ~1|ARS or ~Week|ARS?
# d2_3 <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = lk_num) + s(Week, k = 23) +
#              offset(log(cases)),
#            data = dat_deaths_wave2, family = 'nb', correlation = corARMA(form = ~1|ARS, p = 3))

a1$lme$logLik
d1$lme$logLik

a2$lme$logLik
d2$lme$logLik

a5$lme$logLik
d_full$lme$logLik
d_full_3$lme$logLik
# only actually improves fit for second wave
# using LK-level RE might improve fit though; any way to do nested RE in gams?

plot(a1$gam, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(d1$gam, pages = 1, scheme = 2, shade = TRUE, scale = 0)

plot(a2$gam, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(d2$gam, pages = 1, scheme = 2, shade = TRUE, scale = 0)

plot(a5$gam, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(d_full$gam, pages = 1, scheme = 2, shade = TRUE, scale = 0)

par(mfrow = c(2, 2))
gam.check(a1$gam, rep = 50)
gam.check(d1$gam, rep = 50)

gam.check(a2$gam, rep = 50)
gam.check(d2$gam, rep = 50)

gam.check(a5$gam, rep = 50)
gam.check(d_full$gam, rep = 50)
# Residuals appear unchanged; sometimes makes k too low for spatial effect though?
# Also seems difficult to get model to fit with ARMA and space-time interaction
# RE again has little influence on residuals

dat_deaths_wave1$resid <- resid(d1$lme, type = 'normalized')
dat_deaths_wave2$resid <- resid(d2$lme, type = 'normalized')
dat_deaths_red$resid <- resid(d_full$lme, type = 'normalized')

list_wave1 <- split(dat_deaths_wave1, dat_deaths_wave1$lk)
par(mfrow = c(4, 6))
acfs_wave1 <- lapply(list_wave1, function(ix) {
  acf(ix$resid, lag.max = 10, plot = TRUE)
})

list_wave2 <- split(dat_deaths_wave2, dat_deaths_wave2$lk)
par(mfrow = c(4, 6))
acfs_wave2 <- lapply(list_wave2, function(ix) {
  acf(ix$resid, lag.max = 10, plot = TRUE)
})

list_waves12 <- split(dat_deaths_red, dat_deaths_red$lk)
par(mfrow = c(4, 6))
acfs_waves12 <- lapply(list_waves12, function(ix) {
  acf(ix$resid, lag.max = 10, plot = TRUE)
})
# And some LK still have noticeable autocorrelation - increase p and q?
# In some cases it even appears to make autocorrelation worse - some tradeoff with space?


# 05770, 05754, 05562, 05154, 05124, 05112

# Fit individually; fit without; fit only

dat1 <- dat_deaths_red %>% filter(lk %in% c('05770', '05754', '05562', '05154', '05124', '05112')) %>%
  mutate(ARS = factor(ARS))
dat2 <- dat_deaths_red %>% filter(!(lk %in% c('05770', '05754', '05562', '05154', '05124', '05112'))) %>%
  mutate(ARS = factor(ARS))
dat3 <- dat_deaths_red %>% filter(lk == '05112')

a1 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 6) + s(Week, k = 54) +
            offset(log(cases)),
          data = dat1, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
a2 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 47) + s(Week, k = 54) +
            offset(log(cases)),
          data = dat2, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
a3 <- bam(deaths ~ s(Week, k = 54) + offset(log(cases)),
          data = dat3, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

d1 <- gamm(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 6) + s(Week, k = 54) +
             # ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(6, 54)) +
             offset(log(cases)), random = list(ARS=~1),
           data = dat1, family = 'nb', correlation = corCAR1())

par(mfrow = c(2, 2))
gam.check(a1, rep = 50)
gam.check(a2, rep = 50)
gam.check(a3, rep = 50)

acf(a3$residuals) # actually no temporal autocorrelation when only one LK included
# Model has issue fitting temporal autocorrelation for many LK at once?

dat1$resid <- resid(d1$lme, type = 'normalized')
for (i in levels(dat1$ARS)) {
  acf(dat1$resid[dat1$ARS == i])
}
# Again, seems to be a tradeoff between spatial fit and temporal autocorrelation

b1 <- bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 6) + s(Week, k = 54) +
            ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(6, 54)) +
            offset(log(cases)),
          data = dat1, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
par(mfrow = c(2, 2))
gam.check(b1, rep = 50)
# Temporal autocorrelation already much better by including time-space interaction!
# Incorporating time-space interaction and temporal autocorrelation in residuals leads to smoothest time fxn, but
# also leads to a lot of smoothing in other terms






# ---------------------------------------------------------------------------------------------------------------------

# Fit to monthly data
# Could reduce noisiness of the signal and size of the problem, but sacrifices temporal granularity




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
