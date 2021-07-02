# ---------------------------------------------------------------------------------------------------------------------
# Code to assess temporal/spatial dynamics of transmission, and fit model with no predictors
# ---------------------------------------------------------------------------------------------------------------------

# Load libraries:
library(tidyverse)
library(mgcv)
library(sf)
library(testthat)
library(spdep)

# ---------------------------------------------------------------------------------------------------------------------

# Read in incident mortality data:
dat_inc <- read_csv('data/formatted/weekly_covid_deaths_by_lk_INCIDENT.csv')

# Format data for model fitting:
dat_inc <- dat_inc %>%
  mutate(Week = if_else(Year == 2020, Week, Week + 53)) %>%
  mutate(time = Week - min(Week) + 1) %>%
  # mutate(time = Week) %>%
  drop_na()

dat_inc <- dat_inc %>%
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

# ggplot(data = dat_inc, aes(x = time, y = death_rate, group = lk)) + geom_line() + theme_classic()

# Get Landkreise as factor:
dat_inc <- dat_inc %>%
  mutate(ARS = factor(lk))

# Separate into first and second waves:
dat_wave1 <- dat_inc %>%
  filter(Week <= 22)
dat_wave2 <- dat_inc %>%
  filter(Week >= 40)

# ggplot(data = dat_wave1, aes(x = time, y = death_rate, group = lk)) + geom_line() + theme_classic()
# ggplot(data = dat_wave2, aes(x = time, y = death_rate, group = lk)) + geom_line() + theme_classic()

# ---------------------------------------------------------------------------------------------------------------------

# Fit GAM with no additional predictors:

# Over time:
m1 <- gamm(deaths ~ s(time) + offset(pop), random = list(bundesland = ~1, lk = ~1), data = dat_wave2, correlation = corAR1(form = ~time), family = poisson())
m2 <- gamm(deaths ~ s(time) + offset(pop), random = list(bundesland = ~1, lk = ~1), data = dat_wave2, correlation = corAR1(form = ~time), family = nb())

plot(m1$gam)
plot(m2$gam)

gam.check(m1$gam)
gam.check(m2$gam)

acf(dat_inc$death_rate)
acf(m1$gam$residuals)
acf(m2$gam$residuals)

# Incorporating spatial patterns:
map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')

expect_true(all(unique(map_base$ARS) %in% unique(dat_inc$lk)))
expect_true(all(unique(dat_inc$lk) %in% unique(map_base$ARS)))

nb <- spdep::poly2nb(map_base, row.names = map_base$ARS)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

# map_base$neighbors <- card(nb)
# ggplot(map_base) + geom_sf() + geom_sf(aes(fill = neighbors)) + theme_void()

m3 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)), data = dat_wave2, family = poisson())
m4 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)) + s(time), data = dat_wave2, family = poisson())

m5 <- gam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)) + s(time) + offset(pop), data = dat_wave2, family = poisson())
# Error in gam.fit3(x = X, y = y, sp = L %*% lsp + lsp0, Eb = Eb, UrS = UrS,  : 
#   innere Schleife 1; Schrittweite kann nicht korrigiert werden
# In addition: Warning message:
# Schrittweite wurde wegen Divergenz reduziert

dat_wave2_fac <- dat_wave2 %>% mutate(bundesland = factor(bundesland))
m6 <- bam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)) + s(time) + offset(pop) + s(bundesland, bs = 're'), data = dat_wave2_fac, family = poisson, nthreads = 4)
m6.1 <- bam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)) + s(time) + offset(pop) + s(bundesland, bs = 're') + s(ARS, bs = 'mrf', xt = list(nb = nb), by = time), data = dat_wave2_fac, family = poisson, nthreads = 4)
m6.21 <- bam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)) + s(time, by = ARS) + offset(pop) + s(bundesland, bs = 're'), data = dat_wave2_fac, family = poisson, nthreads = 4)
# m6.2 <- bam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)) + s(time) + offset(pop) + s(bundesland, bs = 're') + s(ARS, time, bs = 'fs', m = 1), data = dat_wave2_fac, family = poisson, nthreads = 4) # too long to fit
m6.22 <- bam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)) + s(time) + offset(pop) + s(bundesland, bs = 're') + s(time, ARS, bs = 'fs', m = 1), data = dat_wave2_fac, family = poisson, nthreads = 4) # too long to fit
# m6.5 <- bam(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)) + s(time) + offset(pop) + s(bundesland, bs = 're') + ti(ARS, time, bs = 'mrf', xt = list(nb = nb)), data = dat_wave2, family = poisson, nthreads = 4)
# https://www.rdocumentation.org/packages/mgcv/versions/1.8-35/topics/smooth.construct.fs.smooth.spec

# ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B")
# ctrl <- gam.control(nthreads = 6)
m7 <- bake(file = 'results/m7.rds',
           expr = gamm(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)) + s(time) + offset(pop), random = list(bundesland = ~1), data = dat_wave2, correlation = corAR1(form = ~time | ARS), family = poisson())#, control = ctrl)
)
m8 <- bake(file = 'results/m8.rds',
           expr = gamm(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)) + s(time) + offset(pop), random = list(bundesland = ~1), data = dat_wave2, correlation = corAR1(form = ~1 | ARS), family = poisson()))
m9 <- bake(file = 'results/m9.rds',
           expr = gamm(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)) + s(time) + offset(pop), random = list(bundesland = ~1), data = dat_wave2, correlation = corARMA(form = ~time | ARS, p = 3), family = poisson()))
# m10 <- gamm(deaths ~ s(ARS, bs = 'mrf', xt = list(nb = nb)) + s(time) + offset(pop), random = list(bundesland = ~1), data = dat_wave2, correlation = corARMA(form = ~1 | ARS, p = 3), family = poisson())
# takes a really long time to fit - ultimately, time needs higher number of knots, and some convergence issues, but does seem to run

plot(m7$gam)
gam.check(m7$gam)
acf(m7$gam$residuals)
pacf(m7$gam$residuals)

# write_rds(m7, file = 'results/m7.rds')
# write_rds(m8, file = 'results/m8.rds')
# write_rds(m9, file = 'results/m9.rds')
# m7 and m8 are very similar, but not exactly the same; same results if you run it several times? - yes

# Take case counts into account (instead of population data):





# ---------------------------------------------------------------------------------------------------------------------

# # Fit BYM2 model with no predictors:
# library(INLA)
# library(gridExtra)
# library(viridis)
# 
# # Read in cumulative data:
# dat_c <- read_csv('data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE.csv')
# 
# # Format data for model fitting:
# dat_c <- dat_c %>%
#   mutate(Week = if_else(Year == 2020, Week, Week + 53)) %>%
#   drop_na()
# 
# dat_c <- dat_c %>%
#   mutate(bundesland = lk,
#          bundesland = if_else(str_starts(lk, '01'), 'SchleswigHolstein', bundesland),
#          bundesland = if_else(str_starts(lk, '02'), 'Hamburg', bundesland),
#          bundesland = if_else(str_starts(lk, '03'), 'Niedersachsen', bundesland),
#          bundesland = if_else(str_starts(lk, '04'), 'Bremen', bundesland),
#          bundesland = if_else(str_starts(lk, '05'), 'NordrheinWestfalen', bundesland),
#          bundesland = if_else(str_starts(lk, '06'), 'Hessen', bundesland),
#          bundesland = if_else(str_starts(lk, '07'), 'RheinlandPfalz', bundesland),
#          bundesland = if_else(str_starts(lk, '08'), 'BadenWuerttemberg', bundesland),
#          bundesland = if_else(str_starts(lk, '09'), 'Bayern', bundesland),
#          bundesland = if_else(str_starts(lk, '10'), 'Saarland', bundesland),
#          bundesland = if_else(str_starts(lk, '11'), 'Berlin', bundesland),
#          bundesland = if_else(str_starts(lk, '12'), 'Brandenburg', bundesland),
#          bundesland = if_else(str_starts(lk, '13'), 'MecklenburgVorpommern', bundesland),
#          bundesland = if_else(str_starts(lk, '14'), 'Sachsen', bundesland),
#          bundesland = if_else(str_starts(lk, '15'), 'SachsenAnhalt', bundesland),
#          bundesland = if_else(str_starts(lk, '16'), 'Thueringen', bundesland))
# 
# # Get Landkreise as factor:
# dat_c <- dat_c %>%
#   mutate(ARS = factor(lk))
# 
# # Separate into first and second waves:
# dat_wave1 <- dat_c %>%
#   filter(Week <= 22)
# dat_wave2 <- dat_c %>%
#   filter(Week >= 40)
# 
# # Limit to week(s) of interest (choose one for now; expand later):
# dat_wave2_snap <- dat_wave2 %>%
#   filter(Week == max(Week))
# 
# # Then fit BYM2 model w/o covariates:
# map_df2 <- map_base %>% left_join(dat_wave2_snap, by = 'ARS')
# 
# map_df2$idarea <- 1:nrow(map_df2)
# map_df2$ARS <- factor(map_df2$ARS)
# 
# nb <- spdep::poly2nb(map_df2, row.names = map_df2$ARS)
# # attr(nb, 'region.id') <- map_base$ARS
# # names(nb) <- attr(nb, 'region.id')
# nb2INLA('map.adj', nb)
# g <- inla.read.graph(filename = 'map.adj')
# 
# prior <- list(
#   prec = list(
#     prior = "pc.prec",
#     param = c(0.5 / 0.31, 0.01)),
#   phi = list(
#     prior = "pc",
#     param = c(0.5, 2 / 3))
# )
# 
# formula <- deaths ~ f(idarea + offset(pop), model = 'bym2', graph = g, hyper = prior) + f(bundesland, model = 'iid')
# # include random effect for BL - is this correct?; what about offset?
# 
# res <- inla(formula, family = 'poisson', data = map_df2, E = expected, verbose = F, control.predictor = list(compute = TRUE))
# # res2 <- inla(formula, family = 'poisson', data = map_df2, verbose = T)
# 
# summary(res)
# head(res$summary.fitted.values)
# 
# map_df2$RR <- res$summary.fitted.values$mean
# map_df2$UL <- res$summary.fitted.values$`0.975quant`
# map_df2$LL <- res$summary.fitted.values$`0.025quant`
# map_df2$median <- res$summary.fitted.values$`0.5quant`
# map_df2$sd <- res$summary.fitted.values$sd
# 
# p.res.rr <- ggplot(map_df2) + geom_sf(aes(fill = log(RR))) + 
#   theme_void() + labs(fill = 'RR') +
#   scale_fill_distiller(palette = 'RdBu', na.value = 'gray75',
#                        limits = c(-1 * lim, lim),
#                        breaks = log(log_labs),
#                        labels = log_labs)
# p.res.ll <- ggplot(map_df2) + geom_sf(aes(fill = log(LL))) + 
#   theme_void() + labs(fill = 'LL') +
#   scale_fill_distiller(palette = 'RdBu', na.value = 'gray75',
#                        limits = c(-1 * lim, lim),
#                        breaks = log(log_labs),
#                        labels = log_labs)
# p.res.ul <- ggplot(map_df2) + geom_sf(aes(fill = log(UL))) + 
#   theme_void() + labs(fill = 'UL') +
#   scale_fill_distiller(palette = 'RdBu', na.value = 'gray75',
#                        limits = c(-1 * lim, lim),
#                        breaks = log(log_labs),
#                        labels = log_labs)
# 
# grid.arrange(p2, p.res.rr, p.res.ll, p.res.ul, ncol = 2)
# 
# p.sd <- ggplot(map_df2) + geom_sf(aes(fill = sd)) +
#   theme_void() + labs(fill = 'St. Dev.') +
#   scale_fill_viridis()
# p.sd
# 
# map_df2$re <- res$summary.random$idarea[1:401, 'mean']
# lim_re <- max(max(map_df2$re), abs(min(map_df2$re)))
# 
# p.re <- ggplot(map_df2) + geom_sf(aes(fill = re)) +
#   theme_void() + labs(fill = 'Ran. Eff.') +
#   scale_fill_distiller(palette = 'RdBu', limits = c(-lim_re, lim_re))
# p.re

# ---------------------------------------------------------------------------------------------------------------------

# Old code to add in predictors:

# Read in and format data

# Read in incident data:
dat_inc <- read_csv('data/formatted/weekly_covid_deaths_by_lk_INCIDENT.csv')

# Format:
dat_inc <- dat_inc %>%
  mutate(Week = if_else(Year == 2020, Week, Week + 53)) %>%
  # mutate(time = Week - min(Week) + 1) %>%
  # filter(deaths <= cases) %>%
  mutate(deaths = ifelse(deaths > cases, NA, deaths)) %>%
  mutate(death_rate = deaths / pop * 100000) %>%
  drop_na()

# Add column for Bundesland:
dat_inc <- dat_inc %>%
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

# Get Landkreise as factor:
dat_inc <- dat_inc %>%
  mutate(ARS = factor(lk))

#Plot:
p1 <- ggplot(data = dat_inc, aes(x = Week, y = case_rate, group = lk)) +
  geom_line(alpha = 0.2) + theme_classic()
p2 <- ggplot(data = dat_inc, aes(x = Week, y = death_rate, group = lk)) +
  geom_line(alpha = 0.2) + theme_classic()
grid.arrange(p1, p2, ncol = 1)

# ---------------------------------------------------------------------------------------------------------------------

# Add covariates
age_dist <- read_csv('data/formatted/age_dist.csv')
dat_inc <- dat_inc %>%
  left_join(age_dist, by = 'lk')

mig_dat <- read_csv('data/formatted/mig_hosp_dat.csv')
dat_inc <- dat_inc %>%
  left_join(mig_dat, by = 'lk')

# ---------------------------------------------------------------------------------------------------------------------

# # Plot covariates against incidence/mortality/each other
# dat_inc %>%
#   filter(Week == 55) %>%
#   select(death_rate, case_rate, pop, prop65, prop.aus:cit.p.pop) %>%
#   pairs(pch = 20)

# ---------------------------------------------------------------------------------------------------------------------

# # Map covariates
# dat_covar <- map_base %>%
#   left_join(dat_inc %>%
#               pivot_longer(c(pop, prop65, prop.aus:cit.p.pop), names_to = 'var') %>%
#               select(ARS, var:value) %>%
#               unique(),
#             by = 'ARS') %>%
#   mutate(var = factor(var))
# 
# for (ix in levels(dat_covar$var)) {
#   p <- ggplot(data = dat_covar[dat_covar$var == ix, ]) + geom_sf(aes(fill = value)) +
#     theme_void() + scale_fill_viridis() + labs(title = ix)
#   print(p)
# }

# ---------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------
# Try adding predictors (for now just as proof of concept)
tic <- Sys.time()
n3b <- bake(file = 'results/fitted_models/n3b_TEST.rds',
            expr = {
              bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 62) + 
                    ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(200, 20)) +
                    s(prop65, k = 100) + s(hosp.beds, k = 100) + #s(prop.aus) +
                    s(bundesland, bs = 're', k = 16) + offset(log(cases)),
                  data = dat_inc_fromCases, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
            }
)
toc <- Sys.time()
print(toc - tic)
# 1.40 hours

n4b <- bam(deaths ~ s(Week, k = 62) + s(prop65, k = 100) + s(hosp.beds, k = 100) +
             s(bundesland, bs = 're', k = 16) + offset(log(cases)),
           data = dat_inc_fromCases, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)

par(mfrow = c(2, 2))
gam.check(n3b)
gam.check(n4b)

# summary(n3b)
# summary(n4b)

n3b.pred <- ggpredict(n3b)
n4b.pred <- ggpredict(n4b)

plot(n3b.pred$lat)
plot(n3b.pred$long)

plot(n3b.pred$Week)
plot(n4b.pred$Week)

plot(n3b.pred$prop65)
plot(n4b.pred$prop65)

plot(n3b.pred$hosp.beds)
plot(n4b.pred$hosp.beds)

plot(n3b.pred$bundesland)
plot(n4b.pred$bundesland)

plot(n3b, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(n4b, pages = 1, scheme = 2, shade = TRUE, scale = 0)

# Finally, try interaction(s) with time and covariates:
tic <- Sys.time()
n5b <- bake(file = 'results/fitted_models/n5b_TEST.rds',
            expr = {
              bam(deaths ~ s(long, lat, bs = 'ds', m = c(1.0, 0.5), k = 401) + s(Week, k = 62) + 
                    ti(long, lat, Week, d = c(2, 1), bs = c('ds', 'tp'), m = list(c(1.0, 0.5), NA), k = c(200, 20)) +
                    s(prop65, k = 100) + s(hosp.beds, k = 100) + #s(prop.aus) +
                    ti(Week, prop65) + ti(Week, hosp.beds) +
                    s(bundesland, bs = 're', k = 16) + offset(log(cases)),
                  data = dat_inc_fromCases, family = 'nb', method = 'fREML', nthreads = 4, discrete = TRUE)
            }
)
toc <- Sys.time()
print(toc - tic)

par(mfrow = c(2, 2))
gam.check(n5b)

n5b.pred <- ggpredict(n5b)

plot(n5b.pred$lat)
plot(n5b.pred$long)
plot(n5b.pred$Week)
plot(n5b.pred$prop65)
plot(n5b.pred$hosp.beds)
# plot(n5b.pred$bundesland)

n5b.pred.int1 <- ggpredict(n5b, terms = c('Week', 'prop65'))
n5b.pred.int2 <- ggpredict(n5b, terms = c('Week', 'hosp.beds'))

plot(n5b.pred.int1)
plot(n5b.pred.int2)

# Plot model fits:
dat_inc_FIT <- dat_inc_fromCases %>%
  mutate(fit1 = predict(n1b, type = 'response'),
         fit2 = predict(n2b, type = 'response'))
dat_inc_FIT2 <- dat_inc_fromCases %>%
  filter(!is.na(prop65)) %>%
  filter(!is.na(hosp.beds)) %>%
  mutate(fit3 = predict(n3b, type = 'response'),
         fit4 = predict(n4b, type = 'response'),
         fit5 = predict(n5b, type = 'response'))

dat_inc_FIT <- dat_inc_FIT %>%
  left_join(dat_inc_FIT2[, c('date', 'Year', 'Week', 'lk', 'fit3', 'fit4', 'fit5')])
rm(dat_inc_FIT2)

map_fit <- map_base %>%
  left_join(dat_inc_FIT, by = c('ARS')) %>%
  # filter(Week %in% seq(min(Week), max(Week), by = 4)) %>%
  filter(Week %in% c(17, 58))

p.dat <- ggplot(map_fit) + geom_sf(aes(fill = deaths / cases)) + facet_wrap(~ Week) +
  theme_void() + scale_fill_viridis(na.value = 'gray80', trans = 'log', limits = c(0.0009, 1.0))
p.fit1 <- ggplot(map_fit) + geom_sf(aes(fill = fit1 / cases)) + facet_wrap(~ Week) +
  theme_void() + scale_fill_viridis(na.value = 'gray80', trans = 'log', limits = c(0.0009, 1.0))
p.fit2 <- ggplot(map_fit) + geom_sf(aes(fill = fit2 / cases)) + facet_wrap(~ Week) +
  theme_void() + scale_fill_viridis(na.value = 'gray80', trans = 'log', limits = c(0.0009, 1.0))
p.fit3 <- ggplot(map_fit) + geom_sf(aes(fill = fit3 / cases)) + facet_wrap(~ Week) +
  theme_void() + scale_fill_viridis(na.value = 'gray80', trans = 'log', limits = c(0.0009, 1.0))
p.fit4 <- ggplot(map_fit) + geom_sf(aes(fill = fit4 / cases)) + facet_wrap(~ Week) +
  theme_void() + scale_fill_viridis(na.value = 'gray80', trans = 'log', limits = c(0.0009, 1.0))
p.fit5 <- ggplot(map_fit) + geom_sf(aes(fill = fit5 / cases)) + facet_wrap(~ Week) +
  theme_void() + scale_fill_viridis(na.value = 'gray80', trans = 'log', limits = c(0.0009, 1.0))

grid.arrange(p.dat, p.fit1, p.fit2, p.fit3, p.fit4, p.fit5, ncol = 2)
grid.arrange(p.dat, p.fit2, p.fit4, p.fit3, ncol = 1)
