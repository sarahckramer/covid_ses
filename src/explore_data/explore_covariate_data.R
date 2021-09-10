# ---------------------------------------------------------------------------------------------------------------------
# Code to explore relationships between and spatial patterns in covariate data
# ---------------------------------------------------------------------------------------------------------------------

# Load libraries:
library(tidyverse)
library(corrplot)
library(viridis)
# library(RColorBrewer)
library(mgcv)
library(spdep)
library(testthat)
library(gridExtra)

# ---------------------------------------------------------------------------------------------------------------------

# Open pdf:
pdf('results/plots/expore_covariate_data.pdf', width = 18, height = 12)

# ---------------------------------------------------------------------------------------------------------------------

# Read in data:
ses_dat <- read_csv('data/formatted/independent_vars/ses_independent_variables.csv')
mobility_dat <- read_csv('data/formatted/independent_vars/mobility_dat_WEEKLY.csv')

# Limit to variables of interest:
ses_dat <- ses_dat %>%
  select(lk_code, hosp_beds:perc_65plus, perc_women:perc_imm, pop_dens:TS_Arbeitswelt_adj)

# Plot associations between variables:
pairs(ses_dat[, -1], pch = 20)#, lower.panel = NULL)

# Plot histograms of variables:
par(mfrow = c(4, 5))
for (i in 2:ncol(ses_dat)) {
  hist(unlist(ses_dat[, i]), breaks = 25, main = names(ses_dat)[i])
}

# ---------------------------------------------------------------------------------------------------------------------

# Explore correlations between exposures
cor_mat_spearman = p_mat_spearman = cor_mat_kendall = p_mat_kendall =
  matrix(NA, nrow = ncol(ses_dat) - 1, ncol = ncol(ses_dat) - 1)
for (ix in 1:(ncol(ses_dat) - 2)) {
  for (jx in (ix + 1):(ncol(ses_dat) - 1)) {
    if (ix != jx) {
      cor_mat_spearman[ix, jx] = cor.test(ses_dat[, ix + 1][[1]], ses_dat[, jx + 1][[1]], method = 'spearman')$estimate
      p_mat_spearman[ix, jx] = cor.test(ses_dat[, ix + 1][[1]], ses_dat[, jx + 1][[1]], method = 'spearman')$p.value
      cor_mat_kendall[ix, jx] = cor.test(ses_dat[, ix + 1][[1]], ses_dat[, jx + 1][[1]], method = 'kendall')$estimate
      p_mat_kendall[ix, jx] = cor.test(ses_dat[, ix + 1][[1]], ses_dat[, jx + 1][[1]], method = 'kendall')$p.value
    }
  }
}
rm(ix, jx)

rownames(cor_mat_spearman) = colnames(cor_mat_spearman) =
  rownames(p_mat_spearman) = colnames(p_mat_spearman) =
  rownames(cor_mat_kendall) = colnames(cor_mat_kendall) =
  rownames(p_mat_kendall) = colnames(p_mat_kendall) =
  names(ses_dat)[2:ncol(ses_dat)]

par(mfrow = c(2, 2))
corrplot(cor_mat_spearman, method = 'number', diag = FALSE, type = 'upper', p.mat = p_mat_spearman)
corrplot(cor_mat_kendall, method = 'number', diag = FALSE, type = 'upper', p.mat = p_mat_kendall)

corrplot(cor_mat_spearman, method = 'color', diag = FALSE, type = 'upper', p.mat = p_mat_spearman, sig.level = 0.05 / 100)
corrplot(cor_mat_kendall, method = 'color', diag = FALSE, type = 'upper', p.mat = p_mat_kendall)
# May be a good idea to remove average distance to pharmacies - this seems to be highly related to population density
# Potentially remove percent of apartments in multi-family housing, too - population density and living area should be enough
# Commuters in and out are also highly positively correlated, but there are likely some regions (cities in particular) where
# there are a lot of incoming and very few outgoing commuters, so they could still both be informative

ses_dat <- ses_dat %>%
  select(-c(avg_dist_pharm, perc_apt_multifamily))

# ---------------------------------------------------------------------------------------------------------------------

# Is travel higher in urban areas?
mobility_test <- mobility_dat %>%
  inner_join(ses_dat %>% select(lk_code, pop_dens)) %>%
  mutate(time = if_else(year == 2021, week + 53, week),
         time = time - min(time) + 1,
         lk_code = factor(lk_code))

# library(mgcv)
# m1 <- bam(mobility_change_weekly ~ s(time, k = 80) + s(lk_code, k = 401, bs = 're') + s(pop_dens), data = mobility_test, nthreads = 4)
# # m2 <- bam(mobility_change_weekly ~ s(time, k = 80) + s(lk_code, k = 401, bs = 're') + s(pop_dens), data = mobility_test, family = 'scat', nthreads = 4)
# 
# par(mfrow = c(2, 2))
# gam.check(m1, rep = 50)
# # gam.check(m2, rep = 50)
# 
# plot(m1, pages = 1, scheme = 2, shade = TRUE, scale = 0)
# # plot(m2, pages = 1, scheme = 2, shade = TRUE, scale = 0)
# 
# acf(m1$residuals) # looks like still substantial temporal autocorrelation

# Note that these data are not mobility, but CHANGE in mobility from the previous year; seems to actually be higher for lower population density

ggplot(data = mobility_test, aes(x = time, y = mobility_change_weekly, group = lk_code, col = pop_dens)) +
  geom_line(alpha = 0.5) + theme_classic() + scale_color_viridis()
# Values do seem to be higher for lower-density areas; but is this a reflection of reality, or b/C lower density areas have more noise in their data?

# m2 <- bam(mobility_change_weekly ~ te(time, week, k = c(80, 53)) + s(lk_code, k = 401, bs = 're') + s(pop_dens), data = mobility_test, nthreads = 4)
# m2 <- gamm(mobility_change_weekly ~ s(time, k = 20) + s(lk_code, k = 401, bs = 're') + s(pop_dens), data = mobility_test, correlation = corARMA(form = ~1|time, p = 1))#, family = 'scat')
# saveRDS(m2, file = 'results/fitted_models/mobility_gamm_k20.rds')
# 
# plot(m2$gam, pages = 1, scheme = 2, shade = TRUE, scale = 0)
# 
# par(mfrow = c(2, 2))
# gam.check(m2$gam, rep = 50)
# 
# acf(residuals(m2$gam), main = 'Raw Residuals')
# acf(residuals(m2$lme, type = 'normalized'), main = 'Standardized Residuals')
# # even the standardized residuals still have a lot of autocorrelation
# pacf(residuals(m2$gam), main = 'Raw Residuals')
# pacf(residuals(m2$lme, type = 'normalized'), main = 'Standardized Residuals')
# # same with pacf
# 
# qqnorm(residuals(m2$gam))
# qqnorm(residuals(m2$lme))
# qqnorm(residuals(m2$lme, type = 'normalized'))
# 
# summary(m2$gam)
# summary(m2$lme)
# 
# intervals(m2$lme, which = "var-cov")$corStruct
# # https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/

# Another question: Clearly too much wiggliness (overfitting) in time; but if we go lower, gam.check will
# tell us to increase k. How do we get the overall pattern/trend in mobility w/o also fitting the noise?

tot_mobility_dat <- mobility_dat %>%
  group_by(lk_code) %>%
  summarise(total_change = sum(mobility_change_weekly),
            mean_change = mean(mobility_change_weekly, na.rm = TRUE)) %>%
  inner_join(ses_dat %>% select(lk_code, pop_dens)) %>%
  mutate(lk_code = factor(lk_code))
m3 <- gam(total_change ~ s(pop_dens), data = tot_mobility_dat, family = 'scat')
par(mfrow = c(2, 2))
gam.check(m3, rep = 50)
plot(m3, pages = 1, scheme = 2, shade = TRUE, scale = 0)
# Still having issues with the qqplot of gam.check...
# But overall seems that mobility change is higher (i.e., less negative) in regions with less population density

rm(mobility_test)#, tot_mobility_dat, m3)

# ---------------------------------------------------------------------------------------------------------------------

# Map data:
map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')

expect_true(all(unique(map_base$ARS) %in% unique(ses_dat$lk_code)))
expect_true(all(unique(ses_dat$lk_code) %in% unique(map_base$ARS)))

nb <- spdep::poly2nb(map_base, row.names = map_base$ARS, queen = TRUE)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

map_base[, c('long', 'lat')] <- st_centroid(map_base) %>% st_transform(., '+proj=longlat') %>% st_coordinates()

map_base <- map_base %>%
  inner_join(ses_dat, by = c('ARS' = 'lk_code'))

vars_to_plot <- names(ses_dat)[2:length(names(ses_dat))]
plot_list <- vector('list', length(vars_to_plot))
for (i in 1:length(vars_to_plot)) {
  var <- vars_to_plot[i]
  map_plot <- map_base %>%
    rename('plot_var' = var)
  
  p.temp <- ggplot(data = map_plot) + geom_sf(aes(fill = plot_var)) +
    scale_fill_viridis(na.value = 'transparent') + theme_void() +
    labs(fill = var)
  
  plot_list[[i]] <- p.temp
}
do.call('grid.arrange', c(plot_list, ncol = 4))

# ---------------------------------------------------------------------------------------------------------------------

# Calculate Moran's I for covariate data:
# https://keen-swartz-3146c4.netlify.app/spatautocorr.html
lw <- nb2listw(nb, style = "W", zero.policy = FALSE)
# with other packages, can also use inverse distance matrix rather than just neighbors

for (var in vars_to_plot) {
  print(var)
  
  if (var %in% c('hosp_beds', 'commuters_out')) {
    moran.mc(map_base[, var][[1]], lw, nsim = 999, alternative = 'less') %>%
      print()
  } else {
    moran.mc(map_base[, var][[1]], lw, nsim = 999) %>%
      print()
  }
  
  print('')
}

# All significant; strongest positive autocorrelation with: GISD and Income/Work dimensions, living area, perc_imm, perc_65plus;
# only hospital beds and outbound commuters significantly less clustered than expected;
# <0.3: perc_women, perc_service, perc_production, commuters_in, education dimension

# Now do local Moran's I:
for (var in vars_to_plot) {
  print(var)
  
  # loc_moran_temp <- localmoran(map_base[, var][[1]], lw, alternative = 'two.sided', conditional = TRUE)
  loc_moran_temp <- localmoran_perm(map_base[, var][[1]], lw, alternative = 'two.sided', nsim = 999)
  
  p_vals <- loc_moran_temp %>% as_tibble() %>% select('Pr(z != 0)') %>% unlist() %>% cbind('bonferroni' = p.adjust(., 'bonferroni'))
  rownames(p_vals) <- NULL
  colnames(p_vals)[1] <- 'none'
  
  apply(p_vals, 2, function(ix) {sum(ix < 0.05)})
  
  map_base$loc_z <- loc_moran_temp[, 'Z.Ii']
  
  par(mfrow = c(1, 1))
  loc_moran_plot <- moran.plot(map_base[, var][[1]], listw = lw, labels = map_base$ARS, cex = 1, pch = 20,
                               xlab = var, ylab = paste0('lagged_', var))
  
  quadr <- interaction(
    cut(loc_moran_plot$x, c(-Inf, mean(loc_moran_plot$x), Inf), labels = c('Low X', 'High X')),
    cut(loc_moran_plot$wx, c(-Inf, mean(loc_moran_plot$wx), Inf), labels = c('Low X', 'High X')),
    sep = ' : '
  )
  print(table(quadr))
  
  p_bon <- 0.05 / 401
  brks <- qnorm(c(p_bon / 2, 0.05 / 2, 1 - (0.05 / 2), 1 - (p_bon / 2)))
  
  map_base$local_cluster <- quadr
  is.na(map_base$local_cluster) <- !(map_base$loc_z < brks[2] | map_base$loc_z > brks[3])
  map_base$local_cluster_bon <- quadr
  is.na(map_base$local_cluster_bon) <- !(map_base$loc_z < brks[1] | map_base$loc_z > brks[4])
  
  rbind(table(map_base$local_cluster), table(map_base$local_cluster_bon)) %>% t() %>% print()
  
  p1 <- ggplot(data = map_base) + geom_sf(aes(fill = local_cluster)) + theme_void() +
    scale_fill_brewer(palette = 'Set2', na.val = 'gray90') +
    labs(fill = 'Hotspot Status', title = paste0('Unadjusted (', var, ')'))
  p2 <- ggplot(data = map_base) + geom_sf(aes(fill = local_cluster_bon)) + theme_void() +
    scale_fill_brewer(palette = 'Set2', na.val = 'gray90') +
    labs(fill = 'Hotspot Status', title = 'Bonferroni')
  grid.arrange(p1, p2, nrow = 1)
  
}

# ---------------------------------------------------------------------------------------------------------------------

# Calculate Moran's I for average mobility change:

map_base <- map_base %>%
  inner_join(tot_mobility_dat %>% select(-pop_dens), by = c('ARS' = 'lk_code'))

p_mob <- ggplot(data = map_base) + geom_sf(aes(fill = mean_change)) +
  scale_fill_viridis(na.value = 'transparent') + theme_void() +
  labs(fill = 'Mean Mobility Change')
print(p_mob)

moran.mc(unlist(tot_mobility_dat$mean_change), lw, nsim = 999)
# significant

loc_moran_temp <- localmoran_perm(map_base$mean_change, lw, alternative = 'two.sided', nsim = 999)

p_vals <- loc_moran_temp %>% as_tibble() %>% select('Pr(z != 0)') %>% unlist() %>% cbind('bonferroni' = p.adjust(., 'bonferroni'))
rownames(p_vals) <- NULL
colnames(p_vals)[1] <- 'none'

apply(p_vals, 2, function(ix) {sum(ix < 0.05)})

map_base$loc_z <- loc_moran_temp[, 'Z.Ii']

loc_moran_plot <- moran.plot(map_base$mean_change, listw = lw, labels = map_base$ARS, cex = 1, pch = 20,
                             xlab = 'mean_change', ylab = 'lagged_mean_change')

quadr <- interaction(
  cut(loc_moran_plot$x, c(-Inf, mean(loc_moran_plot$x), Inf), labels = c('Low X', 'High X')),
  cut(loc_moran_plot$wx, c(-Inf, mean(loc_moran_plot$wx), Inf), labels = c('Low X', 'High X')),
  sep = ' : '
)
print(table(quadr))

p_bon <- 0.05 / 401
brks <- qnorm(c(p_bon / 2, 0.05 / 2, 1 - (0.05 / 2), 1 - (p_bon / 2)))

map_base$local_cluster <- quadr
is.na(map_base$local_cluster) <- !(map_base$loc_z < brks[2] | map_base$loc_z > brks[3])
map_base$local_cluster_bon <- quadr
is.na(map_base$local_cluster_bon) <- !(map_base$loc_z < brks[1] | map_base$loc_z > brks[4])

rbind(table(map_base$local_cluster), table(map_base$local_cluster_bon)) %>% t() %>% print()

p1 <- ggplot(data = map_base) + geom_sf(aes(fill = local_cluster)) + theme_void() +
  scale_fill_brewer(palette = 'Set2', na.val = 'gray90') +
  labs(fill = 'Hotspot Status', title = 'Unadjusted')
p2 <- ggplot(data = map_base) + geom_sf(aes(fill = local_cluster_bon)) + theme_void() +
  scale_fill_brewer(palette = 'Set2', na.val = 'gray90') +
  labs(fill = 'Hotspot Status', title = 'Bonferroni')
grid.arrange(p1, p2, nrow = 1)

# ---------------------------------------------------------------------------------------------------------------------

# Calculate Moran's I for outcome data over time:
# Or else at several representative time points?
# A spatiotemporal statistic might be better, or just the results of a null gam
# ---------------------------------------------------------------------------------------------------------------------

# Where are data missing?:

summary(ses_dat)
summary(mobility_dat$mobility_change_weekly)
# no missing data

# ---------------------------------------------------------------------------------------------------------------------

rm(list = ls())
dev.off()
