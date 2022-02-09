# ---------------------------------------------------------------------------------------------------------------------
# Code to explore relationships between and spatial patterns in covariate data
# ---------------------------------------------------------------------------------------------------------------------

# Load libraries:
library(tidyverse)
library(corrplot)
library(viridis)
library(mgcv)
library(spdep)
library(testthat)
library(gridExtra)

# ---------------------------------------------------------------------------------------------------------------------

# Open pdf:
pdf('results/plots/explore_covariate_data_NEW.pdf', width = 18, height = 12)

# ---------------------------------------------------------------------------------------------------------------------

# Read in data:
ses_dat <- read_csv('data/formatted/independent_vars/ses_independent_variables.csv')
vacc_dat <- read_csv('data/formatted/independent_vars/vacc_dat.csv')

# Limit to variables of interest:
ses_dat <- ses_dat %>%
  select(lk_code, hosp_beds:perc_65plus, perc_women:perc_imm, pop_dens:TS_Arbeitswelt_adj)

# Join with vaccination data:
ses_dat_plus <- ses_dat %>%
  left_join(vacc_dat, by = c('lk_code' = 'ID_County'))
rm(vacc_dat)

# Plot associations between variables:
pairs(ses_dat_plus[, -1], pch = 20)#, lower.panel = NULL)

# Plot histograms of variables:
par(mfrow = c(4, 5))
for (i in 2:ncol(ses_dat_plus)) {
  hist(unlist(ses_dat_plus[, i]), breaks = 25, main = names(ses_dat_plus)[i])
}

# ---------------------------------------------------------------------------------------------------------------------

# Explore correlations between exposures
cor_mat_spearman = p_mat_spearman = cor_mat_kendall = p_mat_kendall =
  matrix(NA, nrow = ncol(ses_dat_plus) - 1, ncol = ncol(ses_dat_plus) - 1)
for (ix in 1:(ncol(ses_dat_plus) - 2)) {
  for (jx in (ix + 1):(ncol(ses_dat_plus) - 1)) {
    if (ix != jx) {
      cor_mat_spearman[ix, jx] = cor.test(ses_dat_plus[, ix + 1][[1]], ses_dat_plus[, jx + 1][[1]], method = 'spearman')$estimate
      p_mat_spearman[ix, jx] = cor.test(ses_dat_plus[, ix + 1][[1]], ses_dat_plus[, jx + 1][[1]], method = 'spearman')$p.value
      cor_mat_kendall[ix, jx] = cor.test(ses_dat_plus[, ix + 1][[1]], ses_dat_plus[, jx + 1][[1]], method = 'kendall')$estimate
      p_mat_kendall[ix, jx] = cor.test(ses_dat_plus[, ix + 1][[1]], ses_dat_plus[, jx + 1][[1]], method = 'kendall')$p.value
    }
  }
}
rm(ix, jx)

rownames(cor_mat_spearman) = colnames(cor_mat_spearman) =
  rownames(p_mat_spearman) = colnames(p_mat_spearman) =
  rownames(cor_mat_kendall) = colnames(cor_mat_kendall) =
  rownames(p_mat_kendall) = colnames(p_mat_kendall) =
  names(ses_dat_plus)[2:ncol(ses_dat_plus)]

par(mfrow = c(2, 2))
corrplot(cor_mat_spearman, method = 'number', diag = FALSE, type = 'upper', p.mat = p_mat_spearman)
corrplot(cor_mat_kendall, method = 'number', diag = FALSE, type = 'upper', p.mat = p_mat_kendall)

corrplot(cor_mat_spearman, method = 'color', diag = FALSE, type = 'upper', p.mat = p_mat_spearman, sig.level = 0.05 / 100)
corrplot(cor_mat_kendall, method = 'color', diag = FALSE, type = 'upper', p.mat = p_mat_kendall)
# May be a good idea to remove average distance to pharmacies - this seems to be highly related to population density
# Potentially remove percent of apartments in multi-family housing, too - population density and living area should be enough
# Commuters in and out are also highly positively correlated, but there are likely some regions (cities in particular) where
# there are a lot of incoming and very few outgoing commuters, so they could still both be informative

ses_dat_plus <- ses_dat_plus %>%
  select(-c(avg_dist_pharm, perc_apt_multifamily))

# ---------------------------------------------------------------------------------------------------------------------

# Map data:
map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')

expect_true(all(unique(map_base$ARS) %in% unique(ses_dat_plus$lk_code)))
expect_true(all(unique(ses_dat_plus$lk_code) %in% unique(map_base$ARS)))

nb <- spdep::poly2nb(map_base, row.names = map_base$ARS, queen = TRUE)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

map_base[, c('long', 'lat')] <- st_centroid(map_base) %>% st_transform(., '+proj=longlat') %>% st_coordinates()

map_base <- map_base %>%
  inner_join(ses_dat_plus, by = c('ARS' = 'lk_code'))

vars_to_plot <- names(ses_dat_plus)[2:length(names(ses_dat_plus))]
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
  
  map_base_temp <- map_base
  lw <- nb2listw(nb, style = "W", zero.policy = FALSE)
  
  if (any(is.na(map_base[, var][[1]]))) {
    map_base_temp <- map_base[!is.na(map_base[, var][[1]]), ]
    nb_temp <- spdep::poly2nb(map_base_temp, row.names = map_base_temp$ARS, queen = TRUE)
    lw <- nb2listw(nb_temp, style = "W", zero.policy = FALSE)
  }
  
  if (var %in% c('hosp_beds', 'commuters_out')) {
    moran.mc(map_base_temp[, var][[1]], lw, nsim = 999, alternative = 'less') %>%
      print()
  } else {
    moran.mc(map_base_temp[, var][[1]], lw, nsim = 999) %>%
      print()
  }
  
  print('')
}

# All significant; strongest positive autocorrelation with: GISD and Income/Work dimensions, living area, perc_imm, perc_65plus;
# vaccinations partway through wave 4 much more clustered than partway through wave 3;
# only hospital beds and outbound commuters significantly less clustered than expected;
# <0.3: perc_women, perc_service, perc_production, commuters_in, education dimension

# Now do local Moran's I:
for (var in vars_to_plot) {
  print(var)
  
  map_base_temp <- map_base
  lw <- nb2listw(nb, style = "W", zero.policy = FALSE)
  
  if (any(is.na(map_base_temp[, var][[1]]))) {
    map_base_temp <- map_base_temp[!is.na(map_base_temp[, var][[1]]), ]
    nb_temp <- spdep::poly2nb(map_base_temp, row.names = map_base_temp$ARS, queen = TRUE)
    lw <- nb2listw(nb_temp, style = "W", zero.policy = FALSE)
  }
  
  loc_moran_temp <- localmoran_perm(map_base_temp[, var][[1]], lw, alternative = 'two.sided', nsim = 999)
  
  p_vals <- loc_moran_temp %>% as_tibble() %>% select('Pr(z != 0)') %>% unlist() %>% cbind('bonferroni' = p.adjust(., 'bonferroni'))
  rownames(p_vals) <- NULL
  colnames(p_vals)[1] <- 'none'
  
  apply(p_vals, 2, function(ix) {sum(ix < 0.05)})
  
  map_base_temp$loc_z <- loc_moran_temp[, 'Z.Ii']
  
  par(mfrow = c(1, 1))
  loc_moran_plot <- moran.plot(map_base_temp[, var][[1]], listw = lw, labels = map_base_temp$ARS, cex = 1, pch = 20,
                               xlab = var, ylab = paste0('lagged_', var))
  
  quadr <- interaction(
    cut(loc_moran_plot$x, c(-Inf, mean(loc_moran_plot$x), Inf), labels = c('Low X', 'High X')),
    cut(loc_moran_plot$wx, c(-Inf, mean(loc_moran_plot$wx), Inf), labels = c('Low X', 'High X')),
    sep = ' : '
  )
  print(table(quadr))
  
  p_bon <- 0.05 / 401
  brks <- qnorm(c(p_bon / 2, 0.05 / 2, 1 - (0.05 / 2), 1 - (p_bon / 2)))
  
  map_base_temp$local_cluster <- quadr
  is.na(map_base_temp$local_cluster) <- !(map_base_temp$loc_z < brks[2] | map_base_temp$loc_z > brks[3])
  map_base_temp$local_cluster_bon <- quadr
  is.na(map_base_temp$local_cluster_bon) <- !(map_base_temp$loc_z < brks[1] | map_base_temp$loc_z > brks[4])
  
  rbind(table(map_base_temp$local_cluster), table(map_base_temp$local_cluster_bon)) %>% t() %>% print()
  
  p1 <- ggplot(data = map_base_temp) + geom_sf(aes(fill = local_cluster)) + theme_void() +
    scale_fill_brewer(palette = 'Set2', na.val = 'gray90') +
    labs(fill = 'Hotspot Status', title = paste0('Unadjusted (', var, ')'))
  p2 <- ggplot(data = map_base_temp) + geom_sf(aes(fill = local_cluster_bon)) + theme_void() +
    scale_fill_brewer(palette = 'Set2', na.val = 'gray90') +
    labs(fill = 'Hotspot Status', title = 'Bonferroni')
  grid.arrange(p1, p2, nrow = 1)
  
}

# ---------------------------------------------------------------------------------------------------------------------

# Where are data missing?:

summary(ses_dat_plus)
# one LK missing vaccination data; probably related to missing infection/death data from CDP

# ---------------------------------------------------------------------------------------------------------------------

rm(list = ls())
dev.off()
