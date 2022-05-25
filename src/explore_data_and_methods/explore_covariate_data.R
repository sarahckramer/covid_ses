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

# Open pdf
pdf('results/plots/explore_covariate_data_NEW.pdf', width = 18, height = 12)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data

# Read in data:
ses_dat <- read_csv('data/formatted/independent_vars/ses_independent_variables.csv')
vacc_dat <- read_csv('data/formatted/independent_vars/vacc_dat_REGIONAL.csv')

# Remove Landkreise (LK(s)) that were later merged:
ses_dat <- ses_dat %>%
  filter(!(lk_code %in% c('16056', '16063')))
vacc_dat <- vacc_dat %>%
  filter(!(ID_County %in% c('16056', '16063')))

# Join with vaccination data:
ses_dat <- ses_dat %>%
  left_join(vacc_dat, by = c('lk_code' = 'ID_County'))
rm(vacc_dat)

# Plot associations between variables:
pairs(ses_dat[, 4:ncol(ses_dat)], pch = 20)#, lower.panel = NULL)

# Plot histograms of variables:
par(mfrow = c(4, 4))
for (i in 4:ncol(ses_dat)) {
  hist(unlist(ses_dat[, i]), breaks = 25, main = names(ses_dat)[i])
}
rm(i)

# ---------------------------------------------------------------------------------------------------------------------

# Explore correlations between exposures
cor_mat_spearman = p_mat_spearman = cor_mat_kendall = p_mat_kendall =
  matrix(NA, nrow = ncol(ses_dat) - 3, ncol = ncol(ses_dat) - 3)
for (ix in 1:(ncol(ses_dat) - 4)) {
  for (jx in (ix + 1):(ncol(ses_dat) - 3)) {
    if (ix != jx) {
      cor_mat_spearman[ix, jx] = cor.test(ses_dat[, ix + 3][[1]], ses_dat[, jx + 3][[1]], method = 'spearman')$estimate
      p_mat_spearman[ix, jx] = cor.test(ses_dat[, ix + 3][[1]], ses_dat[, jx + 3][[1]], method = 'spearman')$p.value
      cor_mat_kendall[ix, jx] = cor.test(ses_dat[, ix + 3][[1]], ses_dat[, jx + 3][[1]], method = 'kendall')$estimate
      p_mat_kendall[ix, jx] = cor.test(ses_dat[, ix + 3][[1]], ses_dat[, jx + 3][[1]], method = 'kendall')$p.value
    }
  }
}
rm(ix, jx)

rownames(cor_mat_spearman) = colnames(cor_mat_spearman) =
  rownames(p_mat_spearman) = colnames(p_mat_spearman) =
  rownames(cor_mat_kendall) = colnames(cor_mat_kendall) =
  rownames(p_mat_kendall) = colnames(p_mat_kendall) =
  names(ses_dat)[4:ncol(ses_dat)]

par(mfrow = c(2, 2))
corrplot(cor_mat_spearman, method = 'number', diag = FALSE, type = 'upper', p.mat = p_mat_spearman)
corrplot(cor_mat_kendall, method = 'number', diag = FALSE, type = 'upper', p.mat = p_mat_kendall)

corrplot(cor_mat_spearman, method = 'color', diag = FALSE, type = 'upper', p.mat = p_mat_spearman, sig.level = 0.05 / 100)
corrplot(cor_mat_kendall, method = 'color', diag = FALSE, type = 'upper', p.mat = p_mat_kendall)

rm(cor_mat_spearman, cor_mat_kendall, p_mat_spearman, p_mat_kendall)

# Remove variables that are highly correlated:
ses_dat <- ses_dat %>%
  select(-c(avg_dist_pharm, living_area))

# ---------------------------------------------------------------------------------------------------------------------

# Map data

# Read in and format map data:
map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')
expect_true(all(unique(ses_dat$lk_code) %in% unique(map_base$ARS)))

nb <- spdep::poly2nb(map_base, row.names = map_base$ARS, queen = TRUE)
attr(nb, 'region.id') <- map_base$ARS
names(nb) <- attr(nb, 'region.id')

map_base <- map_base %>%
  left_join(ses_dat, by = c('ARS' = 'lk_code'))

# Plot:
vars_to_plot <- names(ses_dat)[4:ncol(ses_dat)]
plot_list <- vector('list', length(vars_to_plot))
for (i in 1:length(vars_to_plot)) {
  var <- vars_to_plot[i]
  map_plot <- map_base %>%
    rename('plot_var' = var)
  
  p.temp <- ggplot(data = map_plot) + geom_sf(aes(fill = plot_var)) +
    scale_fill_viridis(na.value = 'gray80') + theme_void() +
    labs(fill = var)
  
  plot_list[[i]] <- p.temp
}
do.call('grid.arrange', c(plot_list, ncol = 4))







# # Check whether centroids are within LK:
# map_cent <- st_centroid(map_base)
# centroid_not_contained <- map_cent %>%
#   mutate(check = map_base$ARS[as.integer(st_intersects(geometry, map_base))]) %>%
#   filter(ARS != check) %>%
#   pull(ARS)
# 
# # If so, use centroids; otherwise, use "point_on_surface":
# map_base[, c('long', 'lat')] <- st_centroid(map_base) %>% st_transform(., '+proj=longlat') %>% st_coordinates()
# map_base[, c('long_ALT', 'lat_ALT')] <- st_point_on_surface(map_base) %>% st_transform(., '+proj=longlat') %>% st_coordinates()
# # https://gis.stackexchange.com/questions/76498/how-is-st-pointonsurface-calculated
# 
# map_base_new <- map_base %>%
#   mutate(long = if_else(ARS %in% centroid_not_contained, long_ALT, long),
#          lat = if_else(ARS %in% centroid_not_contained, lat_ALT, lat)) %>%
#   dplyr::select(-c(long_ALT:lat_ALT))
# expect_true(length(which(map_base_new$long != map_base$long)) == length(centroid_not_contained))
# expect_true(length(which(map_base_new$lat != map_base$lat)) == length(centroid_not_contained))
# 
# map_base <- map_base_new
# rm(map_base_new, map_cent, centroid_not_contained)




# ---------------------------------------------------------------------------------------------------------------------

# Calculate Moran's I for covariate data
# https://keen-swartz-3146c4.netlify.app/spatautocorr.html

for (var in vars_to_plot) {
  print(var)
  
  map_base_temp <- map_base
  lw <- nb2listw(nb, style = "W", zero.policy = FALSE)
  
  if (any(is.na(map_base_temp[, var][[1]]))) {
    map_base_temp <- map_base_temp[!is.na(map_base_temp[, var][[1]]), ]
    nb_temp <- spdep::poly2nb(map_base_temp, row.names = map_base_temp$ARS, queen = TRUE)
    lw <- nb2listw(nb_temp, style = "W", zero.policy = FALSE)
  }
  
  if (var == 'hosp_beds') {
    moran.mc(map_base_temp[, var][[1]], lw, nsim = 999, alternative = 'less') %>%
      print()
  } else {
    moran.mc(map_base_temp[, var][[1]], lw, nsim = 999) %>%
      print()
  }
  
  print('')
}

# All significant; strongest positive autocorrelation with: GISD and Income/Work dimensions, perc_65plus;
# vaccinations partway through wave 4 more clustered than partway through wave 3;
# only hospital beds significantly less clustered than expected

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

# Any missing data?
summary(ses_dat) # no missing data

# ---------------------------------------------------------------------------------------------------------------------

rm(list = ls())
dev.off()
