# ---------------------------------------------------------------------------------------------------------------------
# Assess whether underlying spatial patterns in incidence/CFR are associated with commuting patterns
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(jsonlite)
library(viridis)
library(geosphere)
library(vegan)
library(ncf)
library(testthat)
library(sf)

# ---------------------------------------------------------------------------------------------------------------------

# Get geographic distance matrix

# Get lat/long values for each LK:
source('src/functions/load_data.R')
geo_dat <- dat_cumulative %>%
  select(lk, long, lat) %>%
  arrange(as.numeric(lk))

# Calculate matrix of distance between each pair of LKs, in km:
distGeo_Bessel <- function(p1, p2) {
  return(distGeo(p1, p2, a = 6377397.155, f = 1/299.1528434))
}

mat_dist <- distm(geo_dat[, c('long', 'lat')], fun = distGeo_Bessel)
mat_dist <- mat_dist / 1000
rownames(mat_dist) = colnames(mat_dist) = geo_dat$lk
expect_true(isSymmetric(mat_dist))
expect_true(all(diag(mat_dist) == 0))

# ---------------------------------------------------------------------------------------------------------------------

# Get commuting matrix

# Read in and format commuting data (from Koslow et al.):
comm_dat <- fromJSON('data/raw/commuting/migration_bfa_2020_dim400.json')
comm_dat$lk_out <- colnames(comm_dat)
comm_dat <- comm_dat %>%
  select(-'16063') %>%
  filter(lk_out != '16063')
comm_dat <- comm_dat %>%
  pivot_longer(-lk_out, names_to = 'lk_in', values_to = 'comm')
expect_true(nrow(comm_dat) == 399 ** 2)

# Order LKs from Bundesland 1 to Bundesland 16:
comm_dat <- comm_dat %>%
  arrange(desc(as.numeric(lk_out)), as.numeric(lk_in)) %>%
  mutate(lk_out = factor(lk_out, levels = unique(lk_out)),
         lk_in = factor(lk_in, levels = unique(lk_in)))

# Plot commuting data:
p_comm <- ggplot(data = comm_dat, aes(x = lk_in, y = lk_out, fill = comm)) +
  geom_tile() + scale_fill_viridis(trans = 'log', na.value = 'gray80',
                                   breaks = c(1, 25, 1000, 50000)) +
  theme_classic() + theme(axis.ticks = element_blank(), axis.text = element_blank()) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(x = 'Landkreis (Work)', y = 'Landkreis (Home)', fill = '# Commuters')
print(p_comm)

# Get matrix of commuters between each pair of LKs:
mat_comm <- comm_dat %>%
  mutate(lk_out = as.character(lk_out),
         lk_in = as.character(lk_in)) %>%
  arrange(as.numeric(lk_out), as.numeric(lk_in)) %>%
  pivot_wider(names_from = lk_in, values_from = comm) %>%
  select(-lk_out) %>%
  as.matrix()
rownames(mat_comm) = colnames(mat_comm)

expect_true(all.equal(as.numeric(colnames(mat_comm)), as.numeric(geo_dat$lk)))
expect_true(all(diag(mat_comm) == 0))
expect_false(any(is.na(mat_comm)))
expect_false(isSymmetric(mat_comm))

# ---------------------------------------------------------------------------------------------------------------------

# Get spatial patterns fitted by models

# Load fitted models:
n1_1a_full <- read_rds('results/fitted_models/FULL_n1_1a_ml.rds')
n1_2a_full <- read_rds('results/fitted_models/FULL_n1_2a_ml.rds')
n1b_full <- read_rds('results/fitted_models/FULL_n1b_ml.rds')
n2a_full <- read_rds('results/fitted_models/FULL_n2a_ml.rds')
n2b_full <- read_rds('results/fitted_models/FULL_n2b_ml.rds')
n3a_full <- read_rds('results/fitted_models/FULL_n3a_ml.rds')
n3b_full <- read_rds('results/fitted_models/FULL_n3b_ml.rds')
n4a_full <- read_rds('results/fitted_models/FULL_n4a_ml.rds')
n4b_full <- read_rds('results/fitted_models/FULL_n4b_ml.rds')

# Calculate underlying spatial pattern:
spatial_trend_FULL <- geo_dat %>%
  unique() %>%
  mutate(pop = 10000,
         cases_wave1 = 100,
         cases_wave2 = 100,
         cases_wave3 = 100,
         cases_wave4 = 100,
         cases_pre2_rate = mean(dat_cumulative$cases_pre2_rate),
         cases_pre3_rate = mean(dat_cumulative$cases_pre3_rate),
         cases_pre4_rate = mean(dat_cumulative$cases_pre4_rate),
         cases_wave1_rate = mean(dat_cumulative$cases_wave1_rate),
         cases_wave1_1_rate = mean(dat_cumulative$cases_wave1_1_rate),
         cases_wave2_rate = mean(dat_cumulative$cases_wave2_rate),
         cases_wave3_rate = mean(dat_cumulative$cases_wave3_rate),
         cases_wave4_rate = mean(dat_cumulative$cases_wave4_rate),
         ags2 = '01',
         cases_pre2_rate = mean(dat_cumulative$cases_pre2_rate),
         cases_pre3_rate = mean(dat_cumulative$cases_pre3_rate),
         cases_pre4_rate = mean(dat_cumulative$cases_pre4_rate),
         perc_18to64 = mean(dat_cumulative$perc_18to64),
         perc_lessthan18 = mean(dat_cumulative$perc_lessthan18),
         hosp_beds = mean(dat_cumulative$hosp_beds),
         care_home_beds = mean(dat_cumulative$care_home_beds),
         GISD_Score = mean(dat_cumulative$GISD_Score),
         pop_dens = mean(dat_cumulative$pop_dens),
         living_area = mean(dat_cumulative$living_area),
         perc_service = mean(dat_cumulative$perc_service),
         perc_production = mean(dat_cumulative$perc_production),
         vacc_w3_reg = mean(dat_cumulative$vacc_w3_reg),
         vacc_w4_reg = mean(dat_cumulative$vacc_w4_reg))

spatial_trend_FULL <- spatial_trend_FULL %>%
  mutate(fitted_n1_1a = predict(n1_1a_full, spatial_trend_FULL, type = 'response', exclude = 's(ags2)'),
         fitted_n1_2a = predict(n1_2a_full, spatial_trend_FULL, type = 'response', exclude = 's(ags2)'),
         fitted_n2a = predict(n2a_full, spatial_trend_FULL, type = 'response', exclude = 's(ags2)'),
         fitted_n3a = predict(n3a_full, spatial_trend_FULL, type = 'response', exclude = 's(ags2)'),
         fitted_n4a = predict(n4a_full, spatial_trend_FULL, type = 'response', exclude = 's(ags2)'),
         fitted_n1b = predict(n1b_full, spatial_trend_FULL, type = 'response', exclude = 's(ags2)'),
         fitted_n2b = predict(n2b_full, spatial_trend_FULL, type = 'response', exclude = 's(ags2)'),
         fitted_n3b = predict(n3b_full, spatial_trend_FULL, type = 'response', exclude = 's(ags2)'),
         fitted_n4b = predict(n4b_full, spatial_trend_FULL, type = 'response', exclude = 's(ags2)')) %>%
  select(lk:lat, fitted_n1_1a:fitted_n4b)
expect_true(all.equal(geo_dat$lk, spatial_trend_FULL$lk))

# Calculate "distance" between predicted incidence/CFR of each region:
mats_a <- vector('list', length = 5)
mats_b <- vector('list', length = 4)

mats_a[[1]] <- as.matrix(dist(spatial_trend_FULL$fitted_n1_1a))
mats_a[[2]] <- as.matrix(dist(spatial_trend_FULL$fitted_n1_2a))
mats_a[[3]] <- as.matrix(dist(spatial_trend_FULL$fitted_n2a))
mats_a[[4]] <- as.matrix(dist(spatial_trend_FULL$fitted_n3a))
mats_a[[5]] <- as.matrix(dist(spatial_trend_FULL$fitted_n4a))

mats_b[[1]] <- as.matrix(dist(spatial_trend_FULL$fitted_n1b))
mats_b[[2]] <- as.matrix(dist(spatial_trend_FULL$fitted_n2b))
mats_b[[3]] <- as.matrix(dist(spatial_trend_FULL$fitted_n3b))
mats_b[[4]] <- as.matrix(dist(spatial_trend_FULL$fitted_n4b))

mats_a <- lapply(mats_a, function(ix) {
  rownames(ix) = colnames(ix) = spatial_trend_FULL$lk
  ix
})
mats_b <- lapply(mats_b, function(ix) {
  rownames(ix) = colnames(ix) = spatial_trend_FULL$lk
  ix
})

lapply(mats_a, function(ix) {
  isSymmetric(ix)
}) %>%
  unlist() %>%
  all() %>%
  expect_true()
lapply(mats_b, function(ix) {
  isSymmetric(ix)
}) %>%
  unlist() %>%
  all() %>%
  expect_true()

# ---------------------------------------------------------------------------------------------------------------------

# Run Mantel tests

# Mantel tests:
mantel(mats_a[[1]], -mat_comm, method = 'spearman')
mantel(mats_a[[1]], mat_dist, method = 'spearman')

mantel(mats_a[[2]], -mat_comm, method = 'spearman')
mantel(mats_a[[2]], mat_dist, method = 'spearman')

mantel(mats_a[[3]], -mat_comm, method = 'spearman')
mantel(mats_a[[3]], mat_dist, method = 'spearman')

mantel(mats_a[[4]], -mat_comm, method = 'spearman')
mantel(mats_a[[4]], mat_dist, method = 'spearman')

mantel(mats_a[[5]], -mat_comm, method = 'spearman')
mantel(mats_a[[5]], mat_dist, method = 'spearman')

mantel(mats_b[[1]], -mat_comm, method = 'spearman')
mantel(mats_b[[1]], mat_dist, method = 'spearman')

mantel(mats_b[[2]], -mat_comm, method = 'spearman')
mantel(mats_b[[2]], mat_dist, method = 'spearman')

mantel(mats_b[[3]], -mat_comm, method = 'spearman')
mantel(mats_b[[3]], mat_dist, method = 'spearman')

mantel(mats_b[[4]], -mat_comm, method = 'spearman')
mantel(mats_b[[4]], mat_dist, method = 'spearman')

# Partial Mantel tests (control for geographic distance):
partial.mantel.test(mats_a[[1]], mat_comm, mat_dist, method = 'spearman')
partial.mantel.test(mats_a[[2]], mat_comm, mat_dist, method = 'spearman')
partial.mantel.test(mats_a[[3]], mat_comm, mat_dist, method = 'spearman')
partial.mantel.test(mats_a[[4]], mat_comm, mat_dist, method = 'spearman')
partial.mantel.test(mats_a[[5]], mat_comm, mat_dist, method = 'spearman')

partial.mantel.test(mats_b[[1]], mat_comm, mat_dist, method = 'spearman')
partial.mantel.test(mats_b[[2]], mat_comm, mat_dist, method = 'spearman')
partial.mantel.test(mats_b[[3]], mat_comm, mat_dist, method = 'spearman')
partial.mantel.test(mats_b[[4]], mat_comm, mat_dist, method = 'spearman')
