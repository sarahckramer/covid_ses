# ---------------------------------------------------------------------------------------------------------------------
#
# ---------------------------------------------------------------------------------------------------------------------

# Load libraries:
library(tidyverse)
library(viridis)
library(sf)
library(testthat)
library(gridExtra)

# Read in data:
mortality_dat <- read_csv('data/raw/deaths_rl_crowdsource_by_ags.csv')

# Check that summed data are correct:
print(summary(rowSums(mortality_dat[, 2:(dim(mortality_dat)[2] - 1)]) == mortality_dat[, dim(mortality_dat)[2]]))

# Remove country total:
mortality_dat <- mortality_dat %>%
  select(1:(dim(mortality_dat)[2] - 1))

# Check for obvious weekend effect:
weekends <- unique(mortality_dat$time_iso8601)[format(unique(mortality_dat$time_iso8601), '%w') == '0']
dat_plot <- mortality_dat %>%
  pivot_longer(2:dim(mortality_dat)[2], names_to = 'lk', values_to = 'deaths')
p1 <- ggplot(data = dat_plot, aes(x = time_iso8601, y = deaths, group = lk)) + geom_line() +
  geom_vline(xintercept = weekends) +
  theme_classic() + labs(x = 'Date', y = 'Cumulative Deaths by Landkreis')
print(p1)
rm(weekends, dat_plot, p1)
# There seems to be evidence of fewer deaths being reported over the weekend
# For cumulative data, this is hopefully less of an issue, but it's something to be aware of

# Are any dates missing?:
print(length(seq(as.Date(format(min(mortality_dat$time_iso8601), '%Y-%m-%d')),
                 as.Date(format(max(mortality_dat$time_iso8601), '%Y-%m-%d')),
                 by = 1)) ==
        dim(mortality_dat)[1])
print(length(seq(as.Date(format(min(mortality_dat$time_iso8601), '%Y-%m-%d')),
                 as.Date(format(max(mortality_dat$time_iso8601), '%Y-%m-%d')),
                 by = 1)) -
        dim(mortality_dat)[1])
# One date is missing (2020-03-28)

# Format data:
mortality_dat <- mortality_dat %>%
  rename(date = time_iso8601) %>%
  mutate(date = as.Date(format(date, '%Y-%m-%d'))) %>%
  filter(date != '2021-03-01') %>%
  pivot_longer(!date, names_to = 'lk', values_to = 'deaths') %>%
  mutate(lk = str_pad(lk, width = 5, side = 'left', pad = '0'))

# # Write data to file:
# write_csv(mortality_dat, file = 'data/formatted/covid_deaths_by_lk_cumulative.csv')

# ---------------------------------------------------------------------------------------------------------------------

# # Calculate weekly mortality, rather than cumulative counts?:
# mortality_dat_inc <- mortality_dat %>%
#   pivot_wider(names_from = lk, values_from = deaths)
# 
# for (i in nrow(mortality_dat_inc):2) {
#   mortality_dat_inc[i, 2:ncol(mortality_dat_inc)] <- mortality_dat_inc[i, 2:ncol(mortality_dat_inc)] -
#     mortality_dat_inc[i - 1, 2:ncol(mortality_dat_inc)]
# }
# rm(i)
# 
# mortality_dat_inc <- mortality_dat_inc %>%
#   pivot_longer(!date, names_to = 'lk', values_to = 'deaths') %>%
#   mutate(deaths = ifelse(deaths < 0, NA, deaths))
# 
# p2 <- ggplot(data = mortality_dat_inc, aes(x = date, y = deaths, group = lk)) + geom_line() + theme_classic()
# print(p2)

# Do data in a region always increase, or are corrections made?:
not_strictly_inc <- mortality_dat %>%
  group_by(lk) %>%
  mutate(check = cummax(deaths)) %>%
  filter(deaths != check) %>%
  pull(lk) %>%
  unique()
ggplot(mortality_dat[mortality_dat$lk %in% not_strictly_inc, ], aes(x = date, y = deaths)) + geom_line() +
  facet_wrap(~ lk, scales = 'free_y') + theme_classic()
# In 106 LK, cumulative reported deaths do not always increase; see if these persist in data by week

# Get population data by Landkreis:
pop_dat <- read_csv2('data/raw/pop_counts_12411-0015.csv', col_names = FALSE, skip = 6, n_max = 476)
# Source: https://www-genesis.destatis.de/genesis/online

pop_dat <- pop_dat %>%
  select(-X2) %>%
  rename(lk = X1, pop = X3) %>%
  mutate(pop = as.numeric(pop)) %>%
  filter(!is.na(pop))

# Join with mortality data:
mortality_dat_new <- mortality_dat %>%
  left_join(pop_dat, by = 'lk')
expect_identical(dim(mortality_dat)[1], dim(mortality_dat_new)[1])

mortality_dat <- mortality_dat_new
rm(mortality_dat_new, pop_dat)

# Convert data to rates per 100,000 population:
mortality_dat <- mortality_dat %>%
  mutate(death_rate = deaths / pop * 100000)

# ---------------------------------------------------------------------------------------------------------------------

# Read in shapefile:
map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')
# source: http://gdz.bkg.bund.de/index.php/default/open-data/verwaltungsgebiete-1-2-500-000-stand-01-01-vg2500.html
p3 <- ggplot(map_base) + geom_sf() + theme_void()
print(p3); rm(p3)

# Also want shapefile of Bundeslaender:
map_bl <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_lan.shp')

# Separate into first and second waves:
wave1_dat <- mortality_dat %>%
  filter(date < '2020-06-01')
wave2_dat <- mortality_dat %>%
  filter(date >= '2020-10-01') %>%
  left_join(mortality_dat[mortality_dat$date == '2020-09-30', c('lk', 'deaths')], by = 'lk') %>%
  mutate(deaths = deaths.x - deaths.y) %>%
  select(1:2, 7, 4) %>%
  mutate(death_rate = deaths / pop * 100000)

# Get data weekly:
wave1_sun <- wave1_dat %>%
  mutate(day = format(date, '%w')) %>%
  filter(day == '0') %>%
  select(-day) %>%
  mutate(week_no = as.numeric(format(date, '%U'))) %>%
  relocate(week_no, .after = date)
wave2_sun <- wave2_dat %>%
  mutate(day = format(date, '%w')) %>%
  filter(day == 0) %>%
  select(-day) %>%
  mutate(week_no = as.numeric(format(date, '%U'))) %>%
  relocate(week_no, .after = date)
expect_true(all(wave2_sun$deaths >= 0))
rm(wave1_dat, wave2_dat)

# Check again to see if always increasing:
not_strictly_inc1 <- wave1_sun %>%
  group_by(lk) %>%
  mutate(check = cummax(deaths)) %>%
  filter(deaths != check) %>%
  pull(lk) %>%
  unique()
not_strictly_inc2 <- wave2_sun %>%
  group_by(lk) %>%
  mutate(check = cummax(deaths)) %>%
  filter(deaths != check) %>%
  pull(lk) %>%
  unique()
ggplot(wave2_sun[wave2_sun$lk %in% not_strictly_inc2, ], aes(x = date, y = deaths)) + geom_line() +
  facet_wrap(~ lk, scales = 'free_y') + theme_classic()
# Now only occurs in 16 LK, and only in second wave - perhaps later data will include corrections
rm(not_strictly_inc, not_strictly_inc1, not_strictly_inc2)

# Merge data to shapefile:
map_df1 <- map_base %>%
  left_join(wave1_sun, by = c('ARS' = 'lk'))
map_df2 <- map_base %>%
  left_join(wave2_sun, by = c('ARS' = 'lk'))

expect_equal(nrow(wave1_sun) / length(unique(wave1_sun$lk)) * nrow(map_base), nrow(map_df1))
expect_equal(nrow(wave2_sun) / length(unique(wave2_sun$lk)) * nrow(map_base), nrow(map_df2))

# Plot data at key timepoints - maybe weeks 14-18-22 for wave 1, weeks 43-49-3-9 (44-49-2-7-12) for wave 2
plots_wave1 = plots_wave2 = list()
wks1 <- c(14, 18, 22)
# wks2 <- c(43, 49, 3, 9)
wks2 <- c(40, 43, 46, 49, 52, 3, 6, 9)

for (i in 1:length(wks1)) {
  wk <- wks1[i]
  dat_temp <- map_df1 %>% filter(week_no == wk)
  p_temp <- ggplot(dat_temp) + geom_sf(aes(fill = death_rate)) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
    theme_void() +
    labs(title = paste0('Week ', wk), fill = 'Mortality Rate') +
    scale_fill_viridis(limits = c(0, max(map_df1$death_rate)))
  plots_wave1[[i]] <- p_temp
}
for (i in 1:length(wks2)) {
  wk <- wks2[i]
  dat_temp <- map_df2 %>% filter(week_no == wk)
  p_temp <- ggplot(dat_temp) + geom_sf(aes(fill = death_rate)) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
    theme_void() +
    labs(title = paste0('Week ', wk), fill = 'Mortality Rate') +
    scale_fill_viridis(limits = c(0, max(map_df2$death_rate)))
  plots_wave2[[i]] <- p_temp
}

do.call('grid.arrange', c(plots_wave1, ncol = length(wks1)))
do.call('grid.arrange', c(plots_wave2, ncol = length(wks2) / 2))
grid.arrange(plots_wave1[[length(plots_wave1)]], plots_wave2[[length(plots_wave2)]], ncol = 2)

# ---------------------------------------------------------------------------------------------------------------------

# Calculate Moran's I:
# ___global and local____




# Fit BYM2 model with no predictors?:
library(INLA)
library(spdep)

wave1_sun <- wave1_sun %>% filter(week_no == max(week_no))
wave2_sun <- wave2_sun %>% filter(week_no == max(week_no))

rate1 <- sum(wave1_sun$deaths) / sum(wave1_sun$pop)
rate2 <- sum(wave2_sun$deaths) / sum(wave2_sun$pop)

wave1_sun <- wave1_sun %>% mutate(expected = pop * rate1,
                                  SMR = deaths / expected)
wave2_sun <- wave2_sun %>% mutate(expected = pop * rate2,
                                  SMR = deaths / expected)

map_df1 <- map_base %>% left_join(wave1_sun, by = c('ARS' = 'lk'))
map_df2 <- map_base %>% left_join(wave2_sun, by = c('ARS' = 'lk'))
lim1 <- max(abs(log(map_df1$SMR))[abs(log(map_df1$SMR)) != Inf])
lim2 <- max(abs(log(map_df2$SMR))[abs(log(map_df2$SMR)) != Inf])
lim <- max(lim1, lim2)

log_labs <- c(c(0.03, 0.1, 0.3, 1.0, 3.0, 10.0, 33.0))

p1 <- ggplot(map_df1) + geom_sf(aes(fill = log(SMR))) +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  theme_void() + labs(fill = 'SMR') +
  scale_fill_distiller(palette = 'RdBu', na.value = 'gray75',
                       limits = c(-1 * lim, lim),
                       breaks = log(log_labs),
                       labels = log_labs)
p2 <- ggplot(map_df2) + geom_sf(aes(fill = log(SMR))) +
  geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
  theme_void() + labs(fill = 'SMR') +
  scale_fill_distiller(palette = 'RdBu', na.value = 'gray75',
                       limits = c(-1 * lim, lim),
                       breaks = log(log_labs),
                       labels = log_labs)
grid.arrange(p1, p2, ncol = 2)

# https://www.paulamoraga.com/book-geospatial/sec-arealdatatheory.html
# https://www.r-bloggers.com/2019/11/spatial-data-analysis-with-inla/
# https://stackoverflow.com/questions/63778872/error-message-when-running-mixed-effect-models-using-r-inla

# ---------------------------------------------------------------------------------------------------------------------

# incorporate a couple of predictors? (or at least plot a couple?)
# animate (first and second waves separately)
# download more recent data...
