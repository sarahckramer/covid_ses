# ---------------------------------------------------------------------------------------------------------------------
# Map COVID-19 mortality patterns in Germany
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(testthat)
library(sf)
library(viridis)
library(gridExtra)
library(magick)

# ---------------------------------------------------------------------------------------------------------------------

# Read in data

# Load mortality data:
mortality_c <- read_csv('data/formatted/weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')
mortality_i <- read_csv('data/formatted/weekly_covid_deaths_by_lk_INCIDENT_CDP.csv')

# Convert Week to numeric:
mortality_c <- mortality_c %>%
  mutate(Week = as.numeric(Week))
mortality_i <- mortality_i %>%
  mutate(Week = as.numeric(Week))

# Load shapefiles:
map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')
# source: http://gdz.bkg.bund.de/index.php/default/open-data/verwaltungsgebiete-1-2-500-000-stand-01-01-vg2500.html
map_bl <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_lan.shp')

p1 <- ggplot(map_base) + geom_sf() + geom_sf(data = map_bl, fill = NA, lwd = 1.25) + theme_void()
print(p1)

# ---------------------------------------------------------------------------------------------------------------------

# Map cumulative deaths at key timepoints in first and second waves

# Separate into first and second waves:
wave1_dat <- mortality_c %>%
  filter(Week <= 20 & Year == 2020)

wave2_dat <- mortality_c %>%
  filter((Week >= 40 & Year == 2020) | (Week <= 8 & Year == 2021)) %>%
  left_join(mortality_c[mortality_c$Week == 39 & mortality_c$Year == 2020,
                        c('lk', 'cases', 'deaths')],
            by = 'lk') %>%
  mutate(cases = cases.x - cases.y,
         deaths = deaths.x - deaths.y) %>%
  select(ags2:Week, deaths, cases, pop) %>%
  mutate(death_rate = deaths / pop * 100000,
         .after = deaths) %>%
  mutate(cfr = deaths / cases * 100,
         .after = death_rate) %>%
  mutate(case_rate = cases / pop * 100000,
         .after = cases)
expect_true(all(wave2_dat$deaths >= 0))

wave3_dat <- mortality_c %>%
  filter(Week >= 9 & Week <= 21 & Year == 2021) %>%
  left_join(mortality_c[mortality_c$Week == 8 & mortality_c$Year == 2021,
                        c('lk', 'cases', 'deaths')],
            by = 'lk') %>%
  mutate(cases = cases.x - cases.y,
         deaths = deaths.x - deaths.y) %>%
  select(ags2:Week, deaths, cases, pop) %>%
  mutate(death_rate = deaths / pop * 100000,
         .after = deaths) %>%
  mutate(cfr = deaths / cases * 100,
         .after = death_rate) %>%
  mutate(case_rate = cases / pop * 100000,
         .after = cases)
expect_true(all(wave3_dat$deaths >= 0))

wave4_dat <- mortality_c %>%
  filter(Week >= 32 & Week <= 51 & Year == 2021) %>%
  left_join(mortality_c[mortality_c$Week == 31 & mortality_c$Year == 2021,
                        c('lk', 'cases', 'deaths')],
            by = 'lk') %>%
  mutate(cases = cases.x - cases.y,
         deaths = deaths.x - deaths.y) %>%
  select(ags2:Week, deaths, cases, pop) %>%
  mutate(death_rate = deaths / pop * 100000,
         .after = deaths) %>%
  mutate(cfr = deaths / cases * 100,
         .after = death_rate) %>%
  mutate(case_rate = cases / pop * 100000,
         .after = cases)
expect_true(all(wave4_dat$deaths >= 0))

# Get average mortality rates up to each week and calculated 'expected' cumulative deaths:
avg_rates1 <- wave1_dat %>%
  group_by(Year, Week) %>%
  summarise(rate = sum(deaths) / sum(pop))
avg_rates2 <- wave2_dat %>%
  group_by(Year, Week) %>%
  summarise(rate = sum(deaths) / sum(pop))
avg_rates3 <- wave3_dat %>%
  group_by(Year, Week) %>%
  summarise(rate = sum(deaths) / sum(pop))
avg_rates4 <- wave4_dat %>%
  group_by(Year, Week) %>%
  summarise(rate = sum(deaths) / sum(pop))

wave1_dat <- wave1_dat %>%
  left_join(avg_rates1, by = c('Year', 'Week')) %>%
  mutate(expected = pop * rate,
         SMR = deaths / expected)
wave2_dat <- wave2_dat %>%
  left_join(avg_rates2, by = c('Year', 'Week')) %>%
  mutate(expected = pop * rate,
         SMR = deaths / expected)
wave3_dat <- wave3_dat %>%
  left_join(avg_rates3, by = c('Year', 'Week')) %>%
  mutate(expected = pop * rate,
         SMR = deaths / expected)
wave4_dat <- wave4_dat %>%
  left_join(avg_rates4, by = c('Year', 'Week')) %>%
  mutate(expected = pop * rate,
         SMR = deaths / expected)

# Merge data to shapefile:
map_df1 <- map_base %>%
  left_join(wave1_dat, by = c('ARS' = 'lk'))
map_df2 <- map_base %>%
  left_join(wave2_dat, by = c('ARS' = 'lk'))
map_df3 <- map_base %>%
  left_join(wave3_dat, by = c('ARS' = 'lk'))
map_df4 <- map_base %>%
  left_join(wave4_dat, by = c('ARS' = 'lk'))

expect_equal(nrow(wave1_dat) / length(unique(wave1_dat$lk)) * nrow(map_base), nrow(map_df1))
expect_equal(nrow(wave2_dat) / length(unique(wave2_dat$lk)) * nrow(map_base), nrow(map_df2))

# Plot cumulative deaths at several timepoints:
plots_wave1 = plots_wave2 = plots_wave3 = plots_wave4 =
  plots_smr1 = plots_smr2 = plots_smr3 = plots_smr4 = list()
wks1 <- c(12, 16, 20)
wks2 <- c(40, 43, 46, 49, 52, 2, 5, 8)
wks3 <- c(9, 13, 17, 21)
wks4 <- c(32, 35, 38, 41, 44, 47, 51)

smr_lim1 <- max(max(log(map_df1$SMR)), abs(min(log(map_df1$SMR)[log(map_df1$SMR) != -Inf])))
smr_lim2 <- max(max(log(map_df2$SMR)), abs(min(log(map_df2$SMR)[log(map_df2$SMR) != -Inf])))
smr_lim3 <- max(max(log(map_df3$SMR)), abs(min(log(map_df3$SMR)[log(map_df3$SMR) != -Inf])))
smr_lim4 <- max(max(log(map_df4$SMR)), abs(min(log(map_df4$SMR)[log(map_df4$SMR) != -Inf])))

for (i in 1:length(wks1)) {
  wk <- wks1[i]
  dat_temp <- map_df1 %>% filter(Week == wk)
  p_temp <- ggplot(dat_temp) + geom_sf(aes(fill = death_rate)) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
    theme_void() +
    labs(title = paste0('Week ', wk), fill = 'Mortality Rate') +
    scale_fill_viridis(limits = c(0, max(map_df1$death_rate)))
  p_smr_temp <- ggplot(dat_temp) + geom_sf(aes(fill = log(SMR))) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
    theme_void() +
    labs(title = paste0('Week ', wk), fill = 'SMR') +
    scale_fill_distiller(palette = 'RdBu', na.value = 'gray75',
                         limits = c(-smr_lim1, smr_lim1))
  plots_wave1[[i]] <- p_temp
  plots_smr1[[i]] <- p_smr_temp
}
for (i in 1:length(wks2)) {
  wk <- wks2[i]
  dat_temp <- map_df2 %>% filter(Week == wk)
  p_temp <- ggplot(dat_temp) + geom_sf(aes(fill = death_rate)) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
    theme_void() +
    labs(title = paste0('Week ', wk), fill = 'Mortality Rate') +
    scale_fill_viridis(limits = c(0, max(map_df2$death_rate)))
  p_smr_temp <- ggplot(dat_temp) + geom_sf(aes(fill = log(SMR))) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
    theme_void() +
    labs(title = paste0('Week ', wk), fill = 'SMR') +
    scale_fill_distiller(palette = 'RdBu', na.value = 'gray75',
                         limits = c(-smr_lim2, smr_lim2))
  plots_wave2[[i]] <- p_temp
  plots_smr2[[i]] <- p_smr_temp
}
for (i in 1:length(wks3)) {
  wk <- wks3[i]
  dat_temp <- map_df3 %>% filter(Week == wk)
  p_temp <- ggplot(dat_temp) + geom_sf(aes(fill = death_rate)) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
    theme_void() +
    labs(title = paste0('Week ', wk), fill = 'Mortality Rate') +
    scale_fill_viridis(limits = c(0, max(map_df3$death_rate)))
  p_smr_temp <- ggplot(dat_temp) + geom_sf(aes(fill = log(SMR))) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
    theme_void() +
    labs(title = paste0('Week ', wk), fill = 'SMR') +
    scale_fill_distiller(palette = 'RdBu', na.value = 'gray75',
                         limits = c(-smr_lim3, smr_lim3))
  plots_wave3[[i]] <- p_temp
  plots_smr3[[i]] <- p_smr_temp
}
for (i in 1:length(wks4)) {
  wk <- wks4[i]
  dat_temp <- map_df4 %>% filter(Week == wk)
  p_temp <- ggplot(dat_temp) + geom_sf(aes(fill = death_rate)) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
    theme_void() +
    labs(title = paste0('Week ', wk), fill = 'Mortality Rate') +
    scale_fill_viridis(limits = c(0, max(map_df4$death_rate)))
  p_smr_temp <- ggplot(dat_temp) + geom_sf(aes(fill = log(SMR))) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
    theme_void() +
    labs(title = paste0('Week ', wk), fill = 'SMR') +
    scale_fill_distiller(palette = 'RdBu', na.value = 'gray75',
                         limits = c(-smr_lim4, smr_lim4))
  plots_wave4[[i]] <- p_temp
  plots_smr4[[i]] <- p_smr_temp
}

do.call('grid.arrange', c(plots_wave1, ncol = length(wks1)))
do.call('grid.arrange', c(plots_wave2, ncol = length(wks2) / 2))
do.call('grid.arrange', c(plots_wave3, ncol = length(wks3) / 2))
do.call('grid.arrange', c(plots_wave4, ncol = ceiling(length(wks4) / 2)))
grid.arrange(plots_wave1[[length(plots_wave1)]], plots_wave2[[length(plots_wave2)]],
             plots_wave3[[length(plots_wave3)]], plots_wave4[[length(plots_wave4)]],
             ncol = 2)

do.call('grid.arrange', c(plots_smr1, ncol = length(wks1)))
do.call('grid.arrange', c(plots_smr2, ncol = length(wks2) / 2))
do.call('grid.arrange', c(plots_smr3, ncol = length(wks3) / 2))
do.call('grid.arrange', c(plots_smr4, ncol = ceiling(length(wks4) / 2)))
grid.arrange(plots_smr1[[length(plots_smr1)]], plots_smr2[[length(plots_smr2)]],
             plots_smr3[[length(plots_smr3)]], plots_smr4[[length(plots_smr4)]],
             ncol = 2)

pdf('results/plots/wave1.pdf', width = 12, height = 6.61)
do.call('grid.arrange', c(plots_wave1, ncol = length(wks1)))
do.call('grid.arrange', c(plots_smr1, ncol = length(wks1)))
dev.off()

pdf('results/plots/wave2.pdf', width = 15, height = 8.26)
do.call('grid.arrange', c(plots_wave2, ncol = length(wks2) / 2))
do.call('grid.arrange', c(plots_smr2, ncol = length(wks2) / 2))
dev.off()

pdf('results/plots/wave3.pdf', width = 10, height = 8.26)
do.call('grid.arrange', c(plots_wave3, ncol = length(wks3) / 2))
do.call('grid.arrange', c(plots_smr3, ncol = length(wks3) / 2))
dev.off()

pdf('results/plots/wave4.pdf', width = 15, height = 8.26)
do.call('grid.arrange', c(plots_wave4, ncol = ceiling(length(wks4) / 2)))
do.call('grid.arrange', c(plots_smr4, ncol = ceiling(length(wks4) / 2)))
dev.off()

pdf('results/plots/wave_ends.pdf', width = 10, height = 10)
grid.arrange(plots_wave1[[length(plots_wave1)]], plots_wave2[[length(plots_wave2)]],
             plots_wave3[[length(plots_wave3)]], plots_wave4[[length(plots_wave4)]],
             ncol = 2)
grid.arrange(plots_smr1[[length(plots_smr1)]], plots_smr2[[length(plots_smr2)]],
             plots_smr3[[length(plots_smr3)]], plots_smr4[[length(plots_smr4)]],
             ncol = 2)
dev.off()

# Clean up:
rm(wave1_dat, wave2_dat, wave3_dat, wave4_dat,
   map_df1, map_df2, map_df3, map_df4,
   avg_rates1, avg_rates2, avg_rates3, avg_rates4,
   plots_wave1, plots_wave2, plots_wave3, plots_wave4,
   plots_smr1, plots_smr2, plots_smr3, plots_smr4,
   wks1, wks2, wks3, wks4, wk, dat_temp, p_temp, p_smr_temp,
   smr_lim1, smr_lim2, smr_lim3, smr_lim4, i)

# ---------------------------------------------------------------------------------------------------------------------

# Animate pandemic progression over time (incident and cumulative)

# Merge both datasets to shapefile:
map_c <- map_base %>%
  left_join(mortality_c, by = c('ARS' = 'lk')) %>%
  mutate(Week = if_else(Year == 2021, Week + 53, Week),
         Week = if_else(Year == 2022, Week + 53 + 52, Week)) %>%
  filter(!is.na(Year))
map_i <- map_base %>%
  left_join(mortality_i, by = c('ARS' = 'lk')) %>%
  mutate(Week = if_else(Year == 2021, Week + 53, Week),
         Week = if_else(Year == 2022, Week + 53 + 52, Week)) %>%
  filter(!is.na(Year))

expect_equal(nrow(mortality_c) / length(unique(mortality_c$lk)) * nrow(map_base), nrow(map_c))
expect_equal(nrow(mortality_i) / length(unique(mortality_i$lk)) * nrow(map_base), nrow(map_i))
expect_true(all.equal(unique(map_c$Week), unique(map_i$Week)))

# Create folder for temporary plots:
if (!dir.exists('results/temp/')) {
  dir.create('results/temp/')
}

# Generate and save weekly plots (cumulative, log-scale):
max_c <- max(map_c$death_rate)
for (wk in unique(map_c$Week)) {
  print(wk)
  
  dat_temp <- map_c[map_c$Week == wk, ]
  
  yr <- unique(dat_temp$Year)
  if (length(yr) > 1) {
    stop('Multiple years included in data.')
  }
  
  p_temp <- ggplot() + geom_sf(data = dat_temp, aes(fill = death_rate), lwd = 1.0) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.5, col = 'black') +
    theme_void() + theme(plot.title = element_text(size = 60),
                         legend.title = element_text(size = 42),
                         legend.text = element_text(size = 34),
                         legend.key.width = unit(1.5, 'cm'),
                         legend.key.height = unit(2.75, 'cm')) +
    labs(title = paste0('Week ', wk), fill = 'ln(Mortality Rate)') +
    scale_fill_viridis(limits = c(1, max_c),
                       breaks = c(1, 2, 5, 10, 25, 50, 100, 200, floor(max_c)),
                       trans = 'log',
                       na.value = 'gray65')
  
  png(filename = paste0('results/temp/animate_c_wk', wk, '_log.png'),
      width = 1200, height = 1200)
  print(p_temp)
  dev.off()
}

# Generate and save weekly plots (incident):
max_i <- max(map_i$death_rate, na.rm = TRUE)
for (wk in unique(map_i$Week)) {
  print(wk)
  
  dat_temp <- map_i[map_i$Week == wk, ]
  
  yr <- unique(dat_temp$Year)
  if (length(yr) > 1) {
    stop('Multiple years included in data.')
  }
  
  # Plot on natural scale:
  p_temp <- ggplot() + geom_sf(data = dat_temp, aes(fill = death_rate), lwd = 1.0) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.5, col = 'black') +
    theme_void() + theme(plot.title = element_text(size = 60),
                         legend.title = element_text(size = 42),
                         legend.text = element_text(size = 34),
                         legend.key.width = unit(1.5, 'cm'),
                         legend.key.height = unit(2.75, 'cm')) +
    labs(title = paste0('Week ', wk), fill = 'Mortality Rate') +
    scale_fill_viridis(limits = c(0, max_i),
                       na.value = 'gray65')
  
  png(filename = paste0('results/temp/animate_i_wk', wk, '.png'),
      width = 1200, height = 1200)
  print(p_temp)
  dev.off()
  
  # And where zeros shown as gray:
  p_temp <- ggplot() + geom_sf(data = dat_temp, aes(fill = death_rate), lwd = 1.0) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.5, col = 'black') +
    theme_void() + theme(plot.title = element_text(size = 60),
                         legend.title = element_text(size = 42),
                         legend.text = element_text(size = 34),
                         legend.key.width = unit(1.5, 'cm'),
                         legend.key.height = unit(2.75, 'cm')) +
    labs(title = paste0('Week ', wk), fill = 'Mortality Rate') +
    scale_fill_viridis(limits = c(1, max_i),
                       na.value = 'gray65')
  
  png(filename = paste0('results/temp/animate_i_wk', wk, '_no0.png'),
      width = 1200, height = 1200)
  print(p_temp)
  dev.off()
  
  # Plot on log scale as well:
  p_temp <- ggplot() +
    geom_sf(data = dat_temp, aes(fill = death_rate), lwd = 1.0) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.5, col = 'black') +
    theme_void() + theme(plot.title = element_text(size = 60),
                         legend.title = element_text(size = 42),
                         legend.text = element_text(size = 34),
                         legend.key.width = unit(1.5, 'cm'),
                         legend.key.height = unit(2.75, 'cm')) +
    labs(title = paste0('Week ', wk), fill = 'ln(Mortality Rate)') +
    scale_fill_viridis(limits = c(1, max_i),
                       breaks = c(1, 2, 5, 10, 25, floor(max_i)),
                       trans = 'log',
                       na.value = 'gray65')
  
  png(filename = paste0('results/temp/animate_i_wk', wk, '_log.png'),
      width = 1200, height = 1200)
  print(p_temp)
  dev.off()
}

# Create and save animations:
try(dev.off())

img_c <- image_graph(1200, 1200, res = 72)
for (wk in 13:110) {
  img_c <- c(img_c, image_read(paste0('results/temp/animate_c_wk', wk, '_log.png')))
}
animate_c <- image_animate(img_c, fps = 2, loop = 1)
image_write(animate_c, 'results/plots/animate_cumulative_deaths_LOG.gif')

img_i <- image_graph(1200, 1200, res = 72)
for (wk in 13:110) {
  img_i <- c(img_i, image_read(paste0('results/temp/animate_i_wk', wk, '.png')))
}
animate_i <- image_animate(img_i, fps = 2, loop = 1)
image_write(animate_i, 'results/plots/animate_incident_deaths.gif')

img_i0 <- image_graph(1200, 1200, res = 72)
for (wk in 13:110) {
  img_i0 <- c(img_i0, image_read(paste0('results/temp/animate_i_wk', wk, '_no0.png')))
}
animate_i0 <- image_animate(img_i0, fps = 2, loop = 1)
image_write(animate_i0, 'results/plots/animate_incident_deaths_no0.gif')

img_i_log <- image_graph(1200, 1200, res = 72)
for (wk in 13:110) {
  img_i_log <- c(img_i_log, image_read(paste0('results/temp/animate_i_wk', wk, '_log.png')))
}
animate_i_log <- image_animate(img_i_log, fps = 2, loop = 1)
image_write(animate_i_log, 'results/plots/animate_incident_deaths_LOG.gif')

# Delete pngs:
temp_files_list <- list.files('results/temp/', full.names = TRUE)
for (f in temp_files_list) {
  file.remove(f)
}

# Clean up:
rm(list = ls())
