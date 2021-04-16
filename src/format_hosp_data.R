# ---------------------------------------------------------------------------------------------------------------------
# Format hospitalization data and join into a single data frame
# ---------------------------------------------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(testthat)
library(sf)
library(viridis)
library(gridExtra)

# Get list of files:
file_list <- list.files('data/raw/hospital/', full.names = TRUE)
# RKI seems to have data starting March 7, but csvs only available from April 24

# Read in data as list:
dat_list <- lapply(file_list, function(ix) {
  read_csv(ix, col_types = cols())
})
rm(file_list)

# Check that all were csvs (not pdfs):
expect_false(any(which(lapply(dat_list, names) == '%PDF-1.7')))
# dat_list <- dat_list[-which(lapply(dat_list, names) == '%PDF-1.7')]
# originally had pdf for 2021-01-22; downloaded csv manually

# Check whether column names are consistent...:
column_names <- lapply(dat_list, names)
column_names_unique <- list()
column_names_unique[[1]] <- column_names[[1]]
counter_unique <- 2
for (i in 2:length(column_names)) {
  if (all.equal(column_names[[i]], column_names_unique[[counter_unique - 1]])[1] != TRUE) {
    column_names_unique[[counter_unique]] <- column_names[[i]]
    counter_unique <- counter_unique + 1
  }
}
rm(i, counter_unique)
# mostly vary over the first 7 days, then are consistent until 2021-01-22
# note that number of ICU cases/cases on ventilation not available until 2020-04-26

# ... and correct where they are not:
desired_names <- column_names_unique[[6]]
dat_list[[1]] <- dat_list[[1]] %>%
  rename(gemeindeschluessel = kreis) %>%
  select(-faelle_covid_aktuell_im_bundesland) %>%
  mutate(anzahl_meldebereiche = NA,
         faelle_covid_aktuell = NA,
         faelle_covid_aktuell_beatmet = NA,
         daten_stand = as.Date('2020-04-24'))
dat_list[[2]] <- dat_list[[2]] %>%
  mutate(anzahl_meldebereiche = NA,
         faelle_covid_aktuell = NA,
         faelle_covid_aktuell_beatmet = NA,
         daten_stand = as.Date('2020-04-25'),
         bundesland = str_sub(gemeindeschluessel, 1, 2))
dat_list[[3]] <- dat_list[[3]] %>%
  mutate(daten_stand = as.Date('2020-04-26'))
dat_list[[4]] <- dat_list[[4]] %>%
  mutate(daten_stand = as.Date('2020-04-27'))
dat_list[[6]] <- dat_list[[6]] %>%
  select(-X1)
dat_list[271:length(dat_list)] <- lapply(271:length(dat_list), function(ix) {
  dat_list[[ix]] %>% select(-betten_belegt_nur_erwachsen, -betten_frei_nur_erwachsen)
})
dat_list <- lapply(dat_list, function(ix) {
  ix %>% mutate(daten_stand = as.Date(daten_stand, format = '%Y-%m-%d'))
})

# Now ncol/names should be the same for all entries:
check_ncol <- unlist(lapply(dat_list, ncol))
expect_true(all(check_ncol == length(desired_names)))

check_correct_names <- unlist(lapply(dat_list, function(ix) {
  all(colnames(ix) %in% desired_names)
}))
expect_true(all(check_correct_names))

rm(column_names, column_names_unique, check_ncol, check_correct_names, desired_names)

# Combine all entries:
hosp_dat <- do.call('bind_rows', dat_list)

expect_true(nrow(hosp_dat) == sum(unlist(lapply(dat_list, nrow))))
expect_true(ncol(hosp_dat) == 9)
rm(dat_list)

# Calculate total beds and % of beds used:
hosp_dat <- hosp_dat %>%
  mutate(total_beds = betten_belegt + betten_frei,
         prop_betten_belegt = betten_belegt / (betten_belegt + betten_frei)) %>%
  relocate(total_beds, .after = betten_belegt) %>%
  relocate(prop_betten_belegt, .after = total_beds)

# Check that no other dates have been lost/dropped:
expect_equal(length(min(hosp_dat$daten_stand):max(hosp_dat$daten_stand)),
             length(unique(hosp_dat$daten_stand)) + 3)
# missing: 2020-06-03; 2020-09-27; 2020-10-01

# Ensure that an entry exists for every lk/date:
hosp_dat_full <- hosp_dat %>%
  pivot_wider(id_cols = daten_stand, names_from = gemeindeschluessel, values_from = betten_belegt) %>%
  pivot_longer(cols = !daten_stand, names_to = 'gemeindeschluessel', values_to = 'betten_belegt')
# 298 NAs filled in

hosp_dat_full <- hosp_dat_full %>%
  left_join(hosp_dat, by = c('daten_stand', 'gemeindeschluessel', 'betten_belegt')) %>%
  select(1, 4, 2, 5:6, 3, 7:11)

hosp_dat <- hosp_dat_full
rm(hosp_dat_full)

# Add population size:
pop_dat <- read_csv2('data/raw/pop_counts_12411-0015.csv', col_names = FALSE, skip = 6, n_max = 476)

pop_dat <- pop_dat %>%
  select(-X2) %>%
  rename(lk = X1, pop = X3) %>%
  mutate(pop = as.numeric(pop)) %>%
  filter(!is.na(pop))

hosp_dat_new <- hosp_dat %>%
  left_join(pop_dat, by = c('gemeindeschluessel' = 'lk'))
expect_identical(nrow(hosp_dat_new), nrow(hosp_dat))

hosp_dat <- hosp_dat_new
rm(hosp_dat_new, pop_dat)

# do data vary more by population size, or by number of reporting institutions? (or total number of beds?)
ggplot(data = hosp_dat, aes(x = daten_stand, y = betten_belegt, group = gemeindeschluessel)) + geom_line() + theme_bw()
ggplot(data = hosp_dat, aes(x = daten_stand, y = betten_belegt / anzahl_standorte, group = gemeindeschluessel)) + geom_line() + theme_bw()
ggplot(data = hosp_dat, aes(x = daten_stand, y = betten_belegt / pop, group = gemeindeschluessel)) + geom_line() + theme_bw()
ggplot(data = hosp_dat, aes(x = daten_stand, y = betten_belegt / total_beds, group = gemeindeschluessel)) + geom_line() + theme_bw()

ggplot(data = hosp_dat, aes(x = daten_stand, y = faelle_covid_aktuell, group = gemeindeschluessel)) + geom_line() + theme_bw()
ggplot(data = hosp_dat, aes(x = daten_stand, y = faelle_covid_aktuell / anzahl_standorte, group = gemeindeschluessel)) + geom_line() + theme_bw()
ggplot(data = hosp_dat, aes(x = daten_stand, y = faelle_covid_aktuell / pop, group = gemeindeschluessel)) + geom_line() + theme_bw()
ggplot(data = hosp_dat, aes(x = daten_stand, y = faelle_covid_aktuell / total_beds, group = gemeindeschluessel)) + geom_line() + theme_bw()

ggplot(data = hosp_dat, aes(x = daten_stand, y = faelle_covid_aktuell_beatmet, group = gemeindeschluessel)) + geom_line() + theme_bw()
ggplot(data = hosp_dat, aes(x = daten_stand, y = faelle_covid_aktuell_beatmet / anzahl_standorte, group = gemeindeschluessel)) + geom_line() + theme_bw()
ggplot(data = hosp_dat, aes(x = daten_stand, y = faelle_covid_aktuell_beatmet / pop, group = gemeindeschluessel)) + geom_line() + theme_bw()
ggplot(data = hosp_dat, aes(x = daten_stand, y = faelle_covid_aktuell_beatmet / total_beds, group = gemeindeschluessel)) + geom_line() + theme_bw()

cor.test(hosp_dat$pop, hosp_dat$anzahl_standorte)
cor.test(hosp_dat$pop, hosp_dat$anzahl_meldebereiche)
cor.test(hosp_dat$pop, hosp_dat$total_beds)
# all highly correlated, but total_beds a little less so
# using number of reporting locations actually tends to look better than population
try(dev.off())

# Write to file:
write_csv(hosp_dat, 'data/formatted/covid_hosp_by_lk.csv')

# Merge with map data and plot patterns over time:
map_base <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_krs.shp')
map_bl <- st_read(dsn = 'data/raw/map/vg2500_01-01.gk3.shape/vg2500/vg2500_lan.shp')

map_hosp <- map_base %>%
  left_join(hosp_dat, by = c('ARS' = 'gemeindeschluessel'))
# 4 LK with no data?
# "07338" "09374" "09473" "09573"
# "Rhein-Pfalz-Kreis" "Neustadt a.d. Waldnaab" "Coburg" "Fürth"

plot_list_icu = plot_list_beatmet = plot_list_prop_betten = vector('list', length = 11)
month_ends <- c('2020-04-30', '2020-05-31', '2020-06-30', '2020-07-31', '2020-08-31', '2020-09-30', '2020-10-31', '2020-11-30', '2020-12-31', '2021-01-31', '2021-02-28')

for (i in 1:length(month_ends)) {
  m <- month_ends[i]
  dat_temp <- map_hosp %>% filter(daten_stand == m)
  
  p_temp1 <- ggplot(dat_temp) + geom_sf(aes(fill = faelle_covid_aktuell / pop * 100000)) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
    theme_void() +
    labs(title = paste0('Date: ', m), fill = 'ICU Cases\n(per 100,000)') +
    scale_fill_viridis(limits = c(0, max(hosp_dat$faelle_covid_aktuell / hosp_dat$pop * 100000, na.rm = TRUE)),
                       trans = 'log1p')
  p_temp2 <- ggplot(dat_temp) + geom_sf(aes(fill = faelle_covid_aktuell_beatmet / pop * 100000)) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
    theme_void() +
    labs(title = paste0('Date: ', m), fill = 'ICU Cases\n(Ventilated)\n(per 100,000)') +
    scale_fill_viridis(limits = c(0, max(hosp_dat$faelle_covid_aktuell_beatmet / hosp_dat$pop * 100000, na.rm = TRUE)),
                       trans = 'log1p')
  p_temp3 <- ggplot(dat_temp) + geom_sf(aes(fill = prop_betten_belegt)) +
    geom_sf(data = map_bl, fill = NA, lwd = 1.0, col = 'black') +
    theme_void() +
    labs(title = paste0('Date: ', m), fill = '% Beds in Use') +
    scale_fill_viridis(limits = c(0, 1))
  
  plot_list_icu[[i]] <- p_temp1
  plot_list_beatmet[[i]] <- p_temp2
  plot_list_prop_betten[[i]] <- p_temp3
}

do.call('grid.arrange', c(plot_list_icu, ncol = 4))
do.call('grid.arrange', c(plot_list_beatmet, ncol = 4))
do.call('grid.arrange', c(plot_list_prop_betten, ncol = 4))

ggplot(data = hosp_dat, aes(x = daten_stand, y = prop_betten_belegt, group = gemeindeschluessel)) + geom_line() + theme_classic() + facet_wrap(~ bundesland)
