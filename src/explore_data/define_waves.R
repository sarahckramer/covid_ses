# ---------------------------------------------------------------------------------------------------------------------
# Split cumulative data into distinct waves, and explore whether partial waves or full waves should be used
# ---------------------------------------------------------------------------------------------------------------------

# Load libraries:
library(tidyverse)
library(gridExtra)

# Read in data:
dat_inc_wk <- read_csv('data/formatted/STAND_weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')
dat_inc_wk_i <- read_csv('data/formatted/weekly_covid_deaths_by_lk_INCIDENT_CDP.csv')

# Add time column:
dat_inc_wk <- dat_inc_wk %>%
  mutate(Week = as.numeric(Week)) %>%
  mutate(time = if_else(Year == 2020, Week, Week + 53),
         .before = ags2)
dat_inc_wk_i <- dat_inc_wk_i %>%
  mutate(Week = as.numeric(Week)) %>%
  mutate(time = if_else(Year == 2020, Week, Week + 53),
         .before = Year)

# Calculate cases in whole population:
dat_inc_DE <- dat_inc_wk_i %>%
  group_by(time) %>%
  summarise(cases = sum(cases),
            deaths = sum(deaths),
            pop = sum(pop)) %>%
  mutate(case_rate = cases / pop * 100000,
         death_rate = deaths / pop * 100000,
         ifr = deaths / cases)# %>%
# left_join(dat_inc_wk %>% select(date, time), by = 'time')

# Visualize data:
p1 <- ggplot(data = dat_inc_wk_i, aes(x = time, y = case_rate, group = lk)) +
  geom_line(alpha = 0.5) + theme_classic() +
  geom_vline(xintercept = c(9, 14, 20, 40, 51, 61), lty = 2)
p2 <- ggplot(data = dat_inc_wk_i, aes(x = time, y = death_rate, group = lk)) +
  geom_line(alpha = 0.5) + theme_classic() +
  geom_vline(xintercept = c(9, 14, 20, 40, 51, 61), lty = 2)
p3 <- ggplot(data = dat_inc_wk_i, aes(x = time, y = ifr, group = lk)) +
  geom_line(alpha = 0.5) + theme_classic() +
  geom_vline(xintercept = c(9, 14, 20, 40, 51, 61), lty = 2)
grid.arrange(p1, p2, p3, ncol = 1)

# p1 <- ggplot(data = dat_inc_wk, aes(x = time, y = case_rate, group = lk)) +
#   geom_line(alpha = 0.5) + theme_classic()
# p2 <- ggplot(data = dat_inc_wk, aes(x = time, y = death_rate, group = lk)) +
#   geom_line(alpha = 0.5) + theme_classic()
# p3 <- ggplot(data = dat_inc_wk, aes(x = time, y = ifr, group = lk)) +
#   geom_line(alpha = 0.5) + theme_classic()
# grid.arrange(p1, p2, p3, ncol = 1)

# Determine cutoff points for various waves/partial waves:
p4 <- ggplot(data = dat_inc_DE, aes(x = time, y = case_rate)) +
  geom_line() + theme_classic() +
  scale_x_continuous(breaks = seq(9, 91, by = 2)) +
  geom_vline(xintercept = c(9, 14, 20, 40, 51, 61), lty = 2)
grid.arrange(p1, p4, ncol = 1)

# Clean up:
rm(dat_inc_DE, dat_inc_wk_i)

# Compare to vaccination rates in Bundeslaender:
vacc_dat <- read_csv('data/raw/cdp/impfdaten.csv') %>%
  select(ags2:datum, bl_erstimpf_quote, bl_zweitimpf_quote) %>%
  mutate(Year = format(datum, '%Y'),
         Week = format(datum, '%V'),
         .after = datum) %>%
  mutate(Year = as.numeric(Year),
         Week = as.numeric(Week),
         Year = if_else(Week == 53 & Year == 2021, 2020, Year),
         time = if_else(Year == 2020, Week, Week + 53))

p5 <- ggplot(data = vacc_dat, aes(x = time, y = bl_erstimpf_quote, group = bundesland)) +
  geom_line() + theme_classic() +
  geom_vline(xintercept = c(9, 14, 20, 40, 51, 61), lty = 2)
p6 <- ggplot(data = vacc_dat, aes(x = time, y = bl_zweitimpf_quote, group = bundesland)) +
  geom_line() + theme_classic() +
  geom_vline(xintercept = c(9, 14, 20, 40, 51, 61), lty = 2)
grid.arrange(p5, p6, ncol = 1)

vacc_dat %>%
  filter(time %in% c(61, 75)) %>%
  group_by(bundesland, time) %>%
  filter(datum == max(datum)) %>%
  ungroup() %>%
  print(n = 32)
rm(vacc_dat)

# Calculate total cases/deaths for each wave/partial wave:
dat_cases_cumulative <- dat_inc_wk %>%
  filter(time %in% c(14, 20, 39, 51, 61)) %>%
  select(time:lk, pop, cases) %>%
  mutate(time = paste('wk', time, sep = '_')) %>%
  pivot_wider(names_from = time, values_from = cases) %>%
  mutate(cases_wave1 = wk_20,
         cases_wave2 = wk_61 - wk_39,
         cases_wave1_1 = wk_14,
         cases_wave1_2 = wk_20 - wk_14,
         cases_wave2_1 = wk_51 - wk_39,
         cases_wave2_2 = wk_61 - wk_51,
         cases_summer = wk_39 - wk_20) %>%
  select(ags2:pop, cases_wave1:cases_summer)

dat_deaths_cumulative <- dat_inc_wk %>%
  filter(time %in% c(14, 20, 39, 51, 61)) %>%
  select(time:lk, deaths) %>%
  mutate(time = paste('wk', time, sep = '_')) %>%
  pivot_wider(names_from = time, values_from = deaths) %>%
  mutate(deaths_wave1 = wk_20,
         deaths_wave2 = wk_61 - wk_39,
         deaths_wave1_1 = wk_14,
         deaths_wave1_2 = wk_20 - wk_14,
         deaths_wave2_1 = wk_51 - wk_39,
         deaths_wave2_2 = wk_61 - wk_51,
         deaths_summer = wk_39 - wk_20) %>%
  select(lk, deaths_wave1:deaths_summer)

# Combine and calculate rates:
dat_cumulative <- dat_cases_cumulative %>%
  left_join(dat_deaths_cumulative, by = 'lk') %>%
  mutate(cases_wave1_rate = cases_wave1 / pop * 10000,
         cases_wave2_rate = cases_wave2 / pop * 10000,
         cases_wave1_1_rate = cases_wave1_1 / pop * 10000,
         cases_wave1_2_rate = cases_wave1_2 / pop * 10000,
         cases_wave2_1_rate = cases_wave2_1 / pop * 10000,
         cases_wave2_2_rate = cases_wave2_2 / pop * 10000,
         cases_summer_rate = cases_summer / pop * 10000,
         deaths_wave1_rate = deaths_wave1 / pop * 10000,
         deaths_wave2_rate = deaths_wave2 / pop * 10000,
         deaths_wave1_1_rate = deaths_wave1_1 / pop * 10000,
         deaths_wave1_2_rate = deaths_wave1_2 / pop * 10000,
         deaths_wave2_1_rate = deaths_wave2_1 / pop * 10000,
         deaths_wave2_2_rate = deaths_wave2_2 / pop * 10000,
         deaths_summer_rate = deaths_summer / pop * 10000,
         ifr_wave1 = deaths_wave1 / cases_wave1 * 100,
         ifr_wave2 = deaths_wave2 / cases_wave2 * 100,
         ifr_wave1_1 = deaths_wave1_1 / cases_wave1_1 * 100,
         ifr_wave1_2 = deaths_wave1_2 / cases_wave1_2 * 100,
         ifr_wave2_1 = deaths_wave2_1 / cases_wave2_1 * 100,
         ifr_wave2_2 = deaths_wave2_2 / cases_wave2_2 * 100,
         ifr_summer = deaths_summer / cases_summer * 100) %>%
  pivot_longer(-c(ags2:pop), names_to = 'outcome', values_to = 'val')
rm(dat_cases_cumulative, dat_deaths_cumulative)

# Explore noisiness in cumulative values over waves/partial waves:
dat_cumulative %>%
  filter(str_detect(outcome, 'rate')) %>%
  group_by(outcome) %>%
  summarise(mean = mean(val),
            median = median(val),
            sd = sd(val)) %>%
  print(n = 14)

dat_plot <- dat_cumulative %>%
  filter(str_detect(outcome, 'rate') |
           str_detect(outcome, 'ifr'))

p7 <- ggplot(dat_plot, aes(x = bundesland, y = val, group = bundesland)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  facet_wrap(~ outcome, scales = 'free_y')
print(p7)

p8 <- ggplot(dat_plot %>%
               filter(str_detect(outcome, 'cases')),
             aes(x = outcome, y = val, group = outcome)) +
  geom_violin(fill = 'steelblue2') + theme_classic()
p9 <- ggplot(dat_plot %>%
               filter(str_detect(outcome, 'deaths')),
             aes(x = outcome, y = val, group = outcome)) +
  geom_violin(fill = 'steelblue2') + theme_classic()
p10 <- ggplot(dat_plot %>%
                filter(str_detect(outcome, 'ifr')),
              aes(x = outcome, y = val, group = outcome)) +
  geom_violin(fill = 'steelblue2') + theme_classic()
grid.arrange(p8, p9, p10, ncol = 1)

# Write cumulative counts/rates to file:
write_csv(dat_cumulative, file = 'data/formatted/STAND_cumulative_cases_and_deaths.csv')

# Clean up:
rm(list = ls())
