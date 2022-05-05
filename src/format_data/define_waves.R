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
         time = if_else(Year == 2022, time + 52, time),
         .before = ags2)
dat_inc_wk_i <- dat_inc_wk_i %>%
  mutate(Week = as.numeric(Week)) %>%
  mutate(time = if_else(Year == 2020, Week, Week + 53),
         time = if_else(Year == 2022, time + 52, time),
         .before = Year)

# Calculate cases in whole population:
dat_inc_DE <- dat_inc_wk_i %>%
  group_by(time) %>%
  summarise(cases = sum(cases),
            deaths = sum(deaths),
            pop = sum(pop)) %>%
  mutate(case_rate = cases / pop * 100000,
         death_rate = deaths / pop * 100000,
         cfr = deaths / cases)

# Visualize data:
p1 <- ggplot(data = dat_inc_wk_i, aes(x = time, y = case_rate, group = lk)) +
  geom_line(alpha = 0.5) + theme_classic() +
  geom_vline(xintercept = c(9, 14, 20, 40, 48, 61, 69, 74, 85, 93, 104), lty = 2)
p2 <- ggplot(data = dat_inc_wk_i, aes(x = time, y = death_rate, group = lk)) +
  geom_line(alpha = 0.5) + theme_classic() +
  geom_vline(xintercept = c(9, 14, 20, 40, 48, 61, 69, 74, 85, 93, 104), lty = 2)
p3 <- ggplot(data = dat_inc_wk_i, aes(x = time, y = cfr, group = lk)) +
  geom_line(alpha = 0.5) + theme_classic() +
  geom_vline(xintercept = c(9, 14, 20, 40, 48, 61, 69, 74, 85, 93, 104), lty = 2)
grid.arrange(p1, p2, p3, ncol = 1)

# p1 <- ggplot(data = dat_inc_wk, aes(x = time, y = case_rate, group = lk)) +
#   geom_line(alpha = 0.5) + theme_classic()
# p2 <- ggplot(data = dat_inc_wk, aes(x = time, y = death_rate, group = lk)) +
#   geom_line(alpha = 0.5) + theme_classic()
# p3 <- ggplot(data = dat_inc_wk, aes(x = time, y = cfr, group = lk)) +
#   geom_line(alpha = 0.5) + theme_classic()
# grid.arrange(p1, p2, p3, ncol = 1)

# Determine cutoff points for various waves/partial waves:
p4 <- ggplot(data = dat_inc_DE, aes(x = time, y = case_rate)) +
  geom_line() + theme_classic() +
  scale_x_continuous(breaks = seq(9, 115, by = 2)) +
  geom_vline(xintercept = c(9, 14, 20, 40, 48, 61, 69, 74, 85, 93, 104), lty = 2) + geom_hline(yintercept = 50)
grid.arrange(p1, p4, ncol = 1)

# Clean up:
rm(dat_inc_DE, dat_inc_wk_i)

# Calculate total cases/deaths for each wave/partial waves:
dat_cases_cumulative <- dat_inc_wk %>%
  filter(time %in% c(13, 14, 20, 22, 35, 39, 43, 48, 58, 61, 67, 69, 74, 84, 93, 104, 110)) %>%
  select(time:lk, pop, cases) %>%
  mutate(time = paste('wk', time, sep = '_')) %>%
  pivot_wider(names_from = time, values_from = cases) %>%
  mutate(cases_wave1 = wk_20,
         cases_wave2 = wk_61 - wk_39,
         cases_wave3 = wk_74 - wk_61,
         cases_wave4 = wk_104 - wk_84,
         cases_wave5 = wk_110 - wk_104,
         cases_wave1_1 = wk_14,
         cases_wave1_2 = wk_20 - wk_14,
         cases_wave2_1 = wk_48 - wk_39,
         cases_wave2_2 = wk_61 - wk_48,
         cases_wave3_1 = wk_69 - wk_61,
         cases_wave3_2 = wk_74 - wk_69,
         cases_wave4_1 = wk_93 - wk_84,
         cases_wave4_2 = wk_104 - wk_93,
         cases_pre2 = wk_39 - wk_13,
         cases_pre3 = wk_61 - wk_35,
         cases_pre4 = wk_84 - wk_58,
         cases_pre2_2 = wk_48 - wk_22,
         cases_pre3_2 = wk_69 - wk_43,
         cases_pre4_2 = wk_93 - wk_67,
         cases_summer1 = wk_39 - wk_20,
         cases_summer2 = wk_84 - wk_74,
         cases_pre_summer2 = wk_74 - wk_48) %>%
  select(ags2:pop, cases_wave1:cases_pre_summer2)

dat_deaths_cumulative <- dat_inc_wk %>%
  filter(time %in% c(13, 14, 20, 35, 39, 48, 58, 61, 69, 74, 84, 93, 104, 110)) %>%
  select(time:lk, deaths) %>%
  mutate(time = paste('wk', time, sep = '_')) %>%
  pivot_wider(names_from = time, values_from = deaths) %>%
  mutate(deaths_wave1 = wk_20,
         deaths_wave2 = wk_61 - wk_39,
         deaths_wave3 = wk_74 - wk_61,
         deaths_wave4 = wk_104 - wk_84,
         deaths_wave5 = wk_110 - wk_104,
         deaths_wave1_1 = wk_14,
         deaths_wave1_2 = wk_20 - wk_14,
         deaths_wave2_1 = wk_48 - wk_39,
         deaths_wave2_2 = wk_61 - wk_48,
         deaths_wave3_1 = wk_69 - wk_61,
         deaths_wave3_2 = wk_74 - wk_69,
         deaths_wave4_1 = wk_93 - wk_84,
         deaths_wave4_2 = wk_104 - wk_93,
         deaths_pre2 = wk_39 - wk_13,
         deaths_pre3 = wk_61 - wk_35,
         deaths_pre4 = wk_84 - wk_58,
         deaths_summer1 = wk_39 - wk_20,
         deaths_summer2 = wk_84 - wk_74) %>%
  select(lk, deaths_wave1:deaths_summer2)

# Combine and calculate rates:
dat_cumulative <- dat_cases_cumulative %>%
  left_join(dat_deaths_cumulative, by = 'lk') %>%
  mutate(cases_wave1_rate = cases_wave1 / pop * 10000,
         cases_wave2_rate = cases_wave2 / pop * 10000,
         cases_wave3_rate = cases_wave3 / pop * 10000,
         cases_wave4_rate = cases_wave4 / pop * 10000,
         cases_wave5_rate = cases_wave5 / pop * 10000,
         cases_wave1_1_rate = cases_wave1_1 / pop * 10000,
         cases_wave1_2_rate = cases_wave1_2 / pop * 10000,
         cases_wave2_1_rate = cases_wave2_1 / pop * 10000,
         cases_wave2_2_rate = cases_wave2_2 / pop * 10000,
         cases_wave3_1_rate = cases_wave3_1 / pop * 10000,
         cases_wave3_2_rate = cases_wave3_2 / pop * 10000,
         cases_wave4_1_rate = cases_wave4_1 / pop * 10000,
         cases_wave4_2_rate = cases_wave4_2 / pop * 10000,
         cases_pre2_rate = cases_pre2 / pop * 10000,
         cases_pre3_rate = cases_pre3 / pop * 10000,
         cases_pre4_rate = cases_pre4 / pop * 10000,
         cases_pre2_2_rate = cases_pre2_2 / pop * 10000,
         cases_pre3_2_rate = cases_pre3_2 / pop * 10000,
         cases_pre4_2_rate = cases_pre4_2 / pop * 10000,
         cases_summer1_rate = cases_summer1 / pop * 10000,
         cases_summer2_rate = cases_summer2 / pop * 10000,
         cases_pre_summer2_rate = cases_pre_summer2 / pop * 10000,
         deaths_wave1_rate = deaths_wave1 / pop * 10000,
         deaths_wave2_rate = deaths_wave2 / pop * 10000,
         deaths_wave3_rate = deaths_wave3 / pop * 10000,
         deaths_wave4_rate = deaths_wave4 / pop * 10000,
         deaths_wave5_rate = deaths_wave5 / pop * 10000,
         # deaths_wave1_1_rate = deaths_wave1_1 / pop * 10000,
         # deaths_wave1_2_rate = deaths_wave1_2 / pop * 10000,
         # deaths_wave2_1_rate = deaths_wave2_1 / pop * 10000,
         # deaths_wave2_2_rate = deaths_wave2_2 / pop * 10000,
         deaths_summer1_rate = deaths_summer1 / pop * 10000,
         deaths_summer2_rate = deaths_summer2 / pop * 10000,
         cfr_wave1 = deaths_wave1 / cases_wave1 * 100,
         cfr_wave2 = deaths_wave2 / cases_wave2 * 100,
         cfr_wave3 = deaths_wave3 / cases_wave2 * 100,
         cfr_wave4 = deaths_wave4 / cases_wave2 * 100,
         cfr_wave5 = deaths_wave5 / cases_wave2 * 100,
         cfr_wave1_1 = deaths_wave1_1 / cases_wave1_1 * 100,
         cfr_wave1_2 = deaths_wave1_2 / cases_wave1_2 * 100,
         cfr_wave2_1 = deaths_wave2_1 / cases_wave2_1 * 100,
         cfr_wave2_2 = deaths_wave2_2 / cases_wave2_2 * 100,
         cfr_wave3_1 = deaths_wave3_1 / cases_wave3_1 * 100,
         cfr_wave3_2 = deaths_wave3_2 / cases_wave3_2 * 100,
         cfr_wave4_1 = deaths_wave4_1 / cases_wave4_1 * 100,
         cfr_wave4_2 = deaths_wave4_2 / cases_wave4_2 * 100,
         cfr_summer1 = deaths_summer1 / cases_summer1 * 100,
         cfr_summer2 = deaths_summer2 / cases_summer2 * 100) %>%
  pivot_longer(-c(ags2:pop), names_to = 'outcome', values_to = 'val')
rm(dat_cases_cumulative, dat_deaths_cumulative)

# Explore noisiness in cumulative values over waves/partial waves:
dat_cumulative %>%
  filter(str_detect(outcome, 'rate')) %>%
  group_by(outcome) %>%
  summarise(mean = mean(val),
            median = median(val),
            sd = sd(val)) %>%
  print(n = nrow(dat_cumulative))

dat_plot <- dat_cumulative %>%
  filter(str_detect(outcome, 'rate') |
           str_detect(outcome, 'cfr'))

p5 <- ggplot(dat_plot, aes(x = bundesland, y = val, group = bundesland)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  facet_wrap(~ outcome, scales = 'free_y')
print(p5)

p6 <- ggplot(dat_plot %>%
               filter(str_detect(outcome, 'cases')),
             aes(x = outcome, y = val, group = outcome)) +
  geom_violin(fill = 'steelblue2') + theme_classic()
p7 <- ggplot(dat_plot %>%
               filter(str_detect(outcome, 'deaths')),
             aes(x = outcome, y = val, group = outcome)) +
  geom_violin(fill = 'steelblue2') + theme_classic()
p8 <- ggplot(dat_plot %>%
               filter(str_detect(outcome, 'cfr')),
             aes(x = outcome, y = val, group = outcome)) +
  geom_violin(fill = 'steelblue2') + theme_classic()
grid.arrange(p6, p7, p8, ncol = 1)

# Write cumulative counts/rates to file:
write_csv(dat_cumulative, file = 'data/formatted/STAND_cumulative_cases_and_deaths.csv')

# Clean up:
rm(list = ls())
