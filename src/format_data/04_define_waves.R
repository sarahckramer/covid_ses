# ---------------------------------------------------------------------------------------------------------------------
# Split cumulative data into distinct waves, and explore whether partial waves or full waves should be used
# ---------------------------------------------------------------------------------------------------------------------

# Load libraries:
library(tidyverse)
library(gridExtra)
library(viridis)
library(mgcv)
library(sf)

# Read in data:
dat_inc_wk <- read_csv('data/formatted/STAND_weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')
dat_inc_wk_agestrat <- read_csv('data/formatted/BYAGE_weekly_covid_deaths_by_lk_CUMULATIVE_CDP.csv')
dat_inc_wk_i <- read_csv('data/formatted/weekly_covid_deaths_by_lk_INCIDENT_CDP.csv')

# Add time column:
dat_inc_wk <- dat_inc_wk %>%
  mutate(Week = as.numeric(Week)) %>%
  mutate(time = if_else(Year == 2020, Week, Week + 53),
         time = if_else(Year == 2022, time + 52, time),
         .before = ags2)
dat_inc_wk_agestrat <- dat_inc_wk_agestrat %>%
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

# Determine cutoff points for various waves/partial waves:
p4 <- ggplot(data = dat_inc_DE, aes(x = time, y = case_rate)) +
  geom_line() + theme_classic() +
  scale_x_continuous(breaks = seq(9, 135, by = 2)) +
  geom_vline(xintercept = c(9, 14, 20, 40, 48, 61, 69, 74, 85, 93, 104, 113, 126), lty = 2) + geom_hline(yintercept = 50)
grid.arrange(p1, p4, ncol = 1)

dat_inc_DE <- dat_inc_DE %>%
  inner_join(dat_inc_wk %>% select(time, Year, Week, date) %>% unique(), by = 'time')

x_breaks <- c(9, 17 + 5/7, 26 + 3/7, 35 + 2/7, 44,
              52 + 5/7, 61 + 1/7, 69 + 6/7, 78 + 4/7, 87 + 3/7, 96 + 1/7,
              104 + 6/7, 113 + 2/7, 122)
x_labs <- c('Mar 2020', 'May 2020', 'Jul 2020', 'Sep 2020', 'Nov 2020',
            'Jan 2021', 'Mar 2021', 'May 2021', 'Jul 2021', 'Sep 2021', 'Nov 2021',
            'Jan 2022', 'Mar 2022', 'May 2022')

p.s1.a <- ggplot(data = dat_inc_DE %>% filter(time <= 126), aes(x = time, y = case_rate / 10)) +
  geom_rect(data = dat_inc_DE[1, ], aes(xmin = 9, xmax = 20, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#e41a1c') +
  geom_rect(data = dat_inc_DE[1, ], aes(xmin = 39, xmax = 61, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#4daf4a') +
  geom_rect(data = dat_inc_DE[1, ], aes(xmin = 61, xmax = 74, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#377eb8') +
  geom_rect(data = dat_inc_DE[1, ], aes(xmin = 84, xmax = 104, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#984ea3') +
  geom_rect(data = dat_inc_DE[1, ], aes(xmin = 104, xmax = 126, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#f781bf') +
  geom_line() +
  # geom_vline(xintercept = c(9, 20, 39, 61, 74, 84, 104, 126), col = 'black') +
  geom_vline(xintercept = c(14, 48, 69, 93, 113), col = c('#e41a1c', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'), lty = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 35, vjust = 0.62),
        plot.tag = element_text(size = 20),
        plot.tag.position = c(0.005, 0.97)) +
  scale_x_continuous(breaks = x_breaks, labels = x_labs) +
  # scale_x_continuous(breaks = c(53, 105), labels = c('2021', '2022')) +
  scale_y_continuous(limits = c(0, 200), n.breaks = 10) +
  labs(x = 'Date', y = 'Incidence\n(per 10,000 Pop.)', tag = 'A')
p.s1.b <- ggplot(data = dat_inc_DE %>% filter(time <= 126), aes(x = time, y = cfr * 100)) +
  geom_rect(data = dat_inc_DE[1, ], aes(xmin = 9, xmax = 20, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#e41a1c') +
  geom_rect(data = dat_inc_DE[1, ], aes(xmin = 39, xmax = 61, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#4daf4a') +
  geom_rect(data = dat_inc_DE[1, ], aes(xmin = 61, xmax = 74, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#377eb8') +
  geom_rect(data = dat_inc_DE[1, ], aes(xmin = 84, xmax = 104, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#984ea3') +
  geom_rect(data = dat_inc_DE[1, ], aes(xmin = 104, xmax = 126, ymin = -Inf, ymax = Inf), alpha = 0.1, fill = '#f781bf') +
  geom_line() +
  # geom_vline(xintercept = c(9, 20, 39, 61, 74, 84, 104, 126), col = 'black') +
  geom_vline(xintercept = c(14, 48, 69, 93, 113), col = c('#e41a1c', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'), lty = 2) +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 35, vjust = 0.62),
        plot.tag = element_text(size = 20),
        plot.tag.position = c(0.005, 0.97)) +
  scale_x_continuous(breaks = x_breaks, labels = x_labs) +
  # scale_x_continuous(breaks = c(53, 105), labels = c('2021', '2022')) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = 'Date', y = '\nCase Fatality Rate (%)', tag = 'B')

figs1 <- arrangeGrob(p.s1.a, p.s1.b, ncol = 1)
plot(figs1)
# ggsave('results/FigureS1.svg', width = 9.5, height = 6.5, figs1)

# Clean up:
rm(dat_inc_DE, dat_inc_wk_i, p1, p2, p3, p4, p.s1.a, p.s1.b)

# ---------------------------------------------------------------------------------------------------------------------

# Calculate total cases/deaths for each wave/partial waves:
dat_cases_cumulative <- dat_inc_wk %>%
  filter(time %in% c(13, 14, 20, 22, 35, 39, 43, 48, 58, 61, 67, 69, 74, 78, 84, 87, 93, 104, 113, 126)) %>%
  select(time:lk, pop, cases) %>%
  mutate(time = paste('wk', time, sep = '_')) %>%
  pivot_wider(names_from = time, values_from = cases) %>%
  mutate(cases_wave1 = wk_20,
         cases_wave2 = wk_61 - wk_39,
         cases_wave3 = wk_74 - wk_61,
         cases_wave4 = wk_104 - wk_84,
         cases_wave5 = wk_126 - wk_104,
         cases_wave1_1 = wk_14,
         cases_wave1_2 = wk_20 - wk_14,
         cases_wave2_1 = wk_48 - wk_39,
         cases_wave2_2 = wk_61 - wk_48,
         cases_wave3_1 = wk_69 - wk_61,
         cases_wave3_2 = wk_74 - wk_69,
         cases_wave4_1 = wk_93 - wk_84,
         cases_wave4_2 = wk_104 - wk_93,
         cases_wave5_1 = wk_113 - wk_104,
         cases_wave5_2 = wk_126 - wk_113,
         cases_pre2 = wk_39 - wk_13,
         cases_pre3 = wk_61 - wk_35,
         cases_pre4 = wk_84 - wk_58,
         cases_pre5 = wk_104 - wk_78,
         cases_pre2_2 = wk_48 - wk_22,
         cases_pre3_2 = wk_69 - wk_43,
         cases_pre4_2 = wk_93 - wk_67,
         cases_pre5_2 = wk_113 - wk_87,
         cases_summer1 = wk_39 - wk_20,
         cases_summer2 = wk_84 - wk_74,
         cases_pre_summer2 = wk_74 - wk_48) %>%
  select(ags2:pop, cases_wave1:cases_pre_summer2)

dat_deaths_cumulative <- dat_inc_wk %>%
  filter(time %in% c(13, 14, 20, 35, 39, 48, 58, 61, 69, 74, 78, 84, 93, 104, 113, 126)) %>%
  select(time:lk, deaths) %>%
  mutate(time = paste('wk', time, sep = '_')) %>%
  pivot_wider(names_from = time, values_from = deaths) %>%
  mutate(deaths_wave1 = wk_20,
         deaths_wave2 = wk_61 - wk_39,
         deaths_wave3 = wk_74 - wk_61,
         deaths_wave4 = wk_104 - wk_84,
         deaths_wave5 = wk_126 - wk_104,
         deaths_wave1_1 = wk_14,
         deaths_wave1_2 = wk_20 - wk_14,
         deaths_wave2_1 = wk_48 - wk_39,
         deaths_wave2_2 = wk_61 - wk_48,
         deaths_wave3_1 = wk_69 - wk_61,
         deaths_wave3_2 = wk_74 - wk_69,
         deaths_wave4_1 = wk_93 - wk_84,
         deaths_wave4_2 = wk_104 - wk_93,
         deaths_wave5_1 = wk_113 - wk_104,
         deaths_wave5_2 = wk_126- wk_113,
         deaths_pre2 = wk_39 - wk_13,
         deaths_pre3 = wk_61 - wk_35,
         deaths_pre4 = wk_84 - wk_58,
         deaths_pre5 = wk_104 - wk_78,
         deaths_summer1 = wk_39 - wk_20,
         deaths_summer2 = wk_84 - wk_74) %>%
  select(lk, deaths_wave1:deaths_summer2)

# Same for age-stratified data:
dat_cases_cumulative_agestrat <- dat_inc_wk_agestrat %>%
  filter(time %in% c(13, 14, 20, 22, 35, 39, 43, 48, 58, 61, 67, 69, 74, 78, 84, 87, 93, 104, 113, 126)) %>%
  select(time:age, pop, cases) %>%
  mutate(time = paste('wk', time, sep = '_')) %>%
  pivot_wider(names_from = time, values_from = cases) %>%
  mutate(cases_wave1 = wk_20,
         cases_wave2 = wk_61 - wk_39,
         cases_wave3 = wk_74 - wk_61,
         cases_wave4 = wk_104 - wk_84,
         cases_wave5 = wk_126 - wk_104,
         cases_wave1_1 = wk_14,
         cases_wave1_2 = wk_20 - wk_14,
         cases_wave2_1 = wk_48 - wk_39,
         cases_wave2_2 = wk_61 - wk_48,
         cases_wave3_1 = wk_69 - wk_61,
         cases_wave3_2 = wk_74 - wk_69,
         cases_wave4_1 = wk_93 - wk_84,
         cases_wave4_2 = wk_104 - wk_93,
         cases_wave5_1 = wk_113 - wk_104,
         cases_wave5_2 = wk_126 - wk_113,
         cases_pre2 = wk_39 - wk_13,
         cases_pre3 = wk_61 - wk_35,
         cases_pre4 = wk_84 - wk_58,
         cases_pre5 = wk_104 - wk_78,
         cases_pre2_2 = wk_48 - wk_22,
         cases_pre3_2 = wk_69 - wk_43,
         cases_pre4_2 = wk_93 - wk_67,
         cases_pre5_2 = wk_113 - wk_87,
         cases_summer1 = wk_39 - wk_20,
         cases_summer2 = wk_84 - wk_74,
         cases_pre_summer2 = wk_74 - wk_48) %>%
  select(ags2:pop, cases_wave1:cases_pre_summer2)

dat_deaths_cumulative_agestrat <- dat_inc_wk_agestrat %>%
  filter(time %in% c(13, 14, 20, 35, 39, 48, 58, 61, 69, 74, 78, 84, 93, 104, 113, 126)) %>%
  select(time:age, deaths) %>%
  mutate(time = paste('wk', time, sep = '_')) %>%
  pivot_wider(names_from = time, values_from = deaths) %>%
  mutate(deaths_wave1 = wk_20,
         deaths_wave2 = wk_61 - wk_39,
         deaths_wave3 = wk_74 - wk_61,
         deaths_wave4 = wk_104 - wk_84,
         deaths_wave5 = wk_126 - wk_104,
         deaths_wave1_1 = wk_14,
         deaths_wave1_2 = wk_20 - wk_14,
         deaths_wave2_1 = wk_48 - wk_39,
         deaths_wave2_2 = wk_61 - wk_48,
         deaths_wave3_1 = wk_69 - wk_61,
         deaths_wave3_2 = wk_74 - wk_69,
         deaths_wave4_1 = wk_93 - wk_84,
         deaths_wave4_2 = wk_104 - wk_93,
         deaths_wave5_1 = wk_113 - wk_104,
         deaths_wave5_2 = wk_126- wk_113,
         deaths_pre2 = wk_39 - wk_13,
         deaths_pre3 = wk_61 - wk_35,
         deaths_pre4 = wk_84 - wk_58,
         deaths_pre5 = wk_104 - wk_78,
         deaths_summer1 = wk_39 - wk_20,
         deaths_summer2 = wk_84 - wk_74) %>%
  select(lk, age, deaths_wave1:deaths_summer2)

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
         cases_wave5_1_rate = cases_wave5_1 / pop * 10000,
         cases_wave5_2_rate = cases_wave5_2 / pop * 10000,
         cases_pre2_rate = cases_pre2 / pop * 10000,
         cases_pre3_rate = cases_pre3 / pop * 10000,
         cases_pre4_rate = cases_pre4 / pop * 10000,
         cases_pre5_rate = cases_pre5 / pop * 10000,
         cases_pre2_2_rate = cases_pre2_2 / pop * 10000,
         cases_pre3_2_rate = cases_pre3_2 / pop * 10000,
         cases_pre4_2_rate = cases_pre4_2 / pop * 10000,
         cases_pre5_2_rate = cases_pre5_2 / pop * 10000,
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
         cfr_wave5_1 = deaths_wave5_1 / cases_wave5_1 * 100,
         cfr_wave5_2 = deaths_wave5_2 / cases_wave5_2 * 100,
         cfr_summer1 = deaths_summer1 / cases_summer1 * 100,
         cfr_summer2 = deaths_summer2 / cases_summer2 * 100) %>%
  pivot_longer(-c(ags2:pop), names_to = 'outcome', values_to = 'val')
rm(dat_cases_cumulative, dat_deaths_cumulative)

# And again, for age-stratified data:
dat_cumulative_agestrat <- dat_cases_cumulative_agestrat %>%
  left_join(dat_deaths_cumulative_agestrat, by = c('lk', 'age')) %>%
  mutate(age = if_else(age == '80', '60', age)) %>%
  mutate(age = if_else(age == '35', '15', age)) %>%
  group_by(ags2, bundesland, lk, age) %>%
  summarise(across(pop:deaths_summer2, sum)) %>%
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
         cases_wave5_1_rate = cases_wave5_1 / pop * 10000,
         cases_wave5_2_rate = cases_wave5_2 / pop * 10000,
         cases_pre2_rate = cases_pre2 / pop * 10000,
         cases_pre3_rate = cases_pre3 / pop * 10000,
         cases_pre4_rate = cases_pre4 / pop * 10000,
         cases_pre5_rate = cases_pre5 / pop * 10000,
         cases_pre2_2_rate = cases_pre2_2 / pop * 10000,
         cases_pre3_2_rate = cases_pre3_2 / pop * 10000,
         cases_pre4_2_rate = cases_pre4_2 / pop * 10000,
         cases_pre5_2_rate = cases_pre5_2 / pop * 10000,
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
         cfr_wave5_1 = deaths_wave5_1 / cases_wave5_1 * 100,
         cfr_wave5_2 = deaths_wave5_2 / cases_wave5_2 * 100,
         cfr_summer1 = deaths_summer1 / cases_summer1 * 100,
         cfr_summer2 = deaths_summer2 / cases_summer2 * 100) %>%
  pivot_longer(-c(ags2:pop), names_to = 'outcome', values_to = 'val')
rm(dat_cases_cumulative_agestrat, dat_deaths_cumulative_agestrat)

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
dat_plot_agestrat <- dat_cumulative_agestrat %>%
  filter((str_detect(outcome, 'cases_wave') & str_detect(outcome, 'rate') & !str_detect(outcome, '_1') & !str_detect(outcome, '_2')) |
           str_detect(outcome, 'cfr') & !str_detect(outcome, 'summer') & !str_detect(outcome, '_1') & !str_detect(outcome, '_2'))

p5 <- ggplot(dat_plot, aes(x = bundesland, y = val, group = bundesland)) +
  geom_boxplot(fill = 'steelblue2') + theme_classic() +
  facet_wrap(~ outcome, scales = 'free_y')
print(p5)

# p5_age <- ggplot(dat_plot_agestrat, aes(x = bundesland, y = val, group = bundesland, fill = age)) +
#   geom_boxplot() + theme_classic() + scale_fill_viridis(discrete = TRUE) +
#   facet_grid(outcome ~ age, scales = 'free_y')
# print(p5_age)

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

p6_age <- ggplot(dat_plot_agestrat %>%
                   filter(str_detect(outcome, 'cases')),
                 aes(x = outcome, y = val, group = paste(outcome, age), fill = age)) +
  geom_boxplot() + theme_classic() + scale_fill_viridis(discrete = TRUE)
p8_age <- ggplot(dat_plot_agestrat %>%
                   filter(str_detect(outcome, 'cfr')),
                 aes(x = outcome, y = val, group = paste(outcome, age), fill = age)) +
  geom_boxplot() + theme_classic() + scale_fill_viridis(discrete = TRUE)
grid.arrange(p6_age, p8_age, ncol = 1)

# Write cumulative counts/rates to file:
write_csv(dat_cumulative, file = 'data/formatted/STAND_cumulative_cases_and_deaths.csv')
write_csv(dat_cumulative_agestrat, file = 'data/formatted/BYAGE_cumulative_cases_and_deaths.csv')

# ---------------------------------------------------------------------------------------------------------------------

# Compare reported to excess deaths (at Bundesland level):

# Read in data:
euro_dat <- read_tsv('data/raw/revision1/excess_deaths/eurostat_nuts3_estat_demo_r_mweek3.tsv')
# https://ec.europa.eu/eurostat/cache/metadata/en/demomwk_esms.htm#shortmeta_last_updateDisseminated

# Clean up/format:
euro_dat <- euro_dat %>%
  rename('all_info' = 1) %>%
  pivot_longer(-1, names_to = 'date', values_to = 'deaths')# %>%

euro_dat <- euro_dat %>%
  filter(str_detect(all_info, 'DE')) %>%
  mutate(sex = str_split(all_info, ',', simplify = TRUE)[, 3],
         age = str_split(all_info, ',', simplify = TRUE)[, 4],
         geo = str_split(all_info, ',', simplify = TRUE)[, 5]) %>%
  select(-all_info)

euro_dat <- euro_dat %>%
  mutate(deaths = as.numeric(deaths)) %>%
  mutate(year = str_sub(date, 1, 4),
         week = str_sub(date, -2, -1)) %>%
  mutate(year = as.numeric(year),
         week = as.numeric(week))

# Keep only data of interest:
euro_dat <- euro_dat %>%
  filter(sex == 'T', age != 'UNK', geo != 'DEX') %>%
  filter(!is.na(deaths)) %>%
  select(-sex)
euro_dat <- euro_dat %>%
  filter(year >= 2015 & year < 2023)

# Get BL-level data:
euro_dat_ALL <- euro_dat
euro_dat <- euro_dat %>%
  filter(geo != 'DE') %>%
  select(-age)

# Fit GAM of year/week for years 2015-2019, for each BL:
mod_list <- vector('list', 16)
pred_list <- vector('list', 16)

for (i in 1:16) {
  
  region <- unique(euro_dat$geo)[i]
  
  df_temp <- euro_dat %>%
    filter(geo == region)
  df_temp <- df_temp %>%
    filter(year < 2020)
  
  # Fit GAM:
  m1 <- gam(deaths ~ year + s(week, bs = 'cc', k = 53), data = df_temp, family = 'nb', method = 'ML')
  mod_list[[i]] <- m1
  
  # Predict deaths for 2020-22:
  df_pred <- euro_dat %>%
    filter(geo == region) %>%
    filter(year == 2020)
  
  ilink <- family(m1)$linkinv
  
  df_pred <- df_pred %>%
    bind_cols(predict(m1, df_pred, type = 'link', se.fit = TRUE)) %>%
    mutate(fitted = ilink(fit),
           lower = ilink(fit - (2 * se.fit)),
           upper = ilink(fit + (2 * se.fit))) %>%
    select(geo, week, fitted:upper)
  
  pred_list[[i]] <- df_pred
  
}
rm(i, region, df_temp, df_pred, m1, ilink)

# Join with observed death data:
euro_dat <- euro_dat %>%
  filter(year >= 2020) %>%
  select(geo, date, year:week, deaths) %>%
  left_join(bind_rows(pred_list),
            by = c('geo', 'week'))

# Sum over waves for observed and expected:
df_reported <- euro_dat %>%
  select(geo, date, deaths) %>%
  pivot_wider(names_from = date, values_from = deaths) %>%
  rowwise() %>%
  mutate(reported_wave1 = rowSums(across(`2020-W11`:`2020-W22`)),
         # reported_wave1_1 = rowSums(across(`2020-W11`:`2020-W16`)),
         # reported_wave1_2 = rowSums(across(`2020-W17`:`2020-W22`)),
         reported_wave2 = rowSums(across(`2020-W42`:`2021-W10`)),
         reported_wave3 = rowSums(across(`2021-W11`:`2021-W23`)),
         reported_wave4 = rowSums(across(`2021-W34`:`2022-W01`)),
         reported_wave5 = rowSums(across(`2022-W02`:`2022-W23`))) %>%
  select(geo, contains('reported')) %>%
  pivot_longer(-geo, names_to = 'wave', values_to = 'reported') %>%
  mutate(wave = str_remove(wave, 'reported_'))

df_predicted <- euro_dat %>%
  select(geo, date, fitted) %>%
  pivot_wider(names_from = date, values_from = fitted) %>%
  rowwise() %>%
  mutate(fitted_wave1 = rowSums(across(`2020-W11`:`2020-W22`)),
         # fitted_wave1_1 = rowSums(across(`2020-W11`:`2020-W16`)),
         # fitted_wave1_2 = rowSums(across(`2020-W17`:`2020-W22`)),
         fitted_wave2 = rowSums(across(`2020-W42`:`2021-W10`)),
         fitted_wave3 = rowSums(across(`2021-W11`:`2021-W23`)),
         fitted_wave4 = rowSums(across(`2021-W34`:`2022-W01`)),
         fitted_wave5 = rowSums(across(`2022-W02`:`2022-W23`))) %>%
  select(geo, contains('fitted')) %>%
  pivot_longer(-geo, names_to = 'wave', values_to = 'fitted') %>%
  mutate(wave = str_remove(wave, 'fitted_'))

# Calculate excess deaths by Bundesland and wave:
df_excess <- df_reported %>%
  left_join(df_predicted,
            by = c('geo', 'wave')) %>%
  mutate(excess = reported - fitted)
# rm(df_reported, df_predicted)

# Calculate Bundesland-level reported COVID deaths:
dat_cumulative <- dat_cumulative %>%
  filter(str_detect(outcome, 'deaths_wave') & !str_detect(outcome, 'rate')) %>%
  select(ags2:bundesland, outcome, pop, val) %>%
  pivot_wider(names_from = outcome, values_from = val) %>%
  select(!contains('1_') & !contains('2_') & !contains('3_') & !contains('4_') & !contains('5_')) %>%
  group_by(ags2, bundesland) %>%
  summarise(across(everything(), ~ sum(.x)))# %>%
# mutate(mortality_wave1_1 = deaths_wave1_1 / pop * 10000,
#        mortality_wave1_2 = deaths_wave1_2 / pop * 10000,
#        mortality_wave2 = deaths_wave2 / pop * 10000,
#        mortality_wave3 = deaths_wave3 / pop * 10000,
#        mortality_wave4 = deaths_wave4 / pop * 10000,
#        mortality_wave5 = deaths_wave5 / pop * 10000) %>%
# select(ags2, bundesland, pop, contains('mortality'))

# Join and compare:
map_bl <- st_read(dsn = 'data/raw/map/vg2500_12-31.gk3.shape/vg2500/VG2500_LAN.shp')[1:16, ]

df_comp <- map_bl %>%
  as_tibble() %>%
  select(NUTS, ARS) %>%
  left_join(df_excess %>%
              select(geo, wave, excess),
            by = c('NUTS' = 'geo')) %>%
  left_join(dat_cumulative %>%
              pivot_longer(contains('deaths'), names_to = 'wave', values_to = 'covid_deaths') %>%
              mutate(wave = str_remove(wave, 'deaths_')),
            by = c('ARS' = 'ags2', 'wave' = 'wave')) %>%
  mutate(excess = excess / pop * 10000,
         covid_deaths = covid_deaths / pop * 10000) %>%
  select(NUTS:ARS, bundesland, wave, pop, covid_deaths, excess)

figS11 <- ggplot(data = df_comp, aes(x = covid_deaths, y = excess, col = paste0('Wave ', str_sub(wave, 5)))) +
  geom_rect(data = df_comp[1, ], aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0), alpha = 0.4, fill = 'gray90', col = 'white') +
  geom_abline(slope = 1, intercept = 0, lty = 2, col = 'gray60') +
  geom_point(size = 2.0) +
  theme_classic() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.position = 'right') +#,
  # legend.position.inside = c(0.898, 0.278)) + # c(0.092, 0.85)
  labs(x = 'Reported COVID-19 Deaths (per 10,000 Pop.)',
       y = 'Excess Deaths (per 10,000 Pop.)',
       col = '') +
  geom_text(label = 'rho = 0.00', x = 12, y = 5, col = 'black') +
  scale_x_continuous(transform = 'pseudo_log', expand = c(0.022, 0), breaks = c(0, 1, 2, 5, 10, 15)) +
  scale_y_continuous(transform = 'pseudo_log', breaks = c(-1, 0, 1, 2, 5, 10, 20)) +
  scale_color_manual(values = c('#e41a1c', '#4daf4a', '#377eb8', '#984ea3', '#f781bf'))
ggsave('results/FigureS11_NEW.svg', figS11, width = 8.0, height = 4.267)

cor.test(df_comp$covid_deaths, df_comp$excess)

df_comp <- df_comp %>%
  mutate(undercount = excess / covid_deaths)

df_comp %>%
  group_by(wave) %>%
  summarise(perc_pos = length(undercount[undercount > 0])/length(undercount),
            perc_over1 = length(undercount[undercount > 1]) / length(undercount[undercount > 0]))

# df_comp %>%
#   mutate(undercount = excess / covid_deaths) %>%
#   filter(undercount > 0) %>%
#   ggplot(aes(x = bundesland, y = undercount, col = wave, group = wave)) +
#   geom_point() + geom_line() +
#   theme_classic()

# Clean up:
rm(list = ls())
