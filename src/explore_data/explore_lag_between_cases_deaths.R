# ---------------------------------------------------------------------------------------------------------------------
# Check how lag between cases and deaths varies by dataset
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(gridExtra)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data

# Read in crowdsourced data:
dat_inc_wk <- read_csv('data/formatted/weekly_covid_deaths_by_lk_INCIDENT.csv')

# Read in CDP data:
dat_inc_cdp <- read_csv('data/formatted/weekly_covid_deaths_by_lk_CDP.csv')

# Format:
dat_inc_wk <- dat_inc_wk %>%
  mutate(time = if_else(Year == 2020, Week, Week + 53))
dat_inc_cdp <- dat_inc_cdp %>%
  mutate(Week = as.numeric(Week)) %>%
  mutate(time = if_else(Year == 2020, Week, Week + 53))

# ---------------------------------------------------------------------------------------------------------------------

# Plot and calculate difference:

# Plot data by LK:
p1 <- ggplot(data = dat_inc_wk, aes(x = time, y = case_rate, group = lk)) + geom_line() + theme_classic()
p2 <- ggplot(data = dat_inc_wk, aes(x = time, y = death_rate, group = lk)) + geom_line() + theme_classic()
grid.arrange(p1, p2, ncol = 1)

p1 <- ggplot(data = dat_inc_cdp, aes(x = time, y = case_rate, group = lk)) + geom_line() + theme_classic()
p2 <- ggplot(data = dat_inc_cdp, aes(x = time, y = death_rate, group = lk)) + geom_line() + theme_classic()
grid.arrange(p1, p2, ncol = 1)

# Sum over LKs:
dat_inc_wk <- dat_inc_wk %>%
  group_by(time) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE),
            cases = sum(cases, na.rm = TRUE),
            pop = sum(pop)) %>%
  mutate(case_rate = cases / pop * 100000,
         death_rate = deaths / pop * 1000000)

dat_inc_cdp <- dat_inc_cdp %>%
  group_by(time) %>%
  summarise(deaths = sum(deaths),
            cases = sum(cases),
            pop = sum(pop)) %>%
  mutate(case_rate = cases / pop * 100000,
         death_rate = deaths / pop * 1000000)

# Plot:
dat_inc_plot <- dat_inc_wk %>%
  select(time, case_rate:death_rate) %>%
  pivot_longer(-time, names_to = 'metric', values_to = 'val')
dat_inc_plot_cdp <- dat_inc_cdp %>%
  select(time, case_rate:death_rate) %>%
  pivot_longer(-time, names_to = 'metric', values_to = 'val')

p1 <- ggplot(data = dat_inc_plot, aes(x = time, y = val, color = metric)) +
  geom_line() + scale_color_brewer(palette = 'Set1') + theme_classic()
p2 <- ggplot(data = dat_inc_plot_cdp, aes(x = time, y = val, color = metric)) +
  geom_line() + scale_color_brewer(palette = 'Set1') + theme_classic()
grid.arrange(p1, p2, ncol = 1)

# Calculate lag:
pks_cases <- which(
  sapply(1:length(dat_inc_wk$time), function(ix) {
    dat_inc_wk$case_rate[ix] > dat_inc_wk$case_rate[ix - 1] &
      dat_inc_wk$case_rate[ix] > dat_inc_wk$case_rate[ix + 1]
  }) == TRUE)

pks_deaths <- which(
  sapply(1:length(dat_inc_wk$time), function(ix) {
    dat_inc_wk$death_rate[ix] > dat_inc_wk$death_rate[ix - 1] &
      dat_inc_wk$death_rate[ix] > dat_inc_wk$death_rate[ix + 1]
  }) == TRUE)

pks_cases_cdp <- which(
  sapply(1:length(dat_inc_cdp$time), function(ix) {
    dat_inc_cdp$case_rate[ix] > dat_inc_cdp$case_rate[ix - 1] &
      dat_inc_cdp$case_rate[ix] > dat_inc_cdp$case_rate[ix + 1]
  }) == TRUE)

pks_deaths_cdp <- which(
  sapply(1:length(dat_inc_cdp$time), function(ix) {
    dat_inc_cdp$death_rate[ix] > dat_inc_cdp$death_rate[ix - 1] &
      dat_inc_cdp$death_rate[ix] > dat_inc_cdp$death_rate[ix + 1]
  }) == TRUE)

p1 <- ggplot(data = dat_inc_plot, aes(x = time, y = val, color = metric)) +
  geom_line() + scale_color_brewer(palette = 'Set1') + theme_classic() +
  geom_vline(xintercept = pks_cases + 12) +
  geom_vline(xintercept = pks_deaths + 12, lty = 2)
p2 <- ggplot(data = dat_inc_plot_cdp, aes(x = time, y = val, color = metric)) +
  geom_line() + scale_color_brewer(palette = 'Set1') + theme_classic() +
  geom_vline(xintercept = pks_cases_cdp + 9) +
  geom_vline(xintercept = pks_deaths_cdp + 9, lty = 2)
grid.arrange(p1, p2, ncol = 1)

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
dev.off()
