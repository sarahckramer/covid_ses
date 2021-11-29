# ---------------------------------------------------------------------------------------------------------------------
# Plot incidence/mortality data by GISD (and other predictors?)
# ---------------------------------------------------------------------------------------------------------------------

# Setup

# Load libraries:
library(tidyverse)
library(viridis)
library(gridExtra)

# ---------------------------------------------------------------------------------------------------------------------

# Read in and format data

# Read in weekly data:
dat_inc_wk_cdp <- read_csv('data/formatted/weekly_covid_deaths_by_lk_CDP.csv')

# Create time variable:
dat_inc_wk_cdp <- dat_inc_wk_cdp %>%
  mutate(Week = as.numeric(Week)) %>%
  mutate(time = if_else(Year == 2020, Week, Week + 53)) %>%
  mutate(deaths = ifelse(deaths > cases, NA, deaths)) %>%
  mutate(death_rate = deaths / pop * 100000,
         ifr = deaths / cases * 100)

# Add column for Bundesland:
dat_inc_wk_cdp <- dat_inc_wk_cdp %>%
  mutate(bundesland = lk,
         bundesland = if_else(str_starts(lk, '01'), 'SchleswigHolstein', bundesland),
         bundesland = if_else(str_starts(lk, '02'), 'Hamburg', bundesland),
         bundesland = if_else(str_starts(lk, '03'), 'Niedersachsen', bundesland),
         bundesland = if_else(str_starts(lk, '04'), 'Bremen', bundesland),
         bundesland = if_else(str_starts(lk, '05'), 'NordrheinWestfalen', bundesland),
         bundesland = if_else(str_starts(lk, '06'), 'Hessen', bundesland),
         bundesland = if_else(str_starts(lk, '07'), 'RheinlandPfalz', bundesland),
         bundesland = if_else(str_starts(lk, '08'), 'BadenWuerttemberg', bundesland),
         bundesland = if_else(str_starts(lk, '09'), 'Bayern', bundesland),
         bundesland = if_else(str_starts(lk, '10'), 'Saarland', bundesland),
         bundesland = if_else(str_starts(lk, '11'), 'Berlin', bundesland),
         bundesland = if_else(str_starts(lk, '12'), 'Brandenburg', bundesland),
         bundesland = if_else(str_starts(lk, '13'), 'MecklenburgVorpommern', bundesland),
         bundesland = if_else(str_starts(lk, '14'), 'Sachsen', bundesland),
         bundesland = if_else(str_starts(lk, '15'), 'SachsenAnhalt', bundesland),
         bundesland = if_else(str_starts(lk, '16'), 'Thueringen', bundesland))

# Read in and incorporate covariate data:
inkar_dat <- read_csv('data/formatted/independent_vars/ses_independent_variables.csv')
mobility_dat <- read_csv('data/formatted/independent_vars/mobility_dat_WEEKLY.csv')
policy_dat <- read_csv('data/formatted/independent_vars/policy_dat_WEEKLY.csv')

# Calculate GISD quantiles:
inkar_dat <- inkar_dat %>%
  mutate(GISD_Quant = ntile(GISD_Score, 5))

# predictors: perc_65plus, perc_women, pop_dens, living_area, perc_service, perc_production, care_home_beds, hosp_beds, GISD_Score
# (commuters_in, commuters_out)
# (mobility? policy (masks)?)

dat_inc_wk_cdp <- dat_inc_wk_cdp %>%
  left_join(inkar_dat, by = c('lk' = 'lk_code')) %>%
  left_join(mobility_dat, by = c('lk' = 'lk_code',
                                 'Week' = 'week',
                                 'Year' = 'year')) %>%
  left_join(policy_dat %>%
              mutate(Week = as.numeric(Week)), by = c('lk' = 'ags5',
                                                      'Week' = 'Week',
                                                      'Year' = 'Year'))

# ---------------------------------------------------------------------------------------------------------------------

# Plots

# Save to file:
pdf('results/plots/plot_data_by_covariates.pdf', width = 14, height = 10)

# Get plots:
for (covariate in names(dat_inc_wk_cdp)[c(11, 13)]) {
  dat_plot <- dat_inc_wk_cdp %>%
    rename('plot_covar' = all_of(covariate))
  
  p1 <- ggplot(data = dat_plot, aes(x = time, y = case_rate, group = lk, col = plot_covar)) +
    geom_line(alpha = 0.5) + theme_classic() + scale_color_viridis(discrete = TRUE) + labs(title = covariate)
  p2 <- ggplot(data = dat_plot, aes(x = time, y = death_rate, group = lk, col = plot_covar)) +
    geom_line(alpha = 0.5) + theme_classic() + scale_color_viridis(discrete = TRUE) + labs(title = covariate)
  p3 <- ggplot(data = dat_plot, aes(x = time, y = ifr, group = lk, col = plot_covar)) +
    geom_line(alpha = 0.5) + theme_classic() + scale_color_viridis(discrete = TRUE) + labs(title = covariate)
  grid.arrange(p1, p2, p3, ncol = 1)
}

for (covariate in names(dat_inc_wk_cdp)[c(14, 16, 19:21, 23:24, 26:33, 44:49)]) {
  dat_plot <- dat_inc_wk_cdp %>%
    rename('plot_covar' = all_of(covariate))
  
  p1 <- ggplot(data = dat_plot, aes(x = time, y = case_rate, group = lk, col = plot_covar)) +
    geom_line(alpha = 0.5) + theme_classic() + scale_color_viridis() + labs(title = covariate)
  p2 <- ggplot(data = dat_plot, aes(x = time, y = death_rate, group = lk, col = plot_covar)) +
    geom_line(alpha = 0.5) + theme_classic() + scale_color_viridis() + labs(title = covariate)
  p3 <- ggplot(data = dat_plot, aes(x = time, y = ifr, group = lk, col = plot_covar)) +
    geom_line(alpha = 0.5) + theme_classic() + scale_color_viridis() + labs(title = covariate)
  grid.arrange(p1, p2, p3, ncol = 1)
}

# Plot case/death/ifr values for GISD quantiles:
dat_inc_by_quant <- dat_inc_wk_cdp %>%
  select(Year:time, GISD_Quant) %>%
  group_by(Year, Week, time, GISD_Quant) %>%
  summarise(death_rate = sum(deaths) / sum(pop),
            case_rate = sum(cases) / sum(pop),
            ifr = sum(deaths) / sum(cases))

p1 <- ggplot(data = dat_inc_by_quant, aes(x = time, y = case_rate, group = GISD_Quant, col = GISD_Quant)) +
  geom_line(alpha = 0.5) + theme_classic() + scale_color_viridis() + labs(title = 'GISD_Quant')
p2 <- ggplot(data = dat_inc_by_quant, aes(x = time, y = death_rate, group = GISD_Quant, col = GISD_Quant)) +
  geom_line(alpha = 0.5) + theme_classic() + scale_color_viridis() + labs(title = 'GISD_Quant')
p3 <- ggplot(data = dat_inc_by_quant, aes(x = time, y = ifr, group = GISD_Quant, col = GISD_Quant)) +
  geom_line(alpha = 0.5) + theme_classic() + scale_color_viridis() + labs(title = 'GISD_Quant')
grid.arrange(p1, p2, p3, ncol = 1)

# Explore mobility data by GISD:
p1 <- ggplot(data = dat_inc_wk_cdp, aes(x = time, y = mobility_change_weekly, group = lk, col = GISD_Score)) +
  geom_line(alpha = 0.5) + theme_classic() + scale_color_viridis() + labs(title = 'GISD_Score')
p2 <- ggplot(data = dat_inc_wk_cdp, aes(x = time, y = mobility_change_weekly, group = lk, col = GISD_Quant)) +
  geom_line(alpha = 0.5) + theme_classic() + scale_color_viridis() + labs(title = 'GISD_Quant')
grid.arrange(p1, p2, ncol = 1)

# Close plot file:
dev.off()

# ---------------------------------------------------------------------------------------------------------------------

# Clean up:
rm(list = ls())
