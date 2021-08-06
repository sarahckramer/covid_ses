# ---------------------------------------------------------------------------------------------------------------------
# Read in case/death data
# ---------------------------------------------------------------------------------------------------------------------

# Read in incident data:
dat_inc <- read_csv('data/formatted/weekly_covid_deaths_by_lk_INCIDENT.csv')

# Format:
dat_inc <- dat_inc %>%
  mutate(Week = if_else(Year == 2020, Week, Week + 53)) %>%
  # mutate(time = Week - min(Week) + 1) %>%
  # filter(deaths <= cases) %>%
  mutate(deaths = ifelse(deaths > cases, NA, deaths)) %>%
  mutate(death_rate = deaths / pop * 100000) %>%
  drop_na()

# Add column for Bundesland:
dat_inc <- dat_inc %>%
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

# Get Landkreise as factor:
dat_inc <- dat_inc %>%
  mutate(ARS = factor(lk))

# #Plot:
# p1 <- ggplot(data = dat_inc, aes(x = Week, y = case_rate, group = lk)) +
#   geom_line(alpha = 0.2) + theme_classic()
# p2 <- ggplot(data = dat_inc, aes(x = Week, y = death_rate, group = lk)) +
#   geom_line(alpha = 0.2) + theme_classic()
# grid.arrange(p1, p2, ncol = 1)
