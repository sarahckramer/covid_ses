# ---------------------------------------------------------------------------------------------------------------------
# Functions to process and explore COVID-19 mortality data
# ---------------------------------------------------------------------------------------------------------------------


check_for_missing_dates <- function(dat) {
  # Function to see whether data exist for all possible dates
  # param dat: Data frame or tibble containing data
  # returns: Vector of dates with no data
  
  count_missing <- length(seq(as.Date(format(min(dat$time_iso8601), '%Y-%m-%d')),
                              as.Date(format(max(dat$time_iso8601), '%Y-%m-%d')),
                              by = 1)) -
    dim(dat)[1]
  if (count_missing > 0) {
    print('At least one date has no associated data.')
  }
  
  which_missing <- as.character(seq(as.Date(format(min(dat$time_iso8601), '%Y-%m-%d')),
                                    as.Date(format(max(dat$time_iso8601), '%Y-%m-%d')),
                                    by = 1))[
                                      which(!(as.character(seq(as.Date(format(min(dat$time_iso8601), '%Y-%m-%d')),
                                                               as.Date(format(max(dat$time_iso8601), '%Y-%m-%d')),
                                                               by = 1)) %in%
                                                as.character(unique(format(dat$time_iso8601, '%Y-%m-%d')))))]
  
  return(which_missing)
}


convert_to_incident <- function(dat) {
  # Fxn to take cumulative data and calculate incident data
  # param dat: Data frame or tibble containing cumulative data in long form
  # returns: Tibble containing incident data
  
  # Subtract timepoint i-1 from timepoint i:
  dat_inc <- dat %>%
    select(date, lk, deaths) %>%
    pivot_wider(names_from = lk, values_from = deaths)
  
  for (i in nrow(dat_inc):2) {
    dat_inc[i, 2:ncol(dat_inc)] <- dat_inc[i, 2:ncol(dat_inc)] - dat_inc[i - 1, 2:ncol(dat_inc)]
  }
  
  dat_inc <- dat_inc %>%
    pivot_longer(!date, names_to = 'lk', values_to = 'deaths') %>%
    left_join(dat[, c('date', 'lk', 'pop')],
              by = c('date', 'lk')) %>%
    mutate(Year = format(date, '%Y'),
           Week = format(date, '%V'),
           .after = date) %>%
    mutate(Year = ifelse(Week == 53, '2020', Year)) %>%
    mutate(death_rate = deaths / pop * 100000,
           .after = deaths)
  
  # Check that dimensions are correct:
  expect_equal(dim(dat)[1], dim(dat_inc)[1])
  
  # Return incident data:
  return(dat_inc)
}
