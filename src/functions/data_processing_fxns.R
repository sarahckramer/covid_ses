# ---------------------------------------------------------------------------------------------------------------------
# Functions to process and explore COVID-19 mortality data
# ---------------------------------------------------------------------------------------------------------------------


check_for_missing_dates <- function(dat, dat_source) {
  # Function to see whether data exist for all possible dates
  # param dat: Data frame or tibble containing data
  # param dat_source: String specifying the source of the raw data
  # returns: Vector of dates with no data
  
  if (dat_source == 'crowdsource') {
    
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
    
  } else if (dat_source == 'cdp') {
    
    all_dates <- dat %>%
      select(starts_with('d20')) %>%
      pivot_longer(cols = everything(), names_to = 'date') %>%
      mutate(date = as.Date(str_sub(date, 2, 9), format = '%Y%m%d')) %>%
      pull(date) %>%
      unique()
    
    count_missing <- length(min(all_dates):max(all_dates)) - length(all_dates)
    if (count_missing > 0) {
      print('At least one date has no associated data.')
    }
    
    which_missing <- c(min(all_dates):max(all_dates))[which(!(c(min(all_dates):max(all_dates)) %in% all_dates))]
    
  } else {
    
    stop('Unrecognized data source.')
    
  }
  
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


reallocate_preserving_sum <- function(dat_orig, to_add) {
  # Fxn to reallocate cases with no age info and round, preserving the total number of cases
  # Adapted from: https://stackoverflow.com/questions/32544646/round-vector-of-numerics-to-integer-while-preserving-their-sum
  # param dat_orig: Named numeric vector of case counts in each age group
  # param to_add: Number of cases with no age information
  # returns: Named numeric vector of cases, with cases w/o age info distributed proportionally
  
  dat_update <- dat_orig + to_add * (dat_orig / sum(dat_orig))
  dat_out <- floor(dat_update)
  indices <- tail(order(dat_update - dat_out), round(sum(dat_update)) - sum(dat_out))
  dat_out[indices] <- dat_out[indices] + 1
  return(dat_out)
  
}
