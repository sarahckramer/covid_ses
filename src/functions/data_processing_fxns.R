# ---------------------------------------------------------------------------------------------------------------------
# Functions to process and explore COVID-19 mortality data
# ---------------------------------------------------------------------------------------------------------------------


check_for_missing_dates <- function(dat) {
  # Function to see whether data exist for all possible dates
  # param dat: Data frame or tibble containing data
  # returns: Vector of dates with no data
  
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
  
  return(which_missing)
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
