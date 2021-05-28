# ---------------------------------------------------------------------------------------------------------------------
# Download hospitalization data from list of urls
# ---------------------------------------------------------------------------------------------------------------------

# Load tidyverse
library(tidyverse)

# ---------------------------------------------------------------------------------------------------------------------

# Ensure that relevant directories exist
if (!dir.exists('data/')) {
  dir.create('data/')
}
if (!dir.exists('data/raw/')) {
  dir.create('data/raw/')
}
if (!dir.exists('data/raw/hospital/')) {
  dir.create('data/raw/hospital/')
}

# ---------------------------------------------------------------------------------------------------------------------

# Function to (attempt to) download files
attempt_file_download <- function(url_temp) {
  # Attempts to download file located at the provided url, and prints feedback on progress
  # param url_temp: A URL where a desired file should be located
  
  file_date <- str_sub(url_temp, 50, 59)
  print(file_date)
  
  filename_store <- paste0('data/raw/hospital/hosp_dat_', file_date, '.csv')
  if (file.exists(filename_store)) {
    print('Duplicate detected!')
    filename_store <- paste0('data/raw/hospital/hosp_dat_', file_date, '_DUP.csv')
  }
  
  tryDownload <- try(download.file(url_temp, filename_store, quiet = TRUE))
  if (class(tryDownload) == 'try-error') {
    print('ERROR - File not found')
  } else {
    print('Done.')
  }
  
  print('---------------------------')
}

# ---------------------------------------------------------------------------------------------------------------------

# Read in list of urls
url_list <- read.table('data/raw/hosp_dat_urls.txt')
url_list <- as.list(url_list[, 1])

# ---------------------------------------------------------------------------------------------------------------------

# Check for duplicate dates
dates_included <- c()
for (filename_temp in url_list) {
  dates_included <- c(dates_included, str_sub(filename_temp, 50, 59))
}

print(length(dates_included)) # 319
print(length(unique(dates_included))) # 316
# 3 repeats

dates_included %>%
  table() %>%
  as.data.frame() %>%
  filter(Freq > 1) %>%
  print()
# 2021-01-13; 2021-01-16; 2021-02-05

# ---------------------------------------------------------------------------------------------------------------------

# Iterate over list and save files
lapply(url_list, slowly(attempt_file_download, rate = rate_delay(5)))
# only 3 seem to have thrown errors: 2020-06-03; 2020-09-27; 2020-10-01
# after a quick look, these do seem to be genuinely missing

# ---------------------------------------------------------------------------------------------------------------------

# Clean up
rm(list = ls())
