# ---------------------------------------------------------------------------------------------------------------------
# Download hospitalization data from list of urls
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

# Read in list of urls


# ---------------------------------------------------------------------------------------------------------------------

# Iterate over list and save files

# file.no1 <- 7875
# file.date <- as.Date('2021-03-04')
#
# for (i in 1:50) {
#   filename.temp <- paste0('https://edoc.rki.de/bitstream/handle/176904/',
#                           file.no1,
#                           '/',
#                           file.date,
#                           '_12-15_teilbare_divi_daten.csv?sequence=1&isAllowed=y')
#   
#   file.no1 <- file.no1 - 3
#   file.date <- file.date - 1
#   
#   # print(filename.temp)
#   temp.store <- paste0('data_raw/hosp/hosp_dat_', file.date, '.csv')
#   # temp.store <- tempfile()
#   checkExists <- try(download.file(filename.temp, temp.store, quiet = TRUE))
#   if (class(checkExists) == 'try-error') {
#     print(filename.temp)
#   }
#   # print(class(a))
# }
# for 50 most recent dates, only 2 errors

# NOTE: Deal with duplicates (corrections maybe?); ensure all urls actually lead to a csv

# ---------------------------------------------------------------------------------------------------------------------

# Clean up
rm(list = ls())
