# ---------------------------------------------------------------------------------------------------------------------
# Deal with any duplicate downloaded hospital data
# ---------------------------------------------------------------------------------------------------------------------

# Get list of dates with duplicate files:
dup_dates <- str_sub(list.files('data/raw/hospital/', pattern = 'DUP'), 10, 19)

# First, check how duplicate files differ:
for (d in dup_dates) {
  file_list <- list.files('data/raw/hospital/', pattern = d, full.names = TRUE)
  dat1 <- read_csv(file_list[1])
  dat2 <- read_csv(file_list[2])
  
  print(all.equal(dat1, dat2))
}
# For first and third dates in dup_dates, dat1 appears to be a pdf; delete those files and rename the others
# For the second date, both files seem to be exactly the same

# Delete non-csv/extra files and rename the others:
for (d in dup_dates) {
  file_list <- list.files('data/raw/hospital/', pattern = d, full.names = TRUE)
  file.remove(file_list[1])
  file.rename(file_list[2], file_list[1])
}

# Clean up:
rm(list = ls())
