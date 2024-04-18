# Today's Day-end list of MASTER SCHEDULE ----
# SIS > Start Page > Reports > Enterprise > `Master Catalog with Counts - UAT`

# Define vector of packages to load
some_packages <- c('tibble', 'dplyr', 'tidyr', 'readr', 'stringr', 'forcats', 'purrr', 'rmarkdown','utilities', 'lubridate', 'janitor', 'openxlsx', 'writexl', 'readxl')

# Load packages
lapply(some_packages, library, character.only = TRUE)

# Set Working Dirs ----
print_wd = "O:/Transfer Files/Data_Output_2023-24/PreK Seats Dashboard/2 Working Dir/"
print_del = "O:/Transfer Files/Data_Output_2023-24/PreK Seats Dashboard/3 Deliverables/"

# Inherit from 0a_ the unique list of teachers ----
torBySection = mastSched3

# Inherit temp6a
colnames(temp6a)

# Reorganize and revamp the Known Fixes (n = 33)
matrixA = temp6a |> select(schId3, schName, pkTchsCount, regdeg, ecseTchsCount, splitTchsCount, hsTchsCount, nesYN) |> rename(mastSchedPkTchrCount = 3)
  rm(list = ls(pattern = "^temp")) ## hell yeah

# Pull in the schools that have been identified for review
toCheck0929 <- read_excel("O:/Transfer Files/Data_Output_2023-24/PreK Seats Dashboard/3 Deliverables/Seats Matrix Ongoing troubleshooting.xlsx", sheet = "checks_n35") |> select(1:2,9:10)
  table(toCheck0929$cleanCategory)

# Who are the teachers who are on the MASTER SCHEDULE at these schools?
temp0 = torBySection |> filter(schId3 %in% toCheck0929$schId3)
  temp1 = temp0 |> group_by(schId3) |> summarise(torCount = n_distinct(emplId6))

  ## CSV exports ----
  write_excel_csv(temp1,
                  paste0(print_wd,"2023_0929_TorCountsForInquiry.csv"),
                  na = "NA",
                  append = FALSE,
                  col_names = TRUE,
                  quote = "needed",
                  escape = "none",  eol = "\r\n")

  write_excel_csv(temp0,
                  paste0(print_wd,"2023_0929_TorNamesForInquiry.csv"),
                  na = "NA",
                  append = FALSE,
                  col_names = TRUE,
                  quote = "needed",
                  escape = "none",  eol = "\r\n")
