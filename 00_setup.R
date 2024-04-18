# 00 SETUP

# Define vector of packages to load
some_packages <- c('tibble', 'dplyr', 'tidyr', 'readr', 'stringr', 'forcats', 'purrr', 'rmarkdown','utilities', 'lubridate', 'janitor', 'openxlsx', 'writexl', 'readxl')
# install.packages(some_packages)

# Load all packages at once
lapply(some_packages, library, character.only = TRUE)

# ## Working Dirs ----
print_wd = "O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/2 Working Dir/"
print_del = "O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/3 Data Products/"

## CSV template ----
# write_excel_csv(slimStudyKids,
#   paste0(print_wd,"slimStudyKids_n16139.csv"),
#   na = "",
#   append = FALSE,
#   col_names = TRUE,
#   quote = "needed",
#   escape = "none",  eol = "\r\n")
