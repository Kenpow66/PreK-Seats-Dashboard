# Define vector of packages to load
some_packages <- c('tibble', 'dplyr', 'tidyr', 'readr', 'stringr', 'forcats', 'purrr', 'rmarkdown','utilities', 'lubridate', 'janitor', 'openxlsx', 'writexl', 'readxl')
# install.packages(some_packages)

# Load all packages at once
lapply(some_packages, library, character.only = TRUE)

print_wd = "O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/2 Working Dir/"
print_del = "O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/3 Data Products/"

# Bring in exported file  ----
# Note: I cheated and did a lot of  cleaning before I imported for ID matching

lepToEdit <- read_excel("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/1 Source Files/All GT Records Merged_11_7_23_vLEP.xlsx", sheet = "toEdit", na = c("-99", "NA", "N/A", "n/a", ".", "N.A", "N/", "n/A", "N/A", "N/S", "N:A", "Na", "na", "none", "None"), trim_ws = TRUE) |>  as_tibble()

notesLevels = unique(lepToEdit$`LP Notes`)

lepToEdit2 = read_csv("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/1 Source Files/ID Matching/Nov7ExportCleaned_toMatchIds.csv", col_names = TRUE, na = c("-99", "NA", "N/A", "n/a", ".", "N.A", "N/", "n/A", "N/A", "N/S", "N:A", "Na", "na", "none", "None", ""), trim_ws = TRUE) |> as_tibble()

# Recode as FACTOR ----
# GOOD JOB PAST LAUREN
# Adapted from https://github.com/pricele2/potential-octo-robot/blob/main/0a_processStudentElig.R
lepToEdit3 = lepToEdit2 |> filter(!is.na("LP Notes")) # still 5576 rows
  # table(lepToEdit3$`LP Notes`)
  lepToEdit3$Notes = as_factor(lepToEdit3$`LP Notes`)
  lepToEdit3$Notes = fct_infreq(lepToEdit3$Notes) ## DOUBLE CHECK THIS FOR TRANCHE 2
  nCountLevels = fct_count(lepToEdit3$Notes) #table of nCounts
  # Already tested HISD 2nd grade is factor 8

# Pare down cols ----
toEdit0 = lepToEdit3 |> select(1:3, 45, 5:8, 10, 12:13, 15, 21:24, 25:29, 38:44)
# change notes integer to 7 on two entries:
  # R_ZF40hBm2Oh7J28x
  # R_2YaLxFzryGD5WGY
toEdit1 = toEdit0 |> mutate(NotesInteger = as.integer(Notes))
  toEdit1b = toEdit1 |> mutate(
    NotesInteger =
      case_when(
        svyQid == "R_ZF40hBm2Oh7J28x" ~ 7,
        svyQid == "R_2YaLxFzryGD5WGY" ~ 7,
        TRUE ~ NotesInteger ))
  oodList = c(7) # not enrolled in hisd but can test now
    oodList = as_tibble_col(oodList)

  dropList = c(4, 5, 6, 19)
    dropList = as_tibble_col(dropList)
  gtList = c(11, 12, 13, 14, 16, 18)
    gtList = as_tibble_col(gtList)
  ekList = c(3, 9)
    ekList = as_tibble_col(ekList)
  lookupList = c(10, 15, 17, 1, 2, 8)
    lookupList = as_tibble_col(lookupList)

# drop the identified factor-rows ----
# https://stackoverflow.com/questions/30052042/filter-factor-levels-in-r-using-dplyr

forContact = toEdit1b |> filter(! NotesInteger %in% dropList$value)
# dropped 1099 on the first tranche
# length(unique(toEdit0$svyQid)) - length(unique(forContact$svyQid))

# Export for GT Department ----
forGT = forContact |> filter(NotesInteger %in% gtList$value)
  # write_excel_csv(forGT,
  #   paste0(print_wd,"batch1_GTdept_ToContact_n99.csv"),
  #   na = "",
  #   append = FALSE,
  #   col_names = TRUE,
  #   quote = "needed",
  #   escape = "none",  eol = "\r\n")

# Export for EK process (Carly) ----
forEK = forContact |> filter(NotesInteger %in% ekList$value)
  # write_excel_csv(forEK,
  #                 paste0(print_wd,"batch1_EKprocess_ToContact_n1125.csv"),
  #                 na = "",
  #                 append = FALSE,
  #                 col_names = TRUE,
  #                 quote = "needed",
  #                 escape = "none",  eol = "\r\n")

# Export for campuses even tho they're not currently enrolled
forOodTesting = forContact |> filter(NotesInteger %in% oodList$value) |>
  select(-c("svyStatus", "Notes", "stuEnrolled", "mlOod", "NotesInteger", "stuGradeBand"))

write_excel_csv(forOodTesting,
                paste0(print_wd,"batch1_NotEnrolled_NotEk_n176.csv"),
                na = "",
                append = FALSE,
                col_names = TRUE,
                quote = "needed",
                escape = "none",  eol = "\r\n")

# Try recover their IDs ----
findInSis = forContact |> filter(NotesInteger %in% lookupList$value)
  # write_excel_csv(findInSis,
  #                 paste0(print_wd,"batch1c_FindInSis_ToRecoverId_n3236.csv"),
  #                 na = "",
  #                 append = FALSE,
  #                 col_names = TRUE,
  #                 quote = "needed",
  #                 escape = "none",  eol = "\r\n")

# Clean up environment ----
  rm(list = ls(pattern = "^lepTo"))
  rm(list = ls(pattern = "^for"))
  rm(list = ls(pattern = "^toEdit"))
  rm(list = ls(pattern = "List$"))
rm(nCountLevels)

# Move to 1b_adhoc_recoverStuIds
