# 00 SETUP

# Define vector of packages to load
some_packages <- c('tibble', 'dplyr', 'tidyr', 'readr', 'stringr', 'forcats', 'purrr', 'rmarkdown','utilities', 'lubridate', 'janitor', 'openxlsx', 'writexl', 'readxl')
# install.packages(some_packages)

# Load all packages at once
lapply(some_packages, library, character.only = TRUE)

print_wd = "O:/Transfer Files/Data_Output_2023-24/PIR/WDs between May 1 and Sep 29/"

# Bring in file from Cognos
import <- read_csv("O:/Transfer Files/Data_Output_2023-24/PIR/WDs between May 1 and Sep 29/PWR_stuMobility_dates2.csv")

import2 = import |> rename(SY = 1, stuId = 3, enterDate = 6, wdDate = 7, status = 8, SYgrade = 9) |> as_tibble() |> select(-c(ends_with("Count"), starts_with("Campus"), "status")) |> select(stuId, SY, SYgrade, enterDate, wdDate)

# Bring in Wanda's export ----
wandaFile <- read_excel("O:/Transfer Files/Data_Output_2023-24/PIR/WDs between May 1 and Sep 29/TXEnrollWithdraw_101912__202310231304 (1).xlsx", sheet = "toEdit") |> rename(stuId = 1, stuLast = 2, schId = 3, grade = 4, entryDate = 6, entryComment = 7, exitDate = 8, exitCode = 9, exitComment = 10)

fileSY2223 = wandaFile |> filter(SY == 2223) |> rename(schId.2223 = 3, grade.2223 = 4, entryDate.2223 = 6, entryComment.2223 = 7, exitDate.2223 = 8, exitCode.2223 = 9, exitComment.2223 = 10) |> select(-c("SY"))
fileSY2324 = wandaFile |> filter(SY == 2324) |> rename(schId.2324 = 3, grade.2324 = 4, entryDate.2324 = 6, entryComment.2324 = 7, exitDate.2324 = 8, exitCode.2324 = 9, exitComment.2324 = 10) |> select(-c("SY"))

# create the list of unq IDs ----
unqlist = wandaFile |> select(stuId, stuLast) |> unique() # 222804

# find the anomalies in the SY2223 data ----
temp0 = left_join(unqlist, fileSY2223, by = c("stuId", "stuLast"))
  temp1 = temp0 |> group_by(stuId) |> summarise(moveDates = n_distinct(entryDate.2223)) |>
    filter(moveDates >= 2)
temp0a = temp0 |> filter(! stuId %in% temp1$stuId)
temp0b = temp0 |> filter(stuId %in% temp1$stuId) # 1371 values but 682 unq students

## CSV template ----
# write_excel_csv(temp0b,
#   paste0(print_wd,"duplicatesWithin2223_n682.csv"),
#   na = "NA",
#   append = FALSE,
#   col_names = TRUE,
#   quote = "needed",
#   escape = "none",  eol = "\r\n")
temp0bfixed <- read_excel("O:/Transfer Files/Data_Output_2023-24/PIR/WDs between May 1 and Sep 29/deDuplicateWithin2223_n682.xlsx",
sheet = "deDuplicated")
rm(temp0b)

temp0c = temp0a |> select(stuId, starts_with("entry"), starts_with("exit"))

eoy2223 = rows_append(temp0bfixed, temp0c)
table(eoy2223$exitCode.2223) # 171493 were coded as 'school year end' and were expected back

sye2223 = eoy2223 |> filter(exitCode.2223 == "SYE")
june2223 = eoy2223 |> filter(exitCode.2223 != "SYE") |> filter(exitDate.2223 > as_date("2023-05-31")) # exited after June 1
may2223 = eoy2223 |> filter(! stuId %in% sye2223$stuId) |> filter(! stuId %in% june2223$stuId)
  table(may2223$exitCode.2223) # 34382 w/ds but only 53 exit.codes extrapolated from exit.Comment

### Now do 2324 year to date ----
tempA = fileSY2324 |> group_by(stuId) |> summarise(moveDates = n_distinct(entryDate.2324)) |>
  filter(moveDates >= 2)
  table(tempA$moveDates) # 302 plus 6 (308) have 3 or more entry dates, then 10447 have 2

# tempB file is just fine
tempB = fileSY2324 |> filter(! stuId %in% tempA$stuId) |>
  select(-c("stuLast", "schId.2324", "grade.2324"))

# these need deduplication
tempC = fileSY2324 |> filter(stuId %in% tempA$stuId)  |>
  select(-c("stuLast", "schId.2324", "grade.2324"))
# write_excel_csv(tempA,
#   paste0(print_wd,"unqDuplesWithin2324_n10755.csv"),
#   na = "",
#   append = FALSE,
#   col_names = TRUE,
#   quote = "needed",
#   escape = "none",  eol = "\r\n")
tempCfixed = read_excel("O:/Transfer Files/Data_Output_2023-24/PIR/WDs between May 1 and Sep 29/deDuplicateWithin2324_n10755.xlsx", sheet = "unq10755")
  rm(tempC)
tempCfixed = tempCfixed |> mutate(exitDate.2324 = as_date(exitDate.2324))

# length(unique(fileSY2324$stuId))
# length(unique(tempCfixed$stuId))+ length(unique(tempB$stuId)) # Matches, YAY
eoy2324 = rows_append(tempCfixed, tempB)
length(unique(eoy2324$stuId))

# Build the list wide of SYE-2223 and if they show up in 2324 data----
syeInto2324 = eoy2324 |> filter(stuId %in% sye2223$stuId) |> select(-moveDates)
length(unique(syeInto2324$stuId)) # 171489
table(syeInto2324$exitCode.2324, exclude = NULL)


