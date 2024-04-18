# adapt or borrow script from JNCortez script
# https://github.com/pricele2/lotteryOutcomes/blob/main/00_jnc_Pre-K%20Lottery%20Outcomes_V4.R

# # # Define vector of packages to load
# some_packages <- c('tibble', 'dplyr', 'tidyr', 'readr', 'stringr', 'forcats', 'purrr', 'rmarkdown','utilities', 'lubridate', 'janitor', 'openxlsx', 'writexl', 'readxl')
# # # install.packages(some_packages)
# #
# # # Load all packages at once
# lapply(some_packages, library, character.only = TRUE)
# #
# print_wd = "O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/2 Working Dir/"
# print_del = "O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/3 Data Products/"

# Inherited findInSis from 1a_[...]GtInterestSvy.R
# Check special chars and excess spaces
# Create 'simple' versions of names
colnames(findInSis)

# Build simple names version ----
find0 = findInSis |>
  mutate(stuDob2 = na_if(stuDob2, "02/29/1999")) |>
  mutate(stuDob = mdy(stuDob2)) |>
  mutate(simpleStuFirst = str_replace_all(Stufirst, "[[:punct:]]", "")) |>
    mutate(simpleStuFirst = str_replace_all(simpleStuFirst, "[[:symbol:]]", "")) |>
  mutate(simpleStuLast = str_replace_all(Stulast, "[[:punct:]]", "")) |>
    mutate(simpleStuLast = str_replace_all(simpleStuLast, "[[:symbol:]]", "")) |>
  mutate(simplePgFirst = str_replace_all(Parentfirst, "[[:punct:]]", "")) |>
    mutate(simplePgFirst = str_replace_all(simplePgFirst, "[[:symbol:]]", "")) |>
  mutate(simplePgLast = str_replace_all(Parentlast, "[[:punct:]]", "")) |>
    mutate(simplePgLast = str_replace_all(simplePgLast, "[[:symbol:]]", ""))
# DOB "1999-02-29" was recoded as NA

# Addresses can be added back from here ----
temp1 <- read_csv("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/1 Source Files/ID Matching/Nov7ExportCleaned_toMatchIds.csv")
  temp1 = temp1 |> select(1, 30:34)

# Useful version w address ----
find1 = left_join(find0, temp1, by = "svyQid") |>
    select(-c(ends_with("_ne"), "stuAgeSep1", "stuGradeBand", "stuGtAlready", "svyStatus", ends_with("sName")))
    rm(temp1)

# Match GENDER in SVY DATA ----
## Install genderdata
install.packages("devtools")
  install.packages("gender")
  remotes::install_github("lmullen/genderdata")
# colnames(find1)

library("gender", "genderdata")
find1a = find1 |> mutate(years = year(stuDob)) |>
  filter(!is.na(stuDob)) |>
  filter(years <= 2012)

find1gender = find1a |>
  distinct(simpleStuFirst, years) %>%
  group_by(years) %>%
  do(results = gender(.$simpleStuFirst, years = .$years[1], method = "ssa")) %>%
  do(bind_rows(.$results)) |> as_tibble()

## Glue back on assessed gender ----
find1c = left_join(find1, find1gender, by = c("simpleStuFirst" = "name"), relationship = "many-to-many") |>
  select(-c(starts_with("proportion"), "year_min", "year_max")) |> rename(genderGuess = gender)

find1d = unique(find1c)
find1e = find1d |> filter(NotesInteger != 8)    # Need to find these 160 2nd graders


  table(find1e$genderGuess, exclude = NULL) # 2148 missing

# Ever enrolled students ----
everEnrolled <- read_csv("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/1 Source Files/ID Matching/StuAddresses_everActive_currentYear_inclDOB 2023-11-07.csv") |> as_tibble()

ever0 = everEnrolled |>
  mutate(stuDob = mdy(stuDob)) |>
  mutate(simpleStuFirst = str_replace_all(Stufirst, "[[:punct:]]", "")) |>
    mutate(simpleStuFirst = str_replace_all(simpleStuFirst, "[[:symbol:]]", "")) |>
  mutate(simpleStuLast = str_replace_all(Stulast, "[[:punct:]]", "")) |>
    mutate(simpleStuLast = str_replace_all(simpleStuLast, "[[:symbol:]]", "")) |>
  mutate(simpleStuFirMi = str_replace_all(Stumi, "[[:punct:]]", "")) |>
  mutate(simpleStuFirMi = str_replace_all(simpleStuFirMi, "[[:symbol:]]", ""))

# Let's see what my gates can catch ----
colnames(ever0)
dim(find1e) # 3076

##Gate 1: check first last DOB ----
match1 = inner_join(find1e, ever0, by = c("simpleStuLast", "simpleStuFirst", "stuDob"))
  # 2616 found of 3076 or 85%
df1_b =  anti_join(find1e, ever0, by = c("simpleStuLast", "simpleStuFirst", "stuDob"))
  # 460 remaining or 14.9

## Gate 2: check last DOB zip ----
match2 = inner_join(df1_b, ever0, by = c("simpleStuLast", "stuDob", c("addZip" = "zip5")))
  # Found another 55 up to 2671 or 87%
df2_b =  anti_join(df1_b, ever0, by = c("simpleStuLast", "stuDob", c("addZip" = "zip5")))
  # 407 remaining or 13%

## Gate 3: check first DOB zip ----
match3 = inner_join(df2_b, ever0, by = c("simpleStuFirst", "stuDob", c("addZip" = "zip5")))
  # up to 2671 plus another 186 is 2857 / up to 93%
df3_b =  anti_join(df2_b, ever0, by = c("simpleStuFirst", "stuDob", c("addZip" = "zip5")))
  # down to 222 or 7.2%

## Gate 4: first last zip ----
match4 = inner_join(df3_b, ever0, by = c("simpleStuFirst", "simpleStuLast", c("addZip" = "zip5")))
  # 2857 plus Another 133 is 2990
# Row 136 of `x` matches multiple rows in `y`.
  # df3_b[136, 5:7] # Michael  Anthony Gonzales
# ℹ Row 145035 of `y` matches multiple rows in `x`.
  # ever0[145035, 3:4] # Arush Pokharel
df4_b =  anti_join(df3_b, ever0, by = c("simpleStuFirst", "simpleStuLast", c("addZip" = "zip5")))
df4_b = df4_b |> mutate(GenderGuess = str_to_title(genderGuess))
  # down to 91 or 2.9%

## Gate 5: first last gender ----
match5 = inner_join(df4_b, ever0, by = c("simpleStuFirst", "simpleStuLast", c("GenderGuess" = "sexMf")))
  # another 34, so up to 3024 or 98.2%
df5_b = anti_join(df4_b, ever0, by = c("simpleStuFirst", "simpleStuLast", c("GenderGuess" = "sexMf")))
  # down to 80 somehow, or 2.5%

## Gate 6 : last + gender + dob ----
# Stumi.y from df5_b is equal to simpleStuFirMi
match6 = inner_join(df5_b, ever0, by = c("Stulast", "stuDob"))
  # another 10, so up to 3034 or 98.5%
df6_b = anti_join(df5_b, ever0, by = c("Stulast", "stuDob"))
  # just 71 left, of which 10 do not have DOB

## Export the 3034 matches you've got ----
tempA <- bind_rows(match1, match2)
tempB <- bind_rows(tempA, match3)
tempC <- bind_rows(tempB, match4)
tempD <- bind_rows(tempC, match5)
tempE <- bind_rows(tempD, match6)

write_excel_csv(tempE,
  paste0(print_wd,"sixGates_RecoveredId_n3034.csv"),
  na = "",
  append = FALSE,
  col_names = TRUE,
  quote = "needed",
  escape = "none",  eol = "\r\n")

## Plus the ones I couldn't find: second graders plus names after 6 gates
find2ndGr = find1d |> filter(NotesInteger == 8)
tempF = bind_rows(df6_b, find2ndGr) # 231 obs

write_excel_csv(tempF,
paste0(print_wd,"sixGates_UnrecoveredId_n231.csv"),
na = "",
append = FALSE,
col_names = TRUE,
quote = "needed",
escape = "none",  eol = "\r\n")


## At this point, start messing with PARENT NAMES









# Related Persons of Students Ever Active in SY2324 ----
relPersons0 <- read_csv("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/1 Source Files/ID Matching/RelatedPersons_Contacts_everActive_currentYear_inclDOB 2023-11-07.csv", col_types = cols(`Student Count` = col_skip(),
`School Year` = col_skip(), `Birth Date` = col_date(format = "%Y/%m/%d"),
`Total(Student Count)` = col_skip()), na = c("NA", "N/A", "2024-06-06", ""))

relPersons1 = relPersons0 |> rename(status = 1, entryDate = 2, exitDate = 3, exitNote = 4, exitCode = 5,
    teaStuId = 6, hisdStuId = 7,
    stuLcfm = 8, stuLast = 9, stuFirst = 10,
    grade2324 = 11, sexMf = 12,
    dobDate = 13, dob8 = 14,
    enrCampusId = 15, zoneCampusId = 16,
    add1 = 17, add2 = 18, addCity = 20, addZip = 21,
    rpFirst = 24, rpMi = 25, rpLast = 26,
    rpEmail = 29, rpPhone = 30,
    rpAdd1 = 32, rpAdd2 = 33, rpAddCity = 35, rpAddState = 37, rpAddZip = 36) |>
  select(-c("Building Site Number", "Contact Sequence", "Primary Contact Indicator", "Related Person Full Name", "Relationship Type", "Related Person Address Type", "Related Person Building Site Number" ))
rm(relPersons0)

#combine all matches into single list
all_matches <- bind_rows(match2, ...)




