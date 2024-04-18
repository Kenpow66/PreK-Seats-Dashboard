# Define vector of packages to load
some_packages <- c('tibble', 'dplyr', 'tidyr', 'readr', 'stringr', 'forcats', 'purrr', 'rmarkdown','utilities', 'lubridate', 'janitor', 'openxlsx', 'writexl', 'readxl')

# Load packages
lapply(some_packages, library, character.only = TRUE)

# Set Working Dirs ----
print_wd = "O:/Transfer Files/Data_Output_2023-24/PreK Seats Dashboard/2 Working Dir/"
print_del = "O:/Transfer Files/Data_Output_2023-24/PreK Seats Dashboard/3 Deliverables/"

# Bring in PERMANENT FILES ----
## cilCrosswalk.csv for naming conventions ----
cilCrosswalk = read_csv("O:/Transfer Files/Data_Output_2023-24/Chief Hole Leveling Sept/1 Source Files/cilCrosswalk.csv") |> rename(schId3 = 1, schName = 2, sisName = 3) |> select(1:3) |> as_tibble()

## schsWithPK for identifying which campuses No PK ----
schsWoPK <- read_excel("O://Transfer Files//Data_Output_2023-24//PreK Seats Dashboard//LP 2023-2024 PreK Programs by Campus.xlsx", sheet = "schsWithoutPK")

## schsECSE for identifying which campuses MIGHT ECSE and have No ECSE (room cap 11) ----
ecseSchs <- read_excel("O://Transfer Files//Data_Output_2023-24//PreK Seats Dashboard//LP 2023-2024 PreK Programs by Campus.xlsx", sheet = "schsMayHaveECSE")

## schsHeadStart identifying campuses with HS rooms (some classes cap 18) ----
headStartRooms <- read_excel("O://Transfer Files//Data_Output_2023-24//PreK Seats Dashboard//LP 2023-2024 PreK Programs by Campus.xlsx", sheet = "headStarts")

## NES and NES-A schools vs Not ----
nesSchs <- read_excel("O://Transfer Files//Data_Output_2023-24//PreK Seats Dashboard//LP 2023-2024 PreK Programs by Campus.xlsx", sheet = "NESandNESA") |>
  rename(schId3 = 1, schName = 2, nesYN = 3) # NB: this is all 85+

## labels for the map / value ranges ----
# labVals <- read_excel("O://Transfer Files//Data_Output_2023-24//PreK Seats Dashboard//LP 2023-2024 PreK Programs by Campus.xlsx", sheet = "labValues")

## Columns for map ----
mapCols <- read_excel("O://Transfer Files//Data_Output_2023-24//PreK Seats Dashboard//LP 2023-2024 PreK Programs by Campus.xlsx", sheet = "progFlagsLEPv2") |> select(1:8)

## double-verified ECSE teachers ----
# to capped at ELEVEN students of any grade
ecseTORs = read_excel("O:/Transfer Files/Data_Output_2023-24/PreK Seats Dashboard/1 Source Files/Reviewed ECSE Teacher List 9-6-23.xlsx", sheet = "updated ECSE List LP notes") |>
  select(1:8) |> unique() |>
  filter(ECSEyn==1) |>
  mutate(EmplID = replace(EmplID, EmplID == "TEMP20901", 777.20901)) |>
  mutate(EmplID = as.numeric(EmplID)) |>
  rename(emplId6 = EmplID)
  # Remember to recode TEMP20901 into 777.20901 until the vacancy is hired

## courses on MASTER SCHEDULE and CLASS ROSTER to triangulate unq kids not in EE ----
keepCourses = c("0K999GEN", "0P999GEN", "0P101GEN", "0P201GEN", "0P207IMM", "0E999GEN")
# "ADM002PK", "ADM003KG", "ADM0100"

# Bring in TODAYS FILES ----

## From SIS: Master Schedule with N-Counts ----
# Start > System Reports > Enterprise Reporting > Master Catalog with Counts - UAT
# Download ALL as csv

file.copy("C:/Users/lprice5/Downloads/master_catalog_with_counts_2023_1117.csv",
          "O:/Transfer Files/Data_Output_2023-24/PreK Seats Dashboard/1 Source Files/master_catalog_with_counts_2023_1117.csv", overwrite = FALSE, copy.date = TRUE)

mastSched = read_csv("O:/Transfer Files/Data_Output_2023-24/PreK Seats Dashboard/1 Source Files/master_catalog_with_counts_2023_1117.csv") |> as_tibble() |>
  rename(sisName = 1, tchName = 2, emplidOg = 3, sectionID = 9, hisdCourse = 7, stuCount = 14) |>
  select(1:3, 7, 9, 14) |>
  filter(hisdCourse %in% keepCourses)

### Recode TEMP IDs into 777.xxxx ID (replace `TEMP` with `777.`)
mastSched0 = mastSched |>
  mutate(emplid0 = str_to_lower(emplidOg, locale = "en")) |>
  mutate(emplid1 = str_replace_all(emplid0, "temp", "777.")) |>
  mutate(emplId6 = as.numeric(emplid1)) |>
  select(sisName, emplId6, tchName, hisdCourse, sectionID, stuCount)

mastSched1 = inner_join(mastSched0, cilCrosswalk, by = "sisName") |>
  select(schId3, schName, emplId6, tchName, hisdCourse, sectionID, stuCount)

### Test whether anyone is teaching at 2 schools (please god no) ----
test0 = mastSched1 |> group_by(emplId6) |> summarize(nSchools = n_distinct(schId3)) |>
  filter(nSchools>1)

# FOUR teachers are coded at 2 schools on 20 Sep
# empl 46651 Umali, Gladys B | # SIS shows her with 0 at [Dogan] KEEP Seguin
# empl 61032 Bell-Shelley, Dionne V # SIS shows her at 0 at [Elrod] KEEP DAILY
# empl 112530 Winfield, Jasmine C # SIS shows her with 0 at [Dogan] KEEP Bellfort
# empl 134669 Fraley, Bryonna V # SIS shows her with 0 at [Dogan] KEEP Highland Hts
# So: drop if emplid is X and schID3 is y
## Still just those four on 18 Sep and again on 29 Sep

# Hooray! Just two now, on 3 Oct and again 24 Oct
# and just one! on Nov 1, tchr 61032 and again on 17 Nov
# drop46651 = mastSched1 |> filter(emplId6 == 46651 & schId3 == 140)
drop61032 = mastSched1 |> filter(emplId6 == 61032 & schId3 == 148)
# drop112530 = mastSched1 |> filter(emplId6 == 112530 & schId3 == 140)
# drop134669 = mastSched1 |> filter(emplId6 == 134669 & schId3 == 140)
  schedcols = colnames(drop61032)

# I FUCKING FIGURED IT OUT use ANTI JOIN
mastSched2 = anti_join(mastSched1, drop61032, by = schedcols)
# mastSched2 = anti_join(mastSched2, drop46651, by = schedcols)
# mastSched2 = anti_join(mastSched2, drop112530, by = schedcols)
# mastSched2 = anti_join(mastSched2, drop134669, by = schedcols)

rm(list = ls(pattern = "^drop")) ## hell yeah

### Categorize whether each unq teacher is EE only, PK only, KG only, or split  ----
pkCourses = c("0P101GEN", "0P201GEN", "0P207IMM", "0P999GEN")

unqTchList = mastSched2 |> select(1:4) |> unique()
# 1146 teachers -- addtl courses increased from 1121...
# on 29 Sep is 1148
# on 3 OCt is 1151
# on 11 Oct is 1144
# on 13 Oct is 1145 and also on 17 Oct
# on 24 Oct is 1146
# dang on 1 Nov is 1007
# on 17 Nov just 794? is this people cleaning up their schedules?
tchsEE = mastSched2 |> filter(hisdCourse == "0E999GEN") |> select(1:4) |> unique()
tchsPK = mastSched2 |> filter(hisdCourse %in% pkCourses) |> select(1:4) |> unique()
tchsKG = mastSched2 |> filter(hisdCourse == "0K999GEN") |> select(1:4) |> unique()

  cols1 = colnames(tchsKG)

tchsEEonly = anti_join(tchsEE, tchsPK, by = cols1)
## ONE appears on 29 sep, gone on 3 Oct & 6 Oct & 11
# 67410 Ruiz at 291 Gallegos ES
# zero on 1 Nov and on 17 Nov
tchsBoth = inner_join(tchsPK, tchsKG, by = cols1)
  # 103 on 13 Sep, 108 on 18 sep, 111 on 29 sep,
  # 115 on 3 Oct and 6 Oct
  # 118 on 11 Oct
  # 120 on 13 Oct and 17 Oct
  # 123 on 24 Oct
  # 99 on 1 Nov
  # 71 on 17 Nov
tchsPkOnly = anti_join(tchsPK, tchsBoth, by = cols1)
# 594 on 29 sep, 591 on 3 Oct and 6 Oct, 590 on 11th,
# 588 on Fri 13th and 589 on 17 Oct
# 588 on 24 Oct
# 559 on 1 Nov
# 501 on 17 Nov
tchsKgOnly = anti_join(tchsKG, tchsBoth, by = cols1)
# 444 then same, 443 on 29 sep
# 445 on 3 Oct and 6 oct, 557 on 13 oct (!?)
# 436 on 17 oct, 435 on 24 Oct
# 349 on 1 Nov
# 111 on 17 Nov??
# export for Laurie
# laurie0 = tchsPkOnly |> mutate(tchCategory = "PK Only")
# laurie1 = tchsBoth |> mutate(tchCategory = "PK and KG")
# laurie2 = tchsKgOnly |> mutate(tchCategory = "KG not PK")
#
# temp0 = bind_rows(laurie0, laurie1)
# laurieFinal = bind_rows(temp0, laurie2)
#   length(unique(laurieFinal$emplId6))

## TEST why are these different by one ??
length(unique(unqTchList$emplId6))

length(unique(tchsEEonly$emplId6)) + length(unique(tchsBoth$emplId6)) + length(unique(tchsPkOnly$emplId6)) + length(unique(tchsKgOnly$emplId6)) # yay equal

### Drop the MASTER SCHEDULE rows for those teachers who are KG only ----
mastSched3 = mastSched2 |> filter(!emplId6 %in% tchsKgOnly$emplId6)
  length(unique(mastSched3$emplId6))
# 702 on 20 sep, 700 on 26 sep
# 705 on 29 sep $ 706 on 6 oct and 708 on 11th and on 13th,
# 709 on 17th, 711 on 24th
# 658 on 1 Nov

# # Troubleshoot/FIX: ----
length(unique(mastSched3$schId3))
# Both campus 251 TWAIN and 475 ELMORE ... also COMTY SERVICES 013 has entered the chat
# mapcols is 165 so we are fine

# Build a unique list of teachers and implement logic for room sizes ----
torBySection = mastSched3
  rm(list = ls(pattern = "^mastSched")) ## hell yeah

## unique TORs ----
unqPkTchs0 = torBySection |> select(1:4) |> unique()
  # 702 on 18 sep, 700 on 20 sep, 705 on 29 sep, 706 on 3 Oct and 6 oct
  # 711 on 24 oct
  # 658 on 1 Nov (!)
  # 572 on 17 Nov (!)

## Flag split-teachers ----
unqPkTchs1 = unqPkTchs0 |>
  mutate(roomType =
  case_when(emplId6 %in% tchsPkOnly$emplId6 == TRUE ~ "PK Only",
    emplId6 %in% tchsBoth$emplId6 == TRUE ~ "PK KG Split",
    TRUE ~ NA))
## It worked!!
rm(list = ls(pattern = "^tchs"))

## Flag ECSE teachers on the ECSE list from SPED via ECE
unqPkTchs2 = unqPkTchs1 |>mutate(ecseTchr =
  case_when(emplId6 %in% ecseTORs$emplId6 == TRUE ~ 1, TRUE ~ 0))

# How many TORs in each category are at each school? ----
temp0 = unqPkTchs2 |> group_by(schId3, schName) |>
  summarise(pkTchsCount = n_distinct(emplId6)) |> ungroup()

# Are they NES/A ? TF ----
# less relevant after 1 Sep
temp1 = left_join(temp0, nesSchs, by = c("schId3", "schName"))
temp1 = mutate_all(temp1, ~replace_na(.,0))

## ECSE ----
ecsePerSch = unqPkTchs2 |> filter(ecseTchr == 1) |>  group_by(schId3, schName) |> summarise(ecseTchsCount = n_distinct(emplId6)) |> ungroup()
temp2 = left_join(temp1, ecsePerSch, by = c("schId3", "schName"))

## Splits ----
splitsPerSch = unqPkTchs2 |> filter(roomType == "PK KG Split") |>  group_by(schId3, schName) |> summarise(splitTchsCount = n_distinct(emplId6)) |> ungroup()

temp3 = left_join(temp2, splitsPerSch, by = c("schId3", "schName"))

## Head Start ----
# Are there any non-HS rooms at the HS campuses? Yes ----
hsPerSch = headStartRooms

temp4 = left_join(temp3, hsPerSch, by = c("schId3", "schName"))
temp4b = replace(temp4, is.na(temp4),0) # yaaas it replaced *all*

# colnames(temp4)

## Regular Degular rooms ----
temp5 = temp4b |>
  select(schId3, nesYN, pkTchsCount, ecseTchsCount, splitTchsCount, HS_rooms) |>
  rename(hsTchsCount = 6) |>
  rowwise() |>
  mutate(regdeg = pkTchsCount - sum(ecseTchsCount + splitTchsCount + hsTchsCount)) |>
  ungroup()

## negatives mean that a teacher falls into more than 1 category
temp6 = temp5 |> mutate(regdeg = replace(regdeg, regdeg < 0, 0))

## Ok, I *almost* have a campus-level matrix of how many types of teachers and how many seats each ought to have

temp6a = left_join(temp6, cilCrosswalk, by = c("schId3")) |> select(-sisName) |>
  select(schId3, schName, pkTchsCount, regdeg, ecseTchsCount, splitTchsCount, hsTchsCount, nesYN)

## CSV template ----
write_excel_csv(temp6a,
  paste0(print_wd,"2023_1117_campusPkTorMatrix.csv"),
  na = "",
  append = FALSE,
  col_names = TRUE,
  quote = "needed",
  escape = "none",  eol = "\r\n")
