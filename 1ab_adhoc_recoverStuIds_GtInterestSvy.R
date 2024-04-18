# Inherit batch2 from 1aa_QualExportScrub_GtInterestSvy.R
lapply(some_packages, library, character.only = TRUE)
# Print_del and Print_WD are also inherited

# Create 'simple' versions of names for string matching ----
# Check special chars and excess spaces
colnames(batch2)  # 4502 obs by 41 vars
colnames(batch3)

# find = batch2 |>
#   select(-c("stuDob", "stuDob_ne", "Mayowa_Review")) |>
#   rename(Stufirst = 16, Stumi = 17, Stulast = 18, Stumi = 17, Parentfirst = 5, Parentlast = 6, zip5 = 13, stuGradeBand = 4)

find = batch3 |>
  select(-c("Mayowa_Review")) |>
  rename(Stufirst = 17, Stumi = 18, Stulast = 19, Parentfirst = 6, Parentlast = 7, zip5 = 14, stuGradeBand = 5)

find1 = find |>
  mutate(stuDob = mdy(stuDob2), stuDob_ne = mdy(stuDob2_ne)) |>
  select(-c("stuDob2", "stuDob2_ne")) |>
  mutate(simpleStuFirst = str_replace_all(Stufirst, "[[:punct:]]", "")) |>
  mutate(simpleStuFirst = str_replace_all(simpleStuFirst, "[[:symbol:]]", "")) |>
    mutate(simpleStuFirst = str_to_lower(simpleStuFirst, locale = "en")) |>

  mutate(simpleStuLast = str_replace_all(Stulast, "[[:punct:]]", "")) |>
  mutate(simpleStuLast = str_replace_all(simpleStuLast, "[[:symbol:]]", "")) |>
    mutate(simpleStuLast = str_to_lower(simpleStuLast, locale = "en")) |>

  mutate(simplePgFirst = str_replace_all(Parentfirst, "[[:punct:]]", "")) |>
  mutate(simplePgFirst = str_replace_all(simplePgFirst, "[[:symbol:]]", "")) |>
    mutate(simplePgFirst = str_to_lower(simplePgFirst, locale = "en")) |>

  mutate(simplePgLast = str_replace_all(Parentlast, "[[:punct:]]", "")) |>
  mutate(simplePgLast = str_replace_all(simplePgLast, "[[:symbol:]]", "")) |>
    mutate(simplePgLast = str_to_lower(simplePgLast, locale = "en"))

## Find or glue on GENDER guess ----

# Match GENDER in SVY DATA ----
## Install genderdata
# install.packages("devtools")
# install.packages("gender")
# remotes::install_github("lmullen/genderdata")
# colnames(find1)

library("gender", "genderdata")
find1a = find1 |> mutate(years = year(stuDob)) |>
  filter(!is.na(stuDob)) |>
  filter(years <= 2012)

# find1gender = find1a |>
#   distinct(simpleStuFirst, years) %>%
#   group_by(years) %>%
#   do(results = gender(.$simpleStuFirst, years = .$years[1], method = "ssa")) %>%
#   mutate(across(where(is.logical), as.character)) %>%
#   do(bind_rows(.$results)) |> as_tibble()

# try again without the YEARS aspect ----
# find1a2 = find1 |> select(simpleStuFirst) |> unique()
#
# find1gender = find1a2 |>
#   do(results = gender(.$simpleStuFirst, method = "genderize")) %>%
#   mutate(across(where(is.logical), as.character)) %>%
#   do(bind_rows(.$results)) |> as_tibble()
# Too Many Requests (RFC 6585) (HTTP 429) # DAMN limited to 1000 per day

## Glue back on assessed gender ----
find1c = left_join(find1, find1gender, by = c("simpleStuFirst" = "name"), relationship = "many-to-many") |>
  select(-c(starts_with("proportion"), "year_min", "year_max")) |> rename(genderGuess = gender)

find1d = unique(find1c)
# find1e = find1d |> filter(NotesInteger != 8)    # Need to find these 160 2nd graders
  table(find1d$genderGuess, exclude = NULL) # 3460 missing

## ID and Drop records with missing basic student fields ----
testNa = find1d |> filter(is.na(Stufirst)) |> filter(is.na(stuFirst_ne)) ## 1886 records that may need follow-up
  # table(testNa$stuGtAlready, testNa$stuGradeBand, exclude = NULL)
  # -	Of these, 1445 are missing the parent info (incl email address)
  # -	326 said their kid has already been ID’d as GT
  # -	115 parents of 2nd graders need to know how to access their kid’s scores

# write_excel_csv(testNa,
#     paste0(print_wd,"batch2_missingStuData_n1886.csv"),
#     na = "",
#     append = FALSE,
#     col_names = TRUE,
#     quote = "needed",
#     escape = "none",  eol = "\r\n")
find1e = find1d |> filter(! svyQid %in% testNa$svyQid)
  dim(find1e)

## Check for duplicates ----
dupleCheck = find1 |> group_by(simpleStuFirst, simpleStuLast, stuDob, simplePgLast) |>
  summarise(nApps = n_distinct(svyQid)) |> as_tibble()  # 3405 unique
duples = dupleCheck |> filter(nApps > 1) # 125 obs

find1f = anti_join(find1, duples, by = c("simpleStuFirst", "simpleStuLast", "stuDob", "simplePgLast")) # these 3280 are unique

# handle the duplicates
find1g = inner_join(find1, duples, by = c("simpleStuFirst", "simpleStuLast", "stuDob", "simplePgLast"))
# write_excel_csv(find1g,
#     paste0(print_wd,"batch3a_duplicates_n262.csv"),
#     na = "",
#     append = FALSE,
#     col_names = TRUE,
#     quote = "needed",
#     escape = "none",  eol = "\r\n")
dupleFix <- read_excel("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/2 Working Dir/batch3a_duplicates_n262_keepdrop.xlsx", sheet = "drop115") |> as_tibble() |>
  filter(!is.na(svyQid)) # 115 to drop

# dupleFix = read_excel("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/2 Working Dir/batch2_duplicates_n232_keepdrop.xlsx", sheet = "drop106") |> as_tibble()

find1h = anti_join(find1g, dupleFix, by = "svyQid") # 147 to keep

find1j = bind_rows(find1f, find1h)

## Find ever enrolled student file ----
# everEnrolled <- read_excel("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/1 Source Files/ID Matching/StuAddresses_everActive_currentYear_inclDOB 2023-11-21.xlsx",
# col_types = c("numeric", "numeric", "text", "text", "text", "date", "numeric", "numeric", "text", "skip", "skip", "text", "text", "text", "text", "text")) |> as_tibble()

everEnrolled <- read_excel("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/1 Source Files/ID Matching/StuAddresses_everActive_currentYear_inclDOB_07dec2023.xlsx", col_types = c(col_types = c("numeric", "numeric", "text", "text", "text", "text", "date", "skip", "numeric", "numeric", "text", "skip", "skip", "text", "text", "text", "text")))|> as_tibble()

ever0 = everEnrolled |>
  rename(teaId = 1, hisdStuId = 2, status = 4, stuDob = 7, sisEnrSchId = 8, sisZonedSch = 9,
         add1 = 11, addCity = 11, Stufirst = 3) |>
    mutate(simpleStuFirst = str_replace_all(Stufirst, "[[:punct:]]", "")) |>
    mutate(simpleStuFirst = str_replace_all(simpleStuFirst, "[[:symbol:]]", "")) |>
      mutate(simpleStuFirst = str_to_lower(simpleStuFirst, locale = "en")) |>
    mutate(simpleStuLast = str_replace_all(Stulast, "[[:punct:]]", "")) |>
    mutate(simpleStuLast = str_replace_all(simpleStuLast, "[[:symbol:]]", "")) |>
      mutate(simpleStuLast = str_to_lower(simpleStuLast, locale = "en")) |>
    mutate(simpleStuFirMi = str_replace_all(Stumi, "[[:punct:]]", "")) |>
    mutate(simpleStuFirMi = str_replace_all(simpleStuFirMi, "[[:symbol:]]", "")) |>
      mutate(simpleStuFirMi = str_to_lower(simpleStuFirMi, locale = "en")) |>
  unique()

## 214060

# PRIORITIZING THE KINDER and FIRST GRADERS ----
table(find1j$stuGradeBand, find1j$stuEnrolled, exclude = NULL)

# 1	Entering Kindergarten next school year (24-25) # EK process, routed into different form
# 2	Currently enrolled in Kindergarten or 1st grade.  # PAPER tests
# 6	Currently enrolled in 2nd grade.   # Double check they were actually tested
# 7	Currently enrolled in 3rd grade or above. #online tests
# 8	Entering Pre-kindergarten next school year (24-25) # Routed to end of form

# Let's see what my gates can catch ----
dim(ever0)
colnames(ever0)
dim(find1j) # 3427 rows
colnames(find1j)

##Gate 1: check first last DOB ----
match1 = inner_join(find1j, ever0, by = c("simpleStuLast", "simpleStuFirst", "stuDob"))
# ℹ Row 1777 of `x` matches multiple rows in `y`.
# ℹ Row 166589 of `y` matches multiple rows in `x`.

View(find1j[1258,])
# kameron holmes... truly the kid has two local IDs. Keep 2239603 and drop 2142752
  drop = match1 |> filter(hisdStuId == 2142752)
match1a = anti_join(match1, drop, by = "hisdStuId")
# Row 11164 of `y` matches multiple rows in `x`.

View(ever0[11164,]) # Salah, Rayan Abdulrahman
# drop app R_27O8JbtQhO4tgEx and keep the other one in match1a
    droplist = c("R_27O8JbtQhO4tgEx") |> as_tibble()
    drop = match1a |> filter(svyQid %in% droplist$value)
match1b = anti_join(match1a, drop, by = "svyQid")
  df1_b =  anti_join(find1j, match1b, by = "svyQid")
  df1_b = anti_join(df1_b, drop, by = "svyQid") # Dropped 1 more row
# 601 remaining

## Gate 2: check last DOB zip ----
match2 = inner_join(df1_b, ever0, by = c("simpleStuLast", "stuDob", "zip5"))
  # Found another 40 up to 77.6%
  df2_b =  anti_join(df1_b, ever0, by = c("simpleStuLast", "stuDob", "zip5"))
  # down to 561

## Gate 3: check first DOB zip ----
match3 = inner_join(df2_b, ever0, by = c("simpleStuFirst", "stuDob", "zip5"))
# Row 3 of `x` matches multiple rows in `y`.
View(df2_b[3,]) # lopez, jose alberto
# surname is Lopez-Martinez in SIS data
# keep ID 1889389, drop ID 2158896
match3a = match3 |> filter(hisdStuId != 2158896)
# Row 101764 of `y` matches multiple rows in `x`.
  View(ever0[101764,]) # Perez-Sandoval, Henderson
  # keep R_2Sc4Sgj6goXL9nE drop R_D6QFtptdft3Hlfj
  droplist = c("R_D6QFtptdft3Hlfj") |> as_tibble()
  drop = match3a |> filter(svyQid %in% droplist$value)
match3b = anti_join(match3a, drop, by = "svyQid")
df3_b = anti_join(df2_b, ever0, by = c("simpleStuFirst", "stuDob", "zip5"))
dim(df3_b)

## Gate 4: first last zip ----
match4 = inner_join(df3_b, ever0, by = c("simpleStuFirst", "simpleStuLast", "zip5")) # 76
df4_b = anti_join(df3_b, ever0, by = c("simpleStuFirst", "simpleStuLast", "zip5")) # 270

## Gate 5: first last gender ----
df4_b = df4_b |> mutate(genderGuess = str_to_title(genderGuess))
match5 = inner_join(df4_b, ever0, by = c("simpleStuFirst", "simpleStuLast", c("genderGuess" = "Sex Type (Gender)")))
df5_b = anti_join(df4_b, ever0, by = c("simpleStuFirst", "simpleStuLast", c("genderGuess" = "Sex Type (Gender)")))
table(df5_b$stuEnrolled, exclude = NULL)

## Gate 6 : last + gender + dob ----
# Stumi.y from df5_b is equal to simpleStuFirMi
match6 = inner_join(df5_b, ever0, by = c("Stulast", "stuDob"))
df6_b = anti_join(df5_b, ever0, by = c("Stulast", "stuDob"))

## Final: Flag for Not Enrolled, and lookup By Hand
notEnrolled = df6_b |> filter(stuEnrolled == 2)
goLookUp = df6_b |> filter(stuEnrolled != 2) # 62 observations

write_excel_csv(goLookUp,
    paste0(local_wd,"batch2_lookUp_n62.csv"),
    na = "",
    append = FALSE,
    col_names = TRUE,
    quote = "needed",
    escape = "none",  eol = "\r\n")

## Row-bind everything ----
# match1b
# match2
# match3b
# match4
# match5
# match6
# notEnrolled

local_wd = "C:/Users/lprice5/OneDrive/OneDrive - Houston Independent School District/temp_GTbatch2/"

temp0 = bind_rows(match1b, match2)
temp1 = bind_rows(temp0, match3b)
temp2 = bind_rows(temp1, match4)
temp3 = bind_rows(temp2, match5)
temp4 = bind_rows(temp3, match6)
temp5 = bind_rows(temp4, notEnrolled)

write_excel_csv(temp5,
  paste0(local_wd,"batch2_n2452.csv"),
  na = "",
  append = FALSE,
  col_names = TRUE,
  quote = "needed",
  escape = "none",  eol = "\r\n")
