lapply(some_packages, library, character.only = TRUE)

# Batch 2A ----
## Bring in File and ASCIIFY names ----
# Used Stata to process the TSV initially, imported as UTF-16
# Kept row names but started at Row 159 or so
# Exported as CSV "15Nov_GT_Contact_TSVtoCSV_Readable"
# nov15exportRaw <- read_csv("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/1 Source Files/15Nov_GT_Contact_TSVtoCSV_Readable.csv") |> as_tibble()

dec6exportRaw <- read_csv("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/1 Source Files/6Dec_GT_Contact_TsvToCsv_Readable.csv") |> as_tibble()

# exp0 = nov15exportRaw |> select(2,9,17,18,
#                                 19,
#                                 20,
#                                 21,
#                                 22,
#                                 23,
#                                 24,
#                                 25,
#                                 26,
#                                 27,
#                                 28,
#                                 29,
#                                 30,
#                                 31,
#                                 32,
#                                 33,
#                                 34,
#                                 35,
#                                 36,
#                                 37,
#                                 38,
#                                 39,
#                                 40,
#                                 41,
#                                 42,
#                                 43,
#                                 44,
#                                 45,
#                                 46,
#                                 47, 48, 7) |>
#   rename(svyDate = 1,
#          svyQid = 2,
#          parentLang = 3,
#          stuGrade = 4,
#          parentFirst = 5,
#          parentLast = 6,
#          parentEmail = 7,
#          parentPhone = 8,
#          add1 = 9,
#          add2 = 10,
#          addCity = 11,
#          addState = 12,
#          addZip = 13,
#          stuEnrolled = 14,
#          stuGtAlready = 15,
#          stuFirst = 16,
#          stuMi = 17,
#          stuLast = 18,
#          stuIdSr = 19,
#          stuDob = 20,
#          stuDob2 = 21,
#          stuSchLevel = 22,
#          stuSchIdSr = 23,
#          gradeSr = 24,
#          stuLang_ne = 25,
#          stuFirst_ne = 26,
#          stuMi_ne = 27,
#          stuLast_ne = 28,
#          stuDob_ne = 29,
#          stuDob2_ne = 30,
#          gradeSr_ne = 31,
#          stuSex_ne = 32,
#          stuRace_ne = 33,
#          stuEth_ne = 34, finished = 35)

exp0 = dec6exportRaw |> select(2,
                               7,
                               9,
                               17,
                               18,
                               19,
                               20,
                               21,
                               22,
                               23,
                               24,
                               25,
                               26,
                               27,
                               28,
                               29,
                               30,
                               31,
                               32,
                               33,
                               34,
                               35,
                               36,
                               37,
                               38,
                               39,
                               40,
                               41,
                               42,
                               43,
                               44,
                               45,
                               46,
                               47,
                               48,49) |>
  rename(svyDate = 1,
         finished = 2,
         svyQid = 3,
         parentLang = 4,
         stuGrade = 5,
         parentFirst = 6,
         parentLast = 7,
         parentEmail = 8,
         parentPhone = 9,
         add1 = 10,
         add2 = 11,
         addCity = 12,
         addState = 13,
         addZip = 14,
         stuEnrolled = 15,
         stuGtAlready = 16,
         stuFirst = 17,
         stuMi = 18,
         stuLast = 19,
         stuIdSr = 20,
         stuDob = 21,
         stuDob2 = 22,
         stuSchLevel = 23,
         stuSchIdSr = 24,
         gradeSr = 25,
         stuLang = 26,
         stuLang_ne = 27,
         stuFirst_ne = 28,
         stuMi_ne = 29,
         stuLast_ne = 30,
         stuDob_ne = 31,
         stuDob2_ne = 32,
         gradeSr_ne = 33,
         stuSex_ne = 34,
         stuRace_ne = 35,
         stuEth_ne = 36) |>
  select(-c(stuDob, stuDob_ne))

## Find svyIds with insufficient info ----
table(exp0$stuGrade, exclude = NULL) # 196
table(exp0$stuEnrolled, exclude = NULL) # 1765
  noEmail = exp0 |> filter(is.na(parentEmail) & is.na(parentPhone))
exp1 = anti_join(exp0, noEmail, by = "svyQid") # 4337 obs

## No student info ----
noStu = exp1 |> filter(is.na(stuFirst) & is.na(stuFirst_ne))
exp2 = anti_join(exp1, noStu, by = "svyQid") # down to 3588

## No stu DOB ----
noDob = exp2 |> filter(is.na(stuDob2) & is.na(stuDob2_ne))
# None! Yay!

## No stu LAST ----
noLast = exp2 |> filter(is.na(stuLast) & is.na(stuLast_ne))
# also none hooray!

## Invalid Address ----
invAddress <- read_excel("O:\\Transfer Files\\Data_Output_2023-24\\GT Testing Interest Survey Support\\2 Working Dir\\batch3_NeedsFollowUpThurs.xlsx", sheet = "Invalid Address 27") |> as_tibble()

exp3 = anti_join(exp2, invAddress, by = "svyQid")


## CSV template ----
# write_excel_csv(noStu,
#   paste0(print_wd,"batch3_noStudentName_n749.csv"),
#   na = "",
#   append = FALSE,
#   col_names = TRUE,
#   quote = "needed",
#   escape = "none",  eol = "\r\n")
#
# gisImport2a = read_csv("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/1 Source Files/ML_GT_Testing_Contact_Form_11_15.csv") |> as_tibble()
#   gis0 = gisImport2a |> select(1, 28, 31, 33, 35, 37) |>
#     rename(svyQid = 1, mlOod = 3, zonedEs = 4, zonedMs = 5, zonedHs = 6)

gisImport = read_csv("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/1 Source Files/GT_Testing_Contact_Form_12_6.csv") |> as_tibble()
  gis0 = gisImport |> select(1, 29, 32, 34, 36, 38) |>
    rename(svyQid = 1, mlOod = 3, zonedEs = 4, zonedMs = 5, zonedHs = 6)

# exp1 = full_join(exp0, gis0, by = "svyQid")

exp4 = left_join(exp3, gis0, by = "svyQid")

# Find kids who are not stuEnrolled ==1 and where mlOod is not In District

oodNotEnr = exp4 |> filter((stuEnrolled != 1) & (mlOod != "In District"))
exp5 = anti_join(exp4, oodNotEnr, by = "svyQid")

batch3 = exp5

# ## Identify responses to push to GT for follow-up ----
# exp2 = exp1 |> filter(finished == 0) |> filter(!is.na(parentEmail))
#   # 65 responses were incomplete but had parent email
    # for batch 3 i did this already
# exp3 = exp1 |> filter(mlOod != "In District")
#   # 18 responses where could not be mapped plus 95 OOD
  # for batch 3 i did this already

# export = bind_rows(exp2, exp3)
# CSV template ----
write_excel_csv(batch3,
  paste0(print_wd,"batch3A_toMatch_n3542.csv"),
  na = "",
  append = FALSE,
  col_names = TRUE,
  quote = "needed",
  escape = "none",  eol = "\r\n")



batch2A = anti_join(exp1, export, by = "svyQid") |> filter(!is.na(parentEmail)) |>
  filter(stuGrade != 8) |> # 8	Entering Pre-kindergarten next school year (24-25)
  filter(stuGrade != 1 ) # 1	Entering Kindergarten next school year (24-25)
# colnames(batch2A) at 2033 obs

table(exp4$stuGrade) # all are 2 or 6 or 7

# Sort of incompletes ----
test = anti_join(exp0, gis0, by = "svyQid") # 2140 obs
testA = test |> filter(!is.na(parentEmail)) # 14 rows

# Batch2B ----
## Bring in File and ASCIIFY names ----
nov27exportRaw <- read_csv("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/1 Source Files/27Nov_GT_Contact_TSVtoCSV_Readable.csv") |> as_tibble()
  exp0 = nov27exportRaw |> select(2,
                                  7,
                                  9,
                                  17,
                                  18,
                                  19,
                                  20,
                                  21,
                                  22,
                                  23,
                                  24,
                                  25,
                                  26,
                                  27,
                                  28,
                                  29,
                                  30,
                                  31,
                                  32,
                                  33,
                                  34,
                                  35,
                                  36,
                                  37,
                                  38,
                                  39,
                                  40,
                                  41,
                                  42,
                                  43,
                                  44,
                                  45,
                                  46,
                                  47,
                                  48, 49) |>
    rename(svyDate = 1,
           finished = 2,
           svyQid = 3,
           parentLang = 4,
           stuGrade = 5,
           parentFirst = 6,
           parentLast = 7,
           parentEmail = 8,
           parentPhone = 9,
           add1 = 10,
           add2 = 11,
           addCity = 12,
           addState = 13,
           addZip = 14,
           stuEnrolled = 15,
           stuGtAlready = 16,
           stuFirst = 17,
           stuMi = 18,
           stuLast = 19,
           stuIdSr = 20,
           stuDob = 21,
           stuDob2 = 22,
           stuSchLevel = 23,
           stuSchIdSr = 24,
           gradeSr = 25,
           stuLang = 26,
           stuLang_ne = 27,
           stuFirst_ne = 28,
           stuMi_ne = 29,
           stuLast_ne = 30,
           stuDob_ne = 31,
           stuDob2_ne = 32,
           gradeSr_ne = 33,
           stuSex_ne = 34,
           stuRace_ne = 35, stuEth_ne = 36)

## Bring in GIS fields ----
gisImport2B = read_csv("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/1 Source Files/ML_GT Testing Contact Form_11_27.csv") |> as_tibble()
  gis0 = gisImport2B |> select(1, 29, 32, 34, 36, 38) |>
    rename(svyQid = 1, mlOod = 3, zonedEs = 4, zonedMs = 5, zonedHs = 6)

## Combine the two ----
  exp1 = full_join(exp0, gis0, by = "svyQid") # 2493
# Sort of incompletes ----
  test = anti_join(exp0, gis0, by = "svyQid") # 1100 obs
  testB = test |> filter(!is.na(parentEmail)) ## 49 full responses! all completed today

### Identify responses to push to GT for follow-up ----
  exp2 = exp1 |> filter(finished == 0) |> filter(!is.na(parentEmail)) # 70
  exp3 = exp1 |> filter(mlOod != "In District") |> filter(stuEnrolled == 2) #10
  exp4 = exp1 |> filter(Mayowa_Review == "Invalid Address") #10

temp = bind_rows(exp2, exp3)
export = bind_rows(temp, exp4) |> unique() # from 90 to 89

# write_excel_csv(exp2,
#   paste0(print_wd,"batch2b_GTneeds2FollowUp_n49.csv"),
#   na = "",
#   append = FALSE,
#   col_names = TRUE,
#   quote = "needed",
#   escape = "none",  eol = "\r\n")

# These two I can try to recover from SIS:
# R_2EiZ4771pYpuYwn
# R_1d4TmfqP8u9Qnu4

# batch2B = anti_join(exp1, export, by = "svyQid") # 2404 observations
#
# # Bring in Mayowa's final 64 records as batch2C plus the two I want to try recovering
# temp0_2c <- read_csv("O:/Transfer Files/Data_Output_2023-24/GT Testing Interest Survey Support/1 Source Files/ML_GT Testing Contact Form_11_27__63 Additional Records.csv") |> as_tibble() |>
#   select(-c("svyQid...1", "X", "Y", "Zoned ES", "Zoned MS", "Zoned HS"))
# temp1 = temp0_2c |> rename(mlOod = 15, zonedEs = 16, zonedMs = 17, zonedHs = 18) |>
#   mutate(svyDate = mdy_hms(svyDate))
#
# batch2 = bind_rows(batch2A, batch2B) # 4437 obs
# batch2 = bind_rows(batch2, temp1)

rm(list = ls(pattern = "^exp"))
rm(list = ls(pattern = "^temp"))
rm(list = ls(pattern = "^test"))
rm(list = ls(pattern = "^nov"))
rm(list = ls(pattern = "^gis"))
