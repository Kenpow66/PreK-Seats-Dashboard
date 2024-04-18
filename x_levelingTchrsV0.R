

# Bring in File
spaImport <- read_csv("O:/Transfer Files/Data_Output_2023-24/Chief Hole Leveling Sept/1 Source Files/2023_0901_SPA_By-Kid Enrollment Attendance and Class Schedule.csv") |>
  rename(schId3 = 1, gradeStr = 2, gradeSort = 3, stuID7 = 5, setting = 6, enrollDate = 7) |>
  select(-c("Student Count","Total(Student Count)")) |>
  select(-c("Course 19", "Course 20", gradeSort))
# table(spaImport$`Course 18`, exclude=NULL) # actual values
# table(spaImport$`Course 19`, exclude=NULL) # empty
# table(spaImport$`Course 20`, exclude=NULL) # empty

# Pull unique list of kids before you do any wrangling
kidsUnique = spaImport |> select(schId3, stuID7, gradeStr, setting) |> unique()

# Create a long version of the byKidSchedule df ----
## check for NAs
schedLong = spaImport |> select(schId3, stuID7, gradeStr, starts_with("Course ")) |>
  mutate(across(starts_with("Course "), ~na_if(., ""))) |>
  mutate(across(starts_with("Course "), ~na_if(., " ")))

byKidSched1 = pivot_longer(schedLong, cols=starts_with("Course"), names_to = "CoursePlace", values_to="String", values_drop_na = TRUE)
 # test0 = byKidSched1 |> filter(String=NA)
 # test0 = byKidSched1 |> filter(CoursePlace=NA)
 # Both zero, EXCELLENT

## then split String by 4 components ----
byKidSched2 = separate(byKidSched1, String, sep = " [|] ", into = c("prefix", "Teacher#ID", "hisdCourseCode", "section(ID)"))
# test for NAs across the board -- good! All 0
 # test0 = byKidSched2 |> filter(CoursePlace=NA)
 # test0 = byKidSched2 |> filter(prefix=NA)
 # test0 = byKidSched2 |> filter(`Teacher#ID`=NA)
 # test0 = byKidSched2 |> filter(hisdCourseCode=NA)
 # test0 = byKidSched2 |> filter(`section(ID)`=NA)

### Clean up ----
rm(schedLong, byKidSched1)
rm(spaImport)
rm(test0)

## Qual check:  Looking for NAs or dummy vars in the Teacher field ----
byKidSched3 = separate(byKidSched2, `Teacher#ID`, sep="#", into=c("tchrLCFM", "emplid6"))

byKidSched3$emplid6 = as.integer(byKidSched3$emplid6)
  table(is.na(byKidSched3$emplid6)) # all false, somehow

  arrange(byKidSched3, emplid6)
  length(unique(byKidSched3$emplid6)) # 10848 teachers
  length(unique(byKidSched3$hisdCourseCode)) # 2397

# Drop (flag?) the students who meet Instructional Code criteria ----

instrCode0 <- kidsUnique |> as_tibble() |> select(-c(schId3, gradeStr))
  instrCode0$setting = na_if(instrCode0$setting, "N/A") ##2208 as `N/A` and 166101 as <NA>
  ## now 168309 as NA

instrCode1 = instrCode0 |> filter(!is.na(setting)) # 8860 observations
  table(instrCode1$setting)
  instrCode1$setting = as_factor(instrCode1$setting)
  instrCode1$setting = fct_infreq(instrCode1$setting)
 # spedLevels = fct_count(instrCode1$setting) #table of nCounts

spedKeepDrop = read_csv("O:/Transfer Files/Data_Output_2022-23/Panorama for T-TESS/1 Source Files/fromLisa/spedFactors.csv") |> as_tibble() |> select(-c(factor))
  spedKeepDrop$factorname = fct_inorder(spedKeepDrop$factorname)

instrCode2 = left_join(instrCode1, spedKeepDrop, by=c("setting"="factorname"))
  table(instrCode2$decision, exclude=NULL ) #identified 2899 kids to drop
  instrCode3 =  instrCode2 |> filter(decision=="drop")  # These are the 2412 students to drop

  unqKids1 = kidsUnique |>
    mutate(keepTF = TRUE) |>
    mutate(dropReason = NA) |>
    mutate(keepTF =
             case_when(stuID7 %in% instrCode3$stuID7 ~ FALSE, .default = keepTF)) |>
    mutate(dropReason =
             case_when(stuID7 %in% instrCode3$stuID7 ~ "Instr Setting", .default = dropReason))

## Merge these DECISIONS onto byKidSched3
colnames(byKidSched3)

byKidSched4 = left_join(byKidSched3, unqKids1, by = "stuID7") |> select(-c(ends_with(".y"))) |>
  rename(schId3 = schId3.x, gradeStr = gradeStr.x)

byKidSched5 = byKidSched4 |> filter(keepTF != FALSE)  # These are the ones to keep
byKidSched5b = byKidSched4 |> filter(keepTF == FALSE)  # 19290 observations to drop

# Ncount by CAMPUS by GRADE by ... ?

length(unique(byKidSched5$emplid6)) # 10806 teachers
length(unique(byKidSched5b$emplid6)) # 2511 teachers

length(unique(byKidSched5$stuID7)) # 172096 students
length(unique(byKidSched5b$stuID7)) # 2313 students

# At some point we'll have to decide what to do about / with the teachers who have *some* excluded students and some included students

bySch = byKidSched5 |> group_by(schId3, gradeStr) |> summarise(uniqueStudents = n_distinct(stuID7)) |> ungroup()

bySchbyTch = byKidSched5 |> group_by(schId3, emplid6) |> summarise(uniqueStudents = n_distinct(stuID7)) |> ungroup()

## CSV template ----
write_excel_csv(bySchbyTch,
  paste0(print_del,"bySchoolByTchr.csv"),
  na = "NA",
  append = FALSE,
  col_names = TRUE,
  quote = "needed",
  escape = "none",  eol = "\r\n")

