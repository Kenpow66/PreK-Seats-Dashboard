# RESHAPE BOY Map for Livecchi / Supe

# Define vector of packages to load
some_packages <- c('tibble', 'dplyr', 'tidyr', 'readr', 'stringr', 'forcats', 'purrr', 'rmarkdown','utilities', 'lubridate', 'janitor', 'openxlsx', 'writexl', 'readxl')
# install.packages(some_packages)

# Load all packages at once
lapply(some_packages, library, character.only = TRUE)

print_wd = "R:/RA_Internal/Accountability & Reporting/NWEA MAP/2023-2024/3. Working Files/LEP Reshape/"

boy0 <- read_excel("C:/Users/lprice5/Downloads/BOY_CampusResults_Wide1.xlsx")
colnames(boy0)

boy1 = boy0 |> select(1, 54, 51, 52, 53, ends_with("3"),ends_with("4"),ends_with("5"), ends_with("6"), ends_with("7"),ends_with("8"))
# boyLong = pivot_longer(boy1, cols = 2:38, names_to = "colName", values_to = "value")

tableLangArts = boy1 |> filter(Subject == "Language Arts")
tableMath = boy1 |> filter(Subject == "Mathematics")
tableSci= boy1 |> filter(Subject == "Science")

## CSV template ----
write_excel_csv(tableSci,
  paste0(print_wd,"sci_53.csv"),
  na = "",
  append = FALSE,
  col_names = TRUE,
  quote = "needed",
  escape = "none",  eol = "\r\n")
