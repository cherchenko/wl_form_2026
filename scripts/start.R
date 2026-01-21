Sys.setlocale("LC_CTYPE","russian")
Sys.setlocale("LC_COLLATE","russian")
Sys.setlocale("LC_TIME", "russian")

## silent loading of packages
list("httr", "httr2", "dplyr", "readr", "tidyr", "tibble", "purrr", "stringr", "YDisk4R", "uoir.fs", "rvest", "jsonlite", "digest") |>
  purrr::walk(~suppressWarnings(suppressPackageStartupMessages(
    library(.x, character.only = TRUE, quietly = T))))

#                       ---- setting Data Dirs ----
dir <- getwd()
dir_scripts <- paste0(dir, "/scripts/")
dir_data <- paste0(dir, "/data/")
dir_clean <- paste0(dir, "/clean/")
if(!dir.exists(dir_data)){dir.create(dir_data)}
if(!dir.exists(dir_clean)){dir.create(dir_clean)}
dir_u <- paste0("U:/ISSN-data/mf_jsons")
dir_f <- paste0("U:/ISSN-data/")