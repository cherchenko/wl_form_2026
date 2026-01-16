tasks_wissns_mf <- readxl::read_xlsx(tmf_file) |>
  filter(!is.na(full_title))

f_file <- paste0(dir_f, "params_add_all.xlsx")
form <- readxl::read_xlsx(f_file)

form <- form |>
  mutate(check_2026 = ifelse((!is.na(issn_print) & issn_print %in% tasks_wissns_mf$issn_print) |
                               (!is.na(issn_online) & issn_online %in% tasks_wissns_mf$issn_online),
                             "Y", NA_character_))

# Файл для добавления столбца check_2026 в файл params_add_all.xlsx

form_short <- form |>
  select(elib_id, check_2026)

form_short_file <- paste0(dir_clean, "form_short.xlsx")

writexl::write_xlsx(form_short, form_short_file)

# Для проверки сайтов в уже собранных данных

compare_url <- form |>
  filter(check_2026 == "Y") |>
  left_join(tasks_wissns_mf, by = c("issn_print", "issn_online")) |>
  relocate(contains("full_title"), url.x, url.y)

# Все сайты в compare_url совпадают

# Список журналов для ручной проверки сайтов (добавить строки в файл params_add_all.xlsx)

check <- tasks_wissns_mf |>
  filter(!full_title %in% compare_url$full_title.y)

check_file <- paste0(dir_clean, "check.xlsx")

writexl::write_xlsx(check, check_file)