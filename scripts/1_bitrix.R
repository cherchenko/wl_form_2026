#------------ bitrix ----------------------
tasks <- get_BX_tasks_from_group(58) |> map_df(~.x)
tasks <- tasks |>
  filter(stageId != "679") |>
  mutate(
    type = case_when(
      grepl("(повышение уровня)", title) ~ "level_up",
      grepl("(включение в ЕГПНИ)", title) ~ "include",
      grepl("(этические нарушения)", title) ~ "exclude",
      TRUE ~ NA_character_
    )
  ) |>
  #filter(!grepl("(повышение уровня)", title)) |>
  filter(!grepl("ЗИ", title))

tasks_wissns <- tasks |> 
  mutate(issns = str_replace_all(description, regex('[\\\r\\\n]', multiline = T), "~~")) |> 
  mutate(issns = str_squish(issns)) |> 
  mutate(issns = str_split(issns, '[\\~]+')) |> 
  unnest(issns, keep_empty = T) |> 
  filter(str_detect(issns, '\\d{4}\\-\\d{3}')) |> 
  mutate(issns = str_replace_all(issns, '\\sI\\s', ' | ')) |> 
  mutate(title = str_extract(issns, '[^[\\\r\\\n\\|]]+')) |> 
  mutate(title = str_remove_all(title, '^[\\d\\.\\s]+')) |> 
  filter(str_length(title)<200) |> 
  mutate(issns = sapply(str_extract_all(issns, '\\d{4}\\-\\d{3}.'), function(x) paste(unique(x), collapse = " | "))) |>
  mutate(.by = id, journal_id = row_number()) |> 
  mutate(journal_id = paste0(id, ':', journal_id)) |> 
  mutate(publisher = str_extract(description, enc2utf8('^[\\-]+[^\\\r^\\\n]+[\\\r\\\n\\-]+[^\\\r^\\\n]+'))) |>   
  mutate(publisher = str_remove(publisher, enc2utf8('^[\\-]+[^\\\r^\\\n]+[\\\r\\\n\\-]+'))) |> 
  select(id, stageId, journal_id, title, issns, publisher, title, description, type)

t_file <- paste0(dir_clean, "t.xlsx")

writexl::write_xlsx(tasks_wissns, t_file)
