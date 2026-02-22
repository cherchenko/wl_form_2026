# Перемещение российских журналов, отправленных на экспертизу РАН, на соответствующую стадию канбан доски в Битрикс
tasks <- get_BX_tasks_from_group(58)


tasks <- tasks |>
  filter(!grepl("679|889", stageId )) |>
  mutate(
    type = case_when(
      grepl("(повышение уровня)", title) ~ "level_up",
      grepl("(включение в ЕГПНИ)", title) ~ "include",
      grepl("(этические нарушения)", title) ~ "exclude",
      TRUE ~ NA_character_
    )
  ) |>
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
  mutate(issn = issns) |>
  issn_unnester("issn", sep = "\\| ") |>
  distinct() |>
  mutate(publisher = str_extract(description, enc2utf8('^[\\-]+[^\\\r^\\\n]+[\\\r\\\n\\-]+[^\\\r^\\\n]+'))) |>   
  mutate(publisher = str_remove(publisher, enc2utf8('^[\\-]+[^\\\r^\\\n]+[\\\r\\\n\\-]+'))) |> 
  select(id, stageId, journal_id, title, issns, issn, publisher, title, description, type)

# Списки журналов, отправленные в РАН (распаковать в dir_data архив с отправленными в РАН файлами)
RAN <- list.files(dir_data, pattern = "all_lists", full.names = T) |> 
  lapply(readxl::read_xlsx, col_types = "text", skip = 1) |>
  bind_rows()

RAN <- RAN |>
  select(title=3) |>
  mutate(
    issn = title |>
      str_extract("ISSN=\\([^)]+\\)") |>
      str_replace_all("ISSN=\\(|\\)", "") |>
      str_replace_all("(\\d{4})(\\d{3}[0-9X])", "\\1-\\2")
  ) |>
  issn_unnester("issn", sep = "\\,") |>
  distinct()

# RAN_file <- paste0(dir_clean, "RAN.xlsx")
# 
# writexl::write_xlsx(RAN, RAN_file)

# Отметка в датафрейме журналов из задач, отправлены ли журналы в РАН

tasks_wissns <- tasks_wissns |>
  mutate(in_RAN = ifelse(issn %in% RAN$issn, "Y", NA_character_)) |>
  select(-issn) |>
  distinct()

# Выбор задач, в которых несколько журналов
tasks_journals <- tasks_wissns |>
  group_by(id) |>
  filter(n() > 1) |>
  ungroup()


unique_ids <- unique(tasks_journals$id)

t <- tasks |> filter(id %in% unique_ids) |>
  # отбор только тех, где на самом деле несколько журналов, а не переводные версии или переименования
  filter(grepl("Журналы", title))

unique_ids <- unique(t$id)

# Собираем комментарии для задач с несколькими журналами

tasks_comments <- get_BX_comments(unique_ids)

# Создаем подзадачи с отдельными журналами

# Добавляем к ним комментарии

# Добавляем новые задачи к общему списку задач и убираем из него задачи с несколькими журналами

# Перемещение задач с несколькими журналами на стадию архив = 679

task_change_journals_res <- tasks_journals$id |>   filter(id == "10805") |>
  map_df(~change_BX_task_stage(task_id = .x, stage_id = 679))

# Перемещенеи всех журналов из обращений и датафрейма RAN на стадию "Направлен в РАН"

tasks_RAN <- tasks_wissns |>
  filter(!is.na(in_RAN)) |>
  select(id) |>
  distinct()
  
task_change_res <- tasks_RAN$id |>
  map_df(~change_BX_task_stage(task_id = .x, stage_id = 798))
