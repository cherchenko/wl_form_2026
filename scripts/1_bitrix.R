#------------ bitrix ----------------------

bitrix <- Sys.getenv("BITRIX")
BX_query <- function(url, json_query){
  resp <- httr2::request(url) |>
    httr2::req_body_json(json_query) |>
    httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:131.0) Gecko/20100101 Firefox/131.0") |>
    httr2::req_options(ssl_verifypeer = 0L, ssl_verifyhost = 0L) |> 
    httr2::req_timeout(60) |> 
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform() |> 
    httr2::resp_body_json()
  return(resp)
}

get_BX_tasks_from_group <- function(xgroup_id){
  xurl <- paste0('https://bitrix.rcsi.science/rest/', bitrix, '/tasks.task.list.json')
  xjson <- function(n){
    list(start = n, 
         filter = list(GROUP_ID = xgroup_id),
         select = list('TITLE', 'DESCRIPTION', 'STATUS', 'STAGE_ID', 'CLOSED_DATE'))
  }
  res <- list()
  
  resp <- BX_query(url = xurl, json_query = xjson(0))
  res[[1]] <- resp$result$tasks |> map_df(~.x |> map_df(~.x))
  total <- resp$total
  cycles_left <- ceiling(total/50) - 1
  if(cycles_left>0){
    for (k in 1:cycles_left){
      resp <- BX_query(url = xurl, xjson(50*k))
      res[[k+1]] <- resp$result$tasks |> map_df(~.x |> map_df(~.x))
      print(k)
    }
  }
  return(res)
}

tasks <- get_BX_tasks_from_group(58) |> map_df(~.x)
tasks <- tasks |>
  filter(stageId != "679") |>
  mutate(
    type = case_when(
      grepl("(повышение уровня)", title) ~ "level_up",
      grepl("(включение в ЕГПНИ)", title) ~ "include",
      grepl("(исключение из ЕГПНИ)", title) ~ "exclude",
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
