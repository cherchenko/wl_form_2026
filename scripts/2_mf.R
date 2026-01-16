# Получаем список всех JSON файлов из Метафоры
json_files <- list.files(dir_u, pattern = "\\.json$", full.names = TRUE)

tasks_wissns <- tasks_wissns |>
  mutate(issn = issns) |>
  issn_unnester("issn", sep = "\\| ") |>
  distinct()

json_files <- json_files[gsub(".*/|\\..*", "", json_files) %in% tasks_wissns$issn]

# Создаем пустой список для результатов
all_results <- list()

# Обрабатываем каждый файл
for (i in seq_along(json_files)) {
  file_path <- json_files[i]
  cat(sprintf("Файл %d из %d: %s\n", i, length(json_files), basename(file_path)))
  
  tryCatch({
    if (!file.exists(file_path) || file.size(file_path) == 0) next
    
    data <- fromJSON(file_path, simplifyVector = FALSE)
    
    # Собираем данные
    elib_id <- if (!is.null(data$elibrary) && length(data$elibrary) > 0) {
      ids <- na.omit(sapply(data$elibrary, function(x) gsub(".*id=([0-9]+).*", "\\1", x)))
      if (length(ids) > 0) paste(ids, collapse = " | ") else NA_character_
    } else NA_character_
    
    url <- if (!is.null(data$url_editors) && length(data$url_editors) > 0) {
      urls <- na.omit(sapply(data$url_editors, function(x) {
        if (!grepl("elibrary", x$url, ignore.case = TRUE)) x$url else NA_character_
      }))
      if (length(urls) > 0) paste(urls, collapse = " | ") else NA_character_
    } else NA_character_
    
    url_mirror <- if (!is.null(data$url_mirror) && length(data$url_mirror) > 0) {
      mirrors <- na.omit(sapply(data$url_mirror, function(x) {
        if (!grepl("elibrary", x, ignore.case = TRUE)) x else NA_character_
      }))
      if (length(mirrors) > 0) paste(mirrors, collapse = " | ") else NA_character_
    } else NA_character_
    
    full_title <- ifelse(!is.null(data$title_full), data$title_full, NA_character_)
    
    rkn_number <- if (!is.null(data$rkn) && length(data$rkn) > 0) {
      rkns <- na.omit(sapply(data$rkn, function(x) {
        if (!is.null(x$rkn_num) && x$rkn_num != "") {
          if (!is.null(x$rkn_date) && x$rkn_date != "") {
            paste0(x$rkn_num, " от ", x$rkn_date)
          } else {
            x$rkn_num
          }
        } else NA_character_
      }))
      if (length(rkns) > 0) paste(rkns, collapse = " | ") else NA_character_
    } else NA_character_
    
    # ISSN
    print_issns <- character()
    online_issns <- character()
    other_issns <- character()
    
    if (!is.null(data$issns) && length(data$issns) > 0) {
      for (issn_item in data$issns) {
        if (!is.null(issn_item$issn)) {
          if (issn_item$type == "Print") {
            print_issns <- c(print_issns, issn_item$issn)
          } else if (issn_item$type == "Online") {
            online_issns <- c(online_issns, issn_item$issn)
          } else {
            other_issns <- c(other_issns, issn_item$issn)
          }
        }
      }
    }
    
    # Создаем строки
    if (length(print_issns) > 0 && length(online_issns) > 0) {
      for (p in print_issns) {
        for (o in online_issns) {
          file_df <- data.frame(
            elib_id = elib_id,
            full_title = full_title,
            issn_print = p,
            issn_online = o,
            issn_other = if(length(other_issns) > 0) other_issns[1] else NA_character_,
            url = url,
            url_mirror = url_mirror,
            rkn_number = rkn_number,
            stringsAsFactors = FALSE
          )
          all_results[[length(all_results) + 1]] <- file_df
        }
      }
    } else if (length(print_issns) > 0) {
      for (p in print_issns) {
        file_df <- data.frame(
          elib_id = elib_id,
          full_title = full_title,
          issn_print = p,
          issn_online = NA_character_,
          issn_other = if(length(other_issns) > 0) other_issns[1] else NA_character_,
          url = url,
          url_mirror = url_mirror,
          rkn_number = rkn_number,
          stringsAsFactors = FALSE
        )
        all_results[[length(all_results) + 1]] <- file_df
      }
    } else if (length(online_issns) > 0) {
      for (o in online_issns) {
        file_df <- data.frame(
          elib_id = elib_id,
          full_title = full_title,
          issn_print = NA_character_,
          issn_online = o,
          issn_other = if(length(other_issns) > 0) other_issns[1] else NA_character_,
          url = url,
          url_mirror = url_mirror,
          rkn_number = rkn_number,
          stringsAsFactors = FALSE
        )
        all_results[[length(all_results) + 1]] <- file_df
      }
    }
    
    cat("  OK\n")
    
  }, error = function(e) {
    warning(sprintf("Ошибка в %s: %s", basename(file_path), e$message))
  })
}
# Объединяем все результаты
all <- do.call(rbind, all_results) |>
  distinct() |>
  unite(issn, issn_print, issn_online, sep = " | ", remove = FALSE, na.rm = TRUE) |>
  issn_unnester("issn", sep = "\\| ") |>
  distinct()

tasks_wissns_mf <- tasks_wissns |>
  left_join(all) |>
  select(-issn) |>
  distinct()

tmf_file <- paste0(dir_clean, "tmf.xlsx")

writexl::write_xlsx(tasks_wissns_mf, tmf_file)
