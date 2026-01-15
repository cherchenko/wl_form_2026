# Получаем список всех JSON файлов из Метафоры
json_files <- list.files(dir_u, pattern = "\\.json$", full.names = TRUE)

tasks_wissns <- tasks_wissns |>
  mutate(issn = issns) |>
  issn_unnester("issn", sep = "\\| ") |>
  distinct()

json_files <- json_files[gsub(".*/|\\..*", "", json_files) %in% tasks_wissns$issn]

# Создаем пустой список для результатов
all_results <- list()

# Обрабатываем каждый файл с проверкой ошибок
for (i in seq_along(json_files)) {
  file_path <- json_files[i]
  cat(sprintf("Обработка файла %d из %d: %s\n", i, length(json_files), basename(file_path)))
  
  tryCatch({
    # Проверяем, что файл существует и не пустой
    if (!file.exists(file_path)) {
      warning(sprintf("Файл не существует: %s", file_path))
      next
    }
    
    if (file.size(file_path) == 0) {
      warning(sprintf("Файл пустой: %s", file_path))
      next
    }
    
    # Читаем файл
    data <- fromJSON(file_path, simplifyVector = FALSE)
    
    # Извлекаем нужные данные
    
    # 1. elib_id - извлекаем цифры после id= из elibrary
    elib_id <- NA
    if (!is.null(data$elibrary) && length(data$elibrary) > 0) {
      # Берем первую ссылку
      elib_url <- data$elibrary[[1]]
      # Извлекаем цифры после id=
      elib_id <- gsub(".*id=([0-9]+).*", "\\1", elib_url)
      # Если не извлеклось, оставляем NA
      if (elib_id == elib_url) elib_id <- NA
    }
    
    # 2. Получаем ISSN (Print и Online)
    issn_print <- NA
    issn_online <- NA
    
    if (!is.null(data$issns) && length(data$issns) > 0) {
      for (issn_item in data$issns) {
        if (!is.null(issn_item$type) && !is.null(issn_item$issn)) {
          if (issn_item$type == "Print") {
            issn_print <- issn_item$issn
          } else if (issn_item$type == "Online") {
            issn_online <- issn_item$issn
          }
        }
      }
    }
    
    # 3. URL из url_editors (берем первый)
    url <- NA
    if (!is.null(data$url_editors) && length(data$url_editors) > 0) {
      url_item <- data$url_editors[[1]]
      if (!is.null(url_item$url)) {
        url <- url_item$url
      }
    }
    
    # 4. url_mirror (берем первый)
    url_mirror <- NA
    if (!is.null(data$url_mirror) && length(data$url_mirror) > 0) {
      url_mirror <- data$url_mirror[[1]]
    }
    
    # 5. full_title
    full_title <- ifelse(!is.null(data$title_full), data$title_full, NA)
    
    # 6. rkn_number (если есть rkn, берем первый)
    rkn_number <- NA
    if (!is.null(data$rkn) && length(data$rkn) > 0) {
      rkn <- data$rkn[[1]]  # Берем первый элемент
      
      if (!is.null(rkn$rkn_num) && rkn$rkn_num != "") {
        rkn_number <- rkn$rkn_num
        if (!is.null(rkn$rkn_date) && rkn$rkn_date != "") {
          rkn_number <- paste0(rkn_number, " от ", rkn$rkn_date)
        }
      }
    }
    
    # Создаем строку для этого файла
    file_df <- data.frame(
      elib_id = elib_id,
      full_title = full_title,
      issn_print = issn_print,
      issn_online = issn_online,
      url = url,
      url_mirror = url_mirror,
      rkn_number = rkn_number,
      stringsAsFactors = FALSE
    )
    
    all_results[[length(all_results) + 1]] <- file_df
    cat(sprintf("  Успешно обработано\n"))
    
  }, error = function(e) {
    warning(sprintf("Ошибка при обработке файла %s: %s", basename(file_path), e$message))
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
