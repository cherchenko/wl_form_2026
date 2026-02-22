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
         select = list('TITLE', 'DESCRIPTION', 'STATUS', 'STAGE_ID', 
                       'CREATED_BY', 'RESPONSIBLE_ID', 'ACCOMPLICES', 'TAGS',
                       'GROUP_ID'))
  }
  res <- list()
  
  resp <- BX_query(url = xurl, json_query = xjson(0))
  res[[1]] <- resp$result$tasks
  total <- resp$total
  cycles_left <- ceiling(total/50) - 1
  if(cycles_left>0){
    for (k in 1:cycles_left){
      resp <- BX_query(url = xurl, xjson(50*k))
      res[[k+1]] <- resp$result$tasks
      print(k)
    }
  }
  
  # Преобразуем список задач в датафрейм
  result <- bind_rows(lapply(unlist(res, recursive = FALSE), function(task) {
    as.data.frame(lapply(task, function(x) {
      if(is.null(x)) {
        NA
      } else if(is.list(x)) {
        paste(unlist(x), collapse = ",")
      } else {
        x
      }
    }), stringsAsFactors = FALSE)
  }))
  
  return(result)
}
# use 
# tasks <- get_BX_tasks_from_group(58)


BX_stages_egpni <- function(){
  c("Получен запрос в РЦНИ = 673",
    "Взят в работу = 678", 
    "В таблице на экспертизу = 677",
    "Направлен в РАН = 798",
    "Получено решение = 686",
    "Голосование МРГ = 676",
    "Повторное = 687",
    "отказ = 688",
    "изменения в БС = 693",
    "архив = 679",
    "иное = 889") |> cat(sep = "\n")
}
#use 
# bx_stages_egpni()

change_BX_task_stage <- function(task_id, stage_id){
  resp <- httr2::request(
    paste0('https://bitrix.rcsi.science/rest/', bitrix, '/tasks.task.update')) |>
    httr2::req_body_json(
      list(taskId = task_id, 
           fields = list(STAGE_ID = stage_id)
           )
      ) |>
    httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:131.0) Gecko/20100101 Firefox/131.0") |>
    httr2::req_timeout(60) |> 
    httr2::req_options(ssl_verifypeer = 0L, ssl_verifyhost = 0L) |> 
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform() |> 
    httr2::resp_body_json()
  if(exists("error", resp)){
    cat(paste0("task_id = ", task_id, ": ERROR code =  ", resp$error, "\n"))
    return(list(id = task_id, stage = stage_id, statue = paste0("ERROR code =  ", resp$error)))
  } else {
    if(resp$result$task$stageId == as.character(stage_id)){
      cat(paste0("task_id = ", task_id, ": ok\n"))
      return(list(id = task_id, stage = stage_id, statue = "ok"))
    } else {
      cat(paste0("task_id = ", task_id, ": something WRONG!!! check the values!!!\n"))
      return(list(id = task_id, stage = stage_id, statue = "Wrong status - check the values"))
    }
  }
}

# use
# tasks <- c(12908, 10838, 12801)
# task_change_res <- tasks |> map_df(~change_BX_task_stage(task_id = .x, stage_id = 677))

# task.commentitem.getlist Получает список комментариев к задаче (+ со словами про отправленный ответ)

# Вспомогательная функция для получения комментариев одной задачи
get_task_comments <- function(task_id, batch_size = 50) {
  xurl <- paste0('https://bitrix.rcsi.science/rest/', bitrix, '/task.commentitem.getlist')
  
  comments_list <- list()
  start <- 0
  
  repeat {
    xjson <- list(
      TASKID = task_id,
      start = start
    )
    
    resp <- BX_query(url = xurl, json_query = xjson)
    
    if (length(resp$result) == 0) break
    
    if (length(resp$result) > 0) {
      batch_comments <- resp$result |> 
        purrr::map_df(~{
          data.frame(
            comment_id = .x$ID,
            author_id = .x$AUTHOR_ID,
            comment_text = .x$POST_MESSAGE,
            created_date = .x$POST_DATE,
            stringsAsFactors = FALSE
          )
        })
      comments_list[[length(comments_list) + 1]] <- batch_comments
    }
    
    if (length(resp$result) < batch_size) break
    
    start <- start + batch_size
    Sys.sleep(0.3)
  }
  
  if (length(comments_list) > 0) {
    return(dplyr::bind_rows(comments_list))
  } else {
    return(NULL)
  }
}

get_BX_comments <- function(task_ids, batch_size = 50) {
  
  result <- data.frame(
    task_id = character(),
    all_comments = character(),
    comment_count = integer(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(task_ids)) {
    task_id <- task_ids[i]
    cat(sprintf("Processing task %d of %d: ID = %s\n", i, length(task_ids), task_id))
    
    task_comments <- tryCatch({
      get_task_comments(task_id, batch_size)
    }, error = function(e) {
      cat(sprintf("  Error for task %s: %s\n", task_id, e$message))
      return(NULL)
    })
    
    if (!is.null(task_comments) && nrow(task_comments) > 0) {
      # Очищаем текст и фильтруем
      task_comments <- task_comments |>
        dplyr::mutate(
          clean_text = gsub("<[^>]+>", " ", comment_text),
          clean_text = gsub("\\s+", " ", clean_text),
          clean_text = trimws(clean_text),
          clean_text_lower = tolower(clean_text),
          
          # Ищем фразы про отправку ответа
          is_needed = grepl("отправлен\\s+ответ", clean_text_lower, perl = TRUE) |
            grepl("ответ\\s+отправлен", clean_text_lower, perl = TRUE) |
            grepl("ответ\\s+направлен", clean_text_lower, perl = TRUE)
        )
      
      # Отбираем нужные комментарии
      needed_comments <- task_comments |> 
        dplyr::filter(is_needed) |> 
        dplyr::arrange(created_date)
      
      if (nrow(needed_comments) > 0) {
        # Берем ТЕКСТ ИЗ ИСХОДНОГО ПОЛЯ comment_text, а не clean_text
        all_comments_text <- paste(needed_comments$comment_text, collapse = "\n---\n")
        comment_count <- nrow(needed_comments)
        
        result <- rbind(
          result,
          data.frame(
            task_id = task_id,
            all_comments = all_comments_text,
            comment_count = comment_count,
            stringsAsFactors = FALSE
          )
        )
      } else {
        # Если нужных комментариев нет
        result <- rbind(
          result,
          data.frame(
            task_id = task_id,
            all_comments = "",
            comment_count = 0,
            stringsAsFactors = FALSE
          )
        )
      }
    } else {
      # Если комментариев вообще нет
      result <- rbind(
        result,
        data.frame(
          task_id = task_id,
          all_comments = "",
          comment_count = 0,
          stringsAsFactors = FALSE
        )
      )
    }
    
    Sys.sleep(0.3)
  }
  
  return(result)
}
# Пример использования:
# tasks_comments <- get_BX_comments(tasks_journals$id)