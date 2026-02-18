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
# use 
# tasks <- get_BX_tasks_from_group(58) |> map_df(~.x)


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


