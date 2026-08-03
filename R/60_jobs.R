# =============================================================================
# 檔案：R/60_jobs.R
# 用途：協調「分析 -> 組表 -> 寫 Excel」，記錄耗時並隔離單一工作錯誤。
# 此層不實作統計公式，只負責工作生命週期與 Shiny 需要的結果格式。
# 修改入口：
#   - 結果表欄位：job_result_row()。
#   - 工作成功時保存的資訊：run_one_job() 回傳 list。
#   - 批次錯誤策略：run_job_batch() 的 tryCatch()。
# =============================================================================

# 完成單一科目／年級的分析與 11 份 Excel 輸出。
#
# job：工作定義。
# output_root：本次工作階段的輸出根目錄。
# 回傳值：Shiny 結果頁直接使用的穩定 result list。
# 分別記錄分析與 Excel 時間，方便日後找出效能瓶頸。
run_one_job <- function(job, output_root) {
  started_at <- Sys.time()

  # 第一段只包含讀檔、計分、彙總與排名。
  analysis_started_at <- Sys.time()
  analysis <- analyze_job(job)
  analysis_seconds <- as.numeric(
    difftime(
      Sys.time(),
      analysis_started_at,
      units = "secs"
    )
  )

  # 先組一次輸出表，同一批資料同時供 Excel 與頁面預覽使用。
  export_tables <- build_export_tables(analysis)
  export_started_at <- Sys.time()
  exported_files <- export_job_result(
    analysis,
    output_root,
    export_tables
  )
  export_seconds <- as.numeric(
    difftime(
      Sys.time(),
      export_started_at,
      units = "secs"
    )
  )

  list(
    key = job$key,
    label = format_job_label(
      job$year,
      job$subject_code,
      job$grade
    ),
    year = job$year,
    subject_code = job$subject_code,
    subject_name = job$subject_name,
    grade = job$grade,
    status = "完成",
    message = "",
    # 三類互斥，必須維持：
    # student_count = attended_count + absent_count + special_count。
    student_count = analysis$prepared$n_total,
    attended_count = sum(analysis$summaries$valid_index),
    absent_count = sum(analysis$prepared$absent_flag),
    special_count = sum(analysis$summaries$special_index),
    cronbach_alpha = analysis$ctt_analysis$alpha,
    views = build_result_views(export_tables),
    distribution = analysis$distribution,
    exported_files = exported_files,
    analysis = analysis,
    prepared = analysis$prepared,
    ctt_analysis = analysis$ctt_analysis,
    level_ctt_analysis = analysis$level_ctt_analysis,
    analysis_seconds = analysis_seconds,
    export_seconds = export_seconds,
    elapsed_seconds = as.numeric(
      difftime(Sys.time(), started_at, units = "secs")
    )
  )
}

# 將單一工作拋出的錯誤轉成與成功結果相同欄位結構。
#
# 失敗工作不會有預覽、分布或輸出檔，但保留工作識別及單行錯誤訊息。
# started_at 用於顯示該失敗工作實際花費的總時間。
failed_job_result <- function(job, error, started_at) {
  list(
    key = job$key,
    label = format_job_label(
      job$year,
      job$subject_code,
      job$grade
    ),
    year = job$year,
    subject_code = job$subject_code,
    subject_name = job$subject_name,
    grade = job$grade,
    status = "失敗",
    message = compact_error(error),
    student_count = NA_integer_,
    attended_count = NA_integer_,
    absent_count = NA_integer_,
    special_count = NA_integer_,
    cronbach_alpha = NA_real_,
    views = NULL,
    distribution = NULL,
    exported_files = character(),
    analysis_seconds = NA_real_,
    export_seconds = NA_real_,
    elapsed_seconds = as.numeric(
      difftime(Sys.time(), started_at, units = "secs")
    )
  )
}

# 將一個 result list 轉成結果頁工作表的一列。
# 所有耗時顯示到小數點後 2 位；實際 result 仍保留未四捨五入秒數。
job_result_row <- function(result) {
  data.frame(
    工作代號 = result$key,
    科目 = result$subject_name,
    年級 = result$grade,
    狀態 = result$status,
    學生數 = result$student_count,
    到考數 = result$attended_count,
    缺考數 = result$absent_count,
    特殊生 = result$special_count,
    計算秒數 = round(result$analysis_seconds, 2),
    Excel秒數 = round(result$export_seconds, 2),
    總秒數 = round(result$elapsed_seconds, 2),
    訊息 = result$message,
    stringsAsFactors = FALSE
  )
}

# 依序執行一批工作，單一失敗不會中止其他科目或年級。
#
# 回傳值包含完成時間、輸出根目錄、畫面用工作表及具名結果清單。
# 此函式可在 future 背景程序執行，因此不要依賴 Shiny session 物件。
run_job_batch <- function(jobs, output_root) {
  if (length(jobs) == 0L) {
    abort_score("沒有可執行的工作。")
  }

  output_root <- ensure_directory(output_root)
  # 每個 job 各自 tryCatch，確保批次可部分成功。
  results <- lapply(jobs, function(job) {
    started_at <- Sys.time()
    tryCatch(
      run_one_job(job, output_root),
      error = function(error) {
        failed_job_result(job, error, started_at)
      }
    )
  })
  names(results) <- vapply(
    results,
    `[[`,
    character(1),
    "key"
  )

  # results 以工作 key 命名，讓下拉選單能穩定取得指定結果。
  list(
    completed_at = Sys.time(),
    output_root = output_root,
    job_table = do.call(
      rbind,
      lapply(results, job_result_row)
    ),
    results = results
  )
}
