# =============================================================================
# 檔案：tests/module_test.R
# 用途：以 shiny::testServer() 驗證單科／批次上傳、背景工作及結果頁。
# 執行：在專案根目錄輸入 Rscript tests/module_test.R
# 範圍：測 server reactive 邏輯，不啟動真實瀏覽器。
# =============================================================================

# 確認測試從專案根目錄執行，才能依相對路徑載入 R/。
project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)
if (!file.exists(file.path(project_root, "app.R"))) {
  stop(
    "請在 115-score-shiny 專案根目錄執行測試。",
    call. = FALSE
  )
}

# 依正式 app.R 相同順序載入全部功能檔。
source_files <- sort(list.files(
  file.path(project_root, "R"),
  pattern = "[.]R$",
  full.names = TRUE
))
invisible(lapply(source_files, sys.source, envir = globalenv()))
sys.source(
  file.path(project_root, "tests", "create_fixture.R"),
  envir = globalenv()
)
future::plan(future::multisession, workers = 1L)

# 每次測試使用獨立暫存目錄，結束時無論成功失敗都清除。
test_root <- tempfile("score-shiny-module-")
dir.create(test_root, recursive = TRUE)
on.exit(
  unlink(test_root, recursive = TRUE, force = TRUE),
  add = TRUE
)

fixture <- create_anonymous_fixture(
  file.path(test_root, "fixture")
)

# 把實體檔案包成 Shiny fileInput() 在 server 端會收到的 data.frame 格式。
upload_row <- function(path, type) {
  data.frame(
    name = basename(path),
    size = unname(file.info(path)$size),
    type = type,
    datapath = path,
    stringsAsFactors = FALSE
  )
}

answer_upload <- upload_row(
  fixture$answer_path,
  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)
response_upload <- upload_row(
  fixture$response_path,
  "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)
background_result <- NULL

# ---------------------------------------------------------------------------
# 單科模式：檢查檔案、啟動 ExtendedTask、等待背景計算及檢查輸出。
# ---------------------------------------------------------------------------
shiny::testServer(mod_run_server, {
  session$setInputs(
    mode = "single",
    year = "115",
    subject_code = "C",
    answer_file = answer_upload,
    response_files = response_upload,
    prepare_files = 1
  )
  session$flushReact()

  stopifnot(length(staged_jobs()) == 1L)
  stopifnot(identical(staged_jobs()[[1L]]$key, "115_C4"))
  stopifnot(identical(staged_preview()$狀態, "可執行"))

  session$setInputs(run_analysis = 1)
  session$flushReact()
  # 背景 future 最多等待 30 秒；later::run_now() 讓 promise 可回傳主程序。
  deadline <- Sys.time() + 30
  while (Sys.time() < deadline) {
    if (task$status() %in% c("success", "error")) {
      break
    }
    later::run_now(timeoutSecs = 0.1)
    session$flushReact()
    Sys.sleep(0.05)
  }

  stopifnot(identical(task$status(), "success"))
  calculated <- task$result()
  stopifnot(identical(calculated$job_table$狀態, "完成"))
  stopifnot(length(calculated$results[["115_C4"]]$exported_files) == 14L)
  background_result <<- calculated
})

# ---------------------------------------------------------------------------
# 批次模式：驗證 ZIP 解壓後可建立同一個 C4 工作。
# ---------------------------------------------------------------------------
batch_path <- file.path(test_root, "fixture.zip")
zip::zipr(
  batch_path,
  files = c(
    fixture$answer_path,
    fixture$response_path
  ),
  root = fixture$root
)
batch_upload <- upload_row(batch_path, "application/zip")

shiny::testServer(mod_run_server, {
  session$setInputs(
    mode = "batch",
    year = "115",
    batch_zip = batch_upload,
    prepare_files = 1
  )
  session$flushReact()

  stopifnot(length(staged_jobs()) == 1L)
  stopifnot(identical(staged_jobs()[[1L]]$key, "115_C4"))
  stopifnot(identical(staged_preview()$狀態, "可執行"))
})

# ---------------------------------------------------------------------------
# 結果模組：驗證工作選擇、預覽表、分布圖與摘要卡都能完成 render。
# ---------------------------------------------------------------------------
result_value <- shiny::reactiveVal(background_result)
shiny::testServer(
  mod_results_server,
  args = list(run_result = result_value),
  {
    session$setInputs(
      selected_job = "115_C4",
      selected_view = "county"
    )
    session$flushReact()

    stopifnot(identical(selected_result()$key, "115_C4"))
    stopifnot(nrow(selected_result()$views$county) == 2L)
    stopifnot(!is.null(output$result_table))
    stopifnot(!is.null(output$distribution_plot))
    stopifnot(!is.null(output$summary_cards))
  }
)

# ---------------------------------------------------------------------------
# 資料清洗模組：驗證上傳檔案、觸發清洗、修復診斷卡片與表格預覽。
# ---------------------------------------------------------------------------
shiny::testServer(mod_cleansing_server, {
  session$setInputs(
    raw_file = response_upload,
    opt_fix_gender = TRUE,
    opt_consolidate_special = TRUE,
    opt_match_school = TRUE,
    run_cleansing = 1
  )
  session$flushReact()

  stopifnot(!is.null(cleaned_result()))
  stopifnot(!is.null(output$diagnostic_ui))
  stopifnot(!is.null(output$preview_table))
})

cat(
  paste(
    "Module test passed:",
    "single/batch staging, background execution, and result rendering.\n"
  )
)
