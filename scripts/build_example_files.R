# =============================================================================
# 檔案：scripts/build_example_files.R
# 用途：由 tests/create_fixture.R 重建可下載的匿名 Excel 與批次 ZIP。
# 執行：在專案根目錄輸入 Rscript scripts/build_example_files.R
# 輸出：www/examples/；只含合成姓名、學校及代碼，不可放入真實個資。
# 維護：測試資料格式改變後執行本腳本，讓測試與使用者範例保持同步。
# =============================================================================

# 以目前工作目錄作為專案根目錄，避免寫死開發者電腦路徑。
project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)
# 防止在錯誤目錄執行而把範例寫到非預期位置。
if (!file.exists(file.path(project_root, "app.R"))) {
  stop(
    "請在 115-score-shiny 專案根目錄執行。",
    call. = FALSE
  )
}

# 載入匿名 fixture 產生器；source 時不會自行寫檔。
sys.source(
  file.path(project_root, "tests", "create_fixture.R"),
  envir = globalenv()
)

# 建立單科答案與作答範例。
destination <- file.path(
  project_root,
  "www",
  "examples"
)
fixture <- create_anonymous_fixture(destination)

# 將同兩份 Excel 另包成批次模式範例。
batch_path <- file.path(
  destination,
  "115_匿名範例批次.zip"
)
if (file.exists(batch_path)) {
  # zipr 不應沿用舊壓縮檔內容，因此重建前先移除既有範例。
  unlink(batch_path)
}
zip::zipr(
  batch_path,
  files = c(
    basename(fixture$answer_path),
    basename(fixture$response_path)
  ),
  root = destination
)

cat("Anonymous example files generated in www/examples.\n")
