# =============================================================================
# 檔案：install_dependencies.R
# 用途：一次安裝本機缺少的執行期套件。
# 執行：在專案根目錄輸入 Rscript install_dependencies.R
# 維護：本清單須與 app.R 的 required_packages 及 DESCRIPTION Imports 同步。
# 注意：正常啟動 app.R 不會安裝套件，只有本維護腳本會改動 R library。
# =============================================================================

# 應用執行時必須可用的套件。
packages <- c(
  "bslib",
  "data.table",
  "future",
  "openxlsx",
  "promises",
  "Rmpfr",
  "shiny",
  "writexl",
  "zip"
)

# quietly=TRUE 可在不載入套件的情況下確認 namespace 是否存在。
missing <- packages[
  !vapply(
    packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]
if (length(missing) > 0L) {
  # 只安裝缺少項目，已安裝套件不會被強制更新。
  install.packages(missing)
}

# 即使原本沒有缺件，也提供明確完成訊息。
cat("Dependencies are ready.\n")
