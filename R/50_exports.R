# =============================================================================
# 檔案：R/50_exports.R
# 用途：把分析結果映射成 11 份 Excel、結果頁預覽資料及整批 ZIP。
# 設計原則：畫面預覽和下載檔必須共用 build_export_tables() 的資料，
#           不能各自重新計算，以免兩者數字不一致。
# 修改入口：
#   - 新增／刪除 Excel：job_export_definitions() 與 build_export_tables()。
#   - 調整可預覽報表：build_result_views() 及 R/00_constants.R。
#   - 調整檔名：job_output_filename() 或個別 definition 的 suffix。
# =============================================================================

# 建立單一工作的輸出資料夾：例如 S5、S7、S8。
# 回傳正規化絕對路徑，供後續所有 Excel 寫入共用。
job_output_directory <- function(output_root, job) {
  ensure_directory(
    file.path(
      output_root,
      paste0(job$subject_code, job$grade)
    )
  )
}

# 依既有命名契約組成 Excel 檔名，例如 115_C4_總平均.xlsx。
# suffix 由 job_export_definitions() 集中管理。
job_output_filename <- function(job, suffix) {
  paste0(
    job$year,
    "_",
    job$subject_code,
    job$grade,
    suffix,
    ".xlsx"
  )
}

# 將一張 data.frame 寫成單工作表 Excel。
#
# 使用 writexl 是為了大量輸出效能；工作表固定命名為 "Sheet 1"，
# 以保留既有檔案結構。回傳值為已確認存在的絕對檔案路徑。
write_result_workbook <- function(data, path) {
  writexl::write_xlsx(
    stats::setNames(
      list(data),
      "Sheet 1"
    ),
    path
  )
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

# 定義每份輸出的內部鍵、中文用途、檔名後綴及是否適合畫面預覽。
#
# 內部鍵必須與 build_export_tables() 的清單名稱完全一致。
# preview=FALSE 的報表含個資或明細，不應直接顯示在網頁上。
# 注意 overall_scores 的 suffix 未以底線開頭是沿用既有檔名契約。
job_export_definitions <- function(calc_level = TRUE) {
  defs <- list(
    overall_scores = list(
      label = "全體總答對率",
      suffix = "全體總答對率",
      preview = FALSE
    ),
    county = list(
      label = "縣市平均",
      suffix = "_縣市平均",
      preview = TRUE
    ),
    school = list(
      label = "各校平均",
      suffix = "_各校平均",
      preview = TRUE
    ),
    class = list(
      label = "各班平均",
      suffix = "_各班平均",
      preview = TRUE
    ),
    region = list(
      label = "縣市區域平均",
      suffix = "_縣市區域平均",
      preview = TRUE
    ),
    family = list(
      label = "不同家庭背景平均",
      suffix = "_不同家庭背景平均",
      preview = TRUE
    ),
    county_level = list(
      label = "縣市平均（等級描述）",
      suffix = "_縣市平均(等級描述)",
      preview = TRUE
    ),
    school_level = list(
      label = "各校平均（等級描述）",
      suffix = "_各校平均(等級描述)",
      preview = TRUE
    ),
    class_level = list(
      label = "各班平均（等級描述）",
      suffix = "_各班平均(等級描述)",
      preview = TRUE
    ),
    region_level = list(
      label = "縣市區域平均（等級描述）",
      suffix = "_縣市區域平均(等級描述)",
      preview = TRUE
    ),
    family_level = list(
      label = "不同家庭背景平均（等級描述）",
      suffix = "_不同家庭背景平均(等級描述)",
      preview = TRUE
    ),
    absent = list(
      label = "缺考名單",
      suffix = "_缺考名單",
      preview = FALSE
    ),
    personal_with_count = list(
      label = "個人成績含題數",
      suffix = "_個人成績含題數",
      preview = FALSE
    ),
    all_students = list(
      label = "全體名單成績含缺考",
      suffix = "_全體名單成績含缺考",
      preview = FALSE
    ),
    personal_detail = list(
      label = "個人成績",
      suffix = "_個人成績",
      preview = FALSE
    ),
    total = list(
      label = "總平均",
      suffix = "_總平均",
      preview = TRUE
    ),
    total_level = list(
      label = "總平均（等級描述）",
      suffix = "_總平均(等級描述)",
      preview = TRUE
    )
  )

  if (!isTRUE(calc_level)) {
    level_keys <- c(
      "county_level",
      "school_level",
      "class_level",
      "region_level",
      "family_level",
      "total_level"
    )
    defs <- defs[!names(defs) %in% level_keys]
  }

  defs
}

# 將 analyze_job() 的巢狀結果整理成正式輸出表。
build_export_tables <- function(analysis) {
  summaries <- analysis$summaries
  ranked <- analysis$ranked_outputs
  scores <- analysis$score_data$student_scores
  calc_level <- isTRUE(analysis$prepared$job$calc_level)

  tbls <- list(
    overall_scores = scores,
    county = summaries$county_means,
    school = summaries$school_means,
    class = summaries$class_means,
    region = summaries$region_means,
    family = summaries$family_means,
    county_level = summaries$county_level_means,
    school_level = summaries$school_level_means,
    class_level = summaries$class_level_means,
    region_level = summaries$region_level_means,
    family_level = summaries$family_level_means,
    absent = summaries$absent_list,
    personal_with_count = ranked$personal_with_count,
    all_students = ranked$personal_all,
    personal_detail = ranked$personal_output,
    total = ranked$total_output,
    total_level = ranked$total_level_output
  )

  if (!calc_level) {
    level_keys <- c(
      "county_level",
      "school_level",
      "class_level",
      "region_level",
      "family_level",
      "total_level"
    )
    tbls <- tbls[!names(tbls) %in% level_keys]
  }

  tbls
}

# 寫出單一工作的全部 Excel。
export_job_result <- function(
  analysis,
  output_root,
  export_tables = NULL
) {
  job <- analysis$prepared$job
  output_directory <- job_output_directory(output_root, job)
  calc_level <- isTRUE(job$calc_level)
  definitions <- job_export_definitions(calc_level = calc_level)
  if (is.null(export_tables)) {
    export_tables <- build_export_tables(analysis)
  }

  paths <- lapply(names(definitions), function(key) {
    write_result_workbook(
      export_tables[[key]],
      file.path(
        output_directory,
        job_output_filename(
          job,
          definitions[[key]]$suffix
        )
      )
    )
  })
  names(paths) <- names(definitions)

  unlist(paths, use.names = TRUE)
}

# 只挑選無個資的彙總表供網頁預覽，順序對應 RESULT_VIEWS。
build_result_views <- function(export_tables) {
  preview_order <- c(
    "total",
    "county",
    "school",
    "class",
    "region",
    "family"
  )
  export_tables[preview_order]
}

# 將本次工作階段的全部輸出 Excel 壓成下載 ZIP。
#
# output_root 只應是 session 專用暫存資料夾。
# archive_path 若已存在會先刪除，避免 ZIP 工具附加舊內容。
# 回傳值：已建立 ZIP 的絕對路徑。
create_result_archive <- function(output_root, archive_path) {
  output_root <- normalizePath(
    output_root,
    winslash = "/",
    mustWork = TRUE
  )
  files <- list.files(
    output_root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE
  )
  # list.files() 可能包含子資料夾，ZIP 只需要真正的檔案。
  files <- files[file.info(files)$isdir %in% FALSE]
  if (length(files) == 0L) {
    abort_score("沒有可供下載的輸出檔案。")
  }

  archive_directory <- dirname(archive_path)
  ensure_directory(archive_directory)
  if (file.exists(archive_path)) {
    unlink(archive_path)
  }

  zip::zipr(
    zipfile = archive_path,
    files = files,
    root = output_root,
    include_directories = FALSE
  )
  normalizePath(archive_path, winslash = "/", mustWork = TRUE)
}
