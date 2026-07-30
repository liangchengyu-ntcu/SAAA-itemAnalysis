# =============================================================================
# 檔案：R/06_merge.R
# 用途：提供多個分散學校/班級作答檔之自動識別、C/E/M/S 科目與 3~8 年級
#       分卷拆分、題號補零、縣市與總流水號生成及 Excel 樣式美化整併。
# 載入：依檔名排序在 R/10_validation.R 前載入。
# =============================================================================

# 1. 單一作答檔欄位標題與題號標準化
standardize_merge_columns <- function(df) {
  # 重命名常見別名
  name_map <- c(
    "新移民家庭子女" = "新住民子女",
    "原住民" = "原住民子女",
    "資源班" = "特殊生",
    "分校/分班註記" = "分校註記",
    "鄉政市區" = "鄉鎮區",
    "鄉鎮市區" = "鄉鎮區"
  )
  for (old_name in intersect(names(name_map), names(df))) {
    names(df)[names(df) == old_name] <- name_map[old_name]
  }

  # 題號標準化 (1 -> 題號01, 答案1 -> 題號01)
  for (i in seq_along(names(df))) {
    col <- names(df)[i]
    if (grepl("^\\d+$", col)) {
      names(df)[i] <- sprintf("題號%02d", as.integer(col))
    } else if (grepl("^答案\\d+$", col)) {
      qnum <- as.integer(gsub("^答案", "", col))
      names(df)[i] <- sprintf("題號%02d", qnum)
    } else if (grepl("^題\\d+$", col)) {
      qnum <- as.integer(gsub("^題", "", col))
      names(df)[i] <- sprintf("題號%02d", qnum)
    }
  }

  # 移除不需要的雜項舊欄位
  unwanted <- c("流水號", "學號", "缺考", "代碼鄉鎮區吻合")
  df <- df[, setdiff(names(df), unwanted), drop = FALSE]
  df
}

# 2. 自動從檔名偵測年度、科目代碼與年級 (如 115_C4_校名.xlsx)
detect_file_metadata <- function(filename) {
  filename <- basename(filename)

  # 尋找 3 位數年度
  yr_match <- regmatches(filename, regexpr("\\d{3}", filename))
  year <- if (length(yr_match) > 0L) yr_match[1L] else "115"

  # 尋找科目與年級組合 (C4, M5, 英4, 國5...)
  subj <- "C"
  grade <- "4"

  if (grepl("[C國]", filename, ignore.case = TRUE)) subj <- "C"
  else if (grepl("[E英]", filename, ignore.case = TRUE)) subj <- "E"
  else if (grepl("[M數]", filename, ignore.case = TRUE)) subj <- "M"
  else if (grepl("[S自]", filename, ignore.case = TRUE)) subj <- "S"

  gr_match <- regmatches(filename, regexpr("[3-8]", filename))
  if (length(gr_match) > 0L) grade <- gr_match[1L]

  list(year = year, subject = subj, grade = grade, key = paste0(subj, grade))
}

# 3. 檔案整併與分卷總入口
merge_and_split_files <- function(file_paths, default_year = "115", output_dir = tempfile("merge-")) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  if (length(file_paths) == 0L) {
    stop("請至少提供一個待整併檔案。")
  }

  # 讀取並標記所有檔案內容
  file_list <- list()
  group_buckets <- list()

  for (fpath in file_paths) {
    if (!file.exists(fpath)) next
    meta <- detect_file_metadata(fpath)

    df <- tryCatch({
      df_raw <- openxlsx::read.xlsx(fpath, sheet = 1, colNames = TRUE)
      standardize_merge_columns(df_raw)
    }, error = function(e) NULL)

    if (is.null(df) || nrow(df) == 0L) next

    # 若資料內有科目/年級欄位，優先以資料為主
    subj_col <- grep("科目", names(df), value = TRUE)[1L]
    grade_col <- grep("年級", names(df), value = TRUE)[1L]

    if (!is.na(subj_col) && !is.na(grade_col)) {
      df$group_key <- paste0(trimws(as.character(df[[subj_col]])), trimws(as.character(df[[grade_col]])))
    } else {
      df$group_key <- meta$key
    }

    # 依 group_key 分組歸類
    unique_keys <- unique(df$group_key)
    for (k in unique_keys) {
      sub_df <- df[df$group_key == k, , drop = FALSE]
      sub_df$group_key <- NULL
      group_buckets[[k]] <- c(group_buckets[[k]], list(sub_df))
    }
  }

  if (length(group_buckets) == 0L) {
    stop("未成功讀取任何有效作答資料。")
  }

  # 針對每個 (科目x年級) 分卷進行合併與格式化
  merged_results <- list()
  exported_files <- character()

  # 表頭美化樣式 (綠底 #D9EAD3 + 粗體 + 置中 + 細邊框)
  hdr_style <- openxlsx::createStyle(
    fgFill = "#D9EAD3",
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    borderStyle = "thin"
  )

  for (k in sort(names(group_buckets))) {
    bucket <- group_buckets[[k]]
    combined_df <- do.call(rbind, bucket)

    # 班級代碼補年級前綴 (若為 2 位數)
    if ("班級代碼" %in% names(combined_df) && "年級" %in% names(combined_df)) {
      cls_vals <- trimws(as.character(combined_df$班級代碼))
      gr_vals <- trimws(as.character(combined_df$年級))
      mask2 <- grepl("^\\d{2}$", cls_vals)
      if (any(mask2)) {
        combined_df$班級代碼[mask2] <- paste0(gr_vals[mask2], cls_vals[mask2])
      }
    }

    # 生成縣市流水號與總流水號
    city_col <- if ("縣市" %in% names(combined_df)) "縣市" else if ("縣市名稱" %in% names(combined_df)) "縣市名稱" else NULL
    city_name_val <- if (!is.null(city_col)) combined_df[[city_col]] else "全區"

    combined_df$縣市流水號 <- generate_county_id(
      year = default_year,
      city_name = city_name_val,
      volume = k,
      serial_number = seq_len(nrow(combined_df))
    )

    combined_df$總流水號 <- sprintf("%s_%s_%06d", default_year, k, seq_len(nrow(combined_df)))

    # 排列欄位順序：總流水號 (Column A), 縣市流水號 (Column B), ...其餘資訊欄與題號
    other_cols <- setdiff(names(combined_df), c("總流水號", "縣市流水號"))
    combined_df <- combined_df[, c("總流水號", "縣市流水號", other_cols), drop = FALSE]

    # 匯出至 Excel 並套用美化樣式
    out_name <- sprintf("%s_%s_整併.xlsx", default_year, k)
    out_path <- file.path(output_dir, out_name)

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "整併資料")
    openxlsx::writeData(wb, "整併資料", combined_df, startCol = 1, startRow = 1)
    openxlsx::addStyle(wb, "整併資料", hdr_style, rows = 1, cols = seq_len(ncol(combined_df)), gridExpand = TRUE)
    openxlsx::setColWidths(wb, "整併資料", cols = 1:2, widths = 22)
    openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)

    exported_files <- c(exported_files, out_path)

    # 紀錄分卷摘要
    school_col <- if ("學校名稱" %in% names(combined_df)) "學校名稱" else NULL
    n_schools <- if (!is.null(school_col)) length(unique(combined_df[[school_col]])) else 1L

    merged_results[[k]] <- list(
      key = k,
      year = default_year,
      n_rows = nrow(combined_df),
      n_schools = n_schools,
      id_range = sprintf("%s ~ %s", combined_df$縣市流水號[1L], combined_df$縣市流水號[nrow(combined_df)]),
      file_path = out_path
    )
  }

  # 打包為整併結果 ZIP 檔
  zip_path <- file.path(output_dir, sprintf("%s_檔案整併與分卷結果.zip", default_year))
  zip::zipr(zip_path, files = exported_files, root = output_dir)

  list(
    results = merged_results,
    exported_files = exported_files,
    zip_path = zip_path
  )
}
