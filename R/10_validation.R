# =============================================================================
# 檔案：R/10_validation.R
# 用途：在正式計分前驗證欄位、學生流水號、答案檔、作答檔及工作定義。
# 原則：錯誤應在資料邊界提早攔截，計分函式不應默默猜測不合法資料。
# 修改入口：
#   - 接受新的中文欄名：改 R/00_constants.R 的 INFO_COLUMN_ALIASES。
#   - 更改答案檔工作表規格：改 validate_answer_workbook()。
#   - 更改作答檔最低規格：改 validate_response_workbook()。
# =============================================================================

# 將作答檔前 23 欄的中文標題解析成程式使用的欄位位置。
#
# headers：第一列資訊欄標題。
# 回傳值：具名整數向量，例如 id=1、city=3。
# 若找不到別名，會警告並回退到舊版固定位置；若欄位重複則中止。
resolve_info_columns <- function(headers) {
  normalized_headers <- normalize_header(headers)

  resolved <- vapply(names(INFO_COLUMN_ALIASES), function(field) {
    candidates <- normalize_header(INFO_COLUMN_ALIASES[[field]])
    matched <- which(normalized_headers %in% candidates)

    if (length(matched) > 1L) {
      abort_score(
        "資訊欄位重複：",
        paste(INFO_COLUMN_ALIASES[[field]], collapse = " / ")
      )
    }
    if (length(matched) == 1L) {
      return(matched)
    }

    # 若標題未精確匹配，優先檢查預設固定欄位位置 (若該位置尚未被佔用)
    fallback <- DEFAULT_INFO_COLUMNS[[field]]
    if (!is.null(fallback) && fallback <= length(headers)) {
      # 只有當備用欄位的現有標題屬於未識別的字串時才考慮 fallback
      # 避免把「縣市」強行認作「總流水號」
      return(NA_integer_)
    }

    NA_integer_
  }, integer(1))

  # 驗證必要核心學生資訊欄位
  required_fields <- c("city", "school_code", "school_name", "class_code", "seat_no", "student_name")
  missing_req <- required_fields[is.na(resolved[required_fields])]
  if (length(missing_req) > 0L) {
    req_labels <- vapply(missing_req, function(f) INFO_COLUMN_ALIASES[[f]][[1L]], character(1))
    abort_score(
      "找不到必要資訊欄位：",
      paste(req_labels, collapse = ", ")
    )
  }

  valid_resolved <- resolved[!is.na(resolved)]
  duplicated_positions <- unique(valid_resolved[duplicated(valid_resolved)])
  if (length(duplicated_positions) > 0L) {
    abort_score(
      "多個必要欄位被解析到相同位置：",
      paste(duplicated_positions, collapse = ", ")
    )
  }

  resolved
}

# 確認每位學生都有唯一且非空白的流水號。
#
# 顯示的問題列會加 1，因為原始 Excel 的第一列是標題列。
# 為避免錯誤訊息過長，最多列出前 10 筆問題。
validate_student_ids <- function(student_ids) {
  missing_rows <- which(is.na(student_ids) | trimws(student_ids) == "")
  if (length(missing_rows) > 0L) {
    abort_score(
      "流水號不可空白；問題資料列：",
      paste(head(missing_rows + 1L, 10L), collapse = ", ")
    )
  }

  duplicated_ids <- unique(student_ids[duplicated(student_ids)])
  if (length(duplicated_ids) > 0L) {
    abort_score(
      "流水號不可重複；重複值：",
      paste(head(duplicated_ids, 10L), collapse = ", ")
    )
  }

  invisible(TRUE)
}

# 讀取指定向度欄，並只保留答案表中真正有效的題目。
#
# 動態解析答案檔「向度」工作表中，與指定科目年級匹配的所有向度欄位名稱。
#
# 支援多種向度命名約定：
# 1. 雙維度/多維度帶向度字尾：S51向度, S52向度, E41_向度, E42_向度 或 S5_1向度, S5_1_向度
# 2. 雙維度/多維度純數字標籤：S51, S52 或 S5_1, S5_2
# 3. 通用向度欄位：S5向度 或 S5_向度
# 4. 中文年級標籤：三年級, 四年級 或 三年級向度
# 5. 精確匹配：S5, C4
resolve_dimension_columns <- function(dim_colnames, subject_code, grade) {
  answer_column <- paste0(subject_code, grade)

  # 1. 搜尋包含多重向度字尾欄：如 S51向度, S51_向度, E41_向度, E42_向度 或 E4_1_向度
  pattern1 <- sprintf("^%s[0-9]+_?向度$", answer_column)
  pattern2 <- sprintf("^%s_[0-9]+_?向度$", answer_column)

  matched1 <- grep(pattern1, dim_colnames, value = TRUE)
  matched2 <- grep(pattern2, dim_colnames, value = TRUE)

  if (length(matched1) > 0L) return(sort(matched1))
  if (length(matched2) > 0L) return(sort(matched2))

  # 2. 搜尋純數字向度編號欄：如 S51, S52 或 S5_1, S5_2
  pattern3 <- sprintf("^%s[0-9]+$", answer_column)
  pattern4 <- sprintf("^%s_[0-9]+$", answer_column)

  matched3 <- grep(pattern3, dim_colnames, value = TRUE)
  matched4 <- grep(pattern4, dim_colnames, value = TRUE)

  if (length(matched3) > 0L) return(sort(matched3))
  if (length(matched4) > 0L) return(sort(matched4))

  # 3. 搜尋通用「向度」字尾欄：如 S5向度, S5_向度
  pattern5 <- sprintf("^%s_?向度$", answer_column)
  matched5 <- grep(pattern5, dim_colnames, value = TRUE)
  if (length(matched5) > 0L) return(matched5)

  # 4. 中文年級 Pattern：如「三年級」、「四年級」
  chinese_grade <- unname(GRADE_TO_CHINESE[as.character(grade)])
  if (!is.na(chinese_grade)) {
    if (chinese_grade %in% dim_colnames) return(chinese_grade)
    ch_dim_pat <- sprintf("^%s_?向度$", chinese_grade)
    ch_matched <- grep(ch_dim_pat, dim_colnames, value = TRUE)
    if (length(ch_matched) > 0L) return(ch_matched)
  }

  # 5. 精確匹配答案欄名如 S5, C4
  if (answer_column %in% dim_colnames) {
    return(answer_column)
  }

  abort_score("向度表缺少欄位：", answer_column)
}

# 讀取指定向度欄，並只保留答案表中真正有效的題目。
#
# dimension_table：答案檔「向度」工作表。
# column_name：本次科目／年級對應的向度欄名。
# n_items_raw：作答檔原始題數。
# valid_items：答案不是 NA 或空字串的題號位置。
# 回傳值：與實際計分題目等長的向度標籤向量。
get_dimension_labels <- function(
  dimension_table,
  column_name,
  n_items_raw,
  valid_items
) {
  if (!column_name %in% colnames(dimension_table)) {
    abort_score("向度表缺少欄位：", column_name)
  }

  labels <- as.character(dimension_table[[column_name]])
  if (length(labels) < n_items_raw) {
    abort_score(
      sprintf(
        "向度欄位「%s」只有 %d 列，但原始作答有 %d 題。",
        column_name,
        length(labels),
        n_items_raw
      )
    )
  }

  labels[seq_len(n_items_raw)][valid_items]
}

# 驗證答案活頁簿的基本結構。
#
# 必須包含「答案」與「向度」工作表；若提供 grade，還會確認如 C4 的
# 答案欄存在。此函式只讀取必要範圍，避免檔案檢查階段載入整本活頁簿。
validate_answer_workbook <- function(path, subject_code, grade = NULL) {
  if (!file.exists(path)) {
    abort_score("答案檔不存在：", path)
  }

  sheets <- openxlsx::getSheetNames(path)
  dim_sheet <- find_dimension_sheet(sheets)
  if (is.null(dim_sheet)) {
    abort_score(
      basename(path),
      " 缺少「向度」或「評量指標」工作表。"
    )
  }

  if (!is.null(grade)) {
    answer_headers <- colnames(
      openxlsx::read.xlsx(path, sheet = "答案", rows = 1L)
    )
    answer_column <- paste0(subject_code, grade)
    if (!answer_column %in% answer_headers) {
      abort_score(
        basename(path),
        " 缺少答案欄位：",
        answer_column
      )
    }
  }

  invisible(TRUE)
}

# 快速驗證作答檔至少有標題、1 名學生、23 個資訊欄及 1 個作答欄。
# 完整欄名解析與流水號驗證會在 prepare_job_input() 中進行。
validate_response_workbook <- function(path) {
  if (!file.exists(path)) {
    abort_score("作答檔不存在：", path)
  }

  preview <- openxlsx::read.xlsx(
    path,
    sheet = 1,
    colNames = FALSE,
    rows = 1:2
  )
  if (nrow(preview) < 2L) {
    abort_score(basename(path), " 沒有學生資料列。")
  }
  if (ncol(preview) <= N_INFO_COLUMNS) {
    abort_score(
      sprintf(
        "%s 只有 %d 欄，至少需要 %d 個資訊欄位及 1 個作答欄位。",
        basename(path),
        ncol(preview),
        N_INFO_COLUMNS
      )
    )
  }

  invisible(TRUE)
}

# 驗證工作清單本身，不重新讀取 Excel。
#
# job 至少要有 year、subject_code、grade、answer_path、response_path。
# 本函式適合正式分析入口使用，可避免前面預覽階段已讀過的檔案被重讀。
validate_job_metadata <- function(job) {
  required <- c(
    "year",
    "subject_code",
    "grade",
    "answer_path",
    "response_path"
  )
  missing_fields <- setdiff(required, names(job))
  if (length(missing_fields) > 0L) {
    abort_score(
      "工作定義缺少欄位：",
      paste(missing_fields, collapse = ", ")
    )
  }
  if (!job$subject_code %in% names(SUBJECT_NAMES)) {
    abort_score("不支援的科目代號：", job$subject_code)
  }
  if (length(job$grade) != 1L || is.na(job$grade)) {
    abort_score("年級必須是單一有效值。")
  }
  if (isTRUE(job$calc_level)) {
    if (is.na(job$mastery_cutoff) || is.na(job$basic_cutoff)) {
      abort_score("勾選「計算精熟等級」時，必須輸入「精熟門檻題數」與「基礎門檻題數」。")
    }
    if (job$mastery_cutoff <= job$basic_cutoff) {
      abort_score("精熟門檻題數（", job$mastery_cutoff, "）必須大於基礎門檻題數（", job$basic_cutoff, "）。")
    }
    if (job$basic_cutoff <= 0) {
      abort_score("基礎門檻題數必須大於 0。")
    }
  }
  if (!file.exists(job$answer_path)) {
    abort_score("答案檔不存在：", job$answer_path)
  }
  if (!file.exists(job$response_path)) {
    abort_score("作答檔不存在：", job$response_path)
  }

  invisible(TRUE)
}

# 執行完整工作驗證：工作欄位、答案檔結構及作答檔最低規格。
# 檔案預覽／工作發現階段使用此函式，讓使用者能在計算前看到錯誤。
validate_job_spec <- function(job) {
  validate_job_metadata(job)
  validate_answer_workbook(
    job$answer_path,
    job$subject_code,
    job$grade
  )
  validate_response_workbook(job$response_path)
  invisible(TRUE)
}
