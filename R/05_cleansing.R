# =============================================================================
# 檔案：R/05_cleansing.R
# 用途：提供前置資料清洗、雙層表頭處理、欄名標準化、特教障礙欄位整併、
#       性別與身分證錯置/交換修復、性別自動推導、學校資料校對、
#       異常檢核報表匯出與依科目年級拆分分割。
# 載入：依檔名排序在 R/10_validation.R 前載入。
# =============================================================================

# 1. 處理南投縣原始作答雙層表頭與子表頭結構
handle_nantou_raw_header <- function(df) {
  if (nrow(df) < 1L) return(df)

  # 情況 A：偵測第1列第15欄包含「資賦優異」
  if (ncol(df) >= 15L && !is.na(df[1L, 15L]) && trimws(as.character(df[1L, 15L])) == "資賦優異") {
    sub_vals <- vapply(df[1L, ], function(x) if (is.na(x)) "" else trimws(as.character(x)), character(1L))
    new_names <- names(df)
    for (i in 15:min(21, ncol(df))) {
      if (nzchar(sub_vals[i])) new_names[i] <- sub_vals[i]
    }
    for (i in 22:ncol(df)) {
      if (grepl("^\\d+$", sub_vals[i])) {
        new_names[i] <- sprintf("題號%02d", as.integer(sub_vals[i]))
      }
    }
    names(df) <- new_names
    df <- df[-1L, , drop = FALSE]
  }

  # 情況 B：處理 readxl/openxlsx 的 X1/...1 自動補名
  x_cols <- grep("^(X|\\.\\.\\.)\\d+$", names(df))
  if (length(x_cols) > 0L && nrow(df) >= 1L) {
    first_row_x <- as.character(df[1L, x_cols])
    if (any(!is.na(first_row_x) & grepl("[\u4e00-\u9fff]", first_row_x))) {
      new_names <- names(df)
      for (i in seq_along(new_names)) {
        val <- as.character(df[1L, i])
        if (!is.na(val) && nzchar(val)) new_names[i] <- val
      }
      names(df) <- new_names
      df <- df[-1L, , drop = FALSE]
    }
  }

  # 還原數字或舊欄名（題1 -> 題號01, 1 -> 題號01）
  is_num_col <- grepl("^\\d+$", names(df))
  if (any(is_num_col)) {
    for (i in which(is_num_col)) {
      names(df)[i] <- sprintf("題號%02d", as.integer(names(df)[i]))
    }
  }
  is_old_col <- grepl("^題\\d+$", names(df))
  if (any(is_old_col)) {
    for (i in which(is_old_col)) {
      qnum <- as.integer(gsub("^題", "", names(df)[i]))
      names(df)[i] <- sprintf("題號%02d", qnum)
    }
  }

  df
}

# 2. 統一欄位名稱與常見別名轉換
standardise_column_names <- function(df) {
  name_map <- c(
    "學校所屬區域(鄉鎮區)" = "鄉鎮區",
    "身分證.字號" = "身分證字號",
    "身分證\r\n字號" = "身分證字號",
    "身份證字號" = "身分證字號",
    "特殊生（成績不列入平均）" = "特殊生",
    "特殊生(成績不列入平均)" = "特殊生",
    "非學校型態實驗教育者（在家教育）" = "在家教育",
    "非學校型態實驗教育者(在家教育)" = "在家教育",
    "不列入計分" = "特殊生",
    "測驗科目代碼" = "科目代碼"
  )
  for (old_name in intersect(names(name_map), names(df))) {
    names(df)[names(df) == old_name] <- name_map[old_name]
  }

  # 移除不需要的原始雜項欄位
  drop_cols <- intersect(c("流水號", "學號", "缺考"), names(df))
  if (length(drop_cols) > 0L) {
    df <- df[, setdiff(names(df), drop_cols), drop = FALSE]
  }

  df
}

# 3. 整併 11 項特殊障礙類別欄位為「特殊生」代碼
consolidate_special_students <- function(df) {
  valid_vals <- c("1", "2", "3")
  special_cols <- c(
    "智能障礙", "視覺障礙", "聽覺障礙", "語言障礙",
    "肢體障礙", "身體病弱", "嚴重情障", "學習障礙",
    "多重障礙", "自閉症", "發展遲緩及其他顯著障礙"
  )
  existing <- intersect(special_cols, names(df))
  count_fixed <- 0L

  if (length(existing) > 0L) {
    rows_flagged <- rowSums(
      !is.na(df[, existing, drop = FALSE]) & trimws(as.matrix(df[, existing, drop = FALSE])) != "",
      na.rm = TRUE
    ) > 0

    if ("特殊生" %in% names(df)) {
      has_valid <- !is.na(df$特殊生) & trimws(as.character(df$特殊生)) %in% valid_vals
      fix_target <- rows_flagged & !has_valid
      count_fixed <- sum(fix_target, na.rm = TRUE)
      df$特殊生[fix_target] <- "1"
    } else {
      df$特殊生 <- ifelse(rows_flagged, "1", "")
      count_fixed <- sum(rows_flagged, na.rm = TRUE)
    }

    # 清除非 1/2/3 且非空白之無效特殊生註記
    invalid_mask <- !is.na(df$特殊生) & df$特殊生 != "" & !(df$特殊生 %in% valid_vals)
    df$特殊生[invalid_mask] <- ""

    # 移除被整併的舊欄位
    df <- df[, setdiff(names(df), existing), drop = FALSE]
  }

  # 若有「資賦優異」，將「特殊生」欄位調整至其後方
  if (all(c("資賦優異", "特殊生") %in% names(df))) {
    cols <- names(df)
    sp_idx <- which(cols == "特殊生")
    gf_idx <- which(cols == "資賦優異")
    if (sp_idx != gf_idx + 1L) {
      cols <- setdiff(cols, "特殊生")
      cols <- append(cols, "特殊生", after = gf_idx)
      df <- df[, cols, drop = FALSE]
    }
  }

  list(df = df, count_fixed = count_fixed)
}

# 4. 台灣身分證與居留證檢核碼驗證 (Modulus 10)
validate_taiwan_id_checksum <- function(id) {
  if (is.na(id) || trimws(id) == "") return(FALSE)
  id <- trimws(toupper(as.character(id)))
  if (nchar(id) != 10L) return(FALSE)

  chars <- strsplit(id, "")[[1L]]
  if (!grepl("^[A-Z]$", chars[1L])) return(FALSE)

  code_map <- c(
    A = 10, B = 11, C = 12, D = 13, E = 14, F = 15, G = 16, H = 17,
    J = 18, K = 19, L = 20, M = 21, N = 22, P = 23, Q = 24, R = 25,
    S = 26, T = 27, U = 28, V = 29, X = 30, Y = 31, W = 32, Z = 33,
    I = 34, O = 35
  )

  n1 <- code_map[chars[1L]] %/% 10
  n2 <- code_map[chars[1L]] %% 10

  is_old_style <- grepl("^[A-D]$", chars[2L])

  if (is_old_style) {
    if (!all(grepl("^[0-9]$", chars[3:10]))) return(FALSE)
    m2 <- match(chars[2L], c("A", "B", "C", "D")) - 1L
    digits <- as.integer(chars[3:10])
    sum_val <- n1 * 1 + n2 * 9 + m2 * 8 +
      digits[1] * 7 + digits[2] * 6 + digits[3] * 5 + digits[4] * 4 +
      digits[5] * 3 + digits[6] * 2 + digits[7] * 1 + digits[8] * 1
    (sum_val %% 10 == 0)
  } else {
    if (!all(grepl("^[0-9]$", chars[2:10]))) return(FALSE)
    digits <- as.integer(chars[2:10])
    sum_val <- n1 * 1 + n2 * 9 +
      digits[1] * 8 + digits[2] * 7 + digits[3] * 6 + digits[4] * 5 +
      digits[5] * 4 + digits[6] * 3 + digits[7] * 2 + digits[8] * 1 + digits[9] * 1
    (sum_val %% 10 == 0)
  }
}

# 5. 性別與身分證錯置修復（含交換 Swap、ID單邊錯置、性別單邊錯置）
fix_gender_and_id <- function(df) {
  actual_gender <- if ("性別代碼" %in% names(df)) "性別代碼" else if ("性別" %in% names(df)) "性別" else NULL
  actual_id <- grep("身[份分]證", names(df), value = TRUE)[1L]
  if (is.na(actual_id)) actual_id <- NULL

  n_swap <- 0L
  n_inferred <- 0L
  swap_details <- data.frame()

  if (!is.null(actual_gender) && !is.null(actual_id)) {
    gender_vals <- trimws(as.character(df[[actual_gender]]))
    id_vals <- trimws(as.character(df[[actual_id]]))

    id_pattern <- "^[A-Za-z][0-9]{9}$"
    valid_gender_codes <- c("1", "2", "3", "男", "女")

    looks_like_id <- !is.na(gender_vals) & grepl(id_pattern, gender_vals)
    looks_like_gender <- !is.na(id_vals) & id_vals %in% valid_gender_codes

    # 情況 A：雙向填反（Swap）
    swap_mask <- looks_like_id & looks_like_gender
    n_swap <- sum(swap_mask, na.rm = TRUE)

    # 情況 B：身分證填到性別欄
    id_mis_mask <- looks_like_id & (is.na(id_vals) | id_vals == "")

    # 情況 C：性別碼填到身分證欄
    gender_mis_mask <- looks_like_gender & (is.na(gender_vals) | gender_vals == "")

    if (n_swap > 0L) {
      temp <- df[[actual_gender]][swap_mask]
      df[[actual_gender]][swap_mask] <- df[[actual_id]][swap_mask]
      df[[actual_id]][swap_mask] <- temp
    }

    if (sum(id_mis_mask, na.rm = TRUE) > 0L) {
      df[[actual_id]][id_mis_mask] <- df[[actual_gender]][id_mis_mask]
      df[[actual_gender]][id_mis_mask] <- NA_character_
    }

    if (sum(gender_mis_mask, na.rm = TRUE) > 0L) {
      df[[actual_gender]][gender_mis_mask] <- df[[actual_id]][gender_mis_mask]
      df[[actual_id]][gender_mis_mask] <- NA_character_
    }

    # 推導缺失性別 (身分證第2碼 1/8 -> 男(1), 2/9 -> 女(2))
    gender_vals <- trimws(as.character(df[[actual_gender]]))
    id_vals <- trimws(as.character(df[[actual_id]]))
    missing_gender <- is.na(gender_vals) | gender_vals == "" | !gender_vals %in% valid_gender_codes

    infer_mask <- missing_gender & !is.na(id_vals) & grepl("^[A-Za-z]([1289])", id_vals)
    n_inferred <- sum(infer_mask, na.rm = TRUE)

    if (n_inferred > 0L) {
      digits2 <- substr(id_vals[infer_mask], 2, 2)
      inferred <- ifelse(digits2 %in% c("1", "8"), "1", "2")
      df[[actual_gender]][infer_mask] <- inferred
    }
  }

  list(df = df, n_swap = n_swap, n_inferred = n_inferred)
}

# 6. 學校代碼與校名校對（去前綴模糊比對）
validate_school_reference <- function(df) {
  ref_path <- file.path("reference_data", "schools_reference.rds")
  n_matched <- 0L

  if (file.exists(ref_path) && "學校代碼" %in% names(df)) {
    schools_ref <- tryCatch(readRDS(ref_path), error = function(e) NULL)
    if (!is.null(schools_ref) && "code" %in% names(schools_ref)) {
      codes <- trimws(as.character(df$學校代碼))
      matched_mask <- codes %in% trimws(as.character(schools_ref$code))
      n_matched <- sum(matched_mask, na.rm = TRUE)
    }
  }

  list(df = df, n_matched = n_matched)
}

# 7. 同校代碼與名稱一致性檢查與自動投票修正
validate_school_code_consistency <- function(df) {
  n_corrected <- 0L
  if (all(c("學校代碼", "學校名稱") %in% names(df))) {
    school_name <- trimws(as.character(df$學校名稱))
    school_code <- trimws(as.character(df$學校代碼))

    valid_mask <- !is.na(school_name) & school_name != "" & !is.na(school_code) & school_code != ""
    if (any(valid_mask)) {
      stats <- aggregate(
        rep(1L, sum(valid_mask)),
        by = list(name = school_name[valid_mask], code = school_code[valid_mask]),
        FUN = length
      )
      colnames(stats) <- c("name", "code", "count")

      stats <- stats[order(stats$name, -stats$count), ]
      majority_map <- stats[!duplicated(stats$name), ]

      majority_codes <- setNames(majority_map$code, majority_map$name)
      expected_codes <- unname(majority_codes[school_name])

      inconsistent_mask <- valid_mask & !is.na(expected_codes) & school_code != expected_codes
      n_corrected <- sum(inconsistent_mask, na.rm = TRUE)

      if (n_corrected > 0L) {
        df$學校代碼[inconsistent_mask] <- expected_codes[inconsistent_mask]
      }
    }
  }
  list(df = df, n_corrected = n_corrected)
}

# 8. 依科目年級取得預期標準題數
get_expected_items <- function(subject, grade) {
  grade <- as.integer(grade)
  switch(
    as.character(subject),
    "C" = if (grade >= 3 && grade <= 6) 30L else if (grade >= 7 && grade <= 8) 35L else NA_integer_,
    "M" = if (grade >= 3 && grade <= 8) 25L else NA_integer_,
    "E" = if (grade >= 4 && grade <= 8) 35L else NA_integer_,
    "S" = if (grade == 5) 30L else if (grade >= 7 && grade <= 8) 35L else NA_integer_,
    NA_integer_
  )
}

# 9. 生成多頁籤 Excel 異常報告檔
generate_validation_report <- function(df, year = "115", county_name = "全區", output_file = tempfile(fileext = ".xlsx")) {
  sheets <- list()

  # A. 性別檢核異常
  if ("身分證字號" %in% names(df)) {
    id_bad <- !mapply(validate_taiwan_id_checksum, df$身分證字號) & !is.na(df$身分證字號) & df$身分證字號 != ""
    if (any(id_bad)) {
      sheets[["身分證格式異常"]] <- df[id_bad, , drop = FALSE]
    }
  }

  # B. 學校代碼異常
  ref_path <- file.path("reference_data", "schools_reference.rds")
  if (file.exists(ref_path) && "學校代碼" %in% names(df)) {
    schools_ref <- tryCatch(readRDS(ref_path), error = function(e) NULL)
    if (!is.null(schools_ref)) {
      unmatched <- !trimws(as.character(df$學校代碼)) %in% trimws(as.character(schools_ref$code))
      if (any(unmatched)) {
        sheets[["學校代碼異常"]] <- df[unmatched, , drop = FALSE]
      }
    }
  }

  if (length(sheets) > 0L) {
    openxlsx::write.xlsx(sheets, output_file, overwrite = TRUE)
    return(output_file)
  }
  NULL
}

# 10. 資料清洗總入口
clean_response_data <- function(df, options = list()) {
  # 處理南投雙層表頭
  df <- handle_nantou_raw_header(df)

  # 欄名標準化
  df <- standardise_column_names(df)

  log_messages <- character()

  # 同校代碼一致性修正
  res_consistency <- validate_school_code_consistency(df)
  df <- res_consistency$df
  if (res_consistency$n_corrected > 0L) {
    log_messages <- c(
      log_messages,
      sprintf("已對照多數決修正 %d 筆同校名稱但代碼不一致之資料。", res_consistency$n_corrected)
    )
  }

  # 特教整併
  if (isTRUE(options$consolidate_special %||% TRUE)) {
    res_special <- consolidate_special_students(df)
    df <- res_special$df
    if (res_special$count_fixed > 0L) {
      log_messages <- c(
        log_messages,
        sprintf("已自動將 %d 筆特教障礙類別整併為「特殊生」代碼。", res_special$count_fixed)
      )
    }
  }

  # 性別與身分證修復
  if (isTRUE(options$fix_gender %||% TRUE)) {
    res_gender <- fix_gender_and_id(df)
    df <- res_gender$df
    if (res_gender$n_swap > 0L) {
      log_messages <- c(
        log_messages,
        sprintf("已修復 %d 筆性別與身分證字號填錯格錯置。", res_gender$n_swap)
      )
    }
    if (res_gender$n_inferred > 0L) {
      log_messages <- c(
        log_messages,
        sprintf("已依身分證號第 2 碼自動補齊 %d 筆缺失性別。", res_gender$n_inferred)
      )
    }
  }

  # 學校對照
  if (isTRUE(options$match_school %||% TRUE)) {
    res_school <- validate_school_reference(df)
    df <- res_school$df
    if (res_school$n_matched > 0L) {
      log_messages <- c(
        log_messages,
        sprintf("已成功對照 %d 筆學校代碼（對照檔：schools_reference.rds）。", res_school$n_matched)
      )
    }
  }

  # 自動生成「縣市流水號」（作為 A 欄第一欄）
  if (isTRUE(options$gen_county_id %||% TRUE)) {
    city_col <- if ("縣市" %in% names(df)) "縣市" else if ("縣市名稱" %in% names(df)) "縣市名稱" else NULL
    if (!is.null(city_col)) {
      city_names <- trimws(as.character(df[[city_col]]))
      serial_nums <- seq_len(nrow(df))
      year_val <- options$year %||% "115"
      vol_val <- options$volume %||% "C4"
      df$縣市流水號 <- generate_county_id(
        year = year_val,
        city_name = city_names,
        volume = vol_val,
        serial_number = serial_nums
      )

      # 移至第一欄 Column A
      other_cols <- setdiff(names(df), "縣市流水號")
      df <- df[, c("縣市流水號", other_cols), drop = FALSE]

      log_messages <- c(
        log_messages,
        sprintf("已自動依縣市與序號生成 %d 筆「縣市流水號」（插入至 A 欄，如 %s）。", nrow(df), df$縣市流水號[[1L]])
      )
    }
  }

  # 自動分割
  split_res <- NULL
  if (isTRUE(options$split_by_subject %||% FALSE)) {
    split_res <- split_cleaned_data_by_subject_grade(df, year = options$year %||% "115")
    if (length(split_res$exported_files) > 0L) {
      log_messages <- c(
        log_messages,
        sprintf("已自動依科目與年級拆分為 %d 個分卷檔案（包含 %s）。", length(split_res$exported_files), paste(split_res$groups, collapse = ", "))
      )
    }
  }

  list(
    cleaned_df = df,
    logs = log_messages,
    split_result = split_res
  )
}

# 11. 依科目 (C, E, M, S) 與年級 (3~8) 將清洗後資料拆分並匯出多檔與 ZIP
split_cleaned_data_by_subject_grade <- function(df, year = "115", output_dir = tempfile("split-")) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  subject_col <- grep("科目", names(df), value = TRUE)[1L]
  grade_col <- grep("年級", names(df), value = TRUE)[1L]

  if (is.na(subject_col) || is.na(grade_col)) {
    return(list(groups = character(), exported_files = character(), zip_path = NULL))
  }

  subjects <- trimws(as.character(df[[subject_col]]))
  grades <- trimws(as.character(df[[grade_col]]))

  # 標準化科目代碼 N -> S
  subjects[subjects == "N"] <- "S"
  df[[subject_col]] <- subjects

  groups <- paste0(subjects, grades)
  valid_mask <- grepl("^[CEMS][3-8]$", groups)
  unique_groups <- sort(unique(groups[valid_mask]))

  exported_files <- character()

  for (grp in unique_groups) {
    sub_df <- df[groups == grp & !is.na(groups), , drop = FALSE]
    if (nrow(sub_df) == 0L) next

    # 檢查預期題數與實際題數，若多於預期題數則自動剪裁多餘題號欄
    subj_code <- substr(grp, 1, 1)
    grade_code <- substr(grp, 2, 2)
    exp_n <- get_expected_items(subj_code, grade_code)
    q_cols <- grep("^題號", names(sub_df), value = TRUE)

    if (!is.na(exp_n) && length(q_cols) > exp_n) {
      extra_cols <- q_cols[(exp_n + 1L):length(q_cols)]
      sub_df <- sub_df[, setdiff(names(sub_df), extra_cols), drop = FALSE]
    }

    # 生成縣市流水號（插入為第一欄 A 欄）
    city_col <- if ("縣市" %in% names(sub_df)) "縣市" else if ("縣市名稱" %in% names(sub_df)) "縣市名稱" else NULL
    city_name_val <- if (!is.null(city_col)) sub_df[[city_col]] else "全區"

    sub_df$縣市流水號 <- generate_county_id(
      year = year,
      city_name = city_name_val,
      volume = grp,
      serial_number = seq_len(nrow(sub_df))
    )

    sub_df$總流水號 <- sprintf("%s_%s_%06d", year, grp, seq_len(nrow(sub_df)))

    # 將「縣市流水號」明確移至第一欄 (Column A)
    other_cols <- setdiff(names(sub_df), "縣市流水號")
    sub_df <- sub_df[, c("縣市流水號", other_cols), drop = FALSE]

    out_name <- sprintf("%s_%s_合併.xlsx", year, grp)
    out_path <- file.path(output_dir, out_name)

    openxlsx::write.xlsx(sub_df, out_path, overwrite = TRUE)
    exported_files <- c(exported_files, out_path)
  }

  zip_path <- NULL
  if (length(exported_files) > 0L) {
    zip_path <- file.path(output_dir, sprintf("%s_依科目年級分割作答檔.zip", year))
    zip::zipr(zip_path, files = exported_files, root = output_dir)
  }

  list(
    groups = unique_groups,
    exported_files = exported_files,
    zip_path = zip_path
  )
}
