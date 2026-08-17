# =============================================================================
# 檔案：R/55_ctt_exports.R
# 用途：經典測驗理論 (CTT) 試題分析與診斷 Excel 格式化美化匯出
# 包含：
#   1. write_distractor_analysis_xlsx()：產出「_分析結果.xlsx」（雙分頁）
#   2. write_ctt_analysis_by_subject()：產出「_試題分析總表.xlsx」（含色彩美化）
# =============================================================================

# 1. 寫出單一年級 CTT 分析結果 XLSX（雙分頁：逐選項明細 + 逐題摘要）
write_distractor_analysis_xlsx <- function(
  ctt_res,
  subject_label,
  grade,
  output_path
) {
  dist <- ctt_res$distractor_results
  item_summary <- ctt_res$item_summary
  n_items <- ctt_res$n_items

  wb <- openxlsx::createWorkbook()

  # Sheet 1: 逐選項明細 (distractor_results)
  sheet1_name <- paste0(subject_label, grade, " 分析結果")
  openxlsx::addWorksheet(wb, sheet1_name)
  openxlsx::writeData(wb, sheet1_name, dist, startRow = 1, colNames = TRUE)

  # Sheet 2: 逐題摘要
  openxlsx::addWorksheet(wb, "工作表1")
  openxlsx::writeData(wb, "工作表1", item_summary, startRow = 1, colNames = TRUE)

  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  output_path
}

# 2. 寫出單一工作表 CTT 格式化美化 Excel
write_ctt_analysis_sheet <- function(
  wb,
  sheet_name,
  ctt_res,
  subject_label,
  grade
) {
  if (is.null(ctt_res)) return()
  dist <- ctt_res$distractor_results
  opt_labels <- if (!is.null(ctt_res$opt_labels)) ctt_res$opt_labels else c("1", "2", "3", "4")
  n_total <- if (!is.null(ctt_res$n_total_valid)) ctt_res$n_total_valid else 0L
  n_items <- as.integer(if (!is.null(ctt_res$n_items)) ctt_res$n_items else if (!is.null(dist)) length(unique(dist$Item)) else 0L)
  key_vec <- ctt_res$key_vector
  alpha <- ctt_res$alpha

  n_opts <- length(opt_labels)
  n_cols <- 5L + 3L * (n_opts + 1L)
  data_row_start <- 4L
  data_row_end <- data_row_start + max(0L, n_items - 1L)
  avg_row <- data_row_end + 1L

  mat <- data.frame(
    matrix(NA_character_, nrow = n_items, ncol = n_cols),
    stringsAsFactors = FALSE
  )
  if (n_items > 0L) {
    mat[, 1] <- seq_len(n_items)
  }

  pass_rates <- numeric(n_items)
  diff_index <- numeric(n_items)
  disc_values <- numeric(n_items)
  upper_wrong_higher <- logical(n_items)
  overall_wrong_higher <- logical(n_items)

  unscored_items <- integer(0)

  for (item in seq_len(n_items)) {
    is_unscored <- !is.null(key_vec) && length(key_vec) >= item &&
      (is.na(key_vec[item]) || key_vec[item] == "" || is_unscored_key(key_vec[item]))

    if (is_unscored) {
      unscored_items <- c(unscored_items, item)
      mat[item, 2] <- "該題不予計分"
      mat[item, seq.int(3L, n_cols)] <- NA_character_
      pass_rates[item] <- NA_real_
      diff_index[item] <- NA_real_
      disc_values[item] <- NA_real_
      next
    }

    sub <- subset(dist, Item == paste0("題號", item))
    cr <- subset(sub, correct == "*")

    pass_rate <- if (nrow(cr) > 0) sum(cr$rspP, na.rm = TRUE) else NA_real_
    correct_key <- if (isTRUE(ctt_res$is_dichotomous) && !is.null(key_vec) && length(key_vec) >= item) {
      key_vec[item]
    } else if (nrow(cr) > 0) {
      paste(cr$key, collapse = "、")
    } else {
      NA_character_
    }
    hp <- if (nrow(cr) > 0) sum(cr$upper, na.rm = TRUE) else NA_real_
    lp <- if (nrow(cr) > 0) sum(cr$lower, na.rm = TRUE) else NA_real_
    disc_val <- if (!is.na(hp) && !is.na(lp)) (hp - lp) else NA_real_

    mat[item, 2] <- if (!is.na(disc_val)) sprintf("%.2f", round(disc_val, 2)) else NA_character_
    mat[item, 3] <- if (!is.na(hp) && !is.na(lp)) sprintf("%.2f", round((hp + lp) / 2, 2)) else NA_character_
    mat[item, 4] <- if (!is.na(pass_rate)) sprintf("%.2f", round(pass_rate, 2)) else NA_character_
    mat[item, 5] <- correct_key

    pass_rates[item] <- pass_rate
    diff_index[item] <- if (!is.na(hp) && !is.na(lp)) (hp + lp) / 2 else NA_real_
    disc_values[item] <- disc_val

    cr_max_upper <- if (nrow(cr) > 0) max(cr$upper, na.rm = TRUE) else NA_real_
    cr_max_rspP <- if (nrow(cr) > 0) max(cr$rspP, na.rm = TRUE) else NA_real_

    if (!is.na(cr_max_upper)) {
      for (k in seq_along(opt_labels)) {
        or <- subset(sub, key == opt_labels[k])
        if (nrow(or) > 0 && !is.na(or$upper[1]) && or$upper[1] > cr_max_upper) {
          upper_wrong_higher[item] <- TRUE
          break
        }
      }
    }
    if (!is.na(cr_max_rspP)) {
      for (k in seq_along(opt_labels)) {
        or <- subset(sub, key == opt_labels[k])
        if (nrow(or) > 0 && !is.na(or$rspP[1]) && or$rspP[1] > cr_max_rspP) {
          overall_wrong_higher[item] <- TRUE
          break
        }
      }
    }

    opt_start <- 6L
    for (k in seq_len(n_opts)) {
      opt_row <- subset(sub, key == opt_labels[k])
      if (nrow(opt_row) > 0) {
        mat[item, opt_start + k - 1L] <- sprintf("%.2f", round(opt_row$rspP, 2))
        mat[item, opt_start + (n_opts + 1L) + k - 1L] <- sprintf("%.2f", round(opt_row$upper, 2))
        mat[item, opt_start + 2L * (n_opts + 1L) + k - 1L] <- sprintf("%.2f", round(opt_row$lower, 2))
      }
    }
    other_row <- subset(sub, key == "9")
    if (nrow(other_row) > 0) {
      mat[item, opt_start + n_opts] <- sprintf("%.2f", round(other_row$rspP, 2))
      mat[item, opt_start + (n_opts + 1L) + n_opts] <- sprintf("%.2f", round(other_row$upper, 2))
      mat[item, opt_start + 2L * (n_opts + 1L) + n_opts] <- sprintf("%.2f", round(other_row$lower, 2))
    }
  }

  avg_pass <- round(mean(pass_rates, na.rm = TRUE), 2)
  avg_diff <- round(mean(diff_index, na.rm = TRUE), 2)

  title <- paste0(subject_label, grade, "年級_試題分析結果")
  avg_diff_str <- paste0("試題平均難度：", sprintf("%.2f%%", avg_diff * 100))
  avg_pass_str <- paste0("試題平均通過率：", sprintf("%.2f%%", avg_pass * 100))
  alpha_str <- if (!is.null(alpha) && !is.na(alpha)) paste0("Cronbach's α：", sprintf("%.2f", alpha)) else NA_character_
  n_total_str <- paste0("有效樣本數：", format(n_total, big.mark = ","))

  header1 <- c(
    title, rep(NA, 3),
    avg_diff_str, rep(NA, 3),
    avg_pass_str, rep(NA, 3),
    alpha_str, rep(NA, 3),
    n_total_str, rep(NA, max(0L, n_cols - 17L))
  )
  if (length(header1) < n_cols) {
    header1 <- c(header1, rep(NA, n_cols - length(header1)))
  } else if (length(header1) > n_cols) {
    header1 <- header1[seq_len(n_cols)]
  }
  openxlsx::writeData(wb, sheet_name, t(header1), startRow = 1, colNames = FALSE)

  header2 <- c(
    "題號", "CTT", NA,
    "全體", rep(NA, n_opts),
    "高分組", rep(NA, n_opts),
    "低分組", rep(NA, n_opts)
  )
  if (length(header2) < n_cols) {
    header2 <- c(header2, rep(NA, n_cols - length(header2)))
  } else if (length(header2) > n_cols) {
    header2 <- header2[seq_len(n_cols)]
  }
  openxlsx::writeData(wb, sheet_name, t(header2), startRow = 2, colNames = FALSE)

  header3 <- c(
    NA, "鑑別度", "難度", "通過率", "正確答案",
    opt_labels, "其他",
    opt_labels, "其他",
    opt_labels, "其他"
  )
  if (length(header3) < n_cols) {
    header3 <- c(header3, rep(NA, n_cols - length(header3)))
  } else if (length(header3) > n_cols) {
    header3 <- header3[seq_len(n_cols)]
  }
  openxlsx::writeData(wb, sheet_name, t(header3), startRow = 3, colNames = FALSE)

  if (n_items > 0L) {
    openxlsx::writeData(wb, sheet_name, mat, startRow = 4, colNames = FALSE, rowNames = FALSE)
  }

  mat_avg <- data.frame(matrix(NA_character_, nrow = 1, ncol = n_cols), stringsAsFactors = FALSE)
  mat_avg[1, 1] <- "平均"
  mat_avg[1, 3] <- sprintf("%.2f", avg_diff)
  mat_avg[1, 4] <- sprintf("%.2f", avg_pass)
  openxlsx::writeData(wb, sheet_name, mat_avg, startRow = avg_row, colNames = FALSE, rowNames = FALSE)

  notes_start <- avg_row + 2L
  notes <- data.frame(
    col1 = c("", "試題分析說明", "", "", "", ""),
    col2 = c(
      "",
      "1. 難度 = (高分組答對百分比 + 低分組答對百分比) / 2。",
      "2. 鑑別度 = 高分組答對百分比 - 低分組答對百分比。",
      "3. 題號黃底為高分組錯誤選項選答率高於正確選項。",
      "4. 題號灰底為全體錯誤選項選答率高於正確選項。",
      "5. 鑑別度未達 0.15 以紅字標記，表示鑑別度較低，需注意試題品質。"
    ),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, sheet_name, notes, startRow = notes_start, colNames = FALSE)

  # 合併儲存格
  openxlsx::mergeCells(wb, sheet_name, cols = 1:4, rows = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 5:8, rows = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 9:12, rows = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 13:16, rows = 1)
  if (n_cols >= 20L) {
    openxlsx::mergeCells(wb, sheet_name, cols = 17:n_cols, rows = 1)
  }
  openxlsx::mergeCells(wb, sheet_name, cols = 2:3, rows = 2)

  openxlsx::mergeCells(wb, sheet_name, cols = seq.int(4L, 5L + n_opts), rows = 2)

  c_high_from <- 6L + n_opts
  c_high_to <- c_high_from + n_opts
  openxlsx::mergeCells(wb, sheet_name, cols = seq.int(c_high_from, c_high_to), rows = 2)

  c_low_from <- c_high_to + 1L
  c_low_to <- c_low_from + n_opts
  openxlsx::mergeCells(wb, sheet_name, cols = seq.int(c_low_from, min(n_cols, c_low_to)), rows = 2)

  openxlsx::mergeCells(wb, sheet_name, cols = 1, rows = 2:3)

  # 樣式設定
  if (n_items > 0L) {
    data_rows <- seq.int(data_row_start, data_row_end)
    style2dec <- openxlsx::createStyle(numFmt = "0.00", halign = "center", border = "TopBottomLeftRight", borderStyle = "thin")
    openxlsx::addStyle(wb, sheet_name, style2dec, rows = data_rows, cols = c(2:3, seq.int(6L, n_cols)), gridExpand = TRUE)

    center_style <- openxlsx::createStyle(halign = "center", wrapText = TRUE, valign = "center", border = "TopBottomLeftRight", borderStyle = "thin")
    openxlsx::addStyle(wb, sheet_name, center_style, rows = data_rows, cols = c(1, 4, 5), gridExpand = TRUE)
    openxlsx::setRowHeights(wb, sheet_name, rows = data_rows, heights = 15)
  }

  green_header <- openxlsx::createStyle(fgFill = "#E2EFDA", halign = "center", wrapText = TRUE, valign = "center", border = "TopBottomLeftRight", borderStyle = "thin")
  openxlsx::addStyle(wb, sheet_name, green_header, rows = 1:3, cols = seq_len(n_cols), gridExpand = TRUE)

  header_center_style <- openxlsx::createStyle(halign = "center", wrapText = TRUE, valign = "center", border = "TopBottomLeftRight", borderStyle = "thin")
  openxlsx::addStyle(wb, sheet_name, header_center_style, rows = 1:3, cols = seq_len(n_cols), gridExpand = TRUE)

  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(n_cols), widths = 10)
  openxlsx::setRowHeights(wb, sheet_name, rows = 1:3, heights = 30)

  # 黃底標記正確選項
  if (n_items > 0L) {
    yellow_fill <- openxlsx::createStyle(fgFill = "#FFF2CC", numFmt = "0.00", halign = "center", border = "TopBottomLeftRight", borderStyle = "thin")
    for (item in seq_len(n_items)) {
      correct_val <- as.character(mat[item, 5])
      if (is.na(correct_val) || correct_val == "") next
      for (cv in strsplit(correct_val, "、")[[1]]) {
        opt_idx <- match(cv, opt_labels)
        if (is.na(opt_idx)) next
        openxlsx::addStyle(wb, sheet_name, yellow_fill, rows = item + 3L,
          cols = c(5L + opt_idx, 5L + (n_opts + 1L) + opt_idx, 5L + 2L * (n_opts + 1L) + opt_idx),
          gridExpand = TRUE)
      }
    }

    # 題號黃底與灰底異常標記
    id_yellow <- openxlsx::createStyle(fgFill = "#FFFF00", halign = "center", valign = "center", border = "TopBottomLeftRight", borderStyle = "thin", textDecoration = "bold")
    id_grey <- openxlsx::createStyle(fgFill = "#D9D9D9", halign = "center", valign = "center", border = "TopBottomLeftRight", borderStyle = "thin")
    for (i in seq_len(n_items)) {
      if (upper_wrong_higher[i]) {
        openxlsx::addStyle(wb, sheet_name, id_yellow, rows = i + 3L, cols = 1)
      } else if (overall_wrong_higher[i]) {
        openxlsx::addStyle(wb, sheet_name, id_grey, rows = i + 3L, cols = 1)
      }
    }

    # 鑑別度紅字
    disc_red_font <- openxlsx::createStyle(fontColour = "#FF0000", textDecoration = "bold", numFmt = "0.00", halign = "center", border = "TopBottomLeftRight", borderStyle = "thin")
    for (i in seq_len(n_items)) {
      if (!is.na(disc_values[i]) && disc_values[i] < 0.15) {
        openxlsx::addStyle(wb, sheet_name, disc_red_font, rows = i + 3L, cols = 2, gridExpand = TRUE)
      }
    }

    # 不計分題合併欄位
    unscored_style <- openxlsx::createStyle(halign = "center", valign = "center", textDecoration = "bold")
    for (item in unscored_items) {
      item_row <- item + 3L
      openxlsx::mergeCells(wb, sheet_name, cols = seq.int(2L, n_cols), rows = item_row)
      openxlsx::addStyle(wb, sheet_name, unscored_style, rows = item_row, cols = 2)
    }
  }

  red_bold_style <- openxlsx::createStyle(textDecoration = "bold", fontColour = "#FF0000")
  openxlsx::addStyle(wb, sheet_name, red_bold_style, rows = c(notes_start + 1L, notes_start + 7L), cols = 1)
}

# 3. 寫出整併之 CTT 試題分析總表 Excel（包含多個年級 Sheet）
write_ctt_analysis_by_subject <- function(
  subject_label,
  year,
  output_path,
  grade_ctt_map
) {
  wb <- openxlsx::createWorkbook()
  for (grd in names(grade_ctt_map)) {
    ctt_res <- grade_ctt_map[[grd]]
    sheet_name <- paste0(grd, "年級")
    openxlsx::addWorksheet(wb, sheet_name)
    write_ctt_analysis_sheet(
      wb,
      sheet_name,
      ctt_res,
      subject_label,
      grd
    )
  }
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  output_path
}

# 4. 寫出縣市標準三等級 (精熟/基礎/待加強) 試題分析 Excel（完全對齊簡報截圖美學格式）
write_level_ctt_excel <- function(
  level_ctt_res,
  subject_label,
  grade,
  year,
  output_path
) {
  if (is.null(level_ctt_res)) {
    stop("level_ctt_res is NULL")
  }
  mat <- level_ctt_res$level_summary_table
  n_items <- as.integer(if (!is.null(level_ctt_res$n_items)) level_ctt_res$n_items else if (!is.null(mat)) nrow(mat) else 0L)
  opt_labels <- if (!is.null(level_ctt_res$opt_labels)) level_ctt_res$opt_labels else c("1", "2", "3", "4")
  n_opts <- length(opt_labels)
  n_cols <- 4L + 4L * (n_opts + 1L)

  wb <- openxlsx::createWorkbook()
  sheet_name <- paste0(grade, "年級")
  openxlsx::addWorksheet(wb, sheet_name)

  title <- paste0(year, "年度", subject_label, grade, "年級 試題分析結果")
  header1 <- c(title, rep(NA, n_cols - 1L))
  openxlsx::writeData(wb, sheet_name, t(header1), startRow = 1, colNames = FALSE)

  header2 <- c(
    "題號", "鑑別度", "通過率", "正確答案",
    "全體", rep(NA, n_opts),
    "精熟", rep(NA, n_opts),
    "基礎", rep(NA, n_opts),
    "待加強", rep(NA, n_opts)
  )
  openxlsx::writeData(wb, sheet_name, t(header2), startRow = 2, colNames = FALSE)

  header3 <- c(
    NA, NA, NA, NA,
    opt_labels, "其它",
    opt_labels, "其它",
    opt_labels, "其它",
    opt_labels, "其它"
  )
  openxlsx::writeData(wb, sheet_name, t(header3), startRow = 3, colNames = FALSE)

  if (!is.null(mat) && nrow(mat) > 0L) {
    openxlsx::writeData(wb, sheet_name, mat, startRow = 4, colNames = FALSE, rowNames = FALSE)
  }

  # 合併儲存格
  openxlsx::mergeCells(wb, sheet_name, cols = seq_len(n_cols), rows = 1)
  openxlsx::mergeCells(wb, sheet_name, cols = 1, rows = 2:3)
  openxlsx::mergeCells(wb, sheet_name, cols = 2, rows = 2:3)
  openxlsx::mergeCells(wb, sheet_name, cols = 3, rows = 2:3)
  openxlsx::mergeCells(wb, sheet_name, cols = 4, rows = 2:3)

  col_start <- 5L
  for (grp in 1:4) {
    c_from <- col_start + (grp - 1L) * (n_opts + 1L)
    c_to <- c_from + n_opts
    openxlsx::mergeCells(wb, sheet_name, cols = seq.int(c_from, c_to), rows = 2)
  }

  # 美化樣式 (對齊簡報截圖: #E2EFDA 綠色表頭, 置中與 border)
  green_header <- openxlsx::createStyle(
    fgFill = "#E2EFDA",
    halign = "center",
    valign = "center",
    wrapText = TRUE,
    textDecoration = "bold",
    border = "TopBottomLeftRight",
    borderStyle = "thin"
  )
  openxlsx::addStyle(wb, sheet_name, green_header, rows = 1:3, cols = seq_len(n_cols), gridExpand = TRUE)

  if (n_items > 0L) {
    data_rows <- seq.int(4L, 3L + n_items)
    style_num <- openxlsx::createStyle(numFmt = "0.00", halign = "center", border = "TopBottomLeftRight", borderStyle = "thin")
    openxlsx::addStyle(wb, sheet_name, style_num, rows = data_rows, cols = c(2:3, seq.int(5L, n_cols)), gridExpand = TRUE)

    style_center <- openxlsx::createStyle(halign = "center", border = "TopBottomLeftRight", borderStyle = "thin")
    openxlsx::addStyle(wb, sheet_name, style_center, rows = data_rows, cols = c(1, 4), gridExpand = TRUE)

    openxlsx::setRowHeights(wb, sheet_name, rows = data_rows, heights = 18)
  }

  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(n_cols), widths = 8)
  openxlsx::setRowHeights(wb, sheet_name, rows = 1:3, heights = 25)

  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  output_path
}
