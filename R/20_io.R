# =============================================================================
# 檔案：R/20_io.R
# 用途：處理上傳檔暫存、ZIP 安全解壓、檔名解析、工作配對及 Excel 讀取。
# 資料流：Shiny 上傳資料 -> 暫存目錄 -> 工作清單 job -> 驗證／計分。
# 修改入口：
#   - 放寬或調整檔名：parse_response_filename()、parse_answer_filename()。
#   - 改變單科或批次配對：discover_single_jobs()、discover_batch_jobs()。
#   - 改變 Excel 讀法：read_answer_tables()、read_response_table()。
# 安全提醒：不可移除 safe_basename()、ZIP 路徑檢查或工作階段暫存設計。
# =============================================================================

# 將 Shiny fileInput() 的暫存檔複製到本次工作階段資料夾。
#
# upload_info：fileInput 回傳的 data.frame，至少含 name 與 datapath。
# destination：本次工作階段專用資料夾。
# 回傳值：依上傳順序排列的絕對路徑。
# 同批檔名不可重複，避免後一檔靜默覆蓋前一檔。
stage_uploaded_files <- function(upload_info, destination) {
  if (is.null(upload_info) || nrow(upload_info) == 0L) {
    abort_score("尚未選擇檔案。")
  }

  destination <- ensure_directory(destination)
  names_seen <- character()
  staged_paths <- character(nrow(upload_info))

  for (index in seq_len(nrow(upload_info))) {
    file_name <- safe_basename(upload_info$name[[index]])
    if (file_name %in% names_seen) {
      abort_score("上傳檔名重複：", file_name)
    }
    names_seen <- c(names_seen, file_name)

    target <- file.path(destination, file_name)
    copied <- file.copy(
      upload_info$datapath[[index]],
      target,
      overwrite = TRUE
    )
    if (!copied) {
      abort_score("無法暫存上傳檔：", file_name)
    }
    staged_paths[[index]] <- normalizePath(
      target,
      winslash = "/",
      mustWork = TRUE
    )
  }

  staged_paths
}

# 檢查 ZIP 內每一筆路徑，防止解壓到指定資料夾之外（Zip Slip）。
#
# 拒絕 Windows 磁碟機絕對路徑、Unix 絕對路徑及任何「..」路徑片段。
# 回傳 invisible(entries)，主要用途是驗證成功後繼續解壓。
validate_zip_entries <- function(zip_path) {
  entries <- utils::unzip(zip_path, list = TRUE)$Name
  if (length(entries) == 0L) {
    abort_score("ZIP 檔案是空的。")
  }

  normalized <- gsub("\\\\", "/", entries)
  unsafe <- grepl("^([A-Za-z]:|/)", normalized) |
    vapply(strsplit(normalized, "/", fixed = TRUE), function(parts) {
      any(parts == "..")
    }, logical(1))

  if (any(unsafe)) {
    abort_score(
      "ZIP 含有不安全路徑：",
      paste(head(entries[unsafe], 5L), collapse = ", ")
    )
  }

  invisible(entries)
}

# 先驗證 ZIP 項目，再解壓到工作階段專用資料夾。
# 回傳解壓目的地的正規化絕對路徑。
extract_zip_safely <- function(zip_path, destination) {
  validate_zip_entries(zip_path)
  destination <- ensure_directory(destination)
  utils::unzip(zip_path, exdir = destination)
  destination
}

# 從作答檔名解析年度、科目代號與年級。
#
# 可接受範例：115_C4.xlsx、115-C4-第一次匯出.xlsx。
# 固定規則：開頭必須是年度 + C/E/M/S + 3 至 8 年級；後綴可自訂。
# 無法辨識時回傳 NULL，辨識成功則回傳具名 list。
parse_response_filename <- function(path) {
  name <- basename(path)
  if (grepl("無效清單|無效", name)) {
    return(NULL)
  }
  pattern <- "^([0-9]+)[ _-]*([CEMS])([3-8]).*\\.(xlsx|csv)$"
  matched <- regexec(
    pattern,
    name,
    ignore.case = TRUE,
    perl = TRUE
  )
  parts <- regmatches(name, matched)[[1L]]
  if (length(parts) == 0L) {
    return(NULL)
  }

  list(
    year = parts[[2L]],
    subject_code = toupper(parts[[3L]]),
    grade = as.integer(parts[[4L]]),
    path = normalizePath(path, winslash = "/", mustWork = TRUE)
  )
}

# 從批次模式的答案檔名解析年度與科目。
#
# 可接受 ans、answer 或「答案」關鍵字，例如 115_C_ans.xlsx。
# 單科模式的答案檔不使用這項檔名限制，而是直接驗證內容。
parse_answer_filename <- function(path) {
  name <- basename(path)
  pattern <- paste0(
    "^([0-9]+)[ _-]*([CEMS])",
    "[ _-]*(?:ans|answer|答案).*\\.xlsx$"
  )
  matched <- regexec(
    pattern,
    name,
    ignore.case = TRUE,
    perl = TRUE
  )
  parts <- regmatches(name, matched)[[1L]]
  if (length(parts) == 0L) {
    return(NULL)
  }

  list(
    year = parts[[2L]],
    subject_code = toupper(parts[[3L]]),
    path = normalizePath(path, winslash = "/", mustWork = TRUE)
  )
}

# 建立全專案共用的單一工作定義。
#
# 工作物件同時保存穩定 key、顯示名稱與兩個輸入檔的絕對路徑。
# 後續驗證、分析、匯出及 Shiny 結果頁都使用相同結構。
make_job <- function(
  year,
  subject_code,
  grade,
  answer_path,
  response_path,
  calc_level = FALSE,
  mastery_cutoff = NA,
  basic_cutoff = NA
) {
  list(
    key = job_key(year, subject_code, grade),
    year = as.character(year),
    subject_code = as.character(subject_code),
    subject_name = unname(SUBJECT_NAMES[[subject_code]]),
    grade = as.integer(grade),
    answer_path = normalizePath(
      answer_path,
      winslash = "/",
      mustWork = TRUE
    ),
    response_path = normalizePath(
      response_path,
      winslash = "/",
      mustWork = TRUE
    ),
    calc_level = isTRUE(calc_level),
    mastery_cutoff = if (isTRUE(calc_level)) as.numeric(mastery_cutoff) else NA_real_,
    basic_cutoff = if (isTRUE(calc_level)) as.numeric(basic_cutoff) else NA_real_
  )
}

# 根據單科模式上傳檔建立一個或多個年級工作。
#
# answer_path：共用答案檔。
# response_paths：可含多個年級，但同一年級只允許一檔。
# year、subject_code：使用者在畫面選擇的年度及科目。
# 回傳值：依年級排序的 job 清單。
discover_single_jobs <- function(
  answer_path,
  response_paths,
  year,
  subject_code,
  calc_level = FALSE,
  mastery_cutoff = NA,
  basic_cutoff = NA
) {
  # 自動略過無效清單等輔助報告檔，只保留作答檔案
  is_aux <- grepl("無效清單|無效.*\\.xlsx$|report|summary", basename(response_paths), ignore.case = TRUE)
  if (any(!is_aux)) {
    response_paths <- response_paths[!is_aux]
  } else if (all(is_aux) && length(response_paths) > 0L) {
    abort_score(
      "您所選取的檔案「",
      paste(basename(response_paths), collapse = ", "),
      "」為無效資料名單報告檔，非作答檔。請選取資料夾中的「_analyzed.csv」或「.xlsx」學生作答檔案。"
    )
  }

  # 先逐檔解析；任何檔名無法辨識都整批退回，避免算錯科目。
  parsed <- lapply(response_paths, parse_response_filename)
  recognized <- !vapply(parsed, is.null, logical(1))
  if (!all(recognized)) {
    abort_score(
      paste0(
        "作答檔需以「年度_科目代號年級」開頭，",
        "後方名稱可自訂；無法辨識："
      ),
      paste(
        basename(response_paths[!recognized]),
        collapse = ", "
      )
    )
  }

  parsed <- parsed[recognized]
  # 檔名中的年度／科目必須與畫面選項一致。
  matching <- vapply(parsed, function(item) {
    identical(item$year, as.character(year)) &&
      identical(item$subject_code, subject_code)
  }, logical(1))

  if (!all(matching)) {
    abort_score(
      "單科模式的作答檔必須全部屬於 ",
      year,
      " ",
      subject_code,
      "。"
    )
  }

  # 若同一年級同時上傳了多個檔案（如 _analyzed.csv 與 .xlsx），優先採用 _analyzed.csv
  grades <- vapply(parsed, `[[`, integer(1), "grade")
  if (anyDuplicated(grades)) {
    deduped <- list()
    for (item in parsed) {
      g <- as.character(item$grade)
      if (is.null(deduped[[g]])) {
        deduped[[g]] <- item
      } else if (grepl("analyzed", item$path, ignore.case = TRUE)) {
        deduped[[g]] <- item
      }
    }
    parsed <- unname(deduped)
    grades <- vapply(parsed, `[[`, integer(1), "grade")
  }

  jobs <- lapply(parsed, function(item) {
    make_job(
      year = year,
      subject_code = subject_code,
      grade = item$grade,
      answer_path = answer_path,
      response_path = item$path,
      calc_level = calc_level,
      mastery_cutoff = mastery_cutoff,
      basic_cutoff = basic_cutoff
    )
  })
  jobs[order(grades)]
}

# 掃描批次 ZIP 解壓目錄，依年度與科目自動配對答案檔及作答檔。
#
# root：已安全解壓的根目錄，可包含子資料夾。
# year：只建立指定年度的工作。
# 回傳值：依科目代號、年級排序的 job 清單。
discover_batch_jobs <- function(
  root,
  year,
  calc_level = FALSE,
  mastery_cutoff = NA,
  basic_cutoff = NA
) {
  # 只掃描 .xlsx；其他文件即使存在 ZIP 內也不參與配對。
  files <- list.files(
    root,
    pattern = "\\.xlsx$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0L) {
    abort_score("批次 ZIP 中找不到 Excel 檔案。")
  }

  answer_info <- lapply(files, parse_answer_filename)
  response_info <- lapply(files, parse_response_filename)
  # NULL 代表檔名不符合該類型規則，在此從候選清單移除。
  answer_info <- answer_info[
    !vapply(answer_info, is.null, logical(1))
  ]
  response_info <- response_info[
    !vapply(response_info, is.null, logical(1))
  ]

  answer_info <- Filter(
    function(item) identical(item$year, as.character(year)),
    answer_info
  )
  response_info <- Filter(
    function(item) identical(item$year, as.character(year)),
    response_info
  )

  if (length(response_info) == 0L) {
    abort_score("批次 ZIP 中找不到指定年度的作答檔。")
  }

  answer_keys <- vapply(answer_info, function(item) {
    paste(item$year, item$subject_code, sep = "_")
  }, character(1))
  if (anyDuplicated(answer_keys)) {
    abort_score("同一年度與科目只能有一個答案檔。")
  }
  answer_map <- setNames(
    lapply(answer_info, `[[`, "path"),
    answer_keys
  )

  # 作答檔以「年度_科目年級」為唯一鍵，避免同一工作執行兩次。
  response_keys <- vapply(response_info, function(item) {
    job_key(item$year, item$subject_code, item$grade)
  }, character(1))
  if (anyDuplicated(response_keys)) {
    abort_score("同一科目與年級只能有一個作答檔。")
  }

  jobs <- lapply(response_info, function(item) {
    # 答案檔只需要配對年度與科目；同一答案檔可供多個年級使用。
    lookup <- paste(item$year, item$subject_code, sep = "_")
    answer_path <- answer_map[[lookup]]
    if (is.null(answer_path)) {
      abort_score(
        item$year,
        " ",
        item$subject_code,
        " 找不到對應答案檔。"
      )
    }
    make_job(
      year = item$year,
      subject_code = item$subject_code,
      grade = item$grade,
      answer_path = answer_path,
      response_path = item$path,
      calc_level = calc_level,
      mastery_cutoff = mastery_cutoff,
      basic_cutoff = basic_cutoff
    )
  })

  order_index <- order(
    vapply(jobs, `[[`, character(1), "subject_code"),
    vapply(jobs, `[[`, integer(1), "grade")
  )
  jobs[order_index]
}

# 在真正計算前逐一驗證工作，產生畫面上的「確認工作清單」。
#
# 單一工作驗證失敗只標記為「不可執行」，不會讓整張預覽表消失。
# 回傳欄位格式固定，讓空狀態與有資料狀態可共用同一個 renderTable。
preview_jobs <- function(jobs) {
  if (length(jobs) == 0L) {
    return(empty_job_table())
  }

  rows <- lapply(jobs, function(job) {
    validation <- tryCatch(
      {
        validate_job_spec(job)
        list(status = "可執行", message = "")
      },
      error = function(error) {
        list(status = "不可執行", message = compact_error(error))
      }
    )

    data.frame(
      工作代號 = job$key,
      科目 = job$subject_name,
      年級 = job$grade,
      狀態 = validation$status,
      訊息 = validation$message,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

# 一次讀取答案檔的「答案」與「向度」（或「評量指標」）工作表。
# skipEmptyRows=FALSE 很重要，因為空白答案列仍對應原始題號位置。
read_answer_tables <- function(path) {
  sheets <- openxlsx::getSheetNames(path)
  ans_sheet <- if ("答案" %in% sheets) "答案" else sheets[1L]
  dim_sheet <- find_dimension_sheet(sheets)

  list(
    answers = openxlsx::read.xlsx(
      path,
      sheet = ans_sheet,
      skipEmptyRows = FALSE
    ),
    dimensions = openxlsx::read.xlsx(
      path,
      sheet = dim_sheet,
      skipEmptyRows = FALSE
    )
  )
}

# 讀取作答檔第一個工作表，保留第一列原始標題供欄位解析使用。
# colNames=FALSE 表示不讓 openxlsx 自動把第一列當成 data.frame 欄名。
read_response_table <- function(path) {
  if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    return(data.table::fread(path, data.table = FALSE, header = FALSE, colClasses = "character"))
  }
  openxlsx::read.xlsx(
    path,
    sheet = 1,
    colNames = FALSE
  )
}
