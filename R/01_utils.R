# =============================================================================
# 檔案：R/01_utils.R
# 用途：提供各層都會使用的小型工具，包括錯誤處理、精度計算、
#       路徑處理、工作標籤與空表格。
# 修改提示：這些函式被多個檔案共用；變更前應先搜尋全部呼叫位置。
# =============================================================================

# 中止目前計分流程，並隱藏較不友善的 R 呼叫堆疊。
# `...` 會依序串接成錯誤訊息；call. 通常維持 FALSE。
abort_score <- function(..., call. = FALSE) {
  stop(..., call. = call.)
}

# 發出不會中止流程的警告，例如找不到欄名而回退至舊版欄位位置。
warn_score <- function(...) {
  warning(..., call. = FALSE)
}

# 解碼數字型 HTML 實體編碼 (例如 &#32317;&#27969;&#27700;&#34399; -> 總流水號)。
decode_html_entities <- function(x) {
  if (is.null(x)) return(x)
  vapply(as.character(x), function(s) {
    if (is.na(s) || s == "") return(s)
    matches <- gregexpr("&#([0-9]+);", s)
    if (matches[[1]][1] != -1) {
      reg_matches <- regmatches(s, matches)[[1]]
      for (m in reg_matches) {
        num <- as.integer(gsub("[^0-9]", "", m))
        if (!is.na(num)) {
          char_val <- intToUtf8(num)
          s <- gsub(m, char_val, s, fixed = TRUE)
        }
      }
    }
    s
  }, character(1), USE.NAMES = FALSE)
}

# 將 Excel 標題轉成便於比對的形式：自動解碼 HTML 實體、轉文字、去頭尾空白、移除所有空白。
normalize_header <- function(x) {
  decoded <- decode_html_entities(x)
  gsub("[[:space:]]+", "", trimws(as.character(decoded)))
}

# 統一所有彙總報表的顯示精度，並保留原向量名稱。
# 注意：這裡使用 R 的 round() 規則；不要改成 format()，否則會變文字。
round6 <- function(x) {
  result <- round(x, 6)
  names(result) <- names(x)
  result
}

# 動態識別答案檔中存放向度/評量指標的工作表名稱。
#
# 支援向度工作表別名：向度、評量指標、評量指標代、維度。
# 若無符合名稱，預設回傳第二張工作表（若存在）。
find_dimension_sheet <- function(sheets) {
  candidates <- c("向度", "評量指標", "評量指標代", "維度")
  for (cand in candidates) {
    if (cand %in% sheets) return(cand)
  }
  if (length(sheets) >= 2L) return(sheets[2L])
  NULL
}

# 動態判定學生資訊欄與題目作答欄的分界位置。
#
# 支援 23 欄標準格式（含總流水號與縣市流水號）及 21 欄簡化格式（直接從縣市欄位開始）。
# 亦支援 「item N」 格式的作答欄標題（如 item 1, item 2, ...）。
find_info_boundary <- function(headers) {
  headers_str <- as.character(headers)
  trimmed <- trimws(headers_str)

  # 1. 優先尋找第一個試題欄標題：
  #    - 純數字題號：'1', '2', ...
  #    - Q 前綴題號：'Q1', 'q1', ...
  #    - item 前綴題號：'item 1', 'item1', 'Item 1', ...
  first_num <- which(grepl("^([Qq]?[0-9]+|[Ii]tem\\s*[0-9]+)$", trimmed))
  if (length(first_num) > 0L && first_num[1L] > 5L) {
    return(first_num[1L] - 1L)
  }

  # 2. 尋找所有資訊欄位別名匹配到的最大索引位置
  all_aliases <- normalize_header(unlist(INFO_COLUMN_ALIASES))
  norm_headers <- normalize_header(headers_str)
  matched <- which(norm_headers %in% all_aliases)
  if (length(matched) > 0L) {
    return(max(matched))
  }

  23L
}

# 以 256 位元多重精度執行除法，保留舊版計分所需的精度行為。
bcdiv <- function(a, b) {
  Rmpfr::mpfr(a, 256) / Rmpfr::mpfr(b, 256)
}

# 截斷到小數點後 15 位；這是既有計分契約的一部分，不是一般四捨五入。
bcmul <- function(a, b) {
  floor(a * b * 1e15) / 1e15
}

# 將答對題數換算為答對率。
#
# numerator：一個或多個答對題數，可含 NA。
# denominator：單一且大於 0 的題數。
# 回傳值：與 numerator 等長的數值向量；NA 輸入會保留為 NaN。
#
# 效能說明：先對不重複的分子做 Rmpfr 運算，再查表還原至每位學生，
# 可避免對數萬名學生重複執行相同的高精度計算。
score_ratio <- function(numerator, denominator) {
  if (length(denominator) != 1L || is.na(denominator) || denominator <= 0) {
    abort_score("計分分母必須是大於 0 的單一數值。")
  }

  numerator <- as.numeric(numerator)
  valid_index <- !is.na(numerator)
  result <- rep(NaN, length(numerator))
  if (!any(valid_index)) {
    return(result)
  }

  # 只計算不同的答對題數，例如 0 至 30 題最多只有 31 種結果。
  unique_numerators <- sort(unique(numerator[valid_index]))
  ratio_lookup <- as.numeric(
    bcmul(
      bcdiv(unique_numerators, denominator),
      1
    )
  )
  result[valid_index] <- ratio_lookup[
    match(numerator[valid_index], unique_numerators)
  ]
  result
}

# 確保資料夾存在，並回傳使用正斜線的絕對路徑。
# 適用於工作階段暫存目錄及輸出目錄，不應傳入使用者未授權的位置。
ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

# 建立工作唯一鍵，例如 year=115、subject_code=C、grade=4 會得到 115_C4。
job_key <- function(year, subject_code, grade) {
  paste0(year, "_", subject_code, grade)
}

# 建立畫面使用的工作名稱，例如「115 國語 4 年級」。
format_job_label <- function(year, subject_code, grade) {
  paste0(
    year,
    " ",
    unname(SUBJECT_NAMES[[subject_code]]),
    " ",
    grade,
    " 年級"
  )
}

# 建立尚未檢查檔案時顯示的空工作表，先固定欄型以避免 Shiny 顯示錯誤。
empty_job_table <- function() {
  data.frame(
    工作代號 = character(),
    科目 = character(),
    年級 = integer(),
    狀態 = character(),
    訊息 = character(),
    stringsAsFactors = FALSE
  )
}

# 從完整路徑取得安全檔名，拒絕空值、上層目錄符號或夾帶路徑分隔符。
# 這是上傳檔案搬移與 ZIP 解壓縮流程的一道防護。
safe_basename <- function(path) {
  name <- basename(path)
  invalid_name <- any(c(
    is.na(name),
    !nzchar(name),
    name %in% c(".", ".."),
    grepl("[/\\\\]", name)
  ))
  if (invalid_name) {
    abort_score("不安全的檔名：", name)
  }
  name
}

# 將可能含換行的錯誤濃縮成單行，方便放入工作結果表的「訊息」欄。
compact_error <- function(error) {
  message <- conditionMessage(error)
  gsub("[\r\n]+", " ", message)
}
