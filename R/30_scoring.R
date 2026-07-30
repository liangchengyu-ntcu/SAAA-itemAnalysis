# =============================================================================
# 檔案：R/30_scoring.R
# 用途：把已配對的答案檔與作答檔轉成可計算資料，完成逐題、向度及
#       個人層級的計分。
# 核心流程依序為：整理輸入、逐題計分、向度計分、組裝學生分數表。
# 修改提醒：
#   - 本檔包含已核對的計分契約；改答案判定、缺考或精度前要加測試。
#   - 計算以「學生向量」為單位，只有題目與向度使用迴圈，避免逐生迴圈。
#   - 彙總平均、排名與 PR 不在本檔，請至 R/40_summaries.R 修改。
# =============================================================================

# 讀取並整理單一工作的所有輸入，回傳後續計分共用的 prepared 物件。
#
# job：由 make_job() 建立的工作定義。
# 回傳值：包含學生資訊、有效題目作答矩陣、答案鍵、缺考旗標及向度標籤。
# 此步驟不計算分數，只負責驗證、對齊題目及建立穩定資料結構。
prepare_job_input <- function(job) {
  # 預覽階段可能已讀過 Excel；正式分析只重驗工作欄位與檔案存在性。
  validate_job_metadata(job)

  answer_tables <- read_answer_tables(job$answer_path)
  raw <- read_response_table(job$response_path)

  # 防禦性檢查：即使跳過預覽直接呼叫 analyze_job()，也不會錯位讀欄。
  if (nrow(raw) < 2L) {
    abort_score(basename(job$response_path), " 沒有學生資料列。")
  }
  if (ncol(raw) <= N_INFO_COLUMNS) {
    abort_score(
      sprintf(
        "%s 只有 %d 欄，至少需要 %d 個資訊欄位及 1 個作答欄位。",
        basename(job$response_path),
        ncol(raw),
        N_INFO_COLUMNS
      )
    )
  }

  headers <- as.character(raw[1, ])
  data <- raw[-1, , drop = FALSE]
  n_total <- nrow(data)
  n_items_raw <- ncol(data) - N_INFO_COLUMNS
  # 只解析前 23 欄學生資訊；後方欄位一律視為題目作答。
  info_columns <- resolve_info_columns(
    headers[seq_len(N_INFO_COLUMNS)]
  )

  info_data <- data[, seq_len(N_INFO_COLUMNS), drop = FALSE]
  colnames(info_data) <- headers[seq_len(N_INFO_COLUMNS)]

  student_ids <- trimws(
    as.character(info_data[, info_columns[["id"]]])
  )
  validate_student_ids(student_ids)

  # 強制轉為文字矩陣，確保 A/B/C/D 與數字型答案使用相同比較規則。
  item_data <- data[
    ,
    (N_INFO_COLUMNS + 1L):(N_INFO_COLUMNS + n_items_raw),
    drop = FALSE
  ]
  item_matrix <- as.matrix(item_data)
  item_matrix <- matrix(
    as.character(item_matrix),
    nrow = n_total,
    ncol = n_items_raw,
    dimnames = dimnames(item_matrix)
  )

  # 答案欄命名規則為科目代號加年級，例如國語四年級為 C4。
  answer_column <- paste0(job$subject_code, job$grade)
  if (!answer_column %in% colnames(answer_tables$answers)) {
    abort_score("答案表缺少欄位：", answer_column)
  }

  key_vector <- as.character(
    answer_tables$answers[[answer_column]]
  )
  # 答案鍵與作答題數不同時，以作答檔題數為外框；不足部分補 NA，
  # 多出的答案則截掉，避免索引超出作答矩陣。
  if (length(key_vector) > n_items_raw) {
    key_vector <- key_vector[seq_len(n_items_raw)]
  } else if (length(key_vector) < n_items_raw) {
    warn_score(
      sprintf(
        "%s 的答案鍵只有 %d 題，作答檔有 %d 題；缺少部分視為無答案鍵。",
        job$key,
        length(key_vector),
        n_items_raw
      )
    )
    key_vector <- c(
      key_vector,
      rep(NA_character_, n_items_raw - length(key_vector))
    )
  }

  valid_items <- which(
    !is.na(key_vector) & key_vector != ""
  )
  if (length(valid_items) == 0L) {
    abort_score(job$key, " 沒有任何有效答案題目。")
  }

  original_item_numbers <- valid_items
  # 無答案鍵的題目完全不參與總分、向度與缺考判定。
  key_vector <- key_vector[valid_items]
  item_matrix <- item_matrix[, valid_items, drop = FALSE]
  n_items <- length(valid_items)

  # 缺考定義：所有「有答案鍵的有效題目」均為 NA。
  # 部分漏答不算缺考；答錯也不算缺考。
  absent_flag <- rowSums(is.na(item_matrix)) == n_items

  # 各科答案檔的向度欄命名不同，在這裡集中轉成欲讀取的欄名。
  if (job$subject_code == "M") {
    # 數學有兩組向度定義，最後會並排合併。
    dimension_columns <- c(
      paste0(answer_column, "1_向度"),
      paste0(answer_column, "2_向度")
    )
  } else if (job$subject_code == "S") {
    dimension_columns <- paste0(answer_column, "向度")
  } else if (job$subject_code == "C") {
    # 國語向度欄使用「三年級」等中文年級，而不是 C3。
    chinese_grade <- unname(
      GRADE_TO_CHINESE[as.character(job$grade)]
    )
    if (is.na(chinese_grade)) {
      abort_score("國語向度沒有年級對照：", job$grade)
    }
    dimension_columns <- chinese_grade
  } else {
    dimension_columns <- answer_column
  }

  # 每組標籤都只保留有效題目，與 item_matrix、key_vector 完全對齊。
  dimension_labels <- lapply(dimension_columns, function(column_name) {
    get_dimension_labels(
      answer_tables$dimensions,
      column_name,
      n_items_raw,
      valid_items
    )
  })

  list(
    job = job,
    n_total = n_total,
    n_items_raw = n_items_raw,
    n_items = n_items,
    info_columns = info_columns,
    info_data = info_data,
    student_ids = student_ids,
    item_matrix = item_matrix,
    key_vector = key_vector,
    original_item_numbers = original_item_numbers,
    absent_flag = absent_flag,
    dimension_labels = dimension_labels
  )
}

# 將作答矩陣與答案鍵逐題比較，產生 1（答對）、0（答錯）或 NA。
#
# prepared：prepare_job_input() 的回傳值。
# 回傳值：列為學生、欄為有效題目的整數矩陣，欄名保留原題號 Q1、Q2。
# 答案鍵若使用頓號「、」，代表該題接受其中任一答案。
score_item_matrix <- function(prepared) {
  scored_matrix <- matrix(
    0L,
    nrow = prepared$n_total,
    ncol = prepared$n_items
  )

  for (item_index in seq_len(prepared$n_items)) {
    key <- prepared$key_vector[[item_index]]
    responses <- prepared$item_matrix[, item_index]

    if (grepl("、", key, fixed = TRUE)) {
      # 多重可接受答案範例：答案鍵「A、C」接受 A 或 C。
      options <- strsplit(key, "、", fixed = TRUE)[[1L]]
      scored_matrix[, item_index] <- ifelse(
        responses %in% options,
        1L,
        0L
      )
    } else {
      scored_matrix[, item_index] <- ifelse(
        responses == key,
        1L,
        0L
      )
    }
  }

  colnames(scored_matrix) <- paste0(
    "Q",
    prepared$original_item_numbers
  )
  scored_matrix
}

# 依答案檔向度標籤，計算每位學生各向度的答對率與答對題數。
#
# prepared：已整理輸入。
# scored_matrix：逐題 0/1/NA 得分矩陣。
# 回傳值：list(scores=答對率矩陣, counts=答對題數矩陣)。
# 若數學有兩組向度，兩組會依原順序合併成同一矩陣。
calculate_dimension_matrices <- function(
  prepared,
  scored_matrix
) {
  score_sets <- list()
  count_sets <- list()

  for (set_index in seq_along(prepared$dimension_labels)) {
    # 先正規化再分組，避免「修辭知識」與尾端多空白的
    # 「修辭知識 」被誤判成兩個不同向度。
    labels <- trimws(as.character(
      prepared$dimension_labels[[set_index]]
    ))
    dimension_names <- unique(
      labels[!is.na(labels) & labels != ""]
    )
    set_scores <- NULL
    set_counts <- NULL

    for (dimension_name in dimension_names) {
      # 找出本向度包含的題目，對所有學生一次做向量化 rowSums()。
      item_indices <- which(labels == dimension_name)
      if (length(item_indices) == 0L) {
        next
      }

      subset_scores <- scored_matrix[
        ,
        item_indices,
        drop = FALSE
      ]
      correct_counts <- rowSums(
        subset_scores,
        na.rm = TRUE
      )
      correct_rates <- score_ratio(
        correct_counts,
        length(item_indices)
      )

      set_scores <- cbind(set_scores, correct_rates)
      set_counts <- cbind(set_counts, correct_counts)
      colnames(set_scores)[ncol(set_scores)] <- dimension_name
      colnames(set_counts)[ncol(set_counts)] <- dimension_name
    }

    if (!is.null(set_scores)) {
      # 缺考者不能顯示 0 分，必須在向度答對率及題數中明確保留 NA。
      set_scores[prepared$absent_flag, ] <- NA
      set_counts[prepared$absent_flag, ] <- NA_integer_
    }
    score_sets[[set_index]] <- set_scores
    count_sets[[set_index]] <- set_counts
  }

  dimension_scores <- do.call(cbind, score_sets)
  dimension_counts <- do.call(cbind, count_sets)
  invalid_dimensions <- any(c(
    is.null(dimension_scores),
    is.null(dimension_counts),
    !is.null(dimension_scores) && ncol(dimension_scores) == 0L
  ))
  if (invalid_dimensions) {
    abort_score(prepared$job$key, " 沒有可計算的向度。")
  }

  # 移除向度名稱後方括號說明，保持輸出欄名簡潔。
  # 清理後若重名，使用 make.unique() 加上 _1、_2，避免覆蓋資料。
  clean_names <- trimws(
    gsub("（.*）", "", colnames(dimension_scores))
  )
  if (any(is.na(clean_names) | clean_names == "")) {
    abort_score(prepared$job$key, " 存在空白向度名稱。")
  }
  unique_names <- make.unique(clean_names, sep = "_")
  if (!identical(clean_names, unique_names)) {
    warn_score(
      prepared$job$key,
      " 的向度名稱清理後重複，已加上序號以避免欄位混淆。"
    )
  }
  colnames(dimension_scores) <- unique_names
  colnames(dimension_counts) <- unique_names

  list(
    scores = dimension_scores,
    counts = dimension_counts
  )
}

# 組裝每位學生的基本資料、總答對率、身分標記及所有向度答對率。
#
# prepared：已整理輸入。
# scored_matrix：逐題得分矩陣。
# dimension_matrices：向度答對率與答對題數。
# 回傳值：學生分數表及後續彙總會用到的輔助向量。
build_student_score_table <- function(
  prepared,
  scored_matrix,
  dimension_matrices
) {
  columns <- prepared$info_columns
  info <- prepared$info_data

  # 輸出欄「特殊生」採舊規則：非 NA 且不等於 0 即標為 TRUE。
  # 但平均排除與摘要「特殊生數」只使用代碼 1、2、3，
  # 該互斥分類規則位於 calculate_summary_tables()。
  special_values <- as.character(info[, columns[["special"]]])
  special_flag <- !is.na(special_values) & special_values != "0"

  aboriginal_values <- as.character(
    info[, columns[["aboriginal"]]]
  )
  immigrant_values <- as.character(
    info[, columns[["immigrant"]]]
  )
  aboriginal <- as.integer(
    !is.na(aboriginal_values) & aboriginal_values == "1"
  )
  immigrant <- as.integer(
    !is.na(immigrant_values) & immigrant_values == "1"
  )

  # 家庭背景四分類供「不同家庭背景平均」報表使用。
  family_type <- rep("一般生", prepared$n_total)
  family_type[aboriginal == 1L & immigrant == 0L] <- "原住民子女"
  family_type[aboriginal == 0L & immigrant == 1L] <- "新住民子女"
  family_type[aboriginal == 1L & immigrant == 1L] <- "原且新住民子女"

  total_correct_count <- rowSums(
    scored_matrix,
    na.rm = TRUE
  )
  total_correct_rate <- score_ratio(
    total_correct_count,
    prepared$n_items
  )
  # 全部未作答者的 rowSums() 原為 0，這裡改回 NA 才能區分缺考與零分。
  total_correct_rate[prepared$absent_flag] <- NA

  # 固定欄位順序同時供彙總、排名與 Excel 匯出使用；更名時要全域搜尋。
  student_scores <- data.frame(
    總流水號 = prepared$student_ids,
    縣市流水號 = as.character(info[, columns[["county_id"]]]),
    縣市 = info[, columns[["city"]]],
    鄉鎮區 = info[, columns[["district"]]],
    學校代碼 = as.character(info[, columns[["school_code"]]]),
    學校名稱 = info[, columns[["school_name"]]],
    班級代碼 = as.character(info[, columns[["class_code"]]]),
    座號 = as.integer(info[, columns[["seat_no"]]]),
    姓名 = info[, columns[["student_name"]]],
    總答對率 = total_correct_rate,
    缺考 = prepared$absent_flag,
    特殊生 = special_flag,
    原住民子女 = aboriginal,
    新住民子女 = immigrant,
    身分別 = family_type,
    stringsAsFactors = FALSE
  )

  conflicts <- intersect(
    colnames(dimension_matrices$scores),
    colnames(student_scores)
  )
  # 防止答案檔中的向度名稱覆蓋「總流水號」「總答對率」等固定欄位。
  if (length(conflicts) > 0L) {
    abort_score(
      "向度名稱與固定輸出欄位重複：",
      paste(conflicts, collapse = ", ")
    )
  }

  student_scores <- cbind(
    student_scores,
    dimension_matrices$scores
  )

  list(
    student_scores = student_scores,
    total_correct_count = total_correct_count,
    total_correct_rate = total_correct_rate,
    special_values = special_values,
    special_flag = special_flag,
    aboriginal = aboriginal,
    immigrant = immigrant,
    family_type = family_type
  )
}
