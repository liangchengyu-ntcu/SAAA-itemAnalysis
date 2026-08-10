# =============================================================================
# 檔案：R/40_summaries.R
# 用途：由個人分數建立平均表、排名、PR、個人明細及分數分布。
# 重要規則：
#   1. 平均只使用「到考」：非缺考且特殊生代碼不是 1、2、3。
#   2. 學生數 = 到考數 + 缺考數 + 特殊生，三類互斥。
#   3. 排名母體是所有非缺考者，因此包含特殊生。
#   4. 所有彙總答對率統一保留小數點後 6 位。
# 修改提醒：平均母體與排名母體刻意不同，修改其中之一時不要順手改另一個。
# =============================================================================

# 從指定欄開始，將 data.frame 後方所有欄位套用 round6()。
#
# data：待處理報表。
# first_numeric_column：第一個數值欄的 1-based 位置。
# 回傳值：欄位順序不變、數值已統一精度的 data.frame。
round_table_columns <- function(data, first_numeric_column, digits = 6) {
  if (ncol(data) < first_numeric_column) {
    return(data)
  }
  for (column_index in seq.int(first_numeric_column, ncol(data))) {
    data[, column_index] <- round_half_up(data[, column_index], digits)
  }
  data
}

# 計算總體、縣市、學校、班級、區域與家庭背景平均。
#
# prepared：prepare_job_input() 的結果。
# dimension_matrices：向度答對率／題數。
# score_data：build_student_score_table() 的結果。
# 回傳值：平均報表、缺考名單、三類人員索引及分數欄位置。
calculate_summary_tables <- function(
  prepared,
  dimension_matrices,
  score_data
) {
  student_scores <- score_data$student_scores
  # 「特殊生」摘要與平均排除規則只認代碼 1、2、3。
  is_special_123 <- !is.na(score_data$special_values) &
    score_data$special_values %in% c("1", "2", "3")

  # 三類採互斥設計：先判定缺考，非缺考者才可能歸為特殊生。
  # valid_index 即畫面上的「到考數」及所有平均的母體。
  special_index <- !prepared$absent_flag & is_special_123
  valid_index <- !prepared$absent_flag & !special_index

  if (!any(valid_index)) {
    abort_score(
      prepared$job$key,
      " 沒有可用於平均計算的到考學生。"
    )
  }

  # 計算所有到考學生的三等級標籤（1. 扣除特殊生；2. 含特殊生）
  n_scored <- length(prepared$key_vector)
  m_cut <- if (!is.null(prepared$job$mastery_cutoff) && !is.na(prepared$job$mastery_cutoff)) prepared$job$mastery_cutoff else ceiling(n_scored * 0.75)
  b_cut <- if (!is.null(prepared$job$basic_cutoff) && !is.na(prepared$job$basic_cutoff)) prepared$job$basic_cutoff else ceiling(n_scored * 0.50)

  # 1. 扣除特殊生之等級
  valid_correct_counts <- score_data$total_correct_count[valid_index]
  valid_levels <- rep("待加強", length(valid_correct_counts))
  valid_levels[valid_correct_counts >= b_cut] <- "基礎"
  valid_levels[valid_correct_counts >= m_cut] <- "精熟"

  # 2. 含特殊生之等級（全體到考非缺考者）
  all_valid_index <- !prepared$absent_flag
  all_valid_correct_counts <- score_data$total_correct_count[all_valid_index]
  all_valid_levels <- rep("待加強", length(all_valid_correct_counts))
  all_valid_levels[all_valid_correct_counts >= b_cut] <- "基礎"
  all_valid_levels[all_valid_correct_counts >= m_cut] <- "精熟"

  dimension_start <- ncol(student_scores) -
    ncol(dimension_matrices$scores) + 1L
  score_columns <- c(
    match("總答對率", colnames(student_scores)),
    seq.int(dimension_start, ncol(student_scores))
  )

  overall_mean <- round6(
    colMeans(
      student_scores[
        valid_index,
        score_columns,
        drop = FALSE
      ],
      na.rm = TRUE
    )
  )

  # 全體到考包含特殊生之整體平均
  overall_mean_all <- round6(
    colMeans(
      student_scores[
        all_valid_index,
        score_columns,
        drop = FALSE
      ],
      na.rm = TRUE
    )
  )

  valid_data <- student_scores[valid_index, , drop = FALSE]
  all_valid_data <- student_scores[all_valid_index, , drop = FALSE]

  # 1. 既有平均報表：維持原有向度平均計算（乾淨簡潔，無等級塞欄）
  county_means <- aggregate(
    valid_data[, score_columns, drop = FALSE],
    list(縣市 = valid_data$縣市),
    mean,
    na.rm = TRUE
  )
  county_means <- round_table_columns(county_means, 2L)

  school_means <- aggregate(
    valid_data[, score_columns, drop = FALSE],
    list(
      學校代碼 = valid_data$學校代碼,
      學校名稱 = valid_data$學校名稱
    ),
    mean,
    na.rm = TRUE
  )
  school_means <- round_table_columns(school_means, 3L)

  class_means <- aggregate(
    valid_data[, score_columns, drop = FALSE],
    list(
      學校代碼 = valid_data$學校代碼,
      班級代碼 = valid_data$班級代碼
    ),
    mean,
    na.rm = TRUE
  )
  class_means <- round_table_columns(class_means, 3L)

  region_means <- aggregate(
    valid_data[, score_columns, drop = FALSE],
    list(
      縣市 = valid_data$縣市,
      鄉鎮區 = valid_data$鄉鎮區
    ),
    mean,
    na.rm = TRUE
  )
  region_means <- round_table_columns(region_means, 3L, digits = 14)
  if ("總答對率" %in% colnames(region_means)) {
    region_means$總答對率 <- round_half_up(region_means$總答對率, 2)
  }

  family_means <- aggregate(
    valid_data[, score_columns, drop = FALSE],
    list(
      縣市 = valid_data$縣市,
      身分別 = valid_data$身分別
    ),
    mean,
    na.rm = TRUE
  )
  family_means <- round_table_columns(family_means, 3L, digits = 14)
  if ("總答對率" %in% colnames(family_means)) {
    family_means$總答對率 <- round_half_up(family_means$總答對率, 2) * 100
  }

  # 2. 併表等級描述報表聚合函式：在同一張表內併列呈現【扣除特殊生】與【含特殊生】
  # 欄位順序：[分組標籤] | 到考人數(扣除特殊生) | 總答對率(扣除特殊生) | 精熟人數(扣除特殊生) | 精熟率(%)(扣除特殊生)... | 到考人數(含特殊生)...
  build_combined_level_description_table <- function(
    valid_data, valid_levels,
    all_valid_data, all_valid_levels,
    group_by_list, group_by_list_all,
    first_num_col,
    digits = 6L,
    round_total_digits = NULL,
    scale_100 = FALSE
  ) {
    group_keys <- names(group_by_list)

    # A. 扣除特殊生
    means1 <- aggregate(valid_data[, "總答對率", drop = FALSE], group_by_list, mean, na.rm = TRUE)
    dt1 <- data.table::as.data.table(c(group_by_list, list(level = valid_levels)))
    stats1 <- dt1[
      ,
      list(
        `到考人數(扣除特殊生)` = .N,
        `精熟人數(扣除特殊生)` = sum(level == "精熟", na.rm = TRUE),
        `精熟率(%)(扣除特殊生)` = round6(sum(level == "精熟", na.rm = TRUE) / .N * 100),
        `基礎人數(扣除特殊生)` = sum(level == "基礎", na.rm = TRUE),
        `基礎率(%)(扣除特殊生)` = round6(sum(level == "基礎", na.rm = TRUE) / .N * 100),
        `待加強人數(扣除特殊生)` = sum(level == "待加強", na.rm = TRUE),
        `待加強率(%)(扣除特殊生)` = round6(sum(level == "待加強", na.rm = TRUE) / .N * 100)
      ),
      by = group_keys
    ]
    m1 <- merge(stats1, means1, by = group_keys, sort = FALSE)
    colnames(m1)[colnames(m1) == "總答對率"] <- "總答對率(扣除特殊生)"

    # B. 含特殊生
    means2 <- aggregate(all_valid_data[, "總答對率", drop = FALSE], group_by_list_all, mean, na.rm = TRUE)
    dt2 <- data.table::as.data.table(c(group_by_list_all, list(level = all_valid_levels)))
    stats2 <- dt2[
      ,
      list(
        `到考人數(含特殊生)` = .N,
        `精熟人數(含特殊生)` = sum(level == "精熟", na.rm = TRUE),
        `精熟率(%)(含特殊生)` = round6(sum(level == "精熟", na.rm = TRUE) / .N * 100),
        `基礎人數(含特殊生)` = sum(level == "基礎", na.rm = TRUE),
        `基礎率(%)(含特殊生)` = round6(sum(level == "基礎", na.rm = TRUE) / .N * 100),
        `待加強人數(含特殊生)` = sum(level == "待加強", na.rm = TRUE),
        `待加強率(%)(含特殊生)` = round6(sum(level == "待加強", na.rm = TRUE) / .N * 100)
      ),
      by = group_keys
    ]
    m2 <- merge(stats2, means2, by = group_keys, sort = FALSE)
    colnames(m2)[colnames(m2) == "總答對率"] <- "總答對率(含特殊生)"

    base_groups <- as.data.frame(group_by_list_all, stringsAsFactors = FALSE)
    base_groups <- unique(base_groups)

    res <- merge(base_groups, m1, by = group_keys, all.x = TRUE, sort = FALSE)
    res <- merge(res, m2, by = group_keys, all.x = TRUE, sort = FALSE)

    cols_order <- c(
      group_keys,
      "到考人數(扣除特殊生)",
      "總答對率(扣除特殊生)",
      "精熟人數(扣除特殊生)",
      "精熟率(%)(扣除特殊生)",
      "基礎人數(扣除特殊生)",
      "基礎率(%)(扣除特殊生)",
      "待加強人數(扣除特殊生)",
      "待加強率(%)(扣除特殊生)",
      "到考人數(含特殊生)",
      "總答對率(含特殊生)",
      "精熟人數(含特殊生)",
      "精熟率(%)(含特殊生)",
      "基礎人數(含特殊生)",
      "基礎率(%)(含特殊生)",
      "待加強人數(含特殊生)",
      "待加強率(%)(含特殊生)"
    )
    res_df <- as.data.frame(res, stringsAsFactors = FALSE)[, cols_order, drop = FALSE]
    res_df <- round_table_columns(res_df, first_num_col, digits = digits)
    if (!is.null(round_total_digits)) {
      mult <- if (scale_100) 100 else 1
      if ("總答對率(扣除特殊生)" %in% colnames(res_df)) {
        res_df$`總答對率(扣除特殊生)` <- round_half_up(res_df$`總答對率(扣除特殊生)`, round_total_digits) * mult
      }
      if ("總答對率(含特殊生)" %in% colnames(res_df)) {
        res_df$`總答對率(含特殊生)` <- round_half_up(res_df$`總答對率(含特殊生)`, round_total_digits) * mult
      }
    }
    res_df
  }

  county_level_means <- build_combined_level_description_table(
    valid_data, valid_levels,
    all_valid_data, all_valid_levels,
    list(縣市 = valid_data$縣市),
    list(縣市 = all_valid_data$縣市), 2L
  )

  school_level_means <- build_combined_level_description_table(
    valid_data, valid_levels,
    all_valid_data, all_valid_levels,
    list(學校代碼 = valid_data$學校代碼, 學校名稱 = valid_data$學校名稱),
    list(學校代碼 = all_valid_data$學校代碼, 學校名稱 = all_valid_data$學校名稱), 3L
  )

  class_level_means <- build_combined_level_description_table(
    valid_data, valid_levels,
    all_valid_data, all_valid_levels,
    list(學校代碼 = valid_data$學校代碼, 班級代碼 = valid_data$班級代碼),
    list(學校代碼 = all_valid_data$學校代碼, 班級代碼 = all_valid_data$班級代碼), 3L
  )

  region_level_means <- build_combined_level_description_table(
    valid_data, valid_levels,
    all_valid_data, all_valid_levels,
    list(縣市 = valid_data$縣市, 鄉鎮區 = valid_data$鄉鎮區),
    list(縣市 = all_valid_data$縣市, 鄉鎮區 = all_valid_data$鄉鎮區), 3L,
    digits = 14L,
    round_total_digits = 2L
  )

  family_level_means <- build_combined_level_description_table(
    valid_data, valid_levels,
    all_valid_data, all_valid_levels,
    list(縣市 = valid_data$縣市, 身分別 = valid_data$身分別),
    list(縣市 = all_valid_data$縣市, 身分別 = all_valid_data$身分別), 3L,
    digits = 14L,
    round_total_digits = 2L,
    scale_100 = TRUE
  )

  t_n <- length(valid_levels)
  m_cnt <- sum(valid_levels == "精熟", na.rm = TRUE)
  b_cnt <- sum(valid_levels == "基礎", na.rm = TRUE)
  i_cnt <- sum(valid_levels == "待加強", na.rm = TRUE)

  total_stats <- list(
    到考人數 = t_n,
    精熟人數 = m_cnt,
    `精熟率(%)` = round6(m_cnt / t_n * 100),
    基礎人數 = b_cnt,
    `基礎率(%)` = round6(b_cnt / t_n * 100),
    待加強人數 = i_cnt,
    `待加強率(%)` = round6(i_cnt / t_n * 100)
  )

  t_n_all <- length(all_valid_levels)
  m_cnt_all <- sum(all_valid_levels == "精熟", na.rm = TRUE)
  b_cnt_all <- sum(all_valid_levels == "基礎", na.rm = TRUE)
  i_cnt_all <- sum(all_valid_levels == "待加強", na.rm = TRUE)

  total_stats_all <- list(
    到考人數 = t_n_all,
    精熟人數 = m_cnt_all,
    `精熟率(%)` = round6(m_cnt_all / t_n_all * 100),
    基礎人數 = b_cnt_all,
    `基礎率(%)` = round6(b_cnt_all / t_n_all * 100),
    待加強人數 = i_cnt_all,
    `待加強率(%)` = round6(i_cnt_all / t_n_all * 100)
  )

  absent_list <- student_scores[
    prepared$absent_flag,
    c(
      "總流水號",
      "縣市流水號",
      "縣市",
      "學校代碼",
      "學校名稱",
      "班級代碼",
      "座號",
      "姓名",
      "特殊生",
      "身分別"
    ),
    drop = FALSE
  ]

  list(
    valid_index = valid_index,
    special_index = special_index,
    score_columns = score_columns,
    overall_mean = overall_mean,
    overall_mean_all = overall_mean_all,
    total_stats = total_stats,
    total_stats_all = total_stats_all,
    county_means = county_means,
    school_means = school_means,
    class_means = class_means,
    region_means = region_means,
    family_means = family_means,
    county_level_means = county_level_means,
    school_level_means = school_level_means,
    class_level_means = class_level_means,
    region_level_means = region_level_means,
    family_level_means = family_level_means,
    absent_list = absent_list
  )
}

# 建立排名、PR、三種個人報表、含缺考完整名單及總平均單列報表。
#
# 排名規則：
#   - 母體為所有非缺考者，包含特殊生。
#   - 同分採 ties.method="max"；由高分往低分換算名次。
#   - PR = floor((由低至高名次 - 1) / 母體人數 * 100)。
#   - 缺考者的排名與 PR 一律為 NA。
# 回傳值：個人成績、個人成績含題數、全體名單及總平均等匯出表。
calculate_ranked_outputs <- function(
  prepared,
  scored_matrix,
  dimension_matrices,
  score_data,
  summaries
) {
  student_scores <- score_data$student_scores
  non_absent <- !prepared$absent_flag
  scores_for_rank <- score_data$total_correct_rate[non_absent]

  # base::rank() 先由低到高排名，再反轉成「最高分第 1 名」。
  rank_ascending <- rank(
    scores_for_rank,
    ties.method = "max",
    na.last = "keep"
  )
  rank_all <- length(scores_for_rank) + 1L - rank_ascending
  pr_all <- floor(
    (rank_ascending - 1L) /
      length(scores_for_rank) *
      100
  )

  personal <- student_scores[non_absent, , drop = FALSE]
  personal$全體排名 <- rank_all
  personal$全體PR <- pr_all

  # data.table 依縣市分組計算，避免對每個縣市手動迴圈及切表。
  personal_table <- data.table::as.data.table(personal)
  personal_table[
    ,
    c("縣市排名", "縣市PR") := {
      rank_city <- data.table::frank(
        總答對率,
        ties.method = "max",
        na.last = "keep"
      )
      list(
        .N + 1L - rank_city,
        floor((rank_city - 1L) / .N * 100)
      )
    },
    by = 縣市
  ]
  personal_all <- as.data.frame(personal_table)

  # 將缺考者接回完整名單，但排名欄保持 NA。
  # 在 R >= 4.6 中對零列 data.frame 使用 $<- 會失敗，因此先檢查是否有缺考者。
  if (any(prepared$absent_flag)) {
    absent_rows <- student_scores[
      prepared$absent_flag,
      ,
      drop = FALSE
    ]
    absent_rows$全體排名 <- NA
    absent_rows$全體PR <- NA
    absent_rows$縣市排名 <- NA
    absent_rows$縣市PR <- NA
    personal_all <- rbind(personal_all, absent_rows)
  }

  # 「全體名單成績含缺考」也保留答對題數，方便單一檔案完整查核；
  # 另依作業需求同時輸出下方的「個人成績含題數」相容格式。
  personal_all$答對題數 <- score_data$total_correct_count[
    match(personal_all$總流水號, prepared$student_ids)
  ]

  if (isTRUE(prepared$job$calc_level)) {
    levels <- calculate_student_levels(
      correct_counts = score_data$total_correct_count,
      absent_flag = prepared$absent_flag,
      calc_level = prepared$job$calc_level,
      mastery_cutoff = prepared$job$mastery_cutoff,
      basic_cutoff = prepared$job$basic_cutoff
    )
    personal_all$等級 <- levels[match(personal_all$總流水號, prepared$student_ids)]
  }

  # 「個人成績含題數」沿用舊版欄位契約：
  # 保留全體排名／PR，但不放縣市排名／PR，最後一欄為答對題數。
  personal_with_count <- personal_all
  personal_with_count$縣市排名 <- NULL
  personal_with_count$縣市PR <- NULL

  # 詳細「個人成績」需要把排名後的名單順序對回原始上傳資料。
  # 總流水號在輸入驗證階段已確認唯一，因此可安全作為對齊鍵。
  original_index <- match(
    personal_all$總流水號,
    prepared$student_ids
  )
  if (anyNA(original_index)) {
    abort_score(
      prepared$job$key,
      " 的個人成績無法對回原始作答資料。"
    )
  }

  # 原始作答與 0／1 計分結果都保留原題號。
  # 例如第 3 題沒有答案鍵時，後續題目仍顯示為「第4題」。
  item_names <- paste0(
    "第",
    prepared$original_item_numbers,
    "題"
  )
  item_output <- prepared$item_matrix[
    original_index,
    ,
    drop = FALSE
  ]
  colnames(item_output) <- item_names
  scored_output <- scored_matrix[
    original_index,
    ,
    drop = FALSE
  ]
  colnames(scored_output) <- item_names

  # 每個向度依序輸出「答對題數、答對率」兩欄。
  # 「答對題數」會重複出現，這是既有 Excel 契約，不做自動改名。
  dimension_names <- colnames(dimension_matrices$scores)
  dimension_columns <- vector(
    "list",
    length(dimension_names) * 2L
  )
  for (dimension_index in seq_along(dimension_names)) {
    dimension_name <- dimension_names[[dimension_index]]
    count_position <- dimension_index * 2L - 1L
    score_position <- dimension_index * 2L
    dimension_columns[[count_position]] <-
      dimension_matrices$counts[
        original_index,
        dimension_name
      ]
    dimension_columns[[score_position]] <-
      dimension_matrices$scores[
        original_index,
        dimension_name
      ]
  }
  dimension_detail <- as.data.frame(
    dimension_columns,
    check.names = FALSE
  )
  colnames(dimension_detail) <- as.vector(rbind(
    rep("答對題數", length(dimension_names)),
    dimension_names
  ))

  # 詳細個人成績的基本資料取自原始 23 個資訊欄，
  # 排名與分數則取自同一份 personal_all，避免另外重算。
  info <- prepared$info_data
  columns <- prepared$info_columns
  personal_fields <- list(
    年度 = prepared$job$year,
    總流水號 = personal_all$總流水號,
    縣市流水號 = personal_all$縣市流水號,
    縣市 = personal_all$縣市,
    學校代碼 = personal_all$學校代碼,
    學校名稱 = personal_all$學校名稱,
    科目 = prepared$job$subject_name,
    年級 = as.character(prepared$job$grade),
    班級 = info[original_index, columns[["class_code"]]],
    座號 = personal_all$座號,
    姓名 = personal_all$姓名,
    性別 = info[original_index, columns[["sex"]]],
    導師 = info[original_index, columns[["teacher"]]],
    資賦優異 = info[original_index, columns[["gifted"]]],
    特殊生 = info[original_index, columns[["special"]]],
    原住民子女 = info[original_index, columns[["aboriginal"]]],
    新住民子女 = info[original_index, columns[["immigrant"]]],
    藝才班學生 = info[original_index, columns[["arts"]]],
    體育班學生 = info[original_index, columns[["sports"]]],
    非學校型態實驗教育者 =
      info[original_index, columns[["non_school"]]],
    總平均 = personal_all$總答對率,
    `排名(該縣市)` = personal_all$縣市排名,
    `排名(總參與)` = personal_all$全體排名,
    `PR值(所屬縣市)` = personal_all$縣市PR,
    `PR值(全部參與縣市)` = personal_all$全體PR
  )
  if (isTRUE(prepared$job$calc_level)) {
    personal_fields[["等級"]] <- personal_all$等級
  }
  personal_output <- as.data.frame(
    personal_fields,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  personal_output <- cbind(
    personal_output,
    dimension_detail,
    item_output,
    scored_output
  )

  # 1. 既有總平均：前兩欄標示科目與年級，接續總平均與向度分數
  total_vector <- summaries$overall_mean[
    c("總答對率", dimension_names)
  ]
  total_output <- as.data.frame(
    t(as.numeric(total_vector))
  )
  colnames(total_output) <- c("總平均", dimension_names)
  total_output <- cbind(
    科目代號 = prepared$job$subject_code,
    年級 = as.character(prepared$job$grade),
    total_output
  )
  total_output$總平均 <- round(total_output$總平均, 6)

  # 2. 併表總平均（等級描述）：併列呈現【扣除特殊生】與【含特殊生】
  total_level_output <- data.frame(
    科目代號 = prepared$job$subject_code,
    年級 = as.character(prepared$job$grade),
    `到考人數(扣除特殊生)` = summaries$total_stats$到考人數,
    `總答對率(扣除特殊生)` = round(as.numeric(summaries$overall_mean["總答對率"]), 6),
    `精熟人數(扣除特殊生)` = summaries$total_stats$精熟人數,
    `精熟率(%)(扣除特殊生)` = summaries$total_stats$`精熟率(%)`,
    `基礎人數(扣除特殊生)` = summaries$total_stats$基礎人數,
    `基礎率(%)(扣除特殊生)` = summaries$total_stats$`基礎率(%)`,
    `待加強人數(扣除特殊生)` = summaries$total_stats$待加強人數,
    `待加強率(%)(扣除特殊生)` = summaries$total_stats$`待加強率(%)`,
    `到考人數(含特殊生)` = summaries$total_stats_all$到考人數,
    `總答對率(含特殊生)` = round(as.numeric(summaries$overall_mean_all["總答對率"]), 6),
    `精熟人數(含特殊生)` = summaries$total_stats_all$精熟人數,
    `精熟率(%)(含特殊生)` = summaries$total_stats_all$`精熟率(%)`,
    `基礎人數(含特殊生)` = summaries$total_stats_all$基礎人數,
    `基礎率(%)(含特殊生)` = summaries$total_stats_all$`基礎率(%)`,
    `待加強人數(含特殊生)` = summaries$total_stats_all$待加強人數,
    `待加強率(%)(含特殊生)` = summaries$total_stats_all$`待加強率(%)`,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  list(
    personal_with_count = personal_with_count,
    personal_all = personal_all,
    personal_output = personal_output,
    total_output = total_output,
    total_level_output = total_level_output
  )
}

# 將每一種非缺考答對率彙整成人數，供結果頁長條圖使用。
#
# 不預先切固定級距，因此圖上的每一根柱代表實際出現的一個答對率。
# 若沒有可用分數，回傳具正確欄型的空表，而不是 NULL。
build_score_distribution <- function(total_correct_rate) {
  values <- total_correct_rate[!is.na(total_correct_rate)]
  if (length(values) == 0L) {
    return(data.frame(
      答對率 = numeric(),
      人數 = integer()
    ))
  }

  counts <- table(values)
  data.frame(
    答對率 = as.numeric(names(counts)),
    人數 = as.integer(counts),
    stringsAsFactors = FALSE
  )
}

# 單一工作的純分析總入口。
#
# job：工作定義。
# 回傳值：包含每一階段中間結果的具名 list；Shiny 與匯出共用此物件。
# 將流程集中在這裡可避免畫面預覽與下載 Excel 分別重算而產生差異。
analyze_job <- function(job) {
  # 1. 讀取、驗證並對齊答案與作答。
  prepared <- prepare_job_input(job)
  # 2. 計算逐題 0/1 得分。
  scored_matrix <- score_item_matrix(prepared)
  # 3. 彙整各向度答對題數與答對率。
  dimension_matrices <- calculate_dimension_matrices(
    prepared,
    scored_matrix
  )
  # 4. 組裝學生基本資料、身分與總／向度分數。
  score_data <- build_student_score_table(
    prepared,
    scored_matrix,
    dimension_matrices
  )
  # 5. 依到考母體計算各種平均。
  summaries <- calculate_summary_tables(
    prepared,
    dimension_matrices,
    score_data
  )
  # 6. 依非缺考母體計算排名、PR 與正式明細格式。
  ranked_outputs <- calculate_ranked_outputs(
    prepared,
    scored_matrix,
    dimension_matrices,
    score_data,
    summaries
  )

  list(
    prepared = prepared,
    scored_matrix = scored_matrix,
    dimension_matrices = dimension_matrices,
    score_data = score_data,
    summaries = summaries,
    ranked_outputs = ranked_outputs,
    distribution = build_score_distribution(
      score_data$total_correct_rate
    )
  )
}
