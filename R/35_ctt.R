# =============================================================================
# 檔案：R/35_ctt.R
# 用途：經典測驗理論 (CTT) 試題分析引擎
# 功能：高低分組 27% 劃分、難度/通過率、鑑別度、誘答力矩陣、點二系列相關及
#       Cronbach's alpha 信度計算與試題品質異常診斷。
# =============================================================================

# 計算 Cronbach's alpha 信度
calculate_cronbach_alpha <- function(scored_matrix) {
  if (is.null(scored_matrix) || ncol(scored_matrix) < 2L || nrow(scored_matrix) < 2L) {
    return(NA_real_)
  }

  k <- ncol(scored_matrix)
  item_vars <- apply(scored_matrix, 2, var, na.rm = TRUE)
  total_scores <- rowSums(scored_matrix, na.rm = TRUE)
  total_var <- var(total_scores, na.rm = TRUE)

  if (is.na(total_var) || total_var <= 0) {
    return(NA_real_)
  }

  alpha <- (k / (k - 1L)) * (1 - sum(item_vars, na.rm = TRUE) / total_var)
  round(alpha, 4L)
}

# CTT 試題分析核心函式
calculate_ctt_analysis <- function(
  item_matrix,
  key_vector,
  grade,
  subject_code,
  absent_flag = NULL
) {
  n_total <- nrow(item_matrix)
  n_items <- ncol(item_matrix)

  if (is.null(absent_flag)) {
    absent_flag <- rep(FALSE, n_total)
  }

  # 標記有答案鍵且非空白的計分題目
  scored_mask <- is_valid_key(key_vector)
  key_split <- strsplit(key_vector, "、")
  key_flat <- unique(unlist(key_split))

  unique_matrix_vals <- unique(as.vector(item_matrix))
  is_dichotomous <- all(unique_matrix_vals %in% c("0", "1", "9", "", NA_character_))

  # 建立 0/1 計分矩陣（9 表示無效作答/未答）
  scored_mat <- matrix(0L, nrow = n_total, ncol = n_items)
  for (j in seq_len(n_items)) {
    if (scored_mask[j]) {
      if (is_dichotomous) {
        scored_mat[, j] <- ifelse(!is.na(item_matrix[, j]) & item_matrix[, j] == "1", 1L, 0L)
      } else {
        options <- key_split[[j]]
        scored_mat[, j] <- ifelse(item_matrix[, j] %in% options, 1L, 0L)
      }
    }
  }
  scored_mat[item_matrix == "9"] <- 9L
  colnames(scored_mat) <- paste0("Q", seq_len(n_items))

  # 篩選非全未答/非缺考之有效學生進行 CTT 27% 分組
  n_scored <- sum(scored_mask)
  if (n_scored == 0L) {
    abort_score("沒有可計分的有效題目。")
  }

  unans_mat <- item_matrix[, scored_mask, drop = FALSE] == "9" | is.na(item_matrix[, scored_mask, drop = FALSE])
  unans_counts <- rowSums(unans_mat, na.rm = TRUE)
  valid_rows <- !absent_flag & (unans_counts < n_scored)
  valid_rows[is.na(valid_rows)] <- FALSE
  if (sum(valid_rows) == 0L) {
    valid_rows <- !absent_flag
  }

  item_mat_valid <- item_matrix[valid_rows, , drop = FALSE]
  scored_mat_valid <- scored_mat[valid_rows, , drop = FALSE]
  scores_valid <- rowSums(scored_mat_valid[, scored_mask, drop = FALSE] == 1L, na.rm = TRUE)
  n_total_valid <- nrow(item_mat_valid)

  # 計算 Cronbach's alpha 信度
  alpha_val <- calculate_cronbach_alpha(scored_mat_valid[, scored_mask, drop = FALSE])

  # ---------------------------------------------------------------------------
  # 核心 27% 高低分組演算法（使用者指定核心演算法，嚴格保留不變）
  # ---------------------------------------------------------------------------
  x <- round(n_total_valid * 0.27, 0)
  if (x < 1) x <- 1
  sorted_scores <- sort(scores_valid)
  threshold_low <- sorted_scores[x]
  threshold_high <- sorted_scores[n_total_valid - x + 1]

  group <- rep("mid68", n_total_valid)
  group[scores_valid <= threshold_low] <- "lower"
  group[scores_valid >= threshold_high] <- "upper"
  # ---------------------------------------------------------------------------

  # 設定選項標籤（國中 A~D，國小 1~4；若作答檔為 0/1 已計分檔，則為 1與0）
  grade_num <- suppressWarnings(as.integer(grade))
  opt_labels <- if (is_dichotomous) c("1", "0") else if (!is.na(grade_num) && grade_num >= 7L) c("A", "B", "C", "D") else c("1", "2", "3", "4")
  opts <- sort(unique(c(if (!is_dichotomous) key_flat, opt_labels, "9")))

  results_list <- list()

  for (j in seq_len(n_items)) {
    if (!scored_mask[j]) {
      # 不計分題佔位列
      block <- data.frame(
        Item = paste0("題號", j),
        correct = "*",
        key = "此題不計分",
        n = NA_integer_,
        rspP = NA_real_,
        pBis = NA_real_,
        discrim = NA_real_,
        lower = NA_real_,
        mid68 = NA_real_,
        upper = NA_real_,
        difficultyIndex = NA_real_,
        stringsAsFactors = FALSE
      )
      results_list[[j]] <- block
      next
    }

    item_resp <- item_mat_valid[, j]
    item_key_vec <- if (is_dichotomous) "1" else key_split[[j]]
    if (length(item_key_vec) == 0L || is.na(item_key_vec[1L]) || item_key_vec[1L] == "") {
      item_key_vec <- "9"
    }

    block <- data.frame(
      Item = paste0("題號", j),
      correct = "",
      key = opts,
      n = 0L,
      rspP = NA_real_,
      pBis = NA_real_,
      discrim = NA_real_,
      lower = NA_real_,
      mid68 = NA_real_,
      upper = NA_real_,
      difficultyIndex = NA_real_,
      stringsAsFactors = FALSE
    )

    for (k in seq_along(opts)) {
      opt <- opts[k]
      sel <- !is.na(item_resp) & item_resp == opt
      block$n[k] <- sum(sel, na.rm = TRUE)
      block$rspP[k] <- mean(sel, na.rm = TRUE)
      if (opt %in% item_key_vec) block$correct[k] <- "*"

      grp_n <- tapply(sel, group, sum, na.rm = TRUE)
      grp_total <- tapply(rep(1, n_total_valid), group, sum)

      l_val <- if ("lower" %in% names(grp_n)) grp_n[["lower"]] / grp_total[["lower"]] else 0
      m_val <- if ("mid68" %in% names(grp_n)) grp_n[["mid68"]] / grp_total[["mid68"]] else 0
      u_val <- if ("upper" %in% names(grp_n)) grp_n[["upper"]] / grp_total[["upper"]] else 0

      block$lower[k] <- l_val
      block$mid68[k] <- m_val
      block$upper[k] <- u_val

      point_bis <- tryCatch(
        suppressWarnings(cor(as.numeric(sel), scores_valid, method = "pearson")),
        error = function(e) NA_real_
      )
      block$pBis[k] <- point_bis
    }

    correct_rows <- which(opts %in% item_key_vec)
    if (length(correct_rows) > 0L) {
      block$discrim <- block$upper - block$lower
      for (cr in correct_rows) {
        block$difficultyIndex[cr] <- round((block$upper[cr] + block$lower[cr]) / 2, 4L)
      }
    }
    results_list[[j]] <- block
  }

  distractor_results <- do.call(rbind, results_list)

  # 建立逐題 CTT 摘要 DataFrame (一題一列，包含診斷建議)
  col_zh <- c("題號", "正確答案", "樣本數", "通過率", "pBis", "鑑別度", "低分組", "中分組", "高分組", "難度", "診斷建議")
  item_summary <- data.frame(
    matrix(NA_character_, nrow = n_items, ncol = length(col_zh)),
    stringsAsFactors = FALSE
  )
  colnames(item_summary) <- col_zh
  item_summary$題號 <- seq_len(n_items)

  for (item in seq_len(n_items)) {
    sub <- subset(distractor_results, Item == paste0("題號", item))
    cr <- subset(sub, correct == "*")

    if (nrow(cr) > 0L) {
      if (any(cr$key == "此題不計分", na.rm = TRUE) && all(is.na(cr$rspP))) {
        item_summary$正確答案[item] <- "該題不予計分"
        item_summary$診斷建議[item] <- "不計分"
        next
      }
      item_summary$正確答案[item] <- if (is_dichotomous) key_vector[item] else paste(cr$key, collapse = "、")
      item_summary$樣本數[item] <- as.character(sum(cr$n, na.rm = TRUE))
      item_summary$通過率[item] <- sprintf("%.2f", sum(cr$rspP, na.rm = TRUE))
      item_summary$pBis[item] <- sprintf("%.2f", mean(cr$pBis, na.rm = TRUE))

      hp_val <- sum(cr$upper, na.rm = TRUE)
      lp_val <- sum(cr$lower, na.rm = TRUE)
      disc_val <- hp_val - lp_val
      item_summary$鑑別度[item] <- sprintf("%.2f", disc_val)
      item_summary$低分組[item] <- sprintf("%.2f", lp_val)
      item_summary$中分組[item] <- sprintf("%.2f", sum(cr$mid68, na.rm = TRUE))
      item_summary$高分組[item] <- sprintf("%.2f", hp_val)

      diff_val <- (hp_val + lp_val) / 2
      item_summary$難度[item] <- sprintf("%.2f", diff_val)

      # 診斷建議
      msgs <- character(0)
      if (!is.na(disc_val) && disc_val < 0.05) {
        msgs <- c(msgs, "鑑別度未達0.05建議直接刪除")
      } else if (!is.na(disc_val) && disc_val >= 0.05 && disc_val <= 0.15) {
        msgs <- c(msgs, "鑑別度0.05～0.15需進行試題修改")
      }

      if (!is_dichotomous && nrow(cr) > 0L) {
        cr_max_upper <- max(cr$upper, na.rm = TRUE)
        cr_max_rspP <- max(cr$rspP, na.rm = TRUE)

        for (k in seq_along(opt_labels)) {
          or <- subset(sub, key == opt_labels[k])
          if (nrow(or) > 0L && !is.na(or$upper[1L]) && or$upper[1L] > cr_max_upper) {
            msgs <- c(msgs, "高分組錯誤選項選答率高於正確選項")
            break
          }
        }

        for (k in seq_along(opt_labels)) {
          or <- subset(sub, key == opt_labels[k])
          if (nrow(or) > 0L && !is.na(or$rspP[1L]) && or$rspP[1L] > cr_max_rspP) {
            msgs <- c(msgs, "錯誤選項選答率高於正確選項")
            break
          }
        }
      }

      item_summary$診斷建議[item] <- if (length(msgs) > 0L) paste(msgs, collapse = "；") else "正常"
    }
  }

  list(
    distractor_results = distractor_results,
    item_summary = item_summary,
    alpha = alpha_val,
    n_total_valid = n_total_valid,
    n_items = n_items,
    opt_labels = opt_labels,
    key_vector = key_vector
  )
}

# -----------------------------------------------------------------------------
# 縣市標準三等級 (精熟 / 基礎 / 待加強) 試題分析計算引擎
# 對齊國立臺中教育大學測驗統計中心 / 縣市學檢標準規範
# -----------------------------------------------------------------------------
calculate_level_ctt_analysis <- function(
  item_matrix,
  key_vector,
  grade,
  subject_code,
  absent_flag = NULL,
  mastery_cutoff = NULL,
  basic_cutoff = NULL
) {
  n_total <- nrow(item_matrix)
  n_items <- ncol(item_matrix)

  if (is.null(absent_flag)) {
    absent_flag <- rep(FALSE, n_total)
  }

  scored_mask <- is_valid_key(key_vector)
  key_split <- strsplit(key_vector, "、")
  n_scored <- sum(scored_mask)

  if (n_scored == 0L) {
    abort_score("沒有可計分的有效題目。")
  }

  # 設定預設門檻（若未傳入）
  if (is.null(mastery_cutoff) || is.na(mastery_cutoff)) {
    mastery_cutoff <- ceiling(n_scored * 0.75)
  }
  if (is.null(basic_cutoff) || is.na(basic_cutoff)) {
    basic_cutoff <- ceiling(n_scored * 0.50)
  }

  unique_matrix_vals <- unique(as.vector(item_matrix))
  is_dichotomous <- all(unique_matrix_vals %in% c("0", "1", "9", "", NA_character_))

  # 建立 0/1 計分矩陣
  scored_mat <- matrix(0L, nrow = n_total, ncol = n_items)
  for (j in seq_len(n_items)) {
    if (scored_mask[j]) {
      if (is_dichotomous) {
        scored_mat[, j] <- ifelse(!is.na(item_matrix[, j]) & item_matrix[, j] == "1", 1L, 0L)
      } else {
        options <- key_split[[j]]
        scored_mat[, j] <- ifelse(item_matrix[, j] %in% options, 1L, 0L)
      }
    }
  }

  # 篩選有效學生
  unans_mat <- item_matrix[, scored_mask, drop = FALSE] == "9" | is.na(item_matrix[, scored_mask, drop = FALSE])
  unans_counts <- rowSums(unans_mat, na.rm = TRUE)
  valid_rows <- !absent_flag & (unans_counts < n_scored)
  valid_rows[is.na(valid_rows)] <- FALSE
  if (sum(valid_rows) == 0L) {
    valid_rows <- !absent_flag
  }

  item_mat_valid <- item_matrix[valid_rows, , drop = FALSE]
  scored_mat_valid <- scored_mat[valid_rows, , drop = FALSE]
  scores_valid <- rowSums(scored_mat_valid[, scored_mask, drop = FALSE] == 1L, na.rm = TRUE)
  n_total_valid <- nrow(item_mat_valid)

  # 依據標準門檻分類學生三等級：精熟、基礎、待加強
  level_group <- rep("待加強", n_total_valid)
  level_group[scores_valid >= basic_cutoff] <- "基礎"
  level_group[scores_valid >= mastery_cutoff] <- "精熟"

  level_counts <- table(factor(level_group, levels = c("精熟", "基礎", "待加強")))

  grade_num <- suppressWarnings(as.integer(grade))
  opt_labels <- if (is_dichotomous) c("1", "0") else if (!is.na(grade_num) && grade_num >= 7L) c("A", "B", "C", "D") else c("1", "2", "3", "4")

  # 構建對齊簡報截圖美學格式的縣市三等級試題分析 Data Frame
  n_opts <- length(opt_labels)
  n_cols <- 4 + 4 * (n_opts + 1)

  col_names_level <- c(
    "題號", "鑑別度", "通過率", "正確答案",
    paste0("全體_", c(opt_labels, "其它")),
    paste0("精熟_", c(opt_labels, "其它")),
    paste0("基礎_", c(opt_labels, "其它")),
    paste0("待加強_", c(opt_labels, "其它"))
  )

  mat <- data.frame(
    matrix(NA_real_, nrow = n_items, ncol = n_cols),
    stringsAsFactors = FALSE
  )
  colnames(mat) <- col_names_level
  mat$題號 <- seq_len(n_items)

  for (item in seq_len(n_items)) {
    if (!scored_mask[item]) {
      mat$正確答案[item] <- "該題不予計分"
      next
    }

    item_resp <- item_mat_valid[, item]
    item_key_vec <- key_split[[item]]
    correct_key <- if (is_dichotomous) key_vector[item] else paste(item_key_vec, collapse = "、")
    mat$正確答案[item] <- correct_key

    # 通過率與鑑別度
    is_correct <- if (is_dichotomous) (!is.na(item_resp) & item_resp == "1") else (!is.na(item_resp) & item_resp %in% item_key_vec)
    pass_rate <- mean(is_correct, na.rm = TRUE)
    mat$通過率[item] <- pass_rate

    # 傳統 CTT 27% 鑑別度
    x_27 <- round(n_total_valid * 0.27, 0)
    if (x_27 < 1) x_27 <- 1
    sorted_s <- sort(scores_valid)
    t_low <- sorted_s[x_27]
    t_high <- sorted_s[n_total_valid - x_27 + 1]
    u_mask <- scores_valid >= t_high
    l_mask <- scores_valid <= t_low
    u_rate <- mean(is_correct[u_mask], na.rm = TRUE)
    l_rate <- mean(is_correct[l_mask], na.rm = TRUE)
    disc_val <- u_rate - l_rate
    mat$鑑別度[item] <- disc_val

    # 算 4 組選答百分比：全體、精熟、基礎、待加強
    groups_list <- list(
      "全體" = rep(TRUE, n_total_valid),
      "精熟" = level_group == "精熟",
      "基礎" = level_group == "基礎",
      "待加強" = level_group == "待加強"
    )

    col_idx <- 5L
    for (grp_name in c("全體", "精熟", "基礎", "待加強")) {
      grp_mask <- groups_list[[grp_name]]
      grp_n <- sum(grp_mask, na.rm = TRUE)
      sub_resp <- item_resp[grp_mask]

      for (k in seq_along(opt_labels)) {
        opt <- opt_labels[k]
        rate <- if (grp_n > 0L) sum(!is.na(sub_resp) & sub_resp == opt) / grp_n else 0
        mat[item, col_idx] <- rate
        col_idx <- col_idx + 1L
      }
      # 其它
      other_rate <- if (grp_n > 0L) sum(is.na(sub_resp) | !sub_resp %in% opt_labels) / grp_n else 0
      mat[item, col_idx] <- other_rate
      col_idx <- col_idx + 1L
    }
  }

  list(
    level_summary_table = mat,
    level_counts = level_counts,
    mastery_cutoff = mastery_cutoff,
    basic_cutoff = basic_cutoff,
    n_total_valid = n_total_valid,
    n_items = n_items,
    opt_labels = opt_labels
  )
}
