# =============================================================================
# 檔案：tests/smoke_test.R
# 用途：以匿名資料驗證檔名解析、計分、平均、排名、匯出、ZIP 與錯誤隔離。
# 執行：在專案根目錄輸入 Rscript tests/smoke_test.R
# 原則：這是既有統計行為的保護網；修改計分規則時應先更新預期值理由。
# =============================================================================

# ---------------------------------------------------------------------------
# 測試環境與匿名 fixture
# ---------------------------------------------------------------------------
project_root <- normalizePath(
  file.path(getwd()),
  winslash = "/",
  mustWork = TRUE
)
if (!file.exists(file.path(project_root, "app.R"))) {
  stop(
    "請在 115-score-shiny 專案根目錄執行測試。",
    call. = FALSE
  )
}

source_files <- sort(list.files(
  file.path(project_root, "R"),
  pattern = "[.]R$",
  full.names = TRUE
))
invisible(lapply(source_files, sys.source, envir = globalenv()))
sys.source(
  file.path(project_root, "tests", "create_fixture.R"),
  envir = globalenv()
)

test_root <- tempfile("score-shiny-smoke-")
dir.create(test_root, recursive = TRUE)
on.exit(
  unlink(test_root, recursive = TRUE, force = TRUE),
  add = TRUE
)

fixture <- create_anonymous_fixture(
  file.path(test_root, "fixture")
)

# 範例必須沿用正式來源格式：第 14 欄為「性別代碼」，值只使用 1／2。
fixture_response <- read_response_table(fixture$response_path)
stopifnot(identical(as.character(fixture_response[1, 1]), "總流水號"))
stopifnot(identical(as.character(fixture_response[1, 2]), "縣市流水號"))
stopifnot(identical(as.character(fixture_response[1, 14]), "性別代碼"))
stopifnot(setequal(
  unique(as.character(fixture_response[-1, 14])),
  c("1", "2")
))
fixture_warnings <- character()
fixture_columns <- withCallingHandlers(
  resolve_info_columns(
    as.character(fixture_response[1, seq_len(N_INFO_COLUMNS)])
  ),
  warning = function(warning) {
    fixture_warnings <<- c(fixture_warnings, conditionMessage(warning))
    invokeRestart("muffleWarning")
  }
)
stopifnot(identical(unname(fixture_columns[["sex"]]), 14L))
stopifnot(identical(unname(fixture_columns[["county_id"]]), 2L))
stopifnot(length(fixture_warnings) == 0L)

# ---------------------------------------------------------------------------
# 單科工作發現與檔案預覽
# ---------------------------------------------------------------------------
jobs <- discover_single_jobs(
  answer_path = fixture$answer_path,
  response_paths = fixture$response_path,
  year = "115",
  subject_code = "C"
)

stopifnot(length(jobs) == 1L)
preview <- preview_jobs(jobs)
stopifnot(identical(preview$狀態, "可執行"))

# 單科答案檔名稱不限；作答檔只要求開頭可解析，後綴可自訂。
flexible_answer_path <- file.path(
  test_root,
  "這是答案檔.xlsx"
)
flexible_response_path <- file.path(
  test_root,
  "115-C4-第一次匯出.xlsx"
)
stopifnot(file.copy(
  fixture$answer_path,
  flexible_answer_path
))
stopifnot(file.copy(
  fixture$response_path,
  flexible_response_path
))
flexible_jobs <- discover_single_jobs(
  answer_path = flexible_answer_path,
  response_paths = flexible_response_path,
  year = "115",
  subject_code = "C"
)
stopifnot(identical(flexible_jobs[[1L]]$key, "115_C4"))

# 答案檔保留第 3 題 NA，確認題號不會因空列向前位移。
answer_tables <- read_answer_tables(fixture$answer_path)
stopifnot(length(answer_tables$answers$C4) == 4L)
stopifnot(is.na(answer_tables$answers$C4[[3L]]))

# ---------------------------------------------------------------------------
# 正式分析、互斥人數、耗時欄與預覽／Excel 一致性
# ---------------------------------------------------------------------------
run <- run_job_batch(
  jobs,
  file.path(test_root, "output")
)
stopifnot(identical(run$job_table$狀態, "完成"))
stopifnot(all(
  c("計算秒數", "Excel秒數", "總秒數") %in%
    colnames(run$job_table)
))

result <- run$results[["115_C4"]]
expected_export_keys <- c(
  "overall_scores",
  "county",
  "school",
  "class",
  "region",
  "family",
  "absent",
  "personal_with_count",
  "all_students",
  "personal_detail",
  "total"
)
stopifnot(identical(names(result$exported_files), expected_export_keys))
stopifnot(result$student_count == 5L)
stopifnot(result$attended_count == 3L)
stopifnot(result$absent_count == 1L)
stopifnot(result$special_count == 1L)
# S004 同時是代碼 2 與全未作答；缺考優先後仍必須維持三類恆等式。
stopifnot(
  result$student_count ==
    result$attended_count +
      result$absent_count +
      result$special_count
)
stopifnot(all(
  c("學生數", "到考數", "缺考數", "特殊生") %in%
    colnames(run$job_table)
))

overall <- result$views$total
# 到考三人的預期總平均；特殊生及缺考均不進入平均。
stopifnot(
  isTRUE(all.equal(
    as.numeric(overall$總平均),
    0.777778,
    tolerance = 1e-8
  ))
)
stopifnot(nrow(result$views$county) == 2L)
# 六張頁面預覽逐張讀回 Excel，比對內容完全一致。
for (view_key in names(result$views)) {
  exported_view <- openxlsx::read.xlsx(
    unname(result$exported_files[[view_key]]),
    check.names = FALSE
  )
  stopifnot(isTRUE(all.equal(
    result$views[[view_key]],
    exported_view,
    check.attributes = FALSE,
    tolerance = 0
  )))
}
county_row <- result$views$county[
  result$views$county$縣市 == "甲縣",
  ,
  drop = FALSE
]
stopifnot(
  isTRUE(all.equal(
    as.numeric(county_row$總答對率),
    0.833333,
    tolerance = 1e-8
  ))
)

# ---------------------------------------------------------------------------
# 完整名單：保留兩種流水號、加入答對題數，且缺考排名維持 NA
# ---------------------------------------------------------------------------
all_students <- openxlsx::read.xlsx(
  unname(result$exported_files[["all_students"]]),
  check.names = FALSE
)
stopifnot(nrow(all_students) == 5L)
stopifnot(all(
  c("總流水號", "縣市流水號", "答對題數") %in%
    colnames(all_students)
))
special_row <- all_students[
  all_students$總流水號 == "S003",
  ,
  drop = FALSE
]
absent_row <- all_students[
  all_students$總流水號 == "S004",
  ,
  drop = FALSE
]
stopifnot(identical(as.character(special_row$縣市流水號), "C003"))
stopifnot(as.numeric(special_row$答對題數) == 2)
stopifnot(as.numeric(special_row$全體排名) == 2)
stopifnot(as.numeric(absent_row$答對題數) == 0)
stopifnot(is.na(absent_row$全體排名))

# ---------------------------------------------------------------------------
# 恢復的三份報表：保留兩種流水號，內容沿用既有欄位契約
# ---------------------------------------------------------------------------
absent_list <- openxlsx::read.xlsx(
  unname(result$exported_files[["absent"]]),
  check.names = FALSE
)
stopifnot(nrow(absent_list) == 1L)
stopifnot(identical(
  colnames(absent_list)[1:2],
  c("總流水號", "縣市流水號")
))
stopifnot(identical(as.character(absent_list$總流水號), "S004"))
stopifnot(identical(as.character(absent_list$縣市流水號), "C004"))

personal_with_count <- openxlsx::read.xlsx(
  unname(result$exported_files[["personal_with_count"]]),
  check.names = FALSE
)
stopifnot(nrow(personal_with_count) == 5L)
stopifnot(all(
  c("總流水號", "縣市流水號", "答對題數") %in%
    colnames(personal_with_count)
))
stopifnot(!any(
  c("縣市排名", "縣市PR") %in%
    colnames(personal_with_count)
))
personal_count_absent <- personal_with_count[
  personal_with_count$總流水號 == "S004",
  ,
  drop = FALSE
]
stopifnot(as.numeric(personal_count_absent$答對題數) == 0)

personal_detail <- openxlsx::read.xlsx(
  unname(result$exported_files[["personal_detail"]]),
  check.names = FALSE
)
stopifnot(nrow(personal_detail) == 5L)
stopifnot(identical(
  colnames(personal_detail)[1:3],
  c("年度", "總流水號", "縣市流水號")
))
stopifnot(all(
  c(
    "總平均",
    "排名(該縣市)",
    "排名(總參與)",
    "PR值(所屬縣市)",
    "PR值(全部參與縣市)",
    "第1題",
    "第2題",
    "第4題"
  ) %in% colnames(personal_detail)
))
stopifnot(sum(colnames(personal_detail) == "第1題") == 2L)
stopifnot(sum(colnames(personal_detail) == "第4題") == 2L)

# ---------------------------------------------------------------------------
# 成績等級：勾選「計算精熟等級」時產生「等級」欄位，劃分精熟、基礎、待加強
# ---------------------------------------------------------------------------
level_jobs <- discover_single_jobs(
  answer_path = fixture$answer_path,
  response_paths = fixture$response_path,
  year = "115",
  subject_code = "C",
  calc_level = TRUE,
  mastery_cutoff = 3,
  basic_cutoff = 2
)
level_run <- run_job_batch(
  level_jobs,
  file.path(test_root, "level-output")
)
level_result <- level_run$results[["115_C4"]]
level_personal <- openxlsx::read.xlsx(
  unname(level_result$exported_files[["personal_detail"]]),
  check.names = FALSE
)
stopifnot("等級" %in% colnames(level_personal))
stopifnot(identical(
  match("等級", colnames(level_personal)),
  match("PR值(全部參與縣市)", colnames(level_personal)) + 1L
))
level_expected_keys <- c(
  "overall_scores",
  "county",
  "school",
  "class",
  "region",
  "family",
  "county_level",
  "school_level",
  "class_level",
  "region_level",
  "family_level",
  "absent",
  "personal_with_count",
  "all_students",
  "personal_detail",
  "total",
  "total_level"
)
stopifnot(identical(names(level_result$exported_files), level_expected_keys))

# ---------------------------------------------------------------------------
# 下載 ZIP：未勾選等級描述產出 11 份；勾選時產出 17 份 Excel
# ---------------------------------------------------------------------------
archive_path <- create_result_archive(
  run$output_root,
  file.path(test_root, "results.zip")
)
stopifnot(file.exists(archive_path))
archive_entries <- utils::unzip(
  archive_path,
  list = TRUE
)$Name
stopifnot(length(archive_entries) == 11L)
stopifnot(any(grepl("縣市平均[.]xlsx$", archive_entries)))
stopifnot(!any(grepl("等級描述", archive_entries)))
stopifnot(any(grepl("缺考名單[.]xlsx$", archive_entries)))
stopifnot(any(grepl("個人成績含題數[.]xlsx$", archive_entries)))
stopifnot(any(grepl("_個人成績[.]xlsx$", archive_entries)))

level_archive_path <- create_result_archive(
  level_run$output_root,
  file.path(test_root, "level_results.zip")
)
level_archive_entries <- utils::unzip(level_archive_path, list = TRUE)$Name
stopifnot(length(level_archive_entries) == 17L)
stopifnot(any(grepl("縣市平均[(]等級描述[)][.]xlsx$", level_archive_entries)))

# ---------------------------------------------------------------------------
# 批次模式：基本配對及允許後綴的彈性檔名
# ---------------------------------------------------------------------------
batch_path <- file.path(test_root, "fixture.zip")
zip::zipr(
  batch_path,
  files = c(
    fixture$answer_path,
    fixture$response_path
  ),
  root = fixture$root
)
batch_root <- extract_zip_safely(
  batch_path,
  file.path(test_root, "batch")
)
batch_jobs <- discover_batch_jobs(batch_root, "115")
stopifnot(length(batch_jobs) == 1L)
stopifnot(identical(batch_jobs[[1L]]$key, "115_C4"))

flexible_batch_root <- file.path(test_root, "flexible-batch")
dir.create(flexible_batch_root)
flexible_batch_answer <- file.path(
  flexible_batch_root,
  "115-C-答案-v2.xlsx"
)
flexible_batch_response <- file.path(
  flexible_batch_root,
  "115-C4-校務系統匯出.xlsx"
)
stopifnot(file.copy(
  fixture$answer_path,
  flexible_batch_answer
))
stopifnot(file.copy(
  fixture$response_path,
  flexible_batch_response
))
flexible_batch_jobs <- discover_batch_jobs(
  flexible_batch_root,
  "115"
)
stopifnot(identical(
  flexible_batch_jobs[[1L]]$key,
  "115_C4"
))

# 專案交付時必須包含網頁可下載的三個匿名範例。
example_files <- file.path(
  project_root,
  "www",
  "examples",
  c(
    "115_C_ans.xlsx",
    "115_C4_合併_校名修正.xlsx",
    "115_匿名範例批次.zip"
  )
)
stopifnot(all(file.exists(example_files)))

# ---------------------------------------------------------------------------
# 批次錯誤隔離：一個工作缺少 C5 答案，不得中止已成功的 C4
# ---------------------------------------------------------------------------
invalid_job <- make_job(
  year = "115",
  subject_code = "C",
  grade = 5L,
  answer_path = fixture$answer_path,
  response_path = fixture$response_path
)
mixed_run <- run_job_batch(
  c(jobs, list(invalid_job)),
  file.path(test_root, "mixed-output")
)
stopifnot(identical(
  mixed_run$job_table$狀態,
  c("完成", "失敗")
))

# ---------------------------------------------------------------------------
# 多重可接受答案：答案鍵 B、C 應同時接受 B 或 C
# ---------------------------------------------------------------------------
multi_prepared <- list(
  n_total = 2L,
  n_items = 2L,
  key_vector = c("A", "B、C"),
  item_matrix = matrix(
    c("A", "D", "C", "B"),
    nrow = 2L,
    ncol = 2L
  ),
  original_item_numbers = c(1L, 4L)
)
multi_scores <- score_item_matrix(multi_prepared)
stopifnot(identical(
  unname(multi_scores),
  matrix(c(1L, 0L, 1L, 1L), nrow = 2L)
))
stopifnot(identical(colnames(multi_scores), c("Q1", "Q4")))

# ---------------------------------------------------------------------------
# 向度名稱：分組前先清除前後空白，避免產生「修辭知識_1」
# ---------------------------------------------------------------------------
trim_prepared <- list(
  dimension_labels = list(
    c("修辭知識", "修辭知識 ", "閱讀理解")
  ),
  absent_flag = c(FALSE, FALSE),
  job = list(key = "trim_dimension")
)
trim_scores <- matrix(
  c(
    1L, 0L, 1L,
    1L, 1L, 0L
  ),
  nrow = 2L,
  byrow = TRUE
)
trim_dimensions <- calculate_dimension_matrices(
  trim_prepared,
  trim_scores
)
stopifnot(identical(
  colnames(trim_dimensions$scores),
  c("修辭知識", "閱讀理解")
))
stopifnot(identical(
  as.integer(trim_dimensions$counts[, "修辭知識"]),
  c(1L, 2L)
))

# ---------------------------------------------------------------------------
# 精度與效能：查表版 score_ratio() 必須逐值等同舊版 Rmpfr 計算
# ---------------------------------------------------------------------------
for (denominator in c(1, 3, 7, 30)) {
  numerators <- c(NA_real_, seq.int(0, denominator))
  legacy_ratios <- as.numeric(
    bcmul(bcdiv(numerators, denominator), 1)
  )
  lookup_ratios <- score_ratio(numerators, denominator)
  stopifnot(identical(lookup_ratios, legacy_ratios))
}

cat("Smoke test passed: single, batch discovery, scoring, exports, ZIP.\n")
