# =============================================================================
# 檔案：tests/create_fixture.R
# 用途：產生不含真實個資的最小答案檔與作答檔，供測試及網頁範例共用。
# 修改提醒：
#   - 每新增一種邊界案例，應在 tests/smoke_test.R 加入明確斷言。
#   - 前 23 個資訊欄的順序須符合目前作答檔契約。
#   - 本函式所有姓名、學校、代碼都必須維持合成資料。
# =============================================================================

# 建立匿名 C4 測試資料並回傳根目錄、答案檔與作答檔絕對路徑。
create_anonymous_fixture <- function(destination) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("建立測試資料需要 openxlsx 套件。", call. = FALSE)
  }

  dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  destination <- normalizePath(
    destination,
    winslash = "/",
    mustWork = TRUE
  )

  answer_path <- file.path(destination, "115_C_ans.xlsx")
  response_path <- file.path(
    destination,
    "115_C4_合併_校名修正.xlsx"
  )

  # 第 3 題答案為 NA，專門測試「無答案鍵題目不參與計分」。
  answers <- data.frame(
    C4 = c("A", "B", NA, "D"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  dimensions <- data.frame(
    四年級 = c("字音", "字詞", "無效題", "閱讀"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  openxlsx::write.xlsx(
    list(
      答案 = answers,
      向度 = dimensions
    ),
    answer_path,
    overwrite = TRUE
  )

  # 前 23 欄是學生資訊，第 24 至 27 欄是四題作答。
  headers <- c(
    "總流水號",
    "縣市流水號",
    "縣市",
    "鄉鎮區",
    "學校代碼",
    "學校名稱",
    "備用7",
    "備用8",
    "備用9",
    "班級代碼",
    "備用11",
    "座號",
    "姓名",
    "性別代碼",
    "備用15",
    "導師",
    "資賦優異",
    "特殊生",
    "原住民子女",
    "新住民子女",
    "藝才班學生",
    "體育班學生",
    "非學校型態實驗教育者",
    "第1題",
    "第2題",
    "第3題",
    "第4題"
  )

  # 五名學生涵蓋：
  # S001 一般生全對；S002 一般生部分答錯；S003 特殊生代碼 1；
  # S004 特殊生代碼 2 且全未作答（應優先歸為缺考）；
  # S005 一般到考且為原住民子女。
  info_rows <- rbind(
    c(
      "S001", "C001", "甲縣", "甲區", "100001", "晨光國小",
      "", "", "", "401", "", "1", "匿名甲", "1", "",
      "導師甲", "0", "0", "0", "0", "0", "0", "0"
    ),
    c(
      "S002", "C002", "甲縣", "甲區", "100001", "晨光國小",
      "", "", "", "401", "", "2", "匿名乙", "2", "",
      "導師甲", "0", "0", "0", "0", "0", "0", "0"
    ),
    c(
      "S003", "C003", "甲縣", "乙區", "100002", "星河國小",
      "", "", "", "402", "", "1", "匿名丙", "1", "",
      "導師乙", "0", "1", "0", "0", "0", "0", "0"
    ),
    c(
      "S004", "C004", "乙縣", "丙區", "200001", "遠山國小",
      "", "", "", "401", "", "1", "匿名丁", "2", "",
      "導師丙", "0", "2", "0", "0", "0", "0", "0"
    ),
    c(
      "S005", "C005", "乙縣", "丙區", "200001", "遠山國小",
      "", "", "", "401", "", "2", "匿名戊", "1", "",
      "導師丙", "0", "0", "1", "0", "0", "0", "0"
    )
  )
  # 每一列必須與上方學生資訊同順序；第 3 題雖有值但沒有答案鍵。
  response_rows <- rbind(
    c("A", "B", "C", "D"),
    c("A", "C", "C", "D"),
    c("A", "B", "C", "A"),
    c(NA, NA, NA, NA),
    c("C", "B", "C", "D")
  )
  response_table <- rbind(
    headers,
    cbind(info_rows, response_rows)
  )

  # colNames=FALSE：headers 已經手動放在第一列，不能再由套件加一列。
  openxlsx::write.xlsx(
    as.data.frame(
      response_table,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    response_path,
    colNames = FALSE,
    rowNames = FALSE,
    overwrite = TRUE
  )

  # 測試一律使用正規化絕對路徑，避免背景程序工作目錄不同。
  list(
    root = destination,
    answer_path = normalizePath(
      answer_path,
      winslash = "/",
      mustWork = TRUE
    ),
    response_path = normalizePath(
      response_path,
      winslash = "/",
      mustWork = TRUE
    )
  )
}
