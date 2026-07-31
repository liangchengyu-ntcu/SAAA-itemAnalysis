# =============================================================================
# 檔案：R/00_constants.R
# 用途：集中放置全專案共用且不應在執行期間改變的常數。
# 載入：app.R 會依檔名排序載入 R/，所以本檔最先載入。
# 修改提示：
#   1. 新增科目時，先改 SUBJECT_NAMES，再檢查計分與向度規則。
#   2. 資訊欄位格式改版時，應同步調整 N_INFO_COLUMNS、
#      DEFAULT_INFO_COLUMNS 與 INFO_COLUMN_ALIASES。
#   3. RESULT_VIEWS 的名稱會直接顯示在結果頁下拉選單。
# =============================================================================

# 科目代號與中文名稱。左側代號也會用於檔名及答案欄位，例如 C4。
SUBJECT_NAMES <- c(
  C = "國語",
  E = "英語",
  M = "數學",
  S = "自然"
)

# 成績等級定義
LEVEL_NAMES <- c("精熟", "基礎", "待加強")

# 縣市名稱轉縣市英文代碼對照表（用於生成「縣市流水號」）
CITY_CODE_MAP <- c(
  "基隆市" = "C",
  "台北市" = "A",
  "臺北市" = "A",
  "臺北縣" = "F",
  "台北縣" = "F",
  "新北市" = "F",
  "桃園市" = "H",
  "桃園縣" = "H",
  "新竹市" = "O",
  "新竹縣" = "J",
  "苗栗縣" = "K",
  "台中市" = "B",
  "臺中市" = "B",
  "台中縣" = "L",
  "臺中縣" = "L",
  "彰化縣" = "N",
  "南投縣" = "M",
  "雲林縣" = "P",
  "嘉義市" = "I",
  "嘉義縣" = "Q",
  "台南市" = "D",
  "臺南市" = "D",
  "台南縣" = "R",
  "臺南縣" = "R",
  "高雄市" = "E",
  "高雄縣" = "S",
  "屏東縣" = "T",
  "台東縣" = "V",
  "臺東縣" = "V",
  "花蓮縣" = "U",
  "宜蘭縣" = "G",
  "澎湖縣" = "X",
  "金門縣" = "W",
  "陽明山" = "Y",
  "連江縣" = "Z"
)

# 依據年度、縣市名稱、科目與年級產生「縣市流水號」（例如：115_B_C4_000001）
generate_county_id <- function(year, city_name, volume, serial_number) {
  city_code <- unname(CITY_CODE_MAP[as.character(city_name)])
  if (any(is.na(city_code))) {
    city_code[is.na(city_code)] <- "X"
  }
  serial_formatted <- sprintf("%06d", as.integer(serial_number))
  paste(year, city_code, volume, serial_formatted, sep = "_")
}


# 年級數字轉成答案檔「向度」工作表使用的中文欄名。
GRADE_TO_CHINESE <- c(
  "3" = "三年級",
  "4" = "四年級",
  "5" = "五年級",
  "6" = "六年級",
  "7" = "七年級",
  "8" = "八年級"
)

# 作答檔前段固定的學生基本資料欄數；第 24 欄起才視為題目作答。
N_INFO_COLUMNS <- 23L

# 找不到標準欄名時使用的舊版欄位位置。
# 名稱（id、city 等）是程式內部的穩定欄位鍵，不要任意更名。
DEFAULT_INFO_COLUMNS <- c(
  id = 1L,
  county_id = 2L,
  city = 3L,
  district = 4L,
  school_code = 5L,
  school_name = 6L,
  class_code = 10L,
  seat_no = 12L,
  student_name = 13L,
  sex = 14L,
  teacher = 16L,
  gifted = 17L,
  special = 18L,
  aboriginal = 19L,
  immigrant = 20L,
  arts = 21L,
  sports = 22L,
  non_school = 23L
)

# 每個內部欄位鍵可接受的中文欄名。
# 新來源檔若只是欄名不同，優先在這裡增加別名，不要直接改計分程式。
INFO_COLUMN_ALIASES <- list(
  id = c("總流水號", "流水號", "學生流水號"),
  county_id = c("縣市流水號"),
  city = c("縣市", "縣市名稱"),
  district = c("鄉鎮區", "鄉鎮市區", "鄉鎮區名稱"),
  school_code = c("學校代碼", "學校編號"),
  school_name = c("學校名稱", "校名"),
  class_code = c("班級代碼", "班級編號", "班級"),
  seat_no = c("座號"),
  student_name = c("姓名", "學生姓名"),
  sex = c("性別", "性別代碼"),
  teacher = c("導師", "導師姓名"),
  gifted = c("資賦優異", "資優生"),
  special = c("特殊生", "特殊生註記"),
  aboriginal = c("原住民子女", "原住民子女註記"),
  immigrant = c("新住民子女", "新住民子女註記"),
  arts = c("藝才班學生", "藝術才能班學生"),
  sports = c("體育班學生"),
  non_school = c(
    "非學校型態實驗教育者",
    "非學校型態實驗教育",
    "在家教育"
  )
)

# 結果頁可預覽的彙總報表。
# 向量名稱是畫面文字，值必須對應 build_result_views() 的清單名稱。
RESULT_VIEWS <- c(
  "試題 CTT 品質與診斷" = "ctt",
  "總平均" = "total",
  "縣市平均" = "county",
  "各校平均" = "school",
  "各班平均" = "class",
  "縣市區域平均" = "region",
  "不同家庭背景平均" = "family"
)
