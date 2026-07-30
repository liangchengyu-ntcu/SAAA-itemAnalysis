# =============================================================================
# 檔案：app.R
# 用途：Shiny 應用啟動入口；檢查套件、載入 R/、設定上傳限制與背景程序，
#       並組合全站主題、導覽頁及兩個主要模組。
# 啟動方式（請在專案根目錄執行）：
#   Rscript -e "shiny::runApp('.', host='127.0.0.1', port=3838)"
# 修改入口：
#   - 套件：required_packages、DESCRIPTION、install_dependencies.R 要同步。
#   - 色彩與字型：app_theme；細部版面在 www/styles.css。
#   - 首頁／說明文字：app_ui。
#   - 上傳計算與結果邏輯：R/mod_run.R、R/mod_results.R。
# =============================================================================

# 啟動時只檢查套件，不自動安裝；部署時也能明確暴露缺件問題。
if (dir.exists(".Rlib")) {
  .libPaths(c(".Rlib", .libPaths()))
}

required_packages <- c(
  "bslib",
  "data.table",
  "future",
  "openxlsx",
  "promises",
  "Rmpfr",
  "shiny",
  "shinybusy",
  "writexl",
  "zip"
)
missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]
if (length(missing_packages) > 0L) {
  stop(
    "缺少套件：",
    paste(missing_packages, collapse = ", "),
    "。請先執行 install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

# R/ 檔名的數字前綴代表載入順序：
# 常數 -> 工具 -> 驗證 -> I/O -> 計分 -> 彙總 -> 匯出 -> 工作 -> 模組。
source_files <- list.files(
  "R",
  pattern = "[.]R$",
  full.names = TRUE
)
invisible(lapply(sort(source_files), sys.source, envir = globalenv()))

# 可透過環境變數 SCORE_APP_MAX_UPLOAD_MB 調整上傳上限，預設 500 MB。
# 非數字或非正值會安全回退到預設值。
upload_limit_mb <- suppressWarnings(
  as.numeric(Sys.getenv("SCORE_APP_MAX_UPLOAD_MB", "500"))
)
invalid_upload_limit <- !isTRUE(
  length(upload_limit_mb) == 1L && upload_limit_mb > 0
)
if (invalid_upload_limit) {
  upload_limit_mb <- 500
}
options(shiny.maxRequestSize = upload_limit_mb * 1024^2)

# 使用一個背景程序避免計分凍結主介面；增加 worker 前須評估記憶體。
future::plan(future::multisession, workers = 1L)

# 全站 Bootstrap 5 主題；字型依序使用可用的繁體中文字型。
app_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0f766e",
  secondary = "#475569",
  success = "#15803d",
  danger = "#b91c1c",
  base_font = bslib::font_collection(
    "Noto Sans TC",
    "Microsoft JhengHei",
    "PingFang TC",
    "sans-serif"
  ),
  heading_font = bslib::font_collection(
    "Noto Sans TC",
    "Microsoft JhengHei",
    "PingFang TC",
    "sans-serif"
  )
)

# 建立「使用說明」頁的通用編號卡片。
#
# title：卡片標題；number：步驟編號；items：項目符號文字向量。
guide_card <- function(title, number, items) {
  bslib::card(
    class = "guide-card",
    bslib::card_body(
      shiny::div(
        class = "guide-number",
        number
      ),
      shiny::h3(title),
      shiny::tags$ul(
        lapply(items, shiny::tags$li)
      )
    )
  )
}

# 建立 www/examples/ 下匿名範例檔的靜態下載連結。
# filename 必須與 scripts/build_example_files.R 產生的名稱一致。
example_download_link <- function(label, filename, icon_name) {
  shiny::tags$a(
    class = "btn btn-outline-primary",
    href = paste0("examples/", filename),
    download = filename,
    shiny::icon(icon_name),
    shiny::span(label)
  )
}

# 組合三個匿名範例下載按鈕；此區塊不讀取或生成檔案。
example_download_card <- function() {
  bslib::card(
    class = "example-card",
    bslib::card_body(
      shiny::div(class = "guide-number", "5"),
      shiny::h3("匿名範例檔"),
      shiny::p(
        paste0(
          "建議先用範例確認操作流程與輸出格式。",
          "所有姓名、學校及代碼均為合成資料。"
        )
      ),
      shiny::div(
        class = "example-downloads",
        example_download_link(
          "下載答案檔",
          "115_C_ans.xlsx",
          "file-excel"
        ),
        example_download_link(
          "下載作答檔",
          "115_C4_合併_校名修正.xlsx",
          "file-excel"
        ),
        example_download_link(
          "下載批次 ZIP",
          "115_匿名範例批次.zip",
          "file-zipper"
        )
      )
    )
  )
}

# ---------------------------------------------------------------------------
# 全站 UI
# ---------------------------------------------------------------------------
# page_navbar 包含「成績分析」「結果」「使用說明」三個頁籤。
app_ui <- bslib::page_navbar(
  # 左上品牌區直接使用正式橫式識別標誌。
  title = shiny::div(
    class = "brand-lockup",
    shiny::tags$img(
      class = "brand-logo",
      src = "saaassessment_logo_horizontal_outlined.svg",
      alt = paste0(
        "縣市學生學習能力檢測，",
        "國立臺中教育大學測驗統計與適性學習研究中心"
      )
    )
  ),
  id = "main_nav",
  selected = "analysis",
  fillable = FALSE,
  theme = app_theme,
  header = shiny::tagList(
    shinybusy::add_busy_spinner(
      spin = "fading-circle",
      position = "top-right",
      color = "#0f766e",
      margin = c(15, 15)
    ),
    shinybusy::add_busy_bar(
      color = "#0f766e",
      height = "3px"
    ),
    shiny::tags$head(
      # 導覽列改用圖片後，另設可讀的瀏覽器頁籤標題。
      shiny::tags$title(
        "縣市學生學習能力檢測｜SAAA-itemAnalysis"
      ),
      # 本機工具不需要被搜尋引擎索引。
      shiny::tags$meta(
        name = "robots",
        content = "noindex,nofollow"
      ),
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css"
      )
    )
  ),
  # 第一頁：獨立資料清洗前置頁。
  bslib::nav_panel(
    "資料清洗",
    value = "cleansing",
    shiny::div(
      class = "page-shell",
      mod_cleansing_ui("cleansing")
    )
  ),
  # 第二頁：檔案整併與分卷頁。
  bslib::nav_panel(
    "檔案整併與分卷",
    value = "merge",
    shiny::div(
      class = "page-shell",
      mod_merge_ui("merge")
    )
  ),
  # 第三頁：成績分析與報表（結合設定輸入、背景計算與即時結果預覽下載）
  bslib::nav_panel(
    "成績分析與報表",
    value = "analysis",
    shiny::div(
      class = "page-shell analysis-page",
      mod_run_ui("run"),
      shiny::tags$hr(style = "margin: 35px 0; border-top: 2px solid #cbd5e1;"),
      mod_results_ui("results")
    )
  ),
  # 第四頁：操作、檔名、統計規則、隱私與匿名範例。
  bslib::nav_panel(
    "使用說明",
    value = "guide",
    shiny::div(
      class = "page-shell guide-page",
      bslib::layout_columns(
        col_widths = c(6, 6),
        guide_card(
          "單科模式",
          "1",
          c(
            "選擇年度與科目。",
            "上傳 1 份答案檔，可同時上傳多個年級的作答檔。",
            "檔案檢查通過後開始計算。"
          )
        ),
        guide_card(
          "批次模式",
          "2",
          c(
            "將答案檔與所有作答檔放進同一個 ZIP。",
            "自動解壓並配對科目與年級。"
          )
        ),
        guide_card(
          "資料清洗",
          "3",
          c(
            "單科答案檔名稱不限，內容通過驗證即可。",
            "作答檔以年度、科目代號、年級開頭，後方名稱可自訂。",
            "例如 115_C4.xlsx 或 115-C4-第一次匯出.xlsx。",
            "科目代號：C 國語、E 英語、M 數學、S 自然。"
          )
        ),
        guide_card(
          "計算與隱私",
          "4",
          c(
            "平均排除缺考及特殊生代碼 1、2、3。",
            "學生數 = 到考數 + 缺考數 + 特殊生。",
            "排名母體為所有非缺考學生。",
            "頁面只預覽彙總資料；個人資料僅存在下載報表。"
          )
        )
      ),
      example_download_card()
    )
  ),
  footer = shiny::div(
    class = "app-footer",
    "SAAA-itemAnalysis · 縣市學生學習能力檢測 · 本機與雲端通用版"
  )
)

# ---------------------------------------------------------------------------
# 全站 Server
# ---------------------------------------------------------------------------
# 串接執行模組與結果模組；計算完成後即時傳遞結果至下方報表區。
app_server <- function(input, output, session) {
  mod_cleansing_server("cleansing")
  mod_merge_server("merge")
  run_state <- mod_run_server("run")
  mod_results_server("results", run_state$result)
}

# 建立可由 runApp() 啟動的 Shiny app 物件。
shiny::shinyApp(app_ui, app_server)
