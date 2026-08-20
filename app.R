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
if (dir.exists(".Rlib") && length(list.files(".Rlib")) > 0L) {
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
invisible(lapply(sort(source_files), function(f) {
  source(f, encoding = "UTF-8", local = FALSE)
}))

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
  base_font = bslib::font_google("Noto Sans TC"),
  heading_font = bslib::font_google("Noto Sans TC")
)

# 建立「使用說明」頁的通用編號卡片。
#
# title：卡片標題；number：步驟編號；icon_name：Font Awesome 圖示；
# items：項目符號文字向量（支援 HTML）；color_class：卡片色調 class。
guide_card <- function(title, number, icon_name = NULL, items = character(), color_class = "") {
  bslib::card(
    class = paste("guide-card", color_class),
    bslib::card_body(
      shiny::div(
        class = "guide-card-header",
        shiny::div(
          class = "guide-number",
          if (!is.null(icon_name)) shiny::icon(icon_name) else number
        ),
        shiny::div(
          class = "guide-card-step",
          paste0("STEP ", number)
        )
      ),
      shiny::h3(title),
      shiny::tags$ul(
        lapply(items, function(item) shiny::tags$li(shiny::HTML(item)))
      )
    )
  )
}

# 建立使用說明頁的資訊提示區塊 (帶圖示的說明欄)。
guide_info_block <- function(icon_name, title, content_html, block_class = "guide-info-block") {
  shiny::div(
    class = block_class,
    shiny::div(
      class = "guide-info-icon",
      shiny::icon(icon_name)
    ),
    shiny::div(
      class = "guide-info-body",
      shiny::tags$strong(title),
      shiny::HTML(content_html)
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
      shiny::div(
        class = "guide-card-header",
        shiny::div(class = "guide-number guide-number-example", shiny::icon("flask")),
        shiny::div(class = "guide-card-step", "範例下載")
      ),
      shiny::h3("匿名範例檔"),
      shiny::p(
        paste0(
          "建議先用範例確認操作流程與輸出格式。",
          "所有姓名、學校及代碼均為合成資料，不含任何個人資訊。"
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
# 顯式掛載 www 靜態資源目錄，確保本機與雲端皆能 100% 讀取 SVG 圖檔與 CSS 樣式。
if (dir.exists("www")) {
  shiny::addResourcePath("www", "www")
}

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
  selected = "guide",
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
  # 第三頁：成績計算與報表（結合設定輸入、背景計算與即時結果預覽下載）
  bslib::nav_panel(
    "成績計算與報表",
    value = "analysis",
    shiny::div(
      class = "page-shell analysis-page",
      mod_run_ui("run"),
      shiny::tags$hr(style = "margin: 35px 0; border-top: 2px solid #cbd5e1;"),
      mod_results_ui("results")
    )
  ),
  # 第四頁：獨立試題分析 (CTT) 頁籤
  bslib::nav_panel(
    "試題分析 (CTT)",
    value = "ctt",
    shiny::div(
      class = "page-shell ctt-page",
      mod_ctt_ui("ctt")
    )
  ),
  # 第五頁：操作、檔名、統計規則、隱私與匿名範例。
  bslib::nav_panel(
    "使用說明",
    value = "guide",
    shiny::div(
      class = "page-shell guide-page",

      # ── 頁首橫幅 ──────────────────────────────────────────────
      shiny::div(
        class = "guide-hero",
        shiny::div(
          class = "guide-hero-content",
          shiny::h1(
            class = "guide-hero-title",
            shiny::icon("book-open"),
            "使用說明"
          ),
          shiny::p(
            class = "guide-hero-subtitle",
            paste0(
              "SAAA-itemAnalysis 提供四大核心功能模組：",
              "資料清洗、檔案整併與分卷、成績計算與報表、試題分析（CTT）。",
              "請依照下列說明，按步驟完成資料處理流程。"
            )
          )
        )
      ),

      # ── 快速流程總覽 ──────────────────────────────────────────
      shiny::div(
        class = "guide-flow",
        shiny::div(
          class = "guide-flow-item",
          shiny::div(class = "guide-flow-icon", shiny::icon("wand-magic-sparkles")),
          shiny::div(class = "guide-flow-label", "① 資料清洗")
        ),
        shiny::div(class = "guide-flow-arrow", shiny::icon("chevron-right")),
        shiny::div(
          class = "guide-flow-item",
          shiny::div(class = "guide-flow-icon", shiny::icon("folder-tree")),
          shiny::div(class = "guide-flow-label", "② 檔案整併與分卷")
        ),
        shiny::div(class = "guide-flow-arrow", shiny::icon("chevron-right")),
        shiny::div(
          class = "guide-flow-item",
          shiny::div(class = "guide-flow-icon", shiny::icon("calculator")),
          shiny::div(class = "guide-flow-label", "③ 成績計算與報表")
        ),
        shiny::div(class = "guide-flow-arrow", shiny::icon("chevron-right")),
        shiny::div(
          class = "guide-flow-item",
          shiny::div(class = "guide-flow-icon", shiny::icon("chart-bar")),
          shiny::div(class = "guide-flow-label", "④ 試題分析 (CTT)")
        )
      ),

      # ── 功能模組卡片（2 × 2 格局）────────────────────────────
      shiny::tags$h2(class = "guide-section-title", "功能模組說明"),
      bslib::layout_columns(
        col_widths = c(6, 6),

        # ── 1. 資料清洗 ──
        guide_card(
          title = "資料清洗",
          number = "1",
          icon_name = "wand-magic-sparkles",
          items = c(
            "上傳原始作答檔（<code>.xlsx</code> 或 <code>.zip</code>）。",
            "<strong>自動修復</strong>：依身分證號修復性別欄、整併特教障礙欄位。",
            "<strong>學校校對</strong>：比對全台 3,519 所學校名錄，自動修正校名與代碼。",
            "<strong>縣市流水號</strong>：自動產生格式 <code>115_B_C4_000001</code> 的唯一識別碼。",
            "<strong>依科目年級拆分</strong>：一鍵將混合資料拆為 C/E/M/S × 年級 的分割檔與 ZIP。",
            "清洗完成後可下載修復報告、清洗後 Excel 或分割 ZIP。"
          ),
          color_class = "guide-card-teal"
        ),

        # ── 2. 檔案整併與分卷 ──
        guide_card(
          title = "檔案整併與分卷",
          number = "2",
          icon_name = "folder-tree",
          items = c(
            "上傳多校分散的作答檔（支援複選 <code>.xlsx</code> 或 <code>.zip</code>）。",
            "系統自動依<strong>科目代碼</strong>（C/E/M/S）與<strong>年級</strong>（3–8 年級）整合。",
            "整併後產出每個分卷的人數、學校數、縣市流水號範圍摘要。",
            "一鍵下載包含所有分卷 Excel 的整併 ZIP 檔，可直接用於成績計算。",
            "<em>適用情境</em>：各校或各縣市分批匯出後需集中統一處理的情況。"
          ),
          color_class = "guide-card-blue"
        ),

        # ── 3. 成績計算與報表 ──
        guide_card(
          title = "成績計算與報表",
          number = "3",
          icon_name = "calculator",
          items = c(
            "<strong>單科模式</strong>：選擇年度與科目，上傳 1 份答案檔並選多個年級作答檔。",
            "<strong>批次 ZIP 模式</strong>：將答案檔與所有作答檔放入同一 ZIP，系統自動配對。",
            "<strong>作答檔命名規則</strong>：以年度_科目代碼_年級開頭，例如 <code>115_C4.xlsx</code>、<code>115-C4-第一次匯出.xlsx</code>。",
            "<strong>科目代碼</strong>：C 國語、E 英語、M 數學、S 自然。",
            "<strong>精熟等級</strong>（選配）：勾選後需設定精熟與基礎門檻題數，輸出等級描述欄（精熟／基礎／待加強）。",
            "計算完成後可直接在頁面預覽彙總報表，並下載完整 Excel 成果。"
          ),
          color_class = "guide-card-green"
        ),

        # ── 4. 試題分析 (CTT) ──
        guide_card(
          title = "試題分析 (CTT)",
          number = "4",
          icon_name = "chart-bar",
          items = c(
            "支援兩種分析模式：<strong>標準三等級（精熟／基礎／待加強）</strong>與<strong>傳統 27% 高低分組</strong>。",
            "<strong>三等級模式</strong>：可按縣市篩選範圍，下載全部縣市（多工作表）或單一縣市 Excel。",
            "<strong>27% 模式</strong>：產出試題品質診斷總表（通過率、鑑別度、難度），並附誘答力明細矩陣。",
            "頂部指標卡即時顯示 Cronbach's α 信度、平均通過率、鑑別度偏低題數等關鍵指標。",
            "可在「試題分析 (CTT)」頁籤獨立上傳計算，或直接沿用「成績計算」頁的計算結果。",
            "數值可選擇四捨五入至小數後 2 位（<code>0.00</code>）輸出。"
          ),
          color_class = "guide-card-orange"
        )
      ),

      # ── 統計規則與計算說明 ────────────────────────────────────
      shiny::tags$h2(class = "guide-section-title", "統計規則說明"),
      shiny::div(
        class = "guide-rules-grid",
        guide_info_block(
          "users",
          "學生人數計算",
          "<p>學生數 = <strong>到考數</strong> + <strong>缺考數</strong> + <strong>特殊生數</strong>。<br>
          平均分數<strong>排除缺考生</strong>（代碼 1）及<strong>特殊生</strong>（代碼 2、3）。</p>"
        ),
        guide_info_block(
          "ranking",
          "排名計算",
          "<p>排名母體為<strong>所有非缺考學生</strong>（含特殊生）。<br>
          排名以縣市為單位進行計算，支援並列排名。</p>"
        ),
        guide_info_block(
          "star",
          "精熟等級門檻",
          "<p>精熟門檻題數 &gt; 基礎門檻題數 &gt; 0（三者嚴格遞增）。<br>
          答案檔可內建門檻；未內建時需在畫面手動輸入。<br>
          <strong>精熟</strong>：答對題數 ≥ 精熟門檻　<strong>基礎</strong>：答對題數 ≥ 基礎門檻　<strong>待加強</strong>：其餘。</p>"
        ),
        guide_info_block(
          "file-excel",
          "答案檔格式",
          "<p>單科模式答案檔名稱不限，內容通過驗證即可。<br>
          批次 ZIP 中的答案檔需符合系統命名規則（含年度與科目代碼）。</p>"
        ),
        guide_info_block(
          "clock-rotate-left",
          "試題分析 27% 分組",
          "<p>以全體<strong>有效學生</strong>的總分由高至低排序，取前後各 27% 作為高低分組。<br>
          鑑別度 = 高分組通過率 − 低分組通過率；建議 ≥ 0.15。</p>"
        ),
        guide_info_block(
          "shield-halved",
          "隱私保護",
          "<p>上傳檔案<strong>僅暫存於本次瀏覽器工作階段</strong>，關閉頁面後即自動刪除。<br>
          頁面預覽只顯示彙總資料；學生個人資料<strong>僅出現在下載報表</strong>中。</p>"
        )
      ),

      # ── 常見問題 / 注意事項 ───────────────────────────────────
      shiny::tags$h2(class = "guide-section-title", "常見問題與注意事項"),
      shiny::div(
        class = "guide-faq",
        shiny::div(
          class = "guide-faq-item",
          shiny::div(class = "guide-faq-q", shiny::icon("circle-question"), "作答檔命名有誤，怎麼辦？"),
          shiny::div(
            class = "guide-faq-a",
            "請確認檔名以",
            shiny::tags$code("年度_科目代碼年級"),
            "開頭，例如",
            shiny::tags$code("115_C4.xlsx"),
            "或",
            shiny::tags$code("115-M6-第一批.xlsx"),
            "。科目代碼：C 國語、E 英語、M 數學、S 自然；年級：3–8。"
          )
        ),
        shiny::div(
          class = "guide-faq-item",
          shiny::div(class = "guide-faq-q", shiny::icon("circle-question"), "批次 ZIP 內應如何整理檔案？"),
          shiny::div(
            class = "guide-faq-a",
            "將所有答案檔與作答檔放入同一個 ZIP 根目錄（或子資料夾皆可）。",
            "系統會遞迴掃描所有 .xlsx 檔並自動配對；一個 ZIP 只需上傳一次。"
          )
        ),
        shiny::div(
          class = "guide-faq-item",
          shiny::div(class = "guide-faq-q", shiny::icon("circle-question"), "勾選精熟等級後顯示「門檻留空」錯誤？"),
          shiny::div(
            class = "guide-faq-a",
            "若答案檔未內建門檻欄，請在畫面「精熟門檻題數」與「基礎門檻題數」欄位手動填入數值。",
            "批次模式下，各卷別若有未設定門檻，會在右側彈出逐卷手動輸入介面。"
          )
        ),
        shiny::div(
          class = "guide-faq-item",
          shiny::div(class = "guide-faq-q", shiny::icon("circle-question"), "試題分析與成績計算可以分開使用嗎？"),
          shiny::div(
            class = "guide-faq-a",
            "可以。「試題分析 (CTT)」頁籤上方有獨立的執行區域，可單獨上傳計算；",
            "也可在「成績計算與報表」完成後，直接切換至試題分析頁籤沿用同一份計算結果。"
          )
        ),
        shiny::div(
          class = "guide-faq-item",
          shiny::div(class = "guide-faq-q", shiny::icon("circle-question"), "上傳後頁面重新整理，資料還在嗎？"),
          shiny::div(
            class = "guide-faq-a",
            "不在。",
            shiny::tags$strong("所有上傳資料僅存於當次瀏覽器工作階段，"),
            "重新整理頁面後即清除，需重新上傳。這是本系統的隱私保護設計。"
          )
        )
      ),

      # ── 匿名範例下載 ──────────────────────────────────────────
      shiny::tags$h2(class = "guide-section-title", "匿名範例檔下載"),
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
# 串接執行模組與結果模組；計算完成後即時傳遞結果至下方報表區與 CTT 頁籤。
app_server <- function(input, output, session) {
  mod_cleansing_server("cleansing")
  mod_merge_server("merge")
  run_state <- mod_run_server("run")
  mod_results_server("results", run_state$result)
  mod_ctt_server("ctt", main_run_result = run_state$result)
}

# 建立可由 runApp() 啟動的 Shiny app 物件。
shiny::shinyApp(app_ui, app_server)
