# =============================================================================
# 檔案：R/mod_cleansing.R
# 用途：實作「資料清洗」獨立 Tab 頁面的上傳、規則設定、清洗觸發、
#       修復診斷卡片展示、乾淨資料預覽與 Excel 下載。
# 模組介面：
#   - mod_cleansing_ui(id)
#   - mod_cleansing_server(id)
# =============================================================================

mod_cleansing_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        class = "control-card",
        bslib::card_header(
          shiny::div(
            class = "section-heading",
            shiny::span(class = "step-badge", "1"),
            shiny::span("上傳原始檔與設定清洗規則")
          )
        ),
        bslib::card_body(
          shiny::fileInput(
            ns("raw_file"),
            "選擇原始作答檔 (Excel 或 ZIP)",
            accept = c(".xlsx", ".zip", "application/zip")
          ),
          shiny::tags$hr(),
          shiny::h5("資料清洗規則與對照選項"),
          shiny::checkboxInput(
            ns("opt_fix_gender"),
            "依身分證號自動修復錯置與補齊性別",
            value = TRUE
          ),
          shiny::checkboxInput(
            ns("opt_consolidate_special"),
            "自動整併特教障礙欄位至「特殊生」",
            value = TRUE
          ),
          shiny::checkboxInput(
            ns("opt_match_school"),
            "校對學校代碼與校名 (全台 3,519 所學校名錄)",
            value = TRUE
          ),
          shiny::checkboxInput(
            ns("opt_gen_county_id"),
            "依縣市名稱自動編排「縣市流水號」（如 115_B_C4_000001）",
            value = TRUE
          ),
          shiny::checkboxInput(
            ns("opt_split_by_subject"),
            "自動依「科目代碼 (C/E/M/S)」與「年級 (3~8)」拆分分割檔與 ZIP",
            value = TRUE
          ),
          shiny::div(
            class = "button-row",
            style = "margin-top: 20px;",
            shiny::actionButton(
              ns("run_cleansing"),
              "開始資料清洗",
              icon = shiny::icon("wand-magic-sparkles"),
              class = "btn-primary w-100"
            )
          )
        )
      ),
      bslib::card(
        class = "preview-card",
        bslib::card_header(
          shiny::div(
            class = "section-heading",
            shiny::span(class = "step-badge", "2"),
            shiny::span("清洗修復報告與預覽")
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("diagnostic_ui")),
          shiny::div(
            style = "margin-top: 15px;",
            shiny::uiOutput(ns("download_btn_ui"))
          ),
          shiny::div(
            class = "table-scroll",
            style = "margin-top: 15px;",
            shiny::tableOutput(ns("preview_table"))
          )
        )
      )
    )
  )
}

mod_cleansing_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    cleaned_result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run_cleansing, {
      if (is.null(input$raw_file)) {
        shiny::showNotification("請先上傳原始檔 (.xlsx 或 .zip)。", type = "warning")
        return()
      }

      file_path <- input$raw_file$datapath
      file_name <- input$raw_file$name

      tryCatch({
        # 讀取 Excel 或 ZIP 內第一張表
        if (grepl("\\.zip$", file_name, ignore.case = TRUE)) {
          unzip_dir <- tempfile("unzip-clean-")
          dir.create(unzip_dir, showWarnings = FALSE)
          utils::unzip(file_path, exdir = unzip_dir)
          excel_files <- list.files(unzip_dir, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
          if (length(excel_files) == 0L) {
            stop("ZIP 內找不到 Excel 檔案。")
          }
          target_excel <- excel_files[[1L]]
        } else {
          target_excel <- file_path
        }

        df <- openxlsx::read.xlsx(target_excel, sheet = 1, colNames = TRUE)

        options <- list(
          fix_gender = isTRUE(input$opt_fix_gender),
          consolidate_special = isTRUE(input$opt_consolidate_special),
          match_school = isTRUE(input$opt_match_school),
          gen_county_id = isTRUE(input$opt_gen_county_id),
          split_by_subject = isTRUE(input$opt_split_by_subject)
        )

        res <- clean_response_data(df, options = options)
        cleaned_result(res)

        shiny::showNotification("資料清洗與拆分完成！", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("清洗失敗：", e$message), type = "error")
      })
    })

    output$diagnostic_ui <- shiny::renderUI({
      res <- cleaned_result()
      if (is.null(res)) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            "請在上傳檔案後點擊「開始資料清洗」。"
          )
        )
      }

      logs <- res$logs
      if (length(logs) == 0L) {
        log_items <- shiny::tags$li("✔ 原始資料極為標準，未發現需自動修復之異常項目。")
      } else {
        log_items <- lapply(logs, function(msg) shiny::tags$li(msg))
      }

      shiny::div(
        style = "background-color: #f0fdf4; border: 1px solid #bbf7d0; padding: 15px; border-radius: 8px;",
        shiny::div(
          style = "font-weight: bold; color: #166534; font-size: 1.05rem; margin-bottom: 8px;",
          shiny::icon("wand-magic-sparkles"),
          " 資料自動清洗與修復報告"
        ),
        shiny::tags$ul(
          style = "margin-bottom: 0; padding-left: 20px; color: #15803d;",
          log_items
        )
      )
    })

    output$download_btn_ui <- shiny::renderUI({
      res <- cleaned_result()
      if (is.null(res)) return(NULL)

      has_split <- !is.null(res$split_result) && !is.null(res$split_result$zip_path)

      shiny::div(
        style = "display: flex; gap: 10px;",
        shiny::downloadButton(
          session$ns("download_clean"),
          "下載清洗後 Excel 檔",
          class = "btn-success"
        ),
        if (has_split) {
          shiny::downloadButton(
            session$ns("download_split_zip"),
            "📦 下載依科目與年級分割 ZIP 檔",
            class = "btn-primary"
          )
        }
      )
    })

    output$download_clean <- shiny::downloadHandler(
      filename = function() {
        raw_name <- if (!is.null(input$raw_file$name)) input$raw_file$name else "file"
        paste0("cleaned_", gsub("\\.xlsx$", "", raw_name), ".xlsx")
      },
      content = function(file) {
        res <- cleaned_result()
        if (!is.null(res)) {
          openxlsx::write.xlsx(res$cleaned_df, file)
        }
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    output$download_split_zip <- shiny::downloadHandler(
      filename = function() {
        "115_依科目年級分割作答檔.zip"
      },
      content = function(file) {
        res <- cleaned_result()
        if (!is.null(res) && !is.null(res$split_result$zip_path)) {
          file.copy(res$split_result$zip_path, file, overwrite = TRUE)
        }
      },
      contentType = "application/zip"
    )

    output$preview_table <- shiny::renderTable({
      res <- cleaned_result()
      if (is.null(res)) return(NULL)
      head(res$cleaned_df, 20L)
    }, rownames = FALSE)
  })
}
