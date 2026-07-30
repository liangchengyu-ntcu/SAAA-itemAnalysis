# =============================================================================
# 檔案：R/mod_merge.R
# 用途：實作「檔案整併與分卷」獨立 Tab 頁面的多檔上傳、自動分卷、
#       分卷摘要檢視與一鍵下載整併 ZIP。
# 模組介面：
#   - mod_merge_ui(id)
#   - mod_merge_server(id)
# =============================================================================

mod_merge_ui <- function(id) {
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
            shiny::span("上傳多校分散作答檔與設定")
          )
        ),
        bslib::card_body(
          shiny::fileInput(
            ns("merge_files"),
            "選擇待整併作答檔 (支援複選 .xlsx 或 .zip)",
            multiple = TRUE,
            accept = c(".xlsx", ".zip", "application/zip")
          ),
          shiny::textInput(
            ns("year"),
            "年度代碼",
            value = "115"
          ),
          shiny::div(
            class = "button-row",
            style = "margin-top: 20px;",
            shiny::actionButton(
              ns("run_merge"),
              "開始檔案整併與分卷",
              icon = shiny::icon("folder-tree"),
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
            shiny::span("分卷整併摘要與下載")
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("merge_summary_ui")),
          shiny::div(
            style = "margin-top: 15px;",
            shiny::uiOutput(ns("download_btn_ui"))
          )
        )
      )
    )
  )
}

mod_merge_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    merge_result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run_merge, {
      if (is.null(input$merge_files)) {
        shiny::showNotification("請先選擇至少一個待整併的 .xlsx 檔或 .zip 檔。", type = "warning")
        return()
      }

      files <- input$merge_files
      target_paths <- character()

      tryCatch({
        # 若包含 ZIP 檔，自動解壓取得內含 Excel 檔
        for (i in seq_len(nrow(files))) {
          fpath <- files$datapath[i]
          fname <- files$name[i]
          if (grepl("\\.zip$", fname, ignore.case = TRUE)) {
            unzip_dir <- tempfile("unzip-merge-")
            dir.create(unzip_dir, showWarnings = FALSE)
            utils::unzip(fpath, exdir = unzip_dir)
            xl_files <- list.files(unzip_dir, pattern = "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
            target_paths <- c(target_paths, xl_files)
          } else {
            target_paths <- c(target_paths, fpath)
          }
        }

        if (length(target_paths) == 0L) {
          stop("未找到可供整併的 Excel 檔案。")
        }

        yr_val <- trimws(input$year)
        if (!nzchar(yr_val)) yr_val <- "115"

        res <- merge_and_split_files(target_paths, default_year = yr_val)
        merge_result(res)

        shiny::showNotification("檔案整併與分卷完成！", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("整併失敗：", e$message), type = "error")
      })
    })

    output$merge_summary_ui <- shiny::renderUI({
      res <- merge_result()
      if (is.null(res)) {
        return(
          shiny::div(
            class = "alert alert-info",
            shiny::icon("info-circle"),
            "請在上傳檔案後點擊「開始檔案整併與分卷」。"
          )
        )
      }

      results <- res$results
      cards <- lapply(names(results), function(k) {
        item <- results[[k]]
        shiny::div(
          style = "background-color: #f8fafc; border-left: 4px solid #0f766e; padding: 12px; margin-bottom: 10px; border-radius: 4px;",
          shiny::div(
            style = "font-weight: bold; color: #0f766e; font-size: 1.05rem;",
            sprintf("📘 分卷 [%s]（%s 年度 %s）", k, item$year, k)
          ),
          shiny::div(
            style = "color: #334155; margin-top: 4px;",
            sprintf("總人數: %d 筆 ｜ 學校數: %d 所", item$n_rows, item$n_schools)
          ),
          shiny::div(
            style = "color: #64748b; font-size: 0.9rem;",
            sprintf("縣市流水號範圍: %s", item$id_range)
          )
        )
      })

      shiny::div(
        style = "max-height: 450px; overflow-y: auto;",
        cards
      )
    })

    output$download_btn_ui <- shiny::renderUI({
      res <- merge_result()
      if (is.null(res) || is.null(res$zip_path)) return(NULL)

      shiny::downloadButton(
        session$ns("download_merge_zip"),
        "📦 下載整併與分卷結果 ZIP 檔",
        class = "btn-success"
      )
    })

    output$download_merge_zip <- shiny::downloadHandler(
      filename = function() {
        paste0(input$year, "_檔案整併與分卷結果.zip")
      },
      content = function(file) {
        res <- merge_result()
        if (!is.null(res) && !is.null(res$zip_path)) {
          file.copy(res$zip_path, file, overwrite = TRUE)
        }
      }
    )
  })
}
