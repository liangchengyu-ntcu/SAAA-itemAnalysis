# =============================================================================
# 檔案：R/mod_results.R
# 用途：實作「結果」頁，包括工作狀態、摘要卡、分布圖、報表預覽與下載。
# 資料來源：mod_run_server() 回傳的 run_result reactive。
# 修改入口：
#   - 摘要卡：output$summary_cards 與 metric_tile()。
#   - 圖表：output$distribution_plot。
#   - 預覽列數／格式：output$result_table。
#   - ZIP 檔名或下載流程：output$download_all。
# 隱私原則：頁面只提供六張彙總表，含個資明細只存在下載 Excel。
# =============================================================================

# 建立結果頁的兩個動態容器。
#
# 尚無結果時顯示 empty_state；有結果後改顯示 result_content。
# 使用 uiOutput() 是因為整個區塊會依背景工作的完成狀態建立或移除。
mod_results_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("empty_state")),
    shiny::uiOutput(ns("result_content"))
  )
}

# 將批次執行結果轉成可互動的預覽與下載介面。
#
# id：模組識別字。
# run_result：reactive expression；成功完成時回傳 run_job_batch() 結果。
mod_results_server <- function(id, run_result) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 失敗工作仍顯示在狀態表，但不能成為預覽下拉選項。
    successful_keys <- shiny::reactive({
      result <- run_result()
      if (is.null(result)) {
        return(character())
      }
      names(Filter(
        function(item) identical(item$status, "完成"),
        result$results
      ))
    })

    # 每次新批次結果抵達時，重建可預覽工作選項並預選第一個成功工作。
    shiny::observeEvent(run_result(), {
      result <- run_result()
      if (is.null(result)) {
        return()
      }

      keys <- successful_keys()
      choices <- stats::setNames(
        keys,
        vapply(
          result$results[keys],
          `[[`,
          character(1),
          "label"
        )
      )
      shiny::updateSelectInput(
        session,
        "selected_job",
        choices = choices,
        selected = if (length(keys) > 0L) keys[[1L]] else character()
      )
    })

    # 集中解析目前選定工作，讓摘要卡、圖與表格共用同一 reactive。
    selected_result <- shiny::reactive({
      result <- run_result()
      shiny::req(!is.null(result))
      shiny::req(input$selected_job)
      selected <- result$results[[input$selected_job]]
      shiny::req(!is.null(selected))
      selected
    })

    # 尚未執行任何分析時的提示畫面。
    output$empty_state <- shiny::renderUI({
      if (!is.null(run_result())) {
        return(NULL)
      }
      shiny::div(
        class = "empty-state",
        shiny::icon("chart-column"),
        shiny::h3("尚無計算結果"),
        shiny::p("完成「檢查檔案」及「開始計算」後，結果會顯示在這裡。")
      )
    })

    # 有結果後一次建立工作狀態卡、預覽控制、分布圖與報表表格。
    output$result_content <- shiny::renderUI({
      result <- run_result()
      if (is.null(result)) {
        return(NULL)
      }

      # 批次可部分失敗；只要至少一個工作產生檔案就顯示下載按鈕。
      has_download <- any(
        vapply(
          result$results,
          function(item) length(item$exported_files) > 0L,
          logical(1)
        )
      )

      shiny::tagList(
        # 第一張卡顯示每個工作的成功／失敗與耗時。
        bslib::card(
          bslib::card_header(
            shiny::div(
              class = "result-header",
              shiny::div(
                class = "section-heading",
                shiny::span(class = "step-badge", "3"),
                shiny::span("執行結果")
              ),
              if (has_download) {
                shiny::downloadButton(
                  ns("download_all"),
                  "下載全部 Excel",
                  icon = shiny::icon("file-zipper"),
                  class = "btn-primary"
                )
              }
            )
          ),
          bslib::card_body(
            shiny::div(
              class = "table-scroll",
              shiny::tableOutput(ns("job_status"))
            )
          )
        ),
        # 只有存在成功工作時才建立下方預覽區。
        shiny::conditionalPanel(
          condition = sprintf(
            "output['%s']",
            ns("has_success")
          ),
          bslib::layout_columns(
            col_widths = c(4, 8),
            bslib::card(
              bslib::card_header("選擇預覽"),
              bslib::card_body(
                shiny::selectInput(
                  ns("selected_job"),
                  "計算工作（年度／科目／年級）",
                  choices = character()
                ),
                shiny::selectInput(
                  ns("selected_view"),
                  "預覽輸出報表",
                  choices = RESULT_VIEWS,
                  selected = "total"
                ),
                shiny::div(
                  class = "preview-note",
                  shiny::icon("circle-info"),
                  shiny::span(
                    paste0(
                      "預覽與下載 Excel 使用同一份資料；",
                      "頁面最多顯示前 100 列。"
                    )
                  )
                ),
                shiny::uiOutput(ns("summary_cards"))
              )
            ),
            bslib::card(
              full_screen = TRUE,
              bslib::card_header("分數分布"),
              bslib::card_body(
                shiny::plotOutput(
                  ns("distribution_plot"),
                  height = "280px"
                )
              )
            )
          ),
          # 表格內容會依「預覽輸出報表」下拉選單即時切換。
          bslib::card(
            full_screen = TRUE,
            bslib::card_header(
              "輸出報表預覽（與下載 Excel 內容一致）"
            ),
            bslib::card_body(
              shiny::div(
                class = "table-scroll result-table",
                shiny::tableOutput(ns("result_table"))
              )
            )
          )
        )
      )
    })

    # conditionalPanel 在瀏覽器端讀取此輸出，因此即使頁籤隱藏也不能暫停。
    output$has_success <- shiny::reactive({
      length(successful_keys()) > 0L
    })
    shiny::outputOptions(
      output,
      "has_success",
      suspendWhenHidden = FALSE
    )

    # 顯示所有工作（含失敗工作）的結果與分段耗時。
    output$job_status <- shiny::renderTable(
      {
        result <- run_result()
        shiny::req(!is.null(result))
        result$job_table
      },
      bordered = FALSE,
      striped = TRUE,
      hover = TRUE,
      spacing = "s"
    )

    output$summary_cards <- shiny::renderUI({
      selected <- selected_result()
      alpha_val <- selected$cronbach_alpha
      alpha_str <- if (!is.null(alpha_val) && !is.na(alpha_val)) sprintf("%.2f", alpha_val) else "N/A"

      shiny::div(
        class = "metric-grid",
        metric_tile("學生數", selected$student_count),
        metric_tile("到考數", selected$attended_count),
        metric_tile("缺考數", selected$absent_count),
        metric_tile("特殊生", selected$special_count),
        metric_tile("Cronbach's α", alpha_str)
      )
    })

    # 繪製答對率離散分布；0.6 以上以主色顯示，僅是視覺提示。
    # 此顏色門檻不參與任何成績判定或報表計算。
    output$distribution_plot <- shiny::renderPlot({
      distribution <- selected_result()$distribution
      shiny::validate(
        shiny::need(
          nrow(distribution) > 0L,
          "沒有可繪製的成績資料。"
        )
      )

      bar_colors <- ifelse(
        distribution$答對率 >= 0.6,
        "#0f766e",
        "#94a3b8"
      )
      graphics::par(mar = c(5.5, 5.2, 1.5, 1))
      graphics::barplot(
        height = distribution$人數,
        names.arg = format(
          distribution$答對率,
          trim = TRUE,
          digits = 4
        ),
        col = bar_colors,
        border = NA,
        xlab = "總答對率",
        ylab = "人數",
        las = 2,
        cex.names = 0.75
      )
      graphics::grid(
        nx = NA,
        ny = NULL,
        col = "#e2e8f0",
        lty = 1
      )
    })

    # 預覽直接使用匯出表物件，最多顯示前 100 列以控制瀏覽器負擔。
    # 完整資料不受此限制，仍會寫入下載 Excel。
    output$result_table <- shiny::renderTable(
      {
        selected <- selected_result()
        view <- input$selected_view
        if (is.null(view) || !nzchar(view)) {
          view <- "total"
        }
        utils::head(selected$views[[view]], 100L)
      },
      bordered = FALSE,
      striped = TRUE,
      hover = TRUE,
      spacing = "s",
      digits = 6
    )

    # 將本 session 的成功輸出重新封裝為單一下載 ZIP。
    output$download_all <- shiny::downloadHandler(
      filename = function() {
        result <- run_result()
        year <- if (is.null(result)) {
          format(Sys.Date(), "%Y%m%d")
        } else {
          unique_years <- unique(vapply(
            result$results,
            `[[`,
            character(1),
            "year"
          ))
          paste(unique_years, collapse = "-")
        }
        # 批次若含多年度，以連字號串接年度；目前介面通常只有一個年度。
        paste0(year, "_成績分析結果.zip")
      },
      content = function(file) {
        result <- run_result()
        shiny::req(!is.null(result))
        archive <- create_result_archive(
          result$output_root,
          tempfile(fileext = ".zip")
        )
        copied <- file.copy(
          archive,
          file,
          overwrite = TRUE
        )
        if (!copied) {
          abort_score("無法建立下載檔。")
        }
      },
      contentType = "application/zip"
    )
  })
}

# 建立一張摘要數值卡；樣式由 www/styles.css 的 metric-* 類別控制。
metric_tile <- function(label, value) {
  shiny::div(
    class = "metric-tile",
    shiny::span(class = "metric-value", value),
    shiny::span(class = "metric-label", label)
  )
}
