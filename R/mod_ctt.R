# =============================================================================
# 檔案：R/mod_ctt.R
# 用途：獨立「試題分析 (CTT)」頁籤模組
# 功能：包含檔案上傳區 (mod_run_ui)、試題品質診斷卡、試題分析總表、誘答力明細矩陣與 CTT Excel 專屬下載。
# =============================================================================

mod_ctt_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    mod_run_ui(ns("ctt_run")),
    shiny::tags$hr(style = "margin: 35px 0; border-top: 2px solid #cbd5e1;"),
    shiny::uiOutput(ns("empty_state")),
    shiny::uiOutput(ns("ctt_content"))
  )
}

mod_ctt_server <- function(id, main_run_result = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ctt_run_state <- mod_run_server("ctt_run")

    effective_run_result <- shiny::reactive({
      ctt_res <- ctt_run_state$result()
      if (!is.null(ctt_res)) return(ctt_res)
      main_res <- main_run_result()
      if (!is.null(main_res)) return(main_res)
      NULL
    })

    successful_keys <- shiny::reactive({
      result <- effective_run_result()
      if (is.null(result)) return(character())
      names(Filter(function(item) identical(item$status, "完成"), result$results))
    })

    shiny::observeEvent(effective_run_result(), {
      result <- effective_run_result()
      if (is.null(result)) return()
      keys <- successful_keys()
      choices <- stats::setNames(
        keys,
        vapply(result$results[keys], `[[`, character(1), "label")
      )
      shiny::updateSelectInput(
        session,
        "selected_job",
        choices = choices,
        selected = if (length(keys) > 0L) keys[[1L]] else character()
      )
    })

    selected_job_result <- shiny::reactive({
      result <- effective_run_result()
      shiny::req(!is.null(result))
      shiny::req(input$selected_job)
      selected <- result$results[[input$selected_job]]
      shiny::req(!is.null(selected))
      selected
    })

    output$empty_state <- shiny::renderUI({
      if (!is.null(effective_run_result())) return(NULL)
      shiny::div(
        class = "empty-state",
        shiny::icon("file-circle-plus"),
        shiny::h3("請上傳試題分析檔案"),
        shiny::p("請在上方的「選擇執行方式」中選擇答案檔與作答檔，點擊「檢查檔案」與「開始計算」即可產出試題品質診斷與誘答力分析。")
      )
    })

    output$ctt_content <- shiny::renderUI({
      result <- effective_run_result()
      if (is.null(result)) return(NULL)

      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "試題分析設定",
          width = 300,
          shiny::selectInput(
            ns("selected_job"),
            "選擇分析工作項目",
            choices = character()
          ),
          shiny::tags$hr(),
          shiny::downloadButton(
            ns("download_ctt_excel"),
            "下載 CTT 試題分析總表 (.xlsx)",
            class = "btn-success w-100 mb-2"
          ),
          shiny::downloadButton(
            ns("download_distractor_excel"),
            "下載誘答力明細表 (.xlsx)",
            class = "btn-outline-primary w-100"
          )
        ),
        shiny::div(
          class = "ctt-results-container",
          shiny::uiOutput(ns("ctt_metrics")),
          shiny::tags$br(),
          bslib::card(
            bslib::card_header(
              shiny::div(
                class = "d-flex justify-content-between align-items-center",
                shiny::span("📊 CTT 試題品質診斷總表"),
                shiny::span(class = "badge bg-info", "高低分組 27% 臨界法")
              )
            ),
            bslib::card_body(
              shiny::div(
                class = "table-scroll result-table",
                shiny::tableOutput(ns("ctt_summary_table"))
              )
            )
          ),
          shiny::tags$br(),
          bslib::card(
            bslib::card_header("🔍 逐項誘答力分析矩陣 (Distractor Analysis)"),
            bslib::card_body(
              shiny::div(
                class = "table-scroll result-table",
                shiny::tableOutput(ns("distractor_matrix_table"))
              )
            )
          )
        )
      )
    })

    # CTT 頂部核心指標卡
    output$ctt_metrics <- shiny::renderUI({
      selected <- selected_job_result()
      ctt <- selected$analysis$ctt_analysis
      summary_df <- ctt$item_summary

      alpha_val <- if (!is.null(ctt$alpha) && !is.na(ctt$alpha)) sprintf("%.2f", ctt$alpha) else "N/A"
      n_items <- ctt$n_items

      pass_rates <- suppressWarnings(as.numeric(summary_df$通過率))
      avg_pass <- sprintf("%.2f%%", mean(pass_rates, na.rm = TRUE) * 100)

      disc_vals <- suppressWarnings(as.numeric(summary_df$鑑別度))
      avg_disc <- sprintf("%.2f", mean(disc_vals, na.rm = TRUE))
      low_disc_count <- sum(!is.na(disc_vals) & disc_vals < 0.15)

      shiny::div(
        class = "metric-grid",
        metric_tile("Cronbach's α 信度", alpha_val),
        metric_tile("試題總數", n_items),
        metric_tile("平均通過率", avg_pass),
        metric_tile("平均鑑別度", avg_disc),
        metric_tile("鑑別度偏低題數 (<0.15)", low_disc_count)
      )
    })

    # 輸出 CTT 試題診斷總表
    output$ctt_summary_table <- shiny::renderTable({
      selected <- selected_job_result()
      ctt <- selected$analysis$ctt_analysis
      ctt$item_summary
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

    # 輸出誘答力矩陣表
    output$distractor_matrix_table <- shiny::renderTable({
      selected <- selected_job_result()
      ctt <- selected$analysis$ctt_analysis
      ctt$distractor_results
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

    # 下載 CTT 試題分析總表 Excel
    output$download_ctt_excel <- shiny::downloadHandler(
      filename = function() {
        selected <- selected_job_result()
        job <- selected$prepared$job
        sprintf("%s_%s%s_試題分析總表.xlsx", job$year, job$subject_code, job$grade)
      },
      content = function(file) {
        selected <- selected_job_result()
        job <- selected$prepared$job
        ctt <- selected$analysis$ctt_analysis
        grade_map <- setNames(list(ctt), as.character(job$grade))
        write_ctt_analysis_by_subject(
          subject_label = job$subject_name,
          year = job$year,
          output_path = file,
          grade_ctt_map = grade_map
        )
      }
    )

    # 下載誘答力明細表 Excel
    output$download_distractor_excel <- shiny::downloadHandler(
      filename = function() {
        selected <- selected_job_result()
        job <- selected$prepared$job
        sprintf("%s_%s%s_分析結果.xlsx", job$year, job$subject_code, job$grade)
      },
      content = function(file) {
        selected <- selected_job_result()
        job <- selected$prepared$job
        ctt <- selected$analysis$ctt_analysis
        write_distractor_analysis_xlsx(
          ctt_res = ctt,
          subject_label = job$subject_name,
          grade = job$grade,
          output_path = file
        )
      }
    )
  })
}
