# =============================================================================
# 檔案：R/mod_run.R
# 用途：實作「成績分析」頁的上傳、檢查、背景計算與狀態訊息。
# 模組介面：
#   - mod_run_ui(id)：建立具命名空間的畫面。
#   - mod_run_server(id)：管理工作階段，回傳 result reactive 給結果模組。
# 修改入口：
#   - 調整上傳欄位／按鈕文字：mod_run_ui()。
#   - 調整檢查及配對流程：prepare_files 的 observeEvent。
#   - 調整背景執行：ExtendedTask 與 run_analysis 的 observeEvent。
# 隱私原則：所有上傳與輸出只放在 session_root，工作階段結束即刪除。
# =============================================================================

# 建立檔案上傳與工作預覽介面。
#
# id：模組識別字；所有 input/output ID 都必須經 ns() 包裝。
# 回傳值：Shiny tagList，可放入 app.R 的第一個導覽頁。
mod_run_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # 左欄為輸入控制，右欄為檢查後的工作清單。
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        class = "control-card",
        bslib::card_header(
          shiny::div(
            class = "section-heading",
            shiny::span(class = "step-badge", "1"),
            shiny::span("選擇執行方式")
          )
        ),
        bslib::card_body(
          shiny::radioButtons(
            ns("mode"),
            "模式",
            choices = c(
              "單科（可同時選多個年級）" = "single",
              "整批 ZIP" = "batch"
            ),
            selected = "single"
          ),
          shiny::textInput(
            ns("year"),
            "年度",
            value = "115",
            placeholder = "例如：115"
          ),
          # 單科模式：共用一份答案檔，可一次上傳多個年級作答檔。
          shiny::conditionalPanel(
            condition = sprintf(
              "input['%s'] === 'single'",
              ns("mode")
            ),
            shiny::selectInput(
              ns("subject_code"),
              "科目",
              choices = stats::setNames(
                names(SUBJECT_NAMES),
                unname(SUBJECT_NAMES)
              ),
              selected = "C"
            ),
            shiny::fileInput(
              ns("answer_file"),
              "答案檔",
              accept = ".xlsx"
            ),
            shiny::fileInput(
              ns("response_files"),
              "作答檔（可複選不同年級）",
              multiple = TRUE,
              accept = c(".xlsx", ".csv", "text/csv", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
            )
          ),
          # 批次模式：只接收一個內含答案檔及作答檔的 ZIP。
          shiny::conditionalPanel(
            condition = sprintf(
              "input['%s'] === 'batch'",
              ns("mode")
            ),
            shiny::fileInput(
              ns("batch_zip"),
              "批次 ZIP",
              accept = c(".zip", "application/zip")
            )
          ),
          shiny::tags$hr(),
          shiny::checkboxInput(
            ns("calc_level"),
            "計算精熟等級（等級描述：精熟 / 基礎 / 待加強）",
            value = FALSE
          ),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] === true", ns("calc_level")),
            shiny::div(
              style = "margin-bottom: 15px; padding: 12px; background-color: #f8f9fa; border-radius: 6px; border: 1px solid #dee2e6;",
              shiny::numericInput(
                ns("mastery_cutoff"),
                "精熟門檻題數（答對題數 ≥）",
                value = NA,
                min = 1,
                step = 1
              ),
              shiny::numericInput(
                ns("basic_cutoff"),
                "基礎門檻題數（答對題數 ≥）",
                value = NA,
                min = 1,
                step = 1
              )
            )
          ),
          # 先檢查再計算；run_analysis 使用 task button 顯示忙碌狀態。
          shiny::div(
            class = "button-row",
            shiny::actionButton(
              ns("prepare_files"),
              "檢查檔案",
              icon = shiny::icon("clipboard-check"),
              class = "btn-outline-primary"
            ),
            bslib::input_task_button(
              ns("run_analysis"),
              "開始計算",
              label_busy = "計算中…",
              icon = shiny::icon("play"),
              icon_busy = shiny::icon("spinner"),
              type = "primary"
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
            shiny::span("確認工作清單")
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("status_message")),
          shiny::div(
            class = "table-scroll",
            shiny::tableOutput(ns("job_preview"))
          )
        )
      )
    ),
    shiny::div(
      class = "privacy-note",
      shiny::icon("shield-halved"),
      shiny::span(
        "檔案只暫存在本次瀏覽器工作階段；關閉工作階段後即刪除。"
      )
    )
  )
}

# 驗證畫面輸入的年度，只接受單一純數字字串並回傳去空白結果。
# 這裡不限定三位數，保留未來民國年度格式彈性。
validate_year_input <- function(year) {
  year <- trimws(as.character(year))
  if (length(year) != 1L || !grepl("^[0-9]+$", year)) {
    abort_score("年度必須是純數字，例如 115。")
  }
  year
}

# 管理上傳、檔案檢查、背景工作與狀態顯示。
#
# 回傳 list：
#   result：成功完成後的批次結果 reactive，未完成時為 NULL。
#   task_status：背景工作狀態 reactive。
#   staged_jobs／staged_preview：供測試或其他模組查詢的暫存狀態。
mod_run_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # 每個瀏覽器 session 都有獨立暫存根目錄，彼此不共享上傳資料。
    session_root <- tempfile("score-shiny-session-")
    dir.create(session_root, recursive = TRUE, showWarnings = FALSE)

    # staged_jobs 只有在全部工作通過驗證後才會存入可執行 job。
    staged_jobs <- shiny::reactiveVal(NULL)
    staged_preview <- shiny::reactiveVal(empty_job_table())
    # 控管狀態訊息顯示，避免重新上傳時畫面仍殘留上一次成功／失敗訊息。
    # 這裡的關鍵是在使用者按「檢查檔案」時切回 TRUE，讓使用者明確看到最新
    # 訊息而非上一次工作結果。
    show_staging_status <- shiny::reactiveVal(TRUE)
    stage_message <- shiny::reactiveVal(
      list(type = "info", text = "請先選擇檔案並按「檢查檔案」。")
    )

    # 無論正常關閉或連線中斷，都清除該 session 的輸入與輸出。
    session$onSessionEnded(function() {
      if (dir.exists(session_root)) {
        unlink(session_root, recursive = TRUE, force = TRUE)
      }
    })

    # ExtendedTask 將耗時計算移到 future 背景程序，避免凍結 Shiny 介面。
    # 這個函式只能接收可序列化資料，不可直接使用 input、output、session。
    task <- shiny::ExtendedTask$new(function(jobs, output_root) {
      promises::future_promise({
        run_job_batch(jobs, output_root)
      })
    })
    bslib::bind_task_button(
      task,
      "run_analysis",
      session = session
    )

    # 「檢查檔案」只做暫存、配對與驗證，不會開始正式計分。
    shiny::observeEvent(input$prepare_files, {
      # 每次重查都先清除前一次可執行狀態，避免用舊檔誤算。
      show_staging_status(TRUE)
      staged_jobs(NULL)
      staged_preview(empty_job_table())
      stage_message(list(type = "info", text = "正在檢查檔案…"))

      tryCatch(
        {
          year <- validate_year_input(input$year)

          calc_level <- isTRUE(input$calc_level)
          mastery_cutoff <- if (calc_level) as.numeric(input$mastery_cutoff) else NA_real_
          basic_cutoff <- if (calc_level) as.numeric(input$basic_cutoff) else NA_real_

          if (calc_level) {
            if (is.na(mastery_cutoff) || is.na(basic_cutoff)) {
              abort_score("勾選「計算精熟等級」時，必須輸入「精熟門檻題數」與「基礎門檻題數」。")
            }
            if (mastery_cutoff <= basic_cutoff) {
              abort_score("精熟門檻題數（", mastery_cutoff, "）必須大於基礎門檻題數（", basic_cutoff, "）。")
            }
            if (basic_cutoff <= 0) {
              abort_score("基礎門檻題數必須大於 0。")
            }
          }

          # 一次檢查使用一個新子目錄，避免新舊上傳同名時互相干擾。
          staging_directory <- tempfile(
            "input-",
            tmpdir = session_root
          )
          dir.create(
            staging_directory,
            recursive = TRUE,
            showWarnings = FALSE
          )

          if (identical(input$mode, "single")) {
            # Shiny 的 datapath 是暫時名稱，先以原檔名複製到 session 目錄。
            answer_paths <- stage_uploaded_files(
              input$answer_file,
              file.path(staging_directory, "answers")
            )
            if (length(answer_paths) != 1L) {
              abort_score("單科模式只能提供一個答案檔。")
            }
            response_paths <- stage_uploaded_files(
              input$response_files,
              file.path(staging_directory, "responses")
            )
            jobs <- discover_single_jobs(
              answer_path = answer_paths[[1L]],
              response_paths = response_paths,
              year = year,
              subject_code = input$subject_code,
              calc_level = calc_level,
              mastery_cutoff = mastery_cutoff,
              basic_cutoff = basic_cutoff
            )
          } else {
            # 批次 ZIP 先安全解壓，再掃描所有子資料夾自動配對。
            zip_paths <- stage_uploaded_files(
              input$batch_zip,
              file.path(staging_directory, "archive")
            )
            if (length(zip_paths) != 1L) {
              abort_score("批次模式只能提供一個 ZIP。")
            }
            extracted_directory <- extract_zip_safely(
              zip_paths[[1L]],
              file.path(staging_directory, "extracted")
            )
            jobs <- discover_batch_jobs(
              extracted_directory,
              year
            )
          }

          # 逐工作驗證答案欄、工作表與作答檔最低結構。
          preview <- preview_jobs(jobs)
          staged_preview(preview)
          if (any(preview$狀態 != "可執行")) {
            stage_message(list(
              type = "danger",
              text = "部分檔案未通過檢查，請修正後重新上傳。"
            ))
          } else {
            # 僅當全部工作都可執行時才開放「開始計算」的資料來源。
            staged_jobs(jobs)
            stage_message(list(
              type = "success",
              text = paste0(
                "已通過檢查，共 ",
                length(jobs),
                " 個工作，可開始計算。"
              )
            ))
          }
        },
        error = function(error) {
          # 上傳、解壓或配對任一步出錯，都顯示可讀訊息並禁止執行。
          staged_jobs(NULL)
          stage_message(list(
            type = "danger",
            text = compact_error(error)
          ))
        }
      )
    })

    # 「開始計算」為背景工作的唯一入口。
    shiny::observeEvent(input$run_analysis, {
      jobs <- staged_jobs()
      if (is.null(jobs) || length(jobs) == 0L) {
        shiny::showNotification(
          "請先完成檔案檢查。",
          type = "warning"
        )
        return()
      }

      output_root <- tempfile(
        "result-",
        tmpdir = session_root
      )
      dir.create(
        output_root,
        recursive = TRUE,
        showWarnings = FALSE
      )
      # 切換訊息來源，讓畫面顯示背景工作的 running/success/error。
      show_staging_status(FALSE)
      task$invoke(jobs, output_root)
    })

    # 工作預覽表在未上傳時也保有固定欄名。
    output$job_preview <- shiny::renderTable(
      staged_preview(),
      bordered = FALSE,
      striped = TRUE,
      hover = TRUE,
      spacing = "s"
    )

    # 依背景工作狀態或檔案檢查狀態組成同一個狀態橫幅。
    output$status_message <- shiny::renderUI({
      task_status <- task$status()
      if (identical(task_status, "running")) {
        return(shiny::div(
          class = "status-banner status-running",
          shiny::icon("spinner"),
          "正在背景計算，頁面仍可操作。"
        ))
      }
      show_task_success <- identical(task_status, "success") &&
        !show_staging_status()
      if (show_task_success) {
        # 批次允許部分成功，因此同時顯示成功與失敗工作數。
        result <- task$result()
        succeeded <- sum(result$job_table$狀態 == "完成")
        failed <- sum(result$job_table$狀態 == "失敗")
        return(shiny::div(
          class = if (failed == 0L) {
            "status-banner status-success"
          } else {
            "status-banner status-warning"
          },
          shiny::icon(
            if (failed == 0L) "circle-check" else "triangle-exclamation"
          ),
          paste0(
            "計算完成：",
            succeeded,
            " 個成功",
            if (failed > 0L) paste0("、", failed, " 個失敗") else "",
            "。"
          )
        ))
      }
      show_task_error <- identical(task_status, "error") &&
        !show_staging_status()
      if (show_task_error) {
        # ExtendedTask$result() 在 error 狀態會重新拋錯，以 tryCatch 取訊息。
        error_message <- tryCatch(
          {
            task$result()
            "計算失敗。"
          },
          error = compact_error
        )
        return(shiny::div(
          class = "status-banner status-danger",
          shiny::icon("circle-xmark"),
          error_message
        ))
      }

      # 沒有背景工作狀態時，顯示目前檔案檢查訊息。
      message <- stage_message()
      shiny::div(
        class = paste(
          "status-banner",
          paste0("status-", message$type)
        ),
        shiny::icon(
          switch(
            message$type,
            success = "circle-check",
            danger = "circle-xmark",
            "circle-info"
          )
        ),
        message$text
      )
    })

    # 對外只暴露成功結果；running、error 或尚未執行時都回傳 NULL。
    result <- shiny::reactive({
      if (!identical(task$status(), "success")) {
        return(NULL)
      }
      task$result()
    })

    list(
      result = result,
      task_status = shiny::reactive(task$status()),
      staged_jobs = staged_jobs,
      staged_preview = staged_preview
    )
  })
}
