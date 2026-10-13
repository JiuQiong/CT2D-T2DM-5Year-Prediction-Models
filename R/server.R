# 每个会话单独建立状态；患者资料只存在于该会话内存中。
make_server <- function(bundles, dictionary, css) {
  force(bundles); force(dictionary); force(css)
  function(input, output, session) {
    h <- htmltools::tags
    fields <- unique(unlist(lapply(bundles, required_inputs)))
    state <- shiny::reactiveValues(snapshot = NULL, stale = FALSE, demo = FALSE)
    session$userData$assessment_state <- state
    empty <- function(title, text) h$div(class = "empty-state", h$h3(title), h$p(text))
    patient <- shiny::reactive(setNames(lapply(fields, function(n) input[[n]]), fields))
    shiny::observeEvent(list(patient(), input$confirmed_t2dm), {
      if (!is.null(state$snapshot)) state$stale <- TRUE
    }, ignoreInit = TRUE, priority = 100)
    valid_snapshot <- shiny::reactive({
      s <- state$snapshot
      !is.null(s) && !isTRUE(state$stale) && any(vapply(s$results, function(r) r$status == "ok", logical(1)))
    })
    shiny::observe({
      session$sendCustomMessage("assessment-actions", list(enabled = valid_snapshot()))
    })
    load_demo <- function(level) {
      state$snapshot <- NULL; state$stale <- FALSE; state$demo <- TRUE
      p <- demo_patient(bundles, level)
      for (name in fields) {
        if (dictionary[[name]]$kind == "category") shiny::updateSelectInput(session, name, selected = as.character(p[[name]])) else
          shiny::updateNumericInput(session, name, value = p[[name]])
      }
      shiny::updateCheckboxInput(session, "confirmed_t2dm", value = TRUE)
      bslib::nav_select("main_nav", "input", session = session)
    }
    shiny::observeEvent(input$demo_higher, load_demo("higher"))
    shiny::observeEvent(input$demo_lower, load_demo("lower"))
    shiny::observeEvent(input$reset, {
      state$snapshot <- NULL; state$stale <- FALSE; state$demo <- FALSE
      for (name in fields) {
        if (dictionary[[name]]$kind == "category") shiny::updateSelectInput(session, name, selected = "") else
          shiny::updateNumericInput(session, name, value = NA_real_)
      }
      shiny::updateCheckboxInput(session, "confirmed_t2dm", value = FALSE)
    })
    shiny::observeEvent(input$calculate, {
      p <- patient()
      results <- assess_all(p, bundles, dictionary, demo = state$demo, t2dm_confirmed = isTRUE(input$confirmed_t2dm))
      state$snapshot <- list(patient = p, results = results, demo = state$demo,
        assessed_at = format(Sys.time(), "%Y-%m-%d %H:%M", tz = "Asia/Shanghai"))
      state$stale <- FALSE
      bslib::nav_select("main_nav", "results", session = session)
    }, priority = 0)
    shiny::observeEvent(input$go_input, bslib::nav_select("main_nav", "input", session = session))
    shiny::observeEvent(input$go_report, bslib::nav_select("main_nav", "report", session = session))
    output$mode_banner <- shiny::renderUI({
      if (state$demo) h$div(class = "demo-banner", h$strong("正在使用演示资料"),
        " · 修改示例后仍标为演示。如需评估真实资料，请先点击“清空”按钮。")
    })
    output$wwi_preview <- shiny::renderUI({
      w <- scalar_number(input$WC); kg <- scalar_number(input$weight)
      h$div(class = "derived-preview", h$span("自动计算 WWI"), h$strong(if (is.finite(w) && is.finite(kg) && w > 0 && kg > 0)
        sprintf("%.3f cm/√kg", w / sqrt(kg)) else "等待腰围与体重"),
        h$small("腰围（cm）÷ √体重（kg）"))
    })
    output$completion <- shiny::renderUI({
      p <- patient()
      h$div(class = "completion-list", lapply(bundles, function(b) {
        needed <- required_inputs(b)
        completed <- sum(vapply(needed, function(n) is.finite(scalar_number(p[[n]])), logical(1)))
        existing <- b$endpoint == "CVD" && isTRUE(scalar_number(p$baseline_CVD) == 1)
        h$div(class = "completion-item", h$div(h$span(b$title), h$strong(if (existing) "不适用" else paste0(completed, " / ", length(needed)))),
          h$div(class = "completion-track", h$span(style = sprintf("width:%.1f%%", 100 * completed / length(needed)))))
      }))
    })
    output$stale_notice <- shiny::renderUI({
      if (state$stale) h$div(class = "notice-banner stale-banner", h$strong("资料已修改，原结果已失效。"),
        h$span("请返回填写资料并点击“开始评估”，重新计算后再查看或导出报告。"))
    })
    output$results_content <- shiny::renderUI({
      if (is.null(state$snapshot)) return(empty("评估结果将在这里呈现", "请先填写健康资料，或加载明确标记的演示资料。"))
      if (state$stale) return(empty("请重新评估", "为保证报告与当前资料一致，旧结果已隐藏。"))
      s <- state$snapshot
      htmltools::tagList(if (s$demo) h$div(class = "demo-banner", "演示资料 · 以下结果用于功能展示"),
        h$div(class = "risk-grid", lapply(s$results, result_card, dictionary = dictionary)))
    })
    output$report_content <- shiny::renderUI({
      if (!valid_snapshot()) return(empty("尚无可导出的患者报告", "完成有效评估后可下载HTML，或通过浏览器打印保存为PDF。修改资料后需重新评估。"))
      report_content(state$snapshot, dictionary, bundles)
    })
    output$download_report <- shiny::downloadHandler(
      filename = function() paste0("T2DM_", if (state$demo) "演示_" else "", "预后评估_", format(Sys.time(), "%Y%m%d", tz = "Asia/Shanghai"), ".html"),
      contentType = "text/html; charset=utf-8",
      content = function(file) {
        shiny::req(valid_snapshot())
        writeLines(enc2utf8(report_document(state$snapshot, dictionary, bundles, css)), file, useBytes = TRUE)
      })
  }
}
