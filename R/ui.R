build_ui <- function(bundles, dictionary) {
  h <- htmltools::tags
  fields <- unique(unlist(lapply(bundles, required_inputs)))
  input_widget <- function(name) {
    d <- dictionary[[name]]
    label <- h$span(d$label, if (nzchar(d$unit)) h$span(class = "unit-label", d$unit))
    widget <- if (d$kind == "category") shiny::selectInput(name, label, choices = d$choices, selected = "", width = "100%") else
      shiny::numericInput(name, label, value = NA_real_, min = 0, step = if (name == "age") 1 else 0.1, width = "100%")
    h$div(class = paste("field", if (!d$verified) "pending-field"), widget,
      if (!d$verified) h$div(class = "field-note", "原始编码含义待核对，仅用于演示"))
  }
  groups <- c("基本资料", "疾病与家族史", "体格测量", "血压与心率", "化验指标", "用药情况")
  group_cards <- lapply(seq_along(groups), function(i) {
    ns <- names(dictionary)[vapply(dictionary, function(d) d$group == groups[i], logical(1)) & names(dictionary) %in% fields]
    if (!length(ns)) return(NULL)
    bslib::card(class = "input-card", bslib::card_header(h$span(groups[i])),
      h$div(class = "field-grid", lapply(ns, input_widget)),
      if (groups[i] == "体格测量") shiny::uiOutput("wwi_preview"),
      if (groups[i] == "疾病与家族史") h$p(class = "field-note", "既往已有冠心病或卒中者，仅进行全因死亡评估。"))
  })
  hero <- h$section(class = "app-intro", h$div(
    h$h1("2型糖尿病预后风险评估"), h$p("5年内新发心血管并发症和死亡风险")))
  bslib::page_navbar(
    title = h$div(class = "brand", h$span(class = "brand-mark", htmltools::HTML('<svg width="25" height="25" viewBox="0 0 24 24" aria-hidden="true"><path d="M3 12h4l2-5 4 10 2-5h6" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/></svg>')),
      h$span("糖尿病预后评估")), id = "main_nav", selected = "input",
    window_title = "2型糖尿病 · 预后风险评估", lang = "zh-CN", fillable = FALSE,
    theme = bslib::bs_theme(version = 5, bg = "#F1F5FA", fg = "#19364F", primary = "#1D61A3", base_font = "Microsoft YaHei"),
    navbar_options = bslib::navbar_options(collapsible = TRUE, underline = FALSE),
    header = h$head(shiny::includeCSS("www/styles.css"), h$script(src = "app.js")),
    bslib::nav_panel("填写资料", value = "input", h$main(class = "app-shell", hero,
      h$div(class = "workflow-strip", h$span(class = "active", "01 填写健康资料"), h$span("02 查看两项结果"), h$span("03 获取患者报告")),
      h$div(class = "entry-layout", h$div(class = "entry-main", h$div(class = "form-toolbar", h$div(h$h2("填写健康资料"),
        h$p("请以就诊或体检记录为准，注意各项单位。")), h$div(class = "demo-actions",
          shiny::actionButton("demo_higher", "示例资料 1", class = "btn-outline-secondary btn-sm"),
          shiny::actionButton("demo_lower", "示例资料 2", class = "btn-outline-secondary btn-sm"))),
        shiny::uiOutput("mode_banner"), h$div(class = "form-grid", group_cards)),
        h$aside(class = "entry-sidebar", bslib::card(class = "assessment-control",
          h$h3("评估操作"), shiny::uiOutput("completion"),
          shiny::checkboxInput("confirmed_t2dm", "我确认评估对象已确诊2型糖尿病", FALSE),
          shiny::actionButton("calculate", "开始评估 →", class = "btn-primary calculate-button"),
          shiny::actionButton("reset", "清空", class = "btn-outline-primary reset-button")),
          bslib::card(class = "scope-card", h$h4("评估项目"),
            h$div(class = "scope-item", h$span(class = "scope-dot cvd"), "5年内心血管疾病发生风险"),
            h$div(class = "scope-item", h$span(class = "scope-dot death"), "5年内全因死亡风险")))))),
    bslib::nav_panel("评估结果", value = "results", h$main(class = "app-shell results-shell",
      h$div(class = "page-heading", h$div(h$h1("评估结果"),
        h$p("个人5年风险与主要危险因素。")),
        shiny::actionButton("go_input", "返回修改资料", class = "btn-outline-primary")),
      shiny::uiOutput("stale_notice"), shiny::uiOutput("results_content"),
      h$div(class = "results-actions", shiny::actionButton("go_report", "查看患者报告 →", class = "btn-primary")))),
    bslib::nav_panel("患者报告", value = "report", h$main(class = "app-shell report-shell",
      h$div(class = "page-heading no-print", h$div(h$h1("患者报告")),
        h$div(class = "report-actions", shiny::downloadButton("download_report", "下载 HTML", class = "btn-outline-primary"),
          h$button(id = "print_report", type = "button", class = "btn btn-primary", onclick = "window.print()", "打印 / 保存 PDF"))),
      shiny::uiOutput("report_content")))
  )
}
