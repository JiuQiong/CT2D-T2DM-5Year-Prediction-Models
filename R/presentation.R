# 展示函数使用固定模板，动态内容由 htmltools 转义。
status_label <- function(status) switch(status, ok = "已完成", not_applicable = "不适用",
  dictionary_pending = "编码待核对", followup_pending = "五年模型待核验", invalid = "资料待补充", "待评估")

format_risk_percent <- function(p) {
  if (p < 0.001) return("<0.1")
  if (p > 0.999) return(">99.9")
  sprintf("%.1f", p * 100)
}

result_card <- function(result, dictionary) {
  h <- htmltools::tags
  color <- if (result$endpoint == "CVD") "cvd" else "death"
  if (result$status != "ok") {
    return(h$section(class = paste("risk-card", color, "unavailable"),
      h$h3(result$title), h$div(class = "status-pill neutral", status_label(result$status)),
      h$ul(class = "message-list", lapply(result$messages, h$li))))
  }
  h$section(class = paste("risk-card", color),
    h$div(class = "risk-card-heading", h$h3(result$title)),
    h$p(class = "risk-label", if (result$endpoint == "CVD") "5年内发生概率" else "5年内死亡概率"),
    h$div(class = "risk-number", format_risk_percent(result$probability), h$span("%")),
    if (length(result$cautions)) h$div(class = "demo-banner", lapply(result$cautions, h$p)),
    if (length(result$top_factors)) h$section(class = "factor-section",
      h$h4(class = "factor-heading", "主要危险因素"),
      h$p(class = "factor-context", if (result$endpoint == "CVD")
        "本次评估中，使您心血管疾病发生风险增加的主要因素包括：" else
        "本次评估中，使您全因死亡风险增加的主要因素包括："),
      h$ul(class = "factor-list", lapply(result$top_factors, function(n) {
        d <- result$contributions[result$contributions$variable == n, , drop = FALSE]
        h$li(h$strong(dictionary[[n]]$factor_label),
          h$span(class = "factor-value", format_patient_value(n, d$value, dictionary)))
      }))) else h$p(class = "factor-context", "本次未识别出适合单独列出的危险因素；预测概率由模型中的全部指标共同计算。"))
}

assessment_summary <- function(results, dictionary) {
  sentences <- vapply(seq_along(results), function(i) {
    result <- results[[i]]
    prefix <- if (i == 1L) "①" else "②"
    if (result$status != "ok") {
      return(paste0(prefix, if (result$endpoint == "CVD") "心血管并发症" else "全因死亡",
        "风险暂无法评估：", paste(result$messages, collapse = "；")))
    }
    outcome <- if (result$endpoint == "CVD") "发生心血管并发症" else "发生全因死亡"
    factors <- vapply(dictionary[result$top_factors], `[[`, character(1), "factor_label")
    factor_text <- if (length(factors)) paste0("，主要危险因素包括", paste(factors, collapse = "、")) else
      "；本次未识别出可单独列出的主要危险因素"
    paste0(prefix, "您5年内", outcome, "的概率为", format_risk_percent(result$probability), "%", factor_text)
  }, character(1))
  paste0(paste(sentences, collapse = "；"), "。")
}

report_content <- function(snapshot, dictionary, bundles) {
  h <- htmltools::tags
  results <- snapshot$results
  provided <- names(dictionary)[names(dictionary) %in% names(snapshot$patient)]
  provided <- provided[vapply(provided, function(n) is.finite(scalar_number(snapshot$patient[[n]])), logical(1))]
  h$article(class = "patient-report", `data-model-cvd` = bundles$CVD$metadata$version,
    `data-model-death` = bundles$Death$metadata$version,
    if (snapshot$demo) h$div(class = "demo-watermark", "演示资料 · 非真实患者评估"),
    h$div(class = "report-heading", h$h2("2型糖尿病预后风险评估报告"),
      h$div(class = "report-date", "评估日期", h$br(), snapshot$assessed_at)),
    h$section(class = "report-summary", h$h3("综合评估结果"),
      h$p(assessment_summary(results, dictionary))),
    h$section(class = "report-section", h$h3("本次填写的健康资料"),
      h$table(class = "report-table", h$thead(h$tr(h$th("指标"), h$th("填写内容"))),
        h$tbody(lapply(provided, function(n) h$tr(h$td(dictionary[[n]]$label),
          h$td(format_patient_value(n, snapshot$patient[[n]], dictionary))))))),
    h$p(class = "report-disclaimer", "以上结果仅供参考，具体请结合临床专业检查进行判断。"))
}

report_document <- function(snapshot, dictionary, bundles, css) {
  h <- htmltools::tags
  # renderTags/as.character会将<head>抽离为Shiny依赖；独立文件必须显式保留。
  paste0('<!DOCTYPE html>\n<html lang="zh-CN"><head><meta charset="UTF-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<title>2型糖尿病预后评估报告</title><style>', css,
    'body{background:#fff;padding:20px}.patient-report{max-width:1000px;margin:auto}.print-button{margin:0 auto 20px;display:block}@media print{.print-button{display:none}}',
    '</style></head>', as.character(h$body(h$button(class = "print-button primary-button", onclick = "window.print()", "打印 / 保存为 PDF"),
      report_content(snapshot, dictionary, bundles))), '</html>')
}

