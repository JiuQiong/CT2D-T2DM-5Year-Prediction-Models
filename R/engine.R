# 纯预测函数：无训练、无全局患者状态、无个人资料落盘。
load_prediction_bundles <- function(directory) {
  bundles <- setNames(lapply(c("CVD", "Death"), function(id) {
    b <- readRDS(file.path(directory, paste0(id, ".rds")))
    required <- c("schema_version", "endpoint", "features", "formula", "coefficients", "levels",
                  "contrasts", "reference", "ranges", "threshold", "metadata", "performance")
    if (!all(required %in% names(b)) || b$schema_version != 1L || b$endpoint != id ||
        !is.finite(b$threshold) || b$threshold <= 0 || b$threshold >= 1 ||
        any(!is.finite(b$coefficients))) stop("预测包无效：", id)
    if (!identical(b$metadata$horizon_years, 5L) ||
        !(b$metadata$release_status %in% c("research_ready", "followup_pending"))) {
      stop("当前平台只加载带有核验状态的五年模型：", id)
    }
    b
  }), c("CVD", "Death"))
  bundles
}

feature_matrix <- function(data, bundle) {
  if (!all(bundle$features %in% names(data))) stop("缺少模型所需变量")
  x <- data[bundle$features]
  for (name in bundle$features) {
    z <- suppressWarnings(as.numeric(as.character(x[[name]])))
    if (any(!is.finite(z))) stop("变量缺失或无效：", name)
    if (name %in% names(bundle$levels)) {
      if (any(!(as.character(z) %in% bundle$levels[[name]]))) stop("模型未观察到的分类编码：", name)
      x[[name]] <- factor(as.character(z), levels = bundle$levels[[name]])
    } else x[[name]] <- z
  }
  f <- stats::as.formula(bundle$formula, env = baseenv())
  tt <- stats::delete.response(stats::terms(f))
  mm <- stats::model.matrix(tt, data = x, contrasts.arg = bundle$contrasts, xlev = bundle$levels)
  if (!identical(colnames(mm), names(bundle$coefficients))) stop("预测设计矩阵与模型版本不一致")
  mm
}

predict_encoded <- function(data, bundle) {
  as.numeric(stats::plogis(feature_matrix(data, bundle) %*% bundle$coefficients))
}

# 仅供离线研究复算使用；在线评估只返回概率，不再生成风险等级。
grade_probability <- function(p, threshold) {
  ifelse(p >= threshold, "偏高", "未偏高")
}

explain_encoded <- function(data, bundle) {
  if (nrow(data) != 1L) stop("个人解释只接受一行资料")
  mm <- feature_matrix(data, bundle)
  ref <- as.data.frame(bundle$reference[bundle$features], check.names = FALSE)
  ref_mm <- feature_matrix(ref, bundle)
  values <- as.numeric((mm - ref_mm) * bundle$coefficients)
  assignment <- attr(mm, "assign")
  labels <- attr(stats::terms(stats::as.formula(bundle$formula)), "term.labels")
  contributions <- vapply(bundle$features, function(name) sum(values[assignment == match(name, labels)]), numeric(1))
  reference_logit <- as.numeric(ref_mm %*% bundle$coefficients)
  individual_logit <- as.numeric(mm %*% bundle$coefficients)
  if (abs(reference_logit + sum(contributions) - individual_logit) > 1e-10) stop("贡献分解校验失败")
  list(table = data.frame(variable = bundle$features, contribution = contributions,
                         value = vapply(bundle$features, function(n) as.numeric(as.character(data[[n]][1])), numeric(1)),
                         reference = unlist(bundle$reference[bundle$features]), row.names = NULL),
       reference_probability = stats::plogis(reference_logit), probability = stats::plogis(individual_logit))
}

scalar_number <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(as.character(x)))) return(NA_real_)
  suppressWarnings(as.numeric(as.character(x)))
}

required_inputs <- function(bundle) {
  unique(c("baseline_CVD", setdiff(bundle$features, "WWI"),
           if ("WWI" %in% bundle$features) c("WC", "weight")))
}

prepare_patient <- function(patient, bundle, dictionary, demo = FALSE) {
  fields <- required_inputs(bundle)
  numbers <- setNames(lapply(fields, function(n) scalar_number(patient[[n]])), fields)
  errors <- character()
  for (name in fields) {
    d <- dictionary[[name]]
    z <- numbers[[name]]
    if (!is.finite(z)) { errors <- c(errors, paste0("请补充有效的", d$label)); next }
    if (d$kind == "category") {
      allowed <- d$choices[nzchar(d$choices)]
      if (!(as.character(z) %in% unname(allowed))) errors <- c(errors, paste0(d$label, "编码无效"))
    } else if (z < 0 || (z == 0 && name != "DMTIME")) errors <- c(errors, paste0(d$label, "须为正数，请检查单位"))
  }
  if (length(errors)) return(list(ok = FALSE, errors = unique(errors)))
  if (all(c("SBP", "DBP") %in% fields) && numbers$SBP <= numbers$DBP) {
    errors <- c(errors, "收缩压应大于舒张压，请复核填写内容")
  }
  if ("WWI" %in% bundle$features) {
    for (name in c("WC", "weight")) {
      r <- bundle$input_ranges[[name]]
      if (numbers[[name]] < r[1] - 1e-10 || numbers[[name]] > r[2] + 1e-10) {
        errors <- c(errors, sprintf("%s超出训练集范围（%.2f–%.2f %s），请核对单位及适用范围", dictionary[[name]]$label, r[1], r[2], dictionary[[name]]$unit))
      }
    }
    numbers$WWI <- numbers$WC / sqrt(numbers$weight)
  }
  for (name in bundle$features) {
    z <- numbers[[name]]
    if (name %in% names(bundle$levels)) {
      if (!(as.character(z) %in% bundle$levels[[name]])) errors <- c(errors, paste0(dictionary[[name]]$label, "类别未出现在本模型训练集中"))
    } else {
      r <- bundle$ranges[[name]]
      if (!is.finite(z) || z < r[1] - 1e-10 || z > r[2] + 1e-10) {
        errors <- c(errors, sprintf("%s超出训练集范围（%.2f–%.2f %s），请核对单位及适用范围", dictionary[[name]]$label, r[1], r[2], dictionary[[name]]$unit))
      }
    }
  }
  if (length(errors)) return(list(ok = FALSE, errors = unique(errors)))
  list(ok = TRUE, data = as.data.frame(numbers[bundle$features], check.names = FALSE))
}

assess_patient <- function(patient, bundle, dictionary, demo = FALSE, t2dm_confirmed = FALSE) {
  result <- list(endpoint = bundle$endpoint, title = bundle$title, version = bundle$metadata$version,
                 status = "invalid", messages = character(), demo = isTRUE(demo),
                 horizon = bundle$metadata$horizon, release_status = bundle$metadata$release_status)
  fail <- function(status, messages) { result$status <- status; result$messages <- messages; result }
  if (!isTRUE(t2dm_confirmed)) return(fail("invalid", "请先由医护人员确认评估对象已确诊2型糖尿病"))
  cvd <- scalar_number(patient$baseline_CVD)
  if (!is.finite(cvd) || !(cvd %in% c(1, 2))) return(fail("invalid", "请明确既往是否已确诊冠心病或卒中"))
  if (bundle$endpoint == "CVD" && cvd == 1) {
    return(fail("not_applicable", "现有新发模型不适用：已确诊冠心病或卒中者应使用相应复发或二级预防评估工具。仍可进行全因死亡评估。"))
  }
  pending <- bundle$features[!vapply(dictionary[bundle$features], function(d) isTRUE(d$verified), logical(1))]
  if (length(pending) && !isTRUE(demo)) {
    return(fail("dictionary_pending", paste0("本结局暂未开放真实资料评估，需先核实以下分类编码：",
      paste(vapply(dictionary[pending], `[[`, character(1), "label"), collapse = "、"), "。可使用明确标记的演示资料体验。")))
  }
  prepared <- prepare_patient(patient, bundle, dictionary, demo)
  if (!prepared$ok) return(fail("invalid", prepared$errors))
  if (identical(bundle$metadata$release_status, "followup_pending") && !isTRUE(demo)) {
    return(fail("followup_pending", bundle$metadata$release_message))
  }
  explanation <- explain_encoded(prepared$data, bundle)
  result$status <- "ok"
  result$probability <- explanation$probability
  result$contributions <- explanation$table
  result$reference_probability <- explanation$reference_probability
  result$cautions <- character()
  if (identical(bundle$metadata$release_status, "followup_pending")) {
    result$cautions <- c(result$cautions, bundle$metadata$release_message)
  }
  for (name in names(bundle$category_counts)) {
    code <- as.character(prepared$data[[name]][1])
    n <- bundle$category_counts[[name]][code]
    if (length(n) == 1L && !is.na(n) && n == 1L) {
      result$cautions <- c(result$cautions, paste0(dictionary[[name]]$label, "中本次所选类别的训练样本仅1人，相关系数估计很不稳定。该类别的预测概率不宜用于个体判断，仅供研究探索。"))
    }
  }
  eligible <- vapply(dictionary[result$contributions$variable], function(d) isTRUE(d$patient_factor), logical(1))
  positive <- result$contributions[eligible & result$contributions$contribution > 1e-10, , drop = FALSE]
  positive <- positive[order(-positive$contribution, positive$variable), , drop = FALSE]
  result$top_factors <- head(positive$variable, 4L)
  result
}

assess_all <- function(patient, bundles, dictionary, demo = FALSE, t2dm_confirmed = FALSE) {
  lapply(bundles, function(b) assess_patient(patient, b, dictionary, demo, t2dm_confirmed))
}

format_patient_value <- function(name, value, dictionary) {
  d <- dictionary[[name]]
  z <- scalar_number(value)
  if (!is.finite(z)) return("未填写")
  if (d$kind == "category") {
    label <- names(d$choices)[match(as.character(z), d$choices)]
    if (is.na(label)) return("无效编码")
    return(label)
  }
  paste(format(round(z, 2), trim = TRUE, scientific = FALSE), d$unit)
}

demo_patient <- function(bundles, level = "higher") {
  p <- utils::modifyList(as.list(bundles$CVD$reference), as.list(bundles$Death$reference))
  p$baseline_CVD <- 2
  p$WC <- 90; p$weight <- 65
  p$antihypertensive <- 0; p$lipid_drug <- 0
  if (level == "higher") {
    p$age <- 75; p$SBP <- 165; p$DBP <- 85; p$TC <- 5.8
    p$TG <- 2.5; p$HBA1C <- 9; p$XLV <- 80; p$eGFR <- 60
    p$insulin <- 1; p$Dmjiazushi <- 1
  } else {
    p$age <- 45; p$SBP <- 110; p$DBP <- 70; p$TC <- 4
    p$TG <- 1; p$HBA1C <- 5.5; p$XLV <- 80; p$eGFR <- 110
    p$insulin <- 0; p$Dmjiazushi <- 2
  }
  p$WWI <- NULL
  p[unique(unlist(lapply(bundles, required_inputs)))]
}
