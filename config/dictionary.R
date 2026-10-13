# 学历合并、家族史与既往CVD按最终五年程序；饮酒中文含义由研究者于2026-10-12确认。
variable_dictionary <- function() {
  entry <- function(label, unit = "", group, kind = "numeric", choices = NULL,
                    verified = TRUE, patient_factor = TRUE, advice = "", factor_label = label) {
    list(label = label, unit = unit, group = group, kind = kind,
         choices = choices, verified = verified, patient_factor = patient_factor, advice = advice, factor_label = factor_label)
  }
  yes_no <- c("请选择" = "", "是" = "1", "否" = "0")
  yes_no_12 <- c("请选择" = "", "是" = "1", "否" = "2")
  list(
    age = entry("年龄", "岁", "基本资料", patient_factor = FALSE),
    sex = entry("性别", group = "基本资料", kind = "category",
      choices = c("请选择" = "", "男" = "1", "女" = "2"), patient_factor = FALSE),
    education = entry("教育程度", group = "基本资料", kind = "category",
      choices = c("请选择" = "", "未接受正规教育" = "1", "小学未毕业" = "2", "小学毕业" = "3", "初中毕业" = "4",
                  "高中/中专/技校及以上" = "5"),
      patient_factor = FALSE),
    activity = entry("体力活动", group = "基本资料", kind = "category",
      choices = c("请选择" = "", "从不" = "1", "偶尔" = "2", "经常" = "3"),
      advice = "与医护人员讨论适合自身情况的体力活动安排。"),
    social = entry("社会状况（social）", group = "基本资料", kind = "category",
      choices = c("请选择" = "", "原始编码1（待核对）" = "1", "原始编码2（待核对）" = "2",
        "原始编码3（待核对）" = "3", "原始编码4（待核对）" = "4", "原始编码5（待核对）" = "5", "原始编码6（待核对）" = "6"),
      verified = FALSE, patient_factor = FALSE),
    smoking = entry("吸烟情况", group = "基本资料", kind = "category",
      choices = c("请选择" = "", "衍生编码0（待核对）" = "0", "衍生编码1（待核对）" = "1", "衍生编码2（待核对）" = "2"),
      verified = FALSE, patient_factor = FALSE),
    # 原drink=1现在饮酒、2曾经饮酒、3不饮酒；模型编码=3-原编码。
    drink = entry("饮酒情况", group = "基本资料", kind = "category",
      choices = c("请选择" = "", "不饮酒" = "0", "曾经饮酒" = "1", "现在饮酒" = "2")),
    baseline_CVD = entry("既往是否已确诊冠心病或卒中", group = "疾病与家族史", kind = "category",
      choices = yes_no_12, advice = "结合既往心血管病史安排复诊与长期管理。", factor_label = "既往冠心病或卒中病史"),
    Dmjiazushi = entry("糖尿病家族史", group = "疾病与家族史", kind = "category", choices = yes_no_12),
    CVD_family = entry("冠心病或卒中家族史", group = "疾病与家族史", kind = "category", choices = yes_no_12),
    DMTIME = entry("糖尿病病程", "年", "疾病与家族史",
      advice = "结合糖尿病病程，与医护人员讨论并发症筛查和随访安排。"),
    WC = entry("腰围", "cm", "体格测量"),
    weight = entry("体重", "kg", "体格测量"),
    WWI = entry("腰围与体重的综合指标", "cm/√kg", "体格测量", kind = "derived",
      advice = "与医护人员讨论腰围、体重及营养管理。"),
    SBP = entry("收缩压", "mmHg", "血压与心率", advice = "结合日常监测，与医护人员讨论血压管理。"),
    DBP = entry("舒张压", "mmHg", "血压与心率", advice = "结合日常监测，与医护人员讨论血压管理。"),
    XLV = entry("心率", "次/分", "血压与心率", advice = "向医护人员提供心率测量情况，讨论是否需要进一步复核。"),
    TC = entry("总胆固醇", "mmol/L", "化验指标", advice = "结合完整血脂结果，与医护人员讨论血脂管理。"),
    HDL = entry("高密度脂蛋白胆固醇", "mmol/L", "化验指标", advice = "结合完整血脂结果，与医护人员讨论血脂管理。"),
    TG = entry("甘油三酯", "mmol/L", "化验指标", advice = "结合完整血脂结果，与医护人员讨论血脂管理。"),
    HBA1C = entry("糖化血红蛋白", "%", "化验指标", advice = "结合血糖监测，与医护人员讨论个体化血糖管理。"),
    eGFR = entry("估算肾小球滤过率（eGFR）", "mL/min/1.73m²", "化验指标",
      advice = "结合肾功能检查，与医护人员讨论复查及长期管理。"),
    insulin = entry("是否使用胰岛素", group = "用药情况", kind = "category", choices = yes_no, patient_factor = FALSE),
    oral_drug = entry("是否使用口服降糖药", group = "用药情况", kind = "category", choices = yes_no, patient_factor = FALSE),
    antihypertensive = entry("是否使用降压药", group = "用药情况", kind = "category", choices = yes_no, patient_factor = FALSE),
    lipid_drug = entry("是否使用调脂药", group = "用药情况", kind = "category", choices = yes_no, patient_factor = FALSE)
  )
}

validate_dictionary <- function(dictionary, features) {
  if (length(setdiff(features, names(dictionary)))) stop("变量字典缺少模型字段")
  for (name in intersect(features, names(dictionary))) {
    d <- dictionary[[name]]
    if (isTRUE(d$verified) && d$kind == "category" &&
        any(grepl("待核对|原始编码", names(d$choices)[nzchar(d$choices)]))) {
      stop("已标记核实的字段仍使用占位标签：", name)
    }
  }
  invisible(TRUE)
}
