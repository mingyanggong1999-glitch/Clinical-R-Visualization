# **----------------------------------------------------------------------------**
# ** 数据准备：原始数据 → 统一变量 → teal_data。 ----
# **----------------------------------------------------------------------------**

# 复用此前Shiny模板中的模拟数据，以方便并排观察。
demo_datasets <- function() {
    demo <- make_demo_data()
    ADSL <- demo$population
    ADSL$STUDYID <- "DEMO-01"
    ADSL$AGE <- 35L + (seq_len(nrow(ADSL)) * 7L) %% 40L
    ADSL$SEX <- rep(c("男", "女", "男"), length.out = nrow(ADSL))
    ADSL$WEIGHT <- round(48 + (seq_len(nrow(ADSL)) * 11L) %% 43L + ADSL$AGE / 30, 1)
    ADSL$HEIGHT <- round(145 + (seq_len(nrow(ADSL)) * 13L) %% 43L, 1)
    ADSL$BMI <- round(ADSL$WEIGHT / (ADSL$HEIGHT / 100)^2, 1)
    ADAE <- demo$events
    ADAE$AEBODSYS <- unname(c("头痛" = "神经系统疾病", "恶心" = "胃肠系统疾病",
        "咳嗽" = "呼吸系统疾病", "疲乏" = "全身性疾病", "皮疹" = "皮肤疾病")[ADAE$AEDECOD])
    ADLB <- demo$longitudinal
    ADLB$AVALU <- ifelse(ADLB$PARAMCD == "FVCEFF", "%", "U/L")
    baseline <- ADLB[ADLB$AVISITN == 0, c("USUBJID", "PARAMCD", "AVAL")]
    names(baseline)[3] <- "BASE"
    ADLB <- dplyr::left_join(ADLB, baseline, by = c("USUBJID", "PARAMCD"))
    ADLB$CHG <- ADLB$AVAL - ADLB$BASE
    ADTTE <- demo$survival
    ADTTE$PARAMCD <- "TTE"
    ADTTE$AVAL <- ADTTE$TIME
    ADTTE$AVALU <- "days"
    ADTTE$CNSR <- 1L - ADTTE$EVENT
    list(ADSL = ADSL, ADAE = ADAE, ADLB = ADLB, ADTTE = ADTTE,
        QUERY = demo$queries, KRI = demo$metrics)
}

read_dataset <- function(path, filename = basename(path)) {
    if (!file.exists(path)) stop("找不到输入文件：", path, call. = FALSE)
    # Shiny上传使用临时路径，扩展名以原文件名为准。
    ext <- tolower(tools::file_ext(filename))
    out <- switch(ext,
        sas7bdat = haven::read_sas(path),
        xpt      = haven::read_xpt(path),
        rds      = readRDS(path),
        csv      = readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE),
        xlsx     = readxl::read_excel(path),
        stop("不支持的数据格式：", ext, call. = FALSE))
    if (!is.data.frame(out)) stop("输入不是数据框：", path)
    as.data.frame(out)
}

map_variables <- function(data, mapping, dataset, reserved = character()) {
    absent <- setdiff(unname(mapping), names(data))
    if (length(absent)) stop(dataset, " 映射变量不存在：", paste(absent, collapse = ", "), call. = FALSE)
    if (anyDuplicated(unname(mapping)) || anyDuplicated(names(mapping))) stop(dataset, " 变量映射重复")
    # 保留原列；如选择TRT01A映射为TRT01P，原TRT01P保留为TRT01P_SOURCE。
    original <- names(data)
    if (anyDuplicated(original)) stop(dataset, " 原始变量名重复")
    result <- original
    mapped <- match(unname(mapping), original)
    result[mapped] <- names(mapping)
    for (i in setdiff(seq_along(original), mapped)) {
        if (result[i] %in% c(names(mapping), reserved)) {
            base <- paste0(original[i], "_SOURCE")
            candidate <- base
            j <- 1L
            while (candidate %in% c(result, reserved)) {
                candidate <- paste0(base, j)
                j <- j + 1L
            }
            result[i] <- candidate
        }
    }
    names(data) <- result
    data <- as.data.frame(data)
    attr(data, "source_columns") <- stats::setNames(result, original)
    data
}

to_date <- function(x, origin = DATE_ORIGIN, date_format = DATE_FORMAT) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    if (is.numeric(x)) return(as.Date(x, origin = origin))
    if (identical(date_format, "numeric")) return(as.Date(to_numeric(x), origin = origin))
    out <- as.Date(as.character(x), format = date_format)
    if (any(is.na(out) & !is.na(x) & nzchar(as.character(x)))) stop("日期无法转换，请检查日期格式/原点设置")
    out
}

to_numeric <- function(x) {
    v <- if (is.numeric(x)) as.numeric(x) else suppressWarnings(as.numeric(as.character(x)))
    if (any(is.na(v) & !is.na(x) & nzchar(as.character(x)))) stop("数值变量含非数值内容")
    v
}

keep_records <- function(data, condition) {
    dplyr::filter(data, !!condition)
}

required_columns <- list(
    ADSL = c("USUBJID", "SITEID", "TRT01P"),
    ADAE = c("USUBJID", "AEBODSYS", "AEDECOD"),
    ADLB = c("USUBJID", "PARAMCD", "AVISIT", "AVISITN", "AVAL", "AVALU"),
    ADTTE = c("USUBJID", "PARAMCD", "AVAL", "AVALU", "CNSR"),
    QUERY = c("USUBJID", "QUERYID", "OPEN_DATE", "CLOSE_DATE"),
    KRI = c("SITEID", "KRI", "VALUE", "DEN", "SNAPSHOT")
)

prepare_datasets <- function(datasets = NULL, study_id = STUDY_ID,
    pop_filter = POP_FILTER, record_filter = RECORD_FILTER,
    date_origin = DATE_ORIGIN, date_format = DATE_FORMAT) {
    # 无参数时保留原配置入口；网页入口传入当前会话的数据和规则。
    if (is.null(datasets)) {
        if (DATA_MODE == "demo") {
            datasets <- demo_datasets()
        } else if (DATA_MODE == "files") {
            files <- DATA_FILES[nzchar(DATA_FILES)]
            datasets <- lapply(names(files), function(nm) map_variables(read_dataset(files[[nm]]), VAR_MAP[[nm]], nm))
            names(datasets) <- names(files)
        } else stop("DATA_MODE 只允许 demo 或 files")
    }
    if (!"ADSL" %in% names(datasets)) stop("ADSL 为必需数据集")
    if (!"STUDYID" %in% names(datasets$ADSL)) {
        if (!nzchar(study_id)) stop("NEST需要研究编号；请选择STUDYID变量或填写研究编号")
        datasets$ADSL$STUDYID <- rep(study_id, nrow(datasets$ADSL))
    }

    for (nm in names(datasets)) {
        ds <- datasets[[nm]]
        absent <- setdiff(required_columns[[nm]], names(ds))
        if (length(absent)) stop(nm, " 缺少：", paste(absent, collapse = ", "))
        for (var in intersect(c("RANDDT", "AESTDT", "ADT", "OPEN_DATE", "CLOSE_DATE", "SNAPSHOT"), names(ds)))
            ds[[var]] <- to_date(ds[[var]], origin = date_origin, date_format = date_format)
        for (var in intersect(c("AGE", "WEIGHT", "HEIGHT", "BMI", "AVISITN", "AVAL", "BASE", "CHG", "PCHG", "CNSR", "VALUE", "DEN"), names(ds)))
            ds[[var]] <- to_numeric(ds[[var]])
        for (var in intersect(c("STUDYID", "USUBJID", "SITEID", "TRT01P", "QUERYID", "PARAMCD"), names(ds)))
            ds[[var]] <- as.character(ds[[var]])
        if ("USUBJID" %in% names(ds) && any(is.na(ds$USUBJID) | !nzchar(ds$USUBJID))) stop(nm, " 受试者编号为空")
        datasets[[nm]] <- ds
    }
    ADSL <- datasets$ADSL
    if (anyNA(ADSL$STUDYID) || any(ADSL$STUDYID == "") || length(unique(ADSL$STUDYID)) != 1L)
        stop("当前模板仅支持一个研究；STUDYID须非缺失且唯一")
    if (anyDuplicated(ADSL$USUBJID)) stop("ADSL必须每名受试者一行")
    all_ids <- ADSL$USUBJID
    ADSL <- keep_records(ADSL, pop_filter)
    if (!nrow(ADSL)) stop("选择的分析人群为空")
    if (anyNA(ADSL$TRT01P) || any(ADSL$TRT01P == "")) stop("当前分析人群治疗组缺失，请选择适当的分析集")
    if (anyNA(ADSL$SITEID) || any(ADSL$SITEID == "")) stop("当前分析人群中心编号缺失")
    ADSL$TRT01P <- factor(ADSL$TRT01P)
    datasets$ADSL <- ADSL

    for (nm in intersect(names(datasets), c("ADAE", "ADLB", "ADTTE", "QUERY"))) {
        ds <- datasets[[nm]]
        if (any(!ds$USUBJID %in% all_ids)) stop(nm, " 存在无法关联ADSL的受试者")
        ds <- ds[ds$USUBJID %in% ADSL$USUBJID, , drop = FALSE]
        if (!is.null(record_filter[[nm]])) ds <- keep_records(ds, record_filter[[nm]])
        # 治疗组、中心均以ADSL为准，避免子数据集与分母口径不一致。
        if ("STUDYID" %in% names(ds) && any(is.na(ds$STUDYID) | ds$STUDYID != ADSL$STUDYID[1L]))
            stop(nm, " STUDYID 与ADSL不一致")
        ds$STUDYID <- rep(ADSL$STUDYID[1L], nrow(ds))
        ds$TRT01P <- ADSL$TRT01P[match(ds$USUBJID, ADSL$USUBJID)]
        ds$SITEID <- ADSL$SITEID[match(ds$USUBJID, ADSL$USUBJID)]
        if (nm == "ADLB") {
            if (anyNA(ds$PARAMCD) || any(ds$PARAMCD == "") || anyNA(ds$AVISITN) || anyNA(ds$AVISIT) ||
                any(as.character(ds$AVISIT) == "")) stop("ADLB指标及访视信息不能缺失")
            if (anyDuplicated(ds[c("USUBJID", "PARAMCD", "AVISITN")])) stop("ADLB受试者×指标×访视重复，请配置分析记录筛选")
            visits <- unique(ds[c("AVISITN", "AVISIT")])
            if (anyDuplicated(visits$AVISITN) || anyDuplicated(visits$AVISIT)) stop("访视编码与名称须一一对应")
            ds$AVISIT <- factor(ds$AVISIT, levels = visits$AVISIT[order(visits$AVISITN)])
        }
        if (nm == "ADTTE" && (anyNA(ds$PARAMCD) || any(ds$PARAMCD == "") || anyDuplicated(ds[c("USUBJID", "PARAMCD")]) ||
            any(is.na(ds$CNSR) | !ds$CNSR %in% c(0, 1)) || any(!is.finite(ds$AVAL) | ds$AVAL < 0)))
            stop("ADTTE须每名受试者每指标一行，AVAL>=0，且CNSR为0事件/1删失")
        if (nm == "QUERY" && (anyDuplicated(ds$QUERYID) || anyNA(ds$QUERYID) || any(ds$QUERYID == "") ||
            anyNA(ds$OPEN_DATE) || any(ds$CLOSE_DATE < ds$OPEN_DATE, na.rm = TRUE)))
            stop("Query编号须唯一且日期须有效")
        ds$TEAL_ROWID <- seq_len(nrow(ds))
        datasets[[nm]] <- ds
    }
    if ("KRI" %in% names(datasets)) {
        m <- datasets$KRI
        if (anyNA(m$SITEID) || any(m$SITEID == "") || anyNA(m$KRI) || any(as.character(m$KRI) == "") ||
            anyNA(m$SNAPSHOT) || anyDuplicated(m[c("SITEID", "KRI", "SNAPSHOT")]) || any(m$DEN < 0, na.rm = TRUE))
            stop("KRI中心、指标和快照须非缺失且组合唯一，分母非负")
        m$TEAL_ROWID <- seq_len(nrow(m))
        datasets$KRI <- m
    }
    # 原生NEST表使用变量label；原有SAS标签保留，缺失标签用友好名称补足。
    labels <- c(STUDYID = "研究编号", USUBJID = "受试者编号", SITEID = "中心编号",
        TRT01P = "治疗组", AGE = "年龄（岁）", SEX = "性别", WEIGHT = "体重（kg）",
        HEIGHT = "身高（cm）", BMI = "BMI（kg/m²）", SAFFL = "安全性分析集",
        AEBODSYS = "系统器官分类 (SOC)", AEDECOD = "首选术语 (PT)", AESER = "是否严重AE",
        PARAMCD = "指标代码", AVISIT = "访视", AVAL = "分析值", BASE = "基线值", CHG = "较基线变化")
    datasets <- lapply(datasets, function(ds) {
        for (v in names(ds)) {
            label <- attr(ds[[v]], "label", exact = TRUE)
            if (is.null(label) || length(label) != 1L || is.na(label) || !nzchar(label))
                attr(ds[[v]], "label") <- if (v %in% names(labels)) unname(labels[v]) else v
        }
        ds
    })
    # 空的可选数据集不初始化分析模块。
    datasets[vapply(datasets, nrow, integer(1)) > 0L]
}

make_teal_data <- function(datasets) {
    keys <- teal.data::join_keys(teal.data::join_key("ADSL", "ADSL", c("STUDYID", "USUBJID")))
    for (nm in setdiff(names(datasets), "ADSL")) {
        keys[nm, nm] <- if (nm == "KRI") "TEAL_ROWID" else c("STUDYID", "USUBJID", "TEAL_ROWID")
        if (nm != "KRI") keys["ADSL", nm] <- c("STUDYID", "USUBJID")
    }
    # KRI为已聚合的快照数据，故意不与受试者层数据进行连接。
    td <- do.call(teal.data::teal_data, c(datasets, list(join_keys = keys)))
    # teal原生模块生成的可回放代码使用管道符及ggplot2；显式登记依赖。
    within(td, {
        library(dplyr)
        library(ggplot2)
    })
}

