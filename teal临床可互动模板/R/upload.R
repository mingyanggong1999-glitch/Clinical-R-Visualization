# **----------------------------------------------------------------------------**
# ** 网页数据导入：文件 → 变量映射 → 分析人群 → 会话内teal模块 ----
# **----------------------------------------------------------------------------**

UPLOAD_ROLES <- c(
    ADSL = "受试者级数据（必需）", ADAE = "不良事件",
    ADLB = "连续/访视数据（如ADLB、ADRE、ADVS）",
    ADTTE = "时间-事件数据", QUERY = "Query明细", KRI = "中心KRI快照"
)

UPLOAD_SCHEMA <- list(
    ADSL = c("STUDYID", "USUBJID", "SITEID", "TRT01P", "SAFFL", "RANDDT", "AGE", "SEX", "WEIGHT", "HEIGHT", "BMI"),
    ADAE = c("USUBJID", "AEBODSYS", "AEDECOD", "STUDYID", "AESEQ", "AESER", "AESTDT", "TRTEMFL"),
    ADLB = c("USUBJID", "PARAMCD", "AVISIT", "AVISITN", "AVAL", "AVALU", "STUDYID", "BASE", "CHG", "PCHG"),
    ADTTE = c("USUBJID", "PARAMCD", "AVAL", "AVALU", "CNSR", "STUDYID"),
    QUERY = c("USUBJID", "QUERYID", "OPEN_DATE", "CLOSE_DATE", "STUDYID", "FORM"),
    KRI = c("SITEID", "KRI", "VALUE", "DEN", "SNAPSHOT")
)

# 优先SDTM/ADaM原名，其次常见别名。映射只是建议，用户可按实际口径选择。
UPLOAD_ALIASES <- list(
    STUDYID = c("STUDY_ID", "STUDY"),
    USUBJID = c("SUBJID", "SUBJECT_ID", "SUBJECTID", "SUBJECT", "PATIENT_ID", "ID"),
    SITEID  = c("CENTER", "CENTERID", "SITE", "SITE_ID", "CENTRE"),
    TRT01P  = c("TRT01A", "TRTA", "ARM", "ACTARM", "TRT", "TREATMENT", "GROUP"),
    RANDDT  = c("RAND_DATE", "RANDOMIZATION_DATE"),
    AEBODSYS = c("AESOC", "SOC"),
    AEDECOD = c("PT", "PREFERRED_TERM"),
    PARAMCD = c("PARAM", "TESTCD"),
    AVISIT = c("VISIT"),
    AVISITN = c("VISITNUM", "VISITN"),
    AVAL = c("VALUE", "TIME"),
    AVALU = c("UNIT", "UNITS"),
    CNSR = c("CENSOR", "CENSORFL"),
    QUERYID = c("QUERY_ID"), OPEN_DATE = c("OPEN_DT"), CLOSE_DATE = c("CLOSE_DT"),
    KRI = c("METRIC", "METRICID"), VALUE = c("ACTUAL", "METRIC_VALUE"),
    DEN = c("DENOMINATOR"), SNAPSHOT = c("SNAPSHOT_DATE", "DATE")
)

suggest_mapping <- function(columns, target) {
    candidates <- c(target, UPLOAD_ALIASES[[target]])
    at <- match(toupper(candidates), toupper(columns), nomatch = 0L)
    if (any(at > 0L)) columns[at[at > 0L][1L]] else ""
}

upload_id <- function(prefix, role, variable = NULL) paste(c(prefix, role, variable), collapse = "_")

upload_role_ui <- function(role) {
    shiny::tags$details(
        open = if (role == "ADSL") "open" else NULL, class = "upload-domain",
        shiny::tags$summary(shiny::tags$b(role), " · ", unname(UPLOAD_ROLES[role])),
        shiny::fileInput(upload_id("upload", role), "选择本地文件", multiple = FALSE,
            accept = c(".sas7bdat", ".xpt", ".rds", ".csv", ".xlsx"),
            buttonLabel = "选择文件", placeholder = "尚未选择"),
        if (role != "ADSL") shiny::checkboxInput(upload_id("use", role), "本次纳入分析（已选择文件时生效）", TRUE),
        shiny::uiOutput(upload_id("config", role))
    )
}

upload_page <- function() {
    shiny::tags$div(class = "upload-shell",
        shiny::tags$div(class = "upload-header",
            shiny::h2("临床试验交互分析 · teal"),
            shiny::p("直接在网页中导入本地数据集，核对变量映射后启动分析，不需要修改 R 配置文件。"),
            shiny::actionButton("load_demo", "使用模拟数据", class = "btn btn-outline-primary")),
        shiny::tags$div(class = "upload-notice",
            "仅在本机当前会话中处理上传数据；请使用已获批准的临床数据。每次上传单个文件，支持 SAS、XPT、RDS、CSV、XLSX。"),
        shiny::fluidRow(
            shiny::column(8,
                shiny::h4("01 选择数据集并设置变量"),
                shiny::p("ADSL 必需。其他数据集按需要选择；缺少某个数据集时，对应分析页面不会显示。"),
                lapply(names(UPLOAD_ROLES), upload_role_ui)),
            shiny::column(4,
                shiny::tags$div(class = "upload-settings",
                    shiny::h4("02 分析设置"),
                    shiny::textInput("study_id", "研究编号（仅在ADSL没有映射STUDYID时填写）", ""),
                    shiny::dateInput("analysis_date", "分析截止日期（Query及入组）", value = Sys.Date()),
                    shiny::numericInput("query_days", "Query逾期天数", value = QUERY_DAYS, min = 0, step = 1),
                    shiny::selectInput("date_mode", "日期解析方式", choices = c(
                        "文本日期：YYYY-MM-DD / 数据中已有Date类型" = "text",
                        "SAS数值日期：1960-01-01" = "sas",
                        "Excel数值日期：1899-12-30" = "excel",
                        "Unix数值日期：1970-01-01" = "unix"), selected = "text"),
                    shiny::conditionalPanel("input.date_mode == 'text'",
                        shiny::textInput("date_format", "文本日期格式", value = DATE_FORMAT),
                        shiny::tags$small("SAS原生日期标签会自动识别；未标记的数值日期默认按SAS日期原点。")),
                    shiny::h4("03 启动分析"),
                    shiny::actionButton("start_analysis", "开始分析", class = "btn btn-primary", width = "100%"),
                    shiny::uiOutput("launch_message"),
                    shiny::hr(),
                    shiny::tags$small("上传、筛选或映射出现问题时，会保留本页以便继续修正。要切换项目，可在分析页面点击“更换数据”。")
                ))
        )
    )
}

filter_columns <- function(data) {
    names(data)[vapply(data, function(x) is.atomic(x) && !inherits(x, c("Date", "POSIXt")) &&
        length(unique(x[!is.na(x)])) <= 100L, logical(1))]
}

filter_values <- function(x) sort(unique(as.character(x[!is.na(x)])))

mapping_ui <- function(role, data, file) {
    vars <- names(data)
    required <- required_columns[[role]]
    fields <- UPLOAD_SCHEMA[[role]]
    widgets <- lapply(fields, function(field) {
        label <- paste0(field, if (field %in% required) " *" else if (field == "STUDYID") "（可关联/填写）" else "")
        shiny::column(4, shiny::selectInput(upload_id("map", role, field), label,
            choices = c("未映射" = "", stats::setNames(vars, vars)),
            selected = suggest_mapping(vars, field)))
    })
    shiny::tagList(
        shiny::tags$p(class = "upload-ok", paste0("已读取：", file, "  ·  ", nrow(data), " 行 / ", ncol(data), " 列")),
        shiny::tags$p(class = "upload-note", "带 * 的字段必须对应。SDTM/ADaM变量名已尽量自动匹配，请核对。"),
        do.call(shiny::fluidRow, widgets),
        if (role %in% c("ADSL", "ADAE", "ADLB", "ADTTE")) shiny::tagList(
            shiny::selectInput(upload_id("filter", role), if (role == "ADSL") "分析人群筛选变量（例如SAFFL）" else "分析记录筛选变量（例如ANL01FL）",
                choices = c("不筛选" = "", filter_columns(data)), selected = ""),
            shiny::uiOutput(upload_id("values_ui", role))),
        if (tolower(tools::file_ext(file)) == "csv") shiny::selectizeInput(upload_id("numeric", role),
            "额外数值变量（CSV默认按字符读入，映射的标准数值字段会自动转换）",
            choices = vars, selected = NULL, multiple = TRUE),
        if (role == "KRI") shiny::tagList(
            shiny::checkboxInput("enable_kri_rules", "设置本项目的KRI风险阈值（可选）", FALSE),
            shiny::uiOutput("kri_rules_ui"))
    )
}

mapping_from_input <- function(input, role, data) {
    fields <- UPLOAD_SCHEMA[[role]]
    mapping <- stats::setNames(vapply(fields, function(field) {
        val <- input[[upload_id("map", role, field)]]
        if (is.null(val)) "" else as.character(val)[1L]
    }, character(1)), fields)
    missing <- required_columns[[role]][!nzchar(mapping[required_columns[[role]]])]
    if (length(missing)) stop(role, " 尚未映射必需变量：", paste(missing, collapse = ", "), call. = FALSE)
    mapping[nzchar(mapping)]
}

make_value_filter <- function(data, source_column, values) {
    if (is.null(source_column) || !nzchar(source_column)) return(quote(TRUE))
    target <- unname(attr(data, "source_columns", exact = TRUE)[source_column])
    if (!length(target) || is.na(target) || !target %in% names(data)) stop("筛选变量不存在：", source_column)
    if (!length(values)) stop("请选择筛选变量 ", source_column, " 的保留值")
    # 仅以数据列名和字面值构造条件，不执行用户输入的R代码。
    rlang::expr(as.character(.data[[!!target]]) %in% !!as.character(values))
}

prepare_uploaded <- function(raw, mappings, study_id, filters = list(), numeric_columns = list(),
    date_origin = DATE_ORIGIN, date_format = DATE_FORMAT) {
    mapped <- lapply(names(raw), function(role) {
        data <- raw[[role]]
        extra <- intersect(numeric_columns[[role]], names(data))
        # 主键、分类、组别、日期映射不能被额外数值化，避免破坏前导零或日期原点。
        protected <- setdiff(names(mappings[[role]]), c("AGE", "WEIGHT", "HEIGHT", "BMI",
            "AVISITN", "AVAL", "BASE", "CHG", "PCHG", "CNSR", "VALUE", "DEN", "AESEQ"))
        if (any(extra %in% unname(mappings[[role]][protected])))
            stop(role, " 的额外数值变量不能包含已映射的编号、分类或日期字段")
        for (v in extra) data[[v]] <- to_numeric(data[[v]])
        map_variables(data, mappings[[role]], role, reserved = UPLOAD_SCHEMA[[role]])
    })
    names(mapped) <- names(raw)
    conditions <- lapply(names(mapped), function(role) {
        rule <- filters[[role]]
        if (is.null(rule)) return(quote(TRUE))
        make_value_filter(mapped[[role]], rule$column, rule$values)
    })
    names(conditions) <- names(mapped)
    prepare_datasets(mapped, study_id = study_id, pop_filter = conditions$ADSL,
        record_filter = conditions[setdiff(names(conditions), "ADSL")],
        date_origin = date_origin, date_format = date_format)
}

kri_rule_fields <- c(RED_LOW = "红色下限", AMBER_LOW = "黄色下限",
    AMBER_HIGH = "黄色上限", RED_HIGH = "红色上限", MIN_N = "最小分母")

kri_metrics <- function(raw, input) {
    var <- input$map_KRI_KRI
    if (is.null(raw) || is.null(var) || !nzchar(var) || !var %in% names(raw)) return(character())
    v <- as.character(raw[[var]])
    sort(unique(v[!is.na(v) & nzchar(v)]))
}

kri_settings_ui <- function(metrics) {
    if (!length(metrics)) return(shiny::p("请先映射KRI指标名称字段。"))
    shiny::tagList(shiny::p("红/黄界限均为空时，该指标显示未评估；真实数据不使用模拟阈值。"),
        lapply(seq_along(metrics), function(i) shiny::tags$div(class = "upload-kri-row",
            shiny::tags$b(metrics[i]),
            do.call(shiny::fluidRow, lapply(names(kri_rule_fields), function(field) shiny::column(2,
                shiny::textInput(upload_id("kri", i, field), unname(kri_rule_fields[field]), value = "")))))))
}

read_kri_rules <- function(metrics, input) {
    if (!isTRUE(input$enable_kri_rules) || !length(metrics)) return(KRI_RULES[0, , drop = FALSE])
    rules <- data.frame(METRIC = metrics, stringsAsFactors = FALSE)
    for (field in names(kri_rule_fields)) {
        rules[[field]] <- vapply(seq_along(metrics), function(i) {
            val <- input[[upload_id("kri", i, field)]]
            if (is.null(val) || !nzchar(trimws(val))) return(if (field == "MIN_N") 0 else NA_real_)
            num <- suppressWarnings(as.numeric(val))
            if (!is.finite(num)) stop("KRI ", metrics[i], " 的", kri_rule_fields[field], "须为有效数字")
            num
        }, numeric(1))
    }
    rules <- rules[rowSums(!is.na(rules[c("RED_LOW", "AMBER_LOW", "AMBER_HIGH", "RED_HIGH")])) > 0L, , drop = FALSE]
    rules
}

web_upload_app <- function() {
    ui <- bslib::page_fluid(
        title = APP_TITLE, theme = bslib::bs_theme(version = 5),
        shiny::tags$style(shiny::HTML("
        .upload-shell {max-width:1380px;margin:24px auto;color:#234258;}
        .upload-header {background:#edf4f7;padding:22px;border-radius:8px;margin-bottom:12px;}
        .upload-notice {padding:12px 18px;background:#f1f6f7;border-left:4px solid #28748c;margin-bottom:22px;}
        .upload-domain,.upload-settings {border:1px solid #d6e2e8;border-radius:8px;padding:15px;margin-bottom:12px;background:#fff;}
        .upload-domain summary {font-size:16px;cursor:pointer;padding:4px 0 10px;}
        .upload-domain .form-group {width:100%;}
        .upload-note {color:#5b6d78;font-size:13px;}
        .upload-ok {color:#187852;font-weight:600;}
        .upload-kri-row {border-top:1px solid #ddd;padding:8px 0;}
        .analysis-header {display:flex;justify-content:space-between;gap:12px;align-items:center;background:#edf4f7;padding:14px 24px;margin:8px 0;}
        .analysis-header h3 {margin:0 0 6px 0;color:#214f67;}
        ")),
        shiny::uiOutput("workspace")
    )
    server <- function(input, output, session) {
        # 所有上传文件、映射与分析对象均保存在本会话；不写入全局变量。
        raw <- shiny::reactiveValues()
        files <- shiny::reactiveValues()
        errors <- shiny::reactiveValues()
        active <- shiny::reactiveVal(NULL)
        message <- shiny::reactiveVal(NULL)

        for (role in names(UPLOAD_ROLES)) local({
            nm <- role
            shiny::observeEvent(input[[upload_id("upload", nm)]], {
                f <- input[[upload_id("upload", nm)]]
                files[[nm]] <- f$name[1L]
                raw[[nm]] <- NULL
                errors[[nm]] <- NULL
                message(NULL)
                tryCatch({
                    data <- read_dataset(f$datapath[1L], filename = f$name[1L])
                    if (!ncol(data) || !nrow(data)) stop("输入数据集没有可分析的行列")
                    raw[[nm]] <- data
                }, error = function(e) errors[[nm]] <- conditionMessage(e))
            }, ignoreNULL = TRUE)

            output[[upload_id("config", nm)]] <- shiny::renderUI({
                if (!is.null(errors[[nm]])) return(shiny::tags$div(class = "alert alert-danger", errors[[nm]]))
                if (is.null(raw[[nm]])) return(NULL)
                mapping_ui(nm, raw[[nm]], files[[nm]])
            })
            output[[upload_id("values_ui", nm)]] <- shiny::renderUI({
                var <- input[[upload_id("filter", nm)]]
                data <- raw[[nm]]
                if (is.null(data) || is.null(var) || !nzchar(var) || !var %in% names(data)) return(NULL)
                shiny::selectizeInput(upload_id("values", nm), "保留的变量值", choices = filter_values(data[[var]]),
                    selected = NULL, multiple = TRUE)
            })
        })
        output$kri_rules_ui <- shiny::renderUI({
            if (!isTRUE(input$enable_kri_rules)) return(NULL)
            kri_settings_ui(kri_metrics(raw$KRI, input))
        })
        output$launch_message <- shiny::renderUI({
            if (is.null(message())) return(NULL)
            shiny::tags$div(class = "alert alert-danger", style = "margin-top:12px;", message())
        })
        output$workspace <- shiny::renderUI({
            a <- active()
            if (is.null(a)) return(upload_page())
            shiny::tagList(
                shiny::tags$div(class = "analysis-header",
                    shiny::tags$div(shiny::h3(APP_TITLE),
                        shiny::tags$span(a$description, " · ", paste(a$counts, collapse = " / "))),
                    shiny::actionButton("change_data", "更换数据", class = "btn btn-outline-primary")),
                a$ui,
                shiny::tags$small("本机交互探索 · 统计口径及风险阈值须经项目确认。"))
        })

        launch <- function(data, description, as_of, query_days, rules) {
            if (!is.null(active())) return(invisible(NULL))
            ui_modules <- make_modules(data, as_of = as_of, query_days = query_days, kri_rules = rules)
            td <- make_teal_data(data)
            # 先组装完整界面；如果模块构造出错，保留上传页面供用户纠正。
            module_ui <- teal::ui_teal("analysis_teal", ui_modules)
            counts <- paste0(names(data), ": ", vapply(data, nrow, integer(1)))
            active(list(ui = module_ui, description = description, counts = counts))
            # 在UI插入后初始化teal原生server；每次网页会话只初始化一次。
            session$onFlushed(function() {
                tryCatch(shiny::withReactiveDomain(session, {
                    filters <- teal::teal_slices()
                    attr(filters, "app_id") <- paste0("teal_upload_", session$token)
                    teal::srv_teal("analysis_teal", td, ui_modules, filter = filters)
                }), error = function(e) shiny::showNotification(
                    paste("分析模块初始化未完成，请更换数据后重试：", conditionMessage(e)),
                    duration = NULL, type = "error", session = session))
            }, once = TRUE)
        }

        shiny::observeEvent(input$load_demo, {
            if (!is.null(active())) return()
            message(NULL)
            tryCatch(launch(prepare_datasets(demo_datasets(), study_id = "", pop_filter = quote(TRUE),
                record_filter = list()), "模拟数据", AS_OF, QUERY_DAYS, KRI_RULES),
                error = function(e) message(conditionMessage(e)))
        }, ignoreInit = TRUE)

        shiny::observeEvent(input$start_analysis, {
            if (!is.null(active())) return()
            message(NULL)
            tryCatch({
                if (is.null(raw$ADSL)) stop("请先上传有效的ADSL受试者级数据集。")
                included <- c("ADSL", names(UPLOAD_ROLES)[-1L][vapply(names(UPLOAD_ROLES)[-1L], function(nm)
                    isTRUE(input[[upload_id("use", nm)]]) && !is.null(files[[nm]]), logical(1))])
                for (nm in included)
                    if (is.null(raw[[nm]])) stop(nm, " 文件读取失败，请重新上传或取消纳入。")
                sources <- stats::setNames(lapply(included, function(nm) raw[[nm]]), included)
                maps <- stats::setNames(lapply(included, function(nm) mapping_from_input(input, nm, sources[[nm]])), included)
                filters <- stats::setNames(lapply(included, function(nm) list(
                    column = input[[upload_id("filter", nm)]], values = input[[upload_id("values", nm)]])), included)
                nums <- stats::setNames(lapply(included, function(nm) input[[upload_id("numeric", nm)]]), included)
                date_mode <- input$date_mode
                date_origin <- switch(date_mode, text = DATE_ORIGIN, sas = "1960-01-01",
                    excel = "1899-12-30", unix = "1970-01-01")
                date_format <- if (identical(date_mode, "text")) input$date_format else "numeric"
                if (is.null(date_format) || !nzchar(date_format)) stop("请填写日期格式")
                as_of <- as.Date(input$analysis_date)
                if (is.na(as_of)) stop("请输入有效的分析截止日期")
                days <- input$query_days
                if (is.null(days) || !is.finite(days) || days < 0 || days != floor(days)) stop("Query逾期天数须为非负整数")
                data <- prepare_uploaded(sources, maps, trimws(input$study_id), filters, nums, date_origin, date_format)
                rules <- if ("KRI" %in% names(data)) read_kri_rules(kri_metrics(raw$KRI, input), input) else KRI_RULES[0, , drop = FALSE]
                if ("KRI" %in% names(data)) invisible(apply_risk_rules(data$KRI, rules))
                launch(data, "网页上传数据", as_of, days, rules)
            }, error = function(e) message(conditionMessage(e)))
        }, ignoreInit = TRUE)

        shiny::observeEvent(input$change_data, session$reload(), ignoreInit = TRUE)
    }
    shiny::shinyApp(ui, server)
}
