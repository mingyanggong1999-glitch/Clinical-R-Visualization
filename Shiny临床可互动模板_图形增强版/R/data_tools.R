# **---------------------------------------------------------------------------**
# ** 通用数据读写、变量映射、日期解释和界面工具
# **---------------------------------------------------------------------------**

read_table_file <- function(path, filename = basename(path), encoding = "UTF-8", sheet = "1") {
    extension <- tolower(tools::file_ext(filename))
    data <- switch(
        extension,
        csv = readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
            locale = readr::locale(encoding = encoding), show_col_types = FALSE),
        xpt = haven::read_xpt(path),
        sas7bdat = haven::read_sas(path),
        xlsx = readxl::read_excel(path, sheet = if (grepl("^[0-9]+$", sheet)) as.integer(sheet) else sheet),
        xls = readxl::read_excel(path, sheet = if (grepl("^[0-9]+$", sheet)) as.integer(sheet) else sheet),
        rds = readRDS(path),
        stop("支持 CSV、XLSX、XLS、SAS7BDAT、XPT 和数据框 RDS 文件。")
    )
    if (!is.data.frame(data) || !ncol(data)) stop("文件内容必须是一张至少含一个变量的数据框。")
    if (anyDuplicated(names(data)) || any(!nzchar(names(data)))) stop("变量名必须非空且不重复。")
    as.data.frame(data, stringsAsFactors = FALSE)
}

numeric_values <- function(x, label) {
    if (is.numeric(x)) result <- as.numeric(x) else {
        text <- as.character(x)
        text[text == ""] <- NA_character_
        result <- suppressWarnings(as.numeric(text))
        if (any(!is.na(text) & is.na(result))) stop(label, " 含有不能解释为数值的内容。")
    }
    if (any(!is.na(result) & !is.finite(result))) stop(label, " 包含 Inf 或 NaN。")
    result
}

date_values <- function(x, format = "%Y-%m-%d", origin = "1960-01-01", label = "日期") {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x, tz = "UTC"))
    if (is.numeric(x)) return(as.Date(x, origin = origin))
    text <- as.character(x)
    text[text == ""] <- NA_character_
    result <- as.Date(text, format = format)
    if (any(!is.na(text) & is.na(result))) stop(label, " 不能按所选格式解释，请调整日期格式或数值日期起点。")
    result
}

mapped_column <- function(data, name, optional = FALSE) {
    if (is.null(name) || !length(name) || !nzchar(name)) {
        if (optional) return(NULL)
        stop("请选择所需的变量名。")
    }
    if (!name %in% names(data)) stop("数据中没有变量：", name)
    data[[name]]
}

display_group <- function(x) {
    result <- as.character(x)
    result[is.na(result) | result == ""] <- "（缺失）"
    result
}

filter_values <- function(data, variable = "", values = character()) {
    if (is.null(variable) || !length(variable) || !nzchar(variable)) return(data)
    x <- display_group(mapped_column(data, variable))
    data[x %in% values, , drop = FALSE]
}

ui_result <- function(expr) {
    tryCatch(force(expr), error = function(e) shiny::validate(shiny::need(FALSE, conditionMessage(e))))
}

write_result_csv <- function(data, file) readr::write_excel_csv(as.data.frame(data), file, na = "")

panel_card <- function(...) {
    bslib::card(..., fill = FALSE, wrapper = function(...) bslib::card_body(..., fillable = FALSE, fill = FALSE))
}

standard_table <- function(data, selection = "single", page_length = 10L) {
    DT::datatable(data, rownames = FALSE, filter = "top", selection = selection,
        escape = TRUE, options = list(pageLength = page_length, scrollX = TRUE,
            language = list(search = "搜索：", lengthMenu = "每页 _MENU_ 行",
                info = "第 _START_ 至 _END_ 行，共 _TOTAL_ 行", zeroRecords = "无符合条件的记录")))
}

chart_layout <- function(p, x = "", y = "") {
    p <- plotly::layout(p, font = list(family = "Microsoft YaHei, sans-serif", size = 13),
        margin = list(l = 65, r = 25, t = 30, b = 65),
        xaxis = list(title = x), yaxis = list(title = y),
        paper_bgcolor = "white", plot_bgcolor = "white")
    plotly::config(p, displaylogo = FALSE, toImageButtonOptions = list(format = "png"))
}

column_input <- function(id, label, data, preferred = character(), optional = FALSE, numeric = FALSE) {
    choices <- names(data)
    selected <- intersect(preferred, choices)
    if (!length(selected) && numeric && !optional) selected <- choices[vapply(data, is.numeric, logical(1))]
    selected <- if (length(selected)) selected[1L] else if (optional) "" else choices[1L]
    if (optional) choices <- c("（不使用）" = "", stats::setNames(choices, choices))
    shiny::selectInput(id, label, choices = choices, selected = selected)
}

data_source_ui <- function(id, label) {
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::h5(label),
        shiny::radioButtons(ns("mode"), NULL, c("使用模拟数据" = "demo", "导入我的数据" = "file"), inline = TRUE),
        shiny::conditionalPanel(sprintf("input['%s'] == 'file'", ns("mode")),
            shiny::fileInput(ns("file"), "选择数据文件", accept = c(".csv", ".xlsx", ".xls", ".sas7bdat", ".xpt", ".rds")),
            shiny::selectInput(ns("encoding"), "CSV 编码", c("UTF-8", "GB18030")),
            shiny::textInput(ns("sheet"), "Excel 工作表名称或序号", "1")),
        shiny::textOutput(ns("info"))
    )
}

data_source_server <- function(id, demo) {
    shiny::moduleServer(id, function(input, output, session) {
        data <- shiny::reactive({
            if (is.null(input$mode) || identical(input$mode, "demo")) return(demo)
            shiny::req(input$file)
            ui_result(read_table_file(input$file$datapath, input$file$name,
                if (is.null(input$encoding)) "UTF-8" else input$encoding,
                if (is.null(input$sheet)) "1" else input$sheet))
        })
        output$info <- shiny::renderText(sprintf("%s 行，%s 个变量", nrow(data()), ncol(data())))
        data
    })
}

date_settings_ui <- function(ns) {
    shiny::tagList(
        shiny::textInput(ns("date_format"), "字符日期格式", "%Y-%m-%d"),
        shiny::selectInput(ns("date_origin"), "数值日期的起点（Date 列不受影响）",
            c("SAS：1960-01-01" = "1960-01-01", "R：1970-01-01" = "1970-01-01", "Excel：1899-12-30" = "1899-12-30"))
    )
}

config_ui <- function(ns) {
    shiny::tagList(shiny::downloadButton(ns("config_out"), "保存变量映射"),
        shiny::fileInput(ns("config_in"), "恢复已保存的映射（先导入数据）", accept = ".json"))
}

config_server <- function(input, output, session, fields, extra = function() NULL, restore_extra = function(x) NULL) {
    output$config_out <- shiny::downloadHandler(
        filename = function() paste0("mapping_", session$ns(""), ".json"),
        content = function(file) {
            settings <- lapply(names(fields), function(id) shiny::isolate(input[[id]]))
            names(settings) <- names(fields)
            jsonlite::write_json(list(version = 1L, fields = settings, extra = shiny::isolate(extra())),
                file, auto_unbox = TRUE, pretty = TRUE, na = "null", dataframe = "rows")
        }
    )
    shiny::observeEvent(input$config_in, {
        config <- tryCatch(jsonlite::read_json(input$config_in$datapath, simplifyVector = TRUE), error = identity)
        if (inherits(config, "error") || is.null(config$fields)) {
            shiny::showNotification("无法读取映射文件。", type = "error")
            return()
        }
        for (id in intersect(names(fields), names(config$fields))) {
            value <- config$fields[[id]]
            switch(fields[[id]],
                select = shiny::updateSelectInput(session, id, selected = value),
                multi = shiny::updateSelectizeInput(session, id, selected = value),
                number = shiny::updateNumericInput(session, id, value = value),
                text = shiny::updateTextInput(session, id, value = value),
                check = shiny::updateCheckboxInput(session, id, value = value),
                date = shiny::updateDateInput(session, id, value = value)
            )
        }
        restore_extra(config$extra)
        shiny::showNotification("映射已恢复，请确认与当前数据一致。", type = "message")
    }, ignoreInit = TRUE)
}
