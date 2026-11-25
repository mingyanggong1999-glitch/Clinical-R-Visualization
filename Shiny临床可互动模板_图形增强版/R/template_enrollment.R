# **---------------------------------------------------------------------------**
# ** 模板 07：入组进度。输入每受试者一行（受试者编号 + 入组日期 + 可选分组）。
# ** 计算累计入组曲线，可叠加一条线性计划线。
# **---------------------------------------------------------------------------**

normalize_enrollment <- function(data, id, date, group = "", date_format = "%Y-%m-%d", date_origin = "1960-01-01") {
    result <- data.frame(
        ID = as.character(mapped_column(data, id)),
        GROUP = if (nzchar(group)) display_group(mapped_column(data, group)) else rep("全部", nrow(data)),
        DATE = date_values(mapped_column(data, date), date_format, date_origin, label = "入组日期"))
    if (anyNA(result$DATE)) stop("入组日期不能缺失。")
    if (anyDuplicated(result$ID)) stop("每名受试者应只有一条入组记录。")
    result
}

enrollment_curve <- function(data) {
    days <- sort(unique(data$DATE))
    out <- do.call(rbind, lapply(split(data, data$GROUP), function(g) {
        daily <- table(factor(as.character(g$DATE), levels = as.character(days)))
        data.frame(GROUP = unique(g$GROUP), DATE = days, CUM = cumsum(as.numeric(daily)))
    }))
    as.data.frame(out)
}

enrollment_summary <- function(data) {
    data |> dplyr::group_by(GROUP) |>
        dplyr::summarise(N = dplyr::n(), FIRST = min(DATE), LAST = max(DATE), .groups = "drop") |>
        as.data.frame()
}

enrollment_plan <- function(data, target_n, target_date, date_format, date_origin) {
    if (is.null(target_n) || is.na(target_n) || target_n <= 0) return(NULL)
    if (is.null(target_date) || !nzchar(target_date)) return(NULL)
    end <- date_values(target_date, date_format, date_origin, label = "计划完成日期")
    start <- min(data$DATE)
    if (end <= start) return(NULL)
    total_days <- as.numeric(end - start)
    dates <- seq(start, end, by = "day")
    data.frame(DATE = dates, PLAN = pmin(target_n, target_n * as.numeric(dates - start) / total_days))
}

enrollment_ui <- function(id) {
    ns <- shiny::NS(id)
    bslib::layout_sidebar(fillable = FALSE,
        sidebar = bslib::sidebar(width = 340,
            data_source_ui(ns("source"), "入组数据（每受试者一行）"),
            shiny::uiOutput(ns("mapping")),
            date_settings_ui(ns),
            shiny::numericInput(ns("plan_n"), "计划总入组数（0=不显示计划线）", 0, min = 0),
            shiny::textInput(ns("plan_date"), "计划完成日期（格式同上，留空=不显示）", ""),
            config_ui(ns)),
        panel_card(bslib::card_header("累积入组曲线"),
            shiny::p("实线为实际累计入组；虚线为按计划总人数与完成日期线性推算的计划线。"),
            plotly::plotlyOutput(ns("plot"), height = "440px")),
        panel_card(bslib::card_header("入组汇总"), DT::DTOutput(ns("summary"))),
        panel_card(bslib::card_header("输入记录"), DT::DTOutput(ns("records")))
    )
}

enrollment_server <- function(id, demo) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        raw <- data_source_server("source", demo$population)
        output$mapping <- shiny::renderUI({
            data <- raw()
            shiny::tagList(
                column_input(ns("id"), "受试者编号", data, "USUBJID"),
                column_input(ns("date"), "入组日期变量", data, c("RANDDT", "RANDDT"), optional = FALSE),
                column_input(ns("group"), "分组变量（可不选）", data, c("TRT01P", "SITEID"), optional = TRUE)
            )
        })
        normalized <- shiny::reactive({
            shiny::req(input$id, input$date)
            ui_result(normalize_enrollment(raw(), input$id, input$date, input$group,
                input$date_format, input$date_origin))
        })
        curve <- shiny::reactive({
            data <- normalized()
            shiny::validate(shiny::need(nrow(data) > 0L, "没有可用于绘图的记录。"))
            ui_result(enrollment_curve(data))
        })
        output$plot <- plotly::renderPlotly({
            data <- curve()
            groups <- unique(data$GROUP)
            palette <- graph_palette(length(groups))
            p <- plotly::plot_ly()
            for (i in seq_along(groups)) {
                d <- data[data$GROUP == groups[i], , drop = FALSE]
                p <- plotly::add_trace(p, data = d, x = ~DATE, y = ~CUM, name = groups[i],
                    type = "scatter", mode = "lines", line = list(color = palette[i], width = 2),
                    hoverinfo = "x+y+name")
            }
            plan <- ui_result(enrollment_plan(normalized(), input$plan_n, input$plan_date,
                input$date_format, input$date_origin))
            if (!is.null(plan)) {
                p <- plotly::add_trace(p, data = plan, x = ~DATE, y = ~PLAN, name = "计划",
                    type = "scatter", mode = "lines", line = list(color = "#34495E", dash = "dash", width = 2),
                    hoverinfo = "x+y+name")
            }
            chart_layout(p, "日期", "累计入组人数")
        })
        output$summary <- DT::renderDT(standard_table(enrollment_summary(normalized()), selection = "none"))
        output$records <- DT::renderDT(standard_table(normalized(), selection = "none"))
        config_server(input, output, session,
            c(id = "select", date = "select", group = "select", date_format = "text", date_origin = "select",
                plan_n = "number", plan_date = "text"))
        list(normalized = normalized, curve = curve)
    })
}
