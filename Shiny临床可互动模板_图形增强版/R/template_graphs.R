# **---------------------------------------------------------------------------**
# ** 模板 05：通用交互图形
# ** 变量映射：数值 X / Y、分类、分组、筛选及受试者编号。
# ** 数值图按观察记录绘制；分类频数按记录或去重受试者计算。
# **---------------------------------------------------------------------------**

make_graph_demo <- function(demo) {
    baseline <- demo$longitudinal |>
        dplyr::filter(AVISITN == 0) |>
        dplyr::select(USUBJID, PARAMCD, AVAL) |>
        tidyr::pivot_wider(names_from = PARAMCD, values_from = AVAL)
    demo$population |>
        dplyr::left_join(baseline, by = "USUBJID") |>
        dplyr::mutate(AGE = 35 + (seq_len(dplyr::n()) * 17) %% 46) |>
        as.data.frame()
}

graph_palette <- function(n) grDevices::hcl.colors(max(3L, n), "Dark 3")

graph_counts <- function(data, category, group = "", id = "", distinct = FALSE) {
    frame <- data.frame(CATEGORY = display_group(mapped_column(data, category)),
        GROUP = if (!is.null(group) && nzchar(group)) display_group(mapped_column(data, group)) else rep("全部", nrow(data)))
    if (distinct) {
        frame$ID <- as.character(mapped_column(data, id))
        frame <- frame[!is.na(frame$ID) & frame$ID != "", , drop = FALSE]
        frame <- dplyr::distinct(frame, CATEGORY, GROUP, ID)
    }
    dplyr::count(frame, CATEGORY, GROUP, name = "N")
}

graph_missing <- function(data, variables, blank_missing = TRUE) {
    dplyr::bind_rows(lapply(variables, function(variable) {
        value <- mapped_column(data, variable)
        missing <- is.na(value)
        if (blank_missing && (is.character(value) || is.factor(value))) missing <- missing | (!is.na(value) & value == "")
        data.frame(VARIABLE = variable, N = nrow(data), MISSING = sum(missing),
            PCT = if (nrow(data)) 100 * sum(missing) / nrow(data) else NA_real_)
    })) |> dplyr::arrange(dplyr::desc(PCT), VARIABLE)
}

graph_missing_heatmap <- function(data, row_var, col_var, value_var) {
    row <- display_group(mapped_column(data, row_var))
    col <- display_group(mapped_column(data, col_var))
    value <- mapped_column(data, value_var)
    missing <- is.na(value) | (is.character(value) & !is.na(value) & value == "")
    frame <- data.frame(ROW = row, COL = col, MISSING = missing)
    frame |> dplyr::group_by(ROW, COL) |>
        dplyr::summarise(N = dplyr::n(), MISS = sum(MISSING), PCT_MISS = 100 * sum(MISSING) / dplyr::n(), .groups = "drop") |>
        as.data.frame()
}

graphs_ui <- function(id) {
    ns <- shiny::NS(id)
    bslib::layout_sidebar(fillable = FALSE,
        sidebar = bslib::sidebar(width = 350,
            data_source_ui(ns("source"), "输入数据集"),
            shiny::selectInput(ns("kind"), "图形类型", c("箱线图：按组比较" = "box", "小提琴图：观察分布" = "violin",
                "直方图：观察频数" = "hist", "散点图：两个连续变量" = "scatter",
                "分类柱状图：记录或人数" = "bar", "缺失率图：按变量汇总" = "missing",
                "缺失热力图：行×列模式" = "heatmap")),
            shiny::uiOutput(ns("mapping")), shiny::uiOutput(ns("filter_values_ui")),
            shiny::conditionalPanel(sprintf("input['%s'] == 'hist'", ns("kind")),
                shiny::sliderInput(ns("bins"), "直方图目标分箱数", min = 5, max = 60, value = 20)),
            shiny::conditionalPanel(sprintf("input['%s'] == 'bar'", ns("kind")),
                shiny::checkboxInput(ns("distinct"), "按受试者去重（每组每类别）", FALSE)),
            shiny::conditionalPanel(sprintf("input['%s'] == 'missing'", ns("kind")),
                shiny::checkboxInput(ns("blank_missing"), "空字符串也视为缺失", TRUE)),
            config_ui(ns)),
        panel_card(bslib::card_header("交互图形"),
            shiny::textOutput(ns("status")),
            shiny::p("悬停查看数值，点击图例筛选系列，可缩放、平移及使用右上角相机按钮下载 PNG。"),
            plotly::plotlyOutput(ns("plot"), height = "540px")),
        panel_card(bslib::card_header("图形统计数据"),
            shiny::p("连续图按记录描述；汇总表的 Q1/Q3 使用 R type=2 算法，图形箱体采用 Plotly 的绘图算法。"),
            DT::DTOutput(ns("summary")),
            shiny::downloadButton(ns("download"), "下载图形统计数据")),
        panel_card(bslib::card_header("当前筛选后的输入记录"), DT::DTOutput(ns("records")))
    )
}

graphs_server <- function(id, demo) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        raw <- data_source_server("source", make_graph_demo(demo))
        output$mapping <- shiny::renderUI({
            data <- raw()
            cond <- function(expr, ...) shiny::conditionalPanel(expr, ...)
            kind <- sprintf("input['%s']", ns("kind"))
            shiny::tagList(
                cond(paste0(kind, " != 'missing' && ", kind, " != 'heatmap'"),
                    column_input(ns("group"), "颜色 / 分组变量（可不选）", data, c("TRT01P", "GROUP"), optional = TRUE)),
                cond(paste0(kind, " == 'box' || ", kind, " == 'violin' || ", kind, " == 'hist' || ", kind, " == 'scatter'"),
                    column_input(ns("y"), "分析值（数值变量）", data, c("FVCEFF", "AVAL", "VALUE"), numeric = TRUE)),
                cond(paste0(kind, " == 'scatter'"),
                    column_input(ns("x"), "横轴数值变量", data, c("AGE", "BASE", "ALT"), numeric = TRUE)),
                cond(paste0(kind, " == 'bar'"),
                    column_input(ns("category"), "分类变量", data, c("SITEID", "SEX", "CATEGORY"))),
                cond(paste0(kind, " != 'missing' && ", kind, " != 'heatmap'"),
                    column_input(ns("id"), "受试者编号（可不选）", data, "USUBJID", optional = TRUE)),
                cond(paste0(kind, " == 'missing'"),
                    shiny::selectizeInput(ns("variables"), "检查缺失的变量", choices = names(data),
                        selected = names(data), multiple = TRUE)),
                cond(paste0(kind, " == 'heatmap'"),
                    column_input(ns("heat_row"), "行变量（如受试者/站点）", data, c("USUBJID", "SITEID")),
                    column_input(ns("heat_col"), "列变量（如访视/时间）", data, c("AVISIT", "AVISITN")),
                    column_input(ns("heat_value"), "检查缺失的数值变量", data, c("AVAL", "VALUE"), numeric = TRUE)),
                column_input(ns("filter"), "额外筛选变量（可不选）", data, character(), optional = TRUE)
            )
        })
        output$filter_values_ui <- shiny::renderUI({
            shiny::req(input$filter)
            values <- sort(unique(display_group(mapped_column(raw(), input$filter))))
            shiny::selectizeInput(ns("filter_values"), "保留的取值", choices = values, selected = values, multiple = TRUE)
        })
        selected <- shiny::reactive({
            data <- raw()
            if (!is.null(input$filter) && nzchar(input$filter)) {
                shiny::req(input$filter_values)
                data <- filter_values(data, input$filter, input$filter_values)
            }
            shiny::validate(shiny::need(nrow(data), "筛选后没有记录。"))
            data
        })
        numerical <- shiny::reactive({
            shiny::req(input$kind, input$y)
            data <- selected()
            ui_result({
                group <- if (is.null(input$group) || !nzchar(input$group)) rep("全部", nrow(data)) else display_group(mapped_column(data, input$group))
                frame <- data.frame(GROUP = group, Y = numeric_values(mapped_column(data, input$y), input$y),
                    ROW = seq_len(nrow(data)), ID = if (is.null(input$id) || !nzchar(input$id)) seq_len(nrow(data)) else as.character(mapped_column(data, input$id)))
                if (input$kind == "scatter") {
                    shiny::req(input$x)
                    frame$X <- numeric_values(mapped_column(data, input$x), input$x)
                }
                frame <- frame[!is.na(frame$Y) & (if ("X" %in% names(frame)) !is.na(frame$X) else TRUE), , drop = FALSE]
                shiny::validate(shiny::need(nrow(frame), "所选变量没有可用于绘图的非缺失数值。"))
                frame
            })
        })
        summary <- shiny::reactive({
            shiny::req(input$kind)
            if (input$kind == "missing") {
                shiny::req(input$variables)
                return(ui_result(graph_missing(selected(), input$variables, isTRUE(input$blank_missing))))
            }
            if (input$kind == "heatmap") {
                shiny::req(input$heat_row, input$heat_col, input$heat_value)
                return(ui_result(graph_missing_heatmap(selected(), input$heat_row, input$heat_col, input$heat_value)))
            }
            if (input$kind == "bar") {
                shiny::req(input$category)
                if (isTRUE(input$distinct)) shiny::validate(shiny::need(!is.null(input$id) && nzchar(input$id), "去重人数统计需要选择受试者编号。"))
                return(ui_result(graph_counts(selected(), input$category, input$group, input$id, isTRUE(input$distinct))))
            }
            if (input$kind == "scatter") return(numerical())
            numerical() |> dplyr::group_by(GROUP) |>
                dplyr::summarise(N = dplyr::n(), MEAN = mean(Y), SD = stats::sd(Y), MEDIAN = stats::median(Y),
                    Q1 = unname(stats::quantile(Y, 0.25, type = 2)), Q3 = unname(stats::quantile(Y, 0.75, type = 2)),
                    MIN = min(Y), MAX = max(Y), .groups = "drop")
        })
        output$plot <- plotly::renderPlotly({
            kind <- shiny::req(input$kind)
            if (kind == "missing") {
                data <- summary()
                p <- plotly::plot_ly(data, x = ~PCT, y = ~reorder(VARIABLE, PCT), type = "bar", orientation = "h",
                    marker = list(color = "#417E9B"), text = ~paste0(MISSING, "/", N, "<br>", round(PCT, 2), "%"),
                    textposition = "none", hoverinfo = "y+text")
                return(plotly::layout(chart_layout(p, "缺失率 (%)", "变量"), xaxis = list(range = c(0, 100)),
                    yaxis = list(automargin = TRUE)))
            }
            if (kind == "heatmap") {
                data <- summary()
                shiny::validate(shiny::need(nrow(data), "没有可用于绘图的行×列记录。"))
                p <- plotly::plot_ly(data, x = ~COL, y = ~ROW, z = ~PCT_MISS, type = "heatmap",
                    colorscale = list(c(0, "#2C7FB8"), c(1, "#C0392B")),
                    colorbar = list(title = "缺失 %", ticksuffix = "%"),
                    hovertemplate = "行: %{y}<br>列: %{x}<br>缺失: %{z:.1f}%<extra></extra>")
                return(chart_layout(p, input$heat_col, input$heat_row))
            }
            if (kind == "bar") {
                data <- summary()
                shiny::validate(shiny::need(nrow(data), "没有可以计数的记录或受试者。"))
                p <- plotly::plot_ly(data, x = ~CATEGORY, y = ~N, color = ~GROUP, colors = graph_palette(length(unique(data$GROUP))),
                    type = "bar", hoverinfo = "x+y+name")
                return(plotly::layout(chart_layout(p, input$category, if (isTRUE(input$distinct)) "受试者人数 n" else "记录数"),
                    barmode = "group", xaxis = list(automargin = TRUE)))
            }
            data <- numerical()
            palette <- graph_palette(length(unique(data$GROUP)))
            if (kind == "scatter") {
                p <- plotly::plot_ly(data, x = ~X, y = ~Y, color = ~GROUP, colors = palette, type = "scatter", mode = "markers",
                    marker = list(size = 8, opacity = 0.7), text = ~htmltools::htmlEscape(ID), hoverinfo = "x+y+text+name")
                return(chart_layout(p, input$x, input$y))
            }
            if (kind == "hist") {
                p <- plotly::plot_ly(data, x = ~Y, color = ~GROUP, colors = palette, type = "histogram", nbinsx = input$bins,
                    opacity = 0.62, hoverinfo = "x+y+name")
                return(plotly::layout(chart_layout(p, input$y, "记录频数"), barmode = "overlay"))
            }
            if (kind == "violin") {
                p <- plotly::plot_ly(data, x = ~GROUP, y = ~Y, color = ~GROUP, colors = palette, type = "violin",
                    box = list(visible = TRUE), meanline = list(visible = TRUE), points = "outliers")
            } else {
                p <- plotly::plot_ly(data, x = ~GROUP, y = ~Y, color = ~GROUP, colors = palette,
                    type = "box", boxpoints = "outliers", boxmean = TRUE)
            }
            chart_layout(p, if (!is.null(input$group) && nzchar(input$group)) input$group else "全部", input$y)
        })
        output$status <- shiny::renderText({
            kind <- shiny::req(input$kind)
            if (kind %in% c("bar", "missing", "heatmap")) return(sprintf("当前筛选后 %s 条记录。", nrow(selected())))
            sprintf("当前筛选后 %s 条记录；绘图使用 %s 条非缺失记录。分组按所选列，未自动进行受试者去重。",
                nrow(selected()), nrow(numerical()))
        })
        output$summary <- DT::renderDT({
            data <- summary()
            table <- standard_table(data, selection = "none")
            decimals <- intersect(c("MEAN", "SD", "MEDIAN", "Q1", "Q3", "MIN", "MAX", "PCT", "PCT_MISS", "X", "Y"), names(data))
            if (length(decimals)) table <- DT::formatRound(table, decimals, 3)
            table
        })
        output$records <- DT::renderDT(standard_table(selected(), selection = "none"))
        output$download <- shiny::downloadHandler(filename = function() paste0("graph_", input$kind, "_data.csv"),
            content = function(file) write_result_csv(summary(), file))
        config_server(input, output, session, c(kind = "select", group = "select", x = "select", y = "select", category = "select",
            id = "select", variables = "multi", heat_row = "select", heat_col = "select", heat_value = "select",
            filter = "select", filter_values = "multi", distinct = "check", blank_missing = "check", bins = "number"))
        list(selected = selected, summary = summary, numerical = numerical)
    })
}
