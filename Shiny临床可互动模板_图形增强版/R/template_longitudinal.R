# **---------------------------------------------------------------------------**
# ** 模板 03：带访视 / 日期的连续指标；均值及 t 区间、中位数及四分位数、个体轨迹。
# ** 本工具描述观察数据，不拟合 MMRM，不插补缺失。分位数明确使用 type=2。
# **---------------------------------------------------------------------------**

longitudinal_ui <- function(id) {
    ns <- shiny::NS(id)
    bslib::layout_sidebar(fillable = FALSE,
        sidebar = bslib::sidebar(width = 350,
            data_source_ui(ns("source"), "连续指标长表"), shiny::uiOutput(ns("mapping")),
            shiny::selectInput(ns("x_type"), "横轴类型", c("访视 / 分类时间点" = "visit", "日历日期" = "date")),
            shiny::selectInput(ns("duplicates"), "同人同组同时间多条记录时", c("提示我先筛选分析记录" = "error", "明确先取均值" = "mean")),
            date_settings_ui(ns), config_ui(ns)
        ),
        panel_card(bslib::card_header("指标与显示方式"),
            bslib::layout_columns(shiny::uiOutput(ns("parameter_ui")), shiny::uiOutput(ns("groups_ui"))),
            bslib::layout_columns(
                shiny::selectInput(ns("view"), "图形", c("均值与 95% t 置信区间" = "mean", "中位数与 Q1、Q3" = "median", "受试者个体轨迹" = "individual")),
                shiny::sliderInput(ns("max_subjects"), "个体图最多展示多少名受试者", min = 10, max = 200, value = 60, step = 10)),
            shiny::textOutput(ns("status"))),
        panel_card(bslib::card_header("交互趋势：悬停查看，点击图例切换组别"), plotly::plotlyOutput(ns("plot"), height = "460px")),
        panel_card(bslib::card_header("按组别与时间点汇总"),
            shiny::p("N 为非缺失观测数；MISSING 为该时间点已有记录中的缺失数，不代表所有计划访视的缺失人数。"),
            DT::DTOutput(ns("summary")), shiny::downloadButton(ns("download"), "下载未舍入统计结果")),
        panel_card(bslib::card_header("当前分析记录"), DT::DTOutput(ns("records")))
    )
}

longitudinal_server <- function(id, demo) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        raw <- data_source_server("source", demo$longitudinal)
        output$mapping <- shiny::renderUI({
            data <- raw()
            shiny::tagList(column_input(ns("id"), "受试者编号（可不选）", data, "USUBJID", optional = TRUE),
                column_input(ns("group"), "分组变量", data, c("TRT01P", "SITEID", "GROUP")),
                column_input(ns("x"), "访视或日期变量", data, c("AVISIT", "ADT", "VISIT")),
                column_input(ns("x_order"), "访视排序号变量（可不选）", data, c("AVISITN", "VISITNUM"), optional = TRUE, numeric = TRUE),
                column_input(ns("value"), "连续分析值", data, c("AVAL", "VALUE", "CHG"), numeric = TRUE),
                column_input(ns("parameter"), "指标变量（可不选）", data, c("PARAMCD", "PARAM"), optional = TRUE))
        })
        output$parameter_ui <- shiny::renderUI({
            shiny::req(input$value)
            values <- if (is.null(input$parameter) || !nzchar(input$parameter)) "当前指标" else unique(display_group(raw()[[input$parameter]]))
            shiny::selectInput(ns("focus_parameter"), "分析指标", values)
        })
        output$groups_ui <- shiny::renderUI({
            shiny::req(input$group)
            groups <- unique(display_group(raw()[[input$group]]))
            shiny::selectizeInput(ns("groups"), "展示组别", groups, selected = groups, multiple = TRUE)
        })
        normalized <- shiny::reactive({
            shiny::req(input$group, input$x, input$value, input$focus_parameter, input$x_type)
            ui_result({
                data <- raw()
                if (nzchar(input$parameter)) data <- data[display_group(data[[input$parameter]]) == input$focus_parameter, , drop = FALSE]
                result <- data.frame(ID = if (nzchar(input$id)) as.character(data[[input$id]]) else as.character(seq_len(nrow(data))),
                    GROUP = display_group(data[[input$group]]), VALUE = numeric_values(data[[input$value]], input$value))
                if (anyNA(result$ID) || any(result$ID == "")) stop("映射了受试者编号时，编号不能缺失。")
                if (input$x_type == "date") {
                    result$X <- date_values(data[[input$x]], input$date_format, input$date_origin)
                    if (anyNA(result$X)) stop("横轴日期不能缺失。")
                } else {
                    labels <- display_group(data[[input$x]])
                    if (nzchar(input$x_order)) {
                        order_map <- unique(data.frame(LABEL = labels, ORD = numeric_values(data[[input$x_order]], input$x_order)))
                        if (anyDuplicated(order_map$LABEL) || anyNA(order_map$ORD)) stop("访视标签与排序号应一一对应，且排序号不能缺失。")
                        levels <- order_map$LABEL[order(order_map$ORD, order_map$LABEL)]
                    } else levels <- unique(labels)
                    result$X <- factor(labels, levels = levels)
                }
                result[result$GROUP %in% input$groups, , drop = FALSE]
            })
        })
        result <- shiny::reactive({
            shiny::validate(shiny::need(nrow(normalized()), "请至少选择一个有数据的组别。"))
            ui_result(continuous_summary(normalized(), nzchar(input$id), input$duplicates))
        })
        output$summary <- DT::renderDT({
            standard_table(result()$summary) |> DT::formatRound(c("MEAN", "SD", "MEDIAN", "Q1", "Q3", "LOW", "HIGH"), 3)
        })
        output$records <- DT::renderDT(standard_table(result()$records, selection = "none"))
        output$plot <- plotly::renderPlotly({
            shiny::req(input$view)
            if (identical(input$view, "individual")) {
                shiny::validate(shiny::need(nzchar(input$id), "个体轨迹需要映射受试者编号。"))
                data <- result()$records
                ids <- sort(unique(data$ID))
                data <- data[data$ID %in% head(ids, input$max_subjects), , drop = FALSE]
                data <- data[order(data$ID, data$X), , drop = FALSE]
                p <- plotly::plot_ly(data, x = ~X, y = ~VALUE, color = ~GROUP, split = ~ID, type = "scatter", mode = "lines+markers",
                    text = ~paste0(htmltools::htmlEscape(ID), "<br>", htmltools::htmlEscape(GROUP), "<br>值：", VALUE),
                    hoverinfo = "text", opacity = 0.55, showlegend = FALSE)
            } else {
                data <- result()$summary
                mean_view <- identical(input$view, "mean")
                data$Y <- if (mean_view) data$MEAN else data$MEDIAN
                data$YLOW <- if (mean_view) data$LOW else data$Q1
                data$YHIGH <- if (mean_view) data$HIGH else data$Q3
                data <- data[order(data$GROUP, data$X), , drop = FALSE]
                p <- plotly::plot_ly(data, x = ~X, y = ~Y, color = ~GROUP, type = "scatter", mode = "lines+markers",
                    error_y = list(type = "data", symmetric = FALSE, array = ~YHIGH - Y, arrayminus = ~Y - YLOW),
                    text = ~paste0(htmltools::htmlEscape(GROUP), "<br>N=", N, "；缺失=", MISSING, "<br>统计值=", round(Y, 3)), hoverinfo = "text")
            }
            p <- chart_layout(p, input$x, paste0(input$focus_parameter, "：", input$value))
            if (input$x_type == "visit") p <- plotly::layout(p, xaxis = list(type = "category", categoryorder = "array", categoryarray = levels(normalized()$X)))
            p
        })
        output$status <- shiny::renderText({
            data <- result()$records
            sprintf("当前 %s 条分析记录；%s 名编号；%s 个组别。个体图按编号顺序截取前 %s 名。",
                nrow(data), length(unique(data$ID)), length(unique(data$GROUP)), input$max_subjects)
        })
        output$download <- shiny::downloadHandler(filename = function() "longitudinal_summary.csv", content = function(file) write_result_csv(result()$summary, file))
        config_server(input, output, session, c(id = "select", group = "select", x = "select", x_order = "select", value = "select", parameter = "select",
            x_type = "select", duplicates = "select", date_format = "text", date_origin = "select"))
        list(normalized = normalized, result = result)
    })
}
