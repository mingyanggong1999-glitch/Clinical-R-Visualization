# **---------------------------------------------------------------------------**
# ** 模板 04：按指定快照重建 Query 开关状态，计算年龄、逾期和历史积压。
# ** 输入一行 = 一条 Query。关闭日期晚于快照日时，该快照仍视为未关闭。
# **---------------------------------------------------------------------------**

query_tracking_ui <- function(id) {
    ns <- shiny::NS(id)
    bslib::layout_sidebar(fillable = FALSE,
        sidebar = bslib::sidebar(width = 350,
            data_source_ui(ns("source"), "Query 生命周期明细"), shiny::uiOutput(ns("mapping")),
            shiny::dateInput(ns("as_of"), "评估快照日", as.Date("2026-11-24")),
            shiny::numericInput(ns("overdue"), "未关闭年龄 > 多少天为逾期", 14, min = 0, step = 1),
            date_settings_ui(ns), config_ui(ns)
        ),
        panel_card(bslib::card_header("站点概览"), shiny::textOutput(ns("status")),
            shiny::selectInput(ns("measure"), "柱图指标", c("未关闭 Query 数" = "OPEN", "逾期 Query 数" = "OVERDUE", "逾期比例 (%)" = "OVERDUE_PCT", "未关闭年龄中位数（天）" = "MEDIAN_OPEN_DAYS")),
            plotly::plotlyOutput(ns("bars"), height = "330px")),
        panel_card(bslib::card_header("站点统计：选中一行联动下方明细"), DT::DTOutput(ns("summary")),
            shiny::downloadButton(ns("download"), "下载站点结果"), shiny::downloadButton(ns("matrix_out"), "导出为模板 01 的 KRI 长表")),
        panel_card(bslib::card_header("历史未关闭 Query 积压"), plotly::plotlyOutput(ns("trend"), height = "300px")),
        panel_card(bslib::card_header("Query 钻取"),
            shiny::checkboxInput(ns("only_overdue"), "只看逾期且未关闭的 Query", FALSE), DT::DTOutput(ns("records")))
    )
}

query_tracking_server <- function(id, demo) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        raw <- data_source_server("source", demo$queries)
        output$mapping <- shiny::renderUI({
            data <- raw()
            shiny::tagList(column_input(ns("site"), "站点变量", data, c("SITEID", "SITE")),
                column_input(ns("id"), "Query 唯一编号", data, c("QUERYID", "QUERY_ID")),
                column_input(ns("open"), "开启日期", data, c("OPEN_DATE", "OPEN")),
                column_input(ns("close"), "关闭日期（未关闭为缺失）", data, c("CLOSE_DATE", "CLOSE")),
                column_input(ns("subject"), "受试者编号（可不选）", data, "USUBJID", optional = TRUE))
        })
        normalized <- shiny::reactive({
            shiny::req(input$site, input$id, input$open, input$close)
            ui_result(data.frame(SITE = display_group(raw()[[input$site]]), QUERYID = as.character(raw()[[input$id]]),
                ID = if (nzchar(input$subject)) as.character(raw()[[input$subject]]) else "",
                OPEN = date_values(raw()[[input$open]], input$date_format, input$date_origin, "开启日期"),
                CLOSE = date_values(raw()[[input$close]], input$date_format, input$date_origin, "关闭日期"), SOURCE_ROW = seq_len(nrow(raw()))))
        })
        result <- shiny::reactive({
            shiny::req(input$as_of, !is.null(input$overdue))
            shiny::validate(shiny::need(input$overdue >= 0 && is.finite(input$overdue), "逾期天数须为非负数。"))
            ui_result(query_snapshot(normalized(), as.Date(input$as_of), input$overdue))
        })
        output$summary <- DT::renderDT(standard_table(result()$summary) |> DT::formatRound(c("MEDIAN_OPEN_DAYS", "OVERDUE_PCT"), 2))
        output$bars <- plotly::renderPlotly({
            data <- result()$summary
            shiny::validate(shiny::need(nrow(data), "快照日前没有 Query。"))
            shiny::req(input$measure)
            data$DISPLAY <- data[[input$measure]]
            p <- plotly::plot_ly(data, x = ~SITE, y = ~DISPLAY, type = "bar", marker = list(color = "#376B9C"),
                text = ~paste0(htmltools::htmlEscape(SITE), "<br>未关闭：", OPEN, "<br>逾期：", OVERDUE, "<br>逾期比例：", round(OVERDUE_PCT, 2), "%"), hoverinfo = "text", textposition = "none")
            chart_layout(p, input$site, input$measure)
        })
        selected_site <- shiny::reactive({
            selected <- input$summary_rows_selected
            if (length(selected) && selected[1L] <= nrow(result()$summary)) result()$summary$SITE[selected[1L]] else NULL
        })
        output$trend <- plotly::renderPlotly({
            data <- result()$records
            shiny::validate(shiny::need(nrow(data), "快照日前没有 Query。"))
            first <- min(data$OPEN)
            last <- as.Date(input$as_of)
            dates <- unique(c(seq(first, last, by = max(1L, ceiling(as.numeric(last - first) / 35))), last))
            trend <- query_backlog(data, dates)
            if (!is.null(selected_site())) trend <- trend[trend$SITE == selected_site(), , drop = FALSE]
            palette <- grDevices::hcl.colors(max(3L, length(unique(trend$SITE))), "Dark 3")
            chart_layout(plotly::plot_ly(trend, x = ~DATE, y = ~OPEN, color = ~SITE, colors = palette, type = "scatter", mode = "lines+markers"),
                "快照日期", "当时未关闭 Query 数")
        })
        output$records <- DT::renderDT({
            data <- result()$records
            if (!is.null(selected_site())) data <- data[data$SITE == selected_site(), , drop = FALSE]
            if (isTRUE(input$only_overdue)) data <- data[data$OVERDUE, , drop = FALSE]
            original <- raw()[data$SOURCE_ROW, , drop = FALSE]
            original$.评估状态 <- ifelse(data$IS_OPEN, "未关闭", "已关闭")
            original$.年龄天数 <- data$AGE_DAYS
            original$.是否逾期 <- ifelse(data$OVERDUE, "是", "否")
            standard_table(original, selection = "none")
        })
        output$status <- shiny::renderText({
            data <- result()$records
            sprintf("快照 %s：共 %s 条 Query，其中未关闭 %s 条、逾期 %s 条；逾期比例以未关闭 Query 数为分母。",
                input$as_of, nrow(data), sum(data$IS_OPEN), sum(data$OVERDUE))
        })
        output$download <- shiny::downloadHandler(filename = function() "query_site_summary.csv", content = function(file) write_result_csv(result()$summary, file))
        output$matrix_out <- shiny::downloadHandler(filename = function() "KRI_queries.csv", content = function(file) {
            data <- result()$summary |> dplyr::transmute(SITEID = SITE, KRI = "Query逾期率(%)", VALUE = OVERDUE_PCT, DEN = OPEN, SNAPSHOT = as.Date(input$as_of))
            write_result_csv(data, file)
        })
        config_server(input, output, session, c(site = "select", id = "select", open = "select", close = "select", subject = "select",
            as_of = "date", overdue = "number", date_format = "text", date_origin = "select"))
        list(normalized = normalized, result = result)
    })
}
