# **---------------------------------------------------------------------------**
# ** 模板 02：分母来自受试者数据；事件记录通过受试者编号连接。
# ** N=去重受试者人数，C=事件记录数，PCT=N/DEN*100；合计独立去重计算。
# **---------------------------------------------------------------------------**

event_counts_ui <- function(id) {
    ns <- shiny::NS(id)
    bslib::layout_sidebar(fillable = FALSE,
        sidebar = bslib::sidebar(width = 350,
            data_source_ui(ns("pop_source"), "1. 分母数据（每名受试者一行）"),
            shiny::uiOutput(ns("pop_mapping")), shiny::uiOutput(ns("pop_values_ui")),
            data_source_ui(ns("event_source"), "2. 事件 / 方案违背数据"),
            shiny::uiOutput(ns("event_mapping")), shiny::uiOutput(ns("event_values_ui")),
            shiny::checkboxInput(ns("overall"), "展示独立计算的合计组", TRUE),
            shiny::dateInput(ns("as_of"), "结果快照日期（导出矩阵时使用）", as.Date("2026-11-24")),
            config_ui(ns)
        ),
        panel_card(bslib::card_header("分组统计"), shiny::textOutput(ns("status")),
            shiny::p("百分比分母来自左侧筛选后的受试者数据；事件中未匹配到该人群的记录会排除。"),
            bslib::layout_columns(shiny::uiOutput(ns("category_ui")),
                shiny::selectInput(ns("measure"), "显示统计量", c("受试者发生比例 (%)" = "PCT", "受试者人数 n" = "N", "事件例次" = "C"))),
            plotly::plotlyOutput(ns("bars"), height = "370px")),
        panel_card(bslib::card_header("结果明细：选中一行钻取事件记录"), DT::DTOutput(ns("summary")),
            shiny::downloadButton(ns("download"), "下载统计结果"),
            shiny::downloadButton(ns("matrix_out"), "导出为模板 01 的 KRI 长表")),
        panel_card(bslib::card_header("受试者与事件钻取"), shiny::textOutput(ns("drill_info")), DT::DTOutput(ns("records")))
    )
}

event_counts_server <- function(id, demo) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        pop_raw <- data_source_server("pop_source", demo$population)
        event_raw <- data_source_server("event_source", demo$events)
        output$pop_mapping <- shiny::renderUI({
            data <- pop_raw()
            shiny::tagList(column_input(ns("pop_id"), "受试者编号", data, "USUBJID"),
                column_input(ns("group"), "分组变量（站点 / 治疗组）", data, c("SITEID", "TRT01P")),
                column_input(ns("pop_filter"), "分母筛选变量", data, c("SAFFL", "FASFL"), optional = TRUE))
        })
        output$pop_values_ui <- shiny::renderUI({
            shiny::req(input$pop_filter)
            values <- unique(display_group(pop_raw()[[input$pop_filter]]))
            shiny::selectizeInput(ns("pop_values"), "纳入分母的取值", values,
                selected = if ("Y" %in% values) "Y" else values, multiple = TRUE)
        })
        output$event_mapping <- shiny::renderUI({
            data <- event_raw()
            shiny::tagList(column_input(ns("event_id"), "事件数据中的受试者编号", data, "USUBJID"),
                column_input(ns("category"), "事件分类变量（可不选）", data, c("AEDECOD", "CATEGORY"), optional = TRUE),
                column_input(ns("event_filter"), "事件筛选变量（可不选）", data, character(), optional = TRUE))
        })
        output$event_values_ui <- shiny::renderUI({
            shiny::req(input$event_filter)
            values <- unique(display_group(event_raw()[[input$event_filter]]))
            shiny::selectizeInput(ns("event_values"), "纳入分析的事件取值", values, selected = values, multiple = TRUE)
        })
        population <- shiny::reactive({
            shiny::req(input$pop_id, input$group)
            ui_result(normalize_population(filter_values(pop_raw(), input$pop_filter, input$pop_values), input$pop_id, input$group))
        })
        events <- shiny::reactive({
            shiny::req(input$event_id)
            data <- event_raw()
            data$.SOURCE_ROW <- seq_len(nrow(data))
            ui_result(normalize_events(filter_values(data, input$event_filter, input$event_values), input$event_id, input$category))
        })
        result <- shiny::reactive(ui_result(count_event_groups(population(), events(), isTRUE(input$overall))))
        output$category_ui <- shiny::renderUI(shiny::selectInput(ns("focus_category"), "当前事件类别", unique(result()$summary$CATEGORY)))
        shown <- shiny::reactive({
            shiny::req(input$focus_category)
            result()$summary |> dplyr::filter(CATEGORY == input$focus_category)
        })
        output$summary <- DT::renderDT({
            data <- shown()
            standard_table(data) |> DT::formatRound(c("PCT", "LOW", "HIGH"), 2)
        })
        output$bars <- plotly::renderPlotly({
            data <- shown()
            shiny::validate(shiny::need(nrow(data), "当前人群或类别没有记录。"))
            shiny::req(input$measure)
            data$DISPLAY <- data[[input$measure]]
            p <- plotly::plot_ly(data, x = ~GROUP, y = ~DISPLAY, type = "bar", marker = list(color = "#287D8E"),
                text = ~paste0(htmltools::htmlEscape(GROUP), "<br>n=", N, "；N=", DEN, "<br>例次=", C, "<br>比例=", round(PCT, 2), "%"), hoverinfo = "text", textposition = "none")
            if (identical(input$measure, "PCT")) p <- plotly::style(p, error_y = list(type = "data", symmetric = FALSE,
                array = data$HIGH - data$PCT, arrayminus = data$PCT - data$LOW))
            chart_layout(p, input$group, if (identical(input$measure, "PCT")) "发生比例 (%)，误差线为 Wilson 95% CI" else input$measure)
        })
        drill <- shiny::reactive({
            data <- result()$records
            data <- data[data$CATEGORY == input$focus_category, , drop = FALSE]
            selected <- input$summary_rows_selected
            if (length(selected) && selected[1] <= nrow(shown())) {
                group <- shown()$GROUP[selected[1]]
                if (group != "合计") data <- data[data$GROUP == group, , drop = FALSE]
            }
            data
        })
        output$records <- DT::renderDT({
            data <- drill()
            original <- event_raw()[data$SOURCE_ROW, , drop = FALSE]
            original$.分析分组 <- data$GROUP
            standard_table(original, selection = "none")
        })
        output$drill_info <- shiny::renderText(sprintf("当前显示 %s 条事件记录、%s 名受试者。", nrow(drill()), length(unique(drill()$ID))))
        output$status <- shiny::renderText(sprintf("分母人群 %s 人；匹配事件 %s 条；因不属于当前分母人群排除 %s 条。",
            nrow(population()), nrow(result()$records), result()$unmatched))
        output$download <- shiny::downloadHandler(filename = function() "event_counts.csv", content = function(file) write_result_csv(result()$summary, file))
        output$matrix_out <- shiny::downloadHandler(filename = function() "KRI_event_counts.csv", content = function(file) {
            data <- result()$summary |> dplyr::filter(GROUP != "合计") |>
                dplyr::transmute(SITEID = GROUP, KRI = paste0(CATEGORY, "发生比例(%)"), VALUE = PCT, DEN, SNAPSHOT = as.Date(input$as_of))
            write_result_csv(data, file)
        })
        config_server(input, output, session, c(pop_id = "select", group = "select", pop_filter = "select", pop_values = "multi",
            event_id = "select", category = "select", event_filter = "select", event_values = "multi", overall = "check", as_of = "date"))
        list(population = population, events = events, result = result, shown = shown)
    })
}
