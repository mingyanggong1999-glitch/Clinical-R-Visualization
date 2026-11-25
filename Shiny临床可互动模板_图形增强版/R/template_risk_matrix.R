# **---------------------------------------------------------------------------**
# ** 模板 01：KRI 长表映射、风险矩阵、阈值编辑、快照趋势和明细联动
# ** 输入一行 = 一个站点 × 一个指标 × 一个快照。VALUE 的单位由指标定义。
# **---------------------------------------------------------------------------**

risk_matrix_ui <- function(id) {
    ns <- shiny::NS(id)
    bslib::layout_sidebar(fillable = FALSE,
        sidebar = bslib::sidebar(width = 340,
            data_source_ui(ns("source"), "KRI 汇总长表"),
            shiny::uiOutput(ns("mapping")),
            date_settings_ui(ns), config_ui(ns)
        ),
        panel_card(bslib::card_header("快照与站点"),
            bslib::layout_columns(shiny::uiOutput(ns("snapshot_ui")), shiny::uiOutput(ns("sites_ui"))),
            shiny::textOutput(ns("status"))),
        panel_card(bslib::card_header("站点风险分布：各风险等级的指标数"),
            plotly::plotlyOutput(ns("risk_distribution"), height = "320px")),
        panel_card(bslib::card_header("站点 × KRI 风险矩阵：点击指标单元格查看趋势"),
            shiny::p("红 / 黄 / 绿按下方阈值分级；未设阈值、值缺失或分母不足显示为灰色。"),
            DT::DTOutput(ns("matrix"))),
        panel_card(bslib::card_header("阈值设置：双击数值单元格修改"),
            shiny::p("修改后按 Tab 或点击其他位置保存。低于或等于下限、高于或等于上限触发标记；空白表示不使用该边界。"),
            DT::DTOutput(ns("rules"))),
        panel_card(bslib::card_header("单项 KRI 的站点间比较"),
            bslib::layout_columns(shiny::uiOutput(ns("focus_site_ui")), shiny::uiOutput(ns("focus_metric_ui"))),
            plotly::plotlyOutput(ns("risk_values"), height = "330px")),
        panel_card(bslib::card_header("站点漏斗图：指标值 vs 样本量"),
            shiny::p("按所选指标绘制各站点实测值对分母/样本量；虚线为总体均值及 95% 正态近似控制限，适合率类指标，样本量越小散点越远离中心属正常现象。"),
            plotly::plotlyOutput(ns("funnel"), height = "340px")),
        panel_card(bslib::card_header("当前站点及指标随时间变化"),
            plotly::plotlyOutput(ns("trend"), height = "330px")),
        panel_card(bslib::card_header("当前站点的原始汇总明细"), DT::DTOutput(ns("details")),
            shiny::downloadButton(ns("download"), "下载当前快照结果"))
    )
}

risk_matrix_server <- function(id, demo) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        raw <- data_source_server("source", demo$metrics)
        rules <- shiny::reactiveVal(NULL)

        output$mapping <- shiny::renderUI({
            data <- raw()
            shiny::tagList(
                column_input(ns("site"), "站点变量", data, c("SITEID", "SITE", "GroupID")),
                column_input(ns("metric"), "指标名称变量", data, c("KRI", "METRIC", "MetricID")),
                column_input(ns("value"), "指标数值变量（按原单位）", data, c("VALUE", "Metric", "PCT"), numeric = TRUE),
                column_input(ns("date"), "快照日期变量", data, c("SNAPSHOT", "DATE", "SnapshotDate"), optional = TRUE),
                column_input(ns("den"), "分母 / 有效样本量变量", data, c("DEN", "Denominator"), optional = TRUE, numeric = TRUE)
            )
        })
        normalized <- shiny::reactive({
            shiny::req(input$site, input$metric, input$value)
            ui_result(normalize_metrics(raw(), input$site, input$metric, input$value,
                input$date, input$den, input$date_format, input$date_origin))
        })
        shiny::observeEvent(normalized(), {
            metrics <- sort(unique(normalized()$METRIC))
            current <- shiny::isolate(rules())
            if (is.null(current) || !identical(current$METRIC, metrics)) rules(default_risk_rules(metrics))
        })
        output$rules <- DT::renderDT({
            shiny::req(rules())
            DT::datatable(rules(), rownames = FALSE, selection = "none",
                editable = list(target = "cell", disable = list(columns = 0)),
                colnames = c("指标", "红色下限", "黄色下限", "黄色上限", "红色上限", "最小分母"),
                options = list(dom = "t", scrollX = TRUE, paging = FALSE))
        })
        shiny::observeEvent(input$rules_cell_edit, {
            edit <- input$rules_cell_edit
            current <- rules()
            column <- edit$col + 1L
            if (column < 2L || column > ncol(current)) return()
            value <- if (edit$value == "") NA_real_ else suppressWarnings(as.numeric(edit$value))
            if (!is.na(value) && !is.finite(value)) return()
            current[edit$row, column] <- value
            rules(current)
        })

        assessed <- shiny::reactive({ shiny::req(rules()); ui_result(apply_risk_rules(normalized(), rules())) })
        output$snapshot_ui <- shiny::renderUI({
            dates <- sort(unique(as.character(normalized()$DATE)), decreasing = TRUE)
            labels <- if (nzchar(input$date)) dates else "单次快照"
            shiny::selectInput(ns("snapshot"), "快照日期", stats::setNames(dates, labels), selected = dates[1L])
        })
        output$sites_ui <- shiny::renderUI({
            sites <- sort(unique(normalized()$SITE))
            shiny::selectizeInput(ns("sites"), "展示的站点", choices = sites, selected = sites, multiple = TRUE)
        })
        current <- shiny::reactive({
            shiny::req(input$snapshot)
            assessed() |> dplyr::filter(as.character(DATE) == input$snapshot, SITE %in% input$sites)
        })
        # * 图中不混合不同 KRI 的原始数值；堆积图只展示各状态指标的数量。
        flag_colors <- c("红" = "#C94F4F", "黄" = "#E4AE32", "绿" = "#48A080", "未评估" = "#A4AEBA")
        output$risk_distribution <- plotly::renderPlotly({
            data <- current()
            shiny::validate(shiny::need(nrow(data), "当前快照没有选中的站点。"))
            counts <- data |> dplyr::count(SITE, FLAG, name = "N") |>
                tidyr::complete(SITE, FLAG = names(flag_colors), fill = list(N = 0L))
            counts$FLAG <- factor(counts$FLAG, levels = names(flag_colors))
            p <- NULL
            for (flag in names(flag_colors)) {
                rows <- counts[counts$FLAG == flag, , drop = FALSE]
                if (is.null(p)) {
                    p <- plotly::plot_ly(rows, x = ~SITE, y = ~N, type = "bar", name = flag,
                        marker = list(color = unname(flag_colors[flag])), hoverinfo = "x+y+name")
                } else p <- plotly::add_bars(p, data = rows, x = ~SITE, y = ~N, name = flag,
                    marker = list(color = unname(flag_colors[flag])), hoverinfo = "x+y+name", inherit = FALSE)
            }
            plotly::layout(chart_layout(p, "站点", "指标数"), barmode = "stack", legend = list(orientation = "h"), yaxis = list(dtick = 1))
        })
        output$risk_values <- plotly::renderPlotly({
            shiny::req(input$focus_metric)
            data <- current() |> dplyr::filter(METRIC == input$focus_metric, !is.na(VALUE))
            shiny::validate(shiny::need(nrow(data), "当前指标在此快照没有有效数值。"))
            data <- data[order(data$VALUE), , drop = FALSE]
            p <- NULL
            for (flag in names(flag_colors)) {
                rows <- data[data$FLAG == flag, , drop = FALSE]
                if (!nrow(rows)) next
                if (is.null(p)) {
                    p <- plotly::plot_ly(rows, x = ~SITE, y = ~VALUE, type = "scatter", mode = "markers", name = flag,
                        marker = list(size = 13, color = unname(flag_colors[flag])), text = ~paste0("分母：", DEN), hoverinfo = "x+y+name+text")
                } else p <- plotly::add_markers(p, data = rows, x = ~SITE, y = ~VALUE, name = flag,
                    marker = list(size = 13, color = unname(flag_colors[flag])),
                    text = ~paste0("分母：", DEN), hoverinfo = "x+y+name+text", inherit = FALSE)
            }
            rule <- rules()[rules()$METRIC == input$focus_metric, , drop = FALSE]
            bounds <- unlist(rule[c("RED_LOW", "AMBER_LOW", "AMBER_HIGH", "RED_HIGH")])
            colors <- c("#B93636", "#C89511", "#C89511", "#B93636")
            shapes <- lapply(which(!is.na(bounds)), function(i) list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                y0 = bounds[i], y1 = bounds[i], line = list(color = colors[i], dash = "dash")))
            plotly::layout(chart_layout(p, "站点（按指标值排序）", input$focus_metric), shapes = shapes,
                xaxis = list(type = "category", categoryorder = "array", categoryarray = data$SITE))
        })
        output$funnel <- plotly::renderPlotly({
            shiny::req(input$focus_metric)
            data <- current() |> dplyr::filter(METRIC == input$focus_metric)
            shiny::validate(shiny::need(nrow(data), "当前指标在此快照没有有效数值。"))
            bounds <- funnel_bounds(data$VALUE, data$DEN)
            p <- NULL
            for (flag in names(flag_colors)) {
                rows <- data[data$FLAG == flag & !is.na(data$VALUE) & !is.na(data$DEN) & data$DEN > 0, , drop = FALSE]
                if (!nrow(rows)) next
                if (is.null(p)) {
                    p <- plotly::plot_ly(rows, x = ~DEN, y = ~VALUE, type = "scatter", mode = "markers", name = flag,
                        marker = list(size = 13, color = unname(flag_colors[flag])), text = ~paste0("分母：", DEN), hoverinfo = "x+y+name+text")
                } else p <- plotly::add_markers(p, data = rows, x = ~DEN, y = ~VALUE, name = flag,
                    marker = list(size = 13, color = unname(flag_colors[flag])),
                    text = ~paste0("分母：", DEN), hoverinfo = "x+y+name+text", inherit = FALSE)
            }
            if (is.null(p)) return(chart_layout(plotly::plot_ly(), "分母 / 样本量", input$focus_metric))
            if (!is.null(bounds)) {
                p <- plotly::add_lines(p, data = bounds, x = ~DEN, y = ~HIGH, name = "95% 上限",
                    line = list(color = "#B93636", dash = "dash", width = 1.5), hoverinfo = "skip", inherit = FALSE)
                p <- plotly::add_lines(p, data = bounds, x = ~DEN, y = ~LOW, name = "95% 下限",
                    line = list(color = "#B93636", dash = "dash", width = 1.5), hoverinfo = "skip", inherit = FALSE)
                p <- plotly::add_lines(p, data = bounds, x = ~DEN, y = ~MEAN, name = "总体均值",
                    line = list(color = "#34495E", dash = "dot", width = 1.5), hoverinfo = "skip", inherit = FALSE)
            }
            chart_layout(p, "分母 / 样本量", input$focus_metric)
        })
        matrix_data <- shiny::reactive({
            data <- current()
            shiny::validate(shiny::need(nrow(data), "请选择至少一个站点。"))
            data$DISPLAY <- ifelse(is.na(data$VALUE), "缺失", sprintf("%.2f [%s]", data$VALUE, data$FLAG))
            display <- tidyr::pivot_wider(data[c("SITE", "METRIC", "DISPLAY")], names_from = METRIC, values_from = DISPLAY)
            flags <- tidyr::pivot_wider(data[c("SITE", "METRIC", "FLAG")], names_from = METRIC, values_from = FLAG)
            result <- as.data.frame(display)
            for (i in seq_len(ncol(display) - 1L)) result[[paste0(".flag", i)]] <- flags[[names(display)[i + 1L]]]
            result
        })
        output$matrix <- DT::renderDT({
            data <- matrix_data()
            k <- (ncol(data) - 1L) / 2L
            table <- DT::datatable(data, rownames = FALSE, selection = "single", escape = TRUE,
                options = list(pageLength = 15, scrollX = TRUE,
                    language = list(search = "搜索：", lengthMenu = "每页 _MENU_ 行", info = "共 _TOTAL_ 个站点"),
                    columnDefs = list(list(visible = FALSE, targets = (k + 1L):(2L * k)))))
            for (i in seq_len(k)) table <- DT::formatStyle(table, columns = names(data)[i + 1L],
                valueColumns = paste0(".flag", i),
                backgroundColor = DT::styleEqual(c("红", "黄", "绿", "未评估"), c("#FCE4E4", "#FFF1C2", "#E1F2E9", "#ECEFF3")),
                color = "#17283B", fontWeight = "bold")
            table
        }, server = FALSE)
        output$focus_site_ui <- shiny::renderUI(shiny::selectInput(ns("focus_site"), "当前站点", sort(unique(normalized()$SITE))))
        output$focus_metric_ui <- shiny::renderUI(shiny::selectInput(ns("focus_metric"), "当前指标", sort(unique(normalized()$METRIC))))
        shiny::observeEvent(input$matrix_cell_clicked, {
            click <- input$matrix_cell_clicked
            data <- matrix_data()
            if (is.null(click$row) || click$row < 1 || click$row > nrow(data)) return()
            shiny::updateSelectInput(session, "focus_site", selected = data$SITE[click$row])
            if (!is.null(click$col) && click$col > 0 && click$col <= (ncol(data) - 1) / 2)
                shiny::updateSelectInput(session, "focus_metric", selected = names(data)[click$col + 1L])
        })
        output$trend <- plotly::renderPlotly({
            shiny::req(input$focus_site, input$focus_metric)
            shiny::validate(shiny::need(nzchar(input$date), "选择快照日期变量后，可查看纵向趋势。"))
            data <- assessed() |> dplyr::filter(SITE == input$focus_site, METRIC == input$focus_metric) |> dplyr::arrange(DATE)
            p <- plotly::plot_ly(data, x = ~DATE, y = ~VALUE, type = "scatter", mode = "lines+markers",
                text = ~paste0(SITE, "<br>", htmltools::htmlEscape(METRIC), "<br>数值：", VALUE, "<br>状态：", FLAG), hoverinfo = "text")
            rule <- rules()[rules()$METRIC == input$focus_metric, , drop = FALSE]
            bounds <- unlist(rule[c("RED_LOW", "AMBER_LOW", "AMBER_HIGH", "RED_HIGH")])
            colors <- c("#B93636", "#C89511", "#C89511", "#B93636")
            shapes <- lapply(which(!is.na(bounds)), function(i) list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                y0 = bounds[i], y1 = bounds[i], line = list(color = colors[i], dash = "dash")))
            chart_layout(plotly::layout(p, shapes = shapes), "快照日期", input$focus_metric)
        })
        output$details <- DT::renderDT({
            shiny::req(input$focus_site)
            standard_table(assessed() |> dplyr::filter(SITE == input$focus_site))
        })
        output$status <- shiny::renderText({
            data <- current()
            sprintf("%s 个站点，%s 项指标；红 %s、黄 %s、绿 %s、未评估 %s 个单元格。", length(unique(data$SITE)),
                length(unique(data$METRIC)), sum(data$FLAG == "红"), sum(data$FLAG == "黄"), sum(data$FLAG == "绿"), sum(data$FLAG == "未评估"))
        })
        output$download <- shiny::downloadHandler(filename = function() "KRI_snapshot.csv", content = function(file) write_result_csv(current(), file))
        config_server(input, output, session,
            c(site = "select", metric = "select", value = "select", date = "select", den = "select", date_format = "text", date_origin = "select"),
            extra = function() list(rules = rules()), restore_extra = function(x) if (!is.null(x$rules)) rules(as.data.frame(x$rules)))
        list(normalized = normalized, assessed = assessed, current = current, rules = rules)
    })
}
