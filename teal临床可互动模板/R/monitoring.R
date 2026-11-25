# **----------------------------------------------------------------------------**
# ** teal没有现成的中心化监察模块：本文件补充少量通用统计工具和teal模块。 ----
# **----------------------------------------------------------------------------**

apply_risk_rules <- function(data, rules) {
    if (anyDuplicated(rules$METRIC)) stop("KRI阈值每指标只能有一行")
    if (any(rules$RED_LOW > rules$AMBER_LOW, na.rm = TRUE) ||
        any(rules$AMBER_HIGH > rules$RED_HIGH, na.rm = TRUE) ||
        any(rules$AMBER_LOW > rules$AMBER_HIGH, na.rm = TRUE) ||
        any(rules$MIN_N < 0, na.rm = TRUE)) stop("KRI阈值顺序不正确")
    r <- rules[match(data$KRI, rules$METRIC), , drop = FALSE]
    has_bound <- rowSums(!is.na(r[c("RED_LOW", "AMBER_LOW", "AMBER_HIGH", "RED_HIGH")])) > 0
    enough <- !is.na(r$MIN_N) & (r$MIN_N == 0 | (!is.na(data$DEN) & data$DEN >= r$MIN_N))
    red <- (!is.na(r$RED_LOW) & data$VALUE <= r$RED_LOW) | (!is.na(r$RED_HIGH) & data$VALUE >= r$RED_HIGH)
    amber <- (!is.na(r$AMBER_LOW) & data$VALUE <= r$AMBER_LOW) | (!is.na(r$AMBER_HIGH) & data$VALUE >= r$AMBER_HIGH)
    data$FLAG <- ifelse(!has_bound | !enough | !is.finite(data$VALUE), "未评估",
        ifelse(red, "红", ifelse(amber, "黄", "绿")))
    data
}

query_summary <- function(data, as_of, overdue_days) {
    records <- data |>
        dplyr::filter(OPEN_DATE <= as_of) |>
        dplyr::mutate(IS_OPEN = is.na(CLOSE_DATE) | CLOSE_DATE > as_of,
            AGE_DAYS = as.numeric(dplyr::if_else(IS_OPEN, as_of, CLOSE_DATE) - OPEN_DATE),
            OVERDUE = IS_OPEN & AGE_DAYS > overdue_days)
    summary <- records |>
        dplyr::group_by(SITEID) |>
        dplyr::summarise(TOTAL = dplyr::n(), OPEN = sum(IS_OPEN), CLOSED = sum(!IS_OPEN),
            OVERDUE = sum(OVERDUE), OVERDUE_PCT = ifelse(OPEN > 0, 100 * OVERDUE / OPEN, NA_real_),
            .groups = "drop")
    list(summary = summary, records = records)
}

enrollment_summary <- function(data, as_of, by) {
    data <- data |>
        dplyr::filter(!is.na(RANDDT), RANDDT <= as_of) |>
        dplyr::mutate(MONTH = as.Date(format(RANDDT, "%Y-%m-01")), GROUP = as.character(.data[[by]]))
    if (!nrow(data)) return(data.frame(MONTH = as.Date(character()), GROUP = character(), N = integer(), CUM_N = integer()))
    months <- seq(min(data$MONTH), as.Date(format(as_of, "%Y-%m-01")), by = "month")
    data |>
        dplyr::count(MONTH, GROUP, name = "N") |>
        tidyr::complete(MONTH = months, GROUP, fill = list(N = 0L)) |>
        dplyr::group_by(GROUP) |>
        dplyr::arrange(MONTH, .by_group = TRUE) |>
        dplyr::mutate(CUM_N = cumsum(N)) |>
        dplyr::ungroup()
}

# AE例数：分母是当前ADSL中的独立受试者；零事件的组保留。
event_chart_data <- function(pop, events, variable, top = 10L) {
    den <- pop |> dplyr::count(TRT01P, name = "DEN")
    linked <- events |>
        dplyr::filter(USUBJID %in% pop$USUBJID) |>
        dplyr::mutate(TERM = tidyr::replace_na(as.character(.data[[variable]]), "<缺失>"))
    counts <- linked |> dplyr::group_by(TRT01P, TERM) |>
        dplyr::summarise(C = dplyr::n(), N = dplyr::n_distinct(USUBJID), .groups = "drop")
    terms <- linked |> dplyr::group_by(TERM) |>
        dplyr::summarise(N = dplyr::n_distinct(USUBJID), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(N), TERM) |> utils::head(top)
    tidyr::expand_grid(TRT01P = den$TRT01P, TERM = terms$TERM) |>
        dplyr::left_join(den, by = "TRT01P") |>
        dplyr::left_join(counts, by = c("TRT01P", "TERM")) |>
        dplyr::mutate(N = tidyr::replace_na(N, 0L), C = tidyr::replace_na(C, 0L),
            PCT = ifelse(DEN > 0, 100 * N / DEN, NA_real_),
            TERM = factor(TERM, levels = rev(terms$TERM)))
}

plot_theme <- function() ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "right", panel.grid.minor = ggplot2::element_blank())

interactive_plot <- function(p) plotly::ggplotly(p, tooltip = c("x", "y", "colour", "fill"))

interactive_table <- function(data, selection = "none") {
    DT::datatable(data, rownames = FALSE, selection = selection,
        options = list(pageLength = 10, scrollX = TRUE))
}

overview_module <- function() {
    teal::module(label = "研究总览", datanames = "ADSL",
        ui = function(id, ...) {
            ns <- shiny::NS(id)
            shiny::tagList(shiny::h3("研究数据总览"),
                shiny::p("左侧是 teal 的数据筛选器；切换上方 Module 菜单可复用筛选后的数据。"),
                shiny::fluidRow(shiny::column(4, shiny::h4("受试者人数"), shiny::textOutput(ns("n"))),
                    shiny::column(4, shiny::h4("中心数"), shiny::textOutput(ns("sites"))),
                    shiny::column(4, shiny::h4("治疗组"), shiny::textOutput(ns("arms")))),
                plotly::plotlyOutput(ns("plot"), height = "440px"), DT::DTOutput(ns("table")))
        },
        server = function(id, data) shiny::moduleServer(id, function(input, output, session) {
            pop <- shiny::reactive(data()[["ADSL"]])
            output$n <- shiny::renderText(nrow(pop()))
            output$sites <- shiny::renderText(dplyr::n_distinct(pop()$SITEID))
            output$arms <- shiny::renderText(dplyr::n_distinct(pop()$TRT01P))
            result <- shiny::reactive(dplyr::count(pop(), SITEID, TRT01P, name = "N"))
            output$plot <- plotly::renderPlotly(interactive_plot(
                ggplot2::ggplot(result(), ggplot2::aes(SITEID, N, fill = TRT01P)) +
                    ggplot2::geom_col(position = "dodge") + ggplot2::scale_fill_viridis_d(end = 0.8) +
                    ggplot2::labs(x = "中心", y = "人数", fill = "治疗组") + plot_theme()))
            output$table <- DT::renderDT(interactive_table(result()))
        }))
}

risk_module <- function(rules) {
    teal::module(label = "KRI 风险矩阵", datanames = "KRI",
        ui = function(id, ...) {
            ns <- shiny::NS(id)
            shiny::tagList(shiny::h3("中心 × KRI 风险矩阵"),
                shiny::p("预先计算的KRI结果独立于ADSL筛选；按配置阈值展示，非正式统计告警。选择明细行查看中心指标趋势。"),
                shiny::uiOutput(ns("snapshot_ui")), plotly::plotlyOutput(ns("heatmap"), height = "450px"),
                DT::DTOutput(ns("table")), shiny::h4("选定中心和指标的历史趋势"),
                plotly::plotlyOutput(ns("trend"), height = "330px"),
                shiny::tags$details(shiny::tags$summary("当前阈值配置（更换数据时可设置）"), DT::DTOutput(ns("rules"))))
        },
        server = function(id, data) shiny::moduleServer(id, function(input, output, session) {
            m <- shiny::reactive(apply_risk_rules(data()[["KRI"]], rules))
            output$snapshot_ui <- shiny::renderUI({
                dates <- sort(unique(as.character(m()$SNAPSHOT)), decreasing = TRUE)
                shiny::selectInput(session$ns("snapshot"), "快照日期", dates, selected = dates[1])
            })
            current <- shiny::reactive({
                shiny::req(input$snapshot)
                m() |> dplyr::filter(as.character(SNAPSHOT) == input$snapshot) |> dplyr::arrange(SITEID, KRI)
            })
            colors <- c("红" = "#C84E4E", "黄" = "#E5AC3C", "绿" = "#479B83", "未评估" = "#A8B1BA")
            output$heatmap <- plotly::renderPlotly({
                d <- current(); shiny::validate(shiny::need(nrow(d) > 0L, "当前快照无数据"))
                p <- ggplot2::ggplot(d, ggplot2::aes(KRI, SITEID, fill = FLAG,
                    text = paste0("中心：", htmltools::htmlEscape(SITEID), "<br>指标：", htmltools::htmlEscape(KRI), "<br>值：", VALUE, "<br>分母：", DEN))) +
                    ggplot2::geom_tile(color = "white", linewidth = 1) +
                    ggplot2::geom_text(ggplot2::aes(label = round(VALUE, 1)), size = 3.6) +
                    ggplot2::scale_fill_manual(values = colors, drop = FALSE) +
                    ggplot2::labs(x = NULL, y = "中心", fill = "风险") + plot_theme()
                plotly::ggplotly(p, tooltip = "text")
            })
            output$table <- DT::renderDT({
                tab <- interactive_table(current()[c("SITEID", "KRI", "SNAPSHOT", "VALUE", "DEN", "FLAG")], "single")
                DT::formatStyle(tab, "FLAG", backgroundColor = DT::styleEqual(names(colors), unname(colors)), color = "white")
            })
            output$trend <- plotly::renderPlotly({
                d <- current(); shiny::req(nrow(d) > 0L)
                idx <- input$table_rows_selected
                if (!length(idx) || idx > nrow(d)) idx <- 1L
                z <- m() |> dplyr::filter(SITEID == d$SITEID[idx], KRI == d$KRI[idx]) |> dplyr::arrange(SNAPSHOT)
                interactive_plot(ggplot2::ggplot(z, ggplot2::aes(SNAPSHOT, VALUE)) +
                    ggplot2::geom_line(color = "#267C91") + ggplot2::geom_point(ggplot2::aes(color = FLAG), size = 3) +
                    ggplot2::scale_color_manual(values = colors) + ggplot2::labs(title = paste(d$SITEID[idx], d$KRI[idx]),
                        x = "快照日期", y = "实测值", color = "风险") + plot_theme())
            })
            output$rules <- DT::renderDT(interactive_table(rules))
        }))
}

ae_chart_module <- function() {
    teal::module(label = "AE 图形（SOC/PT）", datanames = c("ADSL", "ADAE"),
        ui = function(id, ...) {
            ns <- shiny::NS(id)
            shiny::tagList(shiny::h3("各治疗组 AE 例数及发生比例"),
                shiny::p("n = 该分类发生事件的独立受试者；N = 当前筛选的ADSL人数。选择SOC或PT后可随时钻取原始记录。"),
                shiny::fluidRow(shiny::column(4, shiny::selectInput(ns("term"), "分类", c("SOC" = "AEBODSYS", "PT" = "AEDECOD"))),
                    shiny::column(4, shiny::selectInput(ns("measure"), "图形指标", c("发生率 (%)" = "PCT", "例数" = "N", "例次" = "C"))),
                    shiny::column(4, shiny::numericInput(ns("top"), "显示前几项", 10, min = 1, max = 50))),
                plotly::plotlyOutput(ns("chart"), height = "450px"), DT::DTOutput(ns("summary")),
                shiny::h4("选定行的AE明细（默认首行）"), DT::DTOutput(ns("detail")))
        },
        server = function(id, data) shiny::moduleServer(id, function(input, output, session) {
            result <- shiny::reactive({
                shiny::req(input$term, input$top)
                event_chart_data(data()[["ADSL"]], data()[["ADAE"]], input$term, as.integer(input$top))
            })
            output$chart <- plotly::renderPlotly({
                d <- result(); shiny::req(input$measure)
                shiny::validate(shiny::need(nrow(d) > 0, "当前筛选无事件记录"))
                interactive_plot(ggplot2::ggplot(d, ggplot2::aes(TERM, .data[[input$measure]], fill = TRT01P)) +
                    ggplot2::geom_col(position = "dodge") + ggplot2::coord_flip() + ggplot2::scale_fill_viridis_d(end = 0.8) +
                    ggplot2::labs(x = NULL, y = input$measure, fill = "治疗组") + plot_theme())
            })
            output$summary <- DT::renderDT(DT::formatRound(interactive_table(result(), "single"), "PCT", 2))
            output$detail <- DT::renderDT({
                d <- result(); idx <- input$summary_rows_selected
                shiny::validate(shiny::need(nrow(d) > 0L, "当前筛选无事件记录"))
                if (!length(idx) || idx > nrow(d)) idx <- 1L
                records <- data()[["ADAE"]]
                records <- records[records$USUBJID %in% data()[["ADSL"]]$USUBJID, , drop = FALSE]
                value <- tidyr::replace_na(as.character(records[[input$term]]), "<缺失>")
                interactive_table(records[as.character(records$TRT01P) == as.character(d$TRT01P[idx]) & value == as.character(d$TERM[idx]), , drop = FALSE])
            })
        }))
}

query_module <- function(as_of, overdue_days) {
    teal::module(label = "Query 追踪", datanames = "QUERY",
        ui = function(id, ...) {
            ns <- shiny::NS(id)
            shiny::tagList(shiny::h3("Query积压及逾期情况"),
                shiny::fluidRow(shiny::column(4, shiny::dateInput(ns("as_of"), "快照日期", as_of)),
                    shiny::column(4, shiny::numericInput(ns("days"), "未关闭超过多少天算逾期", overdue_days, min = 0))),
                plotly::plotlyOutput(ns("plot"), height = "400px"), DT::DTOutput(ns("summary")),
                shiny::checkboxInput(ns("only_overdue"), "仅展示逾期未关闭记录", FALSE),
                DT::DTOutput(ns("records")))
        },
        server = function(id, data) shiny::moduleServer(id, function(input, output, session) {
            result <- shiny::reactive({
                shiny::req(input$as_of, !is.null(input$days), input$days >= 0)
                query_summary(data()[["QUERY"]], as.Date(input$as_of), input$days)
            })
            output$plot <- plotly::renderPlotly({
                z <- result()$summary |>
                    tidyr::pivot_longer(c(OPEN, OVERDUE), names_to = "状态", values_to = "N")
                interactive_plot(ggplot2::ggplot(z, ggplot2::aes(SITEID, N, fill = 状态)) +
                    ggplot2::geom_col(position = "dodge") + ggplot2::labs(x = "中心", y = "Query数") + plot_theme())
            })
            output$summary <- DT::renderDT(DT::formatRound(interactive_table(result()$summary), "OVERDUE_PCT", 2))
            output$records <- DT::renderDT({
                z <- result()$records
                if (isTRUE(input$only_overdue)) z <- z[z$OVERDUE, , drop = FALSE]
                interactive_table(z)
            })
        }))
}

enrollment_module <- function(as_of) {
    teal::module(label = "入组进度", datanames = "ADSL",
        ui = function(id, ...) {
            ns <- shiny::NS(id)
            shiny::tagList(shiny::h3("按月随机入组进度"), shiny::p("基于当前ADSL人群及RANDDT；缺失日期不计入。"),
                shiny::fluidRow(shiny::column(4, shiny::dateInput(ns("as_of"), "截止日期", as_of)),
                    shiny::column(4, shiny::selectInput(ns("group"), "按组汇总", c("中心" = "SITEID", "治疗组" = "TRT01P")))),
                plotly::plotlyOutput(ns("trend"), height = "400px"),
                plotly::plotlyOutput(ns("monthly"), height = "350px"), DT::DTOutput(ns("summary")))
        },
        server = function(id, data) shiny::moduleServer(id, function(input, output, session) {
            result <- shiny::reactive({
                shiny::req(input$as_of, input$group)
                enrollment_summary(data()[["ADSL"]], as.Date(input$as_of), input$group)
            })
            output$trend <- plotly::renderPlotly(interactive_plot(
                ggplot2::ggplot(result(), ggplot2::aes(MONTH, CUM_N, color = GROUP)) +
                    ggplot2::geom_step() + ggplot2::geom_point() + ggplot2::labs(x = "月份", y = "累计入组人数", color = "组别") + plot_theme()))
            output$monthly <- plotly::renderPlotly(interactive_plot(
                ggplot2::ggplot(result(), ggplot2::aes(MONTH, N, fill = GROUP)) +
                    ggplot2::geom_col() + ggplot2::labs(x = "月份", y = "当月入组人数", fill = "组别") + plot_theme()))
            output$summary <- DT::renderDT(interactive_table(result()))
        }))
}

