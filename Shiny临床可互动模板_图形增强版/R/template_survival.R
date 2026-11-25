# **---------------------------------------------------------------------------**
# ** 模板 06：生存分析（Kaplan-Meier）。输入每受试者一行的时间-事件数据。
# ** 事件变量约定 1=事件、0=删失（ADaM 的 CNSR 需先派生为 1=事件）。
# **---------------------------------------------------------------------------**

normalize_survival <- function(data, time, event, group = "", id = "") {
    result <- data.frame(
        ID = if (nzchar(id)) as.character(mapped_column(data, id)) else as.character(seq_len(nrow(data))),
        GROUP = if (nzchar(group)) display_group(mapped_column(data, group)) else rep("全部", nrow(data)),
        TIME = numeric_values(mapped_column(data, time), time),
        EVENT = numeric_values(mapped_column(data, event), event))
    if (any(!result$EVENT %in% c(0, 1), na.rm = TRUE)) stop("事件变量应为 0（删失）或 1（事件）。")
    if (any(result$TIME < 0, na.rm = TRUE)) stop("时间变量不能为负。")
    result[!is.na(result$TIME) & !is.na(result$EVENT), , drop = FALSE]
}

survival_fit <- function(data) {
    if (length(unique(data$GROUP)) > 1L) {
        survival::survfit(survival::Surv(TIME, EVENT) ~ GROUP, data = data)
    } else {
        survival::survfit(survival::Surv(TIME, EVENT) ~ 1, data = data)
    }
}

survival_curves <- function(data) {
    fit <- survival_fit(data)
    s <- summary(fit)
    group <- if (is.null(s$strata)) rep("全部", length(s$time)) else sub("^GROUP=", "", as.character(s$strata))
    data.frame(GROUP = group, TIME = s$time, SURV = s$surv,
        LOWER = if (is.null(s$lower)) NA_real_ else s$lower,
        UPPER = if (is.null(s$upper)) NA_real_ else s$upper,
        N_RISK = s$n.risk, N_EVENT = s$n.event)
}

survival_summary <- function(data) {
    fit <- survival_fit(data)
    tab <- summary(fit)$table
    if (!is.null(fit$strata)) {
        tdf <- as.data.frame(tab)
        data.frame(GROUP = sub("^GROUP=", "", rownames(tdf)), N = tdf$records, EVENTS = tdf$events, MEDIAN = tdf$median)
    } else {
        data.frame(GROUP = "全部", N = fit$n, EVENTS = sum(data$EVENT), MEDIAN = unname(tab["median"]))
    }
}

survival_logrank <- function(data) {
    if (length(unique(data$GROUP)) <= 1L) return(NA_real_)
    test <- survival::survdiff(survival::Surv(TIME, EVENT) ~ GROUP, data = data)
    stats::pchisq(test$chisq, df = length(test$n) - 1L, lower.tail = FALSE)
}

survival_ui <- function(id) {
    ns <- shiny::NS(id)
    bslib::layout_sidebar(fillable = FALSE,
        sidebar = bslib::sidebar(width = 340,
            data_source_ui(ns("source"), "时间-事件数据（每受试者一行）"),
            shiny::uiOutput(ns("mapping")), config_ui(ns)),
        panel_card(bslib::card_header("Kaplan-Meier 生存曲线"),
            shiny::p("事件变量 1=事件、0=删失。曲线为右连续阶梯；虚线为 95% 置信区间。"),
            shiny::checkboxInput(ns("show_ci"), "显示 95% 置信区间", TRUE),
            plotly::plotlyOutput(ns("km"), height = "480px")),
        panel_card(bslib::card_header("生存统计"),
            shiny::textOutput(ns("logrank")),
            DT::DTOutput(ns("summary"))),
        panel_card(bslib::card_header("当前筛选后的输入记录"), DT::DTOutput(ns("records")))
    )
}

survival_server <- function(id, demo) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        raw <- data_source_server("source", demo$survival)
        output$mapping <- shiny::renderUI({
            data <- raw()
            shiny::tagList(
                column_input(ns("time"), "时间变量（数值）", data, c("TIME", "AVAL"), numeric = TRUE),
                column_input(ns("event"), "事件变量（1=事件，0=删失）", data, c("EVENT", "CNSR"), numeric = TRUE),
                column_input(ns("group"), "分组变量（可不选）", data, c("TRT01P", "GROUP"), optional = TRUE),
                column_input(ns("id"), "受试者编号（可不选）", data, "USUBJID", optional = TRUE)
            )
        })
        normalized <- shiny::reactive({
            shiny::req(input$time, input$event)
            ui_result(normalize_survival(raw(), input$time, input$event, input$group, input$id))
        })
        curves <- shiny::reactive({
            data <- normalized()
            shiny::validate(shiny::need(nrow(data) > 0L, "没有可用于分析的非缺失记录。"))
            ui_result(survival_curves(data))
        })
        output$km <- plotly::renderPlotly({
            data <- curves()
            groups <- unique(data$GROUP)
            palette <- graph_palette(length(groups))
            p <- plotly::plot_ly()
            for (i in seq_along(groups)) {
                d <- data[data$GROUP == groups[i], , drop = FALSE]
                col <- palette[i]
                p <- plotly::add_trace(p, data = d, x = ~TIME, y = ~SURV, name = groups[i],
                    type = "scatter", mode = "lines",
                    line = list(color = col, shape = "hv", width = 2), hoverinfo = "x+y+name")
                if (isTRUE(input$show_ci)) {
                    p <- plotly::add_trace(p, data = d, x = ~TIME, y = ~UPPER, type = "scatter", mode = "lines",
                        line = list(color = col, width = 1, dash = "dot"), opacity = 0.5,
                        hoverinfo = "skip", showlegend = FALSE)
                    p <- plotly::add_trace(p, data = d, x = ~TIME, y = ~LOWER, type = "scatter", mode = "lines",
                        line = list(color = col, width = 1, dash = "dot"), opacity = 0.5,
                        hoverinfo = "skip", showlegend = FALSE)
                }
            }
            plotly::layout(chart_layout(p, "时间", "生存概率"), yaxis = list(range = c(0, 1), dtick = 0.25))
        })
        output$logrank <- shiny::renderText({
            pval <- survival_logrank(normalized())
            if (is.na(pval)) "仅一个分组，未计算 log-rank 检验。"
            else sprintf("log-rank 检验 p 值：%s", format.pval(pval, digits = 3))
        })
        output$summary <- DT::renderDT({
            tab <- survival_summary(normalized())
            DT::formatRound(standard_table(tab, selection = "none"), "MEDIAN", 1)
        })
        output$records <- DT::renderDT(standard_table(normalized(), selection = "none"))
        config_server(input, output, session, c(time = "select", event = "select", group = "select", id = "select", show_ci = "check"))
        list(normalized = normalized, curves = curves)
    })
}
