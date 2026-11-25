# **---------------------------------------------------------------------------**
# ** 各模板既可一起打开，也可单独启动。所有数据及映射都位于会话内部。
# ** 统一入口按功能分组：总览 → 风险监控 → 数据描述 → 图形探索。
# **---------------------------------------------------------------------------**

make_dashboard <- function(template = c("all", "overview", "risk", "events", "trend", "query", "graphs", "survival", "enrollment")) {
    template <- match.arg(template)
    demo <- make_demo_data()
    theme <- bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#255F80", base_font = "Microsoft YaHei, sans-serif")

    all_panels <- list(
        overview   = bslib::nav_panel("总览", value = "overview", overview_ui("ov")),
        risk       = bslib::nav_panel("KRI 风险矩阵", value = "risk", risk_matrix_ui("risk")),
        events     = bslib::nav_panel("事件计数与钻取", value = "events", event_counts_ui("events")),
        trend      = bslib::nav_panel("连续指标趋势", value = "trend", longitudinal_ui("trend")),
        query      = bslib::nav_panel("Query 追踪", value = "query", query_tracking_ui("query")),
        survival   = bslib::nav_panel("生存分析", value = "survival", survival_ui("survival")),
        enrollment = bslib::nav_panel("入组进度", value = "enrollment", enrollment_ui("enrollment")),
        graphs     = bslib::nav_panel("通用图形", value = "graphs", graphs_ui("graphs"))
    )

    if (template == "all") {
        navs <- list(
            all_panels$overview,
            bslib::nav_menu("风险监控", all_panels$risk, all_panels$query),
            bslib::nav_menu("数据描述", all_panels$events, all_panels$trend, all_panels$survival, all_panels$enrollment),
            all_panels$graphs
        )
    } else {
        navs <- all_panels[template]
    }

    ui <- do.call(bslib::page_navbar, c(list(title = "临床数据交互分析模板", id = "navbar", theme = theme, fillable = FALSE,
        header = shiny::tags$div(style = "padding:12px 24px;background:#edf4f7;color:#24485c;",
            "先体验模拟数据，再导入自己的数据并选择对应变量。阈值和统计口径可在各模板中设置。"),
        footer = shiny::tags$div(style = "padding:12px 24px;color:#526475;", "本机运行 · 初始数据为完全模拟数据 · 结果与映射可下载")),
        unname(navs)))

    ui <- shiny::tagList(shiny::tags$head(shiny::tags$link(rel = "icon", href = "data:,")), ui)

    server <- function(input, output, session) {
        navigate <- function(value) bslib::nav_select("navbar", value, session)
        run_server <- function(name) {
            switch(name,
                overview   = overview_server("ov", demo, navigate = navigate),
                risk       = risk_matrix_server("risk", demo),
                events     = event_counts_server("events", demo),
                trend      = longitudinal_server("trend", demo),
                query      = query_tracking_server("query", demo),
                survival   = survival_server("survival", demo),
                enrollment = enrollment_server("enrollment", demo),
                graphs     = graphs_server("graphs", demo)
            )
        }
        selected <- if (template == "all") names(all_panels) else template
        for (name in selected) run_server(name)
    }

    shiny::shinyApp(ui, server, options = list(launch.browser = interactive()))
}
