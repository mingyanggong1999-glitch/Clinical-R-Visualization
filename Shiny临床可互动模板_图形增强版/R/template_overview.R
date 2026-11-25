# **---------------------------------------------------------------------------**
# ** 模板 00：总览。KPI 卡片汇总当前会话（初始为模拟数据），并给出各模块入口与定位。
# ** 不单独导入文件；各模块仍在各自页签内独立导入/映射数据。
# **---------------------------------------------------------------------------**

overview_kpis <- function(demo) {
    result <- c(sites = NA_integer_, subjects = NA_integer_, open_queries = NA_integer_, red = NA_integer_)
    pop <- demo$population
    if (is.data.frame(pop) && nrow(pop)) {
        result["sites"] <- length(unique(as.character(pop$SITEID)))
        result["subjects"] <- nrow(pop)
    }
    if (is.data.frame(demo$queries) && "CLOSE_DATE" %in% names(demo$queries)) {
        result["open_queries"] <- sum(is.na(demo$queries$CLOSE_DATE))
    }
    red <- tryCatch({
        norm <- normalize_metrics(demo$metrics, "SITEID", "KRI", "VALUE", "SNAPSHOT", "DEN")
        assessed <- apply_risk_rules(norm, default_risk_rules(unique(norm$METRIC)))
        latest <- max(assessed$DATE, na.rm = TRUE)
        sum(assessed$FLAG[assessed$DATE == latest] == "红")
    }, error = function(e) NA_integer_)
    result["red"] <- red
    result
}

overview_ui <- function(id) {
    ns <- shiny::NS(id)
    shiny::tagList(
        bslib::layout_column_wrap(width = 1 / 4, fill = FALSE,
            bslib::value_box("站点数", shiny::textOutput(ns("kpi_sites")), showcase = shiny::icon("hospital")),
            bslib::value_box("受试者数", shiny::textOutput(ns("kpi_subjects")), showcase = shiny::icon("users")),
            bslib::value_box("未关闭 Query", shiny::textOutput(ns("kpi_queries")), showcase = shiny::icon("comment")),
            bslib::value_box("红色风险单元", shiny::textOutput(ns("kpi_red")), showcase = shiny::icon("exclamation-triangle"))
        ),
        shiny::p("以上为当前会话数据（初始为完全模拟数据）的汇总快照，按默认阈值在最新快照上评估风险单元。",
            style = "color:#6a7a89;margin:12px 4px 4px;"),
        bslib::layout_column_wrap(width = 1 / 3, fill = FALSE,
            bslib::card(
                bslib::card_header(shiny::icon("shield"), " 风险监控"),
                shiny::p("按阈值给站点×指标分级（红/黄/绿），追踪 Query 生命周期与逾期。"),
                shiny::actionButton(ns("go_risk"), "KRI 风险矩阵", class = "btn-outline-primary"),
                shiny::actionButton(ns("go_query"), "Query 追踪", class = "btn-outline-primary")
            ),
            bslib::card(
                bslib::card_header(shiny::icon("table"), " 数据描述"),
                shiny::p("事件计数与 Wilson 置信区间、连续指标趋势、生存分析与入组进度。"),
                shiny::actionButton(ns("go_events"), "事件计数与钻取", class = "btn-outline-primary"),
                shiny::actionButton(ns("go_trend"), "连续指标趋势", class = "btn-outline-primary"),
                shiny::actionButton(ns("go_survival"), "生存分析", class = "btn-outline-primary"),
                shiny::actionButton(ns("go_enrollment"), "入组进度", class = "btn-outline-primary")
            ),
            bslib::card(
                bslib::card_header(shiny::icon("bar-chart"), " 图形探索"),
                shiny::p("任意结构数据的通用图形：分布、关系、计数与缺失模式。"),
                shiny::actionButton(ns("go_graphs"), "通用图形", class = "btn-outline-primary")
            )
        )
    )
}

overview_server <- function(id, demo, navigate = function(value) NULL) {
    shiny::moduleServer(id, function(input, output, session) {
        kpi <- shiny::reactive(overview_kpis(demo))
        output$kpi_sites <- shiny::renderText(kpi()["sites"])
        output$kpi_subjects <- shiny::renderText(kpi()["subjects"])
        output$kpi_queries <- shiny::renderText(kpi()["open_queries"])
        output$kpi_red <- shiny::renderText(kpi()["red"])
        go <- function(value) shiny::observeEvent(input[[value]], navigate(sub("^go_", "", value)))
        go("go_risk"); go("go_query"); go("go_events"); go("go_trend"); go("go_survival"); go("go_enrollment"); go("go_graphs")
    })
}
