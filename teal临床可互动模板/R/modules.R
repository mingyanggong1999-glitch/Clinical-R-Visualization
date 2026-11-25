# **----------------------------------------------------------------------------**
# ** teal原生统计和图形模块：按照本项目实际存在的数据动态组装。 ----
# **----------------------------------------------------------------------------**

make_modules <- function(datasets, as_of = AS_OF, query_days = QUERY_DAYS,
    kri_rules = KRI_RULES, demog_selected = DEMOG_VARS) {
    # 01. 研究总览、风险监察
    home <- list(overview_module())
    monitoring <- list()
    if ("KRI" %in% names(datasets)) monitoring <- c(monitoring, list(risk_module(kri_rules)))
    if ("QUERY" %in% names(datasets)) monitoring <- c(monitoring, list(query_module(as_of, query_days)))
    if ("RANDDT" %in% names(datasets$ADSL)) monitoring <- c(monitoring, list(enrollment_module(as_of)))

    # 02. NEST专用模块：人口学、SOC/PT、纵向分析、Kaplan-Meier。
    clinical <- list()
    demog_choices <- names(datasets$ADSL)[vapply(datasets$ADSL, function(x)
        (is.numeric(x) || is.character(x) || is.factor(x) || is.logical(x)) &&
            !inherits(x, c("Date", "POSIXt")), logical(1))]
    demog_choices <- setdiff(demog_choices, c("USUBJID", "STUDYID", "TRT01P", "TEAL_ROWID"))
    demog_vars <- intersect(demog_selected, demog_choices)
    if (!length(demog_vars)) demog_vars <- intersect("SITEID", demog_choices)
    if (length(demog_vars)) clinical <- c(clinical, list(
        teal.modules.clinical::tm_t_summary(
            label = "人口学与基线", dataname = "ADSL", parentname = "ADSL",
            arm_var = teal.picks::variables(choices = "TRT01P", selected = "TRT01P"),
            summarize_vars = teal.picks::variables(choices = demog_choices, selected = demog_vars, multiple = TRUE),
            numeric_stats = c("n", "mean_sd", "median", "quantiles", "range"),
            add_total = TRUE, total_label = "合计"
        )
    ))

    if ("ADAE" %in% names(datasets)) clinical <- c(clinical, list(
        teal.modules.clinical::tm_t_events(
            label = "AE SOC/PT 统计表", dataname = "ADAE", parentname = "ADSL",
            arm_var = teal.picks::variables("TRT01P", "TRT01P"),
            hlt = teal.picks::variables("AEBODSYS", "AEBODSYS"),
            llt = teal.picks::variables("AEDECOD", "AEDECOD"),
            event_type = "adverse event", add_total = TRUE, total_label = "合计"
        ),
        ae_chart_module()
    ))

    if ("ADLB" %in% names(datasets)) {
        params <- unique(na.omit(as.character(datasets$ADLB$PARAMCD)))
        yvars <- intersect(c("AVAL", "BASE", "CHG", "PCHG"), names(datasets$ADLB))
        clinical <- c(clinical, list(teal.modules.clinical::tm_g_lineplot(
            label = "连续指标访视趋势", dataname = "ADLB", parentname = "ADSL",
            group_var = teal.picks::variables("TRT01P", "TRT01P", multiple = FALSE),
            x = teal.picks::variables("AVISIT", "AVISIT", multiple = FALSE),
            y = teal.picks::variables(yvars, "AVAL", multiple = FALSE),
            y_unit = teal.picks::variables("AVALU", "AVALU"),
            paramcd = teal.picks::picks(teal.picks::variables("PARAMCD", fixed = TRUE),
                teal.picks::values(choices = params, selected = params[1L], multiple = FALSE), check_dataset = FALSE),
            interval = "mean_ci", table = c("n", "mean_sd", "median", "range")
        )))
    }
    if ("ADTTE" %in% names(datasets)) {
        params <- unique(na.omit(as.character(datasets$ADTTE$PARAMCD)))
        strata <- intersect(c("SEX", "SITEID"), names(datasets$ADSL))
        clinical <- c(clinical, list(teal.modules.clinical::tm_g_km(
            label = "KM 生存曲线", dataname = "ADTTE", parentname = "ADSL",
            arm_var = teal.picks::variables("TRT01P", "TRT01P", multiple = FALSE),
            paramcd = teal.picks::picks(teal.picks::datasets("ADTTE"),
                teal.picks::variables("PARAMCD", fixed = TRUE),
                teal.picks::values(choices = params, selected = params[1L], multiple = FALSE)),
            strata_var = teal.picks::variables(choices = strata, selected = NULL, multiple = TRUE),
            facet_var = teal.picks::variables(choices = strata, selected = NULL, multiple = FALSE)
        )))
    }

    # 03. 通用数据探索：这些页面无需另写UI、server或统计函数。
    exploration <- list(
        teal.modules.general::tm_data_table(label = "数据明细", dt_options = list(searching = TRUE, pageLength = 20, scrollX = TRUE)),
        teal.modules.general::tm_variable_browser(label = "变量描述"),
        teal.modules.general::tm_missing_data(label = "缺失值分布")
    )
    # 给通用图形指定有意义的初始变量，同时仍允许用户换数据集和变量。
    numeric_vars <- lapply(datasets, function(d) setdiff(names(d)[vapply(d, is.numeric, logical(1))], c("TEAL_ROWID", "AESEQ")))
    var_pick <- function(dataname, choices, selected) teal.picks::picks(
        teal.picks::datasets(choices = names(datasets), selected = dataname, fixed = FALSE),
        teal.picks::variables(choices = choices, selected = selected, multiple = FALSE), teal.picks::values())
    category_choice <- function(x) is.factor(x) || is.character(x) || is.logical(x)
    optional_pick <- function(dataname) var_pick(dataname, category_choice, NULL)
    numeric_ds <- names(numeric_vars)[lengths(numeric_vars) > 0L]
    if (length(numeric_ds)) {
        dn <- numeric_ds[1L]
        v <- numeric_vars[[dn]][1L]
        grouping <- if ("TRT01P" %in% names(datasets[[dn]])) "TRT01P" else "SITEID"
        exploration <- c(exploration, list(
            teal.modules.general::tm_g_distribution(label = "直方图 / 分布", dist_var = var_pick(dn, is.numeric, v),
                group_var = optional_pick(dn), strata_var = optional_pick(dn)),
            teal.modules.general::tm_g_bivariate(label = "箱线图 / 双变量关系",
                x = var_pick(dn, function(x) is.numeric(x) || is.factor(x) || is.character(x), grouping),
                y = var_pick(dn, is.numeric, v), row_facet = optional_pick(dn), col_facet = optional_pick(dn))
        ))
        scatter_ds <- names(numeric_vars)[lengths(numeric_vars) >= 2L]
        if (length(scatter_ds)) {
            dn <- scatter_ds[1L]
            vv <- numeric_vars[[dn]][1:2]
            exploration <- c(exploration, list(teal.modules.general::tm_g_scatterplot(label = "散点图",
                x = var_pick(dn, is.numeric, vv[1L]), y = var_pick(dn, is.numeric, vv[2L]),
                color_by = optional_pick(dn), size_by = var_pick(dn, is.numeric, NULL),
                row_facet = optional_pick(dn), col_facet = optional_pick(dn))))
        }
    }

    # 利用teal::modules生成原生分组导航。
    groups <- home
    if (length(monitoring)) groups <- c(groups, list(do.call(teal::modules, c(monitoring, list(label = "风险监察")))))
    if (length(clinical)) groups <- c(groups, list(do.call(teal::modules, c(clinical, list(label = "临床统计")))))
    groups <- c(groups, list(do.call(teal::modules, c(exploration, list(label = "通用探索")))))
    do.call(teal::modules, groups)
}

build_teal_app <- function(datasets) {
    td <- make_teal_data(datasets)
    app <- teal::init(data = td, modules = make_modules(datasets))
    app <- teal::modify_title(app, APP_TITLE)
    app <- teal::modify_header(app, shiny::tags$div(
        style = "padding: 14px 24px; background:#edf4f7; color:#214f67;",
        shiny::h3(APP_TITLE, style = "margin:0 0 4px 0;"),
        shiny::p(style = "margin:0;", if (DATA_MODE == "demo") "模拟数据 · NEST / teal 原生模块体验版" else "本地文件模式 · 当前项目数据")))
    teal::modify_footer(app, shiny::tags$div(style = "padding:12px;color:#586b7b;", "本机交互探索 · 正式分析和风险决策须按项目规范核查。"))
}

