# **---------------------------------------------------------------------------**
# ** 小型统计工具：输入标准化后计算；界面、读文件和排版与计算分开。
# **---------------------------------------------------------------------------**

normalize_metrics <- function(data, site, metric, value, snapshot = "", denominator = "", date_format = "%Y-%m-%d", date_origin = "1960-01-01") {
    result <- data.frame(SITE = display_group(mapped_column(data, site)),
        METRIC = display_group(mapped_column(data, metric)), VALUE = numeric_values(mapped_column(data, value), value))
    result$DATE <- if (nzchar(snapshot)) date_values(mapped_column(data, snapshot), date_format, date_origin) else as.Date("2000-01-01")
    result$DEN <- if (nzchar(denominator)) numeric_values(mapped_column(data, denominator), denominator) else NA_real_
    if (anyNA(result$DATE)) stop("快照日期不能缺失。")
    if (anyDuplicated(result[c("SITE", "METRIC", "DATE")])) stop("每个站点×指标×日期只能有一条结果，请先整理汇总数据。")
    if (any(result$DEN < 0, na.rm = TRUE)) stop("分母不能小于 0。")
    result
}

default_risk_rules <- function(metrics) {
    result <- data.frame(METRIC = sort(unique(metrics)), RED_LOW = NA_real_, AMBER_LOW = NA_real_,
        AMBER_HIGH = NA_real_, RED_HIGH = NA_real_, MIN_N = 0)
    for (i in seq_len(nrow(result))) {
        metric <- result$METRIC[i]
        if (metric == "AE例次/100人") result[i, 2:6] <- c(25, 45, 210, 260, 15)
        if (metric == "Query逾期率(%)") result[i, 2:6] <- c(NA, NA, 30, 50, 0)
        if (metric == "录入滞后(天)") result[i, 2:6] <- c(NA, NA, 7, 14, 0)
    }
    result
}

apply_risk_rules <- function(data, rules) {
    if (anyDuplicated(rules$METRIC)) stop("每个指标只能配置一行阈值。")
    r <- rules[match(data$METRIC, rules$METRIC), , drop = FALSE]
    if (any(r$RED_LOW > r$AMBER_LOW, na.rm = TRUE) || any(r$AMBER_HIGH > r$RED_HIGH, na.rm = TRUE) ||
        any(r$AMBER_LOW > r$AMBER_HIGH, na.rm = TRUE) || any(r$MIN_N < 0, na.rm = TRUE)) stop("阈值顺序不正确，或最小分母小于 0。")
    has_bound <- rowSums(!is.na(r[c("RED_LOW", "AMBER_LOW", "AMBER_HIGH", "RED_HIGH")])) > 0
    enough <- !is.na(r$MIN_N) & (r$MIN_N == 0 | (!is.na(data$DEN) & data$DEN >= r$MIN_N))
    red <- (!is.na(r$RED_LOW) & data$VALUE <= r$RED_LOW) | (!is.na(r$RED_HIGH) & data$VALUE >= r$RED_HIGH)
    amber <- (!is.na(r$AMBER_LOW) & data$VALUE <= r$AMBER_LOW) | (!is.na(r$AMBER_HIGH) & data$VALUE >= r$AMBER_HIGH)
    data$FLAG <- ifelse(!has_bound | !enough | is.na(data$VALUE), "未评估", ifelse(red, "红", ifelse(amber, "黄", "绿")))
    data
}

normalize_population <- function(data, id, group) {
    result <- data.frame(ID = as.character(mapped_column(data, id)), GROUP = display_group(mapped_column(data, group)))
    if (anyNA(result$ID) || any(result$ID == "")) stop("分母数据中的受试者编号不能为空。")
    if (anyDuplicated(result$ID)) stop("分母数据须每名受试者一行；请先明确重复记录的处理规则。")
    result
}

normalize_events <- function(data, id, category = "") {
    result <- data.frame(ID = as.character(mapped_column(data, id)),
        CATEGORY = if (nzchar(category)) display_group(mapped_column(data, category)) else rep("全部事件", nrow(data)))
    if (".SOURCE_ROW" %in% names(data)) result$SOURCE_ROW <- data$.SOURCE_ROW
    result
}

wilson_interval <- function(n, den, level = 0.95) {
    z <- stats::qnorm(1 - (1 - level) / 2)
    p <- ifelse(den > 0, n / den, NA_real_)
    center <- (p + z^2 / (2 * den)) / (1 + z^2 / den)
    half <- z * sqrt(p * (1 - p) / den + z^2 / (4 * den^2)) / (1 + z^2 / den)
    data.frame(LOW = ifelse(den > 0, pmax(0, center - half) * 100, NA_real_),
        HIGH = ifelse(den > 0, pmin(1, center + half) * 100, NA_real_))
}

count_event_groups <- function(population, events, include_overall = TRUE) {
    linked <- dplyr::inner_join(events, population, by = "ID")
    categories <- unique(events$CATEGORY)
    if (!length(categories)) categories <- "全部事件"
    calculate <- function(pop, ev) {
        denom <- pop |> dplyr::count(GROUP, name = "DEN")
        counts <- ev |> dplyr::group_by(GROUP, CATEGORY) |>
            dplyr::summarise(C = dplyr::n(), N = dplyr::n_distinct(ID), .groups = "drop")
        tidyr::expand_grid(GROUP = denom$GROUP, CATEGORY = categories) |>
            dplyr::left_join(denom, by = "GROUP") |>
            dplyr::left_join(counts, by = c("GROUP", "CATEGORY")) |>
            dplyr::mutate(C = tidyr::replace_na(C, 0L), N = tidyr::replace_na(N, 0L),
                PCT = ifelse(DEN > 0, 100 * N / DEN, NA_real_))
    }
    result <- calculate(population, linked)
    if (include_overall && nrow(population)) {
        if ("合计" %in% population$GROUP) stop("真实组别名称与合计重名，请先调整显示名称。")
        result <- dplyr::bind_rows(result, calculate(dplyr::mutate(population, GROUP = "合计"), dplyr::mutate(linked, GROUP = "合计")))
    }
    result <- cbind(as.data.frame(result), wilson_interval(result$N, result$DEN))
    list(summary = result, records = as.data.frame(linked),
        unmatched = sum(is.na(events$ID) | !events$ID %in% population$ID))
}

continuous_summary <- function(data, id_used = TRUE, duplicate_action = "error") {
    if (id_used && anyDuplicated(data[c("ID", "GROUP", "X")])) {
        if (duplicate_action == "error") stop("同一受试者×组别×时间点存在多条记录；请筛选分析记录，或明确选择先取均值。")
        data <- data |> dplyr::group_by(ID, GROUP, X) |>
            dplyr::summarise(VALUE = if (all(is.na(VALUE))) NA_real_ else mean(VALUE, na.rm = TRUE), .groups = "drop")
    }
    summary <- data |> dplyr::group_by(GROUP, X) |>
        dplyr::summarise(TOTAL = dplyr::n(), N = sum(!is.na(VALUE)), MISSING = sum(is.na(VALUE)),
            MEAN = if (all(is.na(VALUE))) NA_real_ else mean(VALUE, na.rm = TRUE),
            SD = stats::sd(VALUE, na.rm = TRUE),
            MEDIAN = if (all(is.na(VALUE))) NA_real_ else stats::median(VALUE, na.rm = TRUE),
            Q1 = if (all(is.na(VALUE))) NA_real_ else unname(stats::quantile(VALUE, 0.25, na.rm = TRUE, type = 2)),
            Q3 = if (all(is.na(VALUE))) NA_real_ else unname(stats::quantile(VALUE, 0.75, na.rm = TRUE, type = 2)), .groups = "drop")
    se <- summary$SD / sqrt(summary$N)
    critical <- rep(NA_real_, nrow(summary))
    valid <- summary$N > 1
    critical[valid] <- stats::qt(0.975, summary$N[valid] - 1)
    summary$LOW <- summary$MEAN - critical * se
    summary$HIGH <- summary$MEAN + critical * se
    list(summary = as.data.frame(summary), records = as.data.frame(data))
}

query_snapshot <- function(data, as_of, overdue_days = 14) {
    if (anyDuplicated(data$QUERYID)) stop("Query 编号必须唯一；历史追踪明细请先整理为每条 Query 一行。")
    if (anyNA(data$QUERYID) || any(data$QUERYID == "")) stop("Query 编号不能缺失。")
    if (anyNA(data$OPEN)) stop("Query 开启日期不能缺失。")
    if (any(data$CLOSE < data$OPEN, na.rm = TRUE)) stop("存在关闭日期早于开启日期的记录。")
    data <- data[data$OPEN <= as_of, , drop = FALSE]
    data$IS_OPEN <- is.na(data$CLOSE) | data$CLOSE > as_of
    end <- data$CLOSE
    end[data$IS_OPEN] <- as_of
    data$AGE_DAYS <- as.numeric(end - data$OPEN)
    data$OVERDUE <- data$IS_OPEN & data$AGE_DAYS > overdue_days
    summary <- data |> dplyr::group_by(SITE) |>
        dplyr::summarise(TOTAL = dplyr::n(), OPEN = sum(IS_OPEN), CLOSED = sum(!IS_OPEN), OVERDUE = sum(OVERDUE),
            MEDIAN_OPEN_DAYS = if (any(IS_OPEN)) stats::median(AGE_DAYS[IS_OPEN]) else NA_real_,
            OVERDUE_PCT = if (any(IS_OPEN)) 100 * sum(OVERDUE) / sum(IS_OPEN) else NA_real_, .groups = "drop")
    list(summary = as.data.frame(summary), records = data)
}

query_backlog <- function(data, dates) {
    dplyr::bind_rows(lapply(dates, function(date) {
        data |> dplyr::filter(OPEN <= date, is.na(CLOSE) | CLOSE > date) |>
            dplyr::count(SITE, name = "OPEN") |>
            tidyr::complete(SITE = unique(data$SITE), fill = list(OPEN = 0)) |>
            dplyr::mutate(DATE = as.Date(date, origin = "1970-01-01"))
    })) |> as.data.frame()
}

# 漏斗图控制限：按样本量给出总体均值及 z 倍正态近似控制限（适合率类指标）。
funnel_bounds <- function(value, den, z = 1.96) {
    valid <- !is.na(value) & !is.na(den) & den > 0
    if (sum(valid) < 2) return(NULL)
    v <- value[valid]; d <- den[valid]
    overall <- sum(v * d) / sum(d)
    sd_pool <- sqrt(sum(d * (v - overall)^2) / sum(d))
    grid <- seq(min(d), max(d), length.out = 120)
    data.frame(DEN = grid, MEAN = overall,
        LOW = overall - z * sd_pool / sqrt(grid),
        HIGH = overall + z * sd_pool / sqrt(grid))
}
