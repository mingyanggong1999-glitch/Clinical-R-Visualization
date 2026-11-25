# * 在项目根目录运行 source("tests/check_analysis.R", encoding = "UTF-8")。
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
    invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
source(file.path(root, "R", "bootstrap.R"), encoding = "UTF-8")
load_template_files(root, .GlobalEnv)

pop <- data.frame(ID = c("01", "02", "03"), GROUP = c("A", "A", "B"))
events <- data.frame(ID = c("01", "01", "03", "99"), CATEGORY = "AE")
result <- count_event_groups(pop, events)
stopifnot(identical(result$summary$N, c(1L, 1L, 2L)),
    identical(result$summary$C, c(2L, 1L, 3L)), identical(result$summary$DEN, c(2L, 1L, 3L)),
    result$unmatched == 1, abs(result$summary$PCT[3] - 200/3) < 1e-10)
stopifnot(all(count_event_groups(pop, events[FALSE, ], FALSE)$summary$N == 0))

metrics <- data.frame(SITE = "A", METRIC = "m", VALUE = c(5, 10, 15, 20, NA), DEN = c(10, 10, 10, 1, 10))
rules <- data.frame(METRIC = "m", RED_LOW = 5, AMBER_LOW = 10, AMBER_HIGH = 18, RED_HIGH = 20, MIN_N = 5)
stopifnot(identical(apply_risk_rules(metrics, rules)$FLAG, c("红", "黄", "绿", "未评估", "未评估")))

repeated <- data.frame(ID = c("1", "1", "2"), GROUP = "A", X = 1, VALUE = c(2, 4, NA))
stopifnot(inherits(try(continuous_summary(repeated), silent = TRUE), "try-error"))
result <- continuous_summary(repeated, TRUE, "mean")$summary
stopifnot(result$N == 1, result$MISSING == 1, result$MEAN == 3, is.na(result$LOW))

queries <- data.frame(SITE = "A", QUERYID = c("q1", "q2", "q3"),
    OPEN = as.Date(c("2026-11-01", "2026-11-15", "2026-12-01")),
    CLOSE = as.Date(c("2026-12-01", NA, NA)))
result <- query_snapshot(queries, as.Date("2026-11-24"), 14)
stopifnot(result$summary$OPEN == 2, result$summary$OVERDUE == 1, result$records$AGE_DAYS[1] == 23)
stopifnot(identical(query_backlog(queries, as.Date(c("2026-10-01", "2026-11-24")))$OPEN, c(0L, 2L)))
message("人数、例次、合计、阈值、重复访视及历史 Query 快照检查通过。")

graph_example <- data.frame(ID = c("1", "1", "2", "3"), GROUP = c("A", "A", "A", "B"),
    CATEGORY = c("X", "X", "X", "Y"), TEXT = c("", NA, "X", "Y"))
stopifnot(identical(graph_counts(graph_example, "CATEGORY", "GROUP", "ID", TRUE)$N, c(2L, 1L)))
stopifnot(graph_missing(graph_example, "TEXT", TRUE)$PCT == 50,
    graph_missing(graph_example, "TEXT", FALSE)$PCT == 25)
message("通用图形计数、去重及缺失率检查通过。")
