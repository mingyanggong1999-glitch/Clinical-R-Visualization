# **---------------------------------------------------------------------------**
# ** 完全模拟的数据；与真实项目无关。相同种子可重复生成。
# **---------------------------------------------------------------------------**
make_demo_data <- function(seed = 20261124L) {
    old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit(if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
    set.seed(seed)
    cutoff <- as.Date("2026-11-24")
    n <- 360L
    pop <- data.frame(
        USUBJID = sprintf("DEMO-%04d", seq_len(n)), SITEID = rep(sprintf("S%02d", 1:12), each = 30),
        TRT01P = sample(c("试验组", "对照组"), n, TRUE),
        SAFFL = sample(c("Y", "N"), n, TRUE, prob = c(0.94, 0.06)),
        RANDDT = as.Date("2025-05-01") + sample(0:420, n, TRUE)
    )
    event_counts <- rpois(n, ifelse(pop$SITEID == "S04", 0.3, ifelse(pop$SITEID == "S09", 3, 1.3)))
    ids <- rep(seq_len(n), event_counts)
    ae <- pop[ids, c("USUBJID", "SITEID", "TRT01P", "SAFFL")]
    ae$AESEQ <- sequence(event_counts)
    ae$AEDECOD <- sample(c("头痛", "恶心", "咳嗽", "疲乏", "皮疹"), nrow(ae), TRUE)
    ae$AESTDT <- pmin(cutoff, pop$RANDDT[ids] + sample(1:240, length(ids), TRUE))
    ae$AESER <- sample(c("Y", "N"), nrow(ae), TRUE, prob = c(0.07, 0.93))

    lab <- tidyr::expand_grid(USUBJID = pop$USUBJID, AVISITN = c(0, 4, 12, 24, 52), PARAMCD = c("FVCEFF", "ALT")) |>
        dplyr::left_join(pop, by = "USUBJID") |>
        dplyr::mutate(AVISIT = ifelse(AVISITN == 0, "基线", paste0("第", AVISITN, "周")),
            ADT = RANDDT + 7 * AVISITN,
            AVAL = ifelse(PARAMCD == "FVCEFF", rnorm(dplyr::n(), 72, 9) - AVISITN * 0.1,
                pmax(1, rnorm(dplyr::n(), 30, 10))),
            AVAL = ifelse(runif(dplyr::n()) < 0.08, NA_real_, AVAL)) |>
        dplyr::filter(ADT <= cutoff) |>
        as.data.frame()

    query <- pop[sample(seq_len(n), 950, TRUE), c("USUBJID", "SITEID")]
    query$QUERYID <- sprintf("Q%05d", seq_len(nrow(query)))
    query$OPEN_DATE <- as.Date("2026-06-01") + sample(0:175, nrow(query), TRUE)
    query$OPEN_DATE <- pmin(query$OPEN_DATE, cutoff)
    query$CLOSE_DATE <- query$OPEN_DATE + sample(1:45, nrow(query), TRUE)
    query$CLOSE_DATE[runif(nrow(query)) < 0.3 | query$CLOSE_DATE > cutoff] <- NA
    query$CLOSE_DATE[query$SITEID == "S04" & runif(nrow(query)) < 0.5] <- NA
    query$FORM <- sample(c("不良事件", "实验室检查", "合并用药", "基本信息"), nrow(query), TRUE)

    metrics <- tidyr::expand_grid(SITEID = unique(pop$SITEID),
        KRI = c("AE例次/100人", "Query逾期率(%)", "录入滞后(天)"),
        SNAPSHOT = as.Date(c("2026-06-30", "2026-07-31", "2026-08-31", "2026-09-30", "2026-10-31", "2026-11-24"))) |>
        dplyr::mutate(DEN = sample(20:40, dplyr::n(), TRUE),
            VALUE = dplyr::case_when(KRI == "AE例次/100人" ~ runif(dplyr::n(), 60, 180),
                KRI == "Query逾期率(%)" ~ runif(dplyr::n(), 5, 35),
                TRUE ~ runif(dplyr::n(), 1, 10)),
            VALUE = ifelse(SITEID == "S04" & KRI == "AE例次/100人", VALUE / 6, VALUE),
            VALUE = ifelse(SITEID == "S09" & KRI != "AE例次/100人", VALUE * 2.4, VALUE),
            VALUE = round(VALUE, 2)) |>
        as.data.frame()

    # 时间-事件数据（如停药/SAE 发生时间）：TIME 为天数，EVENT=1 事件、0 删失。
    # 试验组事件率更低，便于展示分组差异；随访上限 420 天，超出记为删失。
    survival <- pop[, c("USUBJID", "SITEID", "TRT01P")]
    survival$RATE <- ifelse(survival$TRT01P == "试验组", 1 / 320, 1 / 200)
    survival$TIME <- round(pmin(420, stats::rexp(n, survival$RATE)))
    survival$EVENT <- as.integer(survival$TIME < 420)
    survival$RATE <- NULL

    list(population = pop, events = ae, longitudinal = lab, queries = query, metrics = metrics,
        survival = survival, cutoff = cutoff)
}
