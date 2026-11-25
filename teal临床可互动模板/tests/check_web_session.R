# ** 在项目根目录运行：Rscript tests/check_web_session.R **
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
    invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
app <- source("app.R", encoding = "UTF-8")$value
file_input <- function(path) data.frame(
    name = basename(path), size = file.info(path)$size, type = "text/csv",
    datapath = normalizePath(path, winslash = "/"), stringsAsFactors = FALSE)

shiny::testServer(app$serverFuncSource(), {
    stopifnot(grepl("upload_ADSL", output$workspace$html, fixed = TRUE))
    session$setInputs(upload_ADSL = file_input("tests/fixtures/upload_adsl.csv"),
        upload_ADAE = file_input("tests/fixtures/upload_adae.csv"))
    stopifnot(nrow(raw$ADSL) == 8L, nrow(raw$ADAE) == 10L,
        grepl("map_ADSL_USUBJID", output$config_ADSL$html, fixed = TRUE))
    session$setInputs(map_ADSL_STUDYID = "STUDY_ID", map_ADSL_USUBJID = "SUBJECT",
        map_ADSL_SITEID = "CENTER", map_ADSL_TRT01P = "ARM", map_ADSL_AGE = "AGE",
        map_ADSL_SEX = "SEX", map_ADSL_SAFFL = "SAFFL",
        map_ADAE_USUBJID = "SUBJECT", map_ADAE_AEBODSYS = "SOC",
        map_ADAE_AEDECOD = "PT", map_ADAE_TRTEMFL = "TRTEMFL",
        use_ADAE = TRUE, filter_ADSL = "SAFFL", filter_ADAE = "TRTEMFL",
        values_ADSL = "Y", values_ADAE = "Y", numeric_ADSL = "SCORE",
        study_id = "", analysis_date = "2026-11-25", query_days = 14,
        date_mode = "text", date_format = "%Y-%m-%d")
    session$setInputs(start_analysis = 1)
    if (is.null(active())) stop("网页启动失败：", message())
    stopifnot(identical(unname(active()$counts), c("ADSL: 7", "ADAE: 8")),
        grepl("更换数据", output$workspace$html, fixed = TRUE))
})

cat("PASS: Shiny web session, file inputs, mappings, filters, dataset counts and native teal UI\n")
