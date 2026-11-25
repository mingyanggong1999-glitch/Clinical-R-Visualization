# ** 在项目根目录运行：Rscript tests/check_upload.R **
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
    invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
source("app.R", encoding = "UTF-8")

files <- c(ADSL = "tests/fixtures/upload_adsl.csv", ADAE = "tests/fixtures/upload_adae.csv")
raw <- lapply(files, read_dataset)
stopifnot(identical(raw$ADSL$SUBJECT[1], "001"))
stopifnot(suggest_mapping(names(raw$ADSL), "USUBJID") == "SUBJECT")
stopifnot(suggest_mapping(names(raw$ADAE), "AEBODSYS") == "SOC")

maps <- list(ADSL = c(STUDYID = "STUDY_ID", USUBJID = "SUBJECT", SITEID = "CENTER",
    TRT01P = "ARM", AGE = "AGE", SEX = "SEX", SAFFL = "SAFFL"),
    ADAE = c(USUBJID = "SUBJECT", AEBODSYS = "SOC", AEDECOD = "PT", TRTEMFL = "TRTEMFL"))

data <- prepare_uploaded(raw, maps, study_id = "",
    filters = list(ADSL = list(column = "SAFFL", values = "Y"), ADAE = list(column = "TRTEMFL", values = "Y")),
    numeric_columns = list(ADSL = "SCORE"))
stopifnot(nrow(data$ADSL) == 7L, nrow(data$ADAE) == 8L,
    all(data$ADAE$USUBJID %in% data$ADSL$USUBJID),
    all(data$ADAE$TRTEMFL == "Y"), is.numeric(data$ADSL$AGE),
    is.numeric(data$ADSL$SCORE), "TRT01P_SOURCE" %in% names(data$ADSL),
    as.character(data$ADSL$TRT01P[1]) == "Control",
    identical(raw$ADSL$TRT01P[1], "Planned-C"))

# 非分析集受试者缺失治疗组不影响已选择的人群。
raw2 <- raw
raw2$ADSL$ARM[raw2$ADSL$SAFFL == "N"] <- NA_character_
selected <- prepare_uploaded(raw2, maps, "", filters = list(ADSL = list(column = "SAFFL", values = "Y")))
stopifnot(nrow(selected$ADSL) == 7L)

# 无研究编号时，使用网页输入值；缺失必需字段会明确报错。
submap <- maps$ADSL[names(maps$ADSL) != "STUDYID"]
data2 <- prepare_uploaded(list(ADSL = raw$ADSL), list(ADSL = submap), "WEB-02")
stopifnot(all(data2$ADSL$STUDYID == "WEB-02"))
stopifnot(inherits(make_teal_data(data), "teal_data"))
stopifnot(inherits(make_modules(data), "teal_modules"))
stopifnot(all(to_date(c("0", "1"), origin = "1960-01-01", date_format = "numeric") ==
    as.Date(c("1960-01-01", "1960-01-02"))))
stopifnot(all(apply_risk_rules(demo_datasets()$KRI, KRI_RULES[0, ])$FLAG == "未评估"))
cat("PASS: upload aliases, leading zeros, mappings, source collision, filtering, types and teal modules\n")
