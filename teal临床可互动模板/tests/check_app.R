# ** 在项目根目录运行：Rscript tests/check_app.R **
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
    invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
source("R/bootstrap.R", encoding = "UTF-8")
check_packages()
suppressPackageStartupMessages({library(dplyr); library(ggplot2)})
source("config.R", encoding = "UTF-8")
source("R/demo_data.R", encoding = "UTF-8")
source("R/data.R", encoding = "UTF-8")
source("R/monitoring.R", encoding = "UTF-8")
source("R/modules.R", encoding = "UTF-8")

stopifnot(DATA_MODE == "demo")
d <- prepare_datasets()
stopifnot(nrow(d$ADSL) == 360L, !anyDuplicated(d$ADSL$USUBJID))
stopifnot(all(d$ADAE$USUBJID %in% d$ADSL$USUBJID))
stopifnot(all(d$ADTTE$CNSR %in% c(0, 1)))
k <- apply_risk_rules(d$KRI, KRI_RULES)
stopifnot(nrow(k) == nrow(d$KRI), all(k$FLAG %in% c("红", "黄", "绿", "未评估")))
q <- query_summary(d$QUERY, AS_OF, QUERY_DAYS)
stopifnot(all(q$summary$TOTAL == q$summary$OPEN + q$summary$CLOSED), all(q$summary$OVERDUE <= q$summary$OPEN))
e <- event_chart_data(d$ADSL, d$ADAE, "AEDECOD")
stopifnot(all(e$N <= e$DEN), all(e$C >= e$N))
en <- enrollment_summary(d$ADSL, AS_OF, "TRT01P")
stopifnot(sum(en$N) == sum(d$ADSL$RANDDT <= AS_OF))
td <- make_teal_data(d)
stopifnot(inherits(td, "teal_data"))
app <- build_teal_app(d)
stopifnot(is.function(app$server), !is.null(app$ui))
cat("PASS: demo, KRI, query, AE, enrollment, teal_data, app construction\n")

# 使用模拟数据临时文件，检验真实文件模式、异名映射、可选数据集和人群筛选。
input <- demo_datasets()$ADSL
names(input)[names(input) == "SITEID"] <- "CENTER"
names(input)[names(input) == "TRT01P"] <- "ARM"
tmp <- tempfile(fileext = ".rds")
saveRDS(input, tmp)
DATA_MODE <- "files"
DATA_FILES["ADSL"] <- tmp
VAR_MAP$ADSL["SITEID"] <- "CENTER"
VAR_MAP$ADSL["TRT01P"] <- "ARM"
POP_FILTER <- quote(SAFFL == "Y")
d <- prepare_datasets()
stopifnot(identical(names(d), "ADSL"), nrow(d$ADSL) == 336L, all(d$ADSL$SAFFL == "Y"))
stopifnot(inherits(build_teal_app(d), "teal_app"))
cat("PASS: file import, rename, population subset and dynamic modules\n")

