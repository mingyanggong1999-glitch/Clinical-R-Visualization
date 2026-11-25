# **----------------------------------------------------------------------------**
# ** 01. Programming Set Up Here ----
# ** ★ 网页交互模式无需修改本文件；以下为默认设置与程序化兼容入口。 **
# **----------------------------------------------------------------------------**

DATA_MODE <- "demo"                       # ★ demo：模拟数据；files：真实文件
APP_TITLE <- "临床试验交互分析 · teal"
AS_OF     <- as.Date("2026-11-24")          # ★ Query / 入组的默认快照日期
STUDY_ID  <- ""                            # ★ 若真实ADSL无STUDYID，在这里填写研究编号

# ★ 输入文件：支持 sas7bdat、xpt、rds、csv、xlsx（默认首个工作表）。
# ADSL必需；其他数据集可留空，对应模块将不显示。
DATA_FILES <- c(
    ADSL  = "",
    ADAE  = "",
    ADLB  = "",
    ADTTE = "",
    QUERY = "",
    KRI   = ""
)

# ★ 左侧是模板内部标准变量，右侧填实际数据集中的变量名。
# 可增添其他人口学、基线变量；不需要的可选变量可以删掉。
VAR_MAP <- list(
    ADSL = c(STUDYID = "STUDYID", USUBJID = "USUBJID", SITEID = "SITEID", TRT01P = "TRT01P",
        SAFFL = "SAFFL", RANDDT = "RANDDT", AGE = "AGE", SEX = "SEX"),
    ADAE = c(USUBJID = "USUBJID", AESEQ = "AESEQ", AEBODSYS = "AEBODSYS",
        AEDECOD = "AEDECOD", AESER = "AESER", AESTDT = "AESTDT"),
    ADLB = c(USUBJID = "USUBJID", PARAMCD = "PARAMCD", AVISIT = "AVISIT",
        AVISITN = "AVISITN", AVAL = "AVAL", AVALU = "AVALU"),
    ADTTE = c(USUBJID = "USUBJID", PARAMCD = "PARAMCD", AVAL = "AVAL",
        AVALU = "AVALU", CNSR = "CNSR"),
    QUERY = c(USUBJID = "USUBJID", QUERYID = "QUERYID", OPEN_DATE = "OPEN_DATE",
        CLOSE_DATE = "CLOSE_DATE", FORM = "FORM"),
    KRI = c(SITEID = "SITEID", KRI = "KRI", VALUE = "VALUE", DEN = "DEN",
        SNAPSHOT = "SNAPSHOT")
)

# ★ 入组分析使用全部ADSL；如需FAS/SS，可配置POP_FILTER。
# 例如 quote(SAFFL == "Y")；子数据集同步保留该人群的记录。
POP_FILTER <- quote(TRUE)

# ★ 可按ADaM规则进一步选择分析记录；默认不额外筛选。
# 例如真实ADAE增加TRTEMFL映射，并将ADAE改为quote(TRTEMFL == "Y")。
RECORD_FILTER <- list(ADAE = quote(TRUE), ADLB = quote(TRUE), ADTTE = quote(TRUE))

# ★ 文件日期若为数值，按 SAS 日期原点转换；CSV日期默认 ISO 格式。
DATE_ORIGIN <- "1960-01-01"
DATE_FORMAT <- "%Y-%m-%d"
DEMOG_VARS  <- c("AGE", "SEX", "WEIGHT", "HEIGHT", "BMI", "SITEID", "SAFFL")
QUERY_DAYS  <- 14L

# ★ KRI为预先计算的独立结果；下方阈值仅供模拟演示，不是正式监察标准。
# 每行对应一个指标；NA表示不设置该方向的界限。
KRI_RULES <- data.frame(
    METRIC     = c("AE例次/100人", "Query逾期率(%)", "录入滞后(天)"),
    RED_LOW    = c(25, NA, NA),
    AMBER_LOW  = c(45, NA, NA),
    AMBER_HIGH = c(210, 30, 7),
    RED_HIGH   = c(260, 50, 14),
    MIN_N      = c(15, 0, 0)
)

