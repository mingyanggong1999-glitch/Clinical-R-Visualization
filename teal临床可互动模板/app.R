# **----------------------------------------------------------------------------**
# ** 临床交互分析：使用teal原生应用框架 ----
# ** 在RStudio中打开本文件，点击 Run App 即可。 **
# **----------------------------------------------------------------------------**

if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
    invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
source("R/bootstrap.R", encoding = "UTF-8", local = TRUE)
check_packages()
suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
})

source("config.R", encoding = "UTF-8", local = TRUE)
source("R/demo_data.R", encoding = "UTF-8", local = TRUE)
source("R/data.R", encoding = "UTF-8", local = TRUE)
source("R/monitoring.R", encoding = "UTF-8", local = TRUE)
source("R/modules.R", encoding = "UTF-8", local = TRUE)
source("R/upload.R", encoding = "UTF-8", local = TRUE)

# 默认浏览器上传模式；配置文件仅保留通用设置及旧的程序化入口。
options(shiny.maxRequestSize = 100 * 1024^2)
web_upload_app()

