# ** 标准 Shiny 入口：RStudio 点击 Run App，或 shiny::runApp(本文件所在目录)。 **
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
    invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
source(file.path("R", "bootstrap.R"), encoding = "UTF-8", local = TRUE)
load_template_files(getwd(), environment())
options(shiny.maxRequestSize = 100 * 1024^2)
make_dashboard("all")
