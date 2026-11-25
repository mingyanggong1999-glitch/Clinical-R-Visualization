# ** 初次使用运行一次；仅安装到独立TealTemplates包库。 **
# 已成功加载的包不会重复安装；之后打开应用不再联网安装软件包。
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
    invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
source("R/bootstrap.R", encoding = "UTF-8")
lib <- configure_library()
options(timeout = 180, repos = c(CRAN = "https://cloud.r-project.org"))
missing <- template_packages[!vapply(template_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, lib = lib, dependencies = NA)
check_packages()
message("teal 环境已就绪：", lib)

