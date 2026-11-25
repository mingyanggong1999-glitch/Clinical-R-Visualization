# **---------------------------------------------------------------------------**
# ** 单独启动：连续指标趋势；保存后可在 RStudio Source 整个文件。
# ** 数据及变量名在打开后的界面选择，无需修改本启动程序。
# **---------------------------------------------------------------------------**
local({
    if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
        invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
    source_paths <- unlist(lapply(rev(sys.frames()), function(frame) frame$ofile), use.names = FALSE)
    command_paths <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    candidates <- c(source_paths, command_paths)
    candidates <- candidates[!is.na(candidates) & file.exists(candidates)]
    root <- if (length(candidates)) dirname(normalizePath(candidates[1L], winslash = "/")) else getwd()
    if (!file.exists(file.path(root, "R", "bootstrap.R"))) stop("请先保存启动脚本，或把工作目录设为模板文件夹。")
    source(file.path(root, "R", "bootstrap.R"), encoding = "UTF-8", local = TRUE)
    load_template_files(root, environment())
    options(shiny.maxRequestSize = 100 * 1024^2)
    shiny::runApp(make_dashboard("trend"), host = "127.0.0.1", launch.browser = interactive())
})
