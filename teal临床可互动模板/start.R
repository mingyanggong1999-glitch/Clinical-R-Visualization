# ** 推荐启动入口：可直接在RStudio中打开本文件后点击Source。 **
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
    invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
ROOT <- local({
    candidates <- c(unlist(lapply(rev(sys.frames()), function(x) x$ofile), use.names = FALSE),
        sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
        candidates <- c(candidates, tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) ""))
    candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
    for (p in candidates) {
        if (identical(tolower(basename(p)), "start.r") && file.exists(p) && file.exists(file.path(dirname(p), "app.R")))
            return(dirname(normalizePath(p, winslash = "/", mustWork = TRUE)))
    }
    if (file.exists("app.R") && file.exists("R/bootstrap.R")) return(normalizePath(getwd(), winslash = "/"))
    stop("请打开本目录的Rproj项目，再Source start.R。")
})

source(file.path(ROOT, "R", "bootstrap.R"), encoding = "UTF-8")
check_packages()
shiny::runApp(ROOT, host = "127.0.0.1", launch.browser = TRUE)

