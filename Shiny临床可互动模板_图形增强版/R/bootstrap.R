# **---------------------------------------------------------------------------**
# ** 只负责加载依赖和公共 / 模板文件，不依赖现有 TFL setup。
# ** 正常启动不自动下载软件包。初次使用先运行 install_packages.R。
# **---------------------------------------------------------------------------**

template_library <- function() {
    version <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")
    root <- if (.Platform$OS.type == "windows") Sys.getenv("LOCALAPPDATA") else path.expand("~/.local/share")
    file.path(root, "R", "ShinyTemplates", version)
}

load_template_files <- function(root, envir = parent.frame()) {
    lib <- template_library()
    if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
    required <- c("shiny", "bslib", "DT", "plotly", "haven", "readxl", "readr", "dplyr", "tidyr", "jsonlite")
    missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing)) stop("缺少软件包：", paste(missing, collapse = ", "), "。请先运行同目录 install_packages.R。", call. = FALSE)
    files <- c("data_tools.R", "demo_data.R", "analysis_tools.R", "template_risk_matrix.R", "template_event_counts.R",
        "template_longitudinal.R", "template_query_tracking.R", "template_graphs.R",
        "template_survival.R", "template_enrollment.R", "template_overview.R", "dashboard.R")
    for (file in files) sys.source(file.path(root, "R", file), envir = envir, keep.source = TRUE)
    invisible(root)
}
