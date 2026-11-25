# **----------------------------------------------------------------------------**
# ** 公共环境：独立teal包库；可读既有Shiny依赖，不修改原项目。 ----
# **----------------------------------------------------------------------------**
teal_library <- function() {
    version <- paste(R.version$major, sub("\\..*$", "", R.version$minor), sep = ".")
    root <- if (.Platform$OS.type == "windows") Sys.getenv("LOCALAPPDATA") else path.expand("~/.local/share")
    file.path(root, "R", "TealTemplates", version)
}

configure_library <- function() {
    lib <- teal_library()
    shiny_lib <- file.path(dirname(dirname(lib)), "ShinyTemplates", basename(lib))
    dir.create(lib, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(lib, shiny_lib, .libPaths()))
    invisible(lib)
}

template_packages <- c("teal", "teal.data", "teal.picks", "teal.modules.general",
    "teal.modules.clinical", "shiny", "DT", "plotly", "ggplot2", "dplyr", "tidyr",
    "haven", "readr", "readxl", "bslib", "rlang", "sparkline")

check_packages <- function() {
    configure_library()
    missing <- template_packages[!vapply(template_packages, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing)) stop("请先运行 install_packages.R。缺少或无法加载：", paste(missing, collapse = ", "), call. = FALSE)
    invisible(TRUE)
}

