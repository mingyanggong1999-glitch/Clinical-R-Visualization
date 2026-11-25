# ** 初次使用运行一次。独立用户包库，不改动现有 TFL 项目的包库。 **
if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]]))
    invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8"))
version <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")
base <- if (.Platform$OS.type == "windows") Sys.getenv("LOCALAPPDATA") else path.expand("~/.local/share")
lib <- file.path(base, "R", "ShinyTemplates", version)
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))
packages <- c("shiny", "bslib", "DT", "plotly", "haven", "readxl", "readr", "dplyr", "tidyr", "jsonlite")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org", lib = lib)
still_missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing)) stop("未安装成功：", paste(still_missing, collapse = ", "))
message("依赖已就绪。现在可以运行 start.R 或各 run_*.R。包库：", lib)
