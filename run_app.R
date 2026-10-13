# Run from the repository root, or use: Rscript run_app.R
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
app_dir <- if (length(script_arg)) dirname(normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/")) else getwd()
if (!file.exists(file.path(app_dir, "app.R"))) stop("Set the working directory to the repository root first.")
shiny::runApp(appDir = app_dir, launch.browser = interactive())
