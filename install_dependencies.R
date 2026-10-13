# Install the application dependencies, without loading original study data.
packages <- c("shiny", "bslib", "htmltools")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
cat("Application dependencies are available.\n")
