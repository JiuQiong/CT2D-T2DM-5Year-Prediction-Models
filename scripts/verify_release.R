# Verify the packaged engine against expected synthetic example probabilities.
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))
stopifnot(file.exists("app.R"), file.exists("manifest.json"))
source("config/dictionary.R", local = TRUE, encoding = "UTF-8")
source("R/engine.R", local = TRUE, encoding = "UTF-8")
bundles <- load_prediction_bundles("models/five_year")
dictionary <- variable_dictionary()
expected <- read.csv("examples/expected_predictions.csv", stringsAsFactors = FALSE)
for (level in unique(expected$example)) {
  patient <- demo_patient(bundles, level)
  actual <- assess_all(patient, bundles, dictionary, demo = TRUE, t2dm_confirmed = TRUE)
  for (endpoint in names(actual)) {
    ref <- expected$probability[expected$example == level & expected$endpoint == endpoint]
    stopifnot(length(ref) == 1L, actual[[endpoint]]$status == "ok",
      abs(actual[[endpoint]]$probability - ref) < 1e-10)
  }
}
stopifnot("WWI" %in% bundles$Death$features, !("WWI" %in% bundles$CVD$features))
cat("PASS: both frozen models reproduce both synthetic examples within 1e-10.\n")
cat("CVD model: ", bundles$CVD$metadata$version, "\n", sep = "")
cat("Death model: ", bundles$Death$metadata$version, "\n", sep = "")
