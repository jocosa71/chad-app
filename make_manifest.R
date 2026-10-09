# Generate manifest.json for Posit Connect Cloud.
# Run from the chad-app folder (setwd() to it, or open it as the working directory).
# The manifest records the R version and the exact package versions used by app.R.

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")

# Check that the files the app reads are in place before writing the manifest
stopifnot(
  file.exists("app.R"),
  file.exists(".here"),
  length(list.files("onsset_results", pattern = "_Results\\.csv$")) == 4,
  file.exists("TCD_Chad/Data/TCD/Boundaries/TCD_adm_0.gpkg")
)

rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
message("manifest.json written in ", normalizePath("."))
