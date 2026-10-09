# Chad MRV electrification platform - Shiny prototype

Shiny app (R) that shows the OnSSET least-cost electrification results for Chad
(four scenarios to 2030, results corrected for grid extension on 9 October 2026).

## Contents
- `app.R` - the app (French interface)
- `onsset_results/TCD_2025_2030_*_Results.csv` - OnSSET results, one file per scenario
- `TCD_Chad/Data/TCD/Boundaries/TCD_adm_0.gpkg` - national boundary
- `.here` - marks the project root for the `here` package
- `make_manifest.R` - writes `manifest.json` for Posit Connect Cloud

## Run locally
```r
shiny::runApp(".")
```

## Publish on Posit Connect Cloud
1. In R, with this folder as working directory: `source("make_manifest.R")`
2. Push this folder to a GitHub repository (public on the free plan).
3. On connect.posit.cloud: Publish > Shiny > choose the repository, the branch and `app.R` > Publish.
