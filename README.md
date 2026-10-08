# CRIO

<!-- badges: start -->
[![R-CMD-check](https://github.com/IFPRI/CRIO/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/IFPRI/CRIO/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Risk and gap analysis for the CGIAR **R**esearch **I**nvestment **O**pportunities (RIO) report.

CRIO reads IMPACT model scenario results (through [GFSir](https://github.com/IFPRI)) and builds:

- hunger, diet (F&V, fats, sugar, calories), nutrient adequacy and blue water indicators against targets;
- pathway analysis: share of scenarios hitting each target and classification trees on socioeconomic pathway and warming;
- 4-panel indicator maps;
- target trajectories with climate-model ranges by SSP, at CG-region or country level;
- distance-to-target overviews across indicators;
- comparisons of Breeding For Tomorrow (B4T) scenarios against the baseline (heatmaps, gap closure).

## Installation

```r
# GFSir and DOORMAT from the IFPRI drat repository
install.packages(c("GFSir", "DOORMAT"), repos = c("https://ifpri.github.io/drat", getOption("repos")))

# gamstransfer ships with GAMS (see the GAMS documentation)

# CRIO from a local source folder
devtools::install("C:/EPTD/package_sources/CRIO")
```

## Scenarios

| Set | Files | Notes |
|---|---|---|
| Climate ensemble | `SSP{1,2,3}-{GFDL,IPSL,MPI,MRI,UKESM}-370-379` | RCP 7.0, no CO2 fertilization |
| Baseline | `SSP3-MRI-370-379` | `crio_baseline_id` |
| B4T | `SSP3-MRI-370-{CWANA,ESA,LAC,SA,SEA,WCA,CG6}` | suffix = CG region where B4T is implemented; `CG6` = all CG regions |

## Usage

Two worked workflows ship with the package:

```r
system.file("scripts", "risk_analysis.R", package = "CRIO")
system.file("scripts", "gap_analysis.R", package = "CRIO")
```

Minimal example:

```r
library(CRIO)

files    <- list_scenarios("path/to/Scenarios/RIO")
fulldata <- load_crio_data(files, nutrients_gdx, faostat_csv, sets_xlsx)
specs    <- indicator_specs(fulldata)

# Hunger trajectories for selected countries
plot_spec_trajectories(specs$HNGR, regions = c("NGA", "ETH", "IND"), b4t_show = "own")

# B4T effects on all indicators
b4t <- build_b4t_comparisons(specs)
```

## Package layout

| File | Contents |
|---|---|
| `R/constants.R` | baseline id, B4T regions, food groups, rest-of-world countries |
| `R/reference_data.R` | RNIs, nutrient labels, fat calorie shares, aquatic nutrient placeholders |
| `R/scenarios.R` | listing and reading scenarios, scenario descriptor columns |
| `R/regions.R` | country to CG region lookup |
| `R/indicators.R` | `get_*()` indicator readers and `load_crio_data()` |
| `R/specs.R` | indicator specs (targets, labels) |
| `R/pathways.R` | pathway analysis and tree plots |
| `R/maps.R` | 4-panel indicator maps |
| `R/trajectories.R` | trajectories and distance-to-target overviews |
| `R/b4t.R` | B4T comparisons, heatmaps, gap closure |
| `R/export.R` | RDS / CSV export |

## Known limitations

- Hunger: the hunger module floors the share at risk at 1%, scaled to each country's FAO 2020 numbers, so countries flatten at a country-specific minimum and scenario differences below it are not resolved.
- Nutrients: availability (supply), not intake. Aquatic food nutrient values are generic placeholders (`aquatic_nutrient_per_kg`).
- B4T runs exist only under SSP3-MRI.

## Authors

Abhijeet Mishra (IFPRI) and Claude (Anthropic, AI coding assistant).
