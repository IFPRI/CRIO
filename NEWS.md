# CRIO 1.0.0

# CRIO 0.1.0

* First version, converted from the RIO report scripts `v3_risk_analysis_RIO.R` and `v3_gap-RIO.R`.
* Single and dynamic threshold versions of the pathway analysis and maps merged into
  `build_pathway_analysis()` and `plot_indicator_maps()` (use `threshold` or `threshold_col`).
* Hard-coded input paths are now function arguments.
* RDS exports are named after the `load_crio_data()` list elements (e.g. `hunger.rds`
  instead of `hngr.rds`).
* New `crio_plot_all()`: makes every figure type for chosen indicators, regions and countries
  in one call, with per-figure-type switches, subfolders and error-tolerant looping.
* New `CALORIES-MDER` indicator spec (total calories vs each country's minimum requirement).
* Plot functions add `.png` to `outfile` when it has no extension.
