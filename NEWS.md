# CRIO (development version)

* Scenarios are no longer tied to the RIO RCP 7.0 set:
  * `pathway` is named from SSP x RCP via `default_pathways` (SSP1-2.6 = "Green world");
    unlisted combinations get `"SSPx-RCPy"`. Band colours are assigned automatically.
  * `warming` is `"Lower emission"` for RCP 2.6 runs (`lower_emission_rcps`).
  * `list_scenarios()` picks up SSP1-5.
  * New `select_scenarios()` and `relabel_scenarios()` work on loaded data.
* B4T runs are compared with their own climate counterpart (`climate_counterpart()`:
  same SSP, GCM, RCP) instead of a single baseline; `build_b4t_comparison()` drops its
  `baseline_id` argument and returns `base_id`. B4T plots show the B4T runs of
  `baseline_id`.

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
