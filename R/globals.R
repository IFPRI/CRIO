# Column names used with dplyr / ggplot2 non-standard evaluation
utils::globalVariables(c(
    # scenario and GFSir columns
    "region", "yrs", "ssp", "gcm", "rcp", "co2", "id", "value", "name",
    "description", "share", "b4t", "ensemble", "pathway", "warming", "co2_fert",
    "cty", "cg_region", "NEW_REGION",
    # indicators
    "fat_kcal", "fat_kcal_total", "total_kcal", "sugar_kcal",
    "CTY", "minKCAL", "Item", "year", "ader", "CG6", "DVDDVG", "WLD",
    "j_c", "cmdty", "kg_per_day", "nutrient_value_per_kg", "nutrient_value",
    "Nutrients", "total_nutrient", "RNI",
    # pathway analysis
    "thr", "hits_target", "n_hits", "n_scenarios", "pct_hits",
    "outcome", "cover", "rule", "cover_num", "total_cover", "n_rules", "rules",
    # plots and comparisons
    "val", "gap", "lo", "hi", "scenario", "indicator", "tile_label", "mean_val",
    "delta", "pct_change", "base_id", "base_val", "base_gap", "base_hits", "gap_closed",
    "status_change"
))
