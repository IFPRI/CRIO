# RIO risk analysis with CRIO
# Replaces v3_risk_analysis_RIO.R. Edit the paths below, then run top to bottom.

library(CRIO)
library(dplyr)

# Paths ----
scendir       <- "C:/EPTD/Modeling/2026 Entropy Fix/f_wasteFix/OutputFiles/Scenarios/RIO/"
nutrients_gdx <- "C:/EPTD/Modeling/Hunger/InputFiles/Nutrients_per_kg.gdx"
faostat_csv   <- "C:/Local/Work/IFPRI/2026 Research Investment Opportunities (RIO) Report/datasets/FAOSTAT Food Security/Food_Security_Data_E_All_Data_(Normalized).csv"
sets_xlsx     <- "C:/EPTD/Modeling/EntropyCode/Entropy_with_IMPACT3_Data/InputFiles/Sets.xlsx"
cty_shp       <- "C:/EPTD/Modeling/impactmodeldrivers_Git/DriverAssumptions/Shapefiles/IMPACT-Regions/IMPACT_Regions_159.shp"

plotdir <- "C:/Local/Work/IFPRI/2026 Research Investment Opportunities (RIO) Report/plots/v3/"
rdsdir  <- "C:/Local/Work/IFPRI/2026 Research Investment Opportunities (RIO) Report/RDS/v3/"
csvdir  <- "C:/Users/AMishra/OneDrive - CGIAR/2026 RIO Report/Datasets/v3/"

dir.create(plotdir, showWarnings = FALSE, recursive = TRUE)

# Countries for country-level figures (B4T lines = own CG region + all CG regions)
focus_countries <- c("NGA", "ETH", "KEN", "IND", "BGD", "PAK", "CIV", "BRA")

# Data ----
files    <- list_scenarios(scendir)
fulldata <- load_crio_data(files, nutrients_gdx, faostat_csv, sets_xlsx)

save_crio_data(fulldata, rdsdir = rdsdir, csvdir = csvdir, suffix = "-[v3]")

ctymap <- load_region_map(cty_shp)
specs  <- indicator_specs(fulldata)
main   <- attr(specs, "main")

# Pathway analysis and maps (climate ensemble) ----

analyses <- list()

for (k in names(specs)) {
    message(k)
    s <- specs[[k]]
    clim_df <- filter_climate(s$df)

    analyses[[k]] <- build_pathway_analysis(
        df = clim_df,
        value_col = s$value_col,
        threshold = s$threshold,
        threshold_col = s$threshold_col,
        direction = s$direction)

    plot_indicator_maps(
        df = clim_df,
        summary_tbl = analyses[[k]]$summary_tbl,
        ctymap = ctymap,
        value_col = s$value_col,
        threshold = s$threshold,
        threshold_col = s$threshold_col,
        direction = s$direction,
        indicator_label = s$label,
        value_labeller = s$labeller,
        outfile = paste0(k, ".png"),
        save = TRUE,
        plotdir = plotdir)
}

## Calories against each country's minimum requirement (MDER) ----

analyses[["CALORIES-MDER"]] <- build_pathway_analysis(
    df = filter_climate(fulldata$MDER),
    value_col = "value",
    threshold_col = "minKCAL",
    direction = "above")

plot_indicator_maps(
    df = filter_climate(fulldata$MDER),
    summary_tbl = analyses[["CALORIES-MDER"]]$summary_tbl,
    ctymap = ctymap,
    value_col = "value",
    threshold_col = "minKCAL",
    direction = "above",
    indicator_label = "Total calories (kcal/capita/day)",
    value_labeller = scales::label_number(big.mark = ","),
    outfile = "CALORIES.png",
    save = TRUE,
    plotdir = plotdir)

## Quick looks ----
plot_pct_hits(analyses$HNGR)
plot_rules_heatmap(analyses$HNGR)

# Target trajectories ----

for (k in names(specs)) {
    plot_spec_trajectories(specs[[k]],
                           outfile = paste0(k, "_trajectories.png"),
                           save = TRUE, plotdir = plotdir)

    plot_spec_trajectories(specs[[k]],
                           regions = focus_countries, b4t_show = "own",
                           outfile = paste0(k, "_trajectories_countries.png"),
                           save = TRUE, plotdir = plotdir)
}

## Nutrient quantities (per capita per day; dotted line = RNI) ----

qty_specs <- nutrient_quantity_specs(fulldata)

for (nut in names(qty_specs)) {
    plot_spec_trajectories(qty_specs[[nut]],
                           outfile = paste0(nut, "_quantity_trajectories.png"),
                           save = TRUE, plotdir = plotdir)

    plot_spec_trajectories(qty_specs[[nut]],
                           regions = focus_countries, b4t_show = "own",
                           outfile = paste0(nut, "_quantity_trajectories_countries.png"),
                           save = TRUE, plotdir = plotdir)
}

# Distance-to-target overviews ----

nutrients <- setdiff(names(specs), main)

plot_gap_overview(specs[main],
                  title = "Distance to target - diet, hunger and water indicators",
                  outfile = "OVERVIEW_gap_main.png", save = TRUE, plotdir = plotdir)

plot_gap_overview(specs[nutrients], keep_above = -0.1,
                  title = "Distance to target - nutrient availability ratios (target = 1.5x RNI)",
                  outfile = "OVERVIEW_gap_nutrients.png", save = TRUE, plotdir = plotdir)

plot_gap_overview(specs[main], regions = focus_countries,
                  title = "Distance to target - diet, hunger and water indicators (countries)",
                  outfile = "OVERVIEW_gap_main_countries.png", save = TRUE, plotdir = plotdir)

plot_gap_overview(specs[nutrients], regions = focus_countries, keep_above = -0.1,
                  title = "Distance to target - nutrient availability ratios (target = 1.5x RNI, countries)",
                  outfile = "OVERVIEW_gap_nutrients_countries.png", save = TRUE, plotdir = plotdir)

# Breeding For Tomorrow (B4T) vs baseline ----

b4t_all <- build_b4t_comparisons(specs)

for (k in names(specs)) {
    s <- specs[[k]]
    plot_b4t_heatmap(
        cmp = b4t_all |> filter(indicator == k),
        direction = s$direction,
        delta_labeller = s$delta_labeller,
        title = paste(s$label, "- B4T change vs baseline (2050)"),
        outfile = paste0(k, "_B4T_heatmap.png"),
        save = TRUE,
        plotdir = plotdir)
}

## Status changes and dump ----

b4t_all |>
    filter(status_change != "No change") |>
    count(indicator, b4t, status_change)

saveRDS(object = b4t_all, file = file.path(rdsdir, "B4T.rds"))
data.table::fwrite(x = b4t_all, file = file.path(csvdir, "B4T_vs_baseline-[v3].csv"))
