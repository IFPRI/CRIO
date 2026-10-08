# RIO hunger gap analysis with CRIO
# Replaces v3_gap-RIO.R. Edit the paths below, then run top to bottom.

library(CRIO)
library(dplyr)

# Paths ----
scendir <- "C:/EPTD/Modeling/2026 Entropy Fix/f_wasteFix/OutputFiles/Scenarios/RIO/"
cty_shp <- "C:/EPTD/Modeling/impactmodeldrivers_Git/DriverAssumptions/Shapefiles/IMPACT-Regions/IMPACT_Regions_159.shp"
plotdir <- "C:/Local/Work/IFPRI/2026 Research Investment Opportunities (RIO) Report/plots/v3/"

dir.create(plotdir, showWarnings = FALSE, recursive = TRUE)

pp_labeller <- scales::label_number(scale = 100, accuracy = 0.1, suffix = " pp", style_positive = "plus")

# Data ----
files  <- list_scenarios(scendir)
hngr   <- get_hunger(files)
ctymap <- load_region_map(cty_shp)

# Decision trees (climate ensemble) ----

hunger_analysis <- build_pathway_analysis(
    df = filter_climate(hngr),
    value_col = "share",
    threshold = 0.05,
    direction = "below")

hunger_analysis$summary_tbl |>
    mutate(status = case_when(
        pct_hits == 0   ~ "never",
        pct_hits == 100 ~ "always",
        TRUE            ~ "partial"
    )) |>
    count(status)

hunger_analysis$rules_tbl |> filter(region %in% b4t_regions)

summary_rules_tbl <- summarise_rules(hunger_analysis$rules_tbl)

plot_pct_hits(hunger_analysis, title = "Share of scenarios reaching <5% hunger by 2050")

plot_rules_heatmap(hunger_analysis, base_size = 20,
                   outfile = "pathways.png", save = TRUE, plotdir = plotdir)

# Hunger maps ----
# (A) 2025 baseline, (B) mean across scenarios 2050, (C) distance from 5% 2050, (D) % scenarios hitting

plot_indicator_maps(
    df = filter_climate(hngr),
    summary_tbl = hunger_analysis$summary_tbl,
    ctymap = ctymap,
    value_col = "share",
    threshold = 0.05,
    direction = "below",
    indicator_label = "hunger share",
    value_labeller = scales::percent_format(accuracy = 1),
    outfile = "HNGR_gap.png",
    save = TRUE,
    plotdir = plotdir)

# B4T gap closure ----

b4t_gap <- build_b4t_comparison(
    df = hngr,
    value_col = "share",
    threshold = 0.05,
    direction = "below")

b4t_gap |> filter(status_change == "Reaches target")

plot_b4t_gap_closed(b4t_gap, indicator_label = "5% hunger",
                    outfile = "HNGR_B4T_gap_closed.png", save = TRUE, plotdir = plotdir)

plot_b4t_gap_maps(
    cmp = b4t_gap,
    ctymap = ctymap,
    b4t_scenario = "All CG regions",
    direction = "below",
    gap_labeller = scales::percent_format(accuracy = 1),
    delta_labeller = pp_labeller,
    indicator_label = "hunger share",
    outfile = "HNGR_B4T_gap.png",
    save = TRUE,
    plotdir = plotdir)
