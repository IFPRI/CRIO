#' Load the IMPACT region shapefile
#'
#' @param shp Path to the IMPACT regions shapefile (with a `NEW_REGION` column).
#'
#' @return A SpatVector.
#' @export
load_region_map <- function(shp) {
    terra::vect(shp)
}

#' World country outline for map borders
#'
#' Built from the `maps` package, without Antarctica, dissolved into one
#' outline. Cached after the first call.
#'
#' @return A SpatVector.
#' @export
world_outline <- function() {
    if (is.null(.crio_env$world)) {
        world_sf <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))
        world <- terra::vect(world_sf)
        world <- world[world$ID != "Antarctica", ]
        .crio_env$world <- terra::aggregate(world)
    }
    .crio_env$world
}

.map_theme <- function() {
    theme_void() +
        theme(
            legend.position = "bottom",
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 7),
            legend.key.width = unit(1, "cm"),
            legend.key.height = unit(0.3, "cm"),
            plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
            plot.margin = margin(2, 2, 2, 2)
        )
}

#' Four-panel indicator maps
#'
#' (A) baseline value in `baseline_year`; (B) mean across scenarios in
#' `target_year`; (C) baseline distance from target in `target_year`;
#' (D) percent of scenarios hitting the target. Rest-of-world countries
#' ([row_countries]) are greyed out.
#'
#' With a fixed `threshold` the value scales diverge at the target; with a
#' dynamic `threshold_col` they use a simple gradient.
#'
#' @param df Indicator data (normally the climate ensemble, [filter_climate()]).
#' @param summary_tbl `summary_tbl` from [build_pathway_analysis()].
#' @param ctymap Region map from [load_region_map()].
#' @param value_col Column with the indicator value.
#' @param baseline_id Baseline scenario id.
#' @param threshold,threshold_col Fixed or dynamic target (see [build_pathway_analysis()]).
#' @param direction `"below"` or `"above"` the target is good.
#' @param indicator_label Label for titles and legends.
#' @param value_labeller Label function for values.
#' @param target_year,baseline_year Years for panels.
#' @param save,outfile,plotdir,width,height Saving options.
#'
#' @return A ggarrange object.
#' @export
plot_indicator_maps <- function(df,
                                summary_tbl,
                                ctymap,
                                value_col,
                                baseline_id = crio_baseline_id,
                                threshold = NULL,
                                threshold_col = NULL,
                                direction = c("below", "above"),
                                indicator_label,
                                value_labeller = scales::percent_format(accuracy = 1),
                                target_year = 2050,
                                baseline_year = 2025,
                                save = FALSE,
                                outfile = NULL,
                                plotdir = ".",
                                width = 10,
                                height = 6) {
    direction <- match.arg(direction)
    .check_threshold(threshold, threshold_col)

    lo <- if (direction == "below") .col_good else .col_bad
    hi <- if (direction == "below") .col_bad else .col_good

    scale_ac <- function(name, labeller) {
        if (is.null(threshold_col)) {
            scale_fill_gradient2(low = lo, mid = "white", high = hi, midpoint = threshold,
                                 name = name, labels = labeller, na.value = "grey95")
        } else {
            # No single midpoint for a dynamic target
            scale_fill_gradient(low = lo, high = hi,
                                name = name, labels = labeller, na.value = "grey95")
        }
    }

    panel_a_data <- df |>
        filter(id == baseline_id, yrs == baseline_year) |>
        select(region, val = all_of(value_col))

    panel_b_data <- df |>
        filter(yrs == target_year) |>
        group_by(region) |>
        summarise(mean_val = mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")

    panel_c_data <- df |>
        filter(id == baseline_id, yrs == target_year) |>
        mutate(
            val = .data[[value_col]],
            thr = if (is.null(threshold_col)) threshold else .data[[threshold_col]],
            delta = if (direction == "below") {
                if_else(val > thr, val - thr, NA_real_)
            } else {
                if_else(val < thr, thr - val, NA_real_)
            }
        ) |>
        select(region, delta)

    panel_d_data <- summary_tbl |>
        filter(pct_hits < 100) |>
        select(region, pct_hits)

    world <- world_outline()
    ctymap_row <- ctymap |> filter(NEW_REGION %in% row_countries)
    ctymap_sub <- ctymap |> filter(!NEW_REGION %in% row_countries)

    ctymap_a <- ctymap_sub |> left_join(panel_a_data, by = c("NEW_REGION" = "region"))
    ctymap_b <- ctymap_sub |> left_join(panel_b_data, by = c("NEW_REGION" = "region"))
    ctymap_c <- ctymap_sub |> left_join(panel_c_data, by = c("NEW_REGION" = "region"))
    ctymap_d <- ctymap_sub |> left_join(panel_d_data, by = c("NEW_REGION" = "region"))

    base_layers <- function(data, fill_var) {
        list(
            geom_spatvector(data = ctymap_row, fill = "grey95", color = NA, linewidth = 0.05),
            geom_spatvector(data = data, aes(fill = .data[[fill_var]]), color = NA, linewidth = 0.05),
            geom_spatvector(data = world, fill = NA, color = "gray40", linewidth = 0.05)
        )
    }

    p_a <- ggplot() +
        base_layers(ctymap_a, "val") +
        scale_ac(indicator_label, value_labeller) +
        labs(title = paste0("(A) ", indicator_label, " (", baseline_year, ", baseline)")) +
        .map_theme()

    p_b <- ggplot() +
        base_layers(ctymap_b, "mean_val") +
        scale_ac(paste("mean", indicator_label), value_labeller) +
        labs(title = paste0("(B) Mean ", indicator_label, " across scenarios (", target_year, ")")) +
        .map_theme()

    p_c <- ggplot() +
        base_layers(ctymap_c, "delta") +
        scale_fill_gradient(low = .col_good, high = .col_bad,
                            name = "distance from target",
                            labels = value_labeller, na.value = "grey95") +
        labs(title = paste0("(C) Distance from target (", target_year, ", baseline)")) +
        .map_theme()

    p_d <- ggplot() +
        base_layers(ctymap_d, "pct_hits") +
        scale_fill_gradient(low = .col_bad, high = .col_good,
                            name = "% of scenarios", na.value = "grey95") +
        labs(title = paste0("(D) Scenarios hitting target (", target_year, ")")) +
        .map_theme()

    p <- ggpubr::ggarrange(p_a, p_b, p_c, p_d, ncol = 2, nrow = 2)

    .save_plot(p, save, outfile, plotdir, width, height)
}
