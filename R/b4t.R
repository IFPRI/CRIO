#' Compare B4T scenarios with the baseline
#'
#' Each B4T run against the baseline in `target_year`.
#' `gap` = distance from target (0 if met); `gap_closed` = share of the
#' baseline gap closed by B4T (NA if the baseline already meets the target).
#'
#' @param df Indicator data with scenario columns (baseline + B4T runs).
#' @param value_col Column with the indicator value.
#' @param threshold,threshold_col Fixed or dynamic target.
#' @param direction `"below"` or `"above"` the target is good.
#' @param target_year Year compared.
#' @param baseline_id Baseline scenario id.
#'
#' @return One row per region x B4T run with baseline and B4T values, absolute
#'   and percent change, gaps, gap closed and target status change.
#' @export
build_b4t_comparison <- function(df,
                                 value_col,
                                 threshold = NULL,
                                 threshold_col = NULL,
                                 direction = c("below", "above"),
                                 target_year = 2050,
                                 baseline_id = crio_baseline_id) {
    direction <- match.arg(direction)
    .check_threshold(threshold, threshold_col)

    df_year <- df |>
        filter(yrs == target_year, id == baseline_id | ensemble == "B4T") |>
        mutate(
            val = .data[[value_col]],
            thr = if (is.null(threshold_col)) threshold else .data[[threshold_col]],
            gap = if (direction == "below") pmax(val - thr, 0) else pmax(thr - val, 0),
            hits_target = as.integer(gap == 0)
        )

    base <- df_year |>
        filter(id == baseline_id) |>
        select(region, base_val = val, base_gap = gap, base_hits = hits_target)

    df_year |>
        filter(ensemble == "B4T") |>
        left_join(base, by = "region") |>
        mutate(
            b4t = factor(b4t, levels = b4t_levels),
            delta = val - base_val,
            pct_change = if_else(base_val != 0, delta / base_val, NA_real_),
            gap_closed = if_else(base_gap > 0, 1 - gap / base_gap, NA_real_),
            status_change = case_when(
                base_hits == 0 & hits_target == 1 ~ "Reaches target",
                base_hits == 1 & hits_target == 0 ~ "Loses target",
                .default = "No change"
            )
        ) |>
        select(region, id, b4t, thr, base_val, val, delta, pct_change,
               base_gap, gap, gap_closed, base_hits, hits_target, status_change)
}

#' B4T comparisons for a list of indicator specs
#'
#' @param specs Indicator specs (see [indicator_specs()]).
#' @param ... Further arguments to [build_b4t_comparison()].
#'
#' @return All comparisons stacked, with an `indicator` column.
#' @export
build_b4t_comparisons <- function(specs, ...) {
    lapply(specs, function(s) {
        build_b4t_comparison(df = s$df, value_col = s$value_col,
                             threshold = s$threshold, threshold_col = s$threshold_col,
                             direction = s$direction, ...)
    }) |>
        bind_rows(.id = "indicator")
}

#' Heatmap of B4T effects: region x where B4T is implemented
#'
#' Fill = percent change vs baseline; tile text = absolute change (top) and
#' percent change (bottom). Own-region effects sit on the diagonal.
#'
#' @param cmp Output of [build_b4t_comparison()].
#' @param regions Region codes (rows).
#' @param direction `"below"` or `"above"` the target is good (sets colours).
#' @param delta_labeller,pct_labeller Label functions for absolute and percent change.
#' @param baseline_id Baseline scenario id (subtitle).
#' @param title Plot title.
#' @param save,outfile,plotdir,width,height Saving options.
#'
#' @return A ggplot.
#' @export
plot_b4t_heatmap <- function(cmp,
                             regions = c(b4t_regions, "WLD"),
                             direction = c("below", "above"),
                             delta_labeller = scales::label_number(accuracy = 0.01, style_positive = "plus"),
                             pct_labeller = scales::percent_format(accuracy = 0.01, style_positive = "plus"),
                             baseline_id = crio_baseline_id,
                             title = NULL,
                             save = FALSE,
                             outfile = NULL,
                             plotdir = ".",
                             width = 8,
                             height = 5.5) {
    direction <- match.arg(direction)

    # For "below" targets a decrease is an improvement
    lo <- if (direction == "below") .col_good else .col_bad
    hi <- if (direction == "below") .col_bad else .col_good

    p <- cmp |>
        filter(region %in% regions) |>
        mutate(region = factor(region, levels = rev(regions)),
               tile_label = paste0(delta_labeller(delta), "\n(", pct_labeller(pct_change), ")")) |>
        ggplot(aes(x = b4t, y = region, fill = pct_change)) +
        geom_tile(color = "white") +
        geom_text(aes(label = tile_label), size = 2.3, lineheight = 0.9) +
        scale_fill_gradient2(low = lo, mid = "white", high = hi, midpoint = 0,
                             name = "% change vs baseline", labels = pct_labeller,
                             na.value = "grey95") +
        labs(x = "B4T implemented in", y = NULL, title = title,
             subtitle = paste0("Change vs baseline (", baseline_id, "): absolute (% change)")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom",
              legend.key.width = unit(2, "cm"))

    .save_plot(p, save, outfile, plotdir, width, height)
}

#' Share of the baseline gap closed by each B4T run
#'
#' Bar chart, one panel per region.
#'
#' @param cmp Output of [build_b4t_comparison()].
#' @param regions Region codes (panels).
#' @param indicator_label Indicator name for the y axis and title.
#' @param title Plot title.
#' @param save,outfile,plotdir,width,height Saving options.
#'
#' @return A ggplot.
#' @export
plot_b4t_gap_closed <- function(cmp,
                                regions = c(b4t_regions, "WLD"),
                                indicator_label = "target",
                                title = NULL,
                                save = FALSE,
                                outfile = NULL,
                                plotdir = ".",
                                width = 10,
                                height = 6) {
    title <- title %||% paste(indicator_label, "- gap closed by B4T (vs baseline)")

    p <- cmp |>
        filter(region %in% regions) |>
        mutate(region = factor(region, levels = regions)) |>
        ggplot(aes(x = b4t, y = gap_closed, fill = b4t == "All CG regions")) +
        geom_col() +
        scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = .col_good), guide = "none") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        facet_wrap(~ region) +
        labs(x = "B4T implemented in", y = paste("% of baseline gap to", indicator_label, "closed"),
             title = title) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    .save_plot(p, save, outfile, plotdir, width, height)
}

#' Maps of the target gap: baseline vs one B4T run
#'
#' (A) baseline gap; (B) gap under the B4T run; (C) change in value vs
#' baseline; (D) share of the baseline gap closed.
#'
#' @param cmp Output of [build_b4t_comparison()].
#' @param ctymap Region map from [load_region_map()].
#' @param b4t_scenario B4T run to map (a CG region or `"All CG regions"`).
#' @param direction `"below"` or `"above"` the target is good (sets colours of C).
#' @param gap_labeller Label function for gaps.
#' @param delta_labeller Label function for the change in value.
#' @param indicator_label Indicator name for titles.
#' @param target_year Year (titles).
#' @param save,outfile,plotdir,width,height Saving options.
#'
#' @return A ggarrange object.
#' @export
plot_b4t_gap_maps <- function(cmp,
                              ctymap,
                              b4t_scenario = "All CG regions",
                              direction = c("below", "above"),
                              gap_labeller = scales::label_number(),
                              delta_labeller = scales::label_number(),
                              indicator_label = "indicator",
                              target_year = 2050,
                              save = FALSE,
                              outfile = NULL,
                              plotdir = ".",
                              width = 10,
                              height = 6) {
    direction <- match.arg(direction)
    lo <- if (direction == "below") .col_good else .col_bad
    hi <- if (direction == "below") .col_bad else .col_good

    sel <- cmp |>
        filter(b4t == b4t_scenario) |>
        mutate(base_gap_pos = if_else(base_gap > 0, base_gap, NA_real_),
               gap_pos = if_else(gap > 0, gap, NA_real_))

    gap_lim <- range(c(sel$base_gap_pos, sel$gap_pos), na.rm = TRUE)

    world <- world_outline()
    m <- ctymap |> left_join(sel, by = c("NEW_REGION" = "region"))

    layers <- function(fill_var) {
        list(
            geom_spatvector(data = m, aes(fill = .data[[fill_var]]), color = "white", linewidth = 0.1),
            geom_spatvector(data = world, fill = NA, color = "gray40", linewidth = 0.05)
        )
    }

    p_a <- ggplot() + layers("base_gap_pos") +
        scale_fill_distiller(palette = "YlOrRd", direction = 1, limits = gap_lim,
                             name = "distance from target", labels = gap_labeller, na.value = "white") +
        labs(title = paste0("(A) Distance from target (", target_year, ", baseline)")) +
        .map_theme()

    p_b <- ggplot() + layers("gap_pos") +
        scale_fill_distiller(palette = "YlOrRd", direction = 1, limits = gap_lim,
                             name = "distance from target", labels = gap_labeller, na.value = "white") +
        labs(title = paste0("(B) Distance from target (", target_year, ", B4T in ", b4t_scenario, ")")) +
        .map_theme()

    p_c <- ggplot() + layers("delta") +
        scale_fill_gradient2(low = lo, mid = "white", high = hi, midpoint = 0,
                             name = paste("change in", indicator_label), labels = delta_labeller,
                             na.value = "grey85") +
        labs(title = paste0("(C) Change in ", indicator_label, " vs baseline (", target_year, ")")) +
        .map_theme()

    p_d <- ggplot() + layers("gap_closed") +
        scale_fill_distiller(palette = "Greens", direction = 1,
                             name = "% of gap closed", labels = scales::percent_format(accuracy = 1),
                             na.value = "grey85") +
        labs(title = paste0("(D) Share of baseline gap closed by B4T (", target_year, ")")) +
        .map_theme()

    p <- ggpubr::ggarrange(p_a, p_b, p_c, p_d, ncol = 2, nrow = 2)

    .save_plot(p, save, outfile, plotdir, width, height)
}
