#' Indicator trajectories against a target
#'
#' Line figure over time, one panel per region. Bands show the range across
#' GCMs for each SSP (climate ensemble); the black line is the baseline;
#' coloured lines are B4T runs (all under the baseline SSP/GCM); the dashed
#' line is the target.
#'
#' Note: if all GCMs give the same value (e.g. hunger at the model's floor),
#' a band has zero width and is not visible.
#'
#' @param df Indicator data with scenario columns (climate + B4T ensembles).
#' @param value_col Column with the indicator value.
#' @param threshold,threshold_col Fixed or dynamic target. A dynamic target
#'   follows the baseline's values over time.
#' @param regions Region codes: CG regions and/or countries (e.g. `c("NGA", "IND")`).
#' @param b4t_show `"all"`: all 7 B4T runs; `"own"`: B4T in the panel's own CG
#'   region + B4T in all CG regions.
#' @param ref_value Optional extra horizontal reference line (dotted), e.g.
#'   the RNI when the target is a multiple of it.
#' @param ref_label Name of the reference line in the subtitle.
#' @param baseline_id Baseline scenario id.
#' @param mapping GFSir mapping file (for country -> CG region).
#' @param indicator_label Y axis label.
#' @param value_labeller Label function for the y axis.
#' @param title Plot title.
#' @param save,outfile,plotdir,width,height Saving options.
#'
#' @return A ggplot.
#' @export
plot_target_trajectories <- function(df,
                                     value_col,
                                     threshold = NULL,
                                     threshold_col = NULL,
                                     regions = c(b4t_regions, "WLD"),
                                     b4t_show = c("all", "own"),
                                     ref_value = NULL,
                                     ref_label = "reference",
                                     baseline_id = crio_baseline_id,
                                     mapping = "mappingCG6.xlsx",
                                     indicator_label,
                                     value_labeller = scales::label_number(),
                                     title = NULL,
                                     save = FALSE,
                                     outfile = NULL,
                                     plotdir = ".",
                                     width = 10,
                                     height = 7) {
    b4t_show <- match.arg(b4t_show)
    .check_threshold(threshold, threshold_col)
    title <- title %||% paste(indicator_label, "- trajectories vs target")

    d <- df |>
        filter(region %in% regions) |>
        mutate(
            year = as.numeric(as.character(yrs)),
            val = .data[[value_col]],
            thr = if (is.null(threshold_col)) threshold else .data[[threshold_col]],
            region = factor(region, levels = regions)
        )

    band <- d |>
        filter(ensemble == "Climate") |>
        group_by(region, year, pathway) |>
        summarise(lo = min(val, na.rm = TRUE), hi = max(val, na.rm = TRUE), .groups = "drop")

    base <- d |> filter(id == baseline_id)

    b4t_runs <- d |> filter(ensemble == "B4T")

    if (b4t_show == "own") {
        b4t_runs <- b4t_runs |>
            filter(b4t == "All CG regions" | b4t == own_cg_region(region, mapping)) |>
            mutate(b4t = if_else(b4t == "All CG regions", "All CG regions", "Own CG region"))
        b4t_cols <- c("Own CG region" = "#D95F02", "All CG regions" = "#7570B3")
    } else {
        b4t_cols <- stats::setNames(RColorBrewer::brewer.pal(length(b4t_levels), "Dark2"), b4t_levels)
    }
    b4t_runs <- b4t_runs |> mutate(b4t = factor(b4t, levels = names(b4t_cols)))

    p <- ggplot() +
        geom_ribbon(data = band, aes(x = year, ymin = lo, ymax = hi, fill = pathway), alpha = 0.3) +
        geom_line(data = base, aes(x = year, y = thr), linetype = "dashed", colour = "grey30") +
        { if (!is.null(ref_value)) geom_hline(yintercept = ref_value, linetype = "dotted", colour = "grey30") } +
        geom_line(data = base, aes(x = year, y = val), colour = "black", linewidth = 1.2) +
        geom_line(data = b4t_runs, aes(x = year, y = val, colour = b4t), linewidth = 0.6) +
        scale_fill_manual(values = .pathway_cols, name = "Range across GCMs") +
        scale_colour_manual(values = b4t_cols, name = paste0("B4T implemented in (", baseline_id, ")")) +
        scale_y_continuous(labels = value_labeller) +
        facet_wrap(~ region, scales = "free_y", labeller = as_labeller(.region_labels(regions, mapping))) +
        labs(title = title,
             subtitle = paste0("Black: baseline (", baseline_id, "). Dashed: target.",
                               if (!is.null(ref_value)) paste0(" Dotted: ", ref_label, ".")),
             x = NULL, y = indicator_label) +
        theme_minimal() +
        theme(legend.position = "bottom", legend.box = "vertical")

    .save_plot(p, save, outfile, plotdir, width, height)
}

#' Trajectories for one indicator spec
#'
#' Convenience wrapper: passes a spec from [indicator_specs()] or
#' [nutrient_quantity_specs()] to [plot_target_trajectories()].
#'
#' @param spec One spec entry.
#' @param ... Further arguments to [plot_target_trajectories()] (e.g.
#'   `regions`, `b4t_show`, `save`, `outfile`, `plotdir`).
#'
#' @return A ggplot.
#' @export
plot_spec_trajectories <- function(spec, ...) {
    plot_target_trajectories(
        df = spec$df,
        value_col = spec$value_col,
        threshold = spec$threshold,
        threshold_col = spec$threshold_col,
        ref_value = spec$ref_value,
        ref_label = spec$ref_label %||% "reference",
        indicator_label = spec$label,
        value_labeller = spec$labeller %||% scales::label_number(),
        title = spec$title,
        ...)
}

#' Distance-to-target overview across indicators
#'
#' Puts all indicators on one scale: gap = (value - target) / target for
#' "below" targets and (target - value) / target for "above" targets, so a
#' positive gap is short of target and zero or negative is target met. Lines are the baseline;
#' markers are B4T in the panel's own CG region (open circle) and in all CG
#' regions (cross). WLD and countries outside CG regions have no own-region run.
#'
#' @param specs Indicator specs (see [indicator_specs()]).
#' @param regions Region codes: CG regions and/or countries.
#' @param keep_above If set, keep only region x indicator lines whose baseline
#'   gap in `target_year` is above this value (e.g. -0.1 = short of target or
#'   within 10 percent of it). Drops indicators far above target that squash the axis.
#' @param target_year Year used by `keep_above`.
#' @param baseline_id Baseline scenario id.
#' @param mapping GFSir mapping file (for country -> CG region).
#' @param title Plot title.
#' @param save,outfile,plotdir,width,height Saving options.
#'
#' @return A ggplot.
#' @export
plot_gap_overview <- function(specs,
                              regions = c(b4t_regions, "WLD"),
                              keep_above = NULL,
                              target_year = 2050,
                              baseline_id = crio_baseline_id,
                              mapping = "mappingCG6.xlsx",
                              title = "Distance to target",
                              save = FALSE,
                              outfile = NULL,
                              plotdir = ".",
                              width = 12,
                              height = 8) {
    base_name <- paste0("Baseline (", baseline_id, ")")

    d <- lapply(names(specs), function(k) {
        s <- specs[[k]]
        s$df |>
            filter(region %in% regions,
                   id == baseline_id | b4t == "All CG regions" | b4t == own_cg_region(region, mapping)) |>
            mutate(
                year = as.numeric(as.character(yrs)),
                val = .data[[s$value_col]],
                thr = if (is.null(s$threshold_col)) s$threshold else .data[[s$threshold_col]],
                gap = if (s$direction == "below") (val - thr) / thr else (thr - val) / thr,
                indicator = trimws(gsub("availability", "", s$short_label %||% s$label)),
                scenario = case_when(
                    id == baseline_id ~ base_name,
                    b4t == "All CG regions" ~ "B4T in all CG regions",
                    .default = "B4T in own region"
                )
            ) |>
            select(region, year, indicator, scenario, gap)
    }) |>
        bind_rows() |>
        filter(!is.na(gap)) |>
        mutate(region = factor(region, levels = regions))

    subtitle <- "Above 0 = short of target (as % of target). Shaded = target met."

    if (!is.null(keep_above)) {
        keep <- d |>
            filter(scenario == base_name, year == target_year, gap > keep_above) |>
            distinct(region, indicator)
        d <- d |> semi_join(keep, by = c("region", "indicator"))
        subtitle <- paste0(subtitle, " Showing indicators short of target or within ",
                           scales::percent(-keep_above), " of it in ", target_year, " (baseline).")
    }

    # B4T runs sit very close to the baseline, so show them as markers on top of
    # the baseline line rather than as overlapping line types
    d_base <- d |> filter(scenario == base_name)
    d_b4t  <- d |> filter(scenario != base_name)

    subtitle <- paste("Lines = baseline; markers = B4T runs.", subtitle)

    p <- ggplot(mapping = aes(x = year, y = gap, colour = indicator)) +
        annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0, fill = .col_good, alpha = 0.08) +
        geom_hline(yintercept = 0, colour = "grey30") +
        geom_line(data = d_base, linewidth = 0.6) +
        geom_point(data = d_b4t, aes(shape = scenario), size = 1.8, stroke = 0.7) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_shape_manual(values = c("B4T in own region" = 1,       # open circle
                                      "B4T in all CG regions" = 4),  # cross
                           breaks = c("B4T in own region", "B4T in all CG regions"),
                           name = NULL) +
        facet_wrap(~ region, scales = "free_y", labeller = as_labeller(.region_labels(regions, mapping))) +
        labs(title = title,
             subtitle = stringr::str_wrap(subtitle, 140),
             x = NULL, y = "distance to target", colour = NULL) +
        theme_minimal() +
        theme(legend.position = "bottom", legend.box = "vertical")

    .save_plot(p, save, outfile, plotdir, width, height)
}
