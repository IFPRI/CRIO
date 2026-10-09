#' Make all RIO figures
#'
#' Runs every figure type for a set of indicators and writes them to
#' `plotdir`. Each figure type and each indicator can be switched on or off,
#' and a failing figure is reported and skipped instead of stopping the run.
#'
#' Figure types (`plots`):
#' \describe{
#'   \item{`maps`}{4-panel indicator maps (climate ensemble). Needs `ctymap`.}
#'   \item{`pct_hits`}{Bar chart of percent of climate scenarios hitting the target.}
#'   \item{`rules`}{Heatmap of decision-tree rules across regions.}
#'   \item{`trajectories`}{Trajectories for `regions` (all B4T runs).}
#'   \item{`trajectories_countries`}{Trajectories for `countries` (own-region B4T).}
#'   \item{`quantities`}{Nutrient quantity trajectories (RNI and target lines),
#'     for `regions` and, if given, `countries`.}
#'   \item{`overview`}{Distance-to-target overviews (main indicators and
#'     nutrients), for `regions` and, if given, `countries`.}
#'   \item{`b4t_heatmap`}{Region x B4T heatmaps (absolute and percent change).}
#'   \item{`b4t_gap`}{Gap closed by B4T (bars) and gap maps for `b4t_map_scenario`.}
#' }
#'
#' @param fulldata Output of [load_crio_data()].
#' @param plotdir Output folder (created if needed).
#' @param plots Figure types to make (see Details). Default: all.
#' @param indicators Indicator names from [indicator_specs()] to include
#'   (e.g. `c("HNGR", "FRTVEG")`). `NULL` = all.
#' @param nutrients Include the nutrient adequacy indicators when
#'   `indicators` is `NULL`.
#' @param regions Region codes for regional figures.
#' @param countries Country codes for country figures. `NULL` skips them.
#' @param ctymap Region map from [load_region_map()]; needed for maps.
#' @param specs Indicator specs. Default `indicator_specs(fulldata, nut_threshold, water_threshold)`.
#' @param nut_threshold,water_threshold Passed to [indicator_specs()] and
#'   [nutrient_quantity_specs()].
#' @param keep_above `keep_above` for the nutrient overviews (see [plot_gap_overview()]).
#' @param b4t_map_scenario B4T run mapped in the `b4t_gap` maps.
#' @param b4t_show_regions,b4t_show_countries B4T lines in trajectory figures for
#'   `regions` and `countries`: `"all"` = all 7 B4T runs; `"own"` = B4T in the
#'   panel's own CG region + all CG regions. Defaults: `"all"` for regions,
#'   `"own"` for countries.
#' @param baseline_id Baseline scenario id.
#' @param target_year Year evaluated for maps, pathway analysis and B4T comparisons.
#' @param subdirs Write each figure type to its own subfolder of `plotdir`.
#' @param format File extension for figures (`"png"`, `"pdf"`, `"svg"`, ...).
#'
#' @return Invisibly, a list with `analyses` (pathway analyses by indicator),
#'   `b4t` (all B4T comparisons) and `failed` (figures that errored, with
#'   the message).
#' @export
crio_plot_all <- function(fulldata,
                          plotdir,
                          plots = c("maps", "pct_hits", "rules", "trajectories",
                                    "trajectories_countries", "quantities", "overview",
                                    "b4t_heatmap", "b4t_gap"),
                          indicators = NULL,
                          nutrients = TRUE,
                          regions = c(b4t_regions, "WLD"),
                          countries = NULL,
                          ctymap = NULL,
                          specs = NULL,
                          nut_threshold = 1.5,
                          water_threshold = 1,
                          keep_above = -0.1,
                          b4t_map_scenario = "All CG regions",
                          b4t_show_regions = c("all", "own"),
                          b4t_show_countries = c("own", "all"),
                          baseline_id = crio_baseline_id,
                          target_year = 2050,
                          subdirs = TRUE,
                          format = "png") {
    b4t_show_regions   <- match.arg(b4t_show_regions)
    b4t_show_countries <- match.arg(b4t_show_countries)
    specs <- specs %||% indicator_specs(fulldata, nut_threshold, water_threshold)
    main  <- attr(specs, "main") %||% names(specs)

    if (is.null(indicators)) {
        indicators <- if (nutrients) names(specs) else main
    }
    unknown <- setdiff(indicators, names(specs))
    if (length(unknown) > 0) stop("Unknown indicators: ", paste(unknown, collapse = ", "))
    specs <- specs[indicators]

    if (any(c("maps", "b4t_gap") %in% plots) && is.null(ctymap)) {
        warning("`ctymap` not supplied: skipping maps")
        plots <- setdiff(plots, "maps")
    }

    failed <- list()

    # Run one figure; record and skip failures
    attempt <- function(label, expr) {
        message("  ", label)
        tryCatch(expr, error = function(e) {
            failed[[label]] <<- conditionMessage(e)
            message("    FAILED: ", conditionMessage(e))
        })
    }

    dir_for <- function(type) {
        d <- if (subdirs) file.path(plotdir, type) else plotdir
        dir.create(d, showWarnings = FALSE, recursive = TRUE)
        d
    }

    file_for <- function(k, suffix = "") paste0(k, suffix, ".", format)

    # Pathway analysis (climate ensemble) ----
    analyses <- list()
    if (any(c("maps", "pct_hits", "rules") %in% plots)) {
        message("Pathway analysis")
        for (k in names(specs)) {
            s <- specs[[k]]
            analyses[[k]] <- attempt(paste("pathways", k), build_pathway_analysis(
                df = filter_climate(s$df), value_col = s$value_col,
                threshold = s$threshold, threshold_col = s$threshold_col,
                direction = s$direction, target_year = target_year))
        }
    }

    # Maps ----
    if ("maps" %in% plots) {
        message("Maps")
        d <- dir_for("maps")
        for (k in names(analyses)) {
            s <- specs[[k]]
            attempt(paste("map", k), plot_indicator_maps(
                df = filter_climate(s$df), summary_tbl = analyses[[k]]$summary_tbl,
                ctymap = ctymap, value_col = s$value_col, baseline_id = baseline_id,
                threshold = s$threshold, threshold_col = s$threshold_col,
                direction = s$direction, indicator_label = s$label,
                value_labeller = s$labeller, target_year = target_year,
                save = TRUE, outfile = file_for(k), plotdir = d))
        }
    }

    # Pathway figures ----
    if ("pct_hits" %in% plots) {
        message("Percent of scenarios hitting target")
        d <- dir_for("pct_hits")
        for (k in names(analyses)) {
            attempt(paste("pct_hits", k), ggsave(
                filename = file.path(d, file_for(k, "_pct_hits")),
                plot = plot_pct_hits(analyses[[k]], title = paste(specs[[k]]$label, "- % scenarios hitting target")),
                width = 7, height = 8, bg = "white"))
        }
    }

    if ("rules" %in% plots) {
        message("Pathway rules")
        d <- dir_for("rules")
        for (k in names(analyses)) {
            if (nrow(analyses[[k]]$rules_tbl) == 0) next
            attempt(paste("rules", k), plot_rules_heatmap(
                analyses[[k]], title = paste(specs[[k]]$label, "- pathways by region"),
                save = TRUE, outfile = file_for(k, "_rules"), plotdir = d,
                width = 12, height = 9))
        }
    }

    # Trajectories ----
    if ("trajectories" %in% plots) {
        message("Trajectories (regions)")
        d <- dir_for("trajectories")
        for (k in names(specs)) {
            attempt(paste("trajectories", k), plot_spec_trajectories(
                specs[[k]], regions = regions, b4t_show = b4t_show_regions, baseline_id = baseline_id,
                save = TRUE, outfile = file_for(k, "_trajectories"), plotdir = d))
        }
    }

    if ("trajectories_countries" %in% plots && !is.null(countries)) {
        message("Trajectories (countries)")
        d <- dir_for("trajectories_countries")
        for (k in names(specs)) {
            attempt(paste("trajectories_countries", k), plot_spec_trajectories(
                specs[[k]], regions = countries, b4t_show = b4t_show_countries, baseline_id = baseline_id,
                save = TRUE, outfile = file_for(k, "_trajectories_countries"), plotdir = d))
        }
    }

    # Nutrient quantities ----
    nut_selected <- intersect(indicators, setdiff(names(specs), main))
    if ("quantities" %in% plots && length(nut_selected) > 0) {
        message("Nutrient quantity trajectories")
        d <- dir_for("quantities")
        qty <- nutrient_quantity_specs(fulldata, nut_threshold)
        qty <- qty[intersect(names(qty), sub("_adequacy$", "", nut_selected))]
        for (nut in names(qty)) {
            attempt(paste("quantities", nut), plot_spec_trajectories(
                qty[[nut]], regions = regions, b4t_show = b4t_show_regions, baseline_id = baseline_id,
                save = TRUE, outfile = file_for(nut, "_quantity_trajectories"), plotdir = d))
            if (!is.null(countries)) {
                attempt(paste("quantities_countries", nut), plot_spec_trajectories(
                    qty[[nut]], regions = countries, b4t_show = b4t_show_countries, baseline_id = baseline_id,
                    save = TRUE, outfile = file_for(nut, "_quantity_trajectories_countries"), plotdir = d))
            }
        }
    }

    # Distance-to-target overviews ----
    if ("overview" %in% plots) {
        message("Distance-to-target overviews")
        d <- dir_for("overview")
        groups <- list(main = intersect(indicators, main), nutrients = nut_selected)
        sets <- list(regions = regions)
        if (!is.null(countries)) sets$countries <- countries

        for (g in names(groups)) {
            if (length(groups[[g]]) == 0) next
            for (st in names(sets)) {
                attempt(paste("overview", g, st), plot_gap_overview(
                    specs[groups[[g]]], regions = sets[[st]],
                    keep_above = if (g == "nutrients") keep_above else NULL,
                    target_year = target_year, baseline_id = baseline_id,
                    title = paste0("Distance to target - ",
                                   if (g == "main") "diet, hunger and water indicators"
                                   else paste0("nutrient availability ratios (target = ", nut_threshold, "x RNI)"),
                                   if (st == "countries") " (countries)" else ""),
                    save = TRUE, outfile = file_for(paste0("OVERVIEW_gap_", g),
                                                    if (st == "countries") "_countries" else ""),
                    plotdir = d))
            }
        }
    }

    # B4T vs baseline ----
    b4t <- NULL
    if (any(c("b4t_heatmap", "b4t_gap") %in% plots)) {
        message("B4T comparisons")
        b4t <- attempt("b4t comparisons", build_b4t_comparisons(
            specs, target_year = target_year))
    }

    if ("b4t_heatmap" %in% plots && is.data.frame(b4t)) {
        message("B4T heatmaps")
        d <- dir_for("b4t_heatmap")
        for (k in names(specs)) {
            s <- specs[[k]]
            attempt(paste("b4t_heatmap", k), plot_b4t_heatmap(
                cmp = b4t |> filter(indicator == k), regions = regions,
                direction = s$direction, delta_labeller = s$delta_labeller,
                baseline_id = baseline_id,
                title = paste0(s$label, " - B4T change vs baseline (", target_year, ")"),
                save = TRUE, outfile = file_for(k, "_B4T_heatmap"), plotdir = d))
        }
    }

    if ("b4t_gap" %in% plots && is.data.frame(b4t)) {
        message("B4T gap closure")
        d <- dir_for("b4t_gap")
        for (k in names(specs)) {
            s <- specs[[k]]
            cmp <- b4t |> filter(indicator == k)
            attempt(paste("b4t_gap_closed", k), plot_b4t_gap_closed(
                cmp, regions = regions, indicator_label = s$label, baseline_id = baseline_id,
                save = TRUE, outfile = file_for(k, "_B4T_gap_closed"), plotdir = d))
            if (!is.null(ctymap)) {
                attempt(paste("b4t_gap_maps", k), plot_b4t_gap_maps(
                    cmp, ctymap = ctymap, b4t_scenario = b4t_map_scenario, baseline_id = baseline_id,
                    direction = s$direction, gap_labeller = s$labeller,
                    delta_labeller = s$delta_labeller, indicator_label = s$label,
                    target_year = target_year,
                    save = TRUE, outfile = file_for(k, "_B4T_gap_maps"), plotdir = d))
            }
        }
    }

    if (length(failed) > 0) {
        message(length(failed), " figure(s) failed; see the `failed` element of the result.")
    } else {
        message("Done.")
    }

    invisible(list(analyses = analyses, b4t = b4t, failed = failed))
}
