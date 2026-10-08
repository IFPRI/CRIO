#' Pathway analysis: which scenarios hit a target
#'
#' For each region, the share of scenarios hitting the target in
#' `target_year`, and a classification tree (rpart) on the scenario descriptors
#' for regions where some but not all scenarios hit it. Run on the climate
#' ensemble ([filter_climate()]).
#'
#' @param df Indicator data with scenario columns.
#' @param value_col Column with the indicator value.
#' @param threshold Fixed target value.
#' @param threshold_col Column with a region/year-specific target (dynamic
#'   target, e.g. `"minKCAL"` or `"ader"`). Use instead of `threshold`.
#' @param direction `"below"`: hitting means value < target; `"above"`: value > target.
#' @param target_year Year evaluated.
#' @param predictors Scenario columns used by the trees.
#'
#' @return A list with `df_year`, `summary_tbl`, `partial_regions`,
#'   `tree_results` and `rules_tbl`.
#' @export
build_pathway_analysis <- function(df,
                                   value_col,
                                   threshold = NULL,
                                   threshold_col = NULL,
                                   direction = c("below", "above"),
                                   target_year = 2050,
                                   predictors = c("pathway", "warming")) {
    direction <- match.arg(direction)
    .check_threshold(threshold, threshold_col)

    df_year <- df |>
        filter(yrs == target_year) |>
        mutate(
            thr = if (is.null(threshold_col)) threshold else .data[[threshold_col]],
            hits_target = if (direction == "below") {
                as.factor(if_else(.data[[value_col]] < thr, 1, 0))
            } else {
                as.factor(if_else(.data[[value_col]] > thr, 1, 0))
            }
        )

    summary_tbl <- df_year |>
        group_by(region) |>
        summarise(
            n_scenarios = n(),
            n_hits = sum(hits_target == 1),
            pct_hits = round(100 * n_hits / n_scenarios, 1),
            .groups = "drop"
        )

    partial_regions <- summary_tbl |>
        filter(pct_hits > 0, pct_hits < 100) |>
        pull(region)

    formula <- as.formula(paste("hits_target ~", paste(predictors, collapse = " + ")))

    tree_results <- df_year |>
        filter(region %in% partial_regions) |>
        group_by(region) |>
        group_map(~ {
            df_r <- .x
            tree <- tryCatch(
                rpart::rpart(formula, data = df_r, method = "class",
                             control = rpart::rpart.control(minsplit = 2, cp = 0.01)),
                error = function(e) NULL
            )
            list(region = .y$region, tree = tree, data = df_r)
        }, .keep = TRUE)
    names(tree_results) <- vapply(tree_results, \(x) as.character(x$region), character(1))
    tree_results <- Filter(\(x) !is.null(x$tree), tree_results)

    rules_tbl <- tree_results |>
        lapply(\(x) tryCatch(extract_rules(x$tree, x$region), error = function(e) NULL)) |>
        bind_rows()

    list(
        df_year = df_year,
        summary_tbl = summary_tbl,
        partial_regions = partial_regions,
        tree_results = tree_results,
        rules_tbl = rules_tbl
    )
}

#' Extract rules from a classification tree
#'
#' @param tree An rpart tree.
#' @param region_name Region code to tag the rules with.
#'
#' @return A data frame with `region`, `outcome` ("hits"/"misses"), `rule`, `cover`.
#' @export
extract_rules <- function(tree, region_name) {
    rules <- rpart.plot::rpart.rules(tree, cover = TRUE, roundint = FALSE) |> as.data.frame()
    outcome_col <- 1
    cover_col <- ncol(rules)
    rule_cols <- 2:(ncol(rules) - 1)

    data.frame(
        region  = region_name,
        outcome = if_else(as.numeric(rules[[outcome_col]]) >= 0.5, "hits", "misses"),
        rule    = apply(rules[, rule_cols, drop = FALSE], 1, \(x) trimws(paste(x, collapse = " "))),
        cover   = trimws(rules[[cover_col]]),
        stringsAsFactors = FALSE
    )
}

#' Summarise tree rules by region and outcome
#'
#' @param rules_tbl `rules_tbl` from [build_pathway_analysis()].
#'
#' @return One row per region with total cover, number of rules and the rules
#'   for "hits" and "misses".
#' @export
summarise_rules <- function(rules_tbl) {
    rules_tbl |>
        mutate(cover_num = as.numeric(gsub("%", "", cover))) |>
        group_by(region, outcome) |>
        summarise(
            total_cover = sum(cover_num),
            n_rules = n(),
            rules = paste(rule, collapse = " | "),
            .groups = "drop"
        ) |>
        tidyr::pivot_wider(
            names_from = outcome,
            values_from = c(total_cover, n_rules, rules),
            names_glue = "{outcome}_{.value}"
        )
}

#' Bar chart of the share of scenarios hitting a target
#'
#' Regions where some but not all scenarios hit the target.
#'
#' @param analysis Output of [build_pathway_analysis()].
#' @param title Plot title.
#'
#' @return A ggplot.
#' @export
plot_pct_hits <- function(analysis, title = "% scenarios hitting target") {
    analysis$summary_tbl |>
        filter(region %in% analysis$partial_regions) |>
        arrange(pct_hits) |>
        mutate(region = factor(region, levels = region)) |>
        ggplot(aes(x = pct_hits, y = region)) +
        geom_col(fill = "steelblue") +
        geom_vline(xintercept = 50, linetype = "dashed", color = "red") +
        labs(x = "% scenarios hitting target", y = NULL, title = title) +
        theme_minimal()
}

#' Plot the classification tree for one region
#'
#' @param analysis Output of [build_pathway_analysis()].
#' @param region_code Region code.
#' @param title Plot title.
#'
#' @return Called for its side effect (base graphics plot).
#' @export
plot_region_tree <- function(analysis, region_code, title = NULL) {
    title <- title %||% paste(region_code, "- pathways")
    rpart.plot::rpart.plot(analysis$tree_results[[region_code]]$tree,
                           main = title, type = 4, extra = 104, roundint = FALSE)
}

#' Heatmap of tree rules across regions
#'
#' @param analysis Output of [build_pathway_analysis()].
#' @param outcome_filter `"hits"` or `"misses"`.
#' @param title Plot title.
#' @param wrap_width Wrap width for rule labels.
#' @param base_size Base font size.
#' @param save,outfile,plotdir,width,height Saving options.
#'
#' @return A ggplot.
#' @export
plot_rules_heatmap <- function(analysis, outcome_filter = "hits",
                               title = "Pathways by region", wrap_width = 30,
                               base_size = 11,
                               save = FALSE, outfile = NULL, plotdir = ".",
                               width = 18, height = 14) {
    p <- analysis$rules_tbl |>
        filter(outcome == outcome_filter) |>
        mutate(
            cover = as.numeric(gsub("%", "", cover)),
            rule = forcats::fct_reorder(rule, cover, .fun = mean)
        ) |>
        ggplot(aes(x = region, y = rule, fill = cover)) +
        geom_tile() +
        scale_fill_viridis_c(name = "coverage (%)") +
        scale_y_discrete(labels = scales::label_wrap(wrap_width)) +
        labs(x = NULL, y = NULL, title = title) +
        theme_minimal(base_size = base_size) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              legend.position = "bottom",
              legend.key.width = unit(3, "cm"))

    .save_plot(p, save, outfile, plotdir, width, height)
}
