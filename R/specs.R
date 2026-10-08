#' Indicator specifications
#'
#' One entry per indicator with its data, target and labels. Used to drive
#' trajectory figures, distance-to-target overviews and B4T comparisons in a
#' loop.
#'
#' Each entry has: `df`, `value_col`, `threshold` or `threshold_col`,
#' `direction` (`"below"` or `"above"` the target is good), `label`,
#' optional `short_label`, `labeller` (axis labels) and `delta_labeller`
#' (absolute change vs baseline; shares in percentage points).
#'
#' @param fulldata Output of [load_crio_data()].
#' @param nut_threshold Nutrient adequacy target as a multiple of the RNI.
#' @param water_threshold Target for blue water use relative to 2025.
#'
#' @return A named list of specs. The names of the non-nutrient indicators are
#'   stored in `attr(, "main")`.
#' @export
indicator_specs <- function(fulldata, nut_threshold = 1.5, water_threshold = 1) {
    pp_labeller <- scales::label_number(scale = 100, accuracy = 0.01, suffix = " pp",
                                        style_positive = "plus")

    specs <- list(
        HNGR = list(df = fulldata$hunger, value_col = "share",
                    threshold = 0.05, direction = "below", label = "Hunger share",
                    labeller = scales::percent_format(accuracy = 1),
                    delta_labeller = pp_labeller),
        FRTVEG = list(df = fulldata$fruits_and_vegetables, value_col = "value",
                      threshold = 400, direction = "above", label = "F&V availability",
                      labeller = scales::label_number(suffix = "g", big.mark = ","),
                      delta_labeller = scales::label_number(accuracy = 0.1, suffix = "g",
                                                            style_positive = "plus")),
        FATS = list(df = fulldata$fats, value_col = "fat_energy_share",
                    threshold = 0.29, direction = "below", label = "Fat share in calories",
                    labeller = scales::percent_format(accuracy = 1),
                    delta_labeller = pp_labeller),
        SUGARS = list(df = fulldata$sugar, value_col = "sugar_energy_share",
                      threshold = 0.05, direction = "below", label = "Sugar share in calories",
                      labeller = scales::percent_format(accuracy = 1),
                      delta_labeller = pp_labeller),
        `CALORIES-MDER` = list(df = fulldata$MDER, value_col = "value",
                               threshold_col = "minKCAL", direction = "above",
                               label = "Total calories (MDER)",
                               labeller = scales::label_number(big.mark = ","),
                               delta_labeller = scales::label_number(accuracy = 0.1, suffix = " kcal",
                                                                     style_positive = "plus")),
        `CALORIES-2400` = list(df = fulldata$MDER, value_col = "value",
                               threshold = 2400, direction = "above", label = "Total calories",
                               labeller = scales::label_number(big.mark = ","),
                               delta_labeller = scales::label_number(accuracy = 0.1, suffix = " kcal",
                                                                     style_positive = "plus")),
        `CALORIES-ADER` = list(df = fulldata$ADER, value_col = "value",
                               threshold_col = "ader", direction = "above",
                               label = "Total calories (ADER)",
                               labeller = scales::label_number(big.mark = ","),
                               delta_labeller = scales::label_number(accuracy = 0.1, suffix = " kcal",
                                                                     style_positive = "plus")),
        BLUWATER = list(df = fulldata$WATER |> filter(name == "Total"), value_col = "ratio2025",
                        threshold = water_threshold, direction = "below",
                        label = "Blue water ratio over current",
                        labeller = scales::label_number(accuracy = 0.01),
                        delta_labeller = scales::label_number(accuracy = 0.001, style_positive = "plus"))
    )

    main <- names(specs)

    for (nut in unique(fulldata$RNI$Nutrients)) {
        specs[[paste0(nut, "_adequacy")]] <- list(
            df = fulldata$RNI |> filter(Nutrients == nut), value_col = "adequacy_ratio",
            threshold = nut_threshold, direction = "above",
            label = paste(nutrient_labels[[nut]], "availability ratio"),
            short_label = nutrient_labels[[nut]],
            labeller = scales::percent_format(accuracy = 1),
            delta_labeller = pp_labeller)
    }

    attr(specs, "main") <- main
    specs
}

#' Nutrient quantity specifications
#'
#' Nutrient availability in physical units (per capita per day) instead of
#' adequacy ratios. The target is `nut_threshold` x RNI; the RNI itself is
#' added as a reference line (`ref_value`).
#'
#' @inheritParams indicator_specs
#'
#' @return A named list of specs (names are nutrient codes), with `title`,
#'   `ref_value` and `ref_label` set.
#' @export
nutrient_quantity_specs <- function(fulldata, nut_threshold = 1.5) {
    specs <- list()
    for (nut in unique(fulldata$RNI$Nutrients)) {
        unit <- sub("^.*_", "", nut) # g / mg / mcg from the nutrient code
        rni  <- rni_lookup$RNI[rni_lookup$Nutrients == nut]

        specs[[nut]] <- list(
            df = fulldata$RNI |> filter(Nutrients == nut), value_col = "total_nutrient",
            threshold = nut_threshold * rni, direction = "above",
            label = paste0(nutrient_labels[[nut]], " availability (", unit, "/capita/day)"),
            short_label = nutrient_labels[[nut]],
            labeller = scales::label_number(big.mark = ",", suffix = paste0(" ", unit)),
            delta_labeller = scales::label_number(accuracy = 0.01, suffix = paste0(" ", unit),
                                                  style_positive = "plus"),
            ref_value = rni,
            ref_label = paste0("RNI (", rni, " ", unit, ")"),
            title = paste0(nutrient_labels[[nut]], " availability - RNI = ", rni, " ", unit,
                           "; target = ", nut_threshold, " x RNI = ", nut_threshold * rni, " ", unit))
    }
    specs
}
