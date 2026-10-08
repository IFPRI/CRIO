# Indicator readers. Each returns one row per region x year x scenario with the
# scenario descriptor columns from add_scenario_cols().

#' Population at risk of hunger
#'
#' Hunger is only solved in 5-year steps; GFSir's `group5` merges it onto
#' annual population, so the in-between years come back as 0. Only years with
#' hunger results are kept.
#'
#' Note: the hunger module floors the share at risk at 1 percent, scaled to each
#' country's FAO 2020 numbers, so countries can flatten at a country-specific
#' minimum (e.g. ~3.3 percent for CIV).
#'
#' @param files Scenario GDX files (see [list_scenarios()]).
#' @param mapping GFSir mapping file name.
#'
#' @return Hunger data with `value` (million people) and `share`.
#' @export
get_hunger <- function(files, mapping = "mappingCG6.xlsx") {
    hngr <- read_scenarios(files, GFSir::group5, mapping = mapping)
    yrs_with_hunger <- unique(hngr$yrs[hngr$value > 0])
    hngr |>
        filter(yrs %in% yrs_with_hunger) |>
        add_scenario_cols()
}

#' Food availability (all commodities and food groups)
#'
#' @inheritParams get_hunger
#'
#' @return GFSir `foodavail` output for all scenarios (no scenario columns).
#' @export
get_foodavail <- function(files, mapping = "mappingCG6.xlsx") {
    read_scenarios(files, GFSir::group3, indicator = "foodavail", mapping = mapping)
}

#' Fruit and vegetable availability (g/person/day)
#'
#' @param foodavail Output of [get_foodavail()].
#'
#' @return F&V availability.
#' @export
get_frtveg <- function(foodavail) {
    foodavail |>
        filter(name %in% "F&V", description %in% "Food availability (g/person/day)") |>
        add_scenario_cols()
}

#' Per capita calories by commodity, with totals and shares
#'
#' Adds a `Total` row over [food_groups] and each row's `share` of the total.
#' GRL is dropped (no demand except fish).
#'
#' @inheritParams get_hunger
#'
#' @return Calories (kcal/person/day).
#' @export
get_calories <- function(files, mapping = "mappingCG6.xlsx") {
    calories <- read_scenarios(files, GFSir::group4, mapping = mapping) |>
        filter(region != "GRL")

    total_rows <- calories |>
        filter(name %in% food_groups) |>
        group_by(region, yrs, ssp, gcm, rcp, co2, id) |>
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        mutate(description = "per capita calorie by commodity (KCal per person per day)",
               name = "Total")

    bind_rows(calories, total_rows) |>
        group_by(region, yrs, ssp, gcm, rcp, co2, id) |>
        mutate(share = value / value[name == "Total"]) |>
        ungroup() |>
        add_scenario_cols()
}

#' Approximate share of calories from fat
#'
#' @param calories Output of [get_calories()].
#' @param fat_shares Fat share of calories by commodity (default [fat_kcal_share]).
#'
#' @return One row per region x year x scenario with `fat_energy_share`.
#' @export
get_fat_share <- function(calories, fat_shares = fat_kcal_share) {
    calories |>
        left_join(fat_shares, by = "name") |>
        filter(!is.na(fat_kcal_share)) |>
        mutate(fat_kcal = value * fat_kcal_share) |>
        group_by(region, yrs, ssp, gcm, rcp, co2, id) |>
        summarise(fat_kcal_total = sum(fat_kcal, na.rm = TRUE), .groups = "drop") |>
        left_join(
            calories |> filter(name == "Total") |> select(region, yrs, ssp, gcm, rcp, co2, id, total_kcal = value),
            by = c("region", "yrs", "ssp", "gcm", "rcp", "co2", "id")
        ) |>
        mutate(fat_energy_share = fat_kcal_total / total_kcal) |>
        add_scenario_cols()
}

#' Share of calories from sugar
#'
#' @param calories Output of [get_calories()].
#' @param sugar_name Commodity name for refined sugar.
#'
#' @return One row per region x year x scenario with `sugar_energy_share`.
#' @export
get_sugar_share <- function(calories, sugar_name = "O&S-Sugar  (p)") {
    calories |>
        filter(name == sugar_name) |>
        group_by(region, yrs, ssp, gcm, rcp, co2, id) |>
        summarise(sugar_kcal = sum(value, na.rm = TRUE), .groups = "drop") |>
        left_join(
            calories |> filter(name == "Total") |> select(region, yrs, ssp, gcm, rcp, co2, id, total_kcal = value),
            by = c("region", "yrs", "ssp", "gcm", "rcp", "co2", "id")
        ) |>
        mutate(sugar_energy_share = sugar_kcal / total_kcal) |>
        add_scenario_cols()
}

#' Minimum calorie requirement by country
#'
#' @param gdx A scenario GDX file containing `MinKCAL` (e.g. the baseline).
#'
#' @return A data frame with `region` and `minKCAL`.
#' @export
get_minkcal <- function(gdx) {
    m <- gamstransfer::Container$new(gdx)
    m$getSymbols("MinKCAL")[[1]]$records |>
        rename(region = CTY, minKCAL = value)
}

#' Total calories against the minimum dietary energy requirement (MDER)
#'
#' @param calories Output of [get_calories()].
#' @param minkcal Output of [get_minkcal()].
#'
#' @return Total calories with `minKCAL` and `hits_target`.
#' @export
get_mder <- function(calories, minkcal) {
    calories |>
        filter(name == "Total") |>
        left_join(minkcal, by = "region") |>
        mutate(hits_target = as.factor(if_else(value >= minKCAL, 1, 0)))
}

#' Total calories against the average dietary energy requirement (ADER)
#'
#' ADER comes from FAOSTAT Food Security and is extended to future years with
#' a linear trend per country.
#'
#' @param calories Output of [get_calories()].
#' @param faostat_csv Path to FAOSTAT `Food_Security_Data_E_All_Data_(Normalized).csv`.
#' @param sets_xlsx Path to the IMPACT `Sets.xlsx` (FAO to IMPACT country codes).
#' @param years Years to extrapolate ADER to.
#'
#' @return Total calories with `ader` and `hits_target`.
#' @export
get_ader <- function(calories, faostat_csv, sets_xlsx, years = 2025:2050) {
    ader <- tibble::as_tibble(data.table::fread(faostat_csv)) |>
        filter(Item %in% "Average dietary energy requirement (kcal/cap/day)")

    regcodes <- openxlsx2::read_xlsx(sets_xlsx, sheet = "Regions", rows = 4:500,
                                     skip_empty_rows = TRUE, cols = 5:6)
    names(ader) <- tolower(names(ader))
    ader <- collapse::join(ader, regcodes, on = c("area code" = "FCTY"), multiple = TRUE)

    ader <- ader |>
        select(CTY, year, value) |>
        mutate(year = as.numeric(as.character(year)),
               value = as.numeric(trimws(as.character(value)))) |>
        filter(!is.na(CTY)) |>
        group_by(CTY, year) |>
        summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

    ader_extended <- ader |>
        group_by(CTY) |>
        group_modify(function(sd, key) {
            sd_clean <- filter(sd, !is.na(value))
            if (nrow(sd_clean) < 2) return(sd_clean)
            fit <- lm(value ~ year, data = sd_clean)
            new_years <- tibble::tibble(year = years)
            new_years$value <- predict(fit, newdata = new_years)
            bind_rows(sd_clean, filter(new_years, !year %in% sd_clean$year))
        }) |>
        ungroup() |>
        rename(region = CTY, yrs = year, ader = value) |>
        mutate(yrs = as.factor(as.character(yrs)))

    calories |>
        filter(name == "Total") |>
        left_join(ader_extended, by = c("region", "yrs")) |>
        mutate(hits_target = as.factor(if_else(value >= ader, 1, 0)))
}

#' Nutrient availability and adequacy ratios
#'
#' Food availability (kg/person/day) x nutrient content per kg, summed over
#' commodities, divided by the RNI. Nutrient contents come from the Hunger
#' model's `Nutrients_per_kg.gdx` (country-specific) plus
#' [aquatic_nutrient_per_kg] placeholders for aquatic foods.
#'
#' This is nutrient supply, not intake.
#'
#' @param foodavail Output of [get_foodavail()].
#' @param nutrients_gdx Path to `Nutrients_per_kg.gdx`.
#' @param mapping GFSir mapping file name (commodity mapping from its `crops` sheet).
#' @param rni RNI table (default [rni_lookup]).
#' @param aquatic Aquatic nutrient values (default [aquatic_nutrient_per_kg]).
#' @param exclude Commodities to exclude (default [exclude_cmdty]).
#'
#' @return One row per region x year x scenario x nutrient with
#'   `total_nutrient` (per capita per day), `RNI` and `adequacy_ratio`.
#' @export
get_nutrient_adequacy <- function(foodavail,
                                  nutrients_gdx,
                                  mapping = "mappingCG6.xlsx",
                                  rni = rni_lookup,
                                  aquatic = aquatic_nutrient_per_kg,
                                  exclude = exclude_cmdty) {
    m_nut <- gamstransfer::Container$new(nutrients_gdx)
    get_records <- function(symbol) m_nut$getSymbols(symbol)[[1]]$records

    nutrient_per_kg_all <- bind_rows(
        get_records("Nutrients_per_kg_cty")    |> rename(region = CTY),
        get_records("Nutrients_per_kg_CG6")    |> rename(region = CG6),
        get_records("Nutrients_per_kg_DVDDVG") |> rename(region = DVDDVG),
        get_records("Nutrients_per_kg_WLD")    |> rename(region = WLD) |> mutate(region = "WLD"))

    nutrient_per_kg_full <- nutrient_per_kg_all |>
        bind_rows(tidyr::crossing(aquatic, region = unique(nutrient_per_kg_all$region)))

    com_map_join <- GFSir::getMapping(type = "crops", file = mapping) |>
        filter(grepl("^c", j_c), grepl("-", name)) |>
        distinct(j_c, name) |>
        rename(cmdty = j_c)

    food_avail_kg <- foodavail |>
        filter(description %in% "Food availability (kg/person/yr)") |>
        mutate(description = "Food availability (kg/person/day)",
               value = value / 365)

    food_avail_kg |>
        filter(grepl("-", name)) |>
        rename(kg_per_day = value) |>
        left_join(com_map_join, by = "name", relationship = "many-to-many") |>
        filter(!cmdty %in% exclude) |>
        left_join(nutrient_per_kg_full |> rename(nutrient_value_per_kg = value),
                  by = c("region", "cmdty"), relationship = "many-to-many") |>
        mutate(nutrient_value = kg_per_day * nutrient_value_per_kg) |>
        group_by(region, yrs, ssp, gcm, rcp, co2, id, Nutrients) |>
        summarise(total_nutrient = sum(nutrient_value, na.rm = TRUE), .groups = "drop") |>
        left_join(rni, by = "Nutrients") |>
        mutate(adequacy_ratio = total_nutrient / RNI) |>
        add_scenario_cols()
}

#' Blue water use, with totals, shares and ratio to 2025
#'
#' Countries with total blue water below `min_volume` in the baseline in 2025
#' are dropped. `ratio2025` is each value over its own 2025 value.
#'
#' @inheritParams get_hunger
#' @param baseline_id Baseline scenario id.
#' @param min_volume Minimum 2025 baseline total (cubic km) to keep a country.
#'
#' @return Blue water data (cubic km).
#' @export
get_water <- function(files,
                      mapping = "mappingCG6.xlsx",
                      baseline_id = crio_baseline_id,
                      min_volume = 1e-2) {
    water <- read_scenarios(files, GFSir::group3, indicator = "bluewater", mapping = mapping) |>
        filter(region != "GRL")

    total_rows <- water |>
        filter(name %in% food_groups) |>
        group_by(region, yrs, ssp, gcm, rcp, co2, id) |>
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
        mutate(description = "Blue water or water from irrigation (cubic km)",
               name = "Total")

    small_cty <- total_rows |>
        filter(id == baseline_id, yrs == 2025, value < min_volume) |>
        pull(region)

    bind_rows(water, total_rows) |>
        filter(!region %in% small_cty) |>
        group_by(region, yrs, ssp, gcm, rcp, co2, id) |>
        mutate(share = value / value[name == "Total"]) |>
        ungroup() |>
        group_by(region, ssp, gcm, name, rcp, co2, id) |>
        mutate(ratio2025 = {
            base_val <- value[yrs == 2025]
            if (length(base_val) == 0) NA_real_ else value / base_val[1]
        }) |>
        ungroup() |>
        add_scenario_cols()
}

#' Build all RIO indicator datasets
#'
#' @param files Scenario GDX files (see [list_scenarios()]).
#' @param nutrients_gdx Path to `Nutrients_per_kg.gdx`.
#' @param faostat_csv Path to the FAOSTAT Food Security csv (for ADER).
#' @param sets_xlsx Path to IMPACT `Sets.xlsx` (for ADER).
#' @param mapping GFSir mapping file name.
#' @param baseline_id Baseline scenario id; its GDX supplies `MinKCAL`.
#'
#' @return A named list: `hunger`, `fruits_and_vegetables`, `calories`,
#'   `fats`, `sugar`, `MDER`, `ADER`, `RNI`, `WATER`, `minkcal`.
#' @export
load_crio_data <- function(files,
                           nutrients_gdx,
                           faostat_csv,
                           sets_xlsx,
                           mapping = "mappingCG6.xlsx",
                           baseline_id = crio_baseline_id) {
    baseline_gdx <- files[scenario_id(files) == baseline_id]
    if (length(baseline_gdx) != 1) stop("Baseline '", baseline_id, "' not found in files")

    foodavail <- get_foodavail(files, mapping)
    calories  <- get_calories(files, mapping)
    minkcal   <- get_minkcal(baseline_gdx)

    list(
        hunger                = get_hunger(files, mapping),
        fruits_and_vegetables = get_frtveg(foodavail),
        calories              = calories,
        fats                  = get_fat_share(calories),
        sugar                 = get_sugar_share(calories),
        MDER                  = get_mder(calories, minkcal),
        ADER                  = get_ader(calories, faostat_csv, sets_xlsx),
        RNI                   = get_nutrient_adequacy(foodavail, nutrients_gdx, mapping),
        WATER                 = get_water(files, mapping, baseline_id),
        minkcal               = minkcal
    )
}
