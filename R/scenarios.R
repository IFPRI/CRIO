#' List RIO scenario GDX files
#'
#' Climate runs are named `SSP-GCM-RCP-379` (no CO2 fertilization). B4T runs
#' replace the last segment with the CG region where B4T is implemented
#' (`CWANA`, `ESA`, ..., `CG6` = all CG regions).
#'
#' @param scendir Folder with scenario GDX files.
#' @param include_nocc Keep `NoCC` (no climate change) runs. Default `FALSE`.
#'
#' @return Character vector of full file paths.
#' @export
list_scenarios <- function(scendir, include_nocc = FALSE) {
    f <- list.files(scendir, pattern = "^SSP[123]-.*\\.gdx$", full.names = TRUE)
    if (!include_nocc) f <- grep(pattern = "NoCC", x = f, invert = TRUE, value = TRUE)
    f
}

#' Scenario id from a GDX file path
#'
#' @param gdx GDX file path(s).
#'
#' @return File name without folder and `.gdx` extension.
#' @export
scenario_id <- function(gdx) {
    sub("\\.gdx$", "", basename(gdx))
}

#' Read one GFSir indicator for many scenarios
#'
#' Calls a GFSir reader (e.g. [GFSir::group5()]) on each GDX file, adds the
#' scenario `id` and binds the results.
#'
#' @param files GDX file paths.
#' @param fun GFSir reader function taking `gdx` as first argument.
#' @param ... Further arguments passed to `fun` (e.g. `mapping`, `indicator`).
#'
#' @return A data.table with all scenarios stacked.
#' @export
read_scenarios <- function(files, fun, ...) {
    tmplist <- lapply(files, function(i) {
        message("Reading ", basename(i))
        tmp <- fun(gdx = i, ...)
        tmp$id <- scenario_id(i)
        tmp
    })
    data.table::rbindlist(l = tmplist)
}

#' Add scenario descriptor columns
#'
#' GFSir parses the 4th id segment into `co2`, so B4T suffixes land there.
#' Everything is derived from `id` instead.
#'
#' Adds `b4t` (`"None"`, a CG region, or `"All CG regions"`), `ensemble`
#' (`"Climate"` or `"B4T"`), `pathway` (socioeconomic narrative), `warming`
#' (low/high by GCM) and resets `co2` / `co2_fert` (all runs are without CO2
#' fertilization).
#'
#' @param df Data with `id`, `ssp` and `gcm` columns.
#' @param low_warming_gcms GCMs classed as "Low warming".
#'
#' @return `df` with the added columns.
#' @export
add_scenario_cols <- function(df, low_warming_gcms = c("GFDL", "MPI", "MRI")) {
    df |>
        mutate(
            co2 = "NoCO2",
            co2_fert = "no",
            b4t = sub("^.*-", "", id),
            b4t = case_when(
                b4t %in% c("379", "NoCC") ~ "None",
                b4t == "CG6" ~ "All CG regions",
                .default = b4t
            ),
            ensemble = if_else(b4t == "None", "Climate", "B4T"),
            pathway = case_when(
                ssp == "SSP1" ~ "Sustainability",
                ssp == "SSP2" ~ "Middle of the road",
                ssp == "SSP3" ~ "Reference",
                .default = NA_character_
            ),
            warming = case_when(
                gcm == "NoCC" ~ "No climate change",
                gcm %in% low_warming_gcms ~ "Low warming",
                .default = "High warming"
            ))
}

#' Keep the climate ensemble only
#'
#' Drops B4T runs, leaving the SSP x GCM runs (including the baseline).
#'
#' @param df Data with an `ensemble` column (see [add_scenario_cols()]).
#'
#' @return Filtered `df`.
#' @export
filter_climate <- function(df) {
    df |> filter(ensemble == "Climate")
}
