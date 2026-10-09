#' List scenario GDX files
#'
#' Climate runs are named `SSP-GCM-RCP-379` (no CO2 fertilization), e.g.
#' `SSP1-GFDL-126-379` or `SSP3-MRI-370-379`. B4T runs replace the last
#' segment with the CG region where B4T is implemented (`CWANA`, `ESA`, ...,
#' `CG6` = all CG regions). Any SSP (1-5), GCM and RCP is picked up; you can
#' also pass your own vector of files to the readers instead.
#'
#' @param scendir Folder with scenario GDX files.
#' @param include_nocc Keep `NoCC` (no climate change) runs. Default `FALSE`.
#'
#' @return Character vector of full file paths.
#' @export
list_scenarios <- function(scendir, include_nocc = FALSE) {
    f <- list.files(scendir, pattern = "^SSP[1-5]-.*\\.gdx$", full.names = TRUE)
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

#' Climate run matching a scenario
#'
#' The climate (non-B4T) run with the same SSP, GCM and RCP, e.g.
#' `SSP3-MRI-370-WCA` -> `SSP3-MRI-370-379`. Used to compare each B4T run
#' with its own counterfactual.
#'
#' @param id Scenario id(s).
#'
#' @return Scenario id(s) of the matching climate run.
#' @export
climate_counterpart <- function(id) {
    sub("-[^-]+$", "-379", id)
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

#' Default pathway names by SSP and RCP
#'
#' Combinations not listed here are named automatically as `"SSPx-RCPy"`.
#' Pass your own table to [add_scenario_cols()] (via [load_crio_data()]) to
#' rename or add pathways.
#'
#' @format A tibble with columns `ssp`, `rcp` (as parsed by GFSir, e.g.
#'   `"2.6"`, `"7.0"`) and `pathway`.
#' @export
default_pathways <- tibble::tribble(
    ~ssp,   ~rcp,  ~pathway,
    "SSP1", "2.6", "Green world",
    "SSP1", "7.0", "Sustainability",
    "SSP2", "7.0", "Middle of the road",
    "SSP3", "7.0", "Reference"
)

#' Add scenario descriptor columns
#'
#' GFSir parses the 4th id segment into `co2`, so B4T suffixes land there.
#' Everything is derived from `id`, `ssp`, `gcm` and `rcp` instead.
#'
#' Adds:
#' \describe{
#'   \item{`b4t`}{`"None"`, the CG region where B4T is implemented, or `"All CG regions"`.}
#'   \item{`ensemble`}{`"Climate"` or `"B4T"`.}
#'   \item{`pathway`}{Name of the SSP x RCP combination from `pathways`
#'     (e.g. `"Green world"`), or `"SSPx-RCPy"` if not listed.}
#'   \item{`warming`}{`"Lower emission"` for `lower_emission_rcps`; otherwise
#'     `"Low warming"` / `"High warming"` by GCM; `"No climate change"` for NoCC.}
#' }
#' and resets `co2` / `co2_fert` (all runs are without CO2 fertilization).
#'
#' @param df Data with `id`, `ssp`, `gcm` and `rcp` columns.
#' @param pathways Table of `ssp`, `rcp` and `pathway` names (default [default_pathways]).
#' @param low_warming_gcms GCMs classed as "Low warming".
#' @param lower_emission_rcps RCPs classed as "Lower emission".
#'
#' @return `df` with the added columns.
#' @export
add_scenario_cols <- function(df,
                              pathways = default_pathways,
                              low_warming_gcms = c("GFDL", "MPI", "MRI"),
                              lower_emission_rcps = "2.6") {
    pathway_keys <- paste(pathways$ssp, pathways$rcp)

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
            pathway = pathways$pathway[match(paste(ssp, rcp), pathway_keys)],
            pathway = coalesce(pathway, paste0(ssp, "-RCP", rcp)),
            warming = case_when(
                gcm == "NoCC" ~ "No climate change",
                rcp %in% lower_emission_rcps ~ "Lower emission",
                gcm %in% low_warming_gcms ~ "Low warming",
                .default = "High warming"
            ))
}

#' Keep the climate ensemble only
#'
#' Drops B4T runs, leaving the SSP x GCM x RCP runs (including the baseline).
#'
#' @param df Data with an `ensemble` column (see [add_scenario_cols()]).
#'
#' @return Filtered `df`.
#' @export
filter_climate <- function(df) {
    df |> filter(ensemble == "Climate")
}

#' Select scenarios in all datasets
#'
#' Filters every element of `fulldata` that has scenario columns, so all
#' later figures and tables use only the chosen runs. `NULL` keeps everything
#' for that dimension.
#'
#' @param fulldata Output of [load_crio_data()].
#' @param ssp SSPs to keep, e.g. `c("SSP2", "SSP3")`.
#' @param rcp RCPs to keep, as parsed by GFSir, e.g. `"7.0"`.
#' @param gcm GCMs to keep, e.g. `c("MRI", "UKESM")`.
#' @param b4t B4T values to keep: `"None"` for climate runs, CG regions,
#'   `"All CG regions"`.
#' @param ids Scenario ids to keep.
#'
#' @return `fulldata` with filtered elements.
#' @export
select_scenarios <- function(fulldata, ssp = NULL, rcp = NULL, gcm = NULL,
                             b4t = NULL, ids = NULL) {
    keep <- list(ssp = ssp, rcp = rcp, gcm = gcm, b4t = b4t, id = ids)
    keep <- keep[!vapply(keep, is.null, logical(1))]

    lapply(fulldata, function(df) {
        if (!is.data.frame(df) || !"id" %in% names(df)) return(df)
        for (col in names(keep)) {
            if (col %in% names(df)) df <- filter(df, .data[[col]] %in% keep[[col]])
        }
        df
    })
}

#' Relabel scenarios in all datasets
#'
#' Re-applies [add_scenario_cols()] to every element of `fulldata`, e.g. to
#' rename pathways or change the GCM classification without re-reading GDXs.
#'
#' @param fulldata Output of [load_crio_data()].
#' @param ... Arguments to [add_scenario_cols()]: `pathways`,
#'   `low_warming_gcms`, `lower_emission_rcps`.
#'
#' @return `fulldata` with updated scenario columns.
#' @export
relabel_scenarios <- function(fulldata, ...) {
    lapply(fulldata, function(df) {
        if (!is.data.frame(df) || !all(c("id", "ssp", "gcm", "rcp") %in% names(df))) return(df)
        add_scenario_cols(df, ...)
    })
}
