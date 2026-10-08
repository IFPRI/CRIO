#' Country to CG region lookup
#'
#' Read from the `regions` sheet of a GFSir mapping file, which is long: one
#' row per (cty, region) membership. Cached after the first call.
#'
#' @param mapping GFSir mapping file name.
#'
#' @return A tibble with columns `cty` and `cg_region`.
#' @export
cg_lookup <- function(mapping = "mappingCG6.xlsx") {
    key <- paste0("cg_lookup_", mapping)
    if (is.null(.crio_env[[key]])) {
        .crio_env[[key]] <- GFSir::getMapping(type = "region", file = mapping) |>
            filter(region %in% b4t_regions) |>
            distinct(cty, cg_region = region)
    }
    .crio_env[[key]]
}

#' CG region a region code belongs to
#'
#' @param r Region codes (countries and/or CG regions).
#' @param mapping GFSir mapping file name.
#'
#' @return The code itself for CG regions, the CG region for countries, `NA`
#'   for anything else (WLD, DVD/DVG, countries outside CG regions).
#' @export
own_cg_region <- function(r, mapping = "mappingCG6.xlsx") {
    r <- as.character(r)
    lookup <- cg_lookup(mapping)
    if_else(r %in% b4t_regions, r, lookup$cg_region[match(r, lookup$cty)])
}

# Facet strip labels: countries get their CG region, e.g. "NGA (WCA)"
.region_labels <- function(regions, mapping = "mappingCG6.xlsx") {
    own <- own_cg_region(regions, mapping)
    stats::setNames(if_else(!is.na(own) & own != regions,
                            paste0(regions, " (", own, ")"), regions),
                    regions)
}
