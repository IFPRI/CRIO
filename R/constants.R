#' Default baseline scenario
#'
#' SSP3, MRI climate model, RCP 7.0, no CO2 fertilization.
#'
#' @format A character string.
#' @export
crio_baseline_id <- "SSP3-MRI-370-379"

#' CGIAR regions where Breeding For Tomorrow (B4T) is implemented
#'
#' @format A character vector of CG region codes.
#' @export
b4t_regions <- c("CWANA", "ESA", "LAC", "SA", "SEA", "WCA")

#' Factor levels for the B4T scenario column
#'
#' The six CG regions plus "All CG regions" (the CG6 scenario).
#'
#' @format A character vector.
#' @export
b4t_levels <- c(b4t_regions, "All CG regions")

#' Food groups used for calorie and water totals
#'
#' @format A character vector of GFSir food group codes.
#' @export
food_groups <- c("ASF", "CER", "RTB", "PUL", "F&V", "O&S", "MLS", "OTH")

#' Rest-of-world countries greyed out on maps
#'
#' @format A character vector of IMPACT region codes.
#' @export
row_countries <- c("ALB", "AUS", "AUT", "BGR", "BLR", "BLT", "BLX", "CAN", "CHP",
                   "CZE", "DEU", "DNK", "FNP", "FRP", "GRC", "GRL", "HRV", "HUN",
                   "IRL", "ISL", "ITP", "JPN", "KOR", "MDA", "NLD", "NOR", "NZL",
                   "OBN", "POL", "PRT", "ROU", "RUS", "SPP", "SVK", "SVN", "SWE",
                   "UKP", "UKR", "USA")

# Colours: "good" / "bad" direction relative to a target
.col_good <- "#1B9E77"
.col_bad  <- "#A6761D"

# Range bands by pathway (see default_pathways)
.pathway_cols <- c("Green world" = "#1A9850",
                   "Sustainability" = "#4575B4",
                   "Middle of the road" = "#FDAE61",
                   "Reference" = "grey50")

# Colours for the pathways present: fixed colours for known ones, extra
# colours for anything else (e.g. auto-named "SSP2-RCP4.5")
.pathway_palette <- function(pathways) {
    pathways <- unique(as.character(pathways))
    extra <- setdiff(pathways, names(.pathway_cols))
    extra_cols <- stats::setNames(
        RColorBrewer::brewer.pal(8, "Set2")[(seq_along(extra) - 1) %% 8 + 1], extra)
    c(.pathway_cols, extra_cols)
}
