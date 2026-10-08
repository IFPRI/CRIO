#' Save indicator datasets as RDS and/or CSV
#'
#' One file per element of `fulldata`. CSVs keep only food groups, their
#' commodities and totals where a `name` column exists.
#'
#' @param fulldata Output of [load_crio_data()].
#' @param rdsdir Folder for RDS files (`NULL` to skip).
#' @param csvdir Folder for CSV files (`NULL` to skip).
#' @param suffix Suffix added to CSV file names, e.g. `"-[v3]"`.
#'
#' @return `fulldata`, invisibly.
#' @export
save_crio_data <- function(fulldata, rdsdir = NULL, csvdir = NULL, suffix = "") {
    fg_out <- c(food_groups, "Total")
    fg_pattern <- paste0("^(", paste(fg_out, collapse = "|"), ")-")

    for (i in names(fulldata)) {
        if (!is.null(rdsdir)) {
            dir.create(rdsdir, showWarnings = FALSE, recursive = TRUE)
            saveRDS(object = fulldata[[i]], file = file.path(rdsdir, paste0(i, ".rds")))
        }
        if (!is.null(csvdir)) {
            dir.create(csvdir, showWarnings = FALSE, recursive = TRUE)
            tmp <- fulldata[[i]]
            if ("name" %in% names(tmp)) {
                tmp <- tmp |> filter(name %in% fg_out | grepl(fg_pattern, name))
            }
            data.table::fwrite(x = tmp, file = file.path(csvdir, paste0(i, suffix, ".csv")))
        }
    }

    invisible(fulldata)
}
