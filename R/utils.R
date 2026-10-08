# Fixed (`threshold`) or dynamic (`threshold_col`) targets: exactly one is needed.
# Use as: mutate(thr = if (is.null(threshold_col)) threshold else .data[[threshold_col]])
.check_threshold <- function(threshold = NULL, threshold_col = NULL) {
    if (is.null(threshold) && is.null(threshold_col)) {
        stop("Supply either `threshold` or `threshold_col`")
    }
    invisible(TRUE)
}

# Save a ggplot if requested, then return it. Adds ".png" when `outfile` has
# no extension.
.save_plot <- function(p, save, outfile, plotdir, width, height) {
    if (save) {
        if (is.null(outfile)) stop("outfile must be specified when save = TRUE")
        if (!grepl("\\.[A-Za-z0-9]+$", outfile)) outfile <- paste0(outfile, ".png")
        ggsave(filename = file.path(plotdir, outfile), plot = p,
               width = width, height = height, bg = "white")
    }
    p
}
