#' @keywords internal
"_PACKAGE"

#' @rawNamespace import(dplyr, except = vars)
#' @import ggplot2
#' @importFrom rlang .data %||%
#' @importFrom stats as.formula lm predict
#' @importFrom tidyterra geom_spatvector
NULL

# Package-level cache (CG region lookup, world outline)
.crio_env <- new.env(parent = emptyenv())
