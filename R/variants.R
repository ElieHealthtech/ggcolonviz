#' Register a Custom Colon Variant
#'
#' Adds a new named blueprint to the package's internal variant registry so it
#' can be referenced by `variant` arguments in [geom_colon()],
#' [geom_colon_label()], [colon_sf()], and [colon_heatmap_grid()].
#'
#' @param name A non-empty character string naming the new variant.
#'   Existing variants (including `"standard"`, `"sescd"` and `"ibd"`) will be
#'   overwritten if the same name is supplied.
#' @param data A data frame with at least three columns:
#'   \describe{
#'     \item{`dx`}{Numeric. Relative x-coordinate of each polygon vertex.}
#'     \item{`dy`}{Numeric. Relative y-coordinate of each polygon vertex.}
#'     \item{`part`}{Character. Segment name each vertex belongs to.}
#'   }
#'   Each unique `part` value must have at least 3 rows (minimum for a polygon).
#' @return The `name` string, invisibly.
#' @export
#' @examples
#' my_blueprint <- data.frame(
#'   dx   = c(0, 1, 1, 0,   2, 3, 3, 2),
#'   dy   = c(0, 0, 1, 1,   0, 0, 1, 1),
#'   part = c(rep("SegA", 4), rep("SegB", 4))
#' )
#' register_colon_variant("my_colon", my_blueprint)
#' list_colon_variants()
register_colon_variant <- function(name, data) {
    if (!is.character(name) || length(name) != 1L || nchar(name) == 0L) {
        stop(
            "`name` must be a single non-empty character string.",
            call. = FALSE
        )
    }
    if (!is.data.frame(data)) {
        stop("`data` must be a data frame.", call. = FALSE)
    }
    required_cols <- c("dx", "dy", "part")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0L) {
        stop(
            "`data` must contain columns: ",
            paste(required_cols, collapse = ", "),
            ". Missing: ",
            paste(missing_cols, collapse = ", "),
            call. = FALSE
        )
    }
    if (!is.numeric(data$dx) || !is.numeric(data$dy)) {
        stop("`dx` and `dy` columns must be numeric.", call. = FALSE)
    }
    counts <- tapply(data$part, data$part, length)
    bad <- names(counts[counts < 3L])
    if (length(bad) > 0L) {
        stop(
            "Each part must have at least 3 rows. Insufficient rows in: ",
            paste(bad, collapse = ", "),
            call. = FALSE
        )
    }
    .ggcolonviz_env$variants[[name]] <- data
    # Label anchors belong to the blueprint they were drawn for
    .ggcolonviz_env$label_anchors[[name]] <- NULL
    invisible(name)
}

#' List Registered Colon Variant Names
#'
#' Returns the names of all variants currently registered in the package
#' registry, including the built-in `"standard"`, `"sescd"` and `"ibd"` variants plus
#' any added via [register_colon_variant()].
#'
#' @return A character vector of variant names.
#' @export
#' @examples
#' list_colon_variants()
list_colon_variants <- function() {
    names(.ggcolonviz_env$variants)
}

# Internal accessor used by geom_colon, geom_colon_label, colon_sf, etc.
.get_variant <- function(variant) {
    v <- .ggcolonviz_env$variants[[variant]]
    if (is.null(v)) {
        stop(
            "Unknown variant '",
            variant,
            "'. ",
            "Available variants: ",
            paste(list_colon_variants(), collapse = ", "),
            call. = FALSE
        )
    }
    v
}
