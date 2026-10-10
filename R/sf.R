#' @title Colon Geometry as sf Object
#' @description This function converts the colon blueprint into an sf object,
#'  which can be used for spatial operations and visualizations. It groups the
#'  blueprint by colon parts and creates polygon geometries for each part.
#' @param variant A character string specifying the variant of the colon
#'   blueprint to use.
#' @details The resulting sf object contains the geometry of the colon parts,
#'  which can be used for various spatial analyses or visualizations, such as
#'  creating heatmaps or overlaying additional data on the colon structure.
#' @return An sf object with polygon geometries for each part of the colon.
#' @export
colon_sf <- function(variant = "standard") {
    .get_variant(variant) |>
        sf::st_as_sf(coords = c("dx", "dy"), crs = NA) |>
        dplyr::group_by(part) |>
        dplyr::summarise(geometry = sf::st_combine(geometry)) |>
        sf::st_cast("POLYGON") |>
        sf::st_make_valid()
}

#' @title Colon Heatmap Grid
#' @description This function creates a grid of points within the colon geometry,
#'  which can be used to generate heatmaps or other spatial visualizations.
#'  The grid is created by intersecting a dense grid of points with the colon geometry,
#'  resulting in a set of points that lie within the colon structure.
#' @details The function first creates a dense grid of points based on the specified resolution,
#'  then intersects this grid with the colon geometry to retain only the points that
#'  lie within the colon. The resulting sf object contains the coordinates of the points
#'  that can be used for further analysis or visualization.
#' @param grid_res The resolution of the grid, which determines how many points are generated.
#'  A higher resolution will create a denser grid, which can lead to
#'  smoother heatmaps but may also increase computation time.
#' @param variant A character string specifying the variant of the colon
#'   blueprint to use.
#' @return An sf object containing the points that lie within the colon geometry,
#'  which can be used for heatmap generation or other spatial analyses.

#' @export
colon_heatmap_grid <- function(grid_res = 500, variant = "standard") {
    suppressWarnings(
        sf::st_intersection(
            colon_grid(grid_res, variant = variant),
            colon_sf(variant = variant)
        )
    )
}

#' Coordinates of a Point Inside a Segment
#'
#' Returns plotting coordinates that fall inside the named segments, so that
#' lesions, biopsies or other findings can be placed by segment name rather
#' than by raw coordinates. The coordinates match plots drawn with
#' [colon_plot()] or [geom_colon()] at `x = 0, y = 0`.
#'
#' @param part A character vector of segment names. Names are matched
#'   leniently, as in [colon_plot()].
#' @param where Position along each segment, between 0 and 1, measured along
#'   the longer side of the segment's bounding box (left to right for wide
#'   segments, bottom to top for tall ones). Recycled to the length of `part`.
#'   The default, 0.5, is the middle.
#' @param variant A character string naming the anatomical layout.
#' @return A data frame with columns `part` (canonical name), `x` and `y`.
#' @export
#' @examples
#' lesions <- data.frame(
#'   part = c("Terminal Ileum", "Sigmoid", "Sigmoid"),
#'   type = c("Ulcer", "Polyp", "Stricture")
#' )
#' lesions <- cbind(
#'   lesions,
#'   colon_position(lesions$part, where = c(0.5, 0.2, 0.8), variant = "ibd")[c("x", "y")]
#' )
#' colon_plot(data.frame(part = "Terminal Ileum"), variant = "ibd") +
#'   ggplot2::geom_point(
#'     data = lesions,
#'     ggplot2::aes(x = x, y = y, shape = type),
#'     size = 3
#'   )
colon_position <- function(part, where = 0.5, variant = "standard") {
    if (!is.numeric(where) || anyNA(where) || any(where < 0 | where > 1)) {
        stop("`where` must contain numbers between 0 and 1.", call. = FALSE)
    }
    canonical <- .normalize_part(part, variant, warn = FALSE)
    parts <- unique(.get_variant(variant)$part)
    unknown <- unique(canonical[!canonical %in% parts])
    if (length(unknown) > 0L) {
        stop(
            "Unknown part(s) ", paste0("'", unknown, "'", collapse = ", "),
            " for variant '", variant, "'. Valid parts: ",
            paste(parts, collapse = ", "), ".",
            call. = FALSE
        )
    }
    n <- max(length(canonical), length(where))
    canonical <- rep_len(canonical, n)
    where <- rep_len(where, n)

    shapes <- colon_sf(variant)
    xy <- vapply(
        seq_len(n),
        function(i) {
            .point_in_part(
                sf::st_geometry(shapes)[[match(canonical[i], shapes$part)]],
                where[i]
            )
        },
        numeric(2)
    )
    data.frame(part = canonical, x = xy[1, ], y = xy[2, ])
}

# Midpoint of the longest chord of `poly` cut perpendicular to its longer
# bounding-box side, at fraction `where` (kept slightly inside the edges).
.point_in_part <- function(poly, where) {
    bb <- sf::st_bbox(poly)
    f <- 0.03 + 0.94 * where
    if (bb[["xmax"]] - bb[["xmin"]] >= bb[["ymax"]] - bb[["ymin"]]) {
        at <- bb[["xmin"]] + f * (bb[["xmax"]] - bb[["xmin"]])
        chord <- cbind(at, c(bb[["ymin"]] - 1, bb[["ymax"]] + 1))
    } else {
        at <- bb[["ymin"]] + f * (bb[["ymax"]] - bb[["ymin"]])
        chord <- cbind(c(bb[["xmin"]] - 1, bb[["xmax"]] + 1), at)
    }
    cut <- sf::st_intersection(
        sf::st_sfc(sf::st_linestring(chord)),
        sf::st_sfc(poly)
    )
    if (any(sf::st_geometry_type(cut) == "GEOMETRYCOLLECTION")) {
        cut <- sf::st_collection_extract(cut, "LINESTRING")
    }
    segments <- sf::st_cast(cut, "LINESTRING")
    longest <- segments[which.max(sf::st_length(segments))]
    sf::st_coordinates(sf::st_centroid(longest))[1, c("X", "Y")]
}

colon_grid <- function(grid_res = 500, variant = "standard") {
    colon_blueprint <- .get_variant(variant)
    expand.grid(
        x = seq(
            min(colon_blueprint$dx),
            max(colon_blueprint$dx),
            length.out = grid_res
        ),
        y = seq(
            min(colon_blueprint$dy),
            max(colon_blueprint$dy),
            length.out = grid_res
        )
    ) |>
        sf::st_as_sf(
            coords = c("x", "y"),
            crs = NA,
            remove = FALSE
        )
}
