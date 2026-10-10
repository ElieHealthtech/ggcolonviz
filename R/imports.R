#' `ggcolonviz` Package
#'
#' `ggcolonviz` implements a ggplot2 extension for visualizing the colon.
#'
#' @aliases ggcolonviz-package
"_PACKAGE"

utils::globalVariables(c("part", "geometry", "region", ".colon_x", ".colon_y", ".colon_part"))

`%||%` <- function(x, y) if (is.null(x)) y else x

#' @importFrom sf st_as_sf st_combine st_cast st_make_valid st_intersection st_coordinates
#' @importFrom dplyr group_by summarise mutate case_when bind_rows
#' @importFrom ggplot2 layer Stat ggproto
NULL
