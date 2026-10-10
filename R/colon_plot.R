#' Plot Segment-Level Data on the Colon in One Call
#'
#' A shortcut for the most common use: one row per segment (and optionally
#' per subject or visit), coloured by a score or category. It builds the
#' [geom_colon()] and [geom_colon_label()] layers, adds the fixed aspect ratio
#' and an empty theme, and returns an ordinary ggplot that you can extend with
#' `+ scale_fill_*()`, `+ labs()`, etc.
#'
#' Segment names in the `part` column are matched leniently: case, spaces and
#' punctuation are ignored and common synonyms such as `"TI"`,
#' `"terminal ileum"`, `"caecum"` or `"sigmoid colon"` are accepted. Rows that
#' cannot be matched are dropped with a warning listing the valid names (see
#' [colon_parts()]).
#'
#' @param data A data frame with one row per segment and, if `facet` is used,
#'   per panel.
#' @param part The column holding segment names. Defaults to a column named
#'   `part`.
#' @param fill Optional column used to colour the segments, for example a
#'   score or a disease stage. Numeric columns get a light-to-dark red scale
#'   (higher values are darker); add your own `scale_fill_*()` to change it.
#' @param facet Optional column to draw one panel per value, for example
#'   `subject` or `visit`.
#' @param variant A character string naming the anatomical layout; see
#'   [list_colon_variants()]. Use `"ibd"` for the whole gastrointestinal tract.
#' @param label If `TRUE`, the default, segment names are written on the
#'   plot.
#' @param show_all If `TRUE`, the default, segments absent from `data` are
#'   still drawn in `missing_fill`, so the anatomy is always complete.
#' @param missing_fill Fill colour of segments absent from `data`.
#' @param outline Colour of segment outlines.
#' @param linewidth Width of segment outlines.
#' @param label_size Text size of the segment names.
#' @return A ggplot object.
#' @seealso [colon_map()] to see every segment of a variant,
#'   [colon_position()] to place lesions on a segment.
#' @export
#' @examples
#' scores <- data.frame(
#'   part = c("Terminal ileum", "Cecum", "Ascending", "Transverse",
#'            "Descending", "Sigmoid", "Rectum"),
#'   score = c(3, 2, 1, 0, 0, 1, 2)
#' )
#' colon_plot(scores, fill = score, variant = "ibd")
#'
#' # Several visits side by side
#' visits <- rbind(
#'   data.frame(visit = "Week 0", part = c("TI", "Rectum"), score = c(3, 2)),
#'   data.frame(visit = "Week 12", part = c("TI", "Rectum"), score = c(1, 0))
#' )
#' colon_plot(visits, fill = score, facet = visit)
colon_plot <- function(
    data,
    part = part,
    fill = NULL,
    facet = NULL,
    variant = "standard",
    label = TRUE,
    show_all = TRUE,
    missing_fill = "grey90",
    outline = "grey30",
    linewidth = 0.4,
    label_size = 2.5
) {
    if (!is.data.frame(data)) {
        stop("`data` must be a data frame.", call. = FALSE)
    }
    part_quo <- rlang::enquo(part)
    fill_quo <- rlang::enquo(fill)
    facet_quo <- rlang::enquo(facet)

    all_parts <- unique(.get_variant(variant)$part)
    plot_data <- data
    plot_data$.colon_part <- .normalize_part(
        rlang::eval_tidy(part_quo, data),
        variant,
        caller = "colon_plot()"
    )
    plot_data$.colon_x <- 0
    plot_data$.colon_y <- 0
    plot_data <- plot_data[plot_data$.colon_part %in% all_parts, , drop = FALSE]

    anatomy <- data.frame(.colon_x = 0, .colon_y = 0, .colon_part = all_parts)
    position <- ggplot2::aes(x = .colon_x, y = .colon_y, part = .colon_part)

    p <- ggplot2::ggplot()
    if (show_all) {
        p <- p +
            geom_colon(
                data = anatomy,
                mapping = position,
                variant = variant,
                fill = missing_fill,
                colour = outline,
                linewidth = linewidth,
                inherit.aes = FALSE
            )
    }
    if (rlang::quo_is_null(fill_quo)) {
        p <- p +
            geom_colon(
                data = plot_data,
                mapping = position,
                variant = variant,
                fill = "lightblue",
                colour = outline,
                linewidth = linewidth,
                inherit.aes = FALSE
            )
    } else {
        p <- p +
            geom_colon(
                data = plot_data,
                mapping = ggplot2::aes(
                    x = .colon_x,
                    y = .colon_y,
                    part = .colon_part,
                    fill = !!fill_quo
                ),
                variant = variant,
                colour = outline,
                linewidth = linewidth,
                inherit.aes = FALSE
            )
    }
    if (label) {
        p <- p +
            geom_colon_label(
                data = if (show_all) anatomy else plot_data,
                mapping = position,
                variant = variant,
                size = label_size,
                inherit.aes = FALSE
            )
    }
    if (
        !rlang::quo_is_null(fill_quo) &&
            is.numeric(rlang::eval_tidy(fill_quo, plot_data))
    ) {
        p <- p + ggplot2::scale_fill_gradient(low = "#FEE8C8", high = "#B30000")
    }
    if (!rlang::quo_is_null(facet_quo)) {
        p <- p + ggplot2::facet_wrap(ggplot2::vars(!!facet_quo))
    }
    p + ggplot2::coord_fixed() + ggplot2::theme_void()
}

#' Reference Map of a Colon Variant
#'
#' Draws every segment of a variant with its name, coloured by anatomical
#' region. Run it first to see which segments exist and how to name them in
#' your data.
#'
#' @inheritParams colon_plot
#' @return A ggplot object.
#' @seealso [colon_parts()] for the same information as a table.
#' @export
#' @examples
#' colon_map()
#' colon_map("ibd")
colon_map <- function(variant = "standard", label_size = 2.2) {
    region_colours <- c(
        "Upper GI" = "#F4CAE4",
        "Small bowel" = "#FDDAAC",
        "Colon" = "#B3CDE3",
        "Rectum" = "#CCEBC5",
        "Perianal" = "#DECBE4"
    )
    info <- colon_parts(variant)
    info$region <- factor(info$region, levels = names(region_colours))
    colon_plot(
        info,
        part = part,
        fill = region,
        variant = variant,
        show_all = FALSE,
        label_size = label_size
    ) +
        ggplot2::scale_fill_manual(
            name = "Region",
            values = region_colours,
            na.value = "grey90",
            drop = TRUE
        )
}
