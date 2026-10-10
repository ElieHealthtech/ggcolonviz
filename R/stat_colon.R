#' @title Colon Geometry Layer for ggplot2
#' @description This function defines a custom ggplot2 layer that allows users to plot
#'  the colon geometry based on the predefined blueprint. It takes in aesthetics for x, y, and part,
#'   and uses the StatColon ggproto object to compute the necessary data for plotting the colon structure.
#'   The resulting layer can be used in conjunction with other ggplot2 layers to create detailed
#'   visualizations of the colon.
#' @details The geom_colon function creates a layer that can be added to a ggplot2 plot.
#'   It requires the aesthetics x, y, and part to be specified, which correspond to the position
#'   and the specific part of the colon to be plotted.
#'
#'   Part names are matched leniently (case, spaces and common synonyms such
#'   as `"TI"` or `"sigmoid colon"` are accepted); rows whose part cannot be
#'   matched are dropped with a warning. See [colon_parts()] for the valid
#'   names and [list_colon_variants()] for the available variants.
#' @inheritParams ggplot2::layer
#' @param variant A character string specifying the variant of the colon blueprint to use.
#' @param na.rm If `FALSE`, the default, missing values are removed with a
#'   warning. If `TRUE`, missing values are silently removed.
#' @param \dots Additional parameters passed to the layer, such as fill, color, size, etc.
#' @return A ggplot2 layer that can be added to a plot to visualize the colon geometry.
#' @export
geom_colon <- function(
    mapping = NULL,
    data = NULL,
    variant = "standard",
    show.legend = NA,
    na.rm = FALSE,
    inherit.aes = TRUE,
    ...
) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = StatColon,
        geom = "polygon",
        position = "identity",
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(variant = variant, na.rm = na.rm, ...)
    )
}

StatColon <- ggplot2::ggproto(
    "StatColon",
    ggplot2::Stat,
    required_aes = c("x", "y", "part"),

    setup_data = function(data, params) {
        data$part <- .normalize_part(
            data$part,
            params$variant %||% "standard",
            caller = "geom_colon()"
        )
        data
    },

    compute_panel = function(data, scales, params, variant = "standard") {
        # Pull the correct blueprint from the registry
        colon_blueprint <- .get_variant(variant)

        # 1. Clean input data
        data <- as.data.frame(data)

        # 2. Build the expanded polygon list
        poly_list <- lapply(seq_len(nrow(data)), function(i) {
            row <- data[i, ]
            coords <- colon_blueprint[
                colon_blueprint$part == as.character(row$part),
            ]

            if (nrow(coords) == 0) {
                return(NULL)
            }

            # 3. Create the data frame for this specific part
            res <- data.frame(
                x = row$x + coords$dx,
                y = row$y + coords$dy,
                # IMPORTANT: Use a simple integer for group
                group = i,
                stringsAsFactors = FALSE,
                row.names = NULL
            )

            # 4. Attach other aesthetics (fill, alpha, etc.)
            other_aes <- row[,
                setdiff(names(row), c("x", "y", "group")),
                drop = FALSE
            ]
            cbind(
                res,
                other_aes[rep(1, nrow(res)), , drop = FALSE],
                row.names = NULL
            )
        })

        # Combine everything back together
        final_df <- do.call(rbind, poly_list)

        if (is.null(final_df) || nrow(final_df) == 0L) {
            return(data.frame(
                x = numeric(0),
                y = numeric(0),
                group = integer(0)
            ))
        }

        # Final safety check: Ensure group is a clean integer factor
        final_df$group <- as.integer(as.factor(final_df$group))

        return(final_df)
    }
)
