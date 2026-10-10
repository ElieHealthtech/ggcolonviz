skip_if_not_installed("sf")

ibd_parts <- c(
    "Esophagus",
    "Stomach",
    "Duodenum",
    "Jejunum",
    "Proximal Ileum",
    "Terminal Ileum",
    "Ileocecal Valve",
    "Cecum",
    "Appendix",
    "Ascending",
    "Transverse",
    "Descending",
    "Sigmoid",
    "Rectum",
    "Anal Canal",
    "Perianal"
)

# Distance between two parts of the "ibd" sf object
part_distance <- function(shapes, a, b) {
    as.numeric(sf::st_distance(
        shapes[shapes$part == a, ],
        shapes[shapes$part == b, ]
    ))
}

test_that("ibd variant is registered", {
    expect_true("ibd" %in% list_colon_variants())
})

test_that("ibd variant has the 16 expected parts", {
    expect_setequal(unique(ggcolonviz:::.get_variant("ibd")$part), ibd_parts)
})

test_that("ibd variant has the blueprint columns and no NA coordinates", {
    bp <- ggcolonviz:::.get_variant("ibd")
    expect_named(bp, c("dx", "dy", "part"))
    expect_false(anyNA(bp))
    counts <- tapply(bp$part, bp$part, length)
    expect_true(all(counts >= 3))
})

test_that("ibd colon segments are identical to the standard ones", {
    std <- ggcolonviz:::.get_variant("standard")
    ibd <- ggcolonviz:::.get_variant("ibd")
    for (p in c("Cecum", "Ascending", "Transverse", "Descending", "Sigmoid", "Rectum")) {
        expect_equal(
            ibd[ibd$part == p, ],
            std[std$part == p, ],
            ignore_attr = TRUE
        )
    }
})

test_that("each ibd part drawn on its own is a valid polygon", {
    bp <- ggcolonviz:::.get_variant("ibd")
    for (p in ibd_parts) {
        m <- as.matrix(bp[bp$part == p, c("dx", "dy")])
        poly <- sf::st_polygon(list(rbind(m, m[1, ])))
        expect_true(sf::st_is_valid(poly), label = p)
    }
})

test_that("colon_sf('ibd') returns 16 valid polygons", {
    shapes <- colon_sf("ibd")
    expect_equal(nrow(shapes), 16)
    expect_true(all(sf::st_is_valid(shapes)))
    expect_equal(unique(as.character(sf::st_geometry_type(shapes))), "POLYGON")
})

test_that("no two ibd parts overlap", {
    shapes <- colon_sf("ibd")
    pairs <- suppressWarnings(sf::st_intersection(shapes, shapes))
    pairs <- pairs[pairs$part != pairs$part.1, ]
    expect_true(all(as.numeric(sf::st_area(pairs)) < 1e-9))
})

test_that("the small bowel connects through the ileocecal valve to the cecum", {
    shapes <- colon_sf("ibd")
    chain <- c(
        "Jejunum",
        "Proximal Ileum",
        "Terminal Ileum",
        "Ileocecal Valve",
        "Cecum"
    )
    for (i in seq_len(length(chain) - 1L)) {
        expect_lt(part_distance(shapes, chain[i], chain[i + 1L]), 1e-9)
    }
})

test_that("upper GI parts are connected and the anal canal meets the rectum", {
    shapes <- colon_sf("ibd")
    expect_lt(part_distance(shapes, "Esophagus", "Stomach"), 1e-9)
    expect_lt(part_distance(shapes, "Stomach", "Duodenum"), 1e-9)
    expect_lt(part_distance(shapes, "Appendix", "Cecum"), 1e-9)
    expect_lt(part_distance(shapes, "Rectum", "Anal Canal"), 1e-9)
})

test_that("geom_colon() draws every ibd part", {
    df <- data.frame(x = 0, y = 0, part = ibd_parts)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, part = part)) +
        geom_colon(variant = "ibd")
    expect_equal(length(unique(ggplot2::layer_data(p)$group)), 16)
})

test_that("ibd labels with an anchor fall inside or next to their part", {
    shapes <- colon_sf("ibd")
    df <- data.frame(x = 0, y = 0, part = ibd_parts)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, part = part)) +
        geom_colon_label(variant = "ibd")
    d <- ggplot2::layer_data(p)
    expect_equal(nrow(d), 16)
    for (i in seq_len(nrow(d))) {
        pt <- sf::st_sfc(sf::st_point(c(d$x[i], d$y[i])))
        dist <- as.numeric(sf::st_distance(pt, shapes[shapes$part == d$label[i], ]))
        expect_lt(dist, 0.2, label = d$label[i])
    }
})

test_that("colon_heatmap_grid() works for the ibd variant", {
    grid <- colon_heatmap_grid(grid_res = 60, variant = "ibd")
    expect_s3_class(grid, "sf")
    expect_true(all(grid$part %in% ibd_parts))
})

test_that("re-registering a variant drops its label anchors", {
    env <- getFromNamespace(".ggcolonviz_env", "ggcolonviz")
    old_bp <- env$variants$ibd
    old_anchors <- env$label_anchors$ibd
    on.exit({
        env$variants$ibd <- old_bp
        env$label_anchors$ibd <- old_anchors
    }, add = TRUE)
    register_colon_variant("ibd", old_bp)
    expect_null(env$label_anchors$ibd)
})
