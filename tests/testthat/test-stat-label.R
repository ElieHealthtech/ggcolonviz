make_label_plot <- function(
    parts,
    x = 0,
    y = 0,
    variant = "standard",
    mapping = NULL,
    ...
) {
    df <- data.frame(
        x = x,
        y = y,
        part = parts,
        stringsAsFactors = FALSE
    )
    ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, part = part)) +
        geom_colon_label(mapping = mapping, variant = variant, ...)
}

test_that("geom_colon_label() returns a Layer object", {
    expect_true(inherits(geom_colon_label(), "Layer"))
})

test_that("label defaults to the part name", {
    p <- make_label_plot("Cecum")
    d <- ggplot2::layer_data(p)
    expect_equal(d$label, "Cecum")
})

test_that("custom label aes overrides the default part name", {
    df <- data.frame(
        x = 0,
        y = 0,
        part = "Cecum",
        label = "My Label",
        stringsAsFactors = FALSE
    )
    p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = x, y = y, part = part, label = label)
    ) +
        geom_colon_label()
    d <- ggplot2::layer_data(p)
    expect_equal(d$label, "My Label")
})

test_that("label centroid x is within the part's dx bounding box", {
    bp <- ggcolonviz:::.get_variant("standard")
    cecum_coords <- bp[bp$part == "Cecum", ]
    p <- make_label_plot("Cecum")
    d <- ggplot2::layer_data(p)
    expect_gte(d$x, min(cecum_coords$dx))
    expect_lte(d$x, max(cecum_coords$dx))
})

test_that("label centroid y is within the part's dy bounding box", {
    bp <- ggcolonviz:::.get_variant("standard")
    cecum_coords <- bp[bp$part == "Cecum", ]
    p <- make_label_plot("Cecum")
    d <- ggplot2::layer_data(p)
    # dy range + possible offset tolerance
    expect_gte(d$y, min(cecum_coords$dy) - 0.5)
    expect_lte(d$y, max(cecum_coords$dy) + 0.5)
})

test_that("Ascending label has angle = 90", {
    p <- make_label_plot("Ascending")
    d <- ggplot2::layer_data(p)
    expect_equal(d$angle, 90)
})

test_that("Descending label has angle = 90", {
    p <- make_label_plot("Descending")
    d <- ggplot2::layer_data(p)
    expect_equal(d$angle, 90)
})

test_that("Left label has angle = 90", {
    p <- make_label_plot("Left", variant = "sescd")
    d <- ggplot2::layer_data(p)
    expect_equal(d$angle, 90)
})

test_that("Right label has angle = 90", {
    p <- make_label_plot("Right", variant = "sescd")
    d <- ggplot2::layer_data(p)
    expect_equal(d$angle, 90)
})

test_that("Transverse label has angle = 0", {
    p <- make_label_plot("Transverse")
    d <- ggplot2::layer_data(p)
    expect_equal(d$angle, 0)
})

test_that("Rectum label has angle = 0", {
    p <- make_label_plot("Rectum")
    d <- ggplot2::layer_data(p)
    expect_equal(d$angle, 0)
})

test_that("Sigmoid label has angle = 0", {
    p <- make_label_plot("Sigmoid")
    d <- ggplot2::layer_data(p)
    expect_equal(d$angle, 0)
})

test_that("all 7 standard parts produce one label row each", {
    parts <- c(
        "Ileum",
        "Cecum",
        "Ascending",
        "Transverse",
        "Descending",
        "Sigmoid",
        "Rectum"
    )
    p <- make_label_plot(parts)
    d <- ggplot2::layer_data(p)
    expect_equal(nrow(d), 7)
})

test_that("all 5 sescd parts produce one label row each", {
    parts <- c("Ileum", "Transverse", "Rectum", "Left", "Right")
    p <- make_label_plot(parts, variant = "sescd")
    d <- ggplot2::layer_data(p)
    expect_equal(nrow(d), 5)
})

test_that("unknown part is dropped from labels with a warning", {
    df <- data.frame(
        x = 0,
        y = 0,
        part = c("Cecum", "BadPart"),
        stringsAsFactors = FALSE
    )
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, part = part)) +
        geom_colon_label()
    expect_warning(d <- ggplot2::layer_data(p), "unknown part.*BadPart")
    expect_equal(nrow(d), 1)
})

test_that("x offset is applied to label position", {
    p_zero <- make_label_plot("Cecum", x = 0)
    p_off <- make_label_plot("Cecum", x = 7)
    d_zero <- ggplot2::layer_data(p_zero)
    d_off <- ggplot2::layer_data(p_off)
    expect_equal(d_off$x - d_zero$x, 7, tolerance = 1e-9)
})

test_that("y offset is applied to label position", {
    p_zero <- make_label_plot("Cecum", y = 0)
    p_off <- make_label_plot("Cecum", y = 3)
    d_zero <- ggplot2::layer_data(p_zero)
    d_off <- ggplot2::layer_data(p_off)
    expect_equal(d_off$y - d_zero$y, 3, tolerance = 1e-9)
})
