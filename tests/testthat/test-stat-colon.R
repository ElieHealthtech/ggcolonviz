make_plot <- function(parts, x = 0, y = 0, variant = "standard", ...) {
    df <- data.frame(
        x = x,
        y = y,
        part = parts,
        stringsAsFactors = FALSE
    )
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, part = part)) +
        geom_colon(variant = variant, ...)
    p
}

test_that("geom_colon() returns a Layer object", {
    expect_true(inherits(geom_colon(), "Layer"))
})

test_that("single part produces polygon rows", {
    p <- make_plot("Cecum")
    d <- ggplot2::layer_data(p)
    expect_gt(nrow(d), 0)
})

test_that("all 7 standard parts produce 7 polygon groups", {
    parts <- c(
        "Ileum",
        "Cecum",
        "Ascending",
        "Transverse",
        "Descending",
        "Sigmoid",
        "Rectum"
    )
    p <- make_plot(parts)
    d <- ggplot2::layer_data(p)
    expect_equal(length(unique(d$group)), 7)
})

test_that("all 5 sescd parts produce 5 polygon groups", {
    parts <- c("Ileum", "Transverse", "Rectum", "Left", "Right")
    p <- make_plot(parts, variant = "sescd")
    d <- ggplot2::layer_data(p)
    expect_equal(length(unique(d$group)), 5)
})

test_that("x offset is applied correctly", {
    p_zero <- make_plot("Cecum", x = 0)
    p_off <- make_plot("Cecum", x = 5)
    d_zero <- ggplot2::layer_data(p_zero)
    d_off <- ggplot2::layer_data(p_off)
    expect_equal(d_off$x - d_zero$x, rep(5, nrow(d_zero)), tolerance = 1e-9)
})

test_that("y offset is applied correctly", {
    p_zero <- make_plot("Cecum", y = 0)
    p_off <- make_plot("Cecum", y = 10)
    d_zero <- ggplot2::layer_data(p_zero)
    d_off <- ggplot2::layer_data(p_off)
    expect_equal(d_off$y - d_zero$y, rep(10, nrow(d_zero)), tolerance = 1e-9)
})

test_that("unknown part is dropped with a warning, without error", {
    df <- data.frame(
        x = 0,
        y = 0,
        part = c("Cecum", "BadPart"),
        stringsAsFactors = FALSE
    )
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, part = part)) +
        geom_colon()
    expect_warning(d <- ggplot2::layer_data(p), "unknown part.*BadPart")
    expect_equal(length(unique(d$group)), 1)
})

test_that("multiple patients with same part produce separate polygon groups", {
    df <- data.frame(
        x = c(0, 10),
        y = c(0, 0),
        part = c("Cecum", "Cecum"),
        stringsAsFactors = FALSE
    )
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, part = part)) +
        geom_colon()
    d <- ggplot2::layer_data(p)
    expect_equal(length(unique(d$group)), 2)
})

test_that("group column is integer", {
    p <- make_plot("Cecum")
    d <- ggplot2::layer_data(p)
    expect_true(is.integer(d$group))
})

test_that("fill aesthetic is passed through to layer data", {
    df <- data.frame(x = 0, y = 0, part = "Cecum", stringsAsFactors = FALSE)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, part = part)) +
        geom_colon(fill = "red")
    d <- ggplot2::layer_data(p)
    expect_true(all(d$fill == "red"))
})

test_that("no rows are returned for a data frame with only unknown parts", {
    df <- data.frame(x = 0, y = 0, part = "NotAPart", stringsAsFactors = FALSE)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, part = part)) +
        geom_colon()
    expect_warning(d <- ggplot2::layer_data(p), "NotAPart")
    expect_equal(nrow(d), 0)
})
