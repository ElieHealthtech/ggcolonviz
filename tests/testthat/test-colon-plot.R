scores <- data.frame(
    part = c("Ileum", "Cecum", "Sigmoid"),
    score = c(3, 1, 2)
)

# Number of polygon groups drawn by layer `i`
n_groups <- function(p, i) {
    length(unique(ggplot2::layer_data(p, i)$group))
}

# ── colon_plot() ──────────────────────────────────────────────────────────────

test_that("colon_plot() returns a ggplot", {
    expect_s3_class(colon_plot(scores, fill = score), "ggplot")
})

test_that("show_all draws the full anatomy beneath the data", {
    p <- colon_plot(scores, fill = score)
    expect_equal(n_groups(p, 1), 7)
    expect_equal(n_groups(p, 2), 3)
    expect_true(all(ggplot2::layer_data(p, 1)$fill == "grey90"))
})

test_that("show_all = FALSE draws only the data", {
    p <- colon_plot(scores, fill = score, show_all = FALSE)
    expect_equal(n_groups(p, 1), 3)
    expect_equal(nrow(ggplot2::layer_data(p, 2)), 3)
})

test_that("label = FALSE drops the label layer", {
    expect_length(colon_plot(scores, label = FALSE)$layers, 2)
    expect_length(colon_plot(scores, label = FALSE, show_all = FALSE)$layers, 1)
})

test_that("without fill the segments get a constant colour", {
    p <- colon_plot(scores, show_all = FALSE)
    expect_true(all(ggplot2::layer_data(p, 1)$fill == "lightblue"))
})

test_that("numeric fill uses darker colours for higher values", {
    p <- colon_plot(scores, fill = score, show_all = FALSE)
    d <- ggplot2::layer_data(p, 1)
    fills <- tapply(d$fill, d$group, unique)
    expect_false(any(is.na(fills)))
    expect_equal(length(unique(fills)), 3)
})

test_that("the part column can have any name", {
    df <- data.frame(segment = c("TI", "Rectum"), stage = c("A", "B"))
    p <- colon_plot(df, part = segment, fill = stage, show_all = FALSE)
    expect_equal(n_groups(p, 1), 2)
})

test_that("unknown parts warn once and are dropped", {
    df <- data.frame(part = c("Cecum", "Spleen"))
    expect_warning(p <- colon_plot(df), "Spleen")
    expect_no_warning(d <- ggplot2::layer_data(p, 2))
    expect_equal(length(unique(d$group)), 1)
})

test_that("facet draws one panel per value with the full anatomy in each", {
    df <- rbind(
        data.frame(visit = "Week 0", part = c("Ileum", "Rectum"), score = c(3, 2)),
        data.frame(visit = "Week 12", part = "Rectum", score = 0)
    )
    p <- colon_plot(df, fill = score, facet = visit)
    background <- ggplot2::layer_data(p, 1)
    expect_equal(length(unique(background$PANEL)), 2)
    expect_equal(nrow(unique(background[c("PANEL", "group")])), 14)
})

test_that("colon_plot() works with the ibd and sescd variants", {
    df <- data.frame(part = montreal_parts("L3"), involved = "yes")
    p <- colon_plot(df, fill = involved, variant = "ibd")
    expect_equal(n_groups(p, 1), 16)
    expect_equal(n_groups(p, 2), length(montreal_parts("L3")))
    expect_s3_class(colon_plot(data.frame(part = "Left"), variant = "sescd"), "ggplot")
})

test_that("colon_plot() rejects non data frames", {
    expect_error(colon_plot(c("Cecum")), "data frame")
})

# ── colon_map() ───────────────────────────────────────────────────────────────

test_that("colon_map() draws and labels every part", {
    for (v in c("standard", "sescd", "ibd")) {
        p <- colon_map(v)
        n <- nrow(colon_parts(v))
        expect_equal(n_groups(p, 1), n)
        expect_equal(nrow(ggplot2::layer_data(p, 2)), n)
    }
})

# ── colon_position() ─────────────────────────────────────────────────────────

test_that("colon_position() returns points inside their part", {
    skip_if_not_installed("sf")
    for (v in c("standard", "ibd")) {
        shapes <- colon_sf(v)
        parts <- colon_parts(v)$part
        for (w in c(0, 0.5, 1)) {
            pos <- colon_position(parts, where = w, variant = v)
            pts <- sf::st_as_sf(pos, coords = c("x", "y"))
            inside <- vapply(
                seq_len(nrow(pos)),
                function(i) {
                    as.numeric(sf::st_distance(
                        pts[i, ],
                        shapes[shapes$part == pos$part[i], ]
                    )) == 0
                },
                logical(1)
            )
            expect_true(all(inside), label = paste(v, w))
        }
    }
})

test_that("colon_position() moves along the segment with `where`", {
    pos <- colon_position("Sigmoid", where = c(0.1, 0.9))
    expect_lt(pos$x[1], pos$x[2])
    pos <- colon_position("Ascending", where = c(0.1, 0.9))
    expect_lt(pos$y[1], pos$y[2])
})

test_that("colon_position() returns canonical names and recycles", {
    pos <- colon_position("TI", where = c(0.2, 0.8), variant = "ibd")
    expect_equal(pos$part, c("Terminal Ileum", "Terminal Ileum"))
    expect_named(pos, c("part", "x", "y"))
})

test_that("colon_position() validates its input", {
    expect_error(colon_position("Spleen"), "Unknown part")
    expect_error(colon_position("Cecum", where = 2), "between 0 and 1")
})
