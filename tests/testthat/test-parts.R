# ── colon_parts() ─────────────────────────────────────────────────────────────

test_that("colon_parts() lists every part of the variant in order", {
    for (v in c("standard", "sescd", "ibd")) {
        info <- colon_parts(v)
        expect_named(info, c("part", "region", "montreal"))
        expect_equal(info$part, unique(ggcolonviz:::.get_variant(v)$part))
    }
})

test_that("built-in parts all have a region", {
    for (v in c("standard", "sescd", "ibd")) {
        expect_false(anyNA(colon_parts(v)$region))
    }
})

test_that("colon_parts() gives NA metadata for unknown user parts", {
    env <- getFromNamespace(".ggcolonviz_env", "ggcolonviz")
    on.exit(env$variants$test_parts <- NULL, add = TRUE)
    register_colon_variant(
        "test_parts",
        data.frame(dx = c(0, 1, 1), dy = c(0, 0, 1), part = "Mystery")
    )
    info <- colon_parts("test_parts")
    expect_equal(info$part, "Mystery")
    expect_true(is.na(info$region))
})

# ── .normalize_part() ─────────────────────────────────────────────────────────

test_that("part names are matched ignoring case, spaces and punctuation", {
    norm <- ggcolonviz:::.normalize_part
    expect_equal(norm(c("cecum", " SIGMOID ", "ascending"), "standard"),
                 c("Cecum", "Sigmoid", "Ascending"))
    expect_equal(norm("terminal-ileum", "ibd"), "Terminal Ileum")
})

test_that("synonyms map to the parts of the requested variant", {
    norm <- ggcolonviz:::.normalize_part
    expect_equal(norm("TI", "standard"), "Ileum")
    expect_equal(norm("TI", "ibd"), "Terminal Ileum")
    expect_equal(norm("Ileum", "ibd"), "Terminal Ileum")
    expect_equal(norm("caecum", "ibd"), "Cecum")
    expect_equal(norm("sigmoid colon", "standard"), "Sigmoid")
    expect_equal(norm("right colon", "sescd"), "Right")
    expect_equal(norm("ICV", "ibd"), "Ileocecal Valve")
    expect_equal(norm("anus", "ibd"), "Anal Canal")
    expect_equal(norm("oesophagus", "ibd"), "Esophagus")
})

test_that("unknown parts are returned unchanged with one warning", {
    norm <- ggcolonviz:::.normalize_part
    expect_warning(
        out <- norm(c("Cecum", "Spleen", "Spleen"), "standard"),
        "unknown part.*'Spleen'"
    )
    expect_equal(out, c("Cecum", "Spleen", "Spleen"))
    expect_no_warning(norm("Spleen", "standard", warn = FALSE))
})

test_that("parts outside the variant are reported as unknown", {
    expect_warning(
        ggcolonviz:::.normalize_part("Stomach", "standard"),
        "Stomach"
    )
})

test_that("NA parts are kept as NA without a warning", {
    expect_no_warning(out <- ggcolonviz:::.normalize_part(c(NA, "Cecum")))
    expect_equal(out, c(NA, "Cecum"))
})

test_that("geom_colon() accepts synonyms", {
    df <- data.frame(x = 0, y = 0, part = c("TI", "caecum"))
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, part = part)) +
        geom_colon()
    expect_no_warning(d <- ggplot2::layer_data(p))
    expect_equal(length(unique(d$group)), 2)
})

# ── montreal_parts() ──────────────────────────────────────────────────────────

test_that("Montreal Crohn's disease locations map to the ibd parts", {
    expect_equal(
        montreal_parts("L1"),
        c("Terminal Ileum", "Ileocecal Valve")
    )
    expect_equal(
        montreal_parts("L2"),
        c("Cecum", "Ascending", "Transverse", "Descending", "Sigmoid", "Rectum")
    )
    expect_equal(montreal_parts("L3"), union(montreal_parts("L1"), montreal_parts("L2")))
    expect_equal(
        montreal_parts("L4"),
        c("Esophagus", "Stomach", "Duodenum", "Jejunum", "Proximal Ileum")
    )
    expect_equal(montreal_parts("p"), c("Anal Canal", "Perianal"))
})

test_that("Montreal ulcerative colitis extents map to the ibd parts", {
    expect_equal(montreal_parts("E1"), "Rectum")
    expect_equal(montreal_parts("E2"), c("Descending", "Sigmoid", "Rectum"))
    expect_equal(
        montreal_parts("E3"),
        c("Cecum", "Ascending", "Transverse", "Descending", "Sigmoid", "Rectum")
    )
})

test_that("several codes return the union in anatomical order", {
    expect_equal(
        montreal_parts(c("p", "L1")),
        c("Terminal Ileum", "Ileocecal Valve", "Anal Canal", "Perianal")
    )
})

test_that("montreal_parts() adapts to coarser variants", {
    expect_equal(montreal_parts("L1", "standard"), "Ileum")
    expect_equal(montreal_parts("E2", "sescd"), c("Rectum", "Left"))
    expect_equal(montreal_parts("L4", "standard"), character(0))
})

test_that("montreal_parts() rejects unknown codes", {
    expect_error(montreal_parts("L5"), "Unknown Montreal code")
})
