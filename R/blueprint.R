# 1. Terminal Ileum
ileum <- data.frame(
    dx = c(-2.75, -1.75, -2, -2.5),
    dy = c(0, 0, 0.5, 0.5),
    part = "Ileum"
)

# 2. Cecum & Ascending (Left Side)
cecum <- data.frame(
    dx = c(-2.5, -3.5, -3.25, -2.75),
    dy = c(0.5, 0.5, 0.0, 0.0),
    part = "Cecum"
)

#3. Ascending (Left Side - Slanted Top)

ascending <- data.frame(
    dx = c(-2.5, -3.5, -3.5, -2.5),
    dy = c(0.5, 0.5, 3.5, 2.75),
    part = "Ascending"
)

# 4. Transverse (Fixed Alignment - 102 points total)
tx_top <- seq(-3.5, 0.5, length.out = 50)
tx_bot <- seq(-0.5, -2.5, length.out = 50)

ty_top <- 3.5 +
    (4.5 - 3.5) * seq(0, 1, length.out = 50) +
    sin(seq(0, pi, length.out = 50)) * 0.6

ty_bot <- seq(3.7, 2.75, length.out = 50) +
    sin(seq(0, pi, length.out = 50)) * 0.6


transverse <- data.frame(
    dx = c(tx_top, 0.5, -0.5, tx_bot),
    dy = c(ty_top, 4.5, 3.7, ty_bot),
    part = "Transverse"
)

# 5. Descending (Right Side - Slanted Top)
descending <- data.frame(
    dx = c(-0.5, 0.5, 0.5, -0.5),
    dy = c(3.7, 4.5, 1.0, 1.0),
    part = "Descending"
)

# 6. Sigmoid & Rectum (Right Side)
sigmoid <- data.frame(
    dx = c(-1.5, 0.5, 0.5, -1.5),
    dy = c(1, 1, 0, 0),
    part = "Sigmoid"
)
rectum <- data.frame(
    dx = c(-1.5, -0.5, -0.75, -1.25),
    dy = c(0, 0, -1, -1),
    part = "Rectum"
)

left_side_merged <- data.frame(
    dx = c(-0.5, 0.5, 0.5, -1.5, -1.5, -0.5, -0.5), # Vertical walls at -0.5 and 0.5
    dy = c(3.7, 4.5, 0.0, 0.0, 1.0, 1.0, 3.7), # Added the 4.5 top-right and 3.7 top-left
    part = "Left"
)

right_side_merged <- data.frame(
    dx = c(-3.25, -3.5, -3.5, -2.50, -2.5, -2.75),
    dy = c(0.00, 0.5, 3.5, 2.75, 0.5, 0.00),
    part = "Right"
)

# 7. Full gastrointestinal tract for inflammatory bowel disease ("ibd")
# The colon re-uses the standard polygons above; the small bowel is drawn as a
# serpentine loop inside the colonic frame and the upper GI tract above the
# transverse colon. Tubular parts are built with .band().

# Polygon outlining a band of constant `width` drawn along the polyline
# (x, y), with mitred corners and flat ends.
.band <- function(x, y, width, part) {
    n <- length(x)
    seg_len <- sqrt(diff(x)^2 + diff(y)^2)
    # Left-hand unit normal of each segment
    nx <- -diff(y) / seg_len
    ny <- diff(x) / seg_len
    in_x <- c(nx[1], nx)
    in_y <- c(ny[1], ny)
    out_x <- c(nx, nx[n - 1])
    out_y <- c(ny, ny[n - 1])
    # Mitre direction at each vertex, scaled to keep the band width constant
    mx <- in_x + out_x
    my <- in_y + out_y
    m_len <- sqrt(mx^2 + my^2)
    mx <- mx / m_len
    my <- my / m_len
    scale <- (width / 2) / (mx * in_x + my * in_y)
    data.frame(
        dx = c(x + mx * scale, rev(x - mx * scale)),
        dy = c(y + my * scale, rev(y - my * scale)),
        part = part
    )
}

esophagus <- .band(c(-0.6, -0.6), c(8.1, 6.9), width = 0.3, part = "Esophagus")

stomach <- data.frame(
    dx = c(-0.75, -0.45, -0.2, 0.15, 0.35, 0.4, 0.25, -0.1, -0.6, -1.1, -1.5,
           -1.5, -1.1, -0.75, -0.65),
    dy = c(6.9, 6.9, 7.05, 6.95, 6.65, 6.25, 5.85, 5.6, 5.5, 5.55, 5.7,
           6.0, 5.9, 6.05, 6.4),
    part = "Stomach"
)

# C-loop from the pylorus; the duodeno-jejunal flexure passes behind the
# transverse colon, shown as a gap before the jejunum.
duodenum <- .band(
    c(-1.5, -2.25, -2.25, -0.95),
    c(5.85, 5.85, 5.1, 5.1),
    width = 0.3,
    part = "Duodenum"
)

jejunum <- .band(
    c(-0.75, -0.75, -2.0, -2.0, -0.75, -0.75, -1.25),
    c(3.15, 2.9, 2.9, 2.5, 2.5, 2.1, 2.1),
    width = 0.22,
    part = "Jejunum"
)

proximal_ileum <- .band(
    c(-1.25, -2.3, -2.3, -0.75, -0.75, -1.35),
    c(2.1, 2.1, 1.7, 1.7, 1.3, 1.3),
    width = 0.22,
    part = "Proximal Ileum"
)

terminal_ileum <- .band(
    c(-1.35, -1.95, -1.95, -2.35),
    c(1.3, 1.3, 0.25, 0.25),
    width = 0.22,
    part = "Terminal Ileum"
)

# Fills the gap between the terminal ileum and the medial wall of the cecum,
# which runs from (-2.75, 0) to (-2.5, 0.5).
ileocecal_valve <- data.frame(
    dx = c(-2.75 + 0.14 / 2, -2.35, -2.35, -2.75 + 0.36 / 2),
    dy = c(0.14, 0.14, 0.36, 0.36),
    part = "Ileocecal Valve"
)

appendix <- data.frame(
    dx = c(-3.15, -2.95, -2.98, -3.05, -3.12),
    dy = c(0, 0, -0.45, -0.55, -0.45),
    part = "Appendix"
)

anal_canal <- data.frame(
    dx = c(-1.25, -0.75, -0.85, -1.15),
    dy = c(-1, -1, -1.4, -1.4),
    part = "Anal Canal"
)

# Half ring cupping the anal canal (fistulae, abscesses, skin tags)
perianal_theta <- seq(pi, 2 * pi, length.out = 25)
perianal <- data.frame(
    dx = c(-1 + 0.45 * cos(perianal_theta), rev(-1 + 0.25 * cos(perianal_theta))),
    dy = c(-1.3 + 0.45 * sin(perianal_theta), rev(-1.3 + 0.25 * sin(perianal_theta))),
    part = "Perianal"
)

# Label anchors (dx, dy, angle) for parts whose bounding-box centre does not
# fall on the part, keyed by variant
ibd_label_anchors <- data.frame(
    part = c("Esophagus", "Stomach", "Duodenum", "Jejunum", "Proximal Ileum",
             "Terminal Ileum", "Ileocecal Valve", "Appendix", "Anal Canal",
             "Perianal"),
    dx = c(-0.6, -0.1, -2.25, -1.375, -1.525, -1.95, -2.3, -3.05, -1, -1),
    dy = c(7.5, 6.3, 5.45, 2.9, 1.7, 0.75, -0.04, -0.25, -1.2, -1.65),
    angle = c(90, 0, 90, 0, 0, 90, 0, 90, 0, 0)
)

# Internal environment that holds all registered variants
.ggcolonviz_env <- new.env(parent = emptyenv())

.ggcolonviz_env$label_anchors <- list(ibd = ibd_label_anchors)

.ggcolonviz_env$variants <- list(
    standard = rbind(
        ileum,
        cecum,
        ascending,
        transverse,
        descending,
        sigmoid,
        rectum
    ),
    sescd = rbind(
        ileum,
        transverse,
        rectum,
        left_side_merged,
        right_side_merged
    ),
    ibd = rbind(
        esophagus,
        stomach,
        duodenum,
        jejunum,
        proximal_ileum,
        terminal_ileum,
        ileocecal_valve,
        cecum,
        appendix,
        ascending,
        transverse,
        descending,
        sigmoid,
        rectum,
        anal_canal,
        perianal
    )
)
