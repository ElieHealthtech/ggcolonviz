# Anatomical region and Montreal classification codes for every built-in part.
# Montreal (Silverberg et al. 2005): Crohn's disease location L1 ileal,
# L2 colonic, L4 upper GI, "p" perianal modifier; ulcerative colitis extent
# E1 proctitis, E2 left-sided (distal to the splenic flexure), E3 extensive.
.part_info <- data.frame(
    part = c(
        "Esophagus", "Stomach", "Duodenum", "Jejunum", "Proximal Ileum",
        "Terminal Ileum", "Ileum", "Ileocecal Valve", "Cecum", "Appendix",
        "Ascending", "Right", "Transverse", "Descending", "Sigmoid", "Left",
        "Rectum", "Anal Canal", "Perianal"
    ),
    region = c(
        rep("Upper GI", 3), rep("Small bowel", 5), rep("Colon", 8),
        "Rectum", rep("Perianal", 2)
    ),
    montreal = c(
        "L4", "L4", "L4", "L4", "L4",
        "L1", "L1", "L1", "L2,E3", "",
        "L2,E3", "L2,E3", "L2,E3", "L2,E2,E3", "L2,E2,E3", "L2,E2,E3",
        "L2,E1,E2,E3", "p", "p"
    ),
    stringsAsFactors = FALSE
)

# Accepted spellings. Keys are compared after .part_key(); when several
# canonical names are listed, the first one present in the variant wins.
.part_aliases <- list(
    esophagus = "Esophagus",
    oesophagus = "Esophagus",
    stomach = "Stomach",
    gastric = "Stomach",
    duodenum = "Duodenum",
    jejunum = "Jejunum",
    proximalileum = "Proximal Ileum",
    ti = c("Terminal Ileum", "Ileum"),
    terminalileum = c("Terminal Ileum", "Ileum"),
    neoterminalileum = c("Terminal Ileum", "Ileum"),
    ileum = c("Ileum", "Terminal Ileum"),
    icv = "Ileocecal Valve",
    ileocecalvalve = "Ileocecal Valve",
    ileocaecalvalve = "Ileocecal Valve",
    cecum = "Cecum",
    caecum = "Cecum",
    appendix = "Appendix",
    ascending = "Ascending",
    ascendingcolon = "Ascending",
    rightcolon = "Right",
    right = "Right",
    transverse = "Transverse",
    transversecolon = "Transverse",
    descending = "Descending",
    descendingcolon = "Descending",
    sigmoid = "Sigmoid",
    sigmoidcolon = "Sigmoid",
    leftcolon = "Left",
    left = "Left",
    rectum = "Rectum",
    analcanal = "Anal Canal",
    anus = "Anal Canal",
    perianal = "Perianal",
    perianalregion = "Perianal",
    perianaldisease = "Perianal"
)

.part_key <- function(x) {
    gsub("[^a-z]", "", tolower(x))
}

# Map user-supplied segment names onto the canonical part names of `variant`.
# Unresolvable names are returned unchanged (so the stats drop them) and, when
# `warn` is TRUE, reported in a single warning.
.normalize_part <- function(x, variant = "standard", warn = TRUE, caller = NULL) {
    x <- as.character(x)
    parts <- unique(.get_variant(variant)$part)
    part_keys <- .part_key(parts)

    resolve <- function(value) {
        if (is.na(value) || value %in% parts) {
            return(value)
        }
        key <- .part_key(value)
        candidates <- c(.part_aliases[[key]], parts[part_keys == key])
        hit <- candidates[candidates %in% parts]
        if (length(hit) > 0L) hit[[1L]] else value
    }

    values <- unique(x)
    resolved <- vapply(values, resolve, character(1), USE.NAMES = FALSE)
    out <- resolved[match(x, values)]

    unknown <- unique(out[!is.na(out) & !out %in% parts])
    if (warn && length(unknown) > 0L) {
        warning(
            if (!is.null(caller)) paste0(caller, ": "),
            "ignoring unknown part(s) ",
            paste0("'", unknown, "'", collapse = ", "),
            " for variant '", variant, "'.\n",
            "Valid parts: ", paste(parts, collapse = ", "), ".\n",
            "See colon_parts(\"", variant, "\").",
            call. = FALSE
        )
    }
    out
}

#' List the Parts of a Colon Variant
#'
#' Returns the anatomical parts (segments) that can be drawn for a variant,
#' together with their region and Montreal classification codes. Use the
#' `part` column to name segments in your own data.
#'
#' Segment names are matched leniently by the plotting functions: case,
#' spaces and punctuation are ignored and common synonyms are accepted, for
#' example `"TI"` or `"terminal ileum"`, `"caecum"`, `"sigmoid colon"`,
#' `"right colon"`, `"ICV"` and `"anus"`.
#'
#' @param variant A character string naming a registered variant; see
#'   [list_colon_variants()].
#' @return A data frame with one row per part and columns `part`, `region`
#'   (`"Upper GI"`, `"Small bowel"`, `"Colon"`, `"Rectum"` or `"Perianal"`)
#'   and `montreal` (comma-separated Montreal codes covering the part).
#'   `region` and `montreal` are `NA` for parts of user-registered variants
#'   that the package does not know.
#' @seealso [montreal_parts()], [colon_map()]
#' @export
#' @examples
#' colon_parts("ibd")
colon_parts <- function(variant = "standard") {
    parts <- unique(.get_variant(variant)$part)
    info <- .part_info[match(parts, .part_info$part), c("region", "montreal")]
    data.frame(
        part = parts,
        region = info$region,
        montreal = info$montreal,
        stringsAsFactors = FALSE,
        row.names = NULL
    )
}

#' Parts Covered by a Montreal Classification Code
#'
#' Translates Montreal disease location (Crohn's disease) or extent
#' (ulcerative colitis) codes into the parts of a variant.
#'
#' @param code One or more of `"L1"` (ileal), `"L2"` (colonic), `"L3"`
#'   (ileocolonic, i.e. L1 + L2), `"L4"` (upper GI), `"p"` (perianal),
#'   `"E1"` (proctitis), `"E2"` (left-sided) and `"E3"` (extensive). When
#'   several codes are given, the union of their parts is returned.
#' @param variant A character string naming a registered variant. Only the
#'   `"ibd"` variant represents every code; other variants return the parts
#'   they have.
#' @return A character vector of part names, in the order of the variant.
#' @seealso [colon_parts()]
#' @export
#' @examples
#' montreal_parts("L3")
#' montreal_parts(c("L1", "p"))
#' montreal_parts("E2", variant = "standard")
montreal_parts <- function(code, variant = "ibd") {
    valid <- c("L1", "L2", "L3", "L4", "p", "E1", "E2", "E3")
    bad <- setdiff(code, valid)
    if (length(bad) > 0L) {
        stop(
            "Unknown Montreal code(s): ", paste(bad, collapse = ", "),
            ". Use one of: ", paste(valid, collapse = ", "), ".",
            call. = FALSE
        )
    }
    code <- unique(c(setdiff(code, "L3"), if ("L3" %in% code) c("L1", "L2")))
    info <- colon_parts(variant)
    covered <- vapply(
        strsplit(ifelse(is.na(info$montreal), "", info$montreal), ","),
        function(codes) any(codes %in% code),
        logical(1)
    )
    info$part[covered]
}
