# ggcolonviz 0.1.0

* New `"ibd"` variant covering the whole gastrointestinal tract for
  inflammatory bowel disease: esophagus, stomach, duodenum, jejunum,
  proximal and terminal ileum, ileocecal valve, cecum, appendix, colon,
  rectum, anal canal and perianal region.
* New `colon_plot()`: one-call plotting of per-segment data, with optional
  faceting, segments absent from the data shown in grey, and a light-to-dark
  default scale for numeric values.
* New `colon_map()`: labelled reference map of a variant, coloured by region.
* New `colon_parts()`: table of the parts of a variant with their region and
  Montreal codes.
* New `montreal_parts()`: parts covered by Montreal codes L1-L4, p, E1-E3.
* New `colon_position()`: coordinates inside a named segment, for placing
  lesions without typing coordinates.
* Segment names are now matched ignoring case, spaces and punctuation, and
  common synonyms are accepted (e.g. `"TI"`, `"caecum"`, `"sigmoid colon"`).
  Unknown parts are still dropped, but now with a warning listing the valid
  names.
* `geom_colon()` and `geom_colon_label()` gain `na.rm` and `inherit.aes`
  arguments; `na.rm` supplied by the user was previously duplicated.
* New vignette "Getting started: IBD data on the gut".

# ggcolonviz 0.0.6

* Improved robustness of internal stat layers when handling unknown or
  missing input parts.

# ggcolonviz 0.0.4

* `geom_colon()` no longer errors when every input row references an
  unknown `part`; the layer now returns an empty polygon layer.
* `geom_colon_label()` correctly honours a user-mapped `label` aesthetic
  (previously overwritten by the derived label).

# ggcolonviz 0.0.3

* Added a variant registry: `register_colon_variant()` and
  `list_colon_variants()` for user-defined colon layouts alongside the
  built-in `"standard"` and `"sescd"` variants.
* Added a `Basics` vignette walking through the core plotting workflow.
* Expanded the test suite to cover blueprint geometry, `sf` helpers,
  stat layers, and the variant registry.

# ggcolonviz 0.0.2

* Initial release.
* `geom_colon()` and `geom_colon_label()` for visualizing the colon with
  `ggplot2`.
* Blueprint system for configurable colon anatomical layouts, with
  `"standard"` and `"sescd"` variants shipped.
