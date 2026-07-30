# Writing deliverables to outputs/ as tracked files.
#
#
# Each function returns the path it wrote, which is what tar_target(format =
# "file") needs: targets then hashes the file, so a change to the plot or the
# table invalidates the artefact rather than silently leaving a stale one on
# disk. That is the same property the raw-input targets have.

# outputs/ is gitignored and so will not exist on a clean checkout.
ensure_outputs_dir <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}

# Writes a data frame to outputs/<filename> and returns the path.
write_output_csv <- function(x, filename) {
  path <- ensure_outputs_dir(file.path("outputs", filename))
  readr::write_csv(x, path)
  path
}

# Writes a ggplot/patchwork object to outputs/<filename> and returns the path.
#
# Defaults match the ggsave() calls in Analysis.qmd, which uses 8 x 8 inches for
# the whole fig3 family. Pass width/height explicitly for anything else.
#
# TYPEFACE. The family is applied HERE rather than inside each figure function,
# for two reasons: it overrides every panel of a patchwork in one place, and it
# means the ten figure functions stay free of styling that is a document-level
# decision rather than a per-figure one.
#
# `font_paths` comes from the inter_fonts target. Registration has to happen in
# the rendering process — targets builds each target in its own process, so a
# registration done elsewhere does not survive.
#
# Rendered through ragg::agg_png(), not the default device. This is not
# optional: the default png device does not do font fallback or shaping
# properly, and on macOS goes through quartz, which resolves families
# differently. Using ragg is what makes the output the same on every machine.
write_output_plot <- function(plot, filename, width = 8, height = 8, dpi = 300,
                              font_paths = NULL, family = "Inter") {
  path <- ensure_outputs_dir(file.path("outputs", filename))

  if (!is.null(font_paths)) {
    register_inter(font_paths, family = family)
    # patchwork needs `&` to reach every panel; a bare ggplot needs `+`.
    styling <- ggplot2::theme(text = ggplot2::element_text(family = family))
    plot <- if (inherits(plot, "patchwork")) plot & styling else plot + styling
  }

  ggplot2::ggsave(path, plot,
    width = width, height = height, dpi = dpi,
    device = ragg::agg_png
  )
  path
}

# fit_fig3() returns a list with $plot alongside the three model fits; this is
# just the accessor, kept so _targets.R reads as one call per artefact.
write_fig3_plot <- function(fig3_out, filename, ...) {
  write_output_plot(fig3_out$plot, filename, ...)
}
