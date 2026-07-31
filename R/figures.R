# All figures in the paper.
#
# One function per figure, each returning the assembled patchwork object. The
# ggsave() call lives in the target (see R/outputs.R), so the figure and the
# file it is written to are separate concerns and the file can be tracked.
#
# Naming: these take `grid_climate` and `data_prop_annual`, which are the
# renamed and filtered tables, distinct from the raw `data_grid_climate`
# target they derive from.

# Shared map styling. Every map panel in the paper uses this same coord_sf
# window and theme; factoring it out is safe because the calls were already
# byte-identical, and it keeps each figure function readable.
map_coords <- function() {
  ggplot2::coord_sf(ylim = c(-45, -10), xlim = c(110, 155), expand = FALSE)
}

map_theme <- function() {
  list(
    ggplot2::theme_bw(),
    ggplot2::theme(
      legend.key.size = ggplot2::unit(0.35, "cm"),
      legend.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 12, hjust = 0.5, vjust = 3),
      axis.title = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 5), size = 10)
    )
  )
}

# Figure 1 — MAP and seasonality maps, Australian climate space, total richness.
# Saved at 9 x 6.
make_fig1 <- function(grid_climate, data_prop_annual, data_ozmaps, data_sea) {
  # 1a
  au_rainfall <-
    ggplot2::ggplot(data_ozmaps) +
    ggplot2::geom_tile(data = grid_climate, ggplot2::aes(x, y, fill = bio12)) +
    ggplot2::scale_fill_continuous(
      type = "viridis", limits = c(30, 4000), trans = "log",
      breaks = c(30, 100, 300, 1000, 3000), name = "Precipitation (mm)"
    ) +
    ggplot2::geom_sf(data = data_sea, fill = "white") +
    map_coords() +
    map_theme() +
    ggplot2::labs(x = "", y = "", title = "Mean annual precipitation")

  # 1b
  au_seasonality <-
    ggplot2::ggplot(data_ozmaps) +
    ggplot2::geom_tile(data = grid_climate, ggplot2::aes(x, y, fill = bio15)) +
    ggplot2::scale_fill_continuous(type = "viridis", name = "") +
    ggplot2::geom_sf(data = data_sea, fill = "white") +
    map_coords() +
    map_theme() +
    ggplot2::labs(x = "", y = "", title = "Precipitation seasonality")

  # 1c
  hexbin_au_climate <-
    ggplot2::ggplot(grid_climate, ggplot2::aes(x = bio12, y = bio15)) +
    ggplot2::stat_bin_hex(ggplot2::aes(fill = ggplot2::after_stat(count / sum(count)))) +
    ggplot2::scale_x_log10(limits = c(50, 6000)) +
    ggplot2::scale_y_continuous(limits = c(0, 150)) +
    ggplot2::labs(
      x = "Mean annual precip (mm)", y = "Precipitation seasonality",
      title = "Australian climate space"
    ) +
    viridis::scale_fill_viridis(
      name = "Relative \nprevalence",
      labels = scales::percent_format()
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      # Matches the aspect of the map panels beside it, at the mid-latitude of
      # the plotted window.
      aspect.ratio = 35 / (45 * cos(27.5 * pi / 180)),
      legend.key.size = ggplot2::unit(0.35, "cm"),
      legend.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 12, hjust = 0.5, vjust = 3),
      axis.title = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 5), size = 10)
    )

  # 1d. Total richness across natives and introduced together; filtered to the
  # native rows because n_spp_per_cell is a cell-level total duplicated across
  # both groups, so taking both would double-plot the same value.
  n_richness <-
    ggplot2::ggplot(data_ozmaps) +
    ggplot2::geom_tile(
      data = dplyr::filter(data_prop_annual, establishment_means == "native"),
      ggplot2::aes(x, y, fill = n_spp_per_cell)
    ) +
    ggplot2::scale_fill_continuous(type = "viridis", name = "Species richness") +
    ggplot2::geom_sf(data = data_sea, fill = "white") +
    map_coords() +
    map_theme() +
    ggplot2::labs(x = "", y = "", title = "Total species richness")

  (au_rainfall + au_seasonality) / (hexbin_au_climate + n_richness) +
    patchwork::plot_annotation(tag_levels = "a") &
    ggplot2::theme(plot.tag = ggplot2::element_text(size = 14, face = "bold"))
}

# Figure S1 — Australian climate space against global climate space.
# Saved at 8 x 3.
#
# NB requires ggnewscale, for the two overlaid fill scales. It is declared in
# tar_option_set(packages).
make_figS1 <- function(grid_climate, global_climate) {
  # The global grid is trimmed to plausible ranges before binning; the extremes
  # are polar and hyper-arid cells that would otherwise dominate the colour
  # scale.
  global_trimmed <- global_climate |>
    dplyr::filter(dplyr::between(bio12, 2, 5500), dplyr::between(bio15, 1, 240))

  hex_theme <- ggplot2::theme(
    aspect.ratio = 35 / (45 * cos(27.5 * pi / 180)),
    legend.key.size = ggplot2::unit(0.35, "cm"),
    legend.text = ggplot2::element_text(size = 8),
    plot.title = ggplot2::element_text(size = 12, hjust = 0.5, vjust = 3),
    axis.title = ggplot2::element_text(size = 12),
    axis.text.y = ggplot2::element_text(size = 10),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 5), size = 10)
  )

  global_hex <-
    ggplot2::ggplot(global_trimmed, ggplot2::aes(x = bio12, y = bio15)) +
    ggplot2::stat_bin_hex(ggplot2::aes(fill = ggplot2::after_stat(count / sum(count)))) +
    ggplot2::scale_x_log10(limits = c(1, 6000)) +
    ggplot2::scale_y_continuous(limits = c(0, 250)) +
    ggplot2::labs(
      x = "Mean annual precip (mm)", y = "Precipitation seasonality",
      title = "Global climate space"
    ) +
    viridis::scale_fill_viridis(
      name = "Relative \nprevalence",
      labels = scales::percent_format()
    ) +
    ggplot2::theme_classic() +
    hex_theme

  # Australia in viridis over a grey global background. The two layers need
  # separate fill scales, which is what ggnewscale provides.
  au_hex <-
    ggplot2::ggplot(grid_climate, ggplot2::aes(x = bio12, y = bio15)) +
    ggplot2::stat_bin_hex(
      data = global_trimmed,
      ggplot2::aes(fill = ggplot2::after_stat(count / sum(count))),
      alpha = 0.8
    ) +
    ggplot2::scale_fill_gradient(low = "grey90", high = "grey30", guide = "none") +
    ggnewscale::new_scale_fill() +
    ggplot2::stat_bin_hex(ggplot2::aes(fill = ggplot2::after_stat(count / sum(count)))) +
    ggplot2::scale_x_log10(limits = c(1, 6000)) +
    ggplot2::scale_y_continuous(limits = c(0, 250)) +
    ggplot2::labs(
      x = "Mean annual precip (mm)", y = "Precipitation seasonality",
      title = "Australian climate space"
    ) +
    viridis::scale_fill_viridis(
      name = "Relative \nprevalence",
      labels = scales::percent_format()
    ) +
    ggplot2::theme_classic() +
    hex_theme

  (global_hex + au_hex) +
    patchwork::plot_annotation(tag_levels = "a") &
    ggplot2::theme(plot.tag = ggplot2::element_text(size = 14, face = "bold"))
}

# Figure 2 — 2x2 richness maps: annual/perennial by native/introduced.
# Saved at 9 x 6.
#
# The four panels carry deliberately different themes (axis titles appear only
# on the left column, titles only on the top row), so they are written out
# rather than sharing map_theme().
make_fig2 <- function(data_prop_annual, data_ozmaps, data_sea) {
  panel <- function(group, fill_var, xlab, ylab, title, theme_extra) {
    ggplot2::ggplot(data_ozmaps) +
      ggplot2::geom_tile(
        data = dplyr::filter(data_prop_annual, establishment_means == group),
        ggplot2::aes(x, y, fill = .data[[fill_var]])
      ) +
      ggplot2::scale_fill_continuous(type = "viridis") +
      ggplot2::geom_sf(data = data_sea, fill = "white") +
      map_coords() +
      ggplot2::theme_bw() +
      ggplot2::labs(x = xlab, y = ylab, title = title) +
      theme_extra
  }

  common <- ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 5), size = 10),
    axis.text.y = ggplot2::element_text(size = 10)
  )

  a <- panel(
    "native", "n_annual", "", "Annuals", "Natives",
    common + ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 12),
      axis.title.y = ggplot2::element_text(size = 12, vjust = 4),
      plot.title = ggplot2::element_text(size = 12, hjust = 0.5, vjust = 3)
    )
  )
  b <- panel(
    "introduced", "n_annual", "", "", "Introduced",
    common + ggplot2::theme(
      axis.title = ggplot2::element_text(size = 12, vjust = 4),
      plot.title = ggplot2::element_text(size = 12, hjust = 0.5, vjust = 3)
    )
  )
  cc <- panel(
    "native", "n_perennial", "", "Perennials", "",
    common + ggplot2::theme(
      axis.title.y = ggplot2::element_text(size = 12, vjust = 4),
      plot.title = ggplot2::element_text(size = 12, hjust = 0.5, vjust = 3)
    )
  )
  d <- panel(
    "introduced", "n_perennial", "", "", "",
    common + ggplot2::theme(
      axis.title = ggplot2::element_text(size = 12, vjust = 4),
      plot.title = ggplot2::element_text(size = 12, hjust = 0, vjust = 3)
    )
  )

  (a + b) / (cc + d) +
    patchwork::plot_annotation(tag_levels = "a") &
    ggplot2::theme(plot.tag = ggplot2::element_text(size = 14, face = "bold"))
}

# Figure S5 — sampling effort in climate space, and as a map.
# Saved at 8 x 3.
make_figS5 <- function(data_prop_annual, data_ozmaps, data_sea) {
  hexbin_effort <-
    ggplot2::ggplot(data_prop_annual, ggplot2::aes(x = bio12, y = bio15)) +
    ggplot2::stat_summary_hex(ggplot2::aes(z = n_obs_per_cell), fun = mean) +
    ggplot2::scale_x_log10(limits = c(50, 6000)) +
    ggplot2::scale_y_continuous(limits = c(0, 150)) +
    ggplot2::labs(
      x = "Mean annual precip (mm)", y = "Precipitation seasonality",
      title = "Sampling effort"
    ) +
    viridis::scale_fill_viridis(name = "Record count", trans = "log10") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      aspect.ratio = 35 / (45 * cos(27.5 * pi / 180)),
      legend.key.size = ggplot2::unit(0.35, "cm"),
      legend.text = ggplot2::element_text(size = 8),
      plot.title = ggplot2::element_text(size = 12, hjust = 0.5, vjust = 3),
      axis.title = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 5), size = 10)
    )

  # Total effort across natives and introduced. Filtered to native rows because
  # n_obs_per_cell is a cell-level total duplicated across both groups.
  effort_map <-
    ggplot2::ggplot(data_ozmaps) +
    ggplot2::geom_tile(
      data = dplyr::filter(data_prop_annual, establishment_means == "native"),
      ggplot2::aes(x, y, fill = n_obs_per_cell)
    ) +
    ggplot2::scale_fill_continuous(
      type = "viridis", trans = "log10",
      name = "Record count"
    ) +
    ggplot2::geom_sf(data = data_sea, fill = "white") +
    map_coords() +
    map_theme() +
    ggplot2::labs(x = "", y = "", title = "Sampling effort")

  (hexbin_effort + effort_map) +
    patchwork::plot_annotation(tag_levels = "a") &
    ggplot2::theme(plot.tag = ggplot2::element_text(size = 14, face = "bold"))
}

# Figure S6 — effort diagnostics: histogram, and effort against each climate
# variable with the fitted effort model overlaid.
# Saved at 9 x 3.
#
# Takes the fitted model rather than refitting it, so the figure and the
# reported model cannot drift apart.
#
# Three panels. A fourth (effort against population accessibility) existed in
# the original code but was never assembled into the figure.
make_figS6 <- function(data_prop_annual, effort_model) {
  panel_theme <- ggplot2::theme(
    axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 5), size = 10),
    axis.title.y = ggplot2::element_text(size = 10, vjust = 4),
    axis.text.y = ggplot2::element_text(size = 10, vjust = 4)
  )
  log_breaks <- c(10, 1000, 100000)
  log_labels <- c("10", "1000", "100,000")

  # 250 dashed / 500 solid mark the sensitivity cutoffs; 500 is the primary.
  p1 <- ggplot2::ggplot(data_prop_annual, ggplot2::aes(n_obs_per_cell)) +
    ggplot2::geom_histogram(bins = 50, fill = "steelblue", colour = "white") +
    ggplot2::geom_vline(xintercept = 250, colour = "red", linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 500, colour = "red", linetype = "solid") +
    ggplot2::scale_x_log10(breaks = log_breaks, labels = log_labels) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Total GBIF records", y = "Grid cell count", title = "") +
    panel_theme +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0, vjust = 3))

  pred_bio12 <- tibble::tibble(
    bio12 = 10^seq(log10(min(data_prop_annual$bio12[data_prop_annual$bio12 > 0])),
      log10(max(data_prop_annual$bio12, na.rm = TRUE)),
      length.out = 200
    ),
    bio15 = mean(data_prop_annual$bio15, na.rm = TRUE)
  )
  pred_bio12$n_obs_per_cell <- 10^stats::predict(effort_model,
    newdata = pred_bio12,
    type = "response"
  )

  pred_bio15 <- tibble::tibble(
    bio15 = seq(min(data_prop_annual$bio15, na.rm = TRUE),
      max(data_prop_annual$bio15, na.rm = TRUE),
      length.out = 200
    ),
    bio12 = mean(data_prop_annual$bio12, na.rm = TRUE)
  )
  pred_bio15$n_obs_per_cell <- 10^stats::predict(effort_model,
    newdata = pred_bio15,
    type = "response"
  )

  scatter <- function(xvar, pred, xlab, logx) {
    p <- ggplot2::ggplot(
      data_prop_annual,
      ggplot2::aes(.data[[xvar]], n_obs_per_cell)
    ) +
      ggplot2::geom_point(alpha = 0.5, size = 0.7, na.rm = TRUE, stroke = 0) +
      ggplot2::geom_line(data = pred, colour = "steelblue", na.rm = TRUE) +
      ggplot2::scale_y_log10(breaks = log_breaks, labels = log_labels) +
      ggplot2::labs(x = xlab, y = "Total GBIF records", title = "") +
      ggplot2::theme_bw() +
      panel_theme +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12, hjust = 0.5, vjust = 3))
    if (logx) p + ggplot2::scale_x_log10() else p
  }

  p2 <- scatter("bio12", pred_bio12, "Mean annual precip (mm)", TRUE)
  p3 <- scatter("bio15", pred_bio15, "Precipitation seasonality", FALSE)

  p1 + p2 + p3 +
    patchwork::plot_annotation(tag_levels = "a") &
    ggplot2::theme(plot.tag = ggplot2::element_text(size = 14, face = "bold"))
}

# Figure 4 — fraction annuals as maps and in climate space, plus richness in
# climate space, for both floras. Saved at 8 x 12.
#
# Eight panels on a strict grid: left column native, right column introduced;
# rows are frac_annuals (map), frac_annuals (climate space), annual richness,
# perennial richness. Only the right column carries a legend and only the
# bottom row carries an x-axis title, which is what the theme differences
# between panels encode.
#
# species_cutoff is the minimum total species per cell for the fraction-annuals
# panels, and is separate from the n_obs_per_cell threshold used in the models:
# this one guards against a fraction computed from a handful of species, which
# is noisy regardless of how many records produced them.
#
# NB rows 1-2 apply it; rows 3-4 do not. That asymmetry is in the original and
# is deliberate — a fraction needs a denominator large enough to mean
# something, a count does not.
make_fig4 <- function(data_prop_annual, data_ozmaps, data_sea,
                      species_cutoff = 5) {
  frac_data <- function(group) {
    dplyr::filter(
      data_prop_annual, n_species_total >= species_cutoff,
      establishment_means == group
    )
  }
  all_data <- function(group) {
    dplyr::filter(data_prop_annual, establishment_means == group)
  }

  base_theme <- function(bottom = 5) {
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 5), size = 10),
      plot.title = ggplot2::element_text(vjust = 2, hjust = 0.5, size = 12),
      plot.margin = ggplot2::margin(t = 5, r = 5, b = bottom, l = 5)
    )
  }
  no_legend <- ggplot2::theme(legend.position = "none")
  legend_pad <- ggplot2::theme(
    legend.title = ggplot2::element_text(margin = ggplot2::margin(b = 10))
  )

  # Row 1 — maps of the annual fraction.
  map_panel <- function(group, title, legend_name, extra) {
    ggplot2::ggplot(data_ozmaps) +
      ggplot2::geom_tile(
        data = frac_data(group),
        ggplot2::aes(x, y, fill = frac_annuals)
      ) +
      ggplot2::scale_fill_continuous(
        type = "viridis", limits = c(0, 1),
        name = legend_name
      ) +
      ggplot2::geom_sf(data = data_sea, fill = "white") +
      map_coords() +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", y = "", title = title) +
      base_theme(8) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_text(size = 12)
      ) +
      extra
  }

  a <- map_panel(
    "native", "Native species", NULL,
    no_legend + ggplot2::theme(legend.title = ggplot2::element_blank())
  )
  b <- map_panel(
    "introduced", "Introduced species", "Fraction annuals",
    legend_pad + ggplot2::theme(legend.margin = ggplot2::margin(l = 15))
  )

  # Rows 2-4 — hexbins in climate space.
  hex_panel <- function(d, zvar, title, ylab, xlab, legend_name, show_legend,
                        percent = FALSE, bottom = 8) {
    p <- ggplot2::ggplot(d) +
      ggplot2::stat_summary_hex(
        ggplot2::aes(x = bio12, y = bio15, z = .data[[zvar]]),
        fun = mean
      ) +
      ggplot2::scale_x_log10(limits = c(100, 3000)) +
      ggplot2::scale_y_continuous(limits = c(0, 150)) +
      ggplot2::labs(x = xlab, y = ylab, title = title) +
      ggplot2::theme_classic() +
      base_theme(bottom) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12))
    p <- p + if (percent) {
      # limits = c(0, 1) and no label formatting: the annual fraction is
      # reported on a 0-1 scale throughout, including the maps in row 1.
      viridis::scale_fill_viridis(name = legend_name, limits = c(0, 1))
    } else {
      viridis::scale_fill_viridis(name = legend_name)
    }
    if (xlab == "") p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    if (show_legend) p + legend_pad else p + no_legend
  }

  cc <- hex_panel(
    frac_data("native"), "frac_annuals", "Native species",
    "Precipitation seasonality", "", "Fraction annuals", FALSE, TRUE
  )
  d <- hex_panel(
    frac_data("introduced"), "frac_annuals", "Introduced species",
    "", "", "Fraction annuals", TRUE, TRUE
  )
  e <- hex_panel(
    all_data("native"), "n_annual", "Native annuals",
    "Precipitation seasonality", "", "Species richness", FALSE
  )
  f <- hex_panel(
    all_data("introduced"), "n_annual", "Introduced annuals",
    "", "", "Species richness", TRUE
  )
  g <- hex_panel(all_data("native"), "n_perennial", "Native perennials",
    "Precipitation seasonality", "Mean annual precip (mm)",
    "Species richness", FALSE,
    bottom = 15
  )
  # No y-axis title, matching the rest of the right-hand column. The original
  # gave this one panel a y-axis title while b, d and f had none.
  h <- hex_panel(all_data("introduced"), "n_perennial", "Introduced perennials",
    "", "Mean annual precip (mm)",
    "Species richness", TRUE,
    bottom = 15
  )

  (a + b) / (cc + d) / (e + f) / (g + h) +
    patchwork::plot_annotation(tag_levels = "a") &
    ggplot2::theme(plot.tag = ggplot2::element_text(size = 14, face = "bold"))
}

# Collapses data_prop_annual to one row per cell, with the introduced fraction
# of each life-history group.
make_prop_annual_collapsed <- function(data_prop_annual) {
  data_prop_annual |>
    dplyr::group_by(cell, x, y, pop_accessibility, bio12, bio15) |>
    dplyr::summarise(
      n_obs_per_cell = dplyr::first(n_obs_per_cell),
      n_spp_per_cell = dplyr::first(n_spp_per_cell),
      prop_invasive_spp = sum(n_species_total[establishment_means == "introduced"]) /
        sum(n_species_total),
      prop_invasive_annual = sum(n_annual[establishment_means == "introduced"]) /
        sum(n_annual),
      prop_invasive_perennial = sum(n_perennial[establishment_means == "introduced"]) /
        sum(n_perennial),
      n_spp_introduced = sum(n_species_total[establishment_means == "introduced"]),
      n_spp_native = sum(n_species_total[establishment_means == "native"]),
      .groups = "drop"
    )
}

# Figure 5 — introduced fraction of annuals, and population accessibility.
# Saved at 10 x 3.5.
#
# Two panels. Variants showing the introduced fraction of all species and of
# perennials existed in the original code but were never assembled in.
make_fig5 <- function(prop_annual_collapsed, data_ozmaps, data_sea) {
  panel <- function(fill_var, title, extra) {
    ggplot2::ggplot(data_ozmaps) +
      ggplot2::geom_tile(
        data = prop_annual_collapsed,
        ggplot2::aes(x, y, fill = .data[[fill_var]])
      ) +
      ggplot2::scale_fill_continuous(type = "viridis") +
      ggplot2::geom_sf(data = data_sea, fill = "white") +
      map_coords() +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "", y = "", title = title) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        plot.title.position = "plot"
      ) +
      extra
  }

  a <- panel(
    "prop_invasive_annual", "Fraction annuals that are introduced",
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(size = 10, vjust = 4),
      plot.title = ggplot2::element_text(size = 12, hjust = .5, vjust = 6)
    )
  )
  b <- panel(
    "pop_accessibility", "Population accessibility    ",
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 12, hjust = 0.5, vjust = 6)
    )
  )

  a + b +
    patchwork::plot_annotation(tag_levels = "a") &
    ggplot2::theme(plot.tag = ggplot2::element_text(size = 14, face = "bold"))
}

# Figures Extended 3 and 4 — richness against a climate variable, coloured by
# population accessibility. Ported from Analysis.qmd.
#
# The two figures are identical apart from the x variable, so one function
# covers both: E3 uses bio12 on a log axis, E4 uses bio15 on a linear one. The
# original code wrote them out as two near-duplicate blocks of eight panels.
#
# data_prop_annual is deliberately not a parameter. The .qmd joins
# pop_accessibility back on from it here:
#
#   left_join(data_prop_annual |> select(cell, pop_accessibility) |> distinct())
#
# with no `by`, so dplyr joins on every common column — cell AND
# pop_accessibility. data_model already carries pop_accessibility (it is derived
# from data_prop_annual), so that join is a no-op which happens to work. Writing
# `by = "cell"` instead produces .x/.y suffixes and breaks. Dropping the join
# entirely is the equivalent that cannot be misread, which leaves the frame
# itself unused — taking it as an argument would only add a false edge to the
# targets DAG.
make_figE_pop <- function(data_model, xvar = c("bio12", "bio15")) {
  xvar <- match.arg(xvar)
  prep <- function(group) {
    data_model |>
      dplyr::mutate(pop_accessibility_log = log10(pop_accessibility)) |>
      dplyr::filter(establishment_means == group)
  }
  native <- prep("native")
  intro <- prep("introduced")

  xlab_full <- if (xvar == "bio12") "Mean annual precip (mm)" else "Precipitation seasonality"
  x_scale <- if (xvar == "bio12") {
    ggplot2::scale_x_log10(limits = c(100, 5000))
  } else {
    ggplot2::scale_x_continuous(limits = c(0, 150))
  }

  panel <- function(d, yvar, ylab, xlab, title, show_legend) {
    p <- ggplot2::ggplot(d, ggplot2::aes(.data[[xvar]], .data[[yvar]],
      fill = pop_accessibility_log
    )) +
      ggplot2::geom_point(alpha = 0.5, stroke = 0.2, shape = 21, na.rm = TRUE) +
      x_scale +
      ggplot2::scale_y_log10(limits = c(1, 1500)) +
      ggplot2::labs(y = ylab, x = xlab, title = title) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 12),
        plot.title = ggplot2::element_text(vjust = 6, hjust = 0.5, size = 12)
      )
    # Whether a panel carries the legend and whether it carries the x title are
    # separate questions: panel c is on the bottom row, so it needs the x title,
    # but the legend only goes on the right-hand column. Tying the two together
    # is what left c unlabelled.
    if (xlab == "") {
      p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank())
    }
    if (show_legend) {
      p + ggplot2::scale_fill_viridis_c(name = "population\naccessibility\n(log 10)") +
        ggplot2::theme(
          legend.key.size = ggplot2::unit(0.35, "cm"),
          legend.text = ggplot2::element_text(size = 8),
          legend.title = ggplot2::element_text(size = 8)
        )
    } else {
      p + ggplot2::scale_fill_viridis_c() +
        ggplot2::theme(legend.position = "none")
    }
  }

  a <- panel(native, "n_annual", "Annual species richness", "", "Native species", FALSE)
  b <- panel(intro, "n_annual", "", "", "Introduced species", TRUE)
  cc <- panel(native, "n_perennial", "Perennial species richness", xlab_full, "", FALSE)
  d <- panel(intro, "n_perennial", "", xlab_full, "", TRUE)

  (a + b) / (cc + d) +
    patchwork::plot_annotation(tag_levels = "a") &
    ggplot2::theme(plot.tag = ggplot2::element_text(size = 14, face = "bold"))
}

# Figure 4b — the native-minus-introduced offset in annual fraction, as a map
# and in climate space. Saved at 6 x 3.
#
# Not currently included in the paper, but produced for completeness.
#
# NB this is the figure whose axis and plot titles are set at size 20 while
# every other figure uses 12 — at 6 x 3 inches that is enormous, and it looks
# like a leftover. Ported as-is; flagged for the styling pass.
make_fig4b <- function(missing_fraction, data_ozmaps, data_sea) {
  offset_scale <- ggplot2::scale_fill_distiller(
    name = "Difference in fraction", palette = "BrBG", limits = c(-1, 1),
    oob = scales::squish, direction = 1, na.value = "grey90"
  )

  map <- ggplot2::ggplot(data_ozmaps) +
    ggplot2::geom_tile(
      data = missing_fraction,
      ggplot2::aes(x, y, fill = frac_offset)
    ) +
    ggplot2::geom_sf(data = data_sea, fill = "white") +
    map_coords() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "", y = "") +
    offset_scale +
    ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12))

  hexbin <- ggplot2::ggplot(missing_fraction) +
    ggplot2::stat_summary_hex(
      ggplot2::aes(x = bio12, y = bio15, z = frac_offset),
      fun = mean
    ) +
    ggplot2::scale_x_log10(limits = c(100, 3000)) +
    ggplot2::scale_y_continuous(limits = c(0, 150)) +
    ggplot2::xlab("Mean annual precip (mm)") +
    ggplot2::ylab("Precipitation seasonality") +
    offset_scale +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none")

  # The title goes on the assembled figure rather than on the left panel. As a
  # panel title it was wider than the panel, so it overran into the legend and
  # was clipped; the vjust offsets that used to push it clear also dragged the
  # axis titles out of position.
  (hexbin + map) +
    patchwork::plot_annotation(
      title = "Difference in fraction annuals, native vs introduced",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 12, hjust = 0.5)
      )
    )
}
