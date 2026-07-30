# Joining occurrences, taxonomy, life history and covariates onto the
# analysis grid.

# One row per retained occurrence record, with climate, population and grid cell.
#
# THIS IS THE PEAK-MEMORY TARGET OF THE PIPELINE. terra::vect() materialises
# every attribute for all 16.6M points, and gbif_clean carries
# all 18 columns (the exact-replication choice — see R/clean_gbif.R), three of
# which are near-unique per record. Budget accordingly: this step peaks at
# 17.4 GB and takes about six minutes.
make_data_analysis_individuals <- function(gbif_clean, names_aligned,
                                           species_life_history,
                                           climate_raster, pop_raster,
                                           grid_square, accepted_names) {
  gbif_climate <-
    terra::vect(
      gbif_clean,
      geom = c("decimalLongitude", "decimalLatitude"),
      crs = terra::crs(climate_raster)
    ) |>
    # Extracting climate values only; cell membership is added separately below
    # against grid_square, since these rasters are at WorldClim resolution.
    terra::extract(
      x = climate_raster, method = "simple", bind = TRUE,
      xy = TRUE, cells = FALSE, ID = FALSE
    ) |>
    terra::extract(
      x = pop_raster, method = "simple", bind = TRUE,
      xy = FALSE, cells = FALSE, ID = FALSE
    ) |>
    tibble::as_tibble() |>
    dplyr::rename(original_name = species) |>
    dplyr::select(x, y, original_name, dplyr::starts_with("wc2.1_2.5m"),
      population_size = Australian_Population_Grid_2011
    )

  # Cell membership for each point on the 0.75° analysis grid.
  gbif_climate$cell <- terra::extract(
    grid_square, cbind(gbif_climate$x, gbif_climate$y),
    cells = TRUE
  )$cell

  gbif_climate |>
    # Cleaned names. The str_remove() repeats what align_taxonomy() already
    # did; it is idempotent.
    dplyr::left_join(
      by = "original_name",
      names_aligned |>
        dplyr::mutate(suggested_name = stringr::str_remove(suggested_name, " \\[.*"))
    ) |>
    dplyr::rename(species = suggested_name) |>
    dplyr::left_join(species_life_history, by = dplyr::join_by(species)) |>
    # establishment_means renamed from native_anywhere_in_aus (the APCalign
    # output name) to match GBIF vocabulary.
    #
    # NB everything() pulls the full APCalign output through, so the result is
    # 23 columns, not the subset documented under "Key data objects".
    dplyr::select(x, y, cell, species, life_history,
      establishment_means = native_anywhere_in_aus,
      dplyr::starts_with("wc2.1"), dplyr::everything()
    ) |>
    # Collaborator change, merged from master 1e8a032: drop every taxon that is
    # not an APC-accepted name.
    #
    # Their comment records 15,579,033 rows remaining, 177,035 removed. Those
    # figures come from the STALE 20 km intermediate (15,756,068 - 177,035
    # = 15,579,033 exactly), not from the current 2 km code, so do not treat
    # them as the expected result here.
    dplyr::filter(species %in% accepted_names)
}

# The analysis-side filter applied to data_analysis_individuals at
# Analysis.qmd, before anything downstream consumes it.
#
# This is strictly an ANALYSIS filter, not part of dataset construction: the
# parquet written by Dataset_construction.qmd is unfiltered, and Analysis.qmd
# narrows it on load. It is declared here because accum_results consumes the
# filtered table, not the raw one — passing the raw table computes rarefaction
# over ocean and out-of-extent cells that no figure ever uses.
#
# Column renaming (wc2.1_2.5m_bio_1 -> bio1 etc.) is deliberately NOT done here.
# It belongs with the derived tables, and nothing that consumes this needs it.
filter_individuals <- function(data_analysis_individuals) {
  data_analysis_individuals |>
    # bio1 and population_size required downstream; NA marks grid-boundary or
    # ocean cells outside the climate raster extent.
    dplyr::filter(!is.na(wc2.1_2.5m_bio_1) & !is.na(population_size)) |>
    # x == 134.1, y == -25.7: desert cell with anomalous species composition
    # relative to its climate neighbours — inspect root cause before publication.
    # cell 2016 additionally removed from the individuals table.
    dplyr::filter(!(x == 134.1 & y == -25.7), cell != 2016)
}

# One row per grid cell, with climate and the population covariates.
#
# The grid/WorldClim alignment fix:
#
# The analysis grid is exactly commensurate with WorldClim (0.75 deg = 18 x 2.5
# arcmin, origins aligned), so all 2530 cell centres land EXACTLY on the corner
# where four WorldClim cells meet — the distance from a cell boundary is
# literally 0, not merely small. method = "simple" therefore has to break a
# four-way tie, and which cell wins depends on floating-point rounding of
# (x - xmin)/xres, hence on the raster's extent. Cropping the raster changed
# the answer in 1125 of 2530 cells, by up to 269 mm in bio12.
#
# method = "bilinear" removes it: at a corner the point is equidistant from all
# four cell centres, so the result is their mean at 0.25 weights each. Nothing
# to tie-break, and measured stable — 0 of 2530 cells differ under the crop
# that moved 1125 with "simple". It also yields FEWER NAs (1251 vs 1270),
# because it still returns a value where one neighbour is ocean.
#
# NB the population layers below stay on the default "simple" on purpose: they
# are already ON grid_square, so each point sits at its own cell's centre and
# there is no tie. The defect is specific to rasters at a different resolution.
# data_analysis_individuals likewise stays "simple" — occurrence coordinates are
# arbitrary reals, so they land inside cells rather than on corners.
make_data_grid_climate <- function(grid_square, climate_raster,
                                   pop_density, pop_accessibility,
                                   climate_method = "bilinear") {
  grid_tibble <- grid_square |>
    terra::xyFromCell(1:terra::ncell(grid_square)) |>
    tibble::as_tibble()
  grid_tibble$cell <- terra::extract(
    grid_square, cbind(grid_tibble$x, grid_tibble$y),
    cells = TRUE
  )$cell

  grid_tibble |>
    terra::vect(geom = c("x", "y"), crs = terra::crs(grid_square)) |>
    terra::extract(
      x = climate_raster, method = climate_method, bind = TRUE,
      xy = FALSE, cells = FALSE, ID = FALSE
    ) |>
    terra::extract(
      x = pop_density, method = "simple", bind = TRUE,
      xy = FALSE, cells = FALSE, ID = FALSE
    ) |>
    terra::extract(
      x = pop_accessibility, method = "simple", bind = TRUE,
      xy = FALSE, cells = FALSE, ID = FALSE
    ) |>
    tibble::as_tibble() |>
    # x/y were dropped by the vect() conversion above; rejoin them by cell.
    dplyr::left_join(grid_tibble, by = "cell") |>
    dplyr::select(x, y, cell, dplyr::everything()) |>
    # The .qmd applies this filter at the write_parquet() call
    # (Dataset_construction.qmd) rather than to the in-memory object.
    dplyr::filter(!is.na(cell), !is.na(x))
}
