# Derived analysis tables.
#
# These are the tables Analysis.qmd builds on load, before any figure or model.
# The renaming to bio1/bio12/... happens here rather than in dataset
# construction, matching the .qmd: the parquet on disk carries the raw WorldClim
# names and the analysis side renames them.

# data_grid_climate as Analysis.qmd uses it (:23-38): filtered and renamed.
prepare_grid_climate <- function(data_grid_climate) {
  data_grid_climate |>
    # bio1 and pop_density required for effort diagnostics; NA marks grid
    # boundary or ocean cells outside the climate raster extent.
    dplyr::filter(!is.na(wc2.1_2.5m_bio_1) & !is.na(pop_density)) |>
    # x == 134.1, y == -25.7: desert cell with anomalous species composition
    # relative to its climate neighbours — inspect root cause before publication.
    dplyr::filter(!(x == 134.1 & y == -25.7)) |>
    dplyr::rename(
      bio1  = wc2.1_2.5m_bio_1,
      bio12 = wc2.1_2.5m_bio_12,
      bio14 = wc2.1_2.5m_bio_14,
      bio15 = wc2.1_2.5m_bio_15,
      bio17 = wc2.1_2.5m_bio_17
    ) |>
    dplyr::rename(pop_accessibility = pop_accessibility_250km)
}

# Richness counts per cell x establishment_means x life_history.
# Analysis.qmd, including the sanity check at :84-88 turned into a real
# assertion — see below.
make_data_analysis_grid <- function(individuals_filtered) {
  out <- individuals_filtered |>
    # TODO: characterise what fraction of records lack life_history or
    # establishment_means, and whether this gap is taxonomically or spatially
    # structured, before publication.
    dplyr::filter(!is.na(life_history), establishment_means != "unknown") |>
    dplyr::select(cell, species, establishment_means, life_history) |>
    # n_obs_per_cell counts only classifiable records, not all records in the
    # cell. Intentional — effort should reflect usable data — but it understates
    # raw sampling effort where many species lack trait classifications.
    dplyr::group_by(cell) |>
    dplyr::mutate(
      n_obs_per_cell = dplyr::n(),
      n_spp_per_cell = dplyr::n_distinct(species)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(cell, establishment_means, life_history) |>
    dplyr::summarise(
      n_obs_per_cell = dplyr::first(n_obs_per_cell),
      n_spp_per_cell = dplyr::first(n_spp_per_cell),
      n_spp = dplyr::n_distinct(species),
      n_obs = dplyr::n(),
      .groups = "drop"
    )

  # Analysis.qmd used to compute the rows below and print
  # them, with a comment saying it "should return 0 rows" and a stale note
  # claiming data_analysis_grid is defined later (it is defined directly above).
  # Printed output nobody reads is not a check, so it becomes an assertion here.
  #
  # WHAT THE INVARIANT ACTUALLY IS. It does NOT follow structurally: this table
  # drops records with NA life_history or unknown establishment_means, so a cell
  # holding only unclassifiable records would legitimately be absent, and the
  # check would fire without anything being wrong.
  #
  # Measured on current data: 0 orphan rows, and all 1,427 cells survive the
  # filter — every cell contains at least one classifiable record. So the
  # expectation is correct today, but it is an empirical property of the data
  # rather than a guarantee.
  #
  # Hence: assert, but fail with an explanation rather than a bare error, since
  # the likely cause of a future failure is benign.
  orphans <- setdiff(unique(individuals_filtered$cell), unique(out$cell))
  if (length(orphans)) {
    stop(
      "data_analysis_grid is missing ", length(orphans), " cell(s) present in ",
      "the individuals table: ", paste(utils::head(orphans, 10), collapse = ", "),
      if (length(orphans) > 10) ", ..." else "", ".\n",
      "This means those cells contain ONLY records with NA life_history or ",
      "establishment_means == 'unknown'. That is possible in principle and was ",
      "not true when this assertion was written (0 such cells over 1,427). ",
      "Check whether trait coverage has changed before assuming a bug."
    )
  }

  out
}

# data_analysis_grid pivoted wide, one row per cell x establishment_means,
# joined to the grid climate..
make_data_prop_annual <- function(data_analysis_grid, grid_climate) {
  data_analysis_grid |>
    dplyr::select(-n_obs) |>
    # values_fill = 0: cells where a group has no records get n = 0. Filtered to
    # n_annual > 1 & n_perennial > 1 in data_model to avoid modelling zeros.
    tidyr::pivot_wider(
      names_from = life_history, values_from = n_spp,
      values_fill = 0
    ) |>
    dplyr::rename(n_annual = annual, n_perennial = perennial) |>
    dplyr::mutate(
      n_species_total = n_annual + n_perennial,
      frac_annuals = n_annual / n_species_total
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(grid_climate, by = "cell") |>
    dplyr::filter(!is.na(x))
}

# One row per cell, with the native/introduced fraction-annuals offset.
# .
#
# DEFERRED DEFECT, ported as-is on purpose. Cells with only introduced records
# get native = NA, and frac_offset = native - introduced propagates that NA
# rather than treating it as 0 — note that `introduced` IS coalesced to 0 and
# `native` is not, so the handling is asymmetric. Analysis.qmd flags this.
# Fixing it changes results, so it belongs in the changes-on-top phase, not
# here.
#
# Currently latent rather than active: every cell holding introduced records
# also holds native ones, so `native` has no NAs and frac_offset none either.
# That could stop being true, which is why the note stays.
make_missing_fraction <- function(data_prop_annual) {
  data_prop_annual |>
    dplyr::select(
      cell, x, y, establishment_means, frac_annuals, bio12, bio15,
      pop_density, pop_accessibility_100km, pop_accessibility,
      pop_accessibility_500km
    ) |>
    tidyr::pivot_wider(names_from = establishment_means, values_from = frac_annuals) |>
    dplyr::mutate(
      introduced = ifelse(is.na(introduced), 0, introduced),
      frac_offset = native - introduced
    )
}

# The fig3 modelling frame, before any effort cutoff..
#
# bio12 > 0 and bio15 > 0 because both are log/ratio-scaled in the models;
# n_annual > 1 and n_perennial > 1 to avoid modelling zeros introduced by the
# values_fill = 0 in make_data_prop_annual().
make_data_model_base <- function(data_prop_annual) {
  data_prop_annual |>
    dplyr::filter(bio12 > 0, bio15 > 0, n_annual > 1, n_perennial > 1)
}

# Effort cutoff, applied as a separate step so the sensitivity series and the
# primary model come from one code path.
#
# NB Analysis.qmd writes the primary frame (cutoff 500) as its own filter
# chain and the sensitivity frames at :657-670 as another. The two are
# equivalent — same predicates, different order — so the primary model here is
# the cutoff-500 branch rather than a separate target.
apply_effort_cutoff <- function(data_model_base, cutoff) {
  dplyr::filter(data_model_base, n_obs_per_cell >= cutoff)
}
