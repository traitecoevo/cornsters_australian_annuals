# Life history classification from AusTraits.

# Returns one row per species with an "annual"/"perennial" classification.
#
# `austraits_path` is the cached AusTraits .rds returned by download_austraits().
# Taking the path as a declared input means the DAG depends on the AusTraits
# release itself, rather than on whatever load_austraits() fetches at build time.
extract_life_history <- function(austraits_path, version = apc_version) {
  austraits_data <- readRDS(austraits_path)

  # Wenk_2023 is the comprehensive life history dataset in AusTraits 7.0.0,
  # covering >20,000 Australian vascular plant species.
  # ~100 taxa have duplicate taxon_name entries with different original names;
  # all but one have identical life_history values and are safely collapsed here.
  species_life_history <- austraits_data |>
    austraits::extract_dataset("Wenk_2023") |>
    austraits::extract_trait("life_history") |>
    purrr::pluck("traits") |>
    dplyr::select(taxon_name, value) |>
    dplyr::group_by(taxon_name) |>
    dplyr::mutate(value = paste0(value, collapse = " ")) |>
    dplyr::ungroup() |>
    dplyr::distinct() |>
    dplyr::rename(species = taxon_name) |>
    # "annual" if "annual" appears anywhere in the value string. Species recorded
    # as both "annual" and "perennial" (facultative annuals) count as annual.
    dplyr::mutate(life_history = ifelse(grepl("annual", value), "annual", "perennial")) |>
    dplyr::select(-value)

  # Collaborator change, merged from master 1e8a032 (Dataset_construction.qmd).
  #
  # AusTraits 7.0.0 was aligned against APCalign 2025-05-09, so its taxon names
  # are stale relative to the release the rest of this pipeline now uses.
  # Re-align them and keep only accepted names.
  #
  # The result is SMALLER by design: names accepted in 2023, when the AusTraits
  # dataset was built, have since been reclassified as excluded, misapplied,
  # APNI-not-APC, and so on.
  #
  # DIVERGENCE FROM MASTER (1e8a032), deliberate. Their version ends with
  #
  #   select(species = accepted_name) |> left_join(species_life_history)
  #
  # which joins the NEW accepted name back against the ORIGINAL AusTraits names,
  # so it matches only where the name did not change. 175 of 29,822 names change
  # under this update and 161 of them find no row, taking life_history = NA —
  # precisely the species the re-alignment exists to repair. At record level that
  # is ~15,300 occurrences losing their classification, which
  # data_analysis_grid then drops via filter(!is.na(life_history)).
  #
  # Carrying original_name through and joining on THAT recovers the trait, while
  # accepted_name remains the label. Measured: 161 NAs -> 0.
  #
  # `resources =`, NOT `version =`. THIS PIN WAS NOT WORKING.
  #
  # create_taxonomic_update_lookup() declares a `version` argument and its body
  # never reads it: the only thing it uses is `resources`, whose default is
  # load_taxonomic_resources(quiet = quiet) — with no version passed through, so
  # it resolves through default_version() to whatever APC release is current.
  # Passing version = here was therefore silently ignored, and this step tracked
  # the latest release while align_taxonomy.R, which passes resources =, stayed
  # correctly pinned.
  #
  # HOW IT SURFACED: a clean rebuild two days after a warm one moved 15 species
  # between the native counts, with introduced counts untouched and
  # names_aligned bit-identical. A new APCalign release had landed in between,
  # which moved default_version() under a target that looked pinned. targets
  # cannot see this — the release is fetched inside the function, so no
  # dependency changes and nothing rebuilds — which is what let it drift
  # unnoticed.
  lookup <- APCalign::create_taxonomic_update_lookup(species_life_history$species,
    resources = apc_resources(version)
  ) |>
    dplyr::filter(taxonomic_status == "accepted") |>
    dplyr::select(original_name, species = accepted_name)

  lookup |>
    dplyr::left_join(species_life_history, by = c("original_name" = "species")) |>
    dplyr::select(species, life_history) |>
    # Several original names can collapse onto one accepted name — 19 groups
    # here, of which 16 agree and 3 genuinely conflict (Arthraxon australiensis,
    # Eragrostis lacunaria, Eragrostis nightingaleae). "annual" wins, which is
    # not an arbitrary tie-break: it is the same rule the classification above
    # already applies, where a taxon recorded as both annual and perennial (a
    # facultative annual) counts as annual.
    dplyr::group_by(species) |>
    dplyr::summarise(
      life_history = dplyr::if_else(any(life_history == "annual", na.rm = TRUE),
        "annual",
        dplyr::first(life_history)
      ),
      .groups = "drop"
    )
}
