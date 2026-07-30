# Taxonomy alignment against the Australian Plant Census.
#
# Aligns GBIF names to the Australian Plant Census (APC) via APCalign, and
# attaches native/introduced status.

# THE APC RELEASE IS PINNED, and pinning it is the point.
#
# load_taxonomic_resources() otherwise defaults to whatever APC has published
# most recently, so a rebuild months later would silently reclassify species
# relative to published results.
#
# ONE VERSION, DELIBERATELY. Running name alignment at 2025-08-19 while
# filtering against 2026-03-25's accepted list would be incoherent: names
# resolved under the old release would be tested against the new release's
# accepted set, dropping taxa for reasons that are an artefact of the mismatch
# rather than of taxonomy. Every APCalign call in this pipeline uses the
# constant below.
#
# Changing it remains a scientific decision, not a maintenance one: it moves
# species between native/introduced and shifts richness counts.
# APCalign::get_versions() lists what is available.
apc_version <- "2026-03-25"

# The APC-accepted canonical names, as a plain character vector.
#
# Split out so the DAG tracks it as its own target: load_taxonomic_resources()
# returns a large multi-table object and hits the network, but only this one
# column is used downstream (to drop non-accepted taxa from the occurrence
# table). Keeping the vector rather than the resources object also means a
# change in some unrelated APC table does not invalidate the occurrence join.
apc_accepted_names <- function(version = apc_version) {
  APCalign::load_taxonomic_resources(version = version)$APC_accepted$canonical_name
}

# Returns one row per unique GBIF species name, with its APC-aligned name and
# native/introduced status.
#
# `species` is the character vector of unique names from the cleaned GBIF table.
align_taxonomy <- function(species, version = apc_version) {
  resources <- APCalign::load_taxonomic_resources(version = version)

  gbif_names_aligned <-
    species |>
    unique() |>
    # taxonomic_splits = "most_likely_species": when a GBIF name maps to multiple
    # APC names, take the single most likely match rather than splitting records,
    # to avoid artificially inflating species counts from taxonomic uncertainty.
    APCalign::create_taxonomic_update_lookup(
      resources = resources,
      taxonomic_splits = "most_likely_species"
    )

  # Strip the alternate-names text on names with possible splits. Those names
  # are not assigned a nativeness or life history because they are not formatted
  # correctly downstream.
  gbif_names_aligned <- gbif_names_aligned |>
    dplyr::mutate(suggested_name = stringr::str_remove(suggested_name, " \\[.*"))

  # native_anywhere_in_australia() returns "native", "introduced", or "unknown".
  # "unknown" covers species absent from APC or with ambiguous origin; these are
  # filtered out in Analysis.qmd (establishment_means != "unknown").
  native_lookup <-
    gbif_names_aligned$suggested_name |>
    APCalign::native_anywhere_in_australia(resources = resources) |>
    dplyr::distinct()

  # ~1243 taxa carry no native status. Confirmed to be APNI, genus-rank,
  # family-rank, or explicitly excluded by APC. Two further names are "accepted"
  # but are hybrids and do not match; those are accepted as dropped.
  gbif_names_aligned |>
    dplyr::left_join(native_lookup, by = dplyr::join_by(suggested_name == species))
}
