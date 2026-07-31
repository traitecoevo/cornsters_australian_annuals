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

# THE ONLY PLACE load_taxonomic_resources() MAY BE CALLED. Every APCalign call
# in this pipeline takes its resources from here.
#
# The paragraph above used to be enforced by passing `version = apc_version` to
# each APCalign function. That does not work, and failed silently for
# create_taxonomic_update_lookup(): it declares a `version` argument that its
# body never reads, and falls back to a `resources` default of
# load_taxonomic_resources(quiet = quiet) — no version — which resolves through
# default_version() to whatever APC release is current. R/life_history.R
# therefore tracked the newest release while this file stayed pinned, and
# nothing anywhere reported a conflict.
#
# It surfaced only because a new APCalign release landed between two builds of
# the same commit and moved 15 species between the native counts. targets cannot
# catch this on its own: the release is fetched inside the function, so no
# tracked dependency changes and no target rebuilds.
#
# PASS resources =, NEVER version =. An APCalign entry point that accepts
# `resources` is using the pin; one given only `version` may or may not be, and
# the difference is invisible at the call site.
apc_resources <- local({
  cache <- new.env(parent = emptyenv())
  function(version = apc_version) {
    key <- as.character(version)
    if (is.null(cache[[key]])) {
      # NB the column is `versions`, plural. Getting it wrong yields NULL and a
      # warning rather than an error, which disables this check silently — the
      # same failure mode as the pin it exists to protect.
      available <- tryCatch(APCalign::get_versions()$versions,
        error = function(e) NULL
      )
      # Only an assertion when the list is actually retrievable; offline, the
      # cached resources below may still work and should not be blocked.
      if (!is.null(available) && !key %in% as.character(available)) {
        stop(
          "APC release '", key, "' is not among the versions APCalign offers.\n",
          "  available: ", paste(utils::head(as.character(available), 8), collapse = ", "),
          "\n  Set apc_version in R/align_taxonomy.R to one of these. Changing it ",
          "moves species between native and introduced, so it is a scientific ",
          "decision rather than a maintenance one.",
          call. = FALSE
        )
      }
      cache[[key]] <- APCalign::load_taxonomic_resources(version = version)
    }
    cache[[key]]
  }
})

# The APC-accepted canonical names, as a plain character vector.
#
# Split out so the DAG tracks it as its own target: load_taxonomic_resources()
# returns a large multi-table object and hits the network, but only this one
# column is used downstream (to drop non-accepted taxa from the occurrence
# table). Keeping the vector rather than the resources object also means a
# change in some unrelated APC table does not invalidate the occurrence join.
apc_accepted_names <- function(version = apc_version) {
  apc_resources(version)$APC_accepted$canonical_name
}

# Returns one row per unique GBIF species name, with its APC-aligned name and
# native/introduced status.
#
# `species` is the character vector of unique names from the cleaned GBIF table.
align_taxonomy <- function(species, version = apc_version) {
  resources <- apc_resources(version)

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
