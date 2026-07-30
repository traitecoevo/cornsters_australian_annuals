# GBIF occurrence cleaning.
#
# Filters applied sequentially to remove records with unreliable coordinates or
# provenance; each filter carries its rationale. The record
# counts in the comments are from the 2025-09-14 download (key
# 0001494-250914085247600): 24,385,439 raw -> 16,604,479 retained.

# Reference tables for CoordinateCleaner. Split out of clean_gbif() because they
# are static reference data, not part of the cleaning logic, and are easier to
# eyeball on their own.
gbif_ref_capitals <- function() {
  CoordinateCleaner::countryref |>
    dplyr::filter(iso2 == "AU") |>
    # Add capital cities missing from countryref.
    dplyr::bind_rows(
      tibble::tibble(
        capital.lat = c(-33.8688, -35.2809, -27.4698, -31.9505, -12.4634, -34.9285, -37.8136, -27.0000),
        capital.lon = c(151.2093, 149.1300, 153.0251, 115.8605, 130.8456, 138.6007, 144.9631, 151.0000),
        capital = c("Sydney", "Canberra", "Brisbane", "Melbourne", "Darwin", "Adelaide", "Hobart", "Perth"),
        name = c("New South Wales", "Australian Capital Territory", "Queensland", "Victoria", "Northern Territory", "South Australia", "Western Australia", "Tasmania")
      ) |>
        dplyr::mutate(iso3 = "AUS", iso2 = "AU", type = "province")
    )
}

gbif_ref_institutions <- function() {
  CoordinateCleaner::institutions |>
    dplyr::filter(country == "AUS") |>
    # Add missing institutions; CSIRO-Atherton is incorrect in the table.
    dplyr::bind_rows(tibble::tibble(
      decimalLatitude = -17.2586301,
      decimalLongitude = 145.482298,
      name = "CSIRO-Atherton"
    ))
}

# Columns read by Dataset_construction.qmd that are never referenced
# again — not in the filters, not in the CoordinateCleaner calls, and not
# downstream: Dataset_construction.qmd selects gbif_climate down to
# x, y, original_name, wc2.1* and population_size, discarding everything else.
#
# Three of them are ~unique per record (occurrenceID, catalogNumber,
# recordNumber), so they defeat R's global string cache: at 24.4M rows each
# costs a CHARSXP plus an 8-byte pointer per row, roughly 6.7 GB between them.
# They are then carried through terra::vect() at :309, which materialises every
# attribute for 16.6M points — the peak-memory step of the whole pipeline.
#
# establishmentMeans is GBIF's own field and is unused by design: the project
# derives establishment_means from APCalign instead.
#
# Dropping these changes the gbif_clean SCHEMA relative to the legacy
# downloads/GBIF/GBIF_aus_clean_*.parquet, though not a single retained value,
# since none of them participates in any filter.
#
# DEFAULT IS TO KEEP THEM. This branch's goal is exact replication, and the
# plan's validation step asserts equivalence with waldo::compare() on the
# parquet tables themselves, not only on the figures they feed. A schema that
# differs from the legacy artefact would fail that comparison for a reason
# unrelated to correctness, which is exactly the kind of noise that makes a
# validation step get ignored.
#
# The saving is real and measured — 5.1 GB and roughly half the read time at
# 24.4M rows — so this is a deliberate trade of memory for verifiability, not
# an oversight. Flip keep_unused_columns = FALSE to take it, once replication
# has been demonstrated and the legacy artefacts are no longer the reference.
gbif_unused_columns <- c(
  "gbifID", "occurrenceID", "establishmentMeans",
  "collectionCode", "catalogNumber", "recordNumber"
)

# The six CoordinateCleaner passes, applied in row blocks.
#
# WHY CHUNK. cc_cen() and cc_cap() both compute their reference-crop extent as
#
#   limits <- terra::ext(terra::buffer(dat, width = buffer))
#
# which buffers every input point into a polygon just to obtain a bounding box,
# so their memory scales with nrow(x). cc_inst() does not have the problem — it
# uses the cheap terra::ext(dat) + buffer.
#
# Reported upstream: ropensci/CoordinateCleaner#113. If that lands, this
# chunking becomes unnecessary rather than wrong — revisit then, do not assume.
#
# Measured on 3M real GBIF records, peak process RSS:
#
#   unchunked     9.34 GB   69.2 s
#   2M blocks     7.48 GB   67.1 s
#   1M blocks     3.94 GB   66.9 s   <- default
#
# All three return byte-identical tables. Wall time is flat, so the block size
# trades nothing for a 58% cut in peak memory. Unchunked peak at the ~19.2M
# records that reach these calls was projected at 14-24 GB, which does not fit
# alongside the live table on a 32 GB machine.
#
# WHY IT IS EXACT, not an approximation. Every pass tests each record against a
# fixed reference set, so no pass depends on which other records are present.
# The one subtlety is the per-chunk reference crop, and it is safe in the
# direction that matters: `limits` is the extent of the *buffered* points, so it
# contains every reference within `buffer` of any record in that chunk. A
# smaller chunk therefore cannot silently drop a match — it can only retain
# fewer references that were never going to match anyway.
#
# Chunking is a memory measure only. It does not speed anything up, and the
# blocks are processed in order so row order is preserved.
clean_gbif_coordinates <- function(x, chunk_rows = 1e6) {
  ref_capitals <- gbif_ref_capitals()
  ref_institutions <- gbif_ref_institutions()

  run_passes <- function(d) {
    d |>
      CoordinateCleaner::cc_val() |>
      CoordinateCleaner::cc_equ() |>
      CoordinateCleaner::cc_gbif() |>
      CoordinateCleaner::cc_cen(buffer = 5000, ref = ref_capitals) |> # 5 km: country/state centroid defaults
      CoordinateCleaner::cc_cap(buffer = 2000, ref = ref_capitals) |> # 2 km: capital-city default coords, without discarding genuine urban records
      CoordinateCleaner::cc_inst(buffer = 2000, ref = ref_institutions) # 2 km: herbarium/zoo in-garden accessions
  }

  if (is.null(chunk_rows) || nrow(x) <= chunk_rows) {
    return(run_passes(x))
  }

  blocks <- split(seq_len(nrow(x)), ceiling(seq_len(nrow(x)) / chunk_rows))
  message(
    "CoordinateCleaner: ", length(blocks), " blocks of up to ",
    format(chunk_rows, big.mark = ","), " rows"
  )
  out <- lapply(seq_along(blocks), function(k) {
    r <- run_passes(x[blocks[[k]], ])
    gc() # release the block's terra allocations before the next one starts
    r
  })
  data.table::rbindlist(out)
}

# Reads the raw GBIF export and returns the cleaned occurrence table.
#
# `gbif_csv` is the raw SIMPLE_CSV download. It is tab-delimited despite the
# extension, and only data.table::fread() reads it successfully — both
# readr::read_csv() and arrow::read_csv_arrow() error on it.
# `chunk_rows` caps the block size handed to CoordinateCleaner; see
# clean_gbif_coordinates() for why. NULL disables chunking.
clean_gbif <- function(gbif_csv, keep_unused_columns = TRUE, chunk_rows = 1e6) {
  read_columns <- c(
    "gbifID", "occurrenceID", "occurrenceStatus", "decimalLongitude", "decimalLatitude",
    "basisOfRecord", "establishmentMeans", "coordinatePrecision", "coordinateUncertaintyInMeters",
    "issue", "species", "taxonRank", "year", "countryCode", "institutionCode",
    "collectionCode", "catalogNumber", "recordNumber"
  )
  if (!keep_unused_columns) {
    read_columns <- setdiff(read_columns, gbif_unused_columns)
  }

  aus_gbif <- data.table::fread(gbif_csv, quote = "", select = read_columns)

  aus_gbif <- aus_gbif |> # 24385439
    dplyr::filter(countryCode == "AU") |> # already filtered on the GBIF website
    dplyr::filter(occurrenceStatus == "PRESENT") |>
    dplyr::filter(!is.na(decimalLongitude)) |>
    dplyr::filter(!is.na(decimalLatitude)) |>
    dplyr::filter(!basisOfRecord %in% c("LIVING_SPECIMEN", "FOSSIL_SPECIMEN")) |>
    dplyr::filter(coordinatePrecision < 0.05 | is.na(coordinatePrecision)) |> # retains most herbarium records
    dplyr::filter(year >= 1900) |> # avoids pre-systematic-botany records with unreliable locations
    dplyr::filter(coordinateUncertaintyInMeters < 10000 | is.na(coordinateUncertaintyInMeters)) |> # ~township scale
    dplyr::filter(!(decimalLatitude == 0 | decimalLongitude == 0)) |>
    dplyr::filter(
      !grepl("COUNTRY_COORDINATE_MISMATCH", issue) &
        !grepl("RECORDED_DATE_UNLIKELY", issue)
    ) |>
    # SUBSPECIES/VARIETY included for breadth; richness is counted at species level downstream.
    dplyr::filter(taxonRank %in% c("SPECIES", "SUBSPECIES", "VARIETY"), !is.na(species)) |>
    dplyr::filter(
      !(institutionCode == "NHMUK"), # digitising of herbarium sheets has proven unreliable (Thomas Mesaglio, pers. comm.)
      !(institutionCode == "CJBG") # 209 records with many georeferencing errors
    ) |>
    dplyr::select(-occurrenceStatus, -countryCode, -coordinatePrecision, -basisOfRecord)

  # 19178225 at this point.

  # Remove points sitting on capitals, state/country centroids and institutions.
  aus_gbif <- clean_gbif_coordinates(aus_gbif, chunk_rows = chunk_rows)

  # 17968776 at 20 km buffers; 18882514 at the 2 km buffers used here.

  # Deduplicate by location x species x year. Retains repeat visits in different
  # years (genuine resurveys) but removes within-year replicates from the same spot.
  aus_gbif |>
    dplyr::distinct(decimalLongitude, decimalLatitude, species, year, .keep_all = TRUE)

  # 15756068 at 20 km buffers; 16604479 at the 2 km buffers used here.
}
