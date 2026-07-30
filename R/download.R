# Canonical acquisition of every raw input.
#
# Each function downloads its input if absent and returns the path(s) to the
# file(s) on disk, which is what a tar_target(format = "file") needs. Every
# input the analysis uses is reachable programmatically, so the pipeline runs
# from a clean checkout with no manual downloads and no machine-specific paths.
# That is what resolves the path-convention question on issue #44 — there is
# nothing left to symlink.
#
# The download-if-absent guard here is NOT the anti-pattern the migration is
# removing. Re-fetching 4 GB on every build would be absurd; the point is that
# targets hashes the returned file, so downstream work invalidates on the
# file's CONTENT rather than on its mere existence.

# --- GBIF ------------------------------------------------------------------

# The exact download the analysis is built on. Citable and frozen: GBIF retains
# executed downloads, so this key resolves to the same 24,385,439 records
# indefinitely rather than to "whatever GBIF holds today".
gbif_download_key <- "0001494-250914085247600"
gbif_download_doi <- "10.15468/dl.b672m4"

# Returns the path to the raw occurrence CSV (~4 GB zipped, SIMPLE_CSV format,
# which is tab-delimited despite the extension — hence fread(quote = "")).
# No GBIF credentials are needed to retrieve an already-executed download.
download_gbif <- function(dest_dir = "downloads/GBIF", key = gbif_download_key) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  csv <- file.path(dest_dir, paste0(key, ".csv"))
  if (file.exists(csv)) {
    return(csv)
  }

  zip <- file.path(dest_dir, paste0(key, ".zip"))
  if (!file.exists(zip)) {
    url <- paste0("https://api.gbif.org/v1/occurrence/download/request/", key, ".zip")
    message("Downloading GBIF ", key, " (~4 GB) — this is slow but happens once.")
    # timeout is per-connection and defaults to 60s, nowhere near enough here.
    withr::with_options(
      list(timeout = 60 * 60),
      utils::download.file(url, zip, mode = "wb")
    )
  }
  utils::unzip(zip, exdir = dest_dir)
  if (!file.exists(csv)) {
    stop(
      "GBIF zip did not contain the expected ", basename(csv),
      ". Contents: ", paste(utils::unzip(zip, list = TRUE)$Name, collapse = ", ")
    )
  }
  csv
}

# --- ABS population grid ---------------------------------------------------

# Australian Population Grid 2011 (ABS cat. 1270.0.55.007), 1 km GDA94 Albers.
# This is a legacy Lotus Notes link carrying an embedded document id. It has
# been stable since 2014, but it is not a scheme ABS guarantees, so the payload
# is checksummed rather than trusted — see verify_sha256 below.
abs_pop_url <- paste0(
  "https://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent",
  "&australian_population_grid_2011_tif_format.zip",
  "&1270.0.55.007&Data%20Cubes&E0D7D30C837EFC26CA257DB10016122C&0&2011&18.12.2014&Latest"
)
abs_pop_zip_sha256 <- "0e324921666a990ffd7e9fa8d8870cc7d9b59d57135b0de45b8f68b902f0dbde"

# Returns the path to Australian_Population_Grid_2011.tif. The extracted
# directory name matches what Dataset_construction.qmd already expects, so
# this drops in without touching the read side.
download_abs_population <- function(dest_dir = "downloads",
                                    expected_sha256 = abs_pop_zip_sha256) {
  out_dir <- file.path(dest_dir, "australian_population_grid_2011_tif_format")
  tif <- file.path(out_dir, "Australian_Population_Grid_2011.tif")
  if (file.exists(tif)) {
    return(tif)
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  zip <- file.path(dest_dir, "australian_population_grid_2011_tif_format.zip")
  if (!file.exists(zip)) {
    utils::download.file(abs_pop_url, zip, mode = "wb")
  }
  verify_sha256(zip, expected_sha256, "ABS population grid")
  utils::unzip(zip, exdir = out_dir)
  if (!file.exists(tif)) stop("ABS zip did not contain ", basename(tif))
  tif
}

# --- WorldClim -------------------------------------------------------------

# WorldClim 2.1 bioclim at 2.5 arcmin, via geodata rather than a hand-fetched
# zip. Same files (wc2.1_2.5m_bio_*.tif), but versioned and scripted.
#
# NB geodata stores these under <dest_dir>/climate/wc2.1_2.5m/, NOT the
# downloads/wc2.1_2.5m_bio/ that Dataset_construction.qmd hardcodes. When
# the climate layers are loaded they must take paths from this target
# rather than rebuilding that string, and the old directory (plus the 658 MB
# wc2.1_2.5m_bio.zip beside it) then becomes redundant.
download_worldclim <- function(dest_dir = "downloads", res = "2.5",
                               vars = c(1, 12, 14, 15, 17)) {
  r <- geodata::worldclim_global(var = "bio", res = res, path = dest_dir)
  if (is.null(r)) stop("geodata::worldclim_global() returned NULL — download failed.")
  fres <- if (identical(as.character(res), "0.5")) "30s" else paste0(res, "m")
  paths <- file.path(
    dest_dir, "climate", paste0("wc2.1_", fres),
    paste0("wc2.1_", fres, "_bio_", vars, ".tif")
  )
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop(
      "WorldClim files missing after download: ",
      paste(basename(missing), collapse = ", ")
    )
  }
  paths
}

# --- AusTraits -------------------------------------------------------------

# AusTraits is already scripted; this only pins the version and surfaces the
# cached .rds as a path so it can be tracked as a file input. v7.0.0 is Zenodo
# doi 10.5281/zenodo.15718081.
download_austraits <- function(dest_dir = "downloads/austraits", version = "7.0.0") {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  # Short-circuit on the cached file. load_austraits() is cache-aware but still
  # reads and deserialises the whole 19 MB object to hand it back, which costs
  # ~9 s on every pipeline run for a function whose only job is to return a path.
  cached <- file.path(dest_dir, paste0("austraits-", version, ".rds"))
  if (file.exists(cached)) {
    return(cached)
  }

  austraits::load_austraits(path = dest_dir, version = version)
  # load_austraits() names the cache after the Zenodo asset; find it rather
  # than hardcoding, since the naming has changed between releases.
  hits <- list.files(dest_dir, pattern = "\\.rds$", full.names = TRUE)
  hits <- hits[!grepl("flattened", hits)]
  if (!length(hits)) stop("load_austraits() left no .rds cache in ", dest_dir)
  hits[which.max(file.info(hits)$size)]
}

# --- Natural Earth ---------------------------------------------------------

# Ocean and land outlines, used for plotting and for masking the population
# kernel.
#
# NOTE: rnaturalearth 1.2.0 writes a GeoPackage, not a
# shapefile. Dataset_construction.qmd, :416 and :461 all check for and
# read ne_10m_{ocean,land}.shp, so those reads are already broken against the
# installed version — the guard sees no .shp, re-downloads every render, and
# then st_read() fails on a file that was never written. Ported code must take
# the path from this function rather than rebuilding a ".shp" string.
download_naturalearth <- function(dest_dir = "downloads") {
  vapply(c("ocean", "land"), function(type) {
    d <- file.path(dest_dir, paste0("ne_10m_", type))
    # .gpkg from rnaturalearth >= 1.1, .shp from older versions; accept either
    # so this keeps working across the version collaborators happen to have.
    found <- function() {
      list.files(d,
        pattern = paste0("^ne_10m_", type, "\\.(gpkg|shp)$"),
        full.names = TRUE
      )
    }
    if (!length(found())) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
      rnaturalearth::ne_download(
        scale = 10, type = type, category = "physical",
        returnclass = "sf", destdir = d
      )
    }
    hits <- found()
    if (!length(hits)) {
      stop(
        "Natural Earth download produced no .gpkg or .shp for '", type,
        "' in ", d, ". Present: ", paste(list.files(d), collapse = ", ")
      )
    }
    hits[[1]]
  }, character(1), USE.NAMES = FALSE)
}

# --- helpers ---------------------------------------------------------------

verify_sha256 <- function(path, expected, label) {
  if (is.null(expected)) {
    return(invisible(path))
  }
  # paste0() rather than as.character(): openssl returns a classed "hash"
  # object and as.character() preserves that class, so identical() against a
  # plain string is FALSE even when the hex matches.
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  got <- paste0(openssl::sha256(con))
  if (!identical(got, expected)) {
    stop(
      label, " checksum mismatch.\n  expected: ", expected, "\n  got:      ", got,
      "\nThe upstream file has changed. Confirm the new payload is the same ",
      "product before updating the expected hash — a silent change here would ",
      "silently change the results."
    )
  }
  invisible(path)
}

# --- Inter (typeface) -------------------------------------------------------

# Inter, SIL Open Font License 1.1, https://github.com/rsms/inter
#
# WHY THIS IS VENDORED RATHER THAN ASSUMED INSTALLED. If a figure asks for a
# font that is not present, R substitutes a default SILENTLY — no warning, no
# log entry. The figure simply looks different on that machine. That is the
# same class of failure as the hand-rolled caches this migration removed: a
# difference that propagates because nothing complains about it.
#
# Fetching the font makes it a tracked input like any other, so every
# collaborator renders with the same typeface and nobody has to install
# anything. Inter is not present on this machine by default (checked: 545
# system fonts, no Inter), so assuming it would have failed immediately.
#
# Only the four faces ggplot2 actually uses are retained. The archive also
# ships variable fonts, which ragg can use but which complicate weight
# selection for no benefit here.
inter_version <- "4.1"
inter_faces <- c("Regular", "Bold", "Italic", "BoldItalic")

download_inter <- function(dest_dir = "downloads/fonts", version = inter_version) {
  out_dir <- file.path(dest_dir, paste0("Inter-", version))
  paths <- file.path(out_dir, paste0("Inter-", inter_faces, ".ttf"))
  if (all(file.exists(paths))) {
    return(paths)
  }

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  zip <- file.path(dest_dir, paste0("Inter-", version, ".zip"))
  if (!file.exists(zip)) {
    url <- paste0(
      "https://github.com/rsms/inter/releases/download/v",
      version, "/Inter-", version, ".zip"
    )
    withr::with_options(
      list(timeout = 600),
      utils::download.file(url, zip, mode = "wb")
    )
  }

  wanted <- file.path("extras/ttf", paste0("Inter-", inter_faces, ".ttf"))
  utils::unzip(zip, files = wanted, exdir = out_dir, junkpaths = TRUE)

  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop("Inter archive did not contain: ", paste(basename(missing), collapse = ", "))
  }
  paths
}

# Registers Inter with systemfonts under the family name "Inter".
#
# Must be called in the process that renders, so it lives here rather than in a
# setup chunk: targets builds each target in its own process and registrations
# do not persist across them.
#
# Returns the family name, so a caller that wants to fail loudly rather than
# fall back can check it.
register_inter <- function(font_paths, family = "Inter") {
  idx <- stats::setNames(seq_along(inter_faces), inter_faces)
  systemfonts::register_font(
    name = family,
    plain = font_paths[[idx[["Regular"]]]],
    bold = font_paths[[idx[["Bold"]]]],
    italic = font_paths[[idx[["Italic"]]]],
    bolditalic = font_paths[[idx[["BoldItalic"]]]]
  )
  if (!family %in% systemfonts::registry_fonts()$family) {
    stop("Inter did not register; figures would silently fall back to a default.")
  }
  family
}
