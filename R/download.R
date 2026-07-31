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

# --- download helpers ------------------------------------------------------

# A TOTAL-TRANSFER TIMEOUT IS THE WRONG TOOL for a 4 GB file, and this file used
# one. R's `timeout` option becomes libcurl's CURLOPT_TIMEOUT, a cap on the whole
# transfer rather than on any single stalled read — ?download.file calls 60 s
# "often insufficient for downloads of large files (50MB or more)". The GBIF
# fetch below raised it to an hour, which aborted any download not finished
# within the hour: not one that had died, one that was merely slow. A
# collaborator on a domestic connection hit exactly that. At 1.5 MB/s the
# archive needs ~45 min of uninterrupted throughput and any hiccup pushes it
# past the cap.
#
# What should abort a download is a DEAD connection, which is what libcurl's
# low-speed guard expresses directly: give up only after a sustained period
# below a floor rate. A slow but live transfer then runs for as long as it
# needs to.
download_stall_bytes <- 1024L # bytes/sec ...
download_stall_seconds <- 120L # ... sustained for this long means dead, not slow

# Floor for the no-curl fallback and for the third-party downloaders we cannot
# pass curl options to (austraits, rnaturalearth). max() rather than plain
# assignment because ?download.file asks packages not to DECREASE a timeout the
# user raised through R_DEFAULT_INTERNET_TIMEOUT — which the
# list(timeout = 60 * 60) this replaced silently did.
#
# BOUNDED, not enormous. A total timeout cannot tell slow from dead, so the only
# question it answers is "how long should a dead connection hang before someone
# is told". These two inputs are ~20 MB each; 30 minutes is absurdly generous for
# them and still fails while a person is watching. The 628 MB and 4 GB downloads
# do not rely on this at all — they go through download_resumable(), which has a
# real stall guard.
download_timeout_floor <- 30L * 60L

with_generous_timeout <- function(expr) {
  withr::with_options(
    list(timeout = max(download_timeout_floor, getOption("timeout", 0L))),
    expr
  )
}

# Fetch `url` to `dest`, resuming a part-finished attempt rather than restarting.
#
# WHY NOT download.file(): no resume, and that turned one timeout into two
# failures. Every guard in this file is `if (!file.exists(zip))`, so a truncated
# file counted as a finished one: the next tar_make() skipped the download and
# handed the truncation to unzip(), which then failed with a complaint about the
# archive rather than about the transfer. The collaborator paid for the whole
# download twice and was told the wrong cause the second time.
#
# The .part staging is what makes `file.exists(dest)` mean "complete" — dest
# only ever appears via the rename at the end — and it is also what curl resumes
# into across runs.
format_bytes <- function(n) {
  if (is.na(n) || n <= 0) {
    return("0 B")
  }
  units <- c("B", "kB", "MB", "GB")
  i <- min(length(units), 1L + floor(log(n, 1024)))
  sprintf("%.*f %s", if (i > 2) 2L else 0L, n / 1024^(i - 1), units[[i]])
}

host_of <- function(url) sub("^(https?://[^/]+).*", "\\1", url)

# Ask the host for the file's size before committing to the transfer, and say
# what came back.
#
# ADVISORY, never fatal. It exists because a stalled download is
# indistinguishable from a slow one in the log — curl prints "0 b/s" either way,
# and a collaborator watching a run sit at zero bytes has no way to tell whether
# to wait or to interrupt. Answering "the host is not responding" up front is the
# whole point.
#
# Not fatal because the ABS link is a Lotus Notes URL with an embedded document
# id, and there is no reason to assume every such endpoint answers a HEAD
# request the way it answers a GET. A probe that refused to proceed would turn a
# working download into a failure. When the host really is dead, the stall guard
# in the transfer below is what stops us, correctly and with a message.
probe_size <- function(url, label, seconds = 20L) {
  if (!requireNamespace("curl", quietly = TRUE)) {
    return(invisible(NULL))
  }
  h <- curl::new_handle(
    nobody = TRUE, followlocation = TRUE,
    connecttimeout = seconds, timeout = seconds + 10L
  )
  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = identity)

  if (inherits(res, "error")) {
    message(
      "  ", host_of(url), " did not answer a size request (",
      conditionMessage(res), ").\n",
      "  Trying the download anyway. If the host is down this will stop after ",
      download_stall_seconds, "s without progress rather than hang."
    )
    return(invisible(NULL))
  }
  n <- suppressWarnings(as.numeric(
    curl::parse_headers_list(res$headers)[["content-length"]]
  ))
  if (length(n) != 1 || is.na(n) || n <= 0) {
    return(invisible(NULL))
  }
  message("  ", label, ": ", format_bytes(n), " reported by ", host_of(url))
  invisible(n)
}

download_resumable <- function(url, dest, label, size_hint = NULL) {
  part <- paste0(dest, ".part")
  resumed_from <- if (file.exists(part)) file.size(part) else 0

  message(
    "Downloading ", label,
    if (!is.null(size_hint)) paste0(" (", size_hint, ")") else "",
    if (resumed_from > 0) {
      paste0(" — resuming, ", format_bytes(resumed_from), " already on disk")
    } else {
      ""
    },
    ". Slow connections are fine; an interrupted download resumes on the next ",
    "targets::tar_make()."
  )
  total <- probe_size(url, label)
  if (!is.null(total) && resumed_from > 0) {
    message(sprintf(
      "  %s of %s still to fetch (%.0f%% already done).",
      format_bytes(total - resumed_from), format_bytes(total),
      100 * resumed_from / total
    ))
  }
  started <- Sys.time()

  if (requireNamespace("curl", quietly = TRUE)) {
    # NB no `timeout` handle option is set, deliberately — that would reinstate
    # the total-transfer cap this function exists to remove. multi_download()
    # applies no overall limit of its own (multi_timeout defaults to Inf).
    res <- curl::multi_download(
      url, part,
      resume = TRUE, progress = TRUE,
      low_speed_limit = download_stall_bytes,
      low_speed_time = download_stall_seconds
    )
    if (!isTRUE(res$success[[1]]) || !file.exists(part)) {
      why <- if (is.na(res$error[[1]])) {
        paste("HTTP status", res$status_code[[1]])
      } else {
        res$error[[1]]
      }
      have <- if (file.exists(part)) file.size(part) else 0
      stop(
        label, " download did not complete: ", why, ".\n",
        if (have > 0) {
          paste0(
            "  ", format_bytes(have), " is kept at ", part,
            " — re-run targets::tar_make() to carry on from there rather than ",
            "starting again."
          )
        } else {
          paste0(
            "  Nothing was fetched, so ", host_of(url), " is most likely down ",
            "rather than slow. This is not a problem with your setup and ",
            "nothing has been lost: re-run targets::tar_make() when the host ",
            "is back and the pipeline continues from here."
          )
        },
        call. = FALSE
      )
    }
  } else {
    # No curl installed: fall back to base R with a timeout long enough that it
    # is not the thing that fails. No resume here, so discard any partial first.
    unlink(part)
    with_generous_timeout(utils::download.file(url, part, mode = "wb"))
  }

  if (!file.rename(part, dest)) {
    stop("Could not move ", part, " into place at ", dest, call. = FALSE)
  }

  secs <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  fetched <- file.size(dest) - resumed_from
  message(sprintf(
    "  %s complete: %s on disk%s.",
    label, format_bytes(file.size(dest)),
    if (secs > 1 && fetched > 0) {
      sprintf(
        ", %s fetched in %s (%s/s)", format_bytes(fetched),
        if (secs < 90) sprintf("%.0fs", secs) else sprintf("%.0f min", secs / 60),
        format_bytes(fetched / secs)
      )
    } else {
      ""
    }
  ))
  dest
}

# Adopt a truncated download left behind by an earlier run.
#
# The download.file() this file used before could not resume AND wrote straight
# to the destination, so a failure left a partial file exactly where a complete
# one goes. One collaborator's run died on the old hour cap at 3,752 of 3,794 MB
# — 99% of the way — and left a <key>.zip that the `if (!file.exists(zip))`
# guards below would have taken for a finished download: the next tar_make()
# would have skipped the fetch, unzipped a truncated archive, and reported a
# problem with the archive rather than with the transfer. The 4 GB would then
# have had to be fetched again from zero.
#
# So any destination file that is not a readable archive is moved to .part,
# where the resume path collects it and fetches only the remainder. This is
# what makes the fix retroactive — nobody has to delete anything or start over.
adopt_truncated <- function(dest, label) {
  if (!file.exists(dest) || is_readable_zip(dest)) {
    return(invisible(FALSE))
  }

  # An empty file carries nothing to resume from and is what a connection that
  # died before its first byte leaves behind — geodata does exactly this. Staging
  # it would only produce a "resuming, 0 B already on disk" message.
  if (file.size(dest) == 0) {
    unlink(dest)
    message(label, ": cleared an empty file left by an earlier failed attempt.")
    return(invisible(TRUE))
  }

  part <- paste0(dest, ".part")
  # If both exist, the bigger file is the better starting point.
  if (file.exists(part) && file.size(part) >= file.size(dest)) {
    unlink(dest)
    return(invisible(TRUE))
  }
  if (!file.rename(dest, part)) {
    stop("Could not stage ", dest, " for resumption at ", part, call. = FALSE)
  }
  message(
    label, ": found an incomplete download of ", format_bytes(file.size(part)),
    " from an earlier run. Keeping it and fetching only the remainder."
  )
  invisible(TRUE)
}

is_readable_zip <- function(path) {
  tryCatch(
    nrow(utils::unzip(path, list = TRUE)) > 0,
    error = function(e) FALSE, warning = function(w) FALSE
  )
}

# An archive that cannot be listed is a failed download, not a corrupt upstream
# file. Say which, and clear it so the next run refetches instead of failing
# identically forever.
verify_zip <- function(path, label) {
  if (!is_readable_zip(path)) {
    unlink(path)
    stop(
      label, " archive at ", path, " could not be read, which almost always ",
      "means the download was truncated rather than that the upstream file is ",
      "bad. It has been deleted; re-run targets::tar_make() to fetch it again.",
      call. = FALSE
    )
  }
  invisible(path)
}

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
  adopt_truncated(zip, "GBIF")
  if (!file.exists(zip)) {
    url <- paste0("https://api.gbif.org/v1/occurrence/download/request/", key, ".zip")
    # Resume is real here, not hopeful: the endpoint 302s to
    # occurrence-download.gbif.org, which advertises accept-ranges: bytes and
    # answers a Range request with 206 and a content-range against the full
    # 3,978,907,996 bytes. Verified against the live endpoint, not assumed.
    download_resumable(url, zip, paste("GBIF", key), size_hint = "~4 GB")
  }
  verify_zip(zip, "GBIF")
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
  # Before the checksum, not after: a truncated download fails verify_sha256()
  # too, and that message says the upstream payload has changed and asks the
  # reader to confirm it is the same product. For a half-fetched file that is the
  # wrong diagnosis pointed at the wrong person. Staging it for resume first
  # leaves the checksum to mean only what it is meant to mean.
  adopt_truncated(zip, "ABS population grid")
  if (!file.exists(zip)) {
    download_resumable(abs_pop_url, zip, "ABS population grid", size_hint = "~2 MB")
  }
  # Checksum first: it distinguishes a truncated download from a changed upstream
  # payload more precisely than verify_zip() can, and its message is the one that
  # matters if ABS ever reissues the file.
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
# The 628 MB archive is fetched by download_resumable() rather than by geodata,
# and geodata is then left to unzip it and assemble the paths.
#
# THIS IS SAFE because geodata's .downloadDirect() opens with
# `if (!file.exists(filename))`: with the zip already in place it skips the
# transfer entirely and worldclim_global() proceeds to its own unzip(). Read off
# the installed source, and exercised below.
#
# WHY BOTHER. geodata fetches with utils::download.file(), which has no low-speed
# option, so the only lever reachable from outside is the total timeout — and a
# total timeout is precisely what this file removed for being unable to tell
# "slow" from "dead". That is not hypothetical: geodata.ucdavis.edu went down
# during testing (no TCP connect at all) and the run sat at zero bytes
# indefinitely, because the generous floor that keeps slow transfers alive also
# keeps dead ones alive. Fetching it ourselves puts the 120 s stall guard back in
# charge.
#
# COST: the base URL is now duplicated from geodata's own .wc_url(). If geodata
# ever moves hosts this line must follow, and the failure would be a clear 404
# from download_resumable() rather than anything silent. Calling
# geodata:::.wc_url() to avoid the duplication would trade a visible copy for a
# dependency on another package's private function.
worldclim_base_url <- "https://geodata.ucdavis.edu/climate/worldclim/2_1/base/"

download_worldclim <- function(dest_dir = "downloads", res = "2.5",
                               vars = c(1, 12, 14, 15, 17)) {
  fres <- if (identical(as.character(res), "0.5")) "30s" else paste0(res, "m")
  wanted <- file.path(
    dest_dir, "climate", paste0("wc2.1_", fres),
    paste0("wc2.1_", fres, "_bio_", vars, ".tif")
  )

  # geodata decides on the tifs, not the zip, so only fetch when they are absent
  # — otherwise a completed run would re-download on every build.
  if (!all(file.exists(wanted))) {
    zip_dir <- file.path(dest_dir, "climate", paste0("wc2.1_", fres))
    dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)
    zip <- file.path(zip_dir, paste0("wc2.1_", fres, "_bio.zip"))
    # A stalled geodata attempt leaves a zero-byte zip here, and .downloadDirect()
    # would then see a file, skip the download, and fail in unzip() as
    # "download failed" on every subsequent run. Clear or resume it first.
    adopt_truncated(zip, "WorldClim")
    if (!file.exists(zip)) {
      download_resumable(
        paste0(worldclim_base_url, basename(zip)), zip,
        paste0("WorldClim 2.1 ", fres, " bioclim"), size_hint = "~600 MB"
      )
    }
  }

  r <- with_generous_timeout(
    geodata::worldclim_global(var = "bio", res = res, path = dest_dir)
  )
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

  # austraits does its own fetching, so there is no stall guard and no resume
  # here — say so, rather than leaving a silent pause. ~19 MB from Zenodo.
  message(
    "Downloading AusTraits ", version, " (~19 MB) from Zenodo. ",
    "This one cannot resume if interrupted; it will restart."
  )
  with_generous_timeout(austraits::load_austraits(path = dest_dir, version = version))
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
# NOTE: rnaturalearth 1.2.0 writes a GeoPackage, not a shapefile.
# Dataset_construction.qmd checks for and
# reads ne_10m_{ocean,land}.shp, so those reads are already broken against the
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
      message(
        "Downloading Natural Earth 10m ", type, " (~10 MB). ",
        "Fetched by rnaturalearth, so no resume if interrupted."
      )
      with_generous_timeout(rnaturalearth::ne_download(
        scale = 10, type = type, category = "physical",
        returnclass = "sf", destdir = d
      ))
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
  adopt_truncated(zip, "Inter")
  if (!file.exists(zip)) {
    url <- paste0(
      "https://github.com/rsms/inter/releases/download/v",
      version, "/Inter-", version, ".zip"
    )
    download_resumable(url, zip, paste0("Inter ", version), size_hint = "~34 MB")
  }
  verify_zip(zip, "Inter")

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
