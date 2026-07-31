# Climate and population covariate rasters — ported from
# Dataset_construction.qmd.
#
# Every function here takes FILE PATHS, not rasters, for anything sourced
# outside the DAG. A SpatRaster cannot be passed between targets by default —
# see the format_spatraster note in _targets.R — so the rasters that do move
# between targets are stored through that format, and everything else is
# rebuilt from a path that targets hashes.

# WorldClim bioclim layers, cropped to Australia.
#
# bio1  = annual mean temperature (°C × 10)
# bio12 = mean annual precipitation (mm) — primary rainfall predictor
# bio14 = precipitation of driest month (mm) — minimum moisture availability
# bio15 = precipitation seasonality (CV%) — rainfall variability
# bio17 = precipitation of driest quarter (mm)
#
# `worldclim_paths` comes from the worldclim_tifs target, already in the order
# bio 1, 12, 14, 15, 17. Layer names are unchanged by the crop
# (wc2.1_2.5m_bio_1 etc.), which is what the downstream renaming keys on.
#
# The crop used to be defined by a committed raster, data/australia.tif, whose
# provenance was never recorded — it was added in 2023 with no stated source,
# and it was the only third-party-looking data file in the repository.
# Everything else is downloaded at build time, so this one file was the only
# thing complicating the repository's own licence.
#
# It is replaced by the bounding box below, which is that raster's extent to
# full precision. Verified equivalent, not assumed: cropping by the file and
# cropping by this box give identical extents AND identical cell values.
#
# Deriving the box from a map package instead (ozmaps, rnaturalearth) would NOT
# be equivalent — ozmap_country spans 105.5-168.0 E because it includes the
# external territories, which would widen the crop and retain occurrence
# records that are currently dropped. The occurrence coordinates sit flush
# against this box, so its edges determine which records get climate at all.
australia_extent <- c(112.925, 153.625, -43.7416666666667, -9.14166666666667)

make_climate_raster <- function(worldclim_paths, crop_extent = australia_extent) {
  worldclim_paths |>
    terra::rast() |>
    terra::crop(terra::ext(crop_extent))
}

# ABS Australian Population Grid 2011, reprojected to WGS84 lon/lat.
#
# Kept as its own target because terra::project() on the 1 km national grid is
# the slow part, and both pop_density and the point-level population_size in
# data_analysis_individuals consume the result.
make_pop_raster <- function(pop_grid_path) {
  terra::rast(pop_grid_path) |>
    terra::project("+proj=longlat +datum=WGS84 +no_defs")
}

# Population density on the 0.75° analysis grid, people per km².
#
# Resamples by SUMMING contributing pixels rather than sampling, so that
# high-density urban centres are not missed — a point sample of a 0.75° cell
# would usually land on empty land. The divisor is the cell area in km².
make_pop_density <- function(pop_raster, grid_square) {
  out <- terra::resample(pop_raster, grid_square, method = "sum") / (0.75 * 0.75 * 111^2)
  names(out) <- "pop_density"
  out
}

# Kernel-smoothed population accessibility at three bandwidths.
#
# Gaussian focal weighted sum over sqrt(pop_density). Ocean is set to 0 (not NA)
# before the kernel so coastal land cells are not penalised for having an ocean
# neighbourhood, then masked back to land afterwards.
#
# All three bands are computed so the choice can be compared (Analysis.qmd
# plots them side by side); 250 km is the one the analysis uses, renamed to
# pop_accessibility on load. 500 km spans ~6 grid cells, capturing cross-state
# city influence.
make_pop_accessibility <- function(pop_density, bandwidths_km = c(h100 = 100, h250 = 250, h500 = 500)) {
  # sqrt transform, ocean treated as zero; shared across all bandwidths.
  base <- terra::subst(pop_density, NA, 0) |>
    terra::app(fun = function(x) ifelse(x > 0, sqrt(x), 0))

  access <- lapply(bandwidths_km, function(h_km) {
    h_deg <- h_km / 111 # 1 degree ~ 111 km at mid-latitudes
    w_gauss <- terra::focalMat(pop_density, h_deg, type = "Gauss")
    base |>
      terra::focal(w = w_gauss, fun = "sum", na.rm = TRUE) |>
      terra::mask(pop_density) # blank ocean back out
  })

  out <- terra::rast(access)
  names(out) <- paste0("pop_accessibility_", bandwidths_km, "km")
  out
}

# Global 1° climate grid, used only for the AU-vs-global climate-space panels
# (figS1).
#
# `land_path` is the Natural Earth land outline from the naturalearth_shp
# target. The .qmd reads "downloads/ne_10m_land/ne_10m_land.shp", which
# rnaturalearth 1.2.0 never writes — it writes a GeoPackage. Taking the path
# from the target fixes that read.
make_global_climate <- function(worldclim_paths, land_path) {
  global_land <- sf::st_read(land_path, quiet = TRUE) |>
    sf::st_transform(crs = sf::st_crs(4326))

  r_template <- terra::rast(
    extent = terra::ext(global_land), # match the extent of the land polygons
    resolution = 1,
    crs = "EPSG:4326"
  )

  global_land$land_value <- 1
  global_land_raster <- terra::rasterize(global_land, r_template,
    field = "land_value", touches = TRUE
  )

  # bio12, bio14, bio15 only. Selected by name rather than by position so this
  # does not silently take the wrong layers if the worldclim_tifs order changes.
  wanted <- paste0("_bio_", c(12, 14, 15), ".tif")
  paths <- vapply(wanted, function(w) {
    hit <- worldclim_paths[endsWith(worldclim_paths, w)]
    if (length(hit) != 1) stop("Expected exactly one WorldClim path matching ", w)
    hit
  }, character(1), USE.NAMES = FALSE)

  global_raster_climate <-
    paths |>
    terra::rast() |>
    terra::crop(terra::rast(global_land_raster))

  # fact = 15 takes 2.5 arcmin to ~0.625°; this is a plotting grid, not an
  # analysis grid.
  global_grid_square <- terra::aggregate(global_raster_climate, fact = 15)

  global_climate <- terra::as.data.frame(global_grid_square, xy = TRUE, na.rm = TRUE) |>
    tibble::as_tibble()
  names(global_climate)[3:5] <- c("bio12", "bio14", "bio15")
  global_climate
}
