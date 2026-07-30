# Analysis grid construction.
#
# The only raster in the pipeline that depends on no external input, so it can
# be built on any machine without downloading anything.

make_grid_square <- function() {
  # 0.75° resolution (~80 km at mid-latitudes). Chosen so that arid-zone cells
  # accumulate enough records to estimate richness reliably. Sensitivity to grid
  # size should be tested at 0.5° before publication.
  terra::rast(
    extent = c(112.5, 154.0, -43.5, -9.0), # xmin, xmax, ymin, ymax for Australia
    resolution = 0.75,
    crs = "EPSG:4326"
  )
}
