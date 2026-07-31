# targets pipeline for the Australian annuals rainfall-gradient analysis.
#
# Every stage of the analysis builds here: raw inputs, occurrence cleaning,
# taxonomy, covariates, the derived tables, the models, the figures and the
# reports.
#
# Both .qmd documents are read-only reports: they display results with
# tar_read() and compute nothing.
#
# Run with: targets::tar_make()

library(targets)
library(tarchetypes)

# Fail in the first second rather than the thirty-fifth minute.
#
# tar_option_set(packages =) below is checked when a target that needs them
# runs, which for the late stages is after the 4 GB download, the occurrence
# clean and the rarefaction. A collaborator lost 34 min 43 s of a run to a
# missing `marginaleffects`, reported as a targets error deep inside a
# tar_map index rather than as "install this".
#
# The three below are the ones no amount of reading the install list prevents,
# because nothing in this repository names them as dependencies:
#
#   marginaleffects  modelbased only SUGGESTS it, so install.packages(
#                    "modelbased") does not bring it in — but
#                    modelbased::estimate_slopes() needs it at runtime.
#                    Used by slopes_shift_table, near the end of the pipeline.
#   quarto           tarchetypes::tar_quarto() drives the report targets
#                    through the quarto R package.
#   austraits        needed by download_austraits() in the first minute; listed
#                    here so a GitHub-installed package missing entirely is
#                    reported alongside the others rather than on its own.
#
# system.file() rather than requireNamespace(): it answers "is this installed"
# without loading the namespace, so this costs nothing at startup.
local({
  needed <- c(
    marginaleffects = "required by modelbased::estimate_slopes(); modelbased only Suggests it",
    quarto = "required by tarchetypes::tar_quarto() for the two report targets",
    austraits = "install with remotes::install_github('traitecoevo/austraits')"
  )
  missing <- names(needed)[!nzchar(vapply(
    names(needed), function(p) system.file(package = p), character(1)
  ))]
  if (length(missing)) {
    stop(
      "Missing package", if (length(missing) > 1) "s" else "", ":\n",
      paste0("  ", missing, " — ", needed[missing], collapse = "\n"),
      "\n\nSee the install block in readme.md. Stopping now rather than partway ",
      "through a build that takes over an hour.",
      call. = FALSE
    )
  }
})

tar_option_set(
  packages = c(
    "tidyverse", "terra", "arrow", "sf",
    "glmmTMB", "broom.mixed", "patchwork",
    "data.table", "CoordinateCleaner", "APCalign", "austraits", "furrr",
    "modelbased", "ozmaps", "viridis", "scales", "ggnewscale", "ragg", "systemfonts"
  ),
  # targets defaults to memory = "persistent", which keeps every target that has
  # been loaded in RAM for the whole pipeline run. That is a bad fit here: the
  # GBIF table is the largest object in the project by an order of magnitude and
  # only two targets need it. "transient" drops each target from memory once its
  # dependents have consumed it, and the explicit gc() is worth the few seconds
  # it costs on a pipeline where a single object can be several GB.
  memory = "transient",
  garbage_collection = TRUE
)

# Sources every .R file in R/ into the pipeline's environment.
tar_source("R")

# ---------------------------------------------------------------------------
# Storage format for terra objects.
#
# A plain tar_target() CANNOT store a SpatRaster. targets' default rds format
# bypasses the S4 hook that makes base saveRDS() work on terra objects, so the
# stored file holds a dead external pointer. Nothing warns: tar_make() reports
# the target completed, and it is only on tar_read() that every accessor fails
# with "external pointer is not valid". Verified for rasters both with and
# without cell values.
#
# terra's own wrap()/unwrap() is the fix and is what this format applies. It
# round-trips extent, resolution, CRS and cell numbering intact.
#
# geotargets::tar_terra_rast() also works, and is preferable where an
# inspectable GeoTIFF on disk is wanted — but it CANNOT store a valueless
# template such as grid_square, because terra::writeRaster() refuses those
# ("there are no cell values"). This format handles both cases, so it is the
# project default; reach for geotargets per-target if an inspectable file is wanted.
# ---------------------------------------------------------------------------
format_spatraster <- tar_format(
  read  = function(path) terra::unwrap(readRDS(path)),
  write = function(object, path) saveRDS(terra::wrap(object), path)
)

# ---------------------------------------------------------------------------
# Raw file inputs — the path-convention question on issue #44, resolved.
#
# The three options on the table were symlinks, a lookup table, and the
# candidate list now in Dataset_construction.qmd. R/download.R takes a fourth:
# every raw input is fetched programmatically and each function returns the
# path(s) it wrote. There is no machine-specific path left to configure and
# nothing to symlink, and the resolved path is a declared input to the DAG
# rather than a build-time property.
#
# The download-if-absent guard inside those functions is NOT the file.exists()
# anti-pattern the migration is removing: targets hashes the returned FILE, so
# downstream work invalidates on content. Re-fetching 4 GB per build would be
# absurd.
#
# The cost of this: tar_make() on a clean checkout downloads ~4.7 GB (GBIF 4.0,
# WorldClim 0.65, AusTraits 0.02, ABS 0.002). That is the price of the pipeline
# running end to end from nothing, and it happens once.
#
# Each acquisition target below sets cue = tar_cue(depend = FALSE), so it tracks
# the file it returns rather than the body of the function that returned it. By
# default targets hashes that body, which makes reformatting R/download.R — or
# editing a comment in it — invalidate the whole acquisition layer and cascade
# through nearly every downstream target. That happened once already and cost a
# full rebuild for a change that altered no behaviour.
#
# This is safe only because every pin is encoded in the path each function
# returns: downloads/GBIF/<key>.csv, austraits-<version>.rds, Inter-<version>/,
# wc2.1_<res>m/. Changing a pin therefore changes the path, and the file cue
# fires even with the depend cue off. pop_grid_tif is the exception — its path
# is fixed — but its checksum is verified only when the file is absent, so this
# removes no guard that was running. KEEP THAT PROPERTY: a pin that stops being
# visible in the returned path stops being tracked at all.
# ---------------------------------------------------------------------------

list(
  # GBIF occurrence download 0001494-250914085247600, doi 10.15468/dl.b672m4.
  # Frozen: GBIF retains executed downloads, so this resolves to the same
  # 24,385,439 records indefinitely rather than to whatever GBIF holds today.
  tar_target(gbif_raw, download_gbif(), format = "file",
             cue = tar_cue(depend = FALSE)),

  # WorldClim 2.1 bioclim, 2.5 arcmin — bio1, 12, 14, 15, 17. Five paths under
  # downloads/climate/wc2.1_2.5m/, NOT the downloads/wc2.1_2.5m_bio/ that
  # Dataset_construction.qmd hardcodes. Phase 3 must take paths from this
  # target rather than rebuilding that string.
  tar_target(worldclim_tifs, download_worldclim(), format = "file",
             cue = tar_cue(depend = FALSE)),

  # ABS Australian Population Grid 2011, sha256-pinned (the ABS URL is a legacy
  # Lotus Notes link, stable since 2014 but not a guaranteed scheme).
  tar_target(pop_grid_tif, download_abs_population(), format = "file",
             cue = tar_cue(depend = FALSE)),

  # Natural Earth ocean and land outlines, in that order.
  #
  # NOTE: rnaturalearth 1.2.0 writes GeoPackages, not
  # shapefiles. Dataset_construction.qmd,281,416,461 and Analysis.qmd
  # all check for and read ne_10m_{ocean,land}.shp, so those reads are already
  # broken against the installed version — the guard sees no .shp, re-downloads
  # every render, and st_read() then fails on a file that was never written.
  # Ported code must take the path from this target.
  tar_target(naturalearth_shp, download_naturalearth(), format = "file",
             cue = tar_cue(depend = FALSE)),

  # AusTraits 7.0.0, Zenodo doi 10.5281/zenodo.15718081.
  tar_target(austraits_rds, download_austraits(), format = "file",
             cue = tar_cue(depend = FALSE)),

  # Inter, the figure typeface (SIL OFL). Vendored rather than assumed
  # installed: a missing font is substituted SILENTLY, so figures would
  # differ between machines with nothing to indicate it. See R/download.R.
  tar_target(inter_fonts, download_inter(), format = "file",
             cue = tar_cue(depend = FALSE)),

  # -------------------------------------------------------------------------
  # Phase 2 — the expensive steps that were hand-cached.
  # -------------------------------------------------------------------------

  # 24.4M raw records -> 16.6M cleaned. format = "parquet" so the stored object
  # is readable directly with arrow.
  # keep_unused_columns is stated explicitly rather than left to the default,
  # because it is a replication decision and belongs where the pipeline is read.
  # TRUE reproduces the legacy parquet schema; see R/clean_gbif.R.
  tar_target(gbif_clean, clean_gbif(gbif_raw, keep_unused_columns = TRUE),
    format = "parquet"
  ),

  # The unique species names, ~10^4 strings, split out as their own target so
  # that names_aligned does not depend on the 16.6M-row table. Without this,
  # every rebuild of names_aligned — which hits the network and so is the
  # target most likely to need re-running — would deserialise the whole GBIF
  # parquet into memory just to read one column out of it.
  tar_target(gbif_species, sort(unique(gbif_clean$species))),

  # Replaces if (!file.exists(file_gbif_names_aligned)) at
  # Dataset_construction.qmd. Hits the network via APCalign; the APC release
  # is pinned in R/align_taxonomy.R; see the note there.
  tar_target(names_aligned, align_taxonomy(gbif_species), format = "parquet"),

  #
  # Consumes the ANALYSIS-filtered table, not the raw parquet — Analysis.qmd
  # narrows data_analysis_individuals on load (:41-56) and the accumulation runs
  # on the narrowed version. Passing the raw table rarefies ocean and
  # out-of-extent cells that no figure uses, and does not fit in memory.
  #
  # Reproducible: Analysis.qmd sets a fixed furrr seed and R/models.R now
  # matches it.
  tar_target(accum_results, run_accumulation(individuals_filtered)),

  # Analysis.qmd. Sits at the phase-3/phase-4 boundary — it is an analysis
  # filter rather than dataset construction, declared now because accum_results
  # needs it.
  tar_target(individuals_filtered, filter_individuals(data_analysis_individuals),
    format = "parquet"
  ),

  # -------------------------------------------------------------------------
  # Phase 3 — the rest of dataset construction.
  #
  # Ported faithfully first; the changes collaborators want to dataset
  # construction go on top as separate commits, so that a difference in results
  # has one identifiable cause rather than two.
  # -------------------------------------------------------------------------

  # AusTraits Wenk_2023 life-history extract, re-aligned to the current APC
  # release. Dataset_construction.qmd after the master merge.
  tar_target(species_life_history, extract_life_history(austraits_rds)),

  # APC-accepted canonical names, used to drop non-accepted taxa from the
  # occurrence table. Its own target so the DAG tracks the release: see
  # R/align_taxonomy.R for why every APCalign call now shares one version.
  tar_target(accepted_names, apc_accepted_names()),

  # WorldClim bio1/12/14/15/17 cropped to Australia. Paths come from the
  # worldclim_tifs target, NOT the hardcoded directory at
  # Dataset_construction.qmd that geodata does not write.
  tar_target(climate_raster, make_climate_raster(worldclim_tifs),
    format = format_spatraster
  ),

  # ABS population grid reprojected to WGS84. Its own target because
  # terra::project() on the 1 km national grid is the slow part and two
  # downstream targets consume the result.
  tar_target(pop_raster, make_pop_raster(pop_grid_tif),
    format = format_spatraster
  ),

  # Population density on the 0.75° grid, and the three accessibility
  # bandwidths computed from it..
  tar_target(pop_density, make_pop_density(pop_raster, grid_square),
    format = format_spatraster
  ),
  tar_target(pop_accessibility, make_pop_accessibility(pop_density),
    format = format_spatraster
  ),

  # The two assembled data tables. Both format = "parquet" so they
  # can be read by the same arrow calls the .qmd uses and diffed directly
  # against outputs/*.parquet with waldo::compare().
  #
  # data_analysis_individuals is the peak-memory target of the pipeline — see
  # the note in R/individuals.R.
  tar_target(
    data_analysis_individuals,
    make_data_analysis_individuals(
      gbif_clean, names_aligned,
      species_life_history,
      climate_raster, pop_raster, grid_square,
      accepted_names
    ),
    format = "parquet"
  ),
  tar_target(
    data_grid_climate,
    make_data_grid_climate(
      grid_square, climate_raster,
      pop_density, pop_accessibility
    ),
    format = "parquet"
  ),

  # Global 1° climate grid for the AU-vs-global panels (figS1). Takes the land
  # outline from naturalearth_shp, which returns c(ocean, land) in that order.
  tar_target(
    global_climate,
    make_global_climate(worldclim_tifs, naturalearth_shp[2])
  ),

  # -------------------------------------------------------------------------
  # Phase 4 — derived analysis tables (Analysis.qmd).
  #
  # The bio1/bio12/... renaming lives on this side rather than in dataset
  # construction, matching the .qmd: the parquet carries the raw WorldClim
  # names and the analysis renames them on load.
  # -------------------------------------------------------------------------

  tar_target(grid_climate, prepare_grid_climate(data_grid_climate)),

  # Richness counts per cell x group. Carries the assertion that replaces the
  # bare printed sanity check at Analysis.qmd — see R/derived.R for what
  # the invariant actually is and why it does not follow structurally.
  tar_target(data_analysis_grid, make_data_analysis_grid(individuals_filtered)),
  tar_target(
    data_prop_annual,
    make_data_prop_annual(data_analysis_grid, grid_climate)
  ),

  # Ports the known NA-handling defect at Analysis.qmd unchanged, on
  # purpose. Fixing it changes results and belongs in the changes-on-top phase.
  #
  # Measured: the defect is currently LATENT. Every cell with introduced records
  # also has native records (1,249 native cells vs 1,140 introduced), so
  # `native` has no NAs and frac_offset has none either. Fixing it would change
  # nothing on current data, which makes it a safe change rather than a risky
  # one — but it is still deferred, because that could stop being true.
  tar_target(missing_fraction, make_missing_fraction(data_prop_annual)),

  # -------------------------------------------------------------------------
  # Phase 4 — models.
  # -------------------------------------------------------------------------

  tar_target(data_model_base, make_data_model_base(data_prop_annual)),

  # The effort-cutoff series. fit_fig3() is already written to take a frame and
  # is already run across 100/250/500/1000 in Analysis.qmd, so this is
  # the branching the plan called for: one code path, four cutoffs, instead of
  # four hand-copied blocks.
  #
  # Cutoff 500 is the PRIMARY analysis (fig3_2km.png, fig3_stats.csv); the other
  # three are the sensitivity figures figS-100/250/1000. Analysis.qmd builds the
  # primary frame separately at :601, but with predicates equivalent to the
  # cutoff-500 branch, so there is no separate target for it.
  tar_map(
    values = list(cutoff = c(100, 250, 500, 1000)),
    names = cutoff,
    tar_target(data_model, apply_effort_cutoff(data_model_base, cutoff)),
    tar_target(fig3, fit_fig3(data_model)),
    tar_target(fig3_stats_table, fig3_stats(fig3)),
    tar_target(rse_table, fit_rse(data_model)),
    # The figure file must be declared INSIDE this tar_map: tar_map only
    # substitutes fig3 -> fig3_<cutoff> for targets defined in the same call.
    tar_target(figS_png, write_fig3_plot(fig3, sprintf("figS-%d.png", cutoff),
      font_paths = inter_fonts
    ),
    format = "file"
    )
  ),

  # Residual standard error per group, at the primary cutoff. Analysis.qmd
  # prints the four sigma() values loose; this returns them as a table.
  tar_target(rse_fits, fit_rse(data_model_500)),

  # Rarefaction sensitivity series (figE2 / figS-resample_*). Same fit_fig3(),
  # with richness projected to a standardised effort level instead of raw
  # counts. Uses the 250-record cutoff, per.
  tar_map(
    values = list(n_eval = c(100, 500)),
    names = n_eval,
    tar_target(
      data_model_resampling,
      make_data_model_resampling(data_model_250, accum_results, n_eval)
    ),
    tar_target(fig3_resample, fit_fig3(data_model_resampling)),
    tar_target(fig3_resample_stats, fig3_stats(fig3_resample)),
    tar_target(figS_resample_png,
      write_fig3_plot(fig3_resample, sprintf("figS-resample_%d.png", n_eval),
        font_paths = inter_fonts
      ),
      format = "file"
    ),
    tar_target(figS_resample_csv,
      write_output_csv(fig3_resample_stats, sprintf("figS-resample_%d.csv", n_eval)),
      format = "file"
    )
  ),

  # Retention of richness under the 500-record standardisation, by rainfall and
  # seasonality quartile. Backs the Extended Data Fig. 2c-f percentages quoted in
  # Results, which nothing computed before.
  tar_target(
    rarefaction_retention_table,
    rarefaction_retention(accum_results, data_model_250, n_eval = 500)
  ),
  tar_target(rarefaction_retention_csv,
    write_output_csv(rarefaction_retention_table, "rarefaction_retention.csv"),
    format = "file"
  ),

  # Raw vs rarefaction-corrected slopes (slopes_shift_table.csv). The .qmd
  # compares against whatever its loop left behind, which is the 500-record
  # standardisation; passed explicitly here.
  tar_target(
    slopes_shift_table,
    make_slopes_shift_table(fig3_500, fig3_resample_500)
  ),

  # -------------------------------------------------------------------------
  # Deliverables — outputs/ as tracked files.
  #
  # Until these existed the pipeline produced R objects and nothing else:
  # outputs/ stayed empty and the CSVs and figures were written only by the
  # .qmd documents. format = "file" means targets hashes the artefact, so a
  # change to a model invalidates the file on disk instead of leaving a stale
  # one next to a rebuilt object. That is exactly what the loose ggsave() calls
  # in Analysis.qmd could not do.
  #
  # Only the fig3 family is here. The ten map and hexbin figures (fig1, fig2,
  # fig4, fig4b, fig5, figS1, figS5, figS6, figE3, figE4) still live in
  # Analysis.qmd and need data_ozmaps / data_sea as targets first.
  # -------------------------------------------------------------------------

  # -------------------------------------------------------------------------
  # Phase 5 — figures.
  #
  # data_ozmaps and data_sea are the shared coastline layers every map panel
  # needs. Plain tar_target() is safe for these: sf objects are ordinary R
  # lists and round-trip through rds intact, unlike SpatRaster — verified,
  # because the terra version of this trap is silent (see format_spatraster).
  # -------------------------------------------------------------------------

  tar_target(data_ozmaps, ozmaps::ozmap_country),
  tar_target(data_sea, sf::read_sf(naturalearth_shp[1])),
  tar_target(fig1, make_fig1(grid_climate, data_prop_annual, data_ozmaps, data_sea)),
  tar_target(fig1_png, write_output_plot(fig1, "fig1.png",
    width = 9, height = 6,
    font_paths = inter_fonts
  ),
  format = "file"
  ),
  tar_target(figS1, make_figS1(grid_climate, global_climate)),
  tar_target(figS1_png, write_output_plot(figS1, "figS1.png",
    width = 8, height = 3,
    font_paths = inter_fonts
  ),
  format = "file"
  ),
  tar_target(fig2, make_fig2(data_prop_annual, data_ozmaps, data_sea)),
  tar_target(fig2_png, write_output_plot(fig2, "fig2.png",
    width = 9, height = 6,
    font_paths = inter_fonts
  ),
  format = "file"
  ),
  tar_target(figS5, make_figS5(data_prop_annual, data_ozmaps, data_sea)),
  tar_target(figS5_png, write_output_plot(figS5, "figS5.png",
    width = 8, height = 3,
    font_paths = inter_fonts
  ), format = "file"),
  tar_target(figS6, make_figS6(data_prop_annual, effort_model)),
  tar_target(figS6_png, write_output_plot(figS6, "figS6_sampling_effort.png",
    width = 9, height = 3,
    font_paths = inter_fonts
  ), format = "file"),
  tar_target(fig4, make_fig4(data_prop_annual, data_ozmaps, data_sea)),
  tar_target(fig4_png, write_output_plot(fig4, "fig4.png",
    width = 8, height = 12,
    font_paths = inter_fonts
  ), format = "file"),
  tar_target(prop_annual_collapsed, make_prop_annual_collapsed(data_prop_annual)),
  tar_target(fig5, make_fig5(prop_annual_collapsed, data_ozmaps, data_sea)),
  tar_target(fig5_png,
    write_output_plot(fig5, "fig5_prop_annuals_accessibility.png",
      width = 10, height = 3.5, font_paths = inter_fonts
    ),
    format = "file"
  ),
  tar_target(figE3, make_figE_pop(data_model_500, "bio12")),
  tar_target(figE3_png, write_output_plot(figE3, "figE3.png",
    width = 6.7, height = 6,
    font_paths = inter_fonts
  ), format = "file"),
  tar_target(figE4, make_figE_pop(data_model_500, "bio15")),
  tar_target(figE4_png, write_output_plot(figE4, "figE4.png",
    width = 6.7, height = 6,
    font_paths = inter_fonts
  ), format = "file"),
  tar_target(fig4b, make_fig4b(missing_fraction, data_ozmaps, data_sea)),
  tar_target(fig4b_png, write_output_plot(fig4b, "fig4b.png",
    width = 8, height = 3.2,
    font_paths = inter_fonts
  ), format = "file"),

  # -------------------------------------------------------------------------
  # Manuscript support.
  # -------------------------------------------------------------------------

  tar_target(table1, make_table1(data_analysis_individuals)),
  tar_target(table1_csv, write_output_csv(table1, "table1_species_counts.csv"),
    format = "file"
  ),
  # Table S2 — the prop_invasive_annual_model coefficients, formatted as the
  # supplement prints them.
  tar_target(tableS2, make_tableS2(prop_invasive_annual_model)),
  tar_target(tableS2_csv,
    write_output_csv(tableS2, "tableS2_prop_invasive_annual.csv"),
    format = "file"
  ),
  tar_target(
    reported_values_table,
    reported_values(
      data_analysis_individuals, data_model_500,
      data_model_resampling_500, fig3_stats_table_500,
      rse_fits, prop_invasive_annual_model,
      fig3_100, fig3_1000,
      rarefaction_retention_table
    )
  ),
  tar_target(reported_values_csv,
    write_output_csv(reported_values_table, "reported_values.csv"),
    format = "file"
  ),

  # -------------------------------------------------------------------------
  # Reports.
  #
  # tar_quarto() parses each document for tar_read()/tar_load() calls and wires
  # the dependencies itself, so a change to any upstream target re-renders the
  # reports that display it. Both documents are read-only: they compute nothing,
  # which is what removed the last of the hand-rolled caches.
  # -------------------------------------------------------------------------

  tar_quarto(report_dataset, "Dataset_construction.qmd"),
  tar_quarto(report_analysis, "Analysis.qmd"),
  tar_target(fig3_stats_csv,
    write_output_csv(fig3_stats_table_500, "fig3_stats.csv"),
    format = "file"
  ),
  tar_target(slopes_shift_csv,
    write_output_csv(slopes_shift_table, "slopes_shift_table.csv"),
    format = "file"
  ),

  # fig3_2km.png is the primary figure; figS-500.png is the same plot saved
  # again under the sensitivity-series name, as Analysis.qmd does.
  tar_target(fig3_png, write_fig3_plot(fig3_500, "fig3.png", font_paths = inter_fonts), format = "file"),


  # The two standalone models.
  tar_target(effort_model, fit_effort(data_prop_annual)),
  tar_target(prop_invasive_annual_model, fit_prop_invasive_annual(data_prop_annual)),

  # -------------------------------------------------------------------------
  # Phase 1 — scaffold.
  # -------------------------------------------------------------------------

  # The 0.75° analysis template. Serves as the phase-1 prototype for the terra
  # storage question above — it is the only raster in the pipeline that depends
  # on no external input, so it builds on any machine.
  #
  # NB round-tripping a valueless template through wrap() gives it an all-NaN
  # value layer, so hasValues() flips FALSE -> TRUE. Cell numbering — the one
  # property downstream code relies on, via terra::extract(..., cells = TRUE) —
  # is identical to a freshly built template. Do not add a hasValues() check
  # against this target.
  tar_target(grid_square, make_grid_square(), format = format_spatraster)
)
