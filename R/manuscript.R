# Manuscript support — every number the paper quotes, computed from the
# pipeline rather than transcribed.
#
# WHY THIS EXISTS. The manuscript lives outside the repo as a .docx, so its
# numbers are typed in by hand and nothing checks them against the code. That
# is the same failure mode as the hand-rolled caches: a value that silently
# stops matching its source. It has already happened here — every quantitative
# claim in the September draft predates the taxonomy update, the grid-alignment
# fix and the life-history join fix.
#
# `manuscript_claims` records what the draft SAYS. reported_values() computes
# what the pipeline now GIVES, and puts them side by side with a changed flag,
# so updating the paper is a checklist rather than a hunt.
#
# When a number in the manuscript is edited, update it here too — that is what
# keeps the comparison meaningful.

# Values quoted in australian_annuals_cornsters_20260912.docx, with where they
# appear. NA where the draft gives no number (the claim is qualitative).
manuscript_claims <- tibble::tribble(
  ~key, ~manuscript, ~location,
  "records_raw", 24400000, "Methods, dataset construction",
  "species_with_records", 23626, "Methods + Table 1",
  "species_australia_total", 26620, "Methods (external figure, not computed here)",
  "taxa_known_lh_origin", 21940, "Methods + Table 1",
  "n_native_annual", 2387, "Table 1",
  "n_native_perennial", 16805, "Table 1",
  "n_introduced_annual", 934, "Table 1",
  "n_introduced_perennial", 1814, "Table 1",
  "cells_gt500", 886, "Results, effort cutoff",
  "cells_effort_standardised", 891, "Results, rarefaction",
  "z_frac_bio12", -63.76, "Results, Supp Table 1a",
  "z_frac_native", -28.21, "Results, Supp Table 1a",
  "z_frac_bio15", 0.16, "Results, Supp Table 1a",
  "p_frac_bio15", 0.869, "Results, Supp Table 1a",
  "rse_native_annual", 0.185, "Results, RSE comparison",
  "rse_introduced_annual", 0.355, "Results, RSE comparison",
  "rse_native_perennial", 0.214, "Results, RSE comparison",
  "rse_introduced_perennial", 0.376, "Results, RSE comparison",
  "z_intro_bio12", 60.83, "Results, Supp Table 2",
  "z_intro_bio15", -71.98, "Results, Supp Table 2",
  "z_intro_popaccess", 42.52, "Results, Supp Table 2",
  "z_intro_popaccess_bio12", -41.35, "Results, Supp Table 2",
  "rse_annual_cutoff1000", 0.26, "Results, cutoff sensitivity",
  "rse_annual_cutoff100", 0.32, "Results, cutoff sensitivity",
  # Retention of richness under the 500-record standardisation. The draft quotes
  # each of these as a RANGE across the four taxon groupings ("14-18% in the
  # wettest quartile"), so each range needs two keys: the range is the claim, and
  # either end can drift on its own.
  #
  # The draft's numbers are the MEDIAN OF THE PER-CELL RATIOS, not the pooled
  # sum(S)/sum(n). Established by fitting all five candidate aggregations against
  # the quoted ranges: median-of-ratios lands within 1-3 points on all four
  # subsets, pooled is out by 9-17 on two of them, and mean-of-ratios,
  # ratio-of-means and ratio-of-medians are worse still. rarefaction_retention()
  # returns both columns; this comparison reads pct_median_cell.
  "retention_wettest_min", 14, "Results, Extended Data Fig. 2c-f",
  "retention_wettest_max", 18, "Results, Extended Data Fig. 2c-f",
  "retention_driest_min", 41, "Results, Extended Data Fig. 2c-f",
  "retention_driest_max", 59, "Results, Extended Data Fig. 2c-f",
  "retention_least_seasonal_min", 19, "Results, Extended Data Fig. 2c-f",
  "retention_least_seasonal_max", 27, "Results, Extended Data Fig. 2c-f",
  "retention_most_seasonal_min", 36, "Results, Extended Data Fig. 2c-f",
  "retention_most_seasonal_max", 46, "Results, Extended Data Fig. 2c-f"
)

# Table 1 — species counts by origin and life history.
make_table1 <- function(data_analysis_individuals) {
  known <- data_analysis_individuals |>
    dplyr::filter(
      !is.na(life_history),
      establishment_means %in% c("native", "introduced")
    )

  by_group <- known |>
    dplyr::distinct(species, establishment_means, life_history) |>
    dplyr::count(establishment_means, life_history, name = "n_species")

  totals <- tibble::tibble(
    establishment_means = "all",
    life_history = "all",
    n_species = dplyr::n_distinct(known$species)
  )

  dplyr::bind_rows(by_group, totals)
}

# Table S2 — model fit for the effect of population accessibility on the
# likelihood that an annual is introduced rather than native.
#
# The presentation form of prop_invasive_annual_model, which Analysis.qmd
# otherwise shows as a raw parameters::parameters() dump. Two things differ from
# a bare tidier: the terms carry the labels the manuscript uses, and estimate and
# SE are preformatted into the single column the paper prints them in.
#
# The unrounded numeric columns are kept alongside the formatted ones so the
# target is still usable as data rather than only as display.
make_tableS2 <- function(prop_invasive_annual_model) {
  # Only terms whose printed name differs from the model's own; anything
  # unmapped keeps the term as written, which is what the draft does for the
  # interactions.
  labels <- c(pop_accessibility = "Population accessibility")

  # SE spans two orders of magnitude here — the bio15 terms are ~3e-4, where two
  # decimal places would print 0.00. Below 0.01 they get three significant
  # figures instead.
  fmt_se <- function(x) {
    ifelse(abs(x) >= 0.01,
      formatC(x, format = "f", digits = 2),
      formatC(signif(x, 3), format = "g")
    )
  }

  broom.mixed::tidy(prop_invasive_annual_model) |>
    dplyr::filter(effect == "fixed") |>
    dplyr::transmute(
      parameter = dplyr::coalesce(labels[term], term),
      estimate_se = paste0(formatC(estimate, format = "f", digits = 2), " ± ", fmt_se(std.error)),
      z_statistic = round(statistic, 2),
      p_value = ifelse(p.value < 0.001, "<0.001", formatC(p.value, format = "f", digits = 3)),
      estimate, std.error, statistic, p.value
    )
}

# One tidy row per number the manuscript quotes.
#
# `fig3_stats` is the primary (cutoff 500) coefficient table; `rse_by_cutoff` is
# a named list of rse_fits tables keyed by cutoff, used for the sensitivity
# claim.
reported_values <- function(data_analysis_individuals, data_model_500,
                            data_model_resampling_500, fig3_stats,
                            rse_fits, prop_invasive_annual_model,
                            fig3_100, fig3_1000,
                            rarefaction_retention_table,
                            gbif_raw_rows = 24385439) {
  known <- data_analysis_individuals |>
    dplyr::filter(
      !is.na(life_history),
      establishment_means %in% c("native", "introduced")
    )
  grp <- known |>
    dplyr::distinct(species, establishment_means, life_history) |>
    dplyr::count(establishment_means, life_history)
  pick <- function(e, l) {
    v <- grp$n[grp$establishment_means == e & grp$life_history == l]
    if (length(v)) v else NA_real_
  }

  fa <- fig3_stats |> dplyr::filter(response == "frac_annuals (logit)")
  term_stat <- function(t, col) {
    v <- fa[[col]][fa$term == t]
    if (length(v)) v else NA_real_
  }
  pia <- as.data.frame(parameters::parameters(prop_invasive_annual_model))
  pia_z <- function(t) {
    v <- pia$z[pia$Parameter == t]
    if (length(v)) v[1] else NA_real_
  }
  rse_of <- function(tbl, g) {
    v <- tbl$rse[tbl$group == g]
    if (length(v)) v else NA_real_
  }

  # Ends of the retention range across the four taxon groupings, for one climate
  # quartile. ROUNDED TO WHOLE PERCENT deliberately: the draft quotes these as
  # integers, and `changed` is meant to fire when a number would print
  # differently at the precision the manuscript uses. Left unrounded, 17.64
  # against a quoted 18 would flag as changed when the paper needs no edit.
  retention_end <- function(gradient, quartile, end) {
    v <- rarefaction_retention_table$pct_median_cell[
      rarefaction_retention_table$gradient == gradient &
        rarefaction_retention_table$quartile == quartile
    ]
    v <- v[!is.na(v)]
    if (!length(v)) return(NA_real_)
    round(if (end == "min") min(v) else max(v))
  }

  # Cells the standardised analysis actually uses.
  #
  # NOT n_distinct(data_model_resampling_500$cell), which was the previous
  # definition and is wrong. make_data_model_resampling() LEFT joins the
  # projections onto data_model_250, so the frame keeps all 1,049 cells of the
  # 250-record cutoff and marks the short ones NA — project_s() returns NA for any
  # cell holding fewer than n_eval records. Counting rows there reported 1,049
  # against the draft's 891 and flagged an 18% drift that was pure artefact: the
  # fits drop those rows, and never saw those cells. Filtering to complete rows
  # gives 906, the quantity the draft's number is.
  resampling_cells <- dplyr::n_distinct(
    data_model_resampling_500$cell[
      !is.na(data_model_resampling_500$n_annual) &
        !is.na(data_model_resampling_500$n_perennial)
    ]
  )

  computed <- tibble::tribble(
    ~key,                        ~current,
    "records_raw",               as.numeric(gbif_raw_rows),
    "species_with_records",      as.numeric(dplyr::n_distinct(data_analysis_individuals$species)),
    "species_australia_total",   NA_real_,
    "taxa_known_lh_origin",      as.numeric(dplyr::n_distinct(known$species)),
    "n_native_annual",           as.numeric(pick("native", "annual")),
    "n_native_perennial",        as.numeric(pick("native", "perennial")),
    "n_introduced_annual",       as.numeric(pick("introduced", "annual")),
    "n_introduced_perennial",    as.numeric(pick("introduced", "perennial")),
    "cells_gt500",               as.numeric(dplyr::n_distinct(data_model_500$cell)),
    "cells_effort_standardised", as.numeric(resampling_cells),
    "z_frac_bio12",              term_stat("log10(bio12)", "statistic"),
    "z_frac_native",             term_stat("establishment_meansnative", "statistic"),
    "z_frac_bio15",              term_stat("bio15", "statistic"),
    "p_frac_bio15",              term_stat("bio15", "p.value"),
    "rse_native_annual",         rse_of(rse_fits, "native_annual"),
    "rse_introduced_annual",     rse_of(rse_fits, "introduced_annual"),
    "rse_native_perennial",      rse_of(rse_fits, "native_perennial"),
    "rse_introduced_perennial",  rse_of(rse_fits, "introduced_perennial"),
    "z_intro_bio12",             pia_z("log10(bio12)"),
    "z_intro_bio15",             pia_z("bio15"),
    "z_intro_popaccess",         pia_z("pop_accessibility"),
    "z_intro_popaccess_bio12",   pia_z("pop_accessibility:log10(bio12)"),
    # The manuscript's "residual standard error for annual richness" across
    # cutoffs is sigma() of the glmmTMB annual-richness model, NOT the per-group
    # OLS in rse_fits. Confirmed by matching the quoted values: 0.262 at cutoff
    # 1000 and 0.323 at 100, against 0.26 and 0.32 in the text. Using the OLS
    # figures here would have reported a spurious 36% drift.
    "rse_annual_cutoff1000",     as.numeric(glmmTMB::sigma(fig3_1000$fit_n_annual)),
    "rse_annual_cutoff100",      as.numeric(glmmTMB::sigma(fig3_100$fit_n_annual)),
    "retention_wettest_min",        retention_end("bio12", 4, "min"),
    "retention_wettest_max",        retention_end("bio12", 4, "max"),
    "retention_driest_min",         retention_end("bio12", 1, "min"),
    "retention_driest_max",         retention_end("bio12", 1, "max"),
    "retention_least_seasonal_min", retention_end("bio15", 1, "min"),
    "retention_least_seasonal_max", retention_end("bio15", 1, "max"),
    "retention_most_seasonal_min",  retention_end("bio15", 4, "min"),
    "retention_most_seasonal_max",  retention_end("bio15", 4, "max")
  )

  manuscript_claims |>
    dplyr::left_join(computed, by = "key") |>
    dplyr::mutate(
      abs_change = current - manuscript,
      pct_change = ifelse(is.na(manuscript) | manuscript == 0, NA_real_,
        100 * (current - manuscript) / abs(manuscript)
      ),
      # "changed" is deliberately strict: anything that would print differently
      # at the precision the manuscript uses counts, because the point of this
      # table is to catch numbers that need retyping.
      changed = !is.na(current) & !is.na(manuscript) &
        signif(current, 4) != signif(manuscript, 4)
    ) |>
    dplyr::select(key, location, manuscript, current, abs_change, pct_change, changed)
}
