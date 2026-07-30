# Model fitting for the richness / fraction-annuals analysis.
#
# Moved verbatim from Analysis.qmd during the targets migration (issue #44,
# Bodies are unchanged; only the surrounding chunk fences are gone.
# Package-qualified calls are used where the original had them — the tidyverse
# and ggplot2 verbs rely on packages loaded via tar_option_set(packages =).

fit_fig3 <- function(data_model) {
  # gaussian() on log10-transformed counts: approximation to a count model.
  # nbinom2 would be more principled for overdispersed counts; gaussian gives
  # interpretable slopes (log10 units) with consistent inference for these patterns.
  # Three-way interaction (log10(bio12) * bio15 * establishment_means) tests whether
  # the rainfall-seasonality surface differs between native and introduced species.
  # bio15 kept on linear scale — it is a coefficient of variation, already dimensionless.
  fit_n_annual <- glmmTMB::glmmTMB(
    log10(n_annual) ~ (log10(bio12) * establishment_means) + (bio15 * establishment_means),
    data = data_model,
    family = gaussian()
  )

  fit_n_perennial <- glmmTMB::glmmTMB(
    log10(n_perennial) ~ (log10(bio12) * establishment_means) + (bio15 * establishment_means),
    data = data_model,
    family = gaussian()
  )
  # binomial() for fraction: proportion data with counts as weights.
  # betabinomial would account for overdispersion in proportions — considered for upgrade.
  fit_frac <- glmmTMB::glmmTMB(
    cbind(n_annual, n_perennial) ~ (log10(bio12) * establishment_means) + (bio15 * establishment_means),
    data = data_model,
    family = binomial()
  )

  # Prediction grids: vary one variable, hold the other at its mean
  bio12_mean <- mean(data_model$bio12, na.rm = TRUE)
  bio15_mean <- mean(data_model$bio15, na.rm = TRUE)

  pred_bio12 <- expand.grid(
    bio12 = 10^seq(log10(100), log10(5000), length.out = 200),
    bio15 = bio15_mean,
    establishment_means = c("native", "introduced")
  )

  pred_bio15 <- expand.grid(
    bio15 = seq(0, 150, length.out = 200),
    bio12 = bio12_mean,
    establishment_means = c("native", "introduced")
  )

  pred_n_annual_bio12 <- pred_bio12 %>% mutate(n_annual = 10^predict(fit_n_annual, newdata = ., type = "response"))
  pred_n_annual_bio15 <- pred_bio15 %>% mutate(n_annual = 10^predict(fit_n_annual, newdata = ., type = "response"))
  pred_n_perennial_bio12 <- pred_bio12 %>% mutate(n_perennial = 10^predict(fit_n_perennial, newdata = ., type = "response"))
  pred_n_perennial_bio15 <- pred_bio15 %>% mutate(n_perennial = 10^predict(fit_n_perennial, newdata = ., type = "response"))
  pred_frac_bio12 <- pred_bio12 %>% mutate(frac_annuals = predict(fit_frac, newdata = ., type = "response"))
  pred_frac_bio15 <- pred_bio15 %>% mutate(frac_annuals = predict(fit_frac, newdata = ., type = "response"))

  # Plot
  native_introduced_colours <- c(introduced = "#440154FF", native = "#43BF71FF")
  xlab_12 <- "Mean annual precip (mm)"
  xlab_15 <- "Precipitation seasonality"
  xlim1 <- c(100, 5000)
  xlim2 <- c(0, 150)

  plot_frac_12 <-
    data_model %>%
    ggplot(aes(bio12, frac_annuals, col = establishment_means)) +
    geom_point(alpha = 0.5, stroke = 0, na.rm = TRUE) +
    geom_line(data = pred_frac_bio12, na.rm = TRUE) +
    scale_x_log10(limits = xlim1) +
    scale_color_manual(values = native_introduced_colours) +
    ylim(c(0, 1)) +
    labs(y = "Fraction annuals", x = "", col = NULL, title = "") +
    theme_bw() +
    theme(
      legend.position = c(1, 1),
      legend.justification = c(1, 1),
      legend.background = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_blank()
    )

  plot_frac_15 <-
    data_model %>%
    ggplot(aes(bio15, frac_annuals, col = establishment_means)) +
    geom_point(alpha = 0.5, stroke = 0, na.rm = TRUE) +
    geom_line(data = pred_frac_bio15, na.rm = TRUE) +
    scale_x_continuous(limits = xlim2) +
    scale_color_manual(values = native_introduced_colours) +
    ylim(c(0, 1)) +
    labs(y = "", x = "", title = "", col = "Establishment means") +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      plot.title = element_blank()
    )

  plot_n_annuals_12 <-
    data_model %>%
    ggplot(aes(bio12, n_annual, col = establishment_means)) +
    geom_point(alpha = 0.5, stroke = 0, na.rm = TRUE) +
    geom_line(data = pred_n_annual_bio12, na.rm = TRUE) +
    scale_x_log10(limits = xlim1) +
    scale_y_log10(limits = c(1, 1500)) +
    scale_color_manual(values = native_introduced_colours) +
    labs(y = "Annual species richness", x = "", title = "") +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      plot.title = element_blank()
    )

  plot_n_annuals_15 <-
    data_model %>%
    ggplot(aes(bio15, n_annual, col = establishment_means)) +
    geom_point(alpha = 0.5, stroke = 0, na.rm = TRUE) +
    geom_line(data = pred_n_annual_bio15, na.rm = TRUE) +
    scale_y_log10(limits = c(1, 1500)) +
    scale_x_continuous(limits = xlim2) +
    scale_color_manual(values = native_introduced_colours) +
    labs(y = "", x = "", title = "") +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      plot.title = element_blank()
    )

  plot_n_perennials_12 <-
    data_model %>%
    ggplot(aes(bio12, n_perennial, col = establishment_means)) +
    geom_point(alpha = 0.5, stroke = 0, na.rm = TRUE) +
    geom_line(data = pred_n_perennial_bio12, na.rm = TRUE) +
    scale_x_log10(limits = xlim1) +
    scale_y_log10(limits = c(1, 1500)) +
    scale_color_manual(values = native_introduced_colours) +
    labs(y = "Perennial species richness", x = xlab_12, title = "") +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_blank()
    )

  plot_n_perennials_15 <-
    data_model %>%
    ggplot(aes(bio15, n_perennial, col = establishment_means)) +
    geom_point(alpha = 0.5, stroke = 0, na.rm = TRUE) +
    geom_line(data = pred_n_perennial_bio15, na.rm = TRUE) +
    scale_x_continuous(limits = xlim2) +
    scale_y_log10(limits = c(1, 1500)) +
    scale_color_manual(values = native_introduced_colours) +
    labs(y = "", x = xlab_15, title = "") +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_blank()
    )

  fig <-
    (plot_frac_12 + plot_frac_15) /
      (plot_n_annuals_12 + plot_n_annuals_15) /
      (plot_n_perennials_12 + plot_n_perennials_15) +
      plot_annotation(tag_levels = "a") &
      theme(
        plot.tag = element_text(size = 14, face = "bold"),
        panel.spacing = unit(0.3, "lines")
      )

  list(
    plot = fig,
    fit_n_annual = fit_n_annual,
    fit_n_perennial = fit_n_perennial,
    fit_frac = fit_frac
  )
}

fig3_stats <- function(fig3_out) {
  tidy_model <- function(fit, response) {
    broom.mixed::tidy(fit, conf.int = TRUE) |>
      filter(effect == "fixed") |>
      select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
      mutate(response = response)
  }

  bind_rows(
    tidy_model(fig3_out$fit_n_annual, "log10(n_annual)"),
    tidy_model(fig3_out$fit_n_perennial, "log10(n_perennial)"),
    tidy_model(fig3_out$fit_frac, "frac_annuals (logit)")
  ) |>
    select(response, term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
    mutate(across(where(is.numeric), \(x) round(x, 5)))
}

# fit_accumulation(): rarefaction-based richness projection
#
# LOGIC:
#   All records in a cell — native and introduced, annual and perennial —
#   form a SINGLE pool for resampling. This matches the data-generating
#   process: a collector samples whatever plants are present regardless of
#   origin, so effort (n_eval) is measured as total records per cell.
#
#   A single bootstrap draw shuffles the combined pool and, as records
#   accumulate in the shuffled sequence, four streams are tallied separately:
#     native_annual, native_perennial, introduced_annual, introduced_perennial.
#
#   For each bootstrap replicate:
#     1. Resample n records with replacement from the combined pool.
#     2. Build accumulation curves for all four streams simultaneously.
#     3. Fit a power law  S = c * n^z  to each stream independently.
#     4. Project S to each n_eval point using the fitted power law.
#   The median across replicates is returned as the point estimate.
#
# OUTPUT: one row per cell with columns
#   n_records,
#   n_native_annual, n_native_perennial, n_introduced_annual, n_introduced_perennial,
#   S_native_annual_100, S_native_perennial_100, S_introduced_annual_100, S_introduced_perennial_100,
#   S_native_annual_500, S_native_perennial_500, S_introduced_annual_500, S_introduced_perennial_500
#   (column names reflect n_eval values)
fit_accumulation <- function(records, n_boot = 20, n_eval = c(100, 500)) {
  sp <- records$species
  lh <- records$life_history
  em <- records$establishment_means
  n <- nrow(records)

  # Records with unknown life_history stay in the pool (they are sampling effort)
  # but must not be tallied into any stream. Left as NA they would make the
  # logical subsetting below return NA elements, which n_distinct() counts as an
  # extra species — inflating every stream of the matching establishment_means
  # by one. Recode to a value that matches no stream.
  lh[is.na(lh)] <- "unknown"

  n_native_annual <- n_distinct(sp[em == "native" & lh == "annual"])
  n_native_perennial <- n_distinct(sp[em == "native" & lh == "perennial"])
  n_introduced_annual <- n_distinct(sp[em == "introduced" & lh == "annual"])
  n_introduced_perennial <- n_distinct(sp[em == "introduced" & lh == "perennial"])

  na_row <- tibble(
    n_records = n,
    n_native_annual = n_native_annual, n_native_perennial = n_native_perennial,
    n_introduced_annual = n_introduced_annual, n_introduced_perennial = n_introduced_perennial
  )
  for (ne in n_eval) {
    na_row[[paste0("S_native_annual_", ne)]] <- NA_real_
    na_row[[paste0("S_native_perennial_", ne)]] <- NA_real_
    na_row[[paste0("S_introduced_annual_", ne)]] <- NA_real_
    na_row[[paste0("S_introduced_perennial_", ne)]] <- NA_real_
  }
  if (n < 10) {
    return(na_row)
  }

  steps <- unique(round(exp(seq(log(1), log(n), length.out = 30))))

  # Four streams tallied simultaneously from the shared shuffled pool
  build_curves <- function(sp_v, lh_v, em_v) {
    map_dfr(steps, \(m) {
      idx <- seq_len(m)
      tibble(
        n_rec = m,
        S_native_annual = n_distinct(sp_v[idx][em_v[idx] == "native" & lh_v[idx] == "annual"]),
        S_native_perennial = n_distinct(sp_v[idx][em_v[idx] == "native" & lh_v[idx] == "perennial"]),
        S_introduced_annual = n_distinct(sp_v[idx][em_v[idx] == "introduced" & lh_v[idx] == "annual"]),
        S_introduced_perennial = n_distinct(sp_v[idx][em_v[idx] == "introduced" & lh_v[idx] == "perennial"])
      )
    })
  }

  # Power law fit to one stream: log(S) = log(c) + z * log(n)
  fit_z <- function(n_vec, s_vec) {
    keep <- s_vec > 0
    if (sum(keep) < 3) {
      return(c(log_c = NA_real_, z = NA_real_))
    }
    f <- lm(log(s_vec[keep]) ~ log(n_vec[keep]))
    c(log_c = unname(coef(f)[1]), z = unname(coef(f)[2]))
  }

  project_s <- function(params, ne) {
    if (anyNA(params) || n < ne) {
      NA_real_
    } else {
      exp(params["log_c"]) * ne^params["z"]
    }
  }

  # Rarefaction by permuting the full record pool; median over replicates
  boot_s <- map_dfr(seq_len(n_boot), function(...) {
    idx <- sample(n)
    obs <- build_curves(sp[idx], lh[idx], em[idx])
    fna <- fit_z(obs$n_rec, obs$S_native_annual)
    fnp <- fit_z(obs$n_rec, obs$S_native_perennial)
    fia <- fit_z(obs$n_rec, obs$S_introduced_annual)
    fip <- fit_z(obs$n_rec, obs$S_introduced_perennial)
    row <- tibble(.rows = 1)
    for (ne in n_eval) {
      row[[paste0("S_native_annual_", ne)]] <- project_s(fna, ne)
      row[[paste0("S_native_perennial_", ne)]] <- project_s(fnp, ne)
      row[[paste0("S_introduced_annual_", ne)]] <- project_s(fia, ne)
      row[[paste0("S_introduced_perennial_", ne)]] <- project_s(fip, ne)
    }
    row
  })

  result <- tibble(
    n_records = n,
    n_native_annual = n_native_annual, n_native_perennial = n_native_perennial,
    n_introduced_annual = n_introduced_annual, n_introduced_perennial = n_introduced_perennial
  )
  for (ne in n_eval) {
    result[[paste0("S_native_annual_", ne)]] <- median(boot_s[[paste0("S_native_annual_", ne)]], na.rm = TRUE)
    result[[paste0("S_native_perennial_", ne)]] <- median(boot_s[[paste0("S_native_perennial_", ne)]], na.rm = TRUE)
    result[[paste0("S_introduced_annual_", ne)]] <- median(boot_s[[paste0("S_introduced_annual_", ne)]], na.rm = TRUE)
    result[[paste0("S_introduced_perennial_", ne)]] <- median(boot_s[[paste0("S_introduced_perennial_", ne)]], na.rm = TRUE)
  }
  result
}

# run_accumulation(): drives fit_accumulation() across every cell.
#
# Ported from Analysis.qmd, which wrapped it in an
# if (file.exists("outputs/accum_results.rds")) cache. That guard is what the
# targets migration removes: it invalidated on the file existing, not on the
# records changing.
#
# THE SEED IS FIXED, and must stay that way. furrr_options(seed = <integer>)
# derives one reproducible RNG stream per element, so the rarefaction gives the
# same answer on every machine and every run. Setting it to TRUE instead draws
# fresh streams each time and makes the analysis irreproducible.
#
# PARALLELISM IS DISABLED, deliberately.
#
# Analysis.qmd runs this under future::plan(multisession). That plan CANNOT be
# created inside a targets build: it dies at cluster construction, before any
# accumulation work, with
#
#   Error: long vectors not supported yet: ../include/Rinlinedfuns.h:551
#
# tar_traceback() puts it in future::plan() itself —
#   plan_init -> makeFutureBackend -> ClusterFutureBackend -> startCluster
#   -> uuid(cl) -> md5sum(bytes = serialize(x, connection = NULL))
# — where future serialises state to hash a cluster UUID and overflows R's 2^31
# limit. It is NOT the data mask and NOT the closure environment of
# fit_accumulation; both were tested and neither fixes it.
#
# THIS COSTS NOTHING SCIENTIFICALLY. furrr derives one RNG stream per element
# from the fixed seed below, independent of the plan and of worker count, so a
# sequential run is bit-identical to a parallel one — verified here on a
# subset, not merely assumed from the documentation. Only wall time changes.
#
# Re-enabling parallelism is a performance question, not a correctness one.
# Anyone doing so must reproduce the cluster-creation failure above first; the
# obvious environment-pruning fixes do not address it.
accumulation_seed <- 20260728L

# `workers` is retained so a caller can opt back into parallelism, but the
# default is sequential for the reason above.
run_accumulation <- function(records, workers = 1) {
  if (workers > 1) {
    future::plan(future::multisession, workers = workers)
  } else {
    future::plan(future::sequential)
  }
  on.exit(future::plan(future::sequential), add = TRUE)

  # Pool all species per cell — effort is cell-level, not group-level.
  # S_100 and S_500 are interpolated at fixed common effort levels.
  #
  # Kept in the mutate() form of Analysis.qmd rather than restructured:
  # with a sequential plan there is nothing to export, so the faithful form is
  # also the working one.
  records |>
    dplyr::select(cell, species, establishment_means, life_history) |>
    tidyr::nest(data = -cell) |>
    dplyr::mutate(result = furrr::future_map(
      data, fit_accumulation,
      .progress = TRUE,
      # Fixed seed, matching.
      .options = furrr::furrr_options(seed = accumulation_seed)
    )) |>
    dplyr::select(-data) |>
    tidyr::unnest(result)
}

# Residual standard error per group.
#
# RSE rather than R^2: native taxa have flatter fits, so their R^2 is inherently
# lower even where the fit is good. RSE measures actual residual error, which is
# comparable across groups. Needs a separate fit per group, hence four models
# rather than the interaction form used in fit_fig3().
#
# The .qmd prints the four sigma() values loose; this returns them as a table so
# they can be read from the DAG.
fit_rse <- function(data_model) {
  fits <- list(
    native_annual = stats::lm(log10(n_annual) ~ log10(bio12) + bio15,
      data = subset(data_model, establishment_means == "native")
    ),
    introduced_annual = stats::lm(log10(n_annual) ~ log10(bio12) + bio15,
      data = subset(data_model, establishment_means == "introduced")
    ),
    native_perennial = stats::lm(log10(n_perennial) ~ log10(bio12) + bio15,
      data = subset(data_model, establishment_means == "native")
    ),
    introduced_perennial = stats::lm(log10(n_perennial) ~ log10(bio12) + bio15,
      data = subset(data_model, establishment_means == "introduced")
    )
  )
  tibble::tibble(
    group = names(fits),
    rse = vapply(fits, stats::sigma, numeric(1)),
    n = vapply(fits, function(f) length(stats::residuals(f)), numeric(1))
  )
}

# Sampling effort as a function of climate..
fit_effort <- function(data_prop_annual) {
  glmmTMB::glmmTMB(
    log10(n_obs_per_cell) ~ log10(bio12) * bio15,
    data = data_prop_annual |> dplyr::filter(bio12 > 0, n_obs_per_cell > 0),
    family = stats::gaussian()
  )
}

# Introduced fraction of annuals against population accessibility and climate.
# .
fit_prop_invasive_annual <- function(data_prop_annual) {
  d <- data_prop_annual |>
    dplyr::select(
      cell, x, y, pop_accessibility, bio12, bio15,
      establishment_means, n_annual
    ) |>
    tidyr::pivot_wider(names_from = establishment_means, values_from = n_annual)

  glmmTMB::glmmTMB(
    cbind(introduced, native) ~ (pop_accessibility * log10(bio12)) + (pop_accessibility * bio15),
    data = d,
    family = stats::binomial()
  )
}

# Rarefaction-corrected modelling frame. Ported from the loop body at
# .
#
# Replaces the raw n_annual / n_perennial counts with richness projected to a
# standardised effort level (n_eval = 100 or 500 records per cell) by
# fit_accumulation(), so the fig3 models can be refitted on effort-corrected
# responses as a sensitivity check.
#
# `data_model_250` rather than the primary cutoff: Analysis.qmd applies
# n_obs_per_cell >= 250 here, not 500.
make_data_model_resampling <- function(data_model_250, accum_results, n_eval) {
  n_eval <- as.character(n_eval)

  accum_formatted <- accum_results |>
    dplyr::select(cell, dplyr::contains(n_eval)) |>
    tidyr::pivot_longer(
      cols = dplyr::contains(n_eval),
      names_to = c("establishment_means", "life_history", "n_eval"),
      names_pattern = "S_(.*)_(.*)_(.*)",
      values_to = "S"
    ) |>
    tidyr::pivot_wider(
      names_from = c(life_history), values_from = S,
      names_sep = "_", names_prefix = "n_"
    )

  data_model_250 |>
    dplyr::select(-n_annual, -n_perennial) |>
    dplyr::left_join(accum_formatted, by = c("cell", "establishment_means")) |>
    dplyr::mutate(frac_annuals = n_annual / (n_annual + n_perennial))
}

# How much richness the effort standardisation removes, by climate quartile.
#
# Supports the Results claim that standardising to a common effort reduces
# richness in every taxon grouping but does so unevenly along both gradients —
# the retention percentages quoted against Extended Data Fig. 2c-f. Nothing else
# in the pipeline computed it, so the numbers in the draft were transcribed from
# a working session rather than derived from a target.
#
# BOTH raw and rarefied counts come from accum_results, not from a join between
# accum_results and data_model_*. They are then guaranteed to describe the same
# record pool: fit_accumulation() tallies n_<group> and projects S_<group>_<ne>
# off one shuffled pool per cell, whereas data_model_* counts are assembled
# through a different filter chain.
#
# CELL SET. Cells where any of the four S columns is NA are dropped: project_s()
# returns NA when a cell holds fewer than n_eval records, so at n_eval = 500 the
# retained set is effectively the >=500-record cells, matching the figure. The
# quartile breaks are computed on that set, cell-level (one bio12 and one bio15
# per cell) rather than row-level, so the four quartiles hold equal numbers of
# cells.
#
# TWO AGGREGATIONS, because "rarefied richness was X% of the raw count" is
# ambiguous and they do not agree:
#   pooled  — sum(S) / sum(n) over the cells in the quartile. Richness-weighted,
#             so species-rich cells dominate.
#   median  — median of the per-cell ratio S / n. Cell-weighted.
# Report whichever the manuscript sentence means, but report which one.
rarefaction_retention <- function(accum_results, data_model_250, n_eval = 500) {
  groups <- c(
    "native_annual", "native_perennial",
    "introduced_annual", "introduced_perennial"
  )
  s_cols <- paste0("S_", groups, "_", n_eval)
  n_cols <- paste0("n_", groups)

  climate <- data_model_250 |>
    dplyr::distinct(cell, bio12, bio15)

  cells <- accum_results |>
    dplyr::select(cell, n_records, dplyr::all_of(c(n_cols, s_cols))) |>
    dplyr::filter(!dplyr::if_any(dplyr::all_of(s_cols), is.na)) |>
    dplyr::inner_join(climate, by = "cell") |>
    dplyr::filter(!is.na(bio12), !is.na(bio15)) |>
    dplyr::mutate(
      q_bio12 = dplyr::ntile(bio12, 4),
      q_bio15 = dplyr::ntile(bio15, 4)
    )

  # One row per cell per group, so both gradients can be summarised in one pass.
  long <- cells |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(c(n_cols, s_cols)),
      names_to = c(".value", "group"),
      names_pattern = paste0("^(n|S)_(.*?)(?:_", n_eval, ")?$")
    ) |>
    dplyr::mutate(group = factor(group, levels = groups))

  summarise_by <- function(gradient, quartile_col) {
    long |>
      dplyr::rename(quartile = dplyr::all_of(quartile_col)) |>
      dplyr::group_by(gradient = gradient, quartile, group) |>
      dplyr::summarise(
        n_cells = dplyr::n(),
        raw = sum(n),
        rarefied = sum(S),
        pct_pooled = 100 * sum(S) / sum(n),
        pct_median_cell = 100 * stats::median(S[n > 0] / n[n > 0]),
        .groups = "drop"
      )
  }

  dplyr::bind_rows(
    summarise_by("bio12", "q_bio12"),
    summarise_by("bio15", "q_bio15")
  ) |>
    dplyr::mutate(
      n_eval = n_eval,
      # Q1/Q4 named as the manuscript describes them.
      quartile_label = dplyr::case_when(
        gradient == "bio12" & quartile == 1 ~ "driest",
        gradient == "bio12" & quartile == 4 ~ "wettest",
        gradient == "bio15" & quartile == 1 ~ "least seasonal",
        gradient == "bio15" & quartile == 4 ~ "most seasonal",
        TRUE ~ paste0("Q", quartile)
      )
    ) |>
    dplyr::select(
      gradient, quartile, quartile_label, group, n_cells,
      raw, rarefied, pct_pooled, pct_median_cell, n_eval
    ) |>
    dplyr::arrange(gradient, quartile, group)
}

# Raw vs rarefaction-corrected slopes. Ported from Analysis.qmd, which
# writes out sixteen near-identical blocks; this is the same computation as two
# nested loops.
#
# NB Analysis.qmd calls estimate_slopes() unqualified and never loads
# modelbased, so that chunk only runs if the package happens to be attached
# some other way. Qualified here.
#
# NB the .qmd compares against fig3_out_resample_n, which after its loop holds
# the LAST iteration — the 500-record standardisation. Made explicit as an
# argument rather than left to loop-variable leakage.
make_slopes_shift_table <- function(fig3_raw, fig3_resampled) {
  slopes_for <- function(fig3_out, label) {
    grid <- tidyr::expand_grid(
      life_history = c("annual", "perennial"),
      var = c("bio12", "bio15")
    )
    purrr::pmap_dfr(grid, function(life_history, var) {
      fit <- if (life_history == "annual") fig3_out$fit_n_annual else fig3_out$fit_n_perennial
      modelbased::estimate_slopes(fit, trend = var, by = "establishment_means") |>
        tibble::tibble() |>
        dplyr::select(Slope, SE, establishment_means) |>
        dplyr::mutate(life_history = life_history, var = var, fit = label)
    })
  }

  raw <- slopes_for(fig3_raw, "raw") |>
    dplyr::rename(raw_data = Slope, se_raw = SE) |>
    dplyr::select(-fit)
  resampled <- slopes_for(fig3_resampled, "resampled") |>
    dplyr::rename(resampled_data = Slope, se_resampled = SE) |>
    dplyr::select(-fit)

  # Join keys given explicitly: with the default (all common columns) the SE
  # columns join too, so raw and resampled rows never match.
  raw |>
    dplyr::left_join(resampled, by = c("establishment_means", "life_history", "var")) |>
    dplyr::mutate(slope_shift = raw_data - resampled_data) |>
    dplyr::select(life_history, establishment_means,
      climate_var = var,
      raw_data, se_raw, resampled_data, se_resampled, slope_shift
    ) |>
    dplyr::arrange(life_history, establishment_means, climate_var)
}
