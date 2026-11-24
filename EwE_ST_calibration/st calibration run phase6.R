#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# st_calibration_run_phase6.R -- config + execution for the PHASE 6 GA run ("ga01").
#
# What changed vs. the July run (phase3_20260724_084924):
#   * 370 candidates per generation (not 910), 30 generations (not 27 usable), spatial weight 15
#   * file names carry the label "ga01" instead of a timestamp, and live in one archive folder
#   * baseline = output/run_base (phase 5 model, scenario 8), NOT sp03_5min_phase3_init
#   * sensitivity workbook = phase6/sens1/LL_rescored.xlsx
#   * 98 parameters (41 vul, 16 env, 32 red tide, 3 disp, 6 fleet) -> panel counts are computed
#
# EDIT ONLY THE "PATHS" BLOCK. Everything else follows from it.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## ---- PATHS ---------------------------------------------------------------------------------
RUN       <- "/Users/daniel/Work/ST_calibration/phase6_ga01"       # ONE folder: inputs/, output/, results/, sens1/, plots_R/
LABEL     <- "ga01"
REPO_DIR  <- "/Users/daniel/Documents/GitHub/personal/EwE_ST_calibration"   # functions + masks
OLD_BASE  <- "/Users/daniel/Work/ST_calibration"                   # only for maxn/ rasters (shared, top level)
SENS_XLSX <- file.path(RUN, "sens1", "LL_rescored.xlsx")
PARVEC    <- file.path(RUN, "results", "parvec_ga01.csv")           # per-parameter GA bounds

FUNCTIONS_PATH <- file.path(REPO_DIR, "st_calibration_functions.R")
DATA_DIR  <- file.path(RUN, "output/gen30_small")                  # the 370 candidate folders
BASE_RUN  <- file.path(RUN, "output/run_base")                     # baseline model output
MAXN_DIR  <- file.path(OLD_BASE, "maxn")                           # observed gag / red grouper rasters
PLOTS     <- file.path(RUN, "plots_R")                             # new folder: old figures untouched
for(d in file.path(PLOTS, c("ts", "spatial", "residuals", "response")))
  if(!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

## ---- SETTINGS THAT DEPEND ON THIS RUN ----------------------------------------------------------
LAST_GEN <- 30      # last generation in ga_results (the old run used 27)
# ensemble = the best ENS_PROP of the final population (lowest NLL), equal weights: 370 x 0.10 = 37 models.
ENS_PROP <- 0.10

# FALSE = only the figures used in the manuscript and its supplement. TRUE = also the extra
# diagnostics (obs vs predicted for baseline and best, all-models time series, best-vs-mean figure,
# generation-by-generation best, per-group red tide mortality, response functions by driver, RRMSE).
EXTRA_PLOTS <- FALSE

library(data.table); library(ggplot2); library(patchwork); library(scales)
if(requireNamespace("RColorBrewer", quietly = TRUE)) library(RColorBrewer)
source(FUNCTIONS_PATH)

## ---- 1. LOAD THE RUN ---------------------------------------------------------------------------
ga_runs <- read_ga_runs(file.path(RUN, "results", paste0("final_ga_runs_", LABEL, ".csv")))
ga_runs <- resolve_run_dirs(ga_runs, gen_dir = DATA_DIR)
N_CAND  <- sum(ga_runs$exists)
message("Candidates on disk: ", N_CAND, " (expected 370)")

best_row <- ga_runs[ga_runs$exists, ][which.min(ga_runs[ga_runs$exists, ]$fitness), ]
message("Best-fit candidate: ", best_row$run_folder, " (fitness = ", signif(best_row$fitness, 6), ")")
best_dir <- best_row$run_dir

obs <- read_ewe_timeseries(file.path(RUN, "inputs/ts_mice_v5_discards_ecospace_regions_sedar105.csv"))

region_attrs <- read.csv(file.path(REPO_DIR, "combined_regions_5min_attributes.csv"), stringsAsFactors = FALSE)
age0_regions <- read_esri_ascii(file.path(REPO_DIR, "age0_survey_regions_5min_mod.asc"))
land_mask    <- read_esri_ascii(file.path(REPO_DIR, "combined_regions_5min.asc"))

catalog <- read_sensitivity_response_catalog(SENS_XLSX)

## ---- 2. GA CONVERGENCE -------------------------------------------------------------------------
ga <- read.csv(file.path(RUN, "results", paste0("ga_results_", LABEL, ".csv")),
               skip = 6, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
ga2 <- subset(ga, gen >= 0 & gen <= LAST_GEN)       # gen -1 is the baseline row, left out of the lines
baseline_NLL <- ga$min_LL[ga$gen == -1]             # total NLL of the baseline parameter set, drawn as a dashed line

p_panel1 <- plot_nll_convergence_panel(ga2, "min_LL", "minimum NLL", "#0072B2",
                                       sd_color = "#D55E00",
                                       gen_breaks = seq(0, LAST_GEN, 5), sd_max = 1.2e5,
                                       primary_min = 6000,   # min_LL ranges ~6459-8550
                                       baseline = baseline_NLL,   # dashed line; give its value in the caption
                                       legend_position = c(0.95, 0.80))
p_panel2 <- plot_nll_mean_median_max_panel(ga2,
                                           mean_color = "#009E73", median_color = "#6A3D9A",
                                           max_color = "#CC79A7", gen_breaks = seq(0, LAST_GEN, 5),
                                           baseline = baseline_NLL)
p_nll_combined <- p_panel1 + p_panel2
p_nll_combined
ggsave(file.path(PLOTS, "ts/ga_convergence_nll.png"), plot = p_nll_combined,
       width = 8, height = 3, dpi = 250, units = "in")

## ---- 3. PARAMETER DISTRIBUTIONS (first vs. final population) -----------------------------------
gapop_final <- read_ga_pop(file.path(RUN, "results", paste0("final_ga_pop", LABEL, ".csv")))
gapop_init  <- read_ga_pop(file.path(RUN, "results", paste0("init_ga_pop",  LABEL, ".csv")))
group_names <- get_region_output(0L, "Biomass", best_dir, new.env(parent = emptyenv()))$group_names

gag_param_summary <- plot_ga_final_pop_distributions(
  gapop_final = gapop_final, gapop_init = gapop_init, ga_runs = ga_runs, catalog = catalog,
  group_names = group_names, species_patterns = "gag", facet_ncol = 8, parvec = PARVEC,
  plots_dir = file.path(PLOTS, "ts"))
print(gag_param_summary)

redgrp_param_summary <- plot_ga_final_pop_distributions(
  gapop_final = gapop_final, gapop_init = gapop_init, ga_runs = ga_runs, catalog = catalog,
  group_names = group_names, species_patterns = "red grouper", facet_ncol = 4, parvec = PARVEC,
  plots_dir = file.path(PLOTS, "ts"))
print(redgrp_param_summary)

# ALL estimated parameters (forced red tide pars, other species' vuls and red tide curves, dispersal,
# fleet dynamics) -- check that nothing in parvec is missing from the figures
all_param_summary <- plot_ga_final_pop_distributions(
  gapop_final = gapop_final, gapop_init = gapop_init, ga_runs = ga_runs, catalog = catalog,
  group_names = group_names, species_patterns = NULL, facet_ncol = 10, parvec = PARVEC,
  png_file = "ga_final_pop_distributions_ALL_parameters.png", plots_dir = file.path(PLOTS, "ts"))
pv <- read.csv(PARVEC, row.names = 1, check.names = FALSE)
message("parvec: ", nrow(pv), " parameters; in the all-parameter figure: ", nrow(all_param_summary),
        "; missing: ", paste(setdiff(rownames(pv), all_param_summary$param), collapse = ", "))

# which gag / red grouper parameters were tested in the sensitivity analysis and which were calibrated
tested_tbl <- plot_tested_vs_calibrated(
  SENS_XLSX, PARVEC, gapop_init, gapop_final, ga_runs, group_names, catalog = catalog,
  species_patterns = c("gag", "red grouper"), plots_dir = file.path(PLOTS, "ts"))
print(table(tested_tbl$type, tested_tbl$status))

# stacked figure for the slides. Panel counts are read from the summaries (the July run had 34
# vulnerability and 20 env/red-tide/disp panels; this run has different numbers).
n_pan <- function(x, default) if(is.data.frame(x) && nrow(x) > 0) nrow(x) else default
vul_summary2 <- plot_ga_final_pop_distributions(
  gapop_final = gapop_final, gapop_init = gapop_init, ga_runs = ga_runs,
  catalog = catalog, group_names = group_names, species_patterns = "gag", parvec = PARVEC,
  types = "vul", facet_ncol = 5, base_size = 12, max_title_chars = 7, title_style = "names", title_scale = 0.76, line_scale = 0.5, x_axis_all = TRUE,
  save_plot = FALSE, plots_dir = file.path(PLOTS, "ts"),
  width = 6 * 3.2, height = ceiling(34 / 6) * 3.2)
n_vul <- n_pan(vul_summary2, 34)
envrt_summary2 <- plot_ga_final_pop_distributions(
  gapop_final = gapop_final, gapop_init = gapop_init, ga_runs = ga_runs,
  catalog = catalog, group_names = group_names, species_patterns = "gag", parvec = PARVEC,
  types = c("env", "redtide", "disp"), facet_ncol = 5, base_size = 12, max_title_chars = 7, title_scale = 0.76, line_scale = 0.5, x_axis_all = TRUE,
  save_plot = FALSE, plots_dir = file.path(PLOTS, "ts"),
  width = 6 * 3.2, height = ceiling(20 / 6) * 3.2)
n_env <- n_pan(envrt_summary2, 20)
message("gag panels: ", n_vul, " vulnerability, ", n_env, " env/red tide/dispersal")

# ONE grid for all panels (vulnerabilities, then responses), so every panel has the same size.
# The vulnerability block is padded to full rows before the response panels start.
pad_to_row <- function(pl, ncol = 5) c(pl, rep(list(patchwork::plot_spacer()), (-length(pl)) %% ncol))
# key to the lines/bars, placed in the first empty cell of the last row
key_df <- data.frame(
  y = 4:1,
  lab = c("Final population", "Baseline model", "Best model", "Search bounds"),
  col = c("grey45", "black", "#0072B2", "red"),
  lt  = c("solid", "22", "solid", "22"),
  lw  = c(2.2, 0.7, 0.7, 0.7), stringsAsFactors = FALSE)
legend_panel <- ggplot2::ggplot(key_df) +
  ggplot2::geom_segment(ggplot2::aes(x = 0.0, xend = 0.2, y = y, yend = y, colour = col, linetype = lt, linewidth = lw)) +
  ggplot2::geom_text(ggplot2::aes(x = 0.25, y = y, label = lab), hjust = 0, size = 3.6) +
  ggplot2::scale_colour_identity() + ggplot2::scale_linetype_identity() + ggplot2::scale_linewidth_identity() +
  ggplot2::scale_x_continuous(limits = c(-0.14, 1.8), expand = c(0, 0)) + ggplot2::scale_y_continuous(limits = c(0.4, 4.6), expand = c(0, 0)) +
  ggplot2::coord_cartesian(clip = "off") + ggplot2::theme_void()
all_panels <- c(pad_to_row(attr(vul_summary2, "panels")), pad_to_row(attr(envrt_summary2, "panels")))
first_empty <- length(pad_to_row(attr(vul_summary2, "panels"))) + length(attr(envrt_summary2, "panels")) + 1
if(first_empty <= length(all_panels)) all_panels[[first_empty]] <- legend_panel
combined_opt2 <- patchwork::wrap_plots(all_panels, ncol = 5, byrow = TRUE)
# 5 columns, 7 in wide (full page width): axis text ~9 pt, panel titles ~9 pt, key ~10 pt at print size
ggplot2::ggsave(file.path(PLOTS, "ts/ga_final_pop_distributions_gag_opt2_stacked.png"),
                plot = combined_opt2, width = 7, height = 9.6, dpi = 300, units = "in")

## ---- 3b. ALL tested parameters (calibrated + not calibrated) for gag and for red grouper -------
# Same layout as the figure above. Parameters that were tested in the sensitivity analysis but not
# calibrated keep their baseline value: grey panels showing only the baseline (black dashed line).
make_key_panel <- function(with_uncal = TRUE){
  k <- data.frame(
    lab = c("Final population", "Baseline model", "Best model", "Search bounds", "Not calibrated\n(baseline only)"),
    col = c("grey45", "black", "#0072B2", "red", "grey80"),
    lt  = c("solid", "22", "solid", "22", "solid"),
    lw  = c(2.2, 0.7, 0.7, 0.7, 4), stringsAsFactors = FALSE)
  if(!with_uncal) k <- k[1:4, ]
  k$y <- rev(seq_len(nrow(k)))
  ggplot2::ggplot(k) +
    ggplot2::geom_segment(ggplot2::aes(x = 0.0, xend = 0.2, y = y, yend = y, colour = col, linetype = lt, linewidth = lw)) +
    ggplot2::geom_text(ggplot2::aes(x = 0.25, y = y, label = lab), hjust = 0, size = 2.2, lineheight = 0.85) +
    ggplot2::scale_colour_identity() + ggplot2::scale_linetype_identity() + ggplot2::scale_linewidth_identity() +
    ggplot2::scale_x_continuous(limits = c(-0.14, 1.08), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0.4, nrow(k) + 0.6), expand = c(0, 0)) +
    ggplot2::coord_cartesian(clip = "off") + ggplot2::theme_void() + ggplot2::theme(aspect.ratio = 1)
}
all_tested_figure <- function(patterns, png_name, page_w = 8.27, ratio = 297 / 210){
  aug <- add_uncalibrated_params(tested_tbl, catalog, gapop_init, gapop_final, PARVEC)
  build <- function(ncol){
    common <- list(gapop_final = aug$gapop_final, gapop_init = aug$gapop_init, ga_runs = ga_runs,
                   catalog = catalog, group_names = group_names, species_patterns = patterns,
                   parvec = aug$parvec, uncal_ids = aug$uncal_ids, facet_ncol = ncol,
                   base_size = if(ncol >= 8) 6.3 else 9, title_scale = if(ncol >= 8) 0.95 else 0.75,   # narrower panels: smaller text
                   max_title_chars = 7, line_scale = if(ncol >= 8) 0.4 else 0.5, panel_aspect = 1,
                   x_axis_all = TRUE, save_plot = FALSE, plots_dir = file.path(PLOTS, "ts"))
    list(vul = do.call(plot_ga_final_pop_distributions, c(common, list(types = "vul", title_style = "names"))),
         other = do.call(plot_ga_final_pop_distributions, c(common, list(types = c("env", "redtide", "disp")))))
  }
  # number of columns that gives the page closest to A4 portrait (panels are ~1.24 x taller than wide
  # including the title and axis labels)
  first <- build(6); nv <- length(attr(first$vul, "panels")); ne <- length(attr(first$other, "panels"))
  cand <- 6:9
  rows_for <- function(nc) ceiling(nv / nc) + ceiling(ne / nc) + as.integer(ceiling(ne / nc) * nc == ne)   # no free cell left for the key: it gets its own row
  ratio_for <- function(nc) rows_for(nc) * 1.24 / nc
  ncol <- cand[which.min(abs(ratio_for(cand) - ratio))]
  b <- if(ncol == 6) first else build(ncol)
  pv <- pad_to_row(attr(b$vul, "panels"), ncol); pe <- pad_to_row(attr(b$other, "panels"), ncol)
  allp <- c(pv, pe)
  fe <- length(pv) + length(attr(b$other, "panels")) + 1
  if(fe > length(allp)) allp <- c(allp, list(make_key_panel()), rep(list(patchwork::plot_spacer()), ncol - 1))
  else allp[[fe]] <- make_key_panel()
  fig <- patchwork::wrap_plots(allp, ncol = ncol, byrow = TRUE)
  h <- ceiling(length(allp) / ncol) * (page_w / ncol) * 1.24 + 0.1
  ggplot2::ggsave(file.path(PLOTS, "ts", png_name), plot = fig, width = page_w,
                  height = h, dpi = 300, units = "in", limitsize = FALSE)
  message(png_name, ": ", nv, " vulnerability + ", ne, " response/dispersal panels, ", ncol,
          " columns, ", page_w, " x ", round(h, 1), " in (A4 = 8.27 x 11.69)")
  invisible(b)
}
gag_all_tested    <- all_tested_figure("gag",         "ga_final_pop_distributions_gag_ALL_tested.png")
redgrp_all_tested <- all_tested_figure("red grouper", "ga_final_pop_distributions_redgrouper_ALL_tested.png")

## ---- 4. OBSERVED vs PREDICTED (baseline = run_base, best fit, ensemble) ------------------------
if(EXTRA_PLOTS){
  fit_summary <- plot_obs_vs_pred(
    obs, runs = c(baseline = BASE_RUN, best_fit = best_dir),
    vars = c("Biomass", "Catch", "F"), plots_dir = file.path(PLOTS, "ts"))
  print(fit_summary[, c("variable", "group_label", "region_codes", "runs_plotted", "png_file", "missing_by_run")])

  diag <- diagnose_candidate_outputs(ga_runs, vars = c("Biomass", "Catch"))
}

if(EXTRA_PLOTS){
  ens_summary <- plot_ga_ensemble(
    obs, ga_runs, init_run_dir = BASE_RUN, vars = c("Biomass", "Catch", "F"),
    species_patterns = c("gag", "red grouper"),
    plots_dir = file.path(PLOTS, "ts"))
  print(ens_summary)
}

if(EXTRA_PLOTS){
  facet_aic <- plot_ga_ensemble_faceted(
    obs, ga_runs, init_run_dir = BASE_RUN, vars = c("Biomass", "Catch", "F"),
    species_patterns = c("gag", "red grouper"),
    ens_mode = "quantile", keep_fitness_quantile = 1,   # ALL last-generation models, coloured by NLL
    palette = "plasma", alpha = 1, line_width = 0.4, file_suffix = "_all_models", plots_dir = file.path(PLOTS, "ts"))    # same colours as Figs 4-5; + baseline + best only
  print(facet_aic)
}

# Figs 4-5 and supplementary catch / F: the best ENS_PROP of the final population, coloured by NLL, + baseline + best
facet_aic_mean <- plot_ga_ensemble_faceted(
  obs, ga_runs, init_run_dir = BASE_RUN, vars = c("Biomass", "Catch", "F"),
  species_patterns = c("gag", "red grouper"), ens_mode = "quantile",
  keep_fitness_quantile = ENS_PROP, show_mean = TRUE, alpha = 1, line_width = 0.4, palette = "plasma",   # manuscript look (Figs 4, 5, S catch / F):
  ylim_from_obs = FALSE,                                             # reversed plasma (yellow = best NLL), full y-axis
  plots_dir = file.path(PLOTS, "ts"))                        # gradient + baseline + best + ensemble mean
if(EXTRA_PLOTS){
  facet_best_vs_mean <- plot_ga_ensemble_faceted(
    obs, ga_runs, init_run_dir = BASE_RUN, vars = c("Biomass", "Catch", "F"),
    species_patterns = c("gag", "red grouper"), ens_mode = "quantile",
    keep_fitness_quantile = ENS_PROP, show_mean = TRUE, show_candidates = FALSE, file_suffix = "_best_vs_mean",
    plots_dir = file.path(PLOTS, "ts"))                        # ONLY baseline + best + mean
}

best_vs_mean <- compare_best_vs_ensemble_mean(ga_runs, keep_fitness_quantile = ENS_PROP)
print(best_vs_mean)   # correlation, mean |% difference|, relative RMSE per stanza and variable
write.csv(best_vs_mean, file.path(PLOTS, "ts", "best_vs_ensemble_mean.csv"), row.names = FALSE)

# regions + WFS panel for gag age 0: baseline (black dashed), best (teal dashed) and all last-generation models (magma = NLL)
region_ts <- plot_region_timeseries_grid(
  ga_runs = ga_runs, species_pattern = "gag 0", init_run_dir = BASE_RUN,
  region_mask = age0_regions, region_attributes = region_attrs, obs = obs,
  domain_obs_pattern = "FWCNMFS", map_show_land = TRUE,
  show_ensemble = TRUE, ensemble_style = "ribbon",   # manuscript look: baseline (dashed) + ensemble mean + 95% band per region
  ens_mode = "quantile", keep_fitness_quantile = ENS_PROP,
  ensemble_max_folders = N_CAND, plots_dir = file.path(PLOTS, "ts"))
print(table(region_ts$run, region_ts$region_id))   # an "init" row per region = baseline is drawn

if(EXTRA_PLOTS){
  gen_summary <- plot_ga_generation_best(
    obs, ga_runs, init_run_dir = BASE_RUN, vars = c("Biomass", "Catch", "F"),
    species_patterns = c("gag", "red grouper"), palette = "magma", plots_dir = file.path(PLOTS, "ts"))
}

## ---- 5. SPATIAL MAPS, RED TIDE MORTALITY, RESIDUALS --------------------------------------------
map_root_dir <- DATA_DIR
all_folders  <- list.dirs(map_root_dir, recursive = FALSE, full.names = TRUE)   # run_base is NOT in here
message(length(all_folders), " candidate folders")
n_cores_available <- if(.Platform$OS.type == "unix" && requireNamespace("parallel", quietly = TRUE))
  max(1, parallel::detectCores() - 5) else 1
lon_breaks <- seq(-86, -82, by = 2); lat_breaks <- seq(26, 30, by = 2)

ens_map_summary_aic <- plot_ecospace_ensemble_maps_paired(
  map_root_dir = map_root_dir, ga_runs = ga_runs, run_folders = all_folders, ens_mode = "quantile",
  keep_fitness_quantile = ENS_PROP, species_patterns = c("gag", "red grouper"),
  vars = c("Biomass", "Catch", "F"), variability = NULL, log_scale = TRUE,
  max_folders = N_CAND, n_cores = n_cores_available, land_mask = land_mask,
  lon_breaks = lon_breaks, lat_breaks = lat_breaks, stanzas_per_row = 3,
  barheight = 2.8, barwidth = 0.45, plots_dir = file.path(PLOTS, "spatial"))
print(ens_map_summary_aic)

ens_map_paired_summary_aic <- plot_ecospace_ensemble_maps_paired(
  map_root_dir = map_root_dir, ga_runs = ga_runs, run_folders = all_folders, ens_mode = "quantile",
  keep_fitness_quantile = ENS_PROP, species_patterns = c("gag", "red grouper"),
  vars = c("Biomass", "Catch", "F"), variability = "cv", log_scale = TRUE,
  max_folders = N_CAND, n_cores = n_cores_available, land_mask = land_mask,
  lon_breaks = lon_breaks, lat_breaks = lat_breaks, stanzas_per_row = 2,
  barheight = 2.8, barwidth = 0.45, plots_dir = file.path(PLOTS, "spatial"))
print(ens_map_paired_summary_aic)

m0_groups <- c(paste0("gag ", c(0:4, "5+")), paste0("red grouper ", c(0:4, "5+")),
               "gag total", "red grouper total")
if(EXTRA_PLOTS){
  ens_m0_aic <- plot_ecospace_ensemble_m0(
    map_root_dir = map_root_dir, ga_runs = ga_runs, ens_mode = "quantile",
    keep_fitness_quantile = ENS_PROP, run_folders = all_folders,
    species_patterns = c("gag", "red grouper"), groups = m0_groups,
    max_folders = N_CAND, plots_dir = file.path(PLOTS, "ts"))
  print(ens_m0_aic)
}

ens_m0_single <- plot_ecospace_ensemble_m0(
  map_root_dir = map_root_dir, ga_runs = ga_runs, ens_mode = "quantile",
  keep_fitness_quantile = ENS_PROP, run_folders = all_folders,
  species_patterns = c("gag", "red grouper"), groups = m0_groups,
  max_folders = N_CAND, single_plot = TRUE, single_plot_uncertainty = TRUE,
  y_lab = expression("Red tide mortality"~(year^{-1})), plots_dir = file.path(PLOTS, "ts"))
print(ens_m0_single)

resid_faceted_aic <- plot_spatial_residuals_batch(
  raster_dir = MAXN_DIR, ga_runs = ga_runs, ens_mode = "quantile",
  keep_fitness_quantile = ENS_PROP, species_patterns = c("gag", "red grouper"),
  land_mask = land_mask, lon_breaks = lon_breaks, lat_breaks = lat_breaks,
  barheight = 2.8, barwidth = 0.45, facet_ncol = 3, plots_dir = file.path(PLOTS, "residuals"))
print(resid_faceted_aic)

resid_faceted_older_aic <- plot_spatial_residuals_older_stanzas(
  raster_dir = MAXN_DIR, ga_runs = ga_runs, ens_mode = "quantile",
  keep_fitness_quantile = ENS_PROP, land_mask = land_mask,
  lon_breaks = lon_breaks, lat_breaks = lat_breaks, barheight = 2.8, barwidth = 0.45,
  facet_ncol = 3, plots_dir = file.path(PLOTS, "residuals"))
print(resid_faceted_older_aic)

## ---- 6. RESPONSE FUNCTIONS ---------------------------------------------------------------------
if(EXTRA_PLOTS){
  driver_map <- build_driver_map_from_sensitivity(ga_runs, catalog)
  print(driver_map)
  for(drv in c("depth", "temp", "red tide"))
    plot_ga_response_by_driver_map(
      ga_runs, driver_map, driver_name = drv, species_patterns = c("gag", "red grouper"),
      keep_fitness_quantile = 0.9, max_folders = N_CAND, catalog = catalog,
      plots_dir = file.path(PLOTS, "response"))

  rrmse_df  <- compute_rrmse_by_candidate(obs, ga_runs, vars = c("Biomass", "Catch", "F"),
                                          species_patterns = c("gag", "red grouper"),
                                          max_candidates = 1000)
  rrmse_cor <- plot_rrmse_vs_fitness(rrmse_df, facet_by = "series", plots_dir = file.path(PLOTS, "ts"))
  print(rrmse_cor)
}

## ---- 7. CONTRIBUTION OF COMPONENTS (manuscript Fig. X) ------------------------------------------
# Needs the output of sections 1 and 3.1 of "st results phase6.R" in plots_R/.
# Stanza biomass series are reference series and are left out (set EXCLUDE_STANZA_BIOMASS in the script).
fig_needs <- file.path(PLOTS, c("nll_temporal_by_series.csv", "nll_spatial_by_map.csv",
                                "nll_partition_temporal_vs_spatial.csv",
                                file.path("discussion_checks", "06_member_series_nll.csv")))
if(all(file.exists(fig_needs))){
  local(source(file.path(REPO_DIR, "fig contribution and parameters phase6.R"), local = TRUE))
} else {
  message("Skipping section 7, missing: ", paste(basename(fig_needs[!file.exists(fig_needs)]), collapse = ", "))
}

message("Done. Figures are in ", PLOTS)
