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
RUN       <- "/Users/daniel/Downloads/ga_results/phase6_ga01"      # archive folder of this run
LABEL     <- "ga01"
REPO_DIR  <- "/Users/daniel/Documents/GitHub/personal/EwE_ST_calibration"   # functions + masks
OLD_BASE  <- "/Users/daniel/Work/ST_calibration"                   # only for maxn/ rasters
SENS_XLSX <- "/Users/daniel/Downloads/phase6/sens1/LL_rescored.xlsx"

FUNCTIONS_PATH <- file.path(REPO_DIR, "st_calibration_functions.R")
DATA_DIR  <- file.path(RUN, "output/gen30_small")                  # the 370 candidate folders
BASE_RUN  <- file.path(RUN, "output/run_base")                     # baseline model output
MAXN_DIR  <- file.path(OLD_BASE, "maxn")                           # observed gag / red grouper rasters
PLOTS     <- file.path(RUN, "plots_R")                             # new folder: old figures untouched
for(d in file.path(PLOTS, c("ts", "spatial", "residuals", "response")))
  if(!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

## ---- SETTINGS THAT DEPEND ON THIS RUN ----------------------------------------------------------
LAST_GEN <- 30      # last generation in ga_results (the old run used 27)
# ensemble = best ENS_PROP of the final population, weighted by AIC. 370 x 0.10 = 37 candidates, so
# target_ess must stay BELOW 37 (50 would fall back to near-uniform weights). To keep 50, use 0.20.
ENS_PROP <- 0.10
ENS_ESS  <- 25

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
ga2 <- subset(ga, gen >= 0 & gen <= LAST_GEN)       # gen -1 is the baseline row, left out

p_panel1 <- plot_nll_convergence_panel(ga2, "min_LL", "minimum NLL", "#0072B2",
                                       sd_color = "#D55E00",
                                       gen_breaks = seq(0, LAST_GEN, 5), sd_max = 1.2e5,
                                       primary_min = 6000)   # min_LL ranges ~6459-8550
p_panel2 <- plot_nll_mean_median_max_panel(ga2,
                                           mean_color = "#009E73", median_color = "#6A3D9A",
                                           max_color = "#CC79A7", gen_breaks = seq(0, LAST_GEN, 5))
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
  group_names = group_names, species_patterns = "gag", facet_ncol = 8,
  plots_dir = file.path(PLOTS, "ts"))
print(gag_param_summary)

redgrp_param_summary <- plot_ga_final_pop_distributions(
  gapop_final = gapop_final, gapop_init = gapop_init, ga_runs = ga_runs, catalog = catalog,
  group_names = group_names, species_patterns = "red grouper", facet_ncol = 4,
  plots_dir = file.path(PLOTS, "ts"))
print(redgrp_param_summary)

# stacked figure for the slides. Panel counts are read from the summaries (the July run had 34
# vulnerability and 20 env/red-tide/disp panels; this run has different numbers).
n_pan <- function(x, default) if(is.data.frame(x) && nrow(x) > 0) nrow(x) else default
vul_summary2 <- plot_ga_final_pop_distributions(
  gapop_final = gapop_final, gapop_init = gapop_init, ga_runs = ga_runs,
  catalog = catalog, group_names = group_names, species_patterns = "gag",
  types = "vul", facet_ncol = 6, base_size = 20, max_title_chars = 8,
  save_plot = FALSE, plots_dir = file.path(PLOTS, "ts"),
  width = 6 * 3.2, height = ceiling(34 / 6) * 3.2)
n_vul <- n_pan(vul_summary2, 34)
envrt_summary2 <- plot_ga_final_pop_distributions(
  gapop_final = gapop_final, gapop_init = gapop_init, ga_runs = ga_runs,
  catalog = catalog, group_names = group_names, species_patterns = "gag",
  types = c("env", "redtide", "disp"), facet_ncol = 6, base_size = 20, max_title_chars = 8,
  save_plot = FALSE, plots_dir = file.path(PLOTS, "ts"),
  width = 6 * 3.2, height = ceiling(20 / 6) * 3.2)
n_env <- n_pan(envrt_summary2, 20)
message("gag panels: ", n_vul, " vulnerability, ", n_env, " env/red tide/dispersal")

combined_opt2 <- patchwork::wrap_plots(
  list(attr(vul_summary2, "plot"), attr(envrt_summary2, "plot")),
  ncol = 1, heights = c(ceiling(n_vul / 6), ceiling(n_env / 6)), axes = "collect")
ggplot2::ggsave(file.path(PLOTS, "ts/ga_final_pop_distributions_gag_opt2_stacked.png"),
                plot = combined_opt2, width = 5 * 3.2,
                height = (ceiling(n_vul / 6) + ceiling(n_env / 6)) * 3, dpi = 200, units = "in")

## ---- 4. OBSERVED vs PREDICTED (baseline = run_base, best fit, ensemble) ------------------------
fit_summary <- plot_obs_vs_pred(
  obs, runs = c(baseline = BASE_RUN, best_fit = best_dir),
  vars = c("Biomass", "Catch", "F"), plots_dir = file.path(PLOTS, "ts"))
print(fit_summary[, c("variable", "group_label", "region_codes", "runs_plotted", "png_file", "missing_by_run")])

diag <- diagnose_candidate_outputs(ga_runs, vars = c("Biomass", "Catch"))

ens_summary <- plot_ga_ensemble(
  obs, ga_runs, init_run_dir = BASE_RUN, vars = c("Biomass", "Catch", "F"),
  species_patterns = c("gag", "red grouper"), keep_fitness_quantile = 0.9,
  plots_dir = file.path(PLOTS, "ts"))
print(ens_summary)

facet_aic <- plot_ga_ensemble_faceted(
  obs, ga_runs, init_run_dir = BASE_RUN, vars = c("Biomass", "Catch", "F"),
  species_patterns = c("gag", "red grouper"), ens_mode = "aic",
  top_n_prop = ENS_PROP, target_ess = ENS_ESS, plots_dir = file.path(PLOTS, "ts"))
print(facet_aic)

# regions + WFS panel for gag age 0, ensemble with the baseline as a black dashed line
region_ts <- plot_region_timeseries_grid(
  ga_runs = ga_runs, species_pattern = "gag 0", init_run_dir = BASE_RUN,
  region_mask = age0_regions, region_attributes = region_attrs, obs = obs,
  domain_obs_pattern = "FWCNMFS", map_show_land = TRUE,
  show_ensemble = TRUE, top_n_prop = ENS_PROP, target_ess = ENS_ESS,
  ensemble_max_folders = N_CAND, plots_dir = file.path(PLOTS, "ts"))
print(table(region_ts$run, region_ts$region_id))   # an "init" row per region = baseline is drawn

gen_summary <- plot_ga_generation_best(
  obs, ga_runs, vars = c("Biomass", "Catch", "F"), species_patterns = c("gag", "red grouper"),
  palette = "viridis", plots_dir = file.path(PLOTS, "ts"))

## ---- 5. SPATIAL MAPS, RED TIDE MORTALITY, RESIDUALS --------------------------------------------
map_root_dir <- DATA_DIR
all_folders  <- list.dirs(map_root_dir, recursive = FALSE, full.names = TRUE)   # run_base is NOT in here
message(length(all_folders), " candidate folders")
n_cores_available <- if(.Platform$OS.type == "unix" && requireNamespace("parallel", quietly = TRUE))
  max(1, parallel::detectCores() - 5) else 1
lon_breaks <- seq(-86, -82, by = 2); lat_breaks <- seq(26, 30, by = 2)

ens_map_summary_aic <- plot_ecospace_ensemble_maps_paired(
  map_root_dir = map_root_dir, ga_runs = ga_runs, run_folders = all_folders, ens_mode = "aic",
  top_n_prop = ENS_PROP, target_ess = ENS_ESS, species_patterns = c("gag", "red grouper"),
  vars = c("Biomass", "Catch", "F"), variability = NULL, log_scale = TRUE,
  max_folders = N_CAND, n_cores = n_cores_available, land_mask = land_mask,
  lon_breaks = lon_breaks, lat_breaks = lat_breaks, stanzas_per_row = 3,
  barheight = 2.8, barwidth = 0.45, plots_dir = file.path(PLOTS, "spatial"))
print(ens_map_summary_aic)

ens_map_paired_summary_aic <- plot_ecospace_ensemble_maps_paired(
  map_root_dir = map_root_dir, ga_runs = ga_runs, run_folders = all_folders, ens_mode = "aic",
  top_n_prop = ENS_PROP, target_ess = ENS_ESS, species_patterns = c("gag", "red grouper"),
  vars = c("Biomass", "Catch", "F"), variability = "cv", log_scale = TRUE,
  max_folders = N_CAND, n_cores = n_cores_available, land_mask = land_mask,
  lon_breaks = lon_breaks, lat_breaks = lat_breaks, stanzas_per_row = 2,
  barheight = 2.8, barwidth = 0.45, plots_dir = file.path(PLOTS, "spatial"))
print(ens_map_paired_summary_aic)

m0_groups <- c(paste0("gag ", c(0:4, "5+")), paste0("red grouper ", c(0:4, "5+")),
               "gag total", "red grouper total")
ens_m0_aic <- plot_ecospace_ensemble_m0(
  map_root_dir = map_root_dir, ga_runs = ga_runs, ens_mode = "aic",
  top_n_prop = ENS_PROP, target_ess = ENS_ESS, run_folders = all_folders,
  species_patterns = c("gag", "red grouper"), groups = m0_groups,
  max_folders = N_CAND, plots_dir = file.path(PLOTS, "ts"))
print(ens_m0_aic)

ens_m0_single <- plot_ecospace_ensemble_m0(
  map_root_dir = map_root_dir, ga_runs = ga_runs, ens_mode = "aic",
  top_n_prop = ENS_PROP, target_ess = ENS_ESS, run_folders = all_folders,
  species_patterns = c("gag", "red grouper"), groups = m0_groups,
  max_folders = N_CAND, single_plot = TRUE, single_plot_uncertainty = TRUE,
  y_lab = expression("Red tide mortality"~(year^{-1})), plots_dir = file.path(PLOTS, "ts"))
print(ens_m0_single)

resid_faceted_aic <- plot_spatial_residuals_batch(
  raster_dir = MAXN_DIR, ga_runs = ga_runs, ens_mode = "aic",
  top_n_prop = ENS_PROP, target_ess = ENS_ESS, species_patterns = c("gag", "red grouper"),
  land_mask = land_mask, lon_breaks = lon_breaks, lat_breaks = lat_breaks,
  barheight = 2.8, barwidth = 0.45, facet_ncol = 3, plots_dir = file.path(PLOTS, "residuals"))
print(resid_faceted_aic)

resid_faceted_older_aic <- plot_spatial_residuals_older_stanzas(
  raster_dir = MAXN_DIR, ga_runs = ga_runs, ens_mode = "aic",
  top_n_prop = ENS_PROP, target_ess = ENS_ESS, land_mask = land_mask,
  lon_breaks = lon_breaks, lat_breaks = lat_breaks, barheight = 2.8, barwidth = 0.45,
  facet_ncol = 3, plots_dir = file.path(PLOTS, "residuals"))
print(resid_faceted_older_aic)

## ---- 6. RESPONSE FUNCTIONS ---------------------------------------------------------------------
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
message("Done. Figures are in ", PLOTS)
