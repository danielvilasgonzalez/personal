#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# st_calibration_run.R -- config + execution. Sources the function library, then runs
# the actual workflow against your real file paths.
#
# TO RUN THIS ON A DIFFERENT MACHINE / AS A DIFFERENT USER: edit ONLY the BASE_DIR line
# below.
#
#   BASE_DIR -- the top-level project folder. st_calibration_functions.R, the
#     phase3_.../ and results/ folders (raw GA output), and the "plots/" output
#     folder (every plots_dir) all live DIRECTLY here.
#   DATA_DIR -- computed automatically as file.path(BASE_DIR, "output"). Observed data
#     (combined_regions_*, age0_survey_*, maxn/) and the candidate run folders
#     themselves (gen_dir, map_root_dir) live inside this "output" subfolder.
#
# Changing BASE_DIR alone is enough to relocate the whole thing, as long as your own
# layout matches this same two-level structure (scripts + phase3/results directly in
# BASE_DIR, everything else one level down in an "output" subfolder). If that's not
# how your files are laid out, or if some OTHER script-like file lives somewhere else
# entirely, let me know and I'll adjust the specific line(s).
#
# Then run the whole file (comment out any block you don't want).
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

BASE_DIR <- "/Users/daniel/Work/ST_calibration"
DATA_DIR <- file.path(BASE_DIR, "output")

FUNCTIONS_PATH <- file.path(BASE_DIR, "st_calibration_functions.R")

# must be library()'d, not just installed -- data.table's .()/:= syntax needs it attached
library(data.table)
library(ggplot2)
library(patchwork)
library(scales)
# readxl is also required (for read_sensitivity_response_catalog(), further down) --
# it's checked via requireNamespace() internally so it doesn't need library() here,
# but it does need to be installed: install.packages("readxl")
if(requireNamespace("RColorBrewer", quietly = TRUE)) library(RColorBrewer)

source(FUNCTIONS_PATH)

## ---- PART 3: GA convergence + vulnerability spread ------------------------------------

ga <- read.csv(
  file.path(BASE_DIR, "phase3_20260724_084924/results/ga_results_20260724_084924.csv"),
  skip = 6, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE
)
ga2 <- subset(ga, gen >= 0 & gen <= 27)

# min | median | max NLL, each with its own SD NLL on a secondary axis (falls back to
# min/max only if median_LL isn't in the CSV). sd_max fixes the secondary axis to the
# SAME [0, sd_max] range across all three panels.
has_median_LL <- "median_LL" %in% names(ga2)
p_min <- plot_nll_convergence_panel(ga2, "min_LL", "minimum NLL", "#1B4F72",
                                    gen_breaks = seq(0, 27, 5), sd_max = 1.5e5,
                                    primary_min = 4000)  # zoomed -- min_LL only ranges ~4700-5700
p_max <- plot_nll_convergence_panel(ga2, "max_LL", "maximum NLL", "#2E8B57",
                                    gen_breaks = seq(0, 27, 5), sd_max = 1.5e5)
if(has_median_LL){
  p_median <- plot_nll_convergence_panel(ga2, "median_LL", "median NLL", "#8E44AD",
                                         gen_breaks = seq(0, 27, 5), sd_max = 1.5e5)
  p_nll_combined <- p_min + p_median + p_max
} else {
  p_nll_combined <- p_min + p_max
}
p_nll_combined

nll_plots_dir <- file.path(BASE_DIR, "plots/ts")
if(!dir.exists(nll_plots_dir)) dir.create(nll_plots_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(nll_plots_dir, "ga_convergence_nll.png"), plot = p_nll_combined,
       width = if(has_median_LL) 12 else 6, height = 3, dpi = 250, units = "in")


## ---- PART 4: observed vs. predicted time series ----------------------------------------

base_dir <- file.path(BASE_DIR, "sp03_5min_phase3_init/")
gen_dir  <- DATA_DIR
# explicit DATA_DIR, not derived from dirname(base_dir) -- that derivation only held
# while base_dir lived one level down inside DATA_DIR; base_dir now correctly points
# directly under BASE_DIR instead (see the fix above), so the two are no longer related
obs <- read_ewe_timeseries(file.path(BASE_DIR, "ts_mice_v5_discards_ecospace_regions_sedar105(in).csv"))

fit_summary <- plot_obs_vs_pred(
  obs,
  runs = c(
    baseline = base_dir,
    best_fit = file.path(gen_dir, "run_g027_i0044_pid27288_hde315d0d1d050769_fD8kg1XW")
  ),
  vars      = c("Biomass", "Catch", "F"),
  plots_dir = file.path(BASE_DIR, "plots/ts")
)
print(fit_summary[, c("variable", "group_label", "region_codes", "runs_plotted", "png_file", "missing_by_run")])

ga_runs <- read_ga_runs(file.path(BASE_DIR, "results/final_ga_runs_20260724_084924.csv"))
ga_runs <- resolve_run_dirs(ga_runs, gen_dir = gen_dir)

best_row <- ga_runs[ga_runs$exists, ][which.min(ga_runs[ga_runs$exists, ]$fitness), ]
message("Best-fit candidate: ", best_row$run_folder, " (fitness = ", signif(best_row$fitness, 6), ")")

diag <- diagnose_candidate_outputs(ga_runs, vars = c("Biomass", "Catch"))

ens_summary <- plot_ga_ensemble(
  obs, ga_runs,
  init_run_dir          = base_dir,
  vars                  = c("Biomass", "Catch", "F"),
  species_patterns      = c("gag", "red grouper"),
  keep_fitness_quantile = 0.9,
  plots_dir             = file.path(BASE_DIR, "plots/ts")
)
print(ens_summary)

# faceted (all stanzas of one species per PNG) -- AIC-weighted ensemble, best 10% of
# candidates by fitness (top_n_prop = 0.10; ~91 of 910 candidates), weighted by AIC
# within that pool. target_ess = 50 is safely below the ~91-candidate pool size (ESS
# must be < pool size or the weights fall back to near-uniform -- see
# select_ensemble_candidates()'s own warning) and lower than the pool size gives the
# weights actual differentiating power, rather than nearly-uniform weighting across
# the whole pool. plot_ga_ensemble_faceted() also draws the AIC-weighted mean time
# series (per year, across this ensemble, weighted by AIC) as a dark red dashed line
# in every panel, alongside the individual candidate lines (colored by fitness), the
# best-fit line (turquoise), and the init line (black dashed).
facet_aic <- plot_ga_ensemble_faceted(
  obs, ga_runs, init_run_dir = base_dir, vars = c("Biomass", "Catch", "F"),
  species_patterns = c("gag", "red grouper"),
  ens_mode = "aic", top_n_prop = 0.10, target_ess = 50,
  plots_dir = file.path(BASE_DIR, "plots/ts")
)
print(facet_aic)

# region map + per-region age0 gag time series (bottom-right panel = whole-domain "WFS").
# obs is required -- only regions with a matched observed series are plotted.
region_attrs <- read.csv(file.path(BASE_DIR, "combined_regions_5min_attributes.csv"),
                         stringsAsFactors = FALSE)
age0_regions <- read_esri_ascii(file.path(BASE_DIR, "age0_survey_regions_5min_mod.asc"))
land_mask <- read_esri_ascii(file.path(BASE_DIR, "combined_regions_5min.asc"))

region_ts <- plot_region_timeseries_grid(
  ga_runs               = ga_runs,
  species_pattern       = "gag 0",
  init_run_dir          = file.path(BASE_DIR, "sp03_5min_phase3_init"),
  region_mask           = age0_regions,
  region_attributes     = region_attrs,
  obs                   = obs,
  domain_obs_pattern    = "FWCNMFS",
  map_show_land         = TRUE,
  show_ensemble         = TRUE,
  top_n_prop            = 0.10,
  target_ess            = 50,
  ensemble_max_folders  = 910,
  plots_dir             = file.path(BASE_DIR, "plots/ts")
)
message("run_dir: ", attr(region_ts, "run_dir"), " | group_token: ", attr(region_ts, "group_token"),
        " | ", length(unique(region_ts$region_id)), " of ", attr(region_ts, "n_regions_total"),
        " region(s) had matched observed data.")

# best archived candidate per origin-generation label. NOTE: all 910 folders are the
# gen-30 population; the "g<NN>" tag is when that individual was FIRST discovered
# (elites persist across generations), so this compares discovery-origin WITHIN the
# final population, not fit-improving-over-generations.
gen_summary <- plot_ga_generation_best(
  obs, ga_runs,
  vars             = c("Biomass", "Catch", "F"),
  species_patterns = c("gag", "red grouper"),
  palette          = "viridis",
  plots_dir        = file.path(BASE_DIR, "plots/ts")
)
print(gen_summary)


## ---- PART 5: spatial maps + red-tide M0 + environmental response functions -------------

map_root_dir <- DATA_DIR
all_folders  <- list.dirs(map_root_dir, recursive = FALSE, full.names = TRUE)
list.files(all_folders[1], pattern = "EcospaceMap|M0_loss_rate")  # sanity check one folder

# time a small subset before committing to reading all ~910 folders
test_tokens <- detect_map_group_tokens(all_folders[1:min(20, length(all_folders))],
                                       "Biomass", c("gag", "red grouper"))
t0 <- Sys.time()
test_stats <- compute_ensemble_map_stats(all_folders[1:min(20, length(all_folders))],
                                         var = "Biomass", tok = test_tokens$token[1],
                                         progress_every = 0)
per_folder_sec <- as.numeric(Sys.time() - t0, units = "secs") / min(20, length(all_folders))
n_stanzas <- 12
n_cores_available <- if(.Platform$OS.type == "unix" && requireNamespace("parallel", quietly = TRUE))
  max(1, parallel::detectCores() - 5) else 1
message(sprintf("~%.2f sec/folder for one (variable, stanza). Biomass+Catch, %d cores: ~%.1f min",
                per_folder_sec, n_cores_available,
                per_folder_sec * n_stanzas * 2 * length(all_folders) / 60 / n_cores_available))

# explicit dashed-gridline breaks matching this model's domain extent (same values
# used in the example scripts this layout is based on) -- reused for every map/
# residuals figure below so grid lines line up consistently across all of them
lon_breaks <- seq(-86, -82, by = 2)
lat_breaks <- seq(26, 30, by = 2)

ens_map_summary_aic <- plot_ecospace_ensemble_maps_paired(
  map_root_dir     = map_root_dir,
  ga_runs          = ga_runs,
  run_folders      = all_folders,
  ens_mode         = "aic",
  top_n_prop       = 0.10,
  target_ess       = 50,
  species_patterns = c("gag", "red grouper"),
  vars             = c("Biomass", "Catch", "F"),
  variability      = NULL,
  log_scale        = TRUE,
  max_folders      = 910,
  n_cores          = n_cores_available,
  land_mask        = land_mask,
  lon_breaks       = lon_breaks,
  lat_breaks       = lat_breaks,
  stanzas_per_row  = 3,
  barheight        = 2.8,
  barwidth         = 0.45,
  plots_dir        = file.path(BASE_DIR, "plots/spatial")
)

print(ens_map_summary_aic)


ens_map_paired_summary_aic <- plot_ecospace_ensemble_maps_paired(
  map_root_dir     = map_root_dir,
  ga_runs          = ga_runs,
  run_folders      = all_folders,
  ens_mode         = "aic",
  top_n_prop       = 0.10,
  target_ess       = 50,
  species_patterns = c("gag", "red grouper"),
  vars             = c("Biomass", "Catch", "F"),
  variability      = "cv",
  log_scale        = TRUE,
  max_folders      = 910,
  n_cores          = n_cores_available,
  land_mask        = land_mask,
  lon_breaks       = lon_breaks,
  lat_breaks       = lat_breaks,
  stanzas_per_row  = 2,
  barheight        = 2.8,
  barwidth         = 0.45,
  plots_dir        = file.path(BASE_DIR, "plots/spatial")
)

print(ens_map_paired_summary_aic)


# red tide M0 mortality, AIC-weighted ensemble only
m0_groups <- c(paste0("gag ", c(0:4, "5+")), paste0("red grouper ", c(0:4, "5+")),
               "gag total", "red grouper total")
ens_m0_aic <- plot_ecospace_ensemble_m0(
  map_root_dir = map_root_dir, ga_runs = ga_runs, ens_mode = "aic",
  top_n_prop = 0.10, target_ess = 50,
  run_folders = all_folders, species_patterns = c("gag", "red grouper"),
  groups = m0_groups, max_folders = 910, plots_dir = file.path(BASE_DIR, "plots/ts")
)
print(ens_m0_aic)

# single combined plot: every age (0-5+, "total" excluded) as its own colored line,
# all on the SAME shared y-axis scale, so magnitudes across ages are directly
# comparable -- rather than the grid above, where each panel gets its own
# independent y-axis. ens_mode="aic" + single_plot_uncertainty=TRUE shows the
# AIC-weighted 5th/95th percentile band (via weighted_quantile()) reflecting how
# much the actual selected ensemble agrees, rather than a plain SD-based spread.
ens_m0_single <- plot_ecospace_ensemble_m0(
  map_root_dir = map_root_dir, ga_runs = ga_runs, ens_mode = "aic",
  top_n_prop = 0.10, target_ess = 50,
  run_folders = all_folders, species_patterns = c("gag", "red grouper"),
  groups = m0_groups, max_folders = 910, single_plot = TRUE,
  single_plot_uncertainty = TRUE,
  plots_dir = file.path(BASE_DIR, "plots/ts")
)
print(ens_m0_single)

# spatial residuals: observed FG survey raster vs. predicted mean biomass
obs_raster_gag1 <- read_esri_ascii(file.path(BASE_DIR, "maxn/GFISHER_maxn_mod21_gag-1_5min_66x78.asc"))
tok_gag1 <- detect_map_group_tokens(all_folders, "Biomass", species_patterns = "gag 1")
token_gag1 <- tok_gag1$token[tolower(tok_gag1$display) == "gag 1"][1]

pred_stats_gag1 <- compute_ensemble_map_stats(all_folders, "Biomass", token_gag1)
if(is.null(pred_stats_gag1$agg))
  stop("No folder had EcospaceMapBiomass-", token_gag1, ".csv archived for any candidate.")
pred_mean_gag1  <- as.data.frame(pred_stats_gag1$agg[, list(row, col, lat, lon, value = mean_val)])
resid_gag1 <- compute_spatial_residuals(obs_raster_gag1, pred_mean_gag1)
plot_spatial_residuals(resid_gag1, title = "gag 1",
                       land_mask = land_mask, lon_breaks = lon_breaks, lat_breaks = lat_breaks)

# batch: every gag/red grouper observed raster in maxn/, combined into one faceted
# figure per species. AIC-weighted ensemble only -- the predicted mean map is now a
# true AIC-weighted average (each candidate's own per-cell mean weighted by its AIC
# weight), not an unweighted mean over the AIC-selected subset.
resid_faceted_aic <- plot_spatial_residuals_batch(
  raster_dir       = file.path(BASE_DIR, "maxn"),
  ga_runs          = ga_runs,
  ens_mode         = "aic",
  top_n_prop       = 0.10,
  target_ess       = 50,
  species_patterns = c("gag", "red grouper"),
  land_mask        = land_mask,
  lon_breaks       = lon_breaks,
  lat_breaks       = lat_breaks,
  barheight        = 2.8,
  barwidth         = 0.45,
  facet_ncol       = 3,
  plots_dir        = file.path(BASE_DIR, "plots/residuals")
)
print(resid_faceted_aic)

# environmental response functions.
#
# Uses the sensitivity analysis xlsx as the catalog source rather than the CSV --
# this file has an EXPLICIT group_index column (fxn_num) for every response function,
# so it isn't dependent on the CSV's row-position-must-match-group_index assumption
# (which silently breaks if that file is ever missing a row or reordered). gag and red
# grouper are always plotted as SEPARATE figures.
#
# NOTE on red tide coverage: gag/red grouper genuinely only have 2 red tide functions
# per species per pathway (mortality, foraging) -- one specific to age 0, and ONE
# SHARED function covering ages 1 through 5+ collectively (named "gag 1+"/
# "red grouper 1+" in the source data). This is real model structure, confirmed
# directly against sensitivity_sp03_5min_phase3_init_2026-07-23.xlsx -- there is no
# separate gag 1/gag 2/.../gag 5+ red tide function to plot, since the model itself
# doesn't define one. Depth, by contrast, DOES have all 6 stanzas separately per
# species; temp has just 1 function shared across all ages per species.
catalog <- read_sensitivity_response_catalog(
  file.path(BASE_DIR, "sensitivity_sp03_5min_phase3_init_2026-07-23.xlsx"))
driver_map <- build_driver_map_from_sensitivity(ga_runs, catalog)
print(driver_map)

curve_by_depth <- plot_ga_response_by_driver_map(
  ga_runs, driver_map, driver_name = "depth",
  species_patterns = c("gag", "red grouper"), keep_fitness_quantile = 0.9,
  max_folders = 910, catalog = catalog,
  plots_dir = file.path(BASE_DIR, "plots/response")
)

# temperature: sparse coverage (only sharks, "red grouper", demersal fish have one; gag
# has none, expect it skipped with a message, not an error)
curve_by_temp <- plot_ga_response_by_driver_map(
  ga_runs, driver_map, driver_name = "temp",
  species_patterns = c("gag", "red grouper"), keep_fitness_quantile = 0.9,
  max_folders = 910, catalog = catalog,
  plots_dir = file.path(BASE_DIR, "plots/response")
)

# red tide mortality functions for ALL gag and red grouper groups -- including ones
# with FIXED (non-GA-tuned) parameters that never appear active in any candidate's
# cmd.txt and were previously silently dropped. driver_map now includes every catalog
# row (see build_driver_map_from_catalog()'s is_fixed column), and passing catalog=
# here lets fixed entries be plotted from their own static parameters instead.
curve_by_redtide <- plot_ga_response_by_driver_map(
  ga_runs, driver_map, driver_name = "red tide",
  species_patterns = c("gag", "red grouper"), keep_fitness_quantile = 0.9,
  max_folders = 910, catalog = catalog,
  plots_dir = file.path(BASE_DIR, "plots/response")
)

# RRMSE per predicted series vs. fitness -- does lower RRMSE mean lower NLL? (real but
# noisy relationship; prints Spearman correlation per facet)
rrmse_df <- compute_rrmse_by_candidate(
  obs, ga_runs,
  vars             = c("Biomass", "Catch", "F"),
  species_patterns = c("gag", "red grouper"),
  max_candidates   = 1000
)
rrmse_cor <- plot_rrmse_vs_fitness(
  rrmse_df, facet_by = "series",
  plots_dir = file.path(BASE_DIR, "plots/ts")
)
print(rrmse_cor)

#residual older
resid_faceted_older_aic <- plot_spatial_residuals_older_stanzas(
  raster_dir       = file.path(BASE_DIR, "maxn"),
  ga_runs          = ga_runs,
  ens_mode         = "aic",
  top_n_prop       = 0.10,
  target_ess       = 50,
  land_mask        = land_mask,
  lon_breaks       = lon_breaks,
  lat_breaks       = lat_breaks,
  barheight        = 2.8,
  barwidth         = 0.45,
  facet_ncol       = 3,
  plots_dir        = file.path(BASE_DIR, "plots/residuals")
)
print(resid_faceted_older_aic)
