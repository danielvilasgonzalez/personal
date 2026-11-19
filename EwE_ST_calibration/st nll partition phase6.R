#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# st nll partition phase6.R -- NLL of the baseline and the best model split into temporal and spatial,
# by functional group and by time series, and the spatial part by map.
# Runs on the Mac (no Windows needed): re-scores the saved Ecospace outputs with R4EwE's own functions.
# Writes four csv files to <RUN>/plots_R/:
#   nll_partition_temporal_vs_spatial.csv   totals (temporal, spatial, % of total)
#   nll_temporal_by_group.csv               temporal NLL per group (LL.<group>) for baseline and best
#   nll_temporal_by_series.csv              temporal NLL per time series, largest first
#   nll_spatial_by_map.csv                  spatial NLL per map (already x spatial.weight)
# NOTE: the GA keeps only the gag and red grouper maps in the candidate folders (compact output), so the
# spatial NLL of the best model can be split by map only for those; the other maps are in the baseline.
# The total spatial NLL of the best model = GA fitness minus its temporal NLL (also written).
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
RUN      <- "/Users/daniel/Work/ST_calibration/phase6_ga01"
LABEL    <- "ga01"
REPO_DIR <- "/Users/daniel/Documents/GitHub/personal/EwE_ST_calibration"
R4_DIR   <- "/Users/daniel/Documents/GitHub/R4EwE"                 # R4EwE source (or install the package)
SDM_DIR  <- "/Users/daniel/Documents/GitHub/GFISHER/output/maps/GFISHER/5min/maxn_sdmTMB/mice"   # sdmTMB maps used by the GA
SPATIAL_WEIGHT <- 15

options(warn = 1)
if (requireNamespace("R4EwE", quietly = TRUE)) { suppressMessages(library(R4EwE)) } else {
  system.file <- function(..., package = "") if (package == "R4EwE") file.path(R4_DIR, "inst", ...) else base::system.file(..., package = package)
  for (f in c("globals.R", "utils.R", "input_output.R", "Ecospace_objective_functions.R", "Spatial_likelihood.R", "fn.ecospace_gif.R"))    suppressMessages(source(file.path(R4_DIR, "R", f)))
}
inp <- file.path(RUN, "inputs")
vuls <- read.csv(file.path(inp, "WFS-MICE EwE 2.13 v67020-Vulnerabilities.csv"), row.names = 1)
group.names <- gsub(" ", "_", vuls[, 1])
fleet.names <- trimws(gsub("(t/km^2/year)", "", as.character(read.csv(file.path(inp, "WFS-MICE EwE 2.13 v67020-Catch.csv"),
                       header = FALSE, nrows = 1)[, -c(1:2)]), fixed = TRUE)); fleet.names <- fleet.names[seq_len(length(fleet.names) - 1)]
obs.ts <- fn.read_ecosim_timeseries(file.path(inp, "ts_mice_v5_discards_ecospace_regions_sedar105.csv"))
ri <- read.csv(file.path(RUN, "run_info.csv")); g <- function(k) ri$value[ri$field == k]
rel <- as.numeric(strsplit(g("setting.relative.types"), " ")[[1]])
obs.ts$ts.head$Absolute[obs.ts$ts.head$Type %in% rel] <- FALSE
sg <- as.numeric(strsplit(g("setting.stanza.groups"), " ")[[1]]); ld <- as.numeric(strsplit(g("setting.lead.stanza"), " ")[[1]])
obs.ts$ts.head$Absolute[obs.ts$ts.head$Type == 104 & obs.ts$ts.head$Poolcode %in% setdiff(sg, ld)] <- FALSE
region.areas <- fn.read_region_areas(file.path(REPO_DIR, "combined_regions_5min_attributes.csv"))

maps <- strsplit(g("setting.spatial.maps"), " ")[[1]]
so <- fn.load_gfisher_sdmTMB(sdm_dir = SDM_DIR, maps = maps, styear = as.numeric(g("setting.spatial.styear")),
                             endyear = as.numeric(g("setting.spatial.endyear")), cv.weight = identical(g("setting.spatial.cv.weight"), "TRUE"))

runs <- read.csv(file.path(RUN, "results", paste0("final_ga_runs_", LABEL, ".csv")))
bi <- which.min(runs$fitness); fit_best <- runs$fitness[bi]
bdir <- list.files(file.path(RUN, "output/gen30_small"), pattern = sub("^(run_g[0-9]+_i[0-9]+).*", "\\1", basename(dirname(gsub("\\\\", "/", runs$cmd_file[bi])))), full.names = TRUE)[1]
dirs <- c(baseline = file.path(RUN, "output/run_base"), best = bdir)
message("best candidate folder: ", bdir, " (fitness ", fit_best, ")")

tmp <- lapply(dirs, function(d) fn.ecospace_objfxn(dir.pred = d, obs.ts = obs.ts, group.names = group.names, fleet.names = fleet.names,
                fit.abs.catch = TRUE, spatial.obs = NULL, spatial.weight = SPATIAL_WEIGHT, region.areas = region.areas, return = "both"))
sp  <- lapply(dirs, function(d) suppressWarnings(fn.spatial_LL(dir.pred = d, spatial.obs = so, spatial.weight = SPATIAL_WEIGHT,
                group.names = group.names, model_styear = 1985)))

out <- file.path(RUN, "plots_R"); dir.create(out, showWarnings = FALSE, recursive = TRUE)
temporal <- sapply(tmp, function(r) unname(r$LL["LL.total"]))
spatial  <- c(baseline = sp$baseline$total, best = fit_best - temporal[["best"]])   # best: only some maps are saved
tot <- temporal + spatial
write.csv(data.frame(model = names(dirs), temporal_NLL = round(temporal, 1), spatial_NLL = round(spatial, 1), total_NLL = round(tot, 1),
                     pct_temporal = round(100 * temporal / tot, 1), pct_spatial = round(100 * spatial / tot, 1)),
          file.path(out, "nll_partition_temporal_vs_spatial.csv"), row.names = FALSE)

grp <- do.call(cbind, lapply(tmp, function(r) round(r$LL[names(r$LL) != "LL.total"], 2)))
write.csv(data.frame(group = sub("^LL\\.", "", rownames(grp)), grp, pct_of_temporal_best = round(100 * grp[, "best"] / temporal[["best"]], 1)),
          file.path(out, "nll_temporal_by_group.csv"), row.names = FALSE)
ser <- do.call(rbind, lapply(names(tmp), function(n) { d <- tmp[[n]]$detail; d <- d[is.finite(d$loglik), c("Title", "Poolcode", "Type.name", "Region", "n", "loglik")]
                                                        d$model <- n; d }))
ser <- reshape(ser, idvar = c("Title", "Poolcode", "Type.name", "Region", "n"), timevar = "model", direction = "wide")
ser <- ser[order(-ser$loglik.best), ]; ser$pct_of_temporal_best <- round(100 * ser$loglik.best / temporal[["best"]], 1)
write.csv(ser, file.path(out, "nll_temporal_by_series.csv"), row.names = FALSE)
mp <- data.frame(map = names(sp$baseline$per_species), baseline = round(sp$baseline$per_species, 2), best = round(sp$best$per_species, 2))
write.csv(mp, file.path(out, "nll_spatial_by_map.csv"), row.names = FALSE)
print(read.csv(file.path(out, "nll_partition_temporal_vs_spatial.csv"))); print(mp)
