#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# st results phase6.R -- numbers behind the Results and Discussion of phase6 / ga01 (one script, runs on the Mac).
# Nothing here starts an Ecospace run. Sections that re-score saved Ecospace outputs use R4EwE (a few minutes).
# Writes csv tables only; the figures are made by 'st calibration run phase6.R' and 'fig contribution and parameters phase6.R'.
#
#  1  NLL partition: baseline and best model split into temporal / spatial, by group, by series and by map
#       -> plots_R/nll_partition_temporal_vs_spatial.csv, nll_temporal_by_group.csv, nll_temporal_by_series.csv, nll_spatial_by_map.csv
#          (the temporal series table is read by section 2 and by the contribution figure)
#  2  checks for the Discussion, base R only (seconds) -> plots_R/discussion_checks/
#       2.1 bounds of the calibrated values, with the name of each curve
#       2.2 ensemble spread and values at a bound
#       2.3 groups in the temporal objective
#       2.4 best parameter set each time the best changed
#       2.5 source of the dispersal baseline and lineage of the members
#  3  re-scoring of the ensemble members (set RUN_RESCORE <- FALSE to skip)
#       3.1 members: NLL by series, do they miss the same features?  (06_member_series_*.csv, used by the contribution figure)
#       3.2 optional: all 370 candidates of the final population (SCORE_FINAL_POP)
#       3.3 entropy of the observed maps and divergence
#       3.4 residual autocorrelation (Moran's I) and distribution metrics
#       3.5 RRMSE of the 12 stanza biomass series (baseline, best, members) -> plots_R/ts/Reference_stanza_biomass_rrmse.csv
#  4  run-to-run noise from parameter sets that the GA evaluated twice (no new runs)
#  5  replicates of the finalists vs the stop tolerance (needs sens1/finalist_replicates.csv from 'ga post-run checks WFS MICE.R')
# The GA keeps only the gag and red grouper maps in the candidate folders (compact output), so the spatial NLL of the best
# model can be split by map only for those; its total spatial NLL = GA fitness minus its temporal NLL.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#USER INPUT------------------------------------------------------------------------------------
RUN      <- Sys.getenv("EWE_RUN",  "/Users/daniel/Work/ST_calibration/phase6_ga01")
LABEL    <- "ga01"
REPO_DIR <- Sys.getenv("EWE_REPO", "/Users/daniel/Documents/GitHub/personal/EwE_ST_calibration")
R4_DIR   <- Sys.getenv("EWE_R4",   "/Users/daniel/Documents/GitHub/R4EwE")                 # R4EwE source (or install the package)
SDM_DIR  <- Sys.getenv("EWE_SDM",  "/Users/daniel/Documents/GitHub/GFISHER/output/maps/GFISHER/5min/maxn_sdmTMB/mice")   # sdmTMB maps used by the GA
SPATIAL_WEIGHT <- 15
N_ENS    <- 296                                      # ensemble = the models with NLL <= the ENS_PROP quantile of the final population (80% of 370 = 296)
ENS_PROP <- 0.80                                     # ensemble of the predictions = models with NLL <= this quantile of the final population (296 of 370)
tol      <- 32.2973                                  # stop tolerance, ga_stop_ga01.csv
RUN_RESCORE <- TRUE                                  # sections 3.x re-score saved outputs; FALSE keeps only the base R sections
PRED_ONLY <- if(exists("PRED_ONLY")) PRED_ONLY else FALSE         # TRUE: skip 3.1 to 3.4 (NLL re-scoring and spatial checks) and run only 3.5 to 3.7 (rRMSE and the ensemble cut-off; temporal predictions only)
run_dir <- RUN; tag <- LABEL; res_dir <- file.path(RUN, "results")
out_dir <- file.path(RUN, "plots_R", "discussion_checks"); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

#SET-UP (R4EwE, observed series, maps)---------------------------------------------------------
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

# ---- 1. NLL PARTITION ----------------------------------------------------------------------------
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

# ---- 2. CHECKS FOR THE DISCUSSION (base R) ----------------------------------------------------------
# ---- load ---------------------------------------------------------------------------------
pv   <- read.csv(file.path(res_dir, paste0("parvec_", tag, ".csv")), row.names = 1, check.names = FALSE)
pop  <- read.csv(file.path(res_dir, paste0("final_ga_pop", tag, ".csv")), row.names = 1, check.names = FALSE)
runs <- read.csv(file.path(res_dir, paste0("final_ga_runs_", tag, ".csv")))
cal  <- read.csv(file.path(run_dir, "plots_R", "ts", "parameters_calibrated_vs_not.csv"))
ts   <- read.csv(file.path(run_dir, "plots_R", "nll_temporal_by_series.csv"), check.names = FALSE)
envp <- read.csv(file.path(run_dir, "inputs", "WFS-MICE env and red tide response shape pars.csv"), check.names = FALSE)
disp <- read.csv(file.path(run_dir, "inputs", "WFS-MICE EwE 2.13 v67020-Dispersal.csv"), check.names = FALSE)
names(pv)[1:3] <- c("L", "base", "U")           # L.bounds, est_par_vec, U.bounds
runs <- runs[order(runs$fitness), ]
E    <- pop[runs$gapop_row[seq_len(N_ENS)], ]   # the ensemble: models at or below the ENS_PROP quantile of the final population

# ids of the 51 calibrated values (a curve id expands to its inflection and slope)
ids <- c()
for (i in cal$id[cal$calibrated %in% TRUE]) {
  if (grepl("^curve_", i)) { n <- sub("curve_", "", i)
    ids <- c(ids, grep(paste0("^(rt|env)", n, "_"), rownames(pv), value = TRUE))
  } else ids <- c(ids, i)
}
curve_name <- function(id) {                     # name of the response function behind a curve id
  n <- suppressWarnings(as.integer(sub("^(rt|env)([0-9]+)_.*$", "\\2", id)))
  ifelse(is.na(n), NA, envp[["Function name"]][match(n, envp$fxn_num)])
}

# ---- 2.1 bounds of the calibrated values ----------------------------------------------------
# Resolves the exact wording of the bounds sentence: ratios of the bounds to the baseline, by value.
b <- data.frame(id = ids, what = curve_name(ids), L = pv[ids, "L"], base = pv[ids, "base"], U = pv[ids, "U"])
b$L_x <- round(b$L / b$base, 2); b$U_x <- round(b$U / b$base, 2)
b$kind <- sub("^([a-z]+)[0-9_].*$", "\\1", b$id)
b$part <- ifelse(grepl("inflection", b$id), "inflection", ifelse(grepl("slope", b$id), "slope", ""))
print(aggregate(cbind(L_x, U_x) ~ kind + part, b, function(v) paste(unique(range(v)), collapse = " to ")))
cat("\nCalibrated vulnerabilities: lower bound", paste(unique(b$L[b$kind == "vul"]), collapse = ", "),
    "| upper bound as a multiple of the baseline:", paste(range(b$U_x[b$kind == "vul"]), collapse = " to "), "\n")
cat("Dispersal (km/yr): baseline, lower, upper\n"); print(pv[grepl("^disp", rownames(pv)), c("base", "L", "U")])
write.csv(b, file.path(out_dir, "01_bounds_of_calibrated_values.csv"), row.names = FALSE)

# ---- 2.2 ensemble spread and values at a bound ------------------------------------------------
cat("\nEnsemble members:", nrow(E), "| elites among them:", sum(runs$role[seq_len(nrow(E))] == "elite"),
    "| distinct parameter vectors:", nrow(unique(E)),
    "| NLL span:", round(diff(range(runs$fitness[seq_len(nrow(E))])), 1), "\n")
pos <- function(i) {                             # position inside the bounds; log scale for vulnerabilities
  x <- E[[i]]; L <- pv[i, "L"]; U <- pv[i, "U"]
  if (grepl("^vul", i)) (log(x) - log(L)) / (log(U) - log(L)) else (x - L) / (U - L)
}
P  <- sapply(ids, pos)
rf <- apply(P, 2, function(v) diff(range(v)))
hit <- data.frame(id = ids, what = curve_name(ids), span = round(rf, 2),
                  at_lower = round(colMeans(P <= 0.05), 2), at_upper = round(colMeans(P >= 0.95), 2),
                  ens_min = sapply(ids, function(i) min(E[[i]])), ens_median = sapply(ids, function(i) median(E[[i]])),
                  ens_max = sapply(ids, function(i) max(E[[i]])))
cat("Of", length(ids), "calibrated values: span > half of range:", sum(rf > 0.5),
    "| span < 1/5:", sum(rf < 0.2), "| at a bound in >= half of members:",
    sum(hit$at_lower >= 0.5 | hit$at_upper >= 0.5), "\n")
print(hit[hit$at_lower >= 0.5 | hit$at_upper >= 0.5, ])
write.csv(hit, file.path(out_dir, "02_ensemble_spread_and_bounds.csv"), row.names = FALSE)

# ---- 2.3 which groups entered the temporal objective -------------------------------------------
ts$group <- sub("_.*$", "", ts$Title)
print(table(ts$group, ts[["Type.name"]]))        # GAG, REDGRP, and 3 relative-catch series of other groups

# ---- 2.4 best parameter set each time the best NLL changed -----------------------------------------
ln  <- readLines(file.path(res_dir, paste0("ga_results_", tag, ".csv")), warn = FALSE)
gar <- read.csv(text = ln[grep('^"?gen"?,', ln)[1]:length(ln)], check.names = FALSE)
changed <- c(TRUE, diff(gar$min_LL[gar$gen >= 0]) < 0)
bestgen <- gar[gar$gen >= 0, ][changed, ]
write.csv(bestgen, file.path(out_dir, "04_best_parameters_when_best_changed.csv"), row.names = FALSE)
vcols <- grep("^vul", names(bestgen), value = TRUE); late <- bestgen[bestgen$gen >= 10, ]
ratio <- apply(late[, vcols], 2, function(v) max(v) / min(v))
cat("\nGenerations where the best NLL changed:", paste(bestgen$gen, collapse = ", "),
    "\nFrom generation 10 on,", nrow(late), "successive best candidates (NLL", round(max(late$min_LL)), "to",
    round(min(late$min_LL)), "): of", length(vcols), "vulnerabilities,", sum(ratio > 10), "differ > 10-fold and",
    sum(ratio > 100), "> 100-fold between candidates.\n")

# ---- 2.5 dispersal baseline and lineage of the members ------------------------------------------------
cat("\nDispersal baseline in the input file (", "WFS-MICE ... -Dispersal.csv", "):\n", sep = "")
print(disp[, c(2, 3)][disp[[3]] %in% unique(pv[grepl("^disp", rownames(pv)), "base"]) |
                       grepl("^(gag|red grouper)", disp[[2]]), ])
cat("Baseline of the calibrated dispersal rates:", unique(pv[ids[grepl("^disp", ids)], "base"]), "km/yr.",
    "The same value appears for gag and red grouper ages 2 to 5+; state where it comes from.\n")
gen_of <- as.integer(sub(".*run_g([0-9]+)_.*", "\\1", gsub("\\\\", "/", runs$cmd_file)))
cat("\nGeneration of origin of the", N_ENS, "ensemble members (elites carry the generation where they first appeared):\n")
print(table(role = runs$role[seq_len(N_ENS)], generation = gen_of[seq_len(N_ENS)]))

# ---- 3. RE-SCORING OF SAVED OUTPUTS ------------------------------------------------------------------
if (RUN_RESCORE) {
  r4 <- function(nm) if (requireNamespace("R4EwE", quietly = TRUE)) get(nm, envir = asNamespace("R4EwE")) else get(nm)

  member_dir <- function(cmd) {                  # folder of a candidate inside output/gen30_small
    key <- sub("^(run_g[0-9]+_i[0-9]+).*", "\\1", basename(dirname(gsub("\\\\", "/", cmd))))
    list.files(file.path(run_dir, "output/gen30_small"), pattern = paste0("^", key), full.names = TRUE)[1]
  }
  skey <- function(d) paste(d$Title, d$Poolcode, d$Type.name, d$Region, d$n, sep = "|")
  score_dir <- function(d) {                     # temporal NLL by series and spatial NLL by map for one output folder
    r  <- fn.ecospace_objfxn(dir.pred = d, obs.ts = obs.ts, group.names = group.names, fleet.names = fleet.names,
                             fit.abs.catch = TRUE, spatial.obs = NULL, spatial.weight = SPATIAL_WEIGHT,
                             region.areas = region.areas, return = "both")
    det <- r$detail[is.finite(r$detail$loglik), ]
    sp  <- suppressWarnings(fn.spatial_LL(dir.pred = d, spatial.obs = so, spatial.weight = SPATIAL_WEIGHT,
                                          group.names = group.names, model_styear = 1985))
    list(temporal = unname(r$LL["LL.total"]), series = setNames(det$loglik, skey(det)), maps = sp$per_species)
  }

  if(!isTRUE(PRED_ONLY)) {   # 3.1 to 3.4 re-score every model with the objective (includes the spatial maps; slow)
  # ---- 3.1 re-score the ensemble members: do they miss the same series? ------------------------------
  cat("\n[3.1] Re-scoring", N_ENS, "members from output/gen30_small ...\n")
  # all 370 candidates of the final population are scored (the ensemble is the best N_ENS of them); the population files
  # (06_population_*.csv) feed the grey points of the contribution figure. Set SCORE_POPULATION <- FALSE to score only the ensemble.
  SCORE_POPULATION <- TRUE
  n_sc <- if(SCORE_POPULATION) nrow(runs) else N_ENS
  dirs <- vapply(runs$cmd_file[seq_len(n_sc)], member_dir, character(1)); stopifnot(!anyNA(dirs))
  sc_all <- lapply(dirs, function(d) tryCatch(score_dir(d), error = function(e) NULL))
  ok_sc <- !vapply(sc_all, is.null, logical(1)); cat("scored:", sum(ok_sc), "of", n_sc, "\n")
  keys <- names(sc_all[[which(ok_sc)[1]]]$series)
  if(SCORE_POPULATION){
    Sp <- t(sapply(sc_all, function(x) if(is.null(x)) rep(NA_real_, length(keys)) else x$series[keys]))
    Mpp <- t(sapply(sc_all, function(x) if(is.null(x)) rep(NA_real_, length(sc_all[[which(ok_sc)[1]]]$maps)) else x$maps)); colnames(Mpp) <- names(sc_all[[which(ok_sc)[1]]]$maps)
    write.csv(data.frame(rank = seq_len(n_sc), fitness = runs$fitness[seq_len(n_sc)], Sp, check.names = FALSE), file.path(out_dir, "06_population_series_matrix.csv"), row.names = FALSE)
    write.csv(data.frame(rank = seq_len(n_sc), fitness = runs$fitness[seq_len(n_sc)], round(Mpp, 2), check.names = FALSE), file.path(out_dir, "06_population_maps.csv"), row.names = FALSE)
  }
  sc <- sc_all[seq_len(N_ENS)]; stopifnot(!anyNA(vapply(sc, is.null, logical(1))))
  S    <- t(sapply(sc, function(x) x$series[keys]))               # members x series
  Tm   <- sapply(sc, function(x) x$temporal)
  write.csv(data.frame(member = seq_len(N_ENS), S, check.names = FALSE), file.path(out_dir, "06_member_series_matrix.csv"), row.names = FALSE)   # NLL of every series for every member (used for the stanza biomass ranges in Fig. X)
  Mp   <- t(sapply(sc, function(x) x$maps))                       # members x maps (NA where the map was not saved)
  fit  <- runs$fitness[seq_len(N_ENS)]
  spat <- fit - Tm                                                # total spatial NLL = GA fitness - temporal NLL
  cat("Temporal NLL of members: ", round(min(Tm)), "to", round(max(Tm)), "| spatial NLL (fitness - temporal):",
      round(min(spat)), "to", round(max(spat)), "\n")
  cat("NLL span of the members:", round(diff(range(fit)), 1), "of which temporal", round(diff(range(Tm)), 1),
      "and spatial", round(diff(range(spat)), 1), "\n")
  base_ser <- setNames(ts$loglik.baseline, skey(ts))
  tab <- data.frame(series = keys, baseline = base_ser[keys], member_mean = colMeans(S), member_sd = apply(S, 2, sd),
                    member_min = apply(S, 2, min), member_max = apply(S, 2, max))
  tab$cv_across_members <- round(tab$member_sd / tab$member_mean, 3)
  tab$share_members_improved <- round(colMeans(sweep(S, 2, tab$baseline, "<")), 2)
  tab$share_of_mean_temporal <- round(100 * tab$member_mean / mean(Tm), 1)
  tab$share_of_member_spread <- round(100 * apply(S, 2, function(v) cov(v, Tm)) / var(Tm), 1)   # who separates members
  tab <- tab[order(-tab$member_mean), ]
  cat("\nLargest series in the ensemble (share of temporal NLL, spread across members, share of the members' spread):\n")
  print(head(tab[, c("series", "member_mean", "cv_across_members", "share_members_improved",
                     "share_of_mean_temporal", "share_of_member_spread")], 10), row.names = FALSE)
  rk <- apply(S, 1, function(v) cor(v, colMeans(S), method = "spearman"))
  cat("\nSpearman correlation of each member's series NLLs with the ensemble mean: min", round(min(rk), 3),
      "median", round(median(rk), 3), "\n(close to 1 for all members = they miss the same series)\n")
  top5 <- apply(S, 1, function(v) sum(sort(v, decreasing = TRUE)[1:5]) / sum(v))
  cat("Share of a member's temporal NLL held by its 5 largest series: ", round(100 * min(top5)), "to", round(100 * max(top5)), "%\n")
  write.csv(tab, file.path(out_dir, "06_member_series_nll.csv"), row.names = FALSE)
  write.csv(data.frame(member = seq_len(N_ENS), role = runs$role[seq_len(N_ENS)], generation = gen_of[seq_len(N_ENS)],
                       fitness = fit, temporal = Tm, spatial = spat, round(Mp, 2), check.names = FALSE),
            file.path(out_dir, "06_member_totals_and_maps.csv"), row.names = FALSE)

  # ---- 3.2 (optional) temporal and spatial NLL of ALL 370 candidates of the final population ---------------------------
  # Shows whether the spatial NLL varies among candidates while the temporal NLL does, which is the closest the saved
  # outputs get to the "temporal and spatial NLL over generations" question (only generations 25 to 30 were kept).
  # Takes much longer than section 6 (370 re-scorings), so it is off by default.
  SCORE_FINAL_POP <- FALSE
  if (SCORE_FINAL_POP) {
    dall <- vapply(runs$cmd_file, member_dir, character(1))
    Tall <- vapply(dall, function(d) tryCatch(unname(fn.ecospace_objfxn(dir.pred = d, obs.ts = obs.ts, group.names = group.names,
                      fleet.names = fleet.names, fit.abs.catch = TRUE, spatial.obs = NULL, spatial.weight = SPATIAL_WEIGHT,
                      region.areas = region.areas, return = "both")$LL["LL.total"]), error = function(e) NA_real_), numeric(1))
    allp <- data.frame(role = runs$role, generation = gen_of, fitness = runs$fitness, temporal = Tall, spatial = runs$fitness - Tall)
    write.csv(allp, file.path(out_dir, "06b_final_population_temporal_vs_spatial.csv"), row.names = FALSE)
    good <- allp[is.finite(allp$temporal) & allp$fitness < 8000, ]
    cat("\n[3.2] Candidates with NLL below 8,000:", nrow(good), "| SD of temporal NLL:", round(sd(good$temporal), 1),
        "| SD of spatial NLL:", round(sd(good$spatial), 1), "| correlation temporal vs spatial:", round(cor(good$temporal, good$spatial), 2), "\n")
    print(summary(allp[, c("temporal", "spatial")]))
  }

  # ---- 3.3 entropy of the observed maps and divergence ----------------------------------------------------
  map_vectors <- function(d, i) {                # same steps as the inner loop of fn.spatial_LL, for row i of 'so'
    row_pcs <- if ("pcs" %in% names(so)) so$pcs else as.list(so$pc)
    pm <- r4("fn.ecospace_spatial_pred")(d, unique(unlist(row_pcs[i])), group.names, styear = so$styear[i],
                                         endyear = so$endyear[i], model_styear = 1985)
    parts <- pm[as.character(row_pcs[[i]])]
    if (!length(parts) || any(vapply(parts, is.null, logical(1)))) return(NULL)
    pred_m <- r4(".sum_maps")(parts); obs_m <- so$map_norm[[i]]
    w_m <- if ("cell_w" %in% names(so)) so$cell_w[[i]] else NULL
    valid <- is.finite(obs_m) & is.finite(pred_m); if (!is.null(w_m)) valid <- valid & is.finite(w_m)
    w_v <- if (is.null(w_m)) 1 else w_m[valid]
    obs_v <- w_v * obs_m[valid]; obs_v <- obs_v / sum(obs_v)
    pred_v <- w_v * pred_m[valid]; pred_v <- pred_v / sum(pred_v)
    rc <- which(valid, arr.ind = TRUE)
    list(obs = obs_v, pred = pred_v, row = rc[, 1], col = rc[, 2])
  }
  Hent <- function(p) { p <- p[p > 0]; -sum(p * log(p)) }
  eps  <- .Machine$double.eps^0.5
  d_base <- file.path(run_dir, "output/run_base"); d_best <- dirs[[1]]
  rows <- list()
  for (i in seq_len(nrow(so))) for (mod in c("baseline", "best")) {
    mv <- tryCatch(map_vectors(if (mod == "baseline") d_base else d_best, i), error = function(e) NULL)
    if (is.null(mv)) next
    ce <- -sum(mv$obs * log(pmax(mv$pred, eps)))
    rows[[length(rows) + 1]] <- data.frame(map = so$species[i], model = mod, n_cells = length(mv$obs),
                                           cross_entropy = ce, entropy_obs = Hent(mv$obs), divergence = ce - Hent(mv$obs))
  }
  ent <- do.call(rbind, rows)
  wide <- merge(ent[ent$model == "baseline", -2], ent[ent$model == "best", -2], by = "map", all.x = TRUE,
                suffixes = c(".base", ".best"))
  wide$change_in_divergence <- wide$divergence.best - wide$divergence.base
  cat("\n[3.3] Cross-entropy, entropy of the observed map and divergence (nats, before the weight of", SPATIAL_WEIGHT, ")\n")
  print(wide, digits = 4, row.names = FALSE)
  chk <- ent[ent$model == "best", ]
  cat("Check against the GA: sum of weighted cross-entropy of the maps saved for the best member =",
      round(SPATIAL_WEIGHT * sum(chk$cross_entropy), 1), "\n")
  write.csv(ent, file.path(out_dir, "07_entropy_and_divergence_by_map.csv"), row.names = FALSE)

  # ---- 3.4 residual autocorrelation (Moran's I) and distribution metrics --------------------------------------
  moran_knn <- function(row, col, r, k = 8, nperm = 499) {        # Moran's I with k nearest neighbours, permutation p
    n <- length(r); z <- r - mean(r); xy <- cbind(col, row)
    nb <- matrix(0L, n, k)
    for (i in seq_len(n)) { dd <- (xy[, 1] - xy[i, 1])^2 + (xy[, 2] - xy[i, 2])^2; nb[i, ] <- order(dd)[2:(k + 1)] }
    I_of <- function(zz) sum(zz * rowMeans(matrix(zz[nb], n, k))) / sum(zz^2)
    I <- I_of(z); perm <- replicate(nperm, I_of(sample(z)))
    data.frame(n = n, moran_I = I, p_perm = (1 + sum(perm >= I)) / (nperm + 1),
               n_eff_approx = if (I > 0) n * (1 - I) / (1 + I) else n)
  }
  dist_metrics <- function(x, y, v) {            # centre of gravity, area holding 95% (in cells) and spread
    ok <- is.finite(v) & v >= 0; x <- x[ok]; y <- y[ok]; v <- v[ok]
    cs <- cumsum(sort(v, decreasing = TRUE)) / sum(v); cx <- sum(x * v) / sum(v); cy <- sum(y * v) / sum(v)
    data.frame(cog_col = cx, cog_row = cy, D95_cells = which(cs >= 0.95)[1], spread = sqrt(sum(v * ((x - cx)^2 + (y - cy)^2)) / sum(v)))
  }
  set.seed(1); mo <- list(); dm <- list()
  for (i in seq_len(nrow(so))) {
    mv <- tryCatch(map_vectors(d_best, i), error = function(e) NULL); if (is.null(mv)) next
    res <- length(mv$obs) * (mv$pred - mv$obs)                     # relative density difference, predicted minus observed
    mo[[length(mo) + 1]] <- cbind(map = so$species[i], moran_knn(mv$row, mv$col, res))
    dm[[length(dm) + 1]] <- rbind(cbind(map = so$species[i], what = "observed", dist_metrics(mv$col, mv$row, mv$obs)),
                                  cbind(map = so$species[i], what = "predicted", dist_metrics(mv$col, mv$row, mv$pred)))
  }
  mo <- do.call(rbind, mo); dm <- do.call(rbind, dm)
  cat("\n[3.4] Moran's I of the residuals of the best member (k = 8 nearest cells; n_eff_approx = n (1 - I) / (1 + I)):\n")
  print(mo, digits = 3, row.names = FALSE)
  cat("\nDistribution metrics, observed vs predicted (grid cells; D95 = cells holding 95% of the mass):\n")
  print(dm, digits = 4, row.names = FALSE)
  write.csv(mo, file.path(out_dir, "08_moran_residuals_best.csv"), row.names = FALSE)
  write.csv(dm, file.path(out_dir, "08_distribution_metrics_best.csv"), row.names = FALSE)
  }   # end of 3.1 to 3.4 (skipped when PRED_ONLY is TRUE)

  # ---- 3.5 relative RMSE of the 12 stanza biomass series (gag and red grouper), baseline vs best vs members ----------
  # These series come from the stock assessments and were part of the GA objective (like all 58 fitted series);
  # the RRMSE below is a reference for them. Relative series are scaled by q = mean(pred) / mean(obs)
  # and RRMSE = rmse / mean(obs), as in the rest of the project. Writes plots_R/ts/Reference_stanza_biomass_rrmse.csv.
  th <- obs.ts$ts.head; j_all <- which(grepl("_age[1-6]_biomass$", th$Title))
  N_RR <- sum(runs$fitness <= quantile(runs$fitness, ENS_PROP))   # members of the ensemble (runs is sorted by fitness); m1 = best model
  obs_yrs <- as.numeric(rownames(obs.ts$ts)); styear <- min(obs_yrs)
  # the predictions of all models are read once in 3.6 below, and this section is computed from them

  # ---- 3.6 relative RMSE of EVERY fitted series (survey indices by region, biomass, catch, F), baseline vs best vs ensemble ----
  # Same prediction extraction and q rescaling as fn.ecospace_objfxn (types 0/1 biomass summed over the stock's pool codes and
  # area-weighted over regions, 6/61 catch, 104 F = catch / biomass); RRMSE = rmse / mean(obs) on the natural scale, only for series
  # with weight > 0. Writes plots_R/ts/Fitted_series_rrmse.csv (read by "fig rrmse fitted series phase6.R").
  fit_rows <- which(th$Type %in% c(0, 1, 6, 61, 104) & !is.na(th$Weight) & th$Weight > 0)
  nm_ <- function(x) gsub("\\s+", "_", trimws(gsub('"', "", as.character(x))))
  pred_all_dir <- function(d){   # predictions of every fitted series, rescaled by q like in the objective (years x series)
    regs <- r4(".detect_ecospace_regions")(d, "annual")
    bio <- r4("fn.read_pred_ecospace_wide")(d, "Biomass", "annual", regs, styear, aggregate_by_group = FALSE)
    cth <- r4("fn.read_pred_ecospace_wide")(d, "Catch", "annual", regs, styear, aggregate_by_group = TRUE)
    fmo <- r4(".derive_fmort_arr")(cth, bio, b.floor.frac = 1e-3, norm = nm_)
    rid <- as.integer(sub("R", "", dimnames(bio)[[3]])); r0 <- if(0L %in% rid) which(rid == 0L) else 1L
    gcol <- function(arr, pc) match(nm_(group.names[pc]), nm_(dimnames(arr)[[2]]))
    NAv <- rep(NA_real_, length(obs_yrs))
    sapply(fit_rows, function(j){
      tc <- th$Type[j]; arr <- switch(as.character(tc), "0" = , "1" = bio, "6" = , "61" = cth, "104" = fmo)
      if(is.null(arr)) return(NAv)
      common <- match(obs_yrs, as.numeric(dimnames(arr)[[1]]))
      if(tc %in% c(0, 1)){
        pcs <- if("Poolcodes" %in% names(th)) th$Poolcodes[[j]] else integer(0); if(length(pcs) == 0) pcs <- as.integer(th$Poolcode[j])
        rg <- if("Regions" %in% names(th)) th$Regions[[j]] else integer(0); if(length(rg) == 0 && !is.na(th$Region[j])) rg <- as.integer(th$Region[j])
        ri_ <- if(length(rg) == 0 || all(is.na(rg)) || any(rg == 0L)) r0 else match(rg, rid); if(anyNA(ri_)) return(NAv)
        cols <- vapply(pcs, function(p) gcol(arr, p), integer(1)); if(length(cols) == 0 || anyNA(cols)) return(NAv)
        w <- r4(".region_weights")(rid[ri_], region.areas); p <- rep(0, length(common))
        for(k in seq_along(ri_)){ sk <- rep(0, length(common)); for(ci in cols) sk <- sk + arr[, ci, ri_[k], 1][common]; p <- p + w[k] * sk }
        p <- p / sum(w)
      } else {
        ri_ <- if(is.na(th$Region[j])) 1L else match(th$Region[j], rid); cl <- gcol(arr, th$Poolcode[j]); if(is.na(ri_) || is.na(cl)) return(NAv)
        p <- arr[, cl, ri_, 1][common]
      }
      o <- obs.ts$ts[, j]; o[o <= 0] <- NA; ok <- !is.na(o) & !is.na(p) & p > 0; if(!any(ok)) return(NAv)
      q <- 1; if(!isTRUE(th$Absolute[j])) q <- mean(p[ok]) / mean(o[ok])
      p / q })
  }
  dirs_all <- c(base = file.path(run_dir, "output/run_base"),
                setNames(vapply(runs$cmd_file, member_dir, character(1)), paste0("m", seq_len(nrow(runs)))))   # every final-generation model, best first
  cache_pa <- file.path(out_dir, "06_pred_all_cache.rds")   # predictions of every model are cached: delete the file to read the output folders again
  if(file.exists(cache_pa) && identical(names(PA <- readRDS(cache_pa)), names(dirs_all))) message("predictions read from ", cache_pa) else {
    one_dir <- function(d) tryCatch(pred_all_dir(d), error = function(e) matrix(NA_real_, length(obs_yrs), length(fit_rows)))
    nc <- if(.Platform$OS.type == "windows") 1L else (if(exists("N_CORES")) as.integer(N_CORES) else as.integer(Sys.getenv("EWE_CORES", "3")))   # cores: set N_CORES or EWE_CORES
    PA <- if(nc > 1L) parallel::mclapply(dirs_all, one_dir, mc.cores = nc) else lapply(dirs_all, one_dir)
    saveRDS(PA, cache_pa) }
  OBS <- obs.ts$ts[, fit_rows, drop = FALSE]; OBS[OBS <= 0] <- NA
  rr_mat <- function(Ps) sapply(seq_along(fit_rows), function(k){ o <- OBS[, k]; p <- Ps[, k]; ok <- !is.na(o) & !is.na(p)
                                 if(!any(ok)) NA_real_ else sqrt(mean((o[ok] - p[ok])^2)) / mean(o[ok]) })
  RA <- sapply(PA, rr_mat); rownames(RA) <- th$Title[fit_rows]
  memA <- RA[, 2:(N_RR + 1), drop = FALSE]   # the ENS_PROP ensemble (members are sorted by NLL, m1 = best)

  # ---- 3.5 (computed from the predictions above) relative RMSE of the 12 stanza biomass series ----
  RR <- RA[th$Title[j_all], , drop = FALSE]
  mem <- RR[, 2:(N_RR + 1), drop = FALSE]
  rr_out <- data.frame(series = th$Title[j_all], rrmse_baseline = RR[, "base"], rrmse_best = RR[, "m1"], rrmse_ens_mean = rowMeans(mem),
                       rrmse_ens_min = apply(mem, 1, min), rrmse_ens_max = apply(mem, 1, max))
  rr_out$species <- ifelse(grepl("^GAG", rr_out$series), "Gag", "Red grouper")
  rr_out$n_members <- N_RR
  rr_out$stanza <- as.integer(sub(".*_age([1-6])_.*", "\\1", rr_out$series)) - 1
  rr_out <- rr_out[order(rr_out$species, rr_out$stanza), ]; print(round(rr_out[, 2:6], 3))
  agg <- function(k) c(baseline = mean(rr_out$rrmse_baseline[k]), best = mean(rr_out$rrmse_best[k]), ens_mean = mean(rr_out$rrmse_ens_mean[k]),
                       ens_min = min(colMeans(mem[rr_out$series[k], , drop = FALSE])), ens_max = max(colMeans(mem[rr_out$series[k], , drop = FALSE])))
  print(round(rbind(Gag = agg(rr_out$species == "Gag"), Red = agg(rr_out$species != "Gag"), All = agg(rep(TRUE, nrow(rr_out))),
                    `Gag no stanza5+` = agg(rr_out$species == "Gag" & rr_out$stanza < 5)), 3))
  write.csv(rr_out, file.path(run_dir, "plots_R", "ts", "Reference_stanza_biomass_rrmse.csv"), row.names = FALSE)
  fa <- data.frame(series = th$Title[fit_rows], type = th$Type.name[fit_rows], region = th$Region[fit_rows], weight = th$Weight[fit_rows],
                   rrmse_baseline = RA[, "base"], rrmse_best = RA[, "m1"], rrmse_ens_mean = rowMeans(memA, na.rm = TRUE),
                   rrmse_ens_min = apply(memA, 1, min, na.rm = TRUE), rrmse_ens_max = apply(memA, 1, max, na.rm = TRUE), n_members = N_RR)
  fa$species <- ifelse(grepl("^GAG", fa$series, ignore.case = TRUE), "Gag", ifelse(grepl("^RED|^RGR", fa$series, ignore.case = TRUE), "Red grouper", "Other"))
  fa <- fa[!is.na(fa$rrmse_baseline) & !is.na(fa$rrmse_best), ]
  cat("\nFitted series with an RRMSE:", nrow(fa), "| share improved by the best model:", round(mean(fa$rrmse_best < fa$rrmse_baseline), 2),
      "| by the ensemble mean:", round(mean(fa$rrmse_ens_mean < fa$rrmse_baseline), 2), "\n")
  print(aggregate(cbind(rrmse_baseline, rrmse_best, rrmse_ens_mean) ~ type, fa, mean), digits = 3)
  write.csv(fa, file.path(run_dir, "plots_R", "ts", "Fitted_series_rrmse.csv"), row.names = FALSE)

  # ---- 3.7 which NLL cut-off for the ensemble? coverage, band width and error of the ensemble mean for cut-offs from the best model alone to all 370 ----
  # Members are the best k models (k from the quantile of the final-generation NLL). Coverage = share of fitted observations inside the min-max band;
  # width = mean band width / mean observation; RRMSE of the ensemble mean (natural scale; "w_" = times the series weight). Writes plots_R/ts/Ensemble_cutoff_assessment.csv.
  PAarr <- array(unlist(PA[-1]), dim = c(length(obs_yrs), length(fit_rows), length(PA) - 1))
  fit_all <- runs$fitness; cut_p <- c(0, seq(0.1, 1, by = 0.1))
  wts <- th$Weight[fit_rows]; typ <- th$Type.name[fit_rows]
  one <- function(k){
    sub <- PAarr[, , seq_len(k), drop = FALSE]
    mn <- apply(sub, c(1, 2), mean, na.rm = TRUE); lo <- apply(sub, c(1, 2), min, na.rm = TRUE); hi <- apply(sub, c(1, 2), max, na.rm = TRUE)
    lo[!is.finite(lo)] <- NA; hi[!is.finite(hi)] <- NA
    cov_ <- sapply(seq_along(fit_rows), function(i){ ok <- !is.na(OBS[, i]) & !is.na(lo[, i]); if(!any(ok)) NA_real_ else mean(OBS[ok, i] >= lo[ok, i] & OBS[ok, i] <= hi[ok, i]) })
    wid_ <- sapply(seq_along(fit_rows), function(i){ ok <- !is.na(OBS[, i]) & !is.na(lo[, i]); if(!any(ok)) NA_real_ else mean(hi[ok, i] - lo[ok, i]) / mean(OBS[ok, i]) })
    data.frame(series = th$Title[fit_rows], type = typ, weight = wts, n_members = k, coverage = cov_, rel_width = wid_, rrmse_mean = rr_mat(mn)) }
  cut_df <- do.call(rbind, lapply(cut_p, function(p){ k <- if(p == 0) 1 else sum(fit_all <= quantile(fit_all, p)); r <- one(k); r$cutoff <- p; r }))
  cut_df <- cut_df[!is.na(cut_df$rrmse_mean), ]; cut_df$w_rrmse_mean <- cut_df$rrmse_mean * cut_df$weight
  summ <- function(d) data.frame(n_members = d$n_members[1], n_series = nrow(d), coverage = mean(d$coverage, na.rm = TRUE), rel_width = mean(d$rel_width, na.rm = TRUE),
                                 rrmse_mean = mean(d$rrmse_mean), w_rrmse_mean = mean(d$w_rrmse_mean), w_rrmse_mean_weighted = weighted.mean(d$rrmse_mean * d$weight, d$weight))
  cut_out <- do.call(rbind, lapply(split(cut_df, cut_df$cutoff), function(d) rbind(cbind(cutoff = d$cutoff[1], group = "All series", summ(d)),
                                   do.call(rbind, lapply(split(d, d$type), function(e) cbind(cutoff = e$cutoff[1], group = e$type[1], summ(e)))))))
  print(round(cut_out[cut_out$group == "All series", c("cutoff", "n_members", "coverage", "rel_width", "rrmse_mean", "w_rrmse_mean")], 3), row.names = FALSE)
  write.csv(cut_out, file.path(run_dir, "plots_R", "ts", "Ensemble_cutoff_assessment.csv"), row.names = FALSE)
  write.csv(cut_df, file.path(run_dir, "plots_R", "ts", "Ensemble_cutoff_assessment_by_series.csv"), row.names = FALSE)
}

# ---- 4. run-to-run noise from parameter sets that the GA evaluated more than once -------------------------
# ga_evals_ga01.csv holds every evaluation of the run. Some offspring were identical parameter vectors, so the
# same set was run twice: the difference between the two scores is a direct measure of the noise of Ecospace
# at the NLL levels the GA worked at. This needs no new runs and answers the noise bracket and the tolerance.
ev <- read.csv(file.path(res_dir, paste0("ga_evals_", tag, ".csv")), check.names = FALSE)
pcols <- setdiff(names(ev), c("gen", "idx", "role", "kept", "fitness", "run"))
ev$key <- do.call(paste, c(lapply(ev[pcols], function(v) format(round(v, 6), trim = TRUE)), sep = "|"))
dup <- ev[ev$key %in% ev$key[duplicated(ev$key)], ]
st  <- do.call(rbind, lapply(split(dup, dup$key), function(d)
  data.frame(n = nrow(d), mean = mean(d$fitness), sd = sd(d$fitness), range = diff(range(d$fitness)))))
cat("\n[4] Parameter vectors evaluated more than once:", nrow(st), "(of", nrow(ev), "evaluations)\n")
big <- st$range > 1000
cat("Pairs that differ by more than 1,000 NLL units:", sum(big), "(", paste(round(st$range[big]), collapse = ", "), ")\n")
noise <- function(x, label) cat(sprintf("%-28s vectors %3d | pooled SD %5.1f | median range %5.1f | relative SD %.2f%%\n", label, nrow(x),
                                        sqrt(mean(x$sd^2)), median(x$range), 100 * sqrt(mean(x$sd^2)) / mean(x$mean)))
ok <- st[!big, ]
noise(ok, "all, without the outliers")
noise(ok[ok$mean < 8000, ], "mean NLL below 8,000")
noise(ok[ok$mean >= 8000 & ok$mean < 10000, ], "mean NLL 8,000 to 10,000")
sdn <- sqrt(mean(ok$sd[ok$mean < 8000]^2))
cat("Stop tolerance", round(tol, 1), "= ", round(tol / sdn, 2), "x the SD of a single run near the optimum.\n")
cat("SD of the difference between two members =", round(sqrt(2) * sdn, 1), "NLL units; ensemble span =",
    round(diff(range(runs$fitness[seq_len(N_ENS)])), 1), "(", round(diff(range(runs$fitness[seq_len(N_ENS)])) / (sqrt(2) * sdn), 1), "such SDs ).\n")
write.csv(st, file.path(out_dir, "04_repeated_evaluations.csv"))

# ---- 5. replicates of the finalists vs the stopping tolerance ---------------------------------------------
# File written by 'ga post-run checks WFS MICE.R' (Windows): columns id, rep, nll, fitness_ga.
replicate_summary <- function(rep_csv, tol = 32.2973, baseline_sd = 21.18, B = 2000) {
  r <- read.csv(rep_csv); stopifnot(all(c("id", "nll", "fitness_ga") %in% names(r)))
  m   <- tapply(r$nll, r$id, mean); sdv <- tapply(r$nll, r$id, sd); ga <- tapply(r$fitness_ga, r$id, mean)
  cat("members:", length(m), "| replicates per member:", paste(range(table(r$id)), collapse = " to "), "\n")
  cat("median within-member SD:", round(median(sdv), 1), "| baseline SD:", baseline_sd, "| tolerance:", round(tol, 1),
      "| SD / tolerance:", round(median(sdv) / tol, 2), "\n")
  cat("spread of the members' mean NLL:", round(diff(range(m)), 1), "| spread of the GA fitness:", round(diff(range(ga)), 1), "\n")
  cat("optimism of the GA score (GA fitness minus replicate mean, averaged over members):", round(mean(ga - m[names(ga)]), 1),
      "| for the best member:", round((ga - m[names(ga)])[which.min(ga)], 1), "\n")
  cat("rank correlation between GA fitness and replicate mean (Spearman):", round(cor(ga, m[names(ga)], method = "spearman"), 2), "\n")
  win <- replicate(B, { d <- sapply(split(r$nll, r$id), sample, 1); names(which.min(d)) })
  cat("share of single draws in which the member with the lowest mean also has the lowest value:",
      round(mean(win == names(which.min(m))), 2), "\n")
}
rep_file <- file.path(run_dir, "sens1", "finalist_replicates.csv")
if (file.exists(rep_file)) replicate_summary(rep_file, tol) else
  cat("\n[5] finalist_replicates.csv not found: run 'ga post-run checks WFS MICE.R' on the Windows machine first.\n")

cat("\nDone. Tables in:", out_dir, "\n")
