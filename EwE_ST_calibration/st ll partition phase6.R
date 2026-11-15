# NLL partition (temporal vs spatial) for the phase6 / ga01 models
# RUN ON THE MACHINE THAT HAS R4EwE + the sdmTMB maps (the Windows laptop):
# the GA did not save the split per candidate, so the best model, the baseline and the
# ensemble members are re-scored here. fn.ecospace_objfxn(return = "both") gives the
# per-group temporal LL and the per-map spatial LL (already x spatial.weight).
# Note: Ecospace is stochastic, but re-scoring the SAME output folders is deterministic.

setwd("<folder that contains ga_results/phase6/inputs>")      # as for the GA scripts
source("setup cal WFS MICE.R")                                 # builds obs.ts, spatial.obs, region.areas, ...

RUN      <- "C:/.../ST_calibration/phase6_ga01"                # <- the folder of the run
ga_runs  <- read.csv(file.path(RUN, "results", "final_ga_runs_ga01.csv"))
ga_runs$dir <- file.path(RUN, "output", "gen30_small", basename(dirname(gsub("\\\\", "/", ga_runs$cmd_file))))
best     <- ga_runs[which.min(ga_runs$fitness), ]
dirs     <- c(baseline = file.path(RUN, "output", "run_base"), best = best$dir)

partition <- function(d){
  r <- fn.ecospace_objfxn(dir.pred = d, obs.ts = obs.ts, group.names = group.names, fleet.names = fleet.names,
                          fit.abs.catch = ph$settings$fit.abs.catch, spatial.weight = spatial.weight,
                          return = "vector", ll.file = file.path(d, "LL_components.csv"))
  sp <- grepl("^LL\\.spatial", names(r)); tot <- grepl("^LL\\.total$", names(r))
  temporal <- sum(r[!sp & !tot], na.rm = TRUE); spatial <- sum(r[sp], na.rm = TRUE)
  data.frame(temporal = temporal, spatial = spatial, total = temporal + spatial,
             pct_temporal = 100 * temporal / (temporal + spatial), pct_spatial = 100 * spatial / (temporal + spatial))
}
res <- do.call(rbind, lapply(dirs, partition)); print(round(res, 1))
write.csv(res, file.path(RUN, "plots_R", "nll_partition_temporal_vs_spatial.csv"))
# optional: same split for the AIC ensemble members -> lapply over ga_runs$dir[order(ga_runs$fitness)][1:40]
