# Supplementary table: composition of the NLL of the best model (temporal vs spatial, species, data type, age-0 indices, focal maps)
# Reads the three nll_*.csv files written by the run script; writes Table_S_nll_composition.csv to plots_R/ts.
if(!exists("RUN")) RUN <- Sys.getenv("EWE_RUN", unset = "..")
P <- file.path(RUN, "plots_R")
part <- read.csv(file.path(P, "nll_partition_temporal_vs_spatial.csv"))
ts_  <- read.csv(file.path(P, "nll_temporal_by_series.csv"))
sp_  <- read.csv(file.path(P, "nll_spatial_by_map.csv"))
b <- part[part$model == "best", ]; b0 <- part[part$model == "baseline", ]
sp_$species <- ifelse(grepl("^gag", sp_$map), "Gag", ifelse(grepl("^red", sp_$map), "Red grouper", "Other (not scored for the best model)"))
ts_$species <- ifelse(grepl("^GAG", ts_$Title), "Gag", ifelse(grepl("^REDGRP", ts_$Title), "Red grouper", "Other groups"))
tt <- b$temporal_NLL; tot <- b$total_NLL
row <- function(block, item, base, best, of, denom) data.frame(block = block, item = item, NLL_baseline = round(base, 1), NLL_best = round(best, 1),
                                                                 pct_of_best = round(100 * best / denom, 1), pct_of = of)
out <- rbind(
  row("Total objective", "Temporal NLL", b0$temporal_NLL, b$temporal_NLL, "total", tot),
  row("Total objective", "Spatial NLL", b0$spatial_NLL, b$spatial_NLL, "total", tot),
  row("Total objective", "Total", b0$total_NLL, tot, "total", tot))
for(s in c("Gag", "Red grouper", "Other groups")){ k <- ts_$species == s
  out <- rbind(out, row("Temporal NLL by species", s, sum(ts_$loglik.baseline[k]), sum(ts_$loglik.best[k]), "temporal NLL", tt)) }
for(ty in unique(ts_$Type.name)){ k <- ts_$Type.name == ty
  out <- rbind(out, row("Temporal NLL by data type", ty, sum(ts_$loglik.baseline[k]), sum(ts_$loglik.best[k]), "temporal NLL", tt)) }
a0 <- grepl("AGE0", ts_$Title)
out <- rbind(out, row("Temporal NLL, age-0 biomass indices", paste0("All age-0 indices (n = ", sum(a0), ")"), sum(ts_$loglik.baseline[a0]), sum(ts_$loglik.best[a0]), "temporal NLL", tt))
top <- ts_[a0, ]; top <- top[order(-top$loglik.best), ]
for(i in seq_len(nrow(top))) out <- rbind(out, row("Temporal NLL, age-0 biomass indices", top$Title[i], top$loglik.baseline[i], top$loglik.best[i], "temporal NLL", tt))
fm <- sp_[!is.na(sp_$best), ]; sb <- sum(fm$best)
for(s in c("Gag", "Red grouper")){ k <- fm$species == s
  out <- rbind(out, row("Spatial NLL, focal maps by species", s, sum(fm$baseline[k]), sum(fm$best[k]), "focal-map spatial NLL", sb)) }
for(i in seq_len(nrow(fm))) out <- rbind(out, row("Spatial NLL, focal maps by stanza", fm$map[i], fm$baseline[i], fm$best[i], "focal-map spatial NLL", sb))
dir.create(file.path(P, "ts"), showWarnings = FALSE)
write.csv(out, file.path(P, "ts", "Table_S_nll_composition.csv"), row.names = FALSE)
print(out)
