#---------------------------------------------------------------------------------------------------------------
# Figure: contribution of components to the fit (phase6 ga01)
#   A  change in NLL (% of the total baseline NLL) for all groups, gag and red grouper, by type of series (biomass indices, catch, F) and spatial
#   B  total change in NLL allocated to each parameter type by its share of the NLL variation (random forest, grouped permutation importance; descriptive)
#   C  gag: change in NLL (% of each series' baseline) by stanza for biomass (indices summed within the stanza), catch and F; best model, ensemble mean and range
#   D  red grouper, same as C
#   (older versions of this figure also had the ten largest series, the ten largest parameters and the spatial maps; the code is kept below but not plotted)
#
# The stanza biomass series (GAG_age1..6_biomass, REDGRP_age1..6_biomass) come from the stock assessments
# and are treated as reference series. They are EXCLUDED here (EXCLUDE_STANZA_BIOMASS = TRUE).
# Set it to FALSE to get the full 58-series objective that the GA actually minimized (baseline about 11,100, best 6,459).
#
# Sourced from the main script (st calibration run phase6.R, section 7) or run on its own.
# Needs only: ggplot2, patchwork (both already used by st_calibration_functions.R).
# Inputs (all already on disk): plots_R/nll_temporal_by_series.csv, plots_R/nll_spatial_by_map.csv,
#   plots_R/nll_partition_temporal_vs_spatial.csv, plots_R/discussion_checks/06_member_series_nll.csv,
#   plots_R/discussion_checks/06_member_totals_and_maps.csv, results/parvec_ga01.csv, results/ga_evals_ga01.csv (needs ranger)
# The PNG is rendered from the PDF (pdftools) so both carry the same typography.
# Output: plots_R/ts/Fig_contribution_of_components.png (+ .pdf) and a csv with every number plotted.
#---------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(patchwork)

RUN <- if(exists("RUN")) RUN else "/Users/daniel/Work/ST_calibration/phase6_ga01"   # uses RUN from the main script when sourced
EXCLUDE_STANZA_BIOMASS <- TRUE
N_TOP <- 10

pr  <- file.path(RUN, "plots_R")
ser <- read.csv(file.path(pr, "nll_temporal_by_series.csv"), check.names = FALSE)
spm <- read.csv(file.path(pr, "nll_spatial_by_map.csv"),      check.names = FALSE)
partn <- read.csv(file.path(pr, "nll_partition_temporal_vs_spatial.csv"), check.names = FALSE)
mem <- read.csv(file.path(pr, "discussion_checks", "06_member_series_nll.csv"), check.names = FALSE)
ms  <- read.csv(file.path(pr, "discussion_checks", "06_member_totals_and_maps.csv"), check.names = FALSE)   # one row per ensemble member
mem_all <- mem; mem_all$id <- sub("\\|.*$", "", mem_all$series)

is_stanza_bio <- function(x) grepl("^(GAG|REDGRP)_age[1-6]_biomass$", sub("\\|.*$", "", x))
ser_all <- ser; ser_all$delta <- ser_all$loglik.best - ser_all$loglik.baseline   # full 58 series, used only for the biomass bars in panel B
if(EXCLUDE_STANZA_BIOMASS){
  ser <- ser[!is_stanza_bio(ser$Title), ]
  mem <- mem[!is_stanza_bio(mem$series), ]
}
ser$base <- ser$loglik.baseline; ser$best <- ser$loglik.best; ser$delta <- ser$best - ser$base
message("series used: ", nrow(ser), " | temporal NLL baseline ", round(sum(ser$base), 1), ", best ", round(sum(ser$best), 1))

# palette taken from the manuscript figures (st_calibration_functions.R): baseline = black, best = turquoise,
# ensemble mean = dark magenta, ensemble range = magenta (members), temporal NLL = dark blue (as in Fig. 2)
col_base <- "black"; col_best <- "#20B2AA"; col_mem <- "#B5367A"; col_mean <- "#6E1F4D"
col_t <- "#1B4F72"; col_s <- "#E69F00"; col_b <- "#56B4E9"; col_c <- "#D55E00"; col_f <- "#999999"
th <- theme_bw(base_size = 18) +
  theme(plot.title = element_text(face = "bold", size = 15), strip.background = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7),
        strip.text = element_text(face = "bold", hjust = 0, size = 15), axis.title = element_text(face = "bold"), legend.position = "bottom",
        legend.title = element_blank(), axis.text = element_text(colour = "black"),
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.4), panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.4, linetype = "dashed"))

#---- A: partition --------------------------------------------------------------------------------------------
sp_best_total <- partn$spatial_NLL[partn$model == "best"]       # best-fit maps are not all saved, so the total comes from fitness - temporal
mapsum <- function(keys, col) sum(spm[[col]][spm$map %in% keys])
temp <- function(sel, col) sum(ser[[col]][sel])
gag <- grepl("^GAG", ser$Title); red <- grepl("^REDGRP", ser$Title)
A <- rbind(
  data.frame(group = "All groups",  model = c("Baseline", "Best"), temporal = c(temp(TRUE, "base"), temp(TRUE, "best")),
             spatial = c(sum(spm$baseline), sp_best_total)),
  data.frame(group = "Gag",         model = c("Baseline", "Best"), temporal = c(temp(gag, "base"), temp(gag, "best")),
             spatial = c(mapsum(c("gag 2-4", "gag 5+"), "baseline"), mapsum(c("gag 2-4", "gag 5+"), "best"))),
  data.frame(group = "Red grouper", model = c("Baseline", "Best"), temporal = c(temp(red, "base"), temp(red, "best")),
             spatial = c(mapsum(c("red grouper 3-4", "red grouper 5+"), "baseline"),
                         mapsum(c("red grouper 3-4", "red grouper 5+"), "best"))))
A$group <- factor(A$group, levels = c("All groups", "Gag", "Red grouper")); A$model <- factor(A$model, levels = c("Baseline", "Best"))
# change in NLL (best minus baseline) split by type of series; the 46 series scored as data (stanza biomass excluded)
stype <- ifelse(grepl("(_catch|_relCatch|SSrelCatch)$", ser$Title), "Catch", ifelse(grepl("_F$", ser$Title), "Fishing mortality", "Survey indices"))
sp_delta <- function(keys) sum(spm$best[spm$map %in% keys] - spm$baseline[spm$map %in% keys])
mk <- function(group, sel, sp){
  d <- tapply(ser$delta[sel], factor(stype[sel], levels = c("Survey indices", "Catch", "Fishing mortality")), sum)
  data.frame(group = group, part = c("Survey indices", "Catch", "Fishing mortality", "Spatial"), delta = c(as.numeric(d), sp))
}
Ad <- rbind(mk("All groups", rep(TRUE, nrow(ser)), sp_best_total - sum(spm$baseline)),
            mk("Gag", gag, sp_delta(c("gag 2-4", "gag 5+"))),
            mk("Red grouper", red, sp_delta(c("red grouper 3-4", "red grouper 5+"))))
Ad$group <- factor(Ad$group, levels = c("All groups", "Gag", "Red grouper"))
Ad$part  <- factor(Ad$part, levels = c("Spatial", "Fishing mortality", "Catch", "Survey indices"))
base_tot <- c("All groups" = A$temporal[1] + A$spatial[1], "Gag" = A$temporal[3] + A$spatial[3], "Red grouper" = A$temporal[5] + A$spatial[5])
net <- aggregate(delta ~ group, Ad, sum)
net$neg <- tapply(pmin(Ad$delta, 0), Ad$group, sum)[as.character(net$group)]
net$pct <- 100 * net$delta / base_tot[as.character(net$group)]
net$lab <- paste0(ifelse(net$delta > 0, "+", "-"), formatC(abs(round(net$delta)), format = "d", big.mark = ","), "\n(", sprintf("%+.0f", net$pct), "%)")
fill_all <- c("Survey indices" = col_t, "Catch" = col_c, "Fishing mortality" = col_f, "Spatial" = col_s)   # one legend for A and B (drawn in B)
Ad$pct <- 100 * Ad$delta / base_tot[["All groups"]]   # change as % of each group's baseline NLL, so one common scale
net$pct <- 100 * net$delta / base_tot[["All groups"]]
net$lab <- paste0(ifelse(net$delta > 0, "+", "-"), formatC(abs(round(net$delta)), format = "d", big.mark = ","), "\n(", sprintf("%+.0f", net$pct), "%)")
net$negp <- tapply(pmin(Ad$pct, 0), Ad$group, sum)[as.character(net$group)]
rg <- Ad[Ad$group == "Red grouper", ]; rg <- rg[order(match(rg$part, c("Survey indices", "Catch", "Fishing mortality", "Spatial"))), ]
cs <- cumsum(rg$pct); mid <- cs - rg$pct / 2   # segment midpoints in the red grouper bar
lab_A <- data.frame(part = factor(c("Biomass", "Catch", "F", "Spatial"), levels = c("Biomass", "Catch", "F", "Spatial")), y = c(mid[1], mid[2], mid[3], min(cs)), ly = c(-7, -14, -21, -28))
xa <- c("All groups" = 1, "Gag" = 2.7, "Red grouper" = 3.7); Ad$xa <- xa[as.character(Ad$group)]; net$xa <- xa[as.character(net$group)]
# A (final): change in NLL as a percentage of the baseline NLL of each type of series, so it is the same quantity as in C and D
tb <- function(sel, ty) sum(ser$base[sel & stype == ty])
bs_all <- c(Biomass = tb(rep(TRUE, nrow(ser)), "Survey indices"), Catch = tb(rep(TRUE, nrow(ser)), "Catch"), F = tb(rep(TRUE, nrow(ser)), "Fishing mortality"), Spatial = sum(spm$baseline))
bs_g   <- c(Biomass = tb(gag, "Survey indices"), Catch = tb(gag, "Catch"), F = tb(gag, "Fishing mortality"), Spatial = mapsum(c("gag 2-4", "gag 5+"), "baseline"))
bs_r   <- c(Biomass = tb(red, "Survey indices"), Catch = tb(red, "Catch"), F = tb(red, "Fishing mortality"), Spatial = mapsum(c("red grouper 3-4", "red grouper 5+"), "baseline"))
Ap <- Ad; Ap$part <- factor(as.character(Ap$part), levels = c("Survey indices", "Catch", "Fishing mortality", "Spatial"), labels = c("Biomass", "Catch", "F", "Spatial"))
Ap$base <- mapply(function(g, p) list(`All groups` = bs_all, Gag = bs_g, `Red grouper` = bs_r)[[as.character(g)]][[as.character(p)]], Ap$group, Ap$part)
Ap <- Ap[Ap$group != "All groups", ]; Ap$group <- droplevels(Ap$group)   # gag and red grouper only
Ap$pct_own <- 100 * Ap$delta / Ap$base
Ap$xa <- (as.integer(Ap$part) - 1) * 3 + as.integer(Ap$group)
bs_gr <- bs_g + bs_r
Ap$txt <- sprintf("%+.0f", Ap$pct_own)
write.csv(Ap[, c("group", "part", "delta", "base", "pct_own")], file.path(pr, "ts", "Fig_contribution_A_pct_own_baseline.csv"), row.names = FALSE)
# A (final): baseline -> fitted, gag and red grouper only, fitted data only (46 scored series + the four focal maps)
# best model, ensemble mean and ensemble range, as a percentage of the baseline NLL of each type
Smx <- read.csv(file.path(pr, "discussion_checks", "06_member_series_matrix.csv"), check.names = FALSE); Smx$member <- NULL; sid_all <- sub("\\|.*$", "", names(Smx))
ptypes <- c("Survey indices", "Catch", "Fishing mortality")
mem_sum <- function(sel, ty) rowSums(Smx[, sid_all %in% ser$Title[sel & stype == ty], drop = FALSE])
mem_sp <- function(keys) rowSums(ms[, keys, drop = FALSE])
mkA <- function(sp, sel, keys){
  rows <- lapply(c(ptypes, "Spatial"), function(ty){
    if(ty == "Spatial"){ m <- mem_sp(keys); b0 <- mapsum(keys, "baseline"); bt <- mapsum(keys, "best") } else { m <- mem_sum(sel, ty); b0 <- tb(sel, ty); bt <- sum(ser$best[sel & stype == ty]) }
    data.frame(species = sp, type = ty, base = b0, best = bt, mean = mean(m), min = min(m), max = max(m)) })
  do.call(rbind, rows) }
A2 <- rbind(mkA("Gag", gag, c("gag 2-4", "gag 5+")), mkA("Red grouper", red, c("red grouper 3-4", "red grouper 5+")))
A2$part <- factor(A2$type, levels = c(ptypes, "Spatial"), labels = c("Biomass", "Catch", "F", "Spatial"))
base_gr <- sum(A2$base)   # one common baseline for A and B: total NLL of gag + red grouper fitted data, so the bars add up to the total change
for(v in c("best", "mean", "min", "max")) A2[[paste0(v, "_p")]] <- 100 * (A2[[v]] - A2$base) / base_gr
A2$xa <- (as.integer(A2$part) - 1) * 3 + ifelse(A2$species == "Gag", 0.85, 2.15)
write.csv(A2, file.path(pr, "ts", "Fig_contribution_A_best_ensemble.csv"), row.names = FALSE)
pA <- ggplot(A2, aes(xa)) + geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = c(3, 6, 9), colour = "grey30") +
  geom_point(aes(y = best_p), colour = col_best, shape = 16, size = 5) +
  geom_errorbar(aes(ymin = min_p, ymax = max_p), colour = "black", width = 0.3, linewidth = 0.5) +
  annotate("text", x = (0:3) * 3 + 1.5, y = Inf, label = c("Biomass", "Catch", "F", "Spatial"), vjust = 1.4, fontface = "bold", size = 5) +
  scale_x_continuous(breaks = as.vector(outer(c(0.85, 2.15), (0:3) * 3, "+")), labels = rep(c("Gag", "Red"), 4), limits = c(0.25, 11.75), expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-45, 15), breaks = seq(-40, 10, 10), expand = c(0, 0)) +
  labs(x = NULL, y = "Change in NLL\n(% of total)") + th +
  theme(axis.text.x = element_text(size = 13), panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.4, linetype = "dashed"), legend.position = "none")

#---- B: change by stanza (catch and F only) ------------------------------------------------------------------
get_e <- function(title){ i <- which(mem_all$id == title)[1]; if(is.na(i)) return(rep(NA_real_, 3))
  c(mem_all$member_mean[i], mem_all$member_min[i], mem_all$member_max[i]) - mem_all$baseline[i] }   # ensemble mean, min, max change
mk_rows <- function(sp, st, meas, tt) data.frame(species = sp, stanza = st, measure = meas, delta = sapply(tt, get_d),
  emean = sapply(tt, function(t) get_e(t)[1]), lo = sapply(tt, function(t) get_e(t)[2]), hi = sapply(tt, function(t) get_e(t)[3]))
get_d <- function(title) { v <- ser_all$delta[ser_all$Title == title]; if(length(v)) v[1] else NA_real_ }
rows <- list()
for(sp in c("Gag", "Red grouper")){
  pre <- if(sp == "Gag") "GAG" else "REDGRP"
  for(a in 1:6){
    ct <- if(sp == "Red grouper" && a < 6) paste0(pre, "_age", a, "_relCatch") else paste0(pre, "_age", a, "_catch")
    rows[[length(rows) + 1]] <- mk_rows(sp, c("0", "1", "2", "3", "4", "5+")[a], c("Catch", "Fishing mortality"), c(ct, paste0(pre, "_age", a, "_F")))
  }
}
B <- do.call(rbind, rows)
B$measure <- factor(B$measure, levels = names(fill_all)[names(fill_all) != "Spatial"])
B$stanza <- factor(B$stanza, levels = c("0", "1", "2", "3", "4", "5+"))
B$species <- factor(B$species, levels = c("Gag", "Red grouper"))
symlog <- scales::trans_new("symlog", function(x) sign(x) * log10(1 + abs(x)), function(y) sign(y) * (10^abs(y) - 1))
ens_scale <- scale_colour_manual(values = c("Best model" = col_best, "Ensemble mean" = col_mean, "Ensemble range" = col_mem),
  breaks = c("Best model", "Ensemble mean", "Ensemble range"),
  guide = guide_legend(override.aes = list(shape = c(18, 16, NA), linetype = c(0, 0, 1), linewidth = c(0, 0, 2.2), size = c(4, 3.5, 1), alpha = c(1, 1, 0.4))))
B$xpos <- as.numeric(B$stanza) + ifelse(B$species == "Red grouper", 7, 0)   # gag at 1-6, red grouper at 8-13, gap in between
make_st <- function(meas, ylab = TRUE){
  d <- B[B$measure == meas, ]
  ggplot(d, aes(xpos)) + geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = 7, colour = "grey40") +
    geom_linerange(aes(ymin = lo, ymax = hi, colour = "Ensemble range"), linewidth = 2.2, alpha = 0.4, na.rm = TRUE) +
    geom_point(aes(y = emean, colour = "Ensemble mean"), size = 3, na.rm = TRUE) +
    geom_point(aes(y = delta, colour = "Best model"), shape = 16, size = 4.2) +
    annotate("text", x = c(3.5, 10.5), y = Inf, label = c("Gag", "Red grouper"), vjust = 1.5, fontface = "bold", size = 4.8) +
    ens_scale + scale_x_continuous(breaks = c(1:6, 8:13), labels = rep(c("0", "1", "2", "3", "4", "5+"), 2), expand = expansion(add = 0.6)) +
    scale_y_continuous(trans = symlog, breaks = c(-1000, -100, -10, 0, 10), labels = c("-1,000", "-100", "-10", "0", "10"), expand = expansion(mult = c(0.05, 0.22))) +
    labs(title = meas, x = "Stanza (age)", y = if(ylab) "Change in NLL" else NULL) + th +
    theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5), axis.text.x = element_text(size = 12, colour = "black"), legend.position = "none",
          panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.4))
}
pBg <- make_st("Catch"); pBr <- make_st("Fishing mortality", ylab = FALSE)

#---- C: focal spatial maps -----------------------------------------------------------------------------------
C <- spm[!is.na(spm$best), ]; C$delta <- C$best - C$baseline; C$pct <- 100 * C$delta / C$baseline
C$lab <- factor(C$map, levels = c("gag 2-4", "gag 5+", "red grouper 3-4", "red grouper 5+"),
                labels = c("Gag\n2-4", "Gag\n5+", "Red\ngrouper\n3-4", "Red\ngrouper\n5+"))
ens_pct <- function(m, f) 100 * (f(ms[[m]]) - C$baseline[C$map == m]) / C$baseline[C$map == m]
C$emean <- sapply(C$map, ens_pct, f = mean); C$lo <- sapply(C$map, ens_pct, f = min); C$hi <- sapply(C$map, ens_pct, f = max)
C$txt <- paste0(sprintf("%+.0f", C$pct), "%\n(", sprintf("%+.1f", C$delta), ")")
#---- D: largest series across members ------------------------------------------------------------------------
nice <- c(GAG_age6_catch = "G 5+ C", GAG_age6_F = "G 5+ F", GAG_age6_biomass = "G 5+ B",
          REDGRP_age6_F = "RG 5+ F", REDGRP_age6_catch = "RG 5+ C",
          GAG_SEDAR105_AGE0_TPR = "G 0, TPR", GAG_SEDAR105_AGE0_TBR = "G 0, TBR",
          GAG_SEDAR105_AGE0_CHR = "G 0, CHR", GAG_SEDAR105_AGE0_FWCNMFS = "G 0, FWC",
          GAG_SEDAR105_AGE0_SAR = "G 0, SAR", GAG_SEDAR105_AGE0_MBB = "G 0, MBB",
          REDGRP_SEDAR88_NMFS_BLL = "RG 5+, BLL", REDGRP_SEDAR88_CmbVid = "RG 5+, video",
          REDGRP_SEDAR88_Rec = "RG 3-4, rec.", GAG_SEDAR72SRFS_Srv_SEAMAP_VIDEO_9 = "G, SEAMAP",
          GAG_SEDAR72SRFS_Rec_HBT_3 = "G, HBT rec.", GAG_SEDAR105_GFISHER = "G, GFISHER")
mem$id <- sub("\\|.*$", "", mem$series)
D <- mem[order(-mem$member_mean), ][seq_len(N_TOP), ]
D$label <- ifelse(D$id %in% names(nice), nice[D$id], D$id)
D$label <- vapply(D$label, function(x) paste(strwrap(x, width = 100), collapse = "\n"), "")
D$label <- factor(D$label, levels = rev(unique(D$label)))
D$best_series <- ser$best[match(D$id, ser$Title)]
pD <- ggplot(D, aes(y = label)) +
  geom_linerange(aes(xmin = member_min, xmax = member_max, colour = "Ensemble range"), linewidth = 2.2, alpha = 0.45) +
  geom_point(aes(x = member_mean, colour = "Ensemble mean"), size = 3) +
  geom_point(aes(x = best_series, colour = "Best model"), shape = 16, size = 5) +
  geom_point(aes(x = baseline, colour = "Baseline model"), shape = 16, size = 5) +
  scale_colour_manual(values = c("Ensemble range" = col_mem, "Ensemble mean" = col_mean, "Best model" = col_best, "Baseline model" = col_base),
                      breaks = c("Baseline model", "Best model", "Ensemble mean", "Ensemble range"),
                      guide = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(linetype = c(0, 0, 0, 1), shape = c(18, 18, 16, NA),
                                                                          linewidth = c(0, 0, 0, 2.2), alpha = c(1, 1, 1, 0.45)))) +
  scale_x_log10(expand = expansion(mult = c(0.03, 0.06))) + labs(x = "Series NLL (log scale)", y = NULL) + th +
  theme(legend.position = "none")

#---- combine -------------------------------------------------------------------------------------------------

#---- E, F: contribution of parameter types to the NLL ---------------------------------------------------------
# Random forest of log10(NLL) on the 98 parameters over the 11,471 GA evaluations; grouped permutation importance.
# Evaluations are not independent (the GA samples near good regions), so the shares are descriptive. Needs ranger.
library(ranger)
par_tab <- read.csv(file.path(RUN, "results", "parvec_ga01.csv"), check.names = FALSE); names(par_tab)[1] <- "par"
e <- read.csv(file.path(RUN, "results", "ga_evals_ga01.csv"), check.names = FALSE)
types <- c("Vulnerabilities", "Red tide mortality", "Red tide foraging", "Environmental curves", "Fleet", "Dispersal")
rt_id <- suppressWarnings(as.integer(sub("^rt(\\d+)_.*", "\\1", par_tab$par)))   # red tide response ids: 39-53 mortality (M0), 54-67 foraging
ptype <- ifelse(grepl("^vul_", par_tab$par), types[1], ifelse(grepl("^rt", par_tab$par), ifelse(rt_id <= 53, types[2], types[3]),
         ifelse(grepl("^env", par_tab$par), types[4], ifelse(grepl("^flt_", par_tab$par), types[5], types[6])))); names(ptype) <- par_tab$par
X <- e[, par_tab$par]; ok <- complete.cases(X) & is.finite(e$fitness); X <- X[ok, ]; y <- log10(e$fitness[ok])
for(j in names(X)) if(min(X[[j]]) > 0 && max(X[[j]]) / min(X[[j]]) > 20) X[[j]] <- log10(X[[j]])
grp <- ptype[colnames(X)]
# repeated random splits, forests and permutations give the uncertainty of each share (mean and 95% interval over NREP repeats)
NREP <- 15
run_once <- function(seed){
  set.seed(seed); te <- sample(seq_along(y), round(0.25 * length(y))); tr <- setdiff(seq_along(y), te)
  fit <- ranger(x = X[tr, ], y = y[tr], num.trees = 300, min.node.size = 5, seed = seed)
  p0 <- predict(fit, X[te, ])$predictions; mse0 <- mean((y[te] - p0)^2); r2 <- 1 - sum((y[te] - p0)^2) / sum((y[te] - mean(y[te]))^2)
  perm <- function(cols){ Xp <- X[te, ]; Xp[, cols] <- Xp[sample(length(te)), cols]; mean((y[te] - predict(fit, Xp)$predictions)^2) - mse0 }
  inc_t <- sapply(types, function(t) mean(replicate(3, perm(names(grp)[grp == t]))))
  inc_p <- sapply(colnames(X), perm)
  message("repeat ", seed, " done")
  list(r2 = r2, t = 100 * inc_t / sum(inc_t), p = 100 * inc_p / sum(inc_t))
}
rds <- file.path(pr, "ts", "param_importance_runs.rds")   # cached; delete it to refit
runs <- if(file.exists(rds)) readRDS(rds) else { r <- lapply(seq_len(NREP), run_once); saveRDS(r, rds); r }
r2 <- mean(sapply(runs, function(r) r$r2))
Tm <- sapply(runs, function(r) r$t); Pm <- sapply(runs, function(r) r$p)
q <- function(M, pr) apply(M, 1, quantile, probs = pr)
G <- data.frame(type = types, n = as.numeric(table(factor(grp, levels = types))), share = rowMeans(Tm), lo = q(Tm, 0.025), hi = q(Tm, 0.975))
G$per_par <- G$share / G$n
P <- data.frame(par = colnames(X), type = grp, share = rowMeans(Pm), lo = q(Pm, 0.025), hi = q(Pm, 0.975), row.names = NULL)
message("mean held-out R2 = ", round(r2, 2)); print(G)

cols <- c("Vulnerabilities" = "#937860", "Red tide mortality" = "#C44E52", "Red tide foraging" = "#E8A0A2", "Environmental curves" = "#55A868", "Fleet" = "#8172B3", "Dispersal" = "#DA8BC3")   # kept apart from the series palette (blue, orange, grey, gold) and the ensemble colours
th_p <- theme_bw(base_size = 18) + theme(strip.background = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7), axis.title = element_text(face = "bold"),
        axis.text = element_text(colour = "black"), legend.position = "none",
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.4), panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.4, linetype = "dashed"))
G$type <- factor(G$type, levels = rev(G$type[order(-G$share)]))
G$lab <- paste0(sprintf("%.0f", G$share), "%")
G$tcol <- as.character(G$type); lev <- levels(G$type); sh <- function(x) sub("Vulnerabilities", "Vuln.", sub("Environmental curves", "Env.", sub("Red tide foraging", "RT for.", sub("Red tide mortality", "RT mort.", x))))
G$type <- factor(sh(paste0(G$type, " (", G$n, ")")), levels = sh(paste0(lev, " (", G$n[match(lev, G$type)], ")")))
th_dot <- theme(panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.4))
pE <- ggplot(G, aes(share, type, colour = tcol)) + geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 1.3) +
  geom_point(size = 5) + geom_text(aes(x = hi, label = lab), hjust = -0.35, size = 4.4, colour = "black") +
  scale_colour_manual(values = cols) + scale_x_continuous(limits = c(0, 68), breaks = c(0, 20, 40, 60), expand = expansion(mult = c(0.02, 0))) +
  labs(x = "Share of variation (%)", y = NULL) + th_p + th_dot
nice_p <- function(x){ x <- sub("^vul_(\\d+)_(\\d+)$", "Vuln. \\1-\\2", x)
  x <- sub("^flt_(\\d+)_(mult|pow)$", "Fleet \\1 \\2", x)
  x <- sub("^rt(\\d+)_type\\d+_(\\w+)\\.adj$", "RT \\1 \\2", x)
  x <- sub("inflection", "infl.", x)
  x <- sub("^env(\\d+)_type\\d+_(\\w+)\\.adj$", "Env. \\1 \\2", x)
  sub("^disp_(\\d+)$", "Dispersal \\1", x) }
Pt <- P[order(-P$share), ][1:10, ]; Pt$lab <- factor(nice_p(Pt$par), levels = rev(nice_p(Pt$par)))
pF <- ggplot(Pt, aes(share, lab, colour = type)) + geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 1.1) + geom_point(size = 4) +
  scale_colour_manual(values = cols, breaks = types) + scale_x_continuous(breaks = c(0, 10, 20, 30), expand = expansion(mult = c(0.02, 0.04))) +
  labs(x = "Share of variation (%)", y = NULL) + th_p + th_dot +
  theme(legend.position = "none")   # colours are explained in panel C


#---- C: stanza catch and fishing mortality, both species, in one panel ---------------------------------------
blk <- c("Gag.Catch" = 0, "Gag.Fishing mortality" = 7, "Red grouper.Catch" = 14, "Red grouper.Fishing mortality" = 21)
B$xp <- as.numeric(B$stanza) + blk[paste(B$species, B$measure, sep = ".")]
pC <- ggplot(B, aes(xp)) + geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = c(7, 14, 21), colour = "grey40") +
  geom_linerange(aes(ymin = lo, ymax = hi, colour = "Ensemble range"), linewidth = 2, alpha = 0.4, na.rm = TRUE) +
  geom_point(aes(y = emean, colour = "Ensemble mean"), size = 2.8, na.rm = TRUE) +
  geom_point(aes(y = delta, colour = "Best model"), shape = 16, size = 3.8) +
  annotate("text", x = c(3.5, 10.5, 17.5, 24.5), y = Inf, label = c("Gag catch", "Gag F", "Red grouper catch", "Red grouper F"), vjust = 1.5, fontface = "bold", size = 4.6) +
  ens_scale + scale_x_continuous(breaks = c(1:6, 8:13, 15:20, 22:27), labels = rep(c("0", "1", "2", "3", "4", "5+"), 4), expand = expansion(add = 0.6)) +
  scale_y_continuous(trans = symlog, breaks = c(-1000, -100, -10, 0, 10), labels = c("-1,000", "-100", "-10", "0", "10"), expand = expansion(mult = c(0.05, 0.2))) +
  labs(x = "Stanza (age)", y = "Change in NLL") + th +
  theme(axis.text.x = element_text(size = 12, colour = "black"), legend.position = "none", panel.grid.major.x = element_blank())

#---- D: parameter types and individual parameters joined -----------------------------------------------------
Gs <- G[order(-G$share), ]; Gs$y <- 17 - (seq_len(nrow(Gs)) - 1); Gs$lab_y <- as.character(Gs$type); Gs$col <- Gs$tcol
Ps <- Pt[order(-Pt$share), ]; Ps$y <- 10 - (seq_len(nrow(Ps)) - 1); Ps$lab_y <- as.character(Ps$lab); Ps$col <- as.character(Ps$type)
DD <- rbind(data.frame(y = Gs$y, lab_y = Gs$lab_y, share = Gs$share, lo = Gs$lo, hi = Gs$hi, col = Gs$col, txt = Gs$lab),
            data.frame(y = Ps$y, lab_y = Ps$lab_y, share = Ps$share, lo = Ps$lo, hi = Ps$hi, col = Ps$col, txt = NA_character_))
pEF <- ggplot(DD, aes(share, y, colour = col)) + geom_hline(yintercept = 12.3, colour = "grey40") +
  geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 1.2, orientation = "y") + geom_point(size = 4.2) +
  geom_text(aes(x = hi, label = txt), hjust = -0.35, size = 4.4, colour = "black", na.rm = TRUE) +
  annotate("text", x = 0.3, y = c(18.5, 11.6), label = c("Parameter types", "Parameters"), hjust = 0, fontface = "bold", size = 4.8, colour = "black") +
  scale_colour_manual(values = cols, guide = "none") +
  scale_y_continuous(breaks = DD$y, labels = DD$lab_y, limits = c(0.4, 19.3), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 56), breaks = c(0, 10, 20, 30, 40, 50), expand = expansion(mult = c(0.02, 0))) +
  labs(x = "Share of NLL variation (%)", y = NULL) + th_p + th_dot

#---- C (final): biomass indices (summed by stanza pool), catch and F; percent change of baseline NLL -------------------
ser$Pool <- ser$Poolcode
sp_of <- function(id) ifelse(grepl("^GAG", id), "Gag", "Red grouper")
cl <- function(id) if(grepl("(_catch|_relCatch)$", id)) "Catch" else if(grepl("_F$", id)) "Fishing mortality" else "Biomass"
CC <- data.frame(id = ser$Title, pool = ser$Pool, base = ser$base, best = ser$best, stringsAsFactors = FALSE)
CC <- CC[grepl("^(GAG|REDGRP)", CC$id), ]
CC$species <- sp_of(CC$id); CC$kind <- vapply(CC$id, cl, "")
ix <- match(CC$id, mem_all$id)
CC$mean <- mem_all$member_mean[ix]; CC$min <- mem_all$member_min[ix]; CC$max <- mem_all$member_max[ix]
CC$stz <- ifelse(CC$species == "Gag", CC$pool - 4, CC$pool - 10)          # pools 4-9 (gag) and 10-15 (red grouper) are the six stanzas
# catch and F: one series per stanza, with the ensemble range. Biomass: indices of a stanza pool are summed (no range for sums)
isb <- CC$kind == "Biomass"
bio <- aggregate(cbind(base, best, mean) ~ species + stz + kind, CC[isb, ], sum); bio$min <- NA_real_; bio$max <- NA_real_
# exact ensemble range of the pooled biomass: sum the indices of the stanza within each member, then take min and max over members.
# Needs 06_member_series_matrix.csv (written by section 6 of ga_discussion_checks.R); without it the biomass points have no range.
mpath <- file.path(pr, "discussion_checks", "06_member_series_matrix.csv")
if(file.exists(mpath)){
  Sm <- read.csv(mpath, check.names = FALSE); Sm$member <- NULL; sid <- sub("\\|.*$", "", names(Sm)); cb <- CC[isb, ]
  for(i in seq_len(nrow(bio))){
    ids_i <- cb$id[cb$species == bio$species[i] & cb$stz == bio$stz[i]]
    tot <- rowSums(Sm[, sid %in% ids_i, drop = FALSE])
    bio$min[i] <- min(tot); bio$max[i] <- max(tot)
    message(bio$species[i], " stanza ", bio$stz[i], " biomass: mean of members ", round(mean(tot), 1), " vs table mean ", round(bio$mean[i], 1))
  }
}
oth <- CC[!isb, c("species", "stz", "kind", "base", "best", "mean", "min", "max")]
CC <- rbind(bio[, names(oth)], oth)
CC$best_p <- 100 * (CC$best - CC$base) / base_gr; CC$mean_p <- 100 * (CC$mean - CC$base) / base_gr
CC$lo_p <- 100 * (CC$min - CC$base) / base_gr; CC$hi_p <- 100 * (CC$max - CC$base) / base_gr
kinds <- c("Biomass", "Catch", "Fishing mortality")
make_c <- function(sp, ylab = TRUE){
  d <- CC[CC$species == sp, ]
  d$xp <- (match(d$kind, kinds) - 1) * 6.5 + d$stz + 1   # three blocks of equal width, equal margin each side of the six stanzas
  bs <- (0:2) * 6.5 + 1
  ggplot(d, aes(xp)) + geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = c(6.75, 13.25), colour = "grey30") +
    geom_point(aes(y = best_p, colour = "Best model"), shape = 16, size = 4.6) +
    geom_errorbar(aes(ymin = lo_p, ymax = hi_p), colour = "black", width = 0.4, linewidth = 0.5, na.rm = TRUE) +
    annotate("text", x = bs + 2.5, y = Inf, label = c("Biomass", "Catch", "F"), vjust = 1.4, fontface = "bold", size = 5.4) +
    ens_scale + scale_x_continuous(breaks = unlist(lapply(bs, function(st) st + 0:5)), labels = rep(c("0", "1", "2", "3", "4", "5+"), 3), limits = c(0.25, 19.75), expand = c(0, 0)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0.05, 0.12)), limits = c(-33, 4)) +
    labs(title = sp, x = "Stanza (age)", y = "Change in NLL\n(% of total)") + th +
    theme(plot.title = element_text(face = "bold", size = 19, hjust = 0.5), legend.position = "none", panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.4, linetype = "dashed"),
          axis.text.x = element_text(size = 11.5, colour = "black"))
}
pCg <- make_c("Gag"); pCr <- make_c("Red grouper", ylab = FALSE)

#---- D (final): parameter types only ---------------------------------------------------------------------------------
# total NLL of gag + red grouper (46 series + four focal maps): baseline, best, and each ensemble member
base_gr <- sum(A2$base); best_gr <- sum(A2$best)
mem_gr <- Reduce(`+`, lapply(c(ptypes), function(ty) mem_sum(gag | red, ty))) + mem_sp(c("gag 2-4", "gag 5+", "red grouper 3-4", "red grouper 5+"))
tot_pct <- 100 * c(best = best_gr, mean = mean(mem_gr), lo = min(mem_gr), hi = max(mem_gr)) / base_gr - 100 * 1   # (x - base) / base in percent
tot_pct <- 100 * (c(best = best_gr, mean = mean(mem_gr), lo = min(mem_gr), hi = max(mem_gr)) - base_gr) / base_gr
message("gag + red grouper total NLL: baseline ", round(base_gr), ", best ", round(best_gr), " (", round(tot_pct["best"], 1), "%), ensemble mean ", round(tot_pct["mean"], 1), "%, range ", round(tot_pct["lo"], 1), " to ", round(tot_pct["hi"], 1), "%")
Gb <- G[order(-G$share), ]
Gb$v <- Gb$share / 100 * tot_pct["best"]; Gb$vm <- Gb$share / 100 * tot_pct["mean"]; Gb$vlo <- Gb$share / 100 * tot_pct["lo"]; Gb$vhi <- Gb$share / 100 * tot_pct["hi"]   # total change allocated by sensitivity share
Gb$pp <- ifelse(Gb$per_par < 0.1, sprintf("%.2f", Gb$per_par), sprintf("%.1f", Gb$per_par))   # share per parameter, so the effect of group size is visible
Gb$lab <- gsub("RT mort\\.", "RT\nmort.", gsub("RT for\\.", "RT\nfor.", sub(" \\(", "\n(", as.character(Gb$type))))
Gb$xl <- factor(Gb$lab, levels = Gb$lab)
Gb$txt <- ifelse(abs(Gb$v) < 0.5, "0%", sprintf("%.0f%%", Gb$v))   # avoids "-0%"
write.csv(Gb[, c("type", "n", "share", "v", "vm", "vlo", "vhi")], file.path(pr, "ts", "Fig_contribution_B_allocation.csv"), row.names = FALSE)
pEF <- ggplot(Gb, aes(xl)) + geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(y = v), colour = col_best, shape = 16, size = 5) +
  geom_errorbar(aes(ymin = vlo, ymax = vhi), colour = "black", width = 0.15, linewidth = 0.6) +
  geom_text(aes(y = vlo, label = txt), vjust = 1.8, size = 6, colour = "black") +
  geom_text(aes(y = 4.3, label = pp), size = 5.2, colour = "grey30") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-26, 6), expand = c(0, 0), breaks = seq(-25, 0, 5)) +
  labs(x = NULL, y = "Change in NLL\n(% of total)") + th_p + th_dot + theme(axis.text.x = element_text(size = 13, colour = "black", lineheight = 0.9), panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.4, linetype = "dashed"), panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.4))


W <- function(p) p   # plain patchwork: panel areas are aligned across rows and columns
TWO_PANEL <- TRUE   # TRUE: only A and B (the stanza detail of C and D is given in the text); FALSE: four panels
fig <- if(TWO_PANEL) (W(pA) + W(pEF) + plot_layout(design = "AB", widths = c(1, 1))) + plot_annotation(tag_levels = list(c("A", "B"))) & theme(plot.tag = element_text(face = "bold", size = 28)) else
  (W(pA) + W(pEF) + W(pCg) + W(pCr) + plot_layout(design = "AB\nCD", widths = c(1, 1), heights = c(1, 1))) + plot_annotation(tag_levels = list(c("A", "B", "C", "D"))) & theme(plot.tag = element_text(face = "bold", size = 28))
FIG_H <- if(TWO_PANEL) 4.9 else 8.4
dir.create(file.path(pr, "ts"), showWarnings = FALSE, recursive = TRUE)
tag <- if(EXCLUDE_STANZA_BIOMASS) "" else "_all58"
ggsave(file.path(pr, "ts", paste0("Fig_contribution_of_components", tag, ".pdf")), fig, width = 11, height = FIG_H, device = cairo_pdf)
# PNG made from the PDF so both carry the same typography
if(requireNamespace("pdftools", quietly = TRUE)){ pdftools::pdf_convert(file.path(pr, "ts", paste0("Fig_contribution_of_components", tag, ".pdf")), format = "png", dpi = 300, filenames = file.path(pr, "ts", paste0("Fig_contribution_of_components", tag, ".png")), verbose = FALSE)
} else ggsave(file.path(pr, "ts", paste0("Fig_contribution_of_components", tag, ".png")), fig, width = 11, height = FIG_H, dpi = 300, bg = "white")


write.csv(CC, file.path(pr, "ts", paste0("Fig_contribution_CD_stanza", tag, ".csv")), row.names = FALSE)
write.csv(rbind(Ad), file.path(pr, "ts", paste0("Fig_contribution_A_change", tag, ".csv")), row.names = FALSE)
write.csv(A, file.path(pr, "ts", paste0("Fig_contribution_A_partition", tag, ".csv")), row.names = FALSE)
write.csv(B, file.path(pr, "ts", paste0("Fig_contribution_B_stanza", tag, ".csv")),    row.names = FALSE)
write.csv(C[, c("map", "baseline", "best", "delta", "pct", "emean", "lo", "hi")], file.path(pr, "ts", paste0("Fig_contribution_maps", tag, ".csv")), row.names = FALSE)
write.csv(D[, c("id", "baseline", "best_series", "member_min", "member_mean", "member_max")], file.path(pr, "ts", paste0("Fig_contribution_D_series", tag, ".csv")), row.names = FALSE)
print(A)
write.csv(G, file.path(pr, "ts", "Fig_parameter_type_E.csv"), row.names = FALSE)
write.csv(P[order(-P$share), ], file.path(pr, "ts", "Fig_parameter_type_F_all_parameters.csv"), row.names = FALSE)
