#---------------------------------------------------------------------------------------------------------------
# Figure: contribution of components to the fit (phase6 ga01), manuscript Fig. X
#   A  composition of the best-model NLL (temporal vs spatial, species, type of series, focal maps)
#   B  change in total NLL by species and type of series (biomass indices, catch, F, spatial), best model and ensemble range,
#      as a percentage of the baseline NLL of the gag and red grouper data (46 scored series + four focal maps)
#   C  the same total change allocated to each type of parameter by its share of the NLL variation across the GA evaluations
#      (random forest, grouped permutation importance; descriptive), with the parameters at a search bound out of all of each type
#
# The stanza biomass series (GAG_age1..6_biomass, REDGRP_age1..6_biomass) come from the stock assessments and are treated as
# reference series: they are EXCLUDED here (EXCLUDE_STANZA_BIOMASS = TRUE). Set it to FALSE to get the full 58-series objective
# that the GA actually minimized (baseline about 11,100, best 6,459); files then get the suffix _all58.
#
# Sourced from 'st calibration run phase6.R' (section 7) or run on its own. Needs ggplot2, patchwork, ranger (and pdftools for the PNG).
# Inputs: plots_R/nll_temporal_by_series.csv, nll_spatial_by_map.csv (section 1 of 'st results phase6.R'),
#   plots_R/discussion_checks/06_member_series_matrix.csv, 06_member_totals_and_maps.csv (section 3.1 of 'st results phase6.R'),
#   results/parvec_ga01.csv, ga_evals_ga01.csv, final_ga_popga01.csv, final_ga_runs_ga01.csv
# Output (plots_R/ts): Fig_contribution_of_components.png/.pdf (PNG rendered from the PDF so both carry the same typography)
#   and the csv files with every number plotted: Fig_contribution_A_best_ensemble.csv, Fig_contribution_B_allocation.csv,
#   Fig_contribution_stanza_detail.csv (change by stanza, quoted in the Results text), Fig_parameter_type_*.csv
#---------------------------------------------------------------------------------------------------------------
library(ggplot2); library(patchwork); library(ranger)

STRIPS <- if(exists("STRIPS")) STRIPS else FALSE   # TRUE: add the share-of-remaining-NLL strips above each panel (old layout)
RUN <- if(exists("RUN")) RUN else Sys.getenv("EWE_RUN", "/Users/daniel/Work/ST_calibration/phase6_ga01")   # uses RUN from the main script when sourced
EXCLUDE_STANZA_BIOMASS <- TRUE
N_ENS <- 37                                    # ensemble = best 10% of the final population
NREP  <- 15                                    # repeated random forests for the uncertainty of the shares
pr  <- file.path(RUN, "plots_R"); dir.create(file.path(pr, "ts"), showWarnings = FALSE, recursive = TRUE)
tag <- if(EXCLUDE_STANZA_BIOMASS) "" else "_all58"

#---- data ---------------------------------------------------------------------------------------------------
ser <- read.csv(file.path(pr, "nll_temporal_by_series.csv"), check.names = FALSE)
spm <- read.csv(file.path(pr, "nll_spatial_by_map.csv"),      check.names = FALSE)
ms  <- read.csv(file.path(pr, "discussion_checks", "06_member_totals_and_maps.csv"), check.names = FALSE)   # one row per ensemble member
Smx <- read.csv(file.path(pr, "discussion_checks", "06_member_series_matrix.csv"), check.names = FALSE); Smx$member <- NULL
sid <- sub("\\|.*$", "", names(Smx))           # series id of each column of the member x series matrix
is_stanza_bio <- function(x) grepl("^(GAG|REDGRP)_age[1-6]_biomass$", sub("\\|.*$", "", x))
if(EXCLUDE_STANZA_BIOMASS) ser <- ser[!is_stanza_bio(ser$Title), ]
ser$base <- ser$loglik.baseline; ser$best <- ser$loglik.best
message("series used: ", nrow(ser), " | temporal NLL baseline ", round(sum(ser$base), 1), ", best ", round(sum(ser$best), 1))
gag <- grepl("^GAG", ser$Title); red <- grepl("^REDGRP", ser$Title)
stype <- ifelse(grepl("(_catch|_relCatch|SSrelCatch)$", ser$Title), "Catch", ifelse(grepl("_F$", ser$Title), "Fishing mortality", "Survey indices"))
ptypes <- c("Survey indices", "Catch", "Fishing mortality")
maps_gag <- c("gag 2-4", "gag 5+"); maps_red <- c("red grouper 3-4", "red grouper 5+")

# palette from the manuscript figures: best model = turquoise
col_best <- "#20B2AA"
th <- theme_bw(base_size = 15) +
  theme(strip.background = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 0.55),
        axis.title = element_text(face = "bold"), axis.text = element_text(colour = "black"), legend.position = "none",
        panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.4),
        panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.4, linetype = "dashed"))

#---- A: change by species and type of series ----------------------------------------------------------------------
mapsum  <- function(keys, col) sum(spm[[col]][spm$map %in% keys])
tb      <- function(sel, ty) sum(ser$base[sel & stype == ty])
mem_sum <- function(sel, ty) rowSums(Smx[, sid %in% ser$Title[sel & stype == ty], drop = FALSE])   # NLL of the type in each member
mem_sp  <- function(keys) rowSums(ms[, keys, drop = FALSE])
mkA <- function(sp, sel, keys){
  do.call(rbind, lapply(c(ptypes, "Spatial"), function(ty){
    if(ty == "Spatial"){ m <- mem_sp(keys); b0 <- mapsum(keys, "baseline"); bt <- mapsum(keys, "best") }
    else { m <- mem_sum(sel, ty); b0 <- tb(sel, ty); bt <- sum(ser$best[sel & stype == ty]) }
    data.frame(species = sp, type = ty, base = b0, best = bt, mean = mean(m), min = min(m), max = max(m)) })) }
A2 <- rbind(mkA("Gag", gag, maps_gag), mkA("Red grouper", red, maps_red))
A2$part <- factor(A2$type, levels = c(ptypes, "Spatial"), labels = c("Biomass", "Catch", "F", "Spatial"))
base_gr <- sum(A2$base)   # one common baseline for A and B: total NLL of the gag + red grouper fitted data, so the points add up to the total change
for(v in c("best", "mean", "min", "max")) A2[[paste0(v, "_p")]] <- 100 * (A2[[v]] - A2$base) / base_gr
A2$xa <- (as.integer(A2$part) - 1) * 3 + ifelse(A2$species == "Gag", 0.85, 2.15)
write.csv(A2, file.path(pr, "ts", "Fig_contribution_A_best_ensemble.csv"), row.names = FALSE)
pA <- ggplot(A2, aes(xa)) + geom_hline(yintercept = 0, linetype = "dashed") + geom_vline(xintercept = c(3, 6, 9), colour = "grey30") +
  geom_errorbar(aes(ymin = min_p, ymax = max_p), colour = "black", width = 0.4, linewidth = 0.5) +
  geom_point(aes(y = best_p), fill = col_best, colour = "grey15", shape = 21, size = 1.9, stroke = 0.4) +
  annotate("text", x = (0:3) * 3 + 1.5, y = Inf, label = c("Biomass", "Catch", "F", "Spatial"), vjust = 1.4, fontface = "bold", size = 4.1) +
  scale_x_continuous(breaks = as.vector(outer(c(0.85, 2.15), (0:3) * 3, "+")), labels = rep(c("Gag", "Red"), 4), limits = c(0, 12), expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-45, 15), breaks = seq(-40, 10, 10), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +   # clip off so the panel border has the same width as in B
  labs(x = NULL, y = "Change in NLL\n(% of baseline total)") + th +
  theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5), axis.text.x = element_text(size = 10.7))

#---- change by stanza (table only, quoted in the Results text) -------------------------------------------------------
# biomass indices of a stanza pool are summed (also within each member, for the range); catch and F are one series per stanza
CC <- data.frame(id = ser$Title, pool = ser$Poolcode, base = ser$base, best = ser$best, stringsAsFactors = FALSE)
CC <- CC[grepl("^(GAG|REDGRP)", CC$id), ]
CC$species <- ifelse(grepl("^GAG", CC$id), "Gag", "Red grouper")
CC$kind <- ifelse(grepl("(_catch|_relCatch)$", CC$id), "Catch", ifelse(grepl("_F$", CC$id), "Fishing mortality", "Biomass"))
CC$stz <- ifelse(CC$species == "Gag", CC$pool - 4, CC$pool - 10)          # pools 4-9 (gag) and 10-15 (red grouper) are the six stanzas
CC$mem_mean <- NA_real_; CC$mem_min <- NA_real_; CC$mem_max <- NA_real_
grp_key <- paste(CC$species, CC$stz, CC$kind)
stz <- do.call(rbind, lapply(unique(grp_key), function(k){
  d <- CC[grp_key == k, ]; tot <- rowSums(Smx[, sid %in% d$id, drop = FALSE])
  data.frame(species = d$species[1], stanza = d$stz[1], kind = d$kind[1], base = sum(d$base), best = sum(d$best),
             ens_mean = mean(tot), ens_min = min(tot), ens_max = max(tot)) }))
for(v in c("best", "ens_mean", "ens_min", "ens_max")) stz[[paste0(v, "_pct_of_total")]] <- 100 * (stz[[v]] - stz$base) / base_gr
write.csv(stz, file.path(pr, "ts", "Fig_contribution_stanza_detail.csv"), row.names = FALSE)

#---- B: contribution of parameter types to the NLL ------------------------------------------------------------------
# Random forest of log10(NLL) on the 98 parameters over the 11,471 GA evaluations; grouped permutation importance.
# Evaluations are not independent (the GA samples near good regions), so the shares are descriptive.
par_tab <- read.csv(file.path(RUN, "results", "parvec_ga01.csv"), check.names = FALSE); names(par_tab)[1] <- "par"
e <- read.csv(file.path(RUN, "results", "ga_evals_ga01.csv"), check.names = FALSE)
types <- c("Vulnerabilities", "Red tide mortality", "Red tide foraging", "Environmental curves", "Fleet", "Dispersal")
rt_id <- suppressWarnings(as.integer(sub("^rt(\\d+)_.*", "\\1", par_tab$par)))   # red tide response ids: up to 53 mortality (M0), 54-67 foraging
ptype <- ifelse(grepl("^vul_", par_tab$par), types[1], ifelse(grepl("^rt", par_tab$par), ifelse(rt_id <= 53, types[2], types[3]),
         ifelse(grepl("^env", par_tab$par), types[4], ifelse(grepl("^flt_", par_tab$par), types[5], types[6])))); names(ptype) <- par_tab$par
X <- e[, par_tab$par]; ok <- complete.cases(X) & is.finite(e$fitness); X <- X[ok, ]; y <- log10(e$fitness[ok])
for(j in names(X)) if(min(X[[j]]) > 0 && max(X[[j]]) / min(X[[j]]) > 20) X[[j]] <- log10(X[[j]])
grp <- ptype[colnames(X)]
run_once <- function(seed){   # one random split, forest and set of permutations
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
rf_runs <- if(file.exists(rds)) readRDS(rds) else NULL
if(!is.null(rf_runs) && (length(rf_runs[[1]]$t) != length(types) || length(rf_runs) != NREP)) rf_runs <- NULL   # stale cache from an older parameter grouping: refit
if(is.null(rf_runs)) { r <- lapply(seq_len(NREP), run_once); saveRDS(r, rds); rf_runs <- r }
r2 <- mean(sapply(rf_runs, function(r) r$r2))
Tm <- sapply(rf_runs, function(r) r$t); Pm <- sapply(rf_runs, function(r) r$p)
q <- function(M, p) apply(M, 1, quantile, probs = p)
G <- data.frame(type = types, n = as.numeric(table(factor(grp, levels = types))), share = rowMeans(Tm), lo = q(Tm, 0.025), hi = q(Tm, 0.975))
G$per_par <- G$share / G$n
P <- data.frame(par = colnames(X), type = grp, share = rowMeans(Pm), lo = q(Pm, 0.025), hi = q(Pm, 0.975), row.names = NULL)
message("mean held-out R2 = ", round(r2, 2)); print(G)
write.csv(G, file.path(pr, "ts", "Fig_parameter_type_E.csv"), row.names = FALSE)
write.csv(P[order(-P$share), ], file.path(pr, "ts", "Fig_parameter_type_F_all_parameters.csv"), row.names = FALSE)

# total change of the gag + red grouper data (46 series + four focal maps): best model and each ensemble member
best_gr <- sum(A2$best)
mem_gr <- Reduce(`+`, lapply(ptypes, function(ty) mem_sum(gag | red, ty))) + mem_sp(c(maps_gag, maps_red))
tot_pct <- 100 * (c(best = best_gr, mean = mean(mem_gr), lo = min(mem_gr), hi = max(mem_gr)) - base_gr) / base_gr
message("gag + red grouper total NLL: baseline ", round(base_gr), ", best ", round(best_gr), " (", round(tot_pct["best"], 1), "%), ensemble mean ",
        round(tot_pct["mean"], 1), "%, range ", round(tot_pct["lo"], 1), " to ", round(tot_pct["hi"], 1), "%")
G$type <- factor(G$type, levels = G$type[order(-G$share)])
sh <- function(x) c(Vulnerabilities = "Vul", `Environmental curves` = "Env fxn", `Red tide foraging` = "RT forag", `Red tide mortality` = "RT mort", Fleet = "Fleet mult", Dispersal = "Disp")[x]
Gb <- G[order(-G$share), ]
Gb$v   <- Gb$share / 100 * tot_pct["best"]                                           # share x total change of the best model
Gb$vm  <- Gb$share / 100 * tot_pct["mean"]; Gb$vlo <- Gb$share / 100 * tot_pct["lo"]; Gb$vhi <- Gb$share / 100 * tot_pct["hi"]   # bar = range of the ensemble members
# parameters at a search bound: median position of the ensemble members in the outer 5% of the range (log scale when the range spans more than a factor of 20)
pop_f <- read.csv(file.path(RUN, "results", "final_ga_popga01.csv"), check.names = FALSE); runs_f <- read.csv(file.path(RUN, "results", "final_ga_runs_ga01.csv"))
Xb <- as.matrix(pop_f[runs_f$gapop_row[order(runs_f$fitness)][seq_len(N_ENS)], par_tab$par]); Lb <- par_tab$L.bounds; Ub <- par_tab$U.bounds; lgb <- Ub / Lb > 20
posb <- sapply(seq_along(par_tab$par), function(j) if(lgb[j]) (log(Xb[, j]) - log(Lb[j])) / (log(Ub[j]) - log(Lb[j])) else (Xb[, j] - Lb[j]) / (Ub[j] - Lb[j]))
medb <- apply(posb, 2, median); edge_b <- tapply(medb < 0.05 | medb > 0.95, ptype[par_tab$par], sum)
Gb$edge <- as.numeric(edge_b[as.character(Gb$type)])
# axis names on two lines; the count "at a bound / total" is drawn at the top, centred (red = at a bound)
Gb$lab <- unname(sub(" ", "\n", sh(as.character(Gb$type)), fixed = TRUE))   # two-line labels: RT/mort, Fleet/mult, Env/fxn, RT/forag; Vul and Disp one line
Gb$lab <- ifelse(grepl("\n", Gb$lab), Gb$lab, paste0(Gb$lab, "\n "))
Gb$xl <- factor(Gb$lab, levels = Gb$lab)
Gb$we <- 0.2 * nchar(as.character(Gb$edge)); Gb$wn <- 0.12 + 0.19 * nchar(as.character(Gb$n))   # text widths in axis units
Gb$xe <- seq_len(nrow(Gb)) - (Gb$we + Gb$wn) / 2; Gb$xn <- Gb$xe + Gb$we
write.csv(Gb[, c("type", "n", "edge", "share", "v", "vm", "vlo", "vhi")], file.path(pr, "ts", "Fig_contribution_B_allocation.csv"), row.names = FALSE)
pB <- ggplot(Gb, aes(xl)) + geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = vlo, ymax = vhi), colour = "black", width = 0.3, linewidth = 0.6) +
  geom_point(aes(y = v), fill = col_best, colour = "grey15", shape = 21, size = 1.9, stroke = 0.4) +
  geom_text(aes(x = xe, y = 5.4, label = edge), hjust = 0, size = 4.3, colour = "#C0392B", fontface = "bold") +
  geom_text(aes(x = xn, y = 5.4, label = paste0("/", n)), hjust = 0, size = 4.3, colour = "black") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(-25, 0, 5), expand = c(0, 0)) + scale_x_discrete(expand = expansion(add = 0.6)) +
  coord_cartesian(ylim = c(-26, 7), clip = "off") +
  labs(x = NULL, y = "Change in NLL\n(% of baseline total)") + th +
  theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5), axis.text.x = element_text(size = 10.7, colour = "black", lineheight = 0.9))

#---- Fig. composition: what the best-model NLL is made of ------------------------------------------------------------
# stacked bars, each row = 100% of the quantity named on the left (best model; the full objective: 46 series + all maps)
sp_best_total <- { pt <- file.path(pr, "nll_partition_temporal_vs_spatial.csv"); if(file.exists(pt)) read.csv(pt)$spatial_NLL[read.csv(pt)$model == "best"] else NA_real_ }
tb_best <- sum(ser$best)
age0 <- grepl("_AGE0_", ser$Title) & gag
fz <- ser$best
comp <- rbind(
  data.frame(row = "Total NLL", part = c("Temporal", "Spatial"), v = c(tb_best, sp_best_total)),
  data.frame(row = "Temporal", part = c("Gag", "Red grouper", "Other"), v = c(sum(fz[gag]), sum(fz[red]), sum(fz[!gag & !red]))),
  data.frame(row = "Temporal, gag", part = c("Age-0 indices", "Catch", "F", "Other"),
             v = c(sum(fz[age0]), sum(fz[gag & stype == "Catch"]), sum(fz[gag & stype == "Fishing mortality"]), sum(fz[gag & stype == "Survey indices" & !age0]))),
  data.frame(row = "Spatial (focal maps)", part = c("Gag 2-4", "Gag 5+", "Red grouper 3-4", "Red grouper 5+"),
             v = sapply(c("gag 2-4", "gag 5+", "red grouper 3-4", "red grouper 5+"), function(k) spm$best[spm$map == k])))
comp$pct <- 100 * ave(comp$v, comp$row, FUN = function(x) x / sum(x))
rows_ord <- c("Total NLL", "Temporal", "Temporal, gag", "Spatial (focal maps)")
comp$row <- factor(comp$row, levels = rev(rows_ord)); comp <- comp[comp$pct >= 0.5, ]
comp$part <- factor(comp$part, levels = unique(c("Temporal", "Spatial", "Gag", "Red grouper", "Age-0 indices", "Catch", "F", "Other", "Gag 2-4", "Gag 5+", "Red grouper 3-4", "Red grouper 5+")))
comp$grp <- paste(comp$row, comp$part)
comp$lab <- ifelse(comp$pct >= 3.5, sprintf("%s\n%.1f%%", comp$part, comp$pct), "")
pal <- c(Temporal = "#1B4F72", Spatial = "#E69F00", Gag = "#3A9D5D", `Red grouper` = "#C44E52", `Age-0 indices` = "#56B4E9", Catch = "#D55E00", F = "#999999", Other = "#B0C4DE",
         `Gag 2-4` = "#3A9D5D", `Gag 5+` = "#1F6B3A", `Red grouper 3-4` = "#C44E52", `Red grouper 5+` = "#8E2F33")
pComp <- ggplot(comp, aes(pct, row, fill = part)) + geom_col(width = 0.84, colour = "white", linewidth = 0.5, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = lab, size = pct < 8), position = position_stack(vjust = 0.5, reverse = TRUE), colour = "white", lineheight = 0.9) + scale_size_manual(values = c(`FALSE` = 3.0, `TRUE` = 2.4), guide = "none") +
  scale_fill_manual(values = pal, guide = "none") + scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(x = "Share of the best-model NLL within each row", y = NULL) + th + theme(panel.grid = element_blank(), panel.border = element_blank(), plot.margin = margin(5.5, 14, 5.5, 5.5), axis.line.x = element_line(colour = "black"))
comp$pct <- round(comp$pct, 1); write.csv(comp[, c("row", "part", "v", "pct")], file.path(pr, "ts", "Fig_composition_best_model.csv"), row.names = FALSE)
#---- combine and save: A share of the best-model NLL (top strip) and change from baseline (below), by species and type; B change allocated to parameter types ----
# share = best-model NLL of each species x type as a percentage of the best-model NLL of gag + red grouper (46 series + four focal maps; same basis as the change)
A2$share <- 100 * A2$best / sum(A2$best)
# ensemble: share of each member's own gag + red grouper NLL (best-model share = bar; ensemble mean = point; ensemble min-max = line)
mem_part <- function(sp, ty){ sel <- if(sp == "Gag") gag else red; if(ty == "Spatial") mem_sp(if(sp == "Gag") c("gag 2-4", "gag 5+") else c("red grouper 3-4", "red grouper 5+")) else mem_sum(sel, ty) }
for(i in seq_len(nrow(A2))){ m <- 100 * mem_part(A2$species[i], A2$type[i]) / mem_gr; A2$sh_mean[i] <- mean(m); A2$sh_lo[i] <- min(m); A2$sh_hi[i] <- max(m) }
write.csv(A2[, c("species", "part", "base", "best", "share", "best_p", "mean_p", "min_p", "max_p", "sh_mean", "sh_lo", "sh_hi")], file.path(pr, "ts", "Fig_contribution_A_best_ensemble.csv"), row.names = FALSE)
# all candidates of the final population (grey points; written by section 3.1 of 'st results phase6.R'): same quantities as the ensemble
f_ps <- file.path(pr, "discussion_checks", "06_population_series_matrix.csv"); f_pm <- file.path(pr, "discussion_checks", "06_population_maps.csv")
pop_pts <- NULL
if(file.exists(f_ps) && file.exists(f_pm)){
  Ps <- read.csv(f_ps, check.names = FALSE); Pmp <- read.csv(f_pm, check.names = FALSE); rank_p <- Ps$rank; fit_p <- Ps$fitness; Ps$rank <- NULL; Ps$fitness <- NULL; Pmp$rank <- NULL; Pmp$fitness <- NULL
  sid_p <- sub("\\|.*$", "", names(Ps))
  part_p <- function(sp, ty){ sel <- if(sp == "Gag") gag else red
    if(ty == "Spatial") rowSums(Pmp[, if(sp == "Gag") maps_gag else maps_red, drop = FALSE]) else rowSums(Ps[, sid_p %in% ser$Title[sel & stype == ty], drop = FALSE]) }
  vals <- sapply(seq_len(nrow(A2)), function(i) part_p(A2$species[i], A2$type[i]))     # models x (species, type)
  tot_p <- rowSums(vals)
  pop_pts <- do.call(rbind, lapply(seq_len(nrow(A2)), function(i) data.frame(xa = A2$xa[i], share = 100 * vals[, i] / tot_p,
                                      change = 100 * (vals[, i] - A2$base[i]) / base_gr, fit = fit_p, rk = rank_p)))
  pop_pts <- pop_pts[is.finite(pop_pts$share) & is.finite(pop_pts$change), ]
  if(!exists("ENS_PROP")) ENS_PROP <- 0.8
if(!exists("CONTRIB_ENSEMBLE_ONLY")) CONTRIB_ENSEMBLE_ONLY <- FALSE   # FALSE = all 370 final-generation models; TRUE = only the ensemble
fit_cut <- as.numeric(quantile(runs_f$fitness, ENS_PROP))   # ensemble = final-generation models with NLL at or below this quantile (same cut as every other ensemble figure)
if(isTRUE(CONTRIB_ENSEMBLE_ONLY)) pop_pts <- pop_pts[pop_pts$fit <= fit_cut, ]   # default: all final-generation models (NLL results); predictions use the ensemble
pop_pts <- pop_pts[order(-pop_pts$fit), ]   # draw the worst models first so the lowest NLL ends up on top
  message("population points: ", nrow(pop_pts) / nrow(A2), " models; total NLL worse than baseline: ", sum(tot_p > base_gr, na.rm = TRUE))
  write.csv(pop_pts, file.path(pr, "ts", "Fig_contribution_A_population.csv"), row.names = FALSE)
}
fit_scale <- NULL
if(!is.null(pop_pts)){
  lo_f <- min(pop_pts$fit, na.rm = TRUE); hi_f <- as.numeric(quantile(pop_pts$fit, 0.95, na.rm = TRUE))
  fit_brk <- unique(c(lo_f, pretty(c(lo_f, hi_f), 4)[pretty(c(lo_f, hi_f), 4) > lo_f + 0.08 * (hi_f - lo_f) & pretty(c(lo_f, hi_f), 4) < hi_f - 0.08 * (hi_f - lo_f)], hi_f))   # drop round ticks that would touch the end labels
  fit_scale <- scale_colour_viridis_c(option = "plasma", direction = -1, limits = c(lo_f, hi_f), oob = scales::squish, name = "NLL",
                                      breaks = fit_brk, labels = function(x) format(round(x), big.mark = ""),
                                      guide = "none")   # the colour bar is drawn as its own panel (pBar) so that it has the height of the panels
}
A2$base_share <- 100 * A2$base / sum(A2$base)     # baseline model, same basis as the strip
col_sp <- c(Gag = "#3A9D5D", Red = "#C44E52"); A2$sp2 <- ifelse(A2$species == "Gag", "Gag", "Red")
xs <- scale_x_continuous(breaks = as.vector(outer(1:2, (0:3) * 3, "+")), labels = rep(c("Gag", "Red"), 4), limits = c(0, 12), expand = c(0, 0))
pS <- ggplot(A2, aes(xa, share, fill = sp2)) + geom_vline(xintercept = c(3, 6, 9), colour = "grey40") + geom_col(width = 0.8, colour = "grey15", linewidth = 0.3) +
  geom_segment(aes(x = xa - 0.45, xend = xa + 0.45, y = base_share, yend = base_share), colour = "black", linewidth = 0.9, linetype = "21") +
  geom_linerange(aes(ymin = sh_lo, ymax = sh_hi), colour = "black", linewidth = 0.6) +
  annotate("text", x = (0:3) * 3 + 1.5, y = Inf, label = c("Biomass", "Catch", "F", "Spatial"), vjust = 1.4, fontface = "bold", size = 4.4) +
  scale_fill_manual(values = col_sp, guide = "none") + xs +
  scale_y_continuous(limits = c(0, 85), breaks = c(0, 25, 50), labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  labs(x = NULL, y = "Share of total\nNLL (%)") + th +
  coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.4, linetype = "dashed"), plot.margin = margin(12, 5.5, 0, 12), axis.title.y = element_text(size = 12), panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 0.55))
pA2 <- pA + coord_cartesian(clip = "off") + theme(panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.4, linetype = "dashed")) + theme(axis.title.y = element_text(size = 12), plot.margin = margin(2, 5.5, 5.5, 5.5))
pS$layers <- c(if(!is.null(pop_pts)) list(geom_jitter(data = pop_pts[pop_pts$share <= 85, ], aes(x = xa, y = share, colour = fit), inherit.aes = FALSE, width = 0.28, height = 0, alpha = 0.7, size = 0.7)), pS$layers)
pA2$layers <- c(if(!is.null(pop_pts)) list(geom_jitter(data = pop_pts[pop_pts$change >= -45 & pop_pts$change <= 15, ], aes(x = xa, y = change, colour = fit), inherit.aes = FALSE, width = 0.28, height = 0, alpha = 0.7, size = 0.7)), pA2$layers)
if(STRIPS) pA2$layers <- pA2$layers[!vapply(pA2$layers, function(l) inherits(l$geom, "GeomText"), logical(1))]   # with strips the titles are drawn once, in the strip above
pB <- pB + theme(axis.title.y = element_text(size = 12))
pSB <- ggplot(Gb, aes(xl, share)) + geom_col(width = 0.7, fill = "grey55", colour = "grey15", linewidth = 0.3) +
  scale_y_continuous(limits = c(0, 62), breaks = c(0, 25, 50), labels = function(x) paste0(x, "%"), expand = c(0, 0)) + scale_x_discrete(expand = expansion(add = 0.6)) +
  labs(x = NULL, y = "Share of NLL\nvariation (%)") + th + coord_cartesian(clip = "off") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_line(colour = "grey80", linewidth = 0.4, linetype = "dashed"),
        plot.margin = margin(12, 5.5, 0, 5.5), axis.title.y = element_text(size = 12), panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 0.55))
pB2 <- pB + theme(plot.margin = margin(2, 5.5, 5.5, 5.5))
if(STRIPS){   # two rows per side: share of the remaining NLL (strip) above the change from baseline
  right <- (pSB / pB2) + plot_layout(heights = c(0.55, 1))
  left <- (pS / pA2) + plot_layout(heights = c(0.55, 1))
  fig <- (wrap_elements(full = left) + wrap_elements(full = right) + plot_layout(widths = c(1, 1))) + plot_annotation(tag_levels = list(c("A", "B"))) & theme(plot.tag = element_text(face = "bold", size = 23), plot.tag.position = c(0.01, 0.99))
  fig_h <- 5.2
} else {     # one panel per side: A = change from baseline by species x type; B = that change allocated to parameter types
  pA1 <- pA2 + theme(plot.margin = margin(14, 8, 5.5, 12)); pB1 <- pB2 + theme(plot.margin = margin(14, 8, 5.5, 8))
  if(!is.null(pop_pts)){
    # panel B (surrogate): a random forest on ALL evaluated parameter sets predicts the total NLL; for every final-population model the parameters of one type are
    # set back to the baseline values and the model is predicted again. Effect of the type = predicted NLL as calibrated - predicted NLL with the type reverted
    # (negative = the type's calibration lowered the NLL), as % of the baseline NLL of the full objective. One point per model and type, coloured by the model's NLL.
    log_cols <- colnames(X)[vapply(colnames(X), function(j){ v <- e[[j]][ok]; min(v) > 0 && max(v) / min(v) > 20 }, logical(1))]
    to_x <- function(M){ M <- as.data.frame(M[, colnames(X), drop = FALSE]); for(j in log_cols) M[[j]] <- log10(M[[j]]); M }
    fit_all <- ranger(x = X, y = y, num.trees = 500, min.node.size = 5, seed = 1)
    ord_f <- order(runs_f$fitness); Pf <- to_x(pop_f[runs_f$gapop_row[ord_f], ]); fit_f <- runs_f$fitness[ord_f]
    base_x <- to_x(setNames(as.data.frame(t(par_tab$est_par_vec)), par_tab$par)); base_full <- e$fitness[e$role == "base"][1]
    pred0 <- 10^predict(fit_all, Pf)$predictions
    message("surrogate vs. actual NLL of the ", nrow(Pf), " final models: r = ", round(cor(pred0, fit_f), 2), "; baseline NLL ", round(base_full))
    eff <- lapply(types, function(t){ cols <- names(grp)[grp == t]; Pr <- Pf; for(cn in cols) Pr[[cn]] <- base_x[[cn]]
      100 * (pred0 - 10^predict(fit_all, Pr)$predictions) / base_full })
    names(eff) <- types
    spp <- do.call(rbind, lapply(types, function(t) data.frame(type = t, xl = Gb$xl[match(t, as.character(Gb$type))], y = eff[[t]], fit = fit_f, rank = seq_along(fit_f))))
    if(isTRUE(CONTRIB_ENSEMBLE_ONLY)) spp <- spp[spp$fit <= fit_cut, ]
    write.csv(spp[, c("type", "rank", "fit", "y")], file.path(pr, "ts", "Fig_parameter_type_effect_surrogate.csv"), row.names = FALSE)
    lo_y <- floor(min(quantile(spp$y, 0.005), na.rm = TRUE) / 5) * 5; hi_y <- 5
    spp_in <- spp[is.finite(spp$y) & spp$y >= lo_y & spp$y <= hi_y, ]
    # ---- supplement: table of the parameter types and validation of the surrogate (held-out evaluations) ----
    tab_t <- data.frame(type = as.character(Gb$type), n_parameters = Gb$n, at_bound = Gb$edge, importance_share_pct = round(Gb$share, 1),
      effect_best_pct = sapply(as.character(Gb$type), function(t) round(eff[[t]][1], 1)), effect_median_pct = sapply(as.character(Gb$type), function(t) round(median(eff[[t]], na.rm = TRUE), 1)),
      effect_q10_pct = sapply(as.character(Gb$type), function(t) round(quantile(eff[[t]], 0.10, na.rm = TRUE), 1)), effect_q90_pct = sapply(as.character(Gb$type), function(t) round(quantile(eff[[t]], 0.90, na.rm = TRUE), 1)), row.names = NULL)
    write.csv(tab_t, file.path(pr, "ts", "Table_parameter_types.csv"), row.names = FALSE); print(tab_t)
    set.seed(2); te_v <- sample(seq_along(y), round(0.25 * length(y))); fit_v <- ranger(x = X[-te_v, ], y = y[-te_v], num.trees = 300, min.node.size = 5, seed = 2)
    val <- data.frame(obs = 10^y[te_v], pred = 10^predict(fit_v, X[te_v, ])$predictions)
    r2_v <- 1 - sum((log10(val$obs) - log10(val$pred))^2) / sum((log10(val$obs) - mean(log10(val$obs)))^2); gd <- val$obs < 8000; r_gd <- cor(val$pred[gd], val$obs[gd])
    lim_v <- range(c(val$obs, val$pred)); write.csv(val, file.path(pr, "ts", "Fig_surrogate_validation.csv"), row.names = FALSE)
    pV <- ggplot(val, aes(obs, pred)) + geom_abline(slope = 1, intercept = 0, linetype = "dashed") + geom_point(colour = "grey25", alpha = 0.35, size = 0.8) +
      annotate("text", x = lim_v[1], y = lim_v[2], hjust = 0, vjust = 1, size = 4, lineheight = 1.05,
               label = sprintf("Held-out R\u00b2 = %.2f (log10 NLL)\nr = %.2f for NLL < 8,000 (n = %d)", r2_v, r_gd, sum(gd))) +
      scale_x_log10(limits = lim_v) + scale_y_log10(limits = lim_v) + coord_fixed() + labs(x = "Simulated NLL", y = "Surrogate-predicted NLL") + th
    ggsave(file.path(pr, "ts", "Fig_surrogate_validation.png"), pV, width = 4.2, height = 4.2, dpi = 300, bg = "white")
    message("surrogate validation: held-out R2 = ", round(r2_v, 2), ", r (NLL < 8000) = ", round(r_gd, 2))
    spp_in <- spp_in[order(-spp_in$fit), ]   # worst first, lowest NLL on top
    pB1$layers <- pB1$layers[!vapply(pB1$layers, function(l) inherits(l$geom, "GeomPoint") || inherits(l$geom, "GeomText"), logical(1))]   # drop the allocation dot and the bound counts (re-added higher, above the points)
    pB1 <- pB1 + geom_text(data = Gb, aes(x = xe, y = 10, label = edge), inherit.aes = FALSE, hjust = 0, size = 4.3, colour = "#C0392B", fontface = "bold") +
      geom_text(data = Gb, aes(x = xn, y = 10, label = paste0("/", n)), inherit.aes = FALSE, hjust = 0, size = 4.3, colour = "black")
    # B: every final-population model (all 370) as one point per parameter type, coloured by its NLL (worst drawn first, lowest NLL on top); dot = best model
    lo_y <- floor(min(quantile(spp$y, 0.005, na.rm = TRUE), eff[[1]][1]) / 10) * 10
    spp_in <- spp[is.finite(spp$y) & spp$y >= lo_y & spp$y <= 6, ]; spp_in <- spp_in[order(-spp_in$fit), ]
    pB1$layers <- c(list(geom_point(data = spp_in, aes(x = xl, y = y, colour = fit), inherit.aes = FALSE, size = 0.8, alpha = 0.7, position = position_jitter(width = 0.25, height = 0, seed = 1))), pB1$layers)
    pB1 <- pB1 + geom_point(data = spp[spp$rank == 1, ], aes(x = xl, y = y), inherit.aes = FALSE, fill = col_best, colour = "grey15", shape = 21, size = 2.6, stroke = 0.4) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(lo_y, 0, 10), expand = c(0, 0)) + coord_cartesian(ylim = c(lo_y - 3, 15), clip = "off") +
      labs(y = "Estimated effect of the type\n(% of baseline NLL)")
    pA1 <- pA1 + fit_scale; pB1 <- pB1 + fit_scale
  }
  drop_eb <- function(pp){ pp$layers <- pp$layers[!vapply(pp$layers, function(l) inherits(l$geom, "GeomErrorbar"), logical(1))]; pp }   # no ensemble range bars: the coloured points show the spread
  pA1 <- drop_eb(pA1); pB1 <- drop_eb(pB1)
  if(!is.null(pop_pts)){   # NLL colour bar as a third panel with the same top / bottom space as A and B, so it spans exactly the panel height
    pBar <- ggplot(data.frame(y = seq(lo_f, hi_f, length.out = 200)), aes(0, y, fill = y)) + geom_raster() +
      scale_fill_viridis_c(option = "plasma", direction = -1, limits = c(lo_f, hi_f), guide = "none") +
      scale_x_continuous(limits = c(-0.5, 0.5), breaks = 0, labels = "X\nX", expand = c(0, 0)) +
      scale_y_continuous(position = "right", breaks = fit_brk, labels = function(x) format(round(x), big.mark = ""), expand = c(0, 0)) +
      coord_cartesian(clip = "off") + labs(x = NULL, y = "NLL") + th +
      theme(panel.grid = element_blank(), panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 0.55),
            axis.text.x = element_text(size = 10.7, colour = NA, lineheight = 0.9), axis.ticks.x = element_line(colour = NA),
            axis.title.y.right = element_text(size = 12, angle = 90), axis.title.y.left = element_blank(), plot.margin = margin(14, 5.5, 5.5, 8))
    fig <- (pA1 + pB1 + pBar + plot_layout(widths = c(1, 1, 0.06))) + plot_annotation(tag_levels = list(c("A", "B", ""))) & theme(plot.tag = element_text(face = "bold", size = 23), plot.tag.position = c(0.01, 0.99))
  } else
  fig <- (pA1 + pB1 + plot_layout(widths = c(1, 1))) + plot_annotation(tag_levels = list(c("A", "B"))) & theme(plot.tag = element_text(face = "bold", size = 23), plot.tag.position = c(0.01, 0.99))
  fig_h <- 3.6
}
f_pdf <- file.path(pr, "ts", paste0("Fig_contribution_of_components", tag, ".pdf")); f_png <- sub("\\.pdf$", ".png", f_pdf)
ggsave(f_pdf, fig, width = 9.5, height = fig_h, device = cairo_pdf)
if(requireNamespace("pdftools", quietly = TRUE)){ pdftools::pdf_convert(f_pdf, format = "png", dpi = 300, filenames = f_png, verbose = FALSE)
} else ggsave(f_png, fig, width = 9.5, height = fig_h, dpi = 300, bg = "white")

#---- supplement tables: (S-A) data series used in the calibration with their source; (S-B) the parameters that moved most, by type ------------------------------------
# S-A: one row per series. The source is read from the series name (SEDAR assessment index / assessment output / catch); check the full survey names before submission.
ser_all <- read.csv(file.path(pr, "nll_temporal_by_series.csv"), check.names = FALSE)
raw_in <- read.csv(file.path(RUN, "inputs", "ts_mice_v5_discards_ecospace_regions_sedar105.csv"), header = FALSE, colClasses = "character", check.names = FALSE)
ttl <- unlist(raw_in[raw_in[[1]] == "Title", -1]); wts <- suppressWarnings(as.numeric(raw_in[raw_in[[1]] == "Weight", -1])); names(wts) <- ttl
body_in <- raw_in[suppressWarnings(!is.na(as.numeric(raw_in[[1]]))), ]; yrs_in <- as.numeric(body_in[[1]])
yr_rng <- function(t){ v <- body_in[[which(raw_in[raw_in[[1]] == "Title", ] == t)[1]]]; ok <- !is.na(suppressWarnings(as.numeric(v))) & nzchar(v); if(any(ok)) paste(range(yrs_in[ok]), collapse = "-") else NA }
f_bio <- file.path(RUN, "output", "run_base", "Ecospace_Annual_Average_Biomass.csv")
grp_names <- if(file.exists(f_bio)){ l <- readLines(f_bio, warn = FALSE); gsub('"', "", strsplit(l[grep("^Year,", l)[1]], ",")[[1]][-1]) } else NULL
src_of <- function(t) ifelse(grepl("SEDAR105", t), "SEDAR 105 gag age-0 index", ifelse(grepl("SEDAR72", t), "SEDAR 72 gag index (SRFS)", ifelse(grepl("SEDAR88", t), "SEDAR 88 red grouper index",
          ifelse(grepl("SSrelCatch$", t), "Stock Synthesis catch (relative)", ifelse(grepl("^GAG_age", t), "Stock assessment output, gag (SEDAR 72)", ifelse(grepl("^REDGRP_age", t), "Stock assessment output, red grouper (SEDAR 88)", "other"))))))
kind_of <- c(BiomassRel = "Relative biomass index", Catches = "Catch", CatchesRel = "Catch (relative)", FishingMortalityRef = "Fishing mortality (reference)")
tabA <- data.frame(series = ser_all$Title, species = ifelse(grepl("^GAG", ser_all$Title), "Gag", ifelse(grepl("^REDGRP", ser_all$Title), "Red grouper", "Other group")),
  group = if(is.null(grp_names)) ser_all$Poolcode else grp_names[ser_all$Poolcode], data_type = unname(kind_of[ser_all$Type.name]), source_from_series_name = src_of(ser_all$Title),
  region = ser_all$Region, years = vapply(ser_all$Title, yr_rng, character(1)), n_years = ser_all$n, weight = round(wts[ser_all$Title], 2),
  scored_in_figure = !is_stanza_bio(ser_all$Title), NLL_baseline = round(ser_all$loglik.baseline, 1), NLL_best = round(ser_all$loglik.best, 1), row.names = NULL)
tabA <- tabA[order(tabA$species, tabA$data_type, tabA$series), ]
write.csv(tabA, file.path(pr, "ts", "Table_S_data_sources.csv"), row.names = FALSE)

# S-B: for the best model, the change of every parameter from its baseline value as a position within its search range (log scale when the range spans more than a factor of 20);
# the five parameters that moved most in each type, with their random-forest importance
posr <- function(v, j) if(lgb[j]) (log(v) - log(Lb[j])) / (log(Ub[j]) - log(Lb[j])) else (v - Lb[j]) / (Ub[j] - Lb[j])
bi <- which.min(runs_f$fitness); best_par <- unlist(pop_f[runs_f$gapop_row[bi], par_tab$par]); base_par <- par_tab$est_par_vec
pos_b <- sapply(seq_along(best_par), function(j) posr(best_par[j], j)); pos_0 <- sapply(seq_along(base_par), function(j) posr(base_par[j], j))
chg <- data.frame(parameter = par_tab$par, type = unname(ptype[par_tab$par]), baseline = signif(base_par, 4), best = signif(best_par, 4), lower_bound = Lb, upper_bound = Ub,
                  fold_change = ifelse(base_par > 0 & best_par > 0, round(best_par / base_par, 2), NA), shift_in_range_pct = round(100 * (pos_b - pos_0), 1),
                  position_best_pct = round(100 * pos_b, 1), at_bound = pos_b < 0.05 | pos_b > 0.95, importance_pct = round(P$share[match(par_tab$par, P$par)], 2), row.names = NULL)
tabB <- do.call(rbind, lapply(split(chg, chg$type), function(d) head(d[order(-abs(d$shift_in_range_pct)), ], 5)))
tabB <- tabB[order(match(tabB$type, as.character(Gb$type)), -abs(tabB$shift_in_range_pct)), ]
write.csv(tabB, file.path(pr, "ts", "Table_S_parameter_changes.csv"), row.names = FALSE)
write.csv(chg[order(chg$type, -abs(chg$shift_in_range_pct)), ], file.path(pr, "ts", "Table_S_parameter_changes_all.csv"), row.names = FALSE)
print(tabB)
