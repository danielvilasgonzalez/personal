#---------------------------------------------------------------------------------------------------------------
# Figure: contribution of components to the fit (phase6 ga01), manuscript Fig. X
#   A  change in total NLL by species and type of series (biomass indices, catch, F, spatial), best model and ensemble range,
#      as a percentage of the baseline NLL of the gag and red grouper data (46 scored series + four focal maps)
#   B  the same total change allocated to each type of parameter by its share of the NLL variation across the GA evaluations
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

RUN <- if(exists("RUN")) RUN else "/Users/daniel/Work/ST_calibration/phase6_ga01"   # uses RUN from the main script when sourced
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
  theme(strip.background = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.7),
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
  geom_point(aes(y = best_p), fill = col_best, colour = "grey15", shape = 21, size = 2.9, stroke = 0.5) +
  annotate("text", x = (0:3) * 3 + 1.5, y = Inf, label = c("Biomass", "Catch", "F", "Spatial"), vjust = 1.4, fontface = "bold", size = 4.1) +
  scale_x_continuous(breaks = as.vector(outer(c(0.85, 2.15), (0:3) * 3, "+")), labels = rep(c("Gag", "Red"), 4), limits = c(0, 12), expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-45, 15), breaks = seq(-40, 10, 10), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +   # clip off so the panel border has the same width as in B
  labs(x = NULL, y = "Change in total NLL (%)") + th +
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
rf_runs <- if(file.exists(rds)) readRDS(rds) else { r <- lapply(seq_len(NREP), run_once); saveRDS(r, rds); r }
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
sh <- function(x) sub("Vulnerabilities", "Vuln.", sub("Environmental curves", "Env.", sub("Red tide foraging", "RT for.", sub("Red tide mortality", "RT mort.", x))))
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
Gb$lab <- gsub("RT mort\\.", "RT\nmort.", gsub("RT for\\.", "RT\nfor.", sh(as.character(Gb$type))))
Gb$lab <- ifelse(grepl("\n", Gb$lab), Gb$lab, paste0(Gb$lab, "\n "))
Gb$xl <- factor(Gb$lab, levels = Gb$lab)
Gb$we <- 0.2 * nchar(as.character(Gb$edge)); Gb$wn <- 0.12 + 0.19 * nchar(as.character(Gb$n))   # text widths in axis units
Gb$xe <- seq_len(nrow(Gb)) - (Gb$we + Gb$wn) / 2; Gb$xn <- Gb$xe + Gb$we
write.csv(Gb[, c("type", "n", "edge", "share", "v", "vm", "vlo", "vhi")], file.path(pr, "ts", "Fig_contribution_B_allocation.csv"), row.names = FALSE)
pB <- ggplot(Gb, aes(xl)) + geom_hline(yintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = vlo, ymax = vhi), colour = "black", width = 0.3, linewidth = 0.6) +
  geom_point(aes(y = v), fill = col_best, colour = "grey15", shape = 21, size = 2.9, stroke = 0.5) +
  geom_text(aes(x = xe, y = 5.4, label = edge), hjust = 0, size = 4.3, colour = "#C0392B", fontface = "bold") +
  geom_text(aes(x = xn, y = 5.4, label = paste0("/", n)), hjust = 0, size = 4.3, colour = "black") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(-25, 0, 5), expand = c(0, 0)) + scale_x_discrete(expand = expansion(add = 0.6)) +
  coord_cartesian(ylim = c(-26, 7), clip = "off") +
  labs(x = NULL, y = "Change in total NLL (%)") + th +
  theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5), axis.text.x = element_text(size = 10.7, colour = "black", lineheight = 0.9))

#---- combine and save ------------------------------------------------------------------------------------------------
fig <- (pA + pB + plot_layout(design = "AB", widths = c(1, 1))) + plot_annotation(tag_levels = list(c("A", "B"))) & theme(plot.tag = element_text(face = "bold", size = 23))
f_pdf <- file.path(pr, "ts", paste0("Fig_contribution_of_components", tag, ".pdf")); f_png <- sub("\\.pdf$", ".png", f_pdf)
ggsave(f_pdf, fig, width = 9, height = 3.8, device = cairo_pdf)
if(requireNamespace("pdftools", quietly = TRUE)){ pdftools::pdf_convert(f_pdf, format = "png", dpi = 300, filenames = f_png, verbose = FALSE)
} else ggsave(f_png, fig, width = 9, height = 3.8, dpi = 300, bg = "white")
