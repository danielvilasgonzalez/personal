#---------------------------------------------------------------------------------------------------------------
# Figure: contribution of components to the fit (phase6 ga01)
#   A  change in temporal and spatial NLL, best minus baseline (all groups, gag, red grouper)
#   B  change in NLL by stanza, catch and fishing mortality (gag, red grouper)
#   C  ten largest temporal series, range across the ensemble members
#   D  change in spatial NLL for the four focal maps
#   E  share of the NLL variation explained by each type of parameter (random forest, grouped permutation importance)
#   F  the ten individual parameters with the largest share
#
# The stanza biomass series (GAG_age1..6_biomass, REDGRP_age1..6_biomass) come from the stock assessments
# and are treated as reference series. They are EXCLUDED here (EXCLUDE_STANZA_BIOMASS = TRUE).
# Set it to FALSE to get the full 58-series objective that the GA actually minimized.
#
# Sourced from the main script (st calibration run phase6.R, section 7) or run on its own.
# Needs only: ggplot2, patchwork (both already used by st_calibration_functions.R).
# Inputs (all already on disk): plots_R/nll_temporal_by_series.csv, plots_R/nll_spatial_by_map.csv,
#   plots_R/nll_partition_temporal_vs_spatial.csv, plots_R/discussion_checks/06_member_series_nll.csv
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
th <- theme_classic(base_size = 15) +
  theme(plot.title = element_text(face = "bold", size = 15), strip.background = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 15), axis.title = element_text(face = "bold"), legend.position = "bottom",
        legend.title = element_blank(), axis.text = element_text(colour = "black"))

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
stype <- ifelse(grepl("(_catch|_relCatch|SSrelCatch)$", ser$Title), "Catch", ifelse(grepl("_F$", ser$Title), "Fishing mortality", "Indices"))
sp_delta <- function(keys) sum(spm$best[spm$map %in% keys] - spm$baseline[spm$map %in% keys])
mk <- function(group, sel, sp){
  d <- tapply(ser$delta[sel], factor(stype[sel], levels = c("Indices", "Catch", "Fishing mortality")), sum)
  data.frame(group = group, part = c("Indices", "Catch", "Fishing mortality", "Spatial"), delta = c(as.numeric(d), sp))
}
Ad <- rbind(mk("All groups", rep(TRUE, nrow(ser)), sp_best_total - sum(spm$baseline)),
            mk("Gag", gag, sp_delta(c("gag 2-4", "gag 5+"))),
            mk("Red grouper", red, sp_delta(c("red grouper 3-4", "red grouper 5+"))))
Ad$group <- factor(Ad$group, levels = c("All groups", "Gag", "Red grouper"))
Ad$part  <- factor(Ad$part, levels = c("Spatial", "Fishing mortality", "Catch", "Indices"))
base_tot <- c("All groups" = A$temporal[1] + A$spatial[1], "Gag" = A$temporal[3] + A$spatial[3], "Red grouper" = A$temporal[5] + A$spatial[5])
net <- aggregate(delta ~ group, Ad, sum)
net$neg <- tapply(pmin(Ad$delta, 0), Ad$group, sum)[as.character(net$group)]
net$pct <- 100 * net$delta / base_tot[as.character(net$group)]
net$lab <- paste0(ifelse(net$delta > 0, "+", "-"), formatC(abs(round(net$delta)), format = "d", big.mark = ","), "\n(", sprintf("%+.0f", net$pct), "%)")
fill_all <- c("Indices" = col_t, "Biomass" = col_b, "Catch" = col_c, "Fishing mortality" = col_f, "Spatial" = col_s)   # one legend for A and B (drawn in B)
pA <- ggplot(Ad, aes(group, delta, fill = part)) + geom_hline(yintercept = 0) + geom_col(width = 0.6) +
  geom_text(data = net, aes(x = group, y = neg, label = lab), inherit.aes = FALSE, vjust = 1.15, size = 4.6, lineheight = 0.9) +
  scale_fill_manual(values = fill_all, limits = names(fill_all)) +
  facet_wrap(~group, scales = "free", nrow = 1) +
  scale_y_continuous(expand = expansion(mult = c(0.28, 0.12)), labels = function(x) format(x, big.mark = ",")) +
  labs(x = NULL, y = "Change in NLL") + th +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), strip.text = element_text(face = "bold", hjust = 0.5, size = 13),
        axis.title.y = element_text(face = "bold", margin = margin(r = -140)),
        legend.position = "none", legend.justification = c(0, 0.5), legend.title = element_blank(), legend.key.height = unit(1.1, "lines"))

#---- B: change by stanza (catch and F only) ------------------------------------------------------------------
get_d <- function(title) { v <- ser_all$delta[ser_all$Title == title]; if(length(v)) v[1] else NA_real_ }
rows <- list()
for(sp in c("Gag", "Red grouper")){
  pre <- if(sp == "Gag") "GAG" else "REDGRP"
  for(a in 1:6){
    ct <- if(sp == "Red grouper" && a < 6) paste0(pre, "_age", a, "_relCatch") else paste0(pre, "_age", a, "_catch")
    rows[[length(rows) + 1]] <- data.frame(species = sp, stanza = c("0", "1", "2", "3", "4", "5+")[a],
      measure = c("Catch", "Fishing mortality"), delta = c(get_d(ct), get_d(paste0(pre, "_age", a, "_F"))))
    rows[[length(rows) + 1]] <- data.frame(species = sp, stanza = c("0", "1", "2", "3", "4", "5+")[a],
        measure = "Biomass", delta = get_d(paste0(pre, "_age", a, "_biomass")))
  }
}
B <- do.call(rbind, rows)
B$measure <- factor(B$measure, levels = names(fill_all)[names(fill_all) != "Spatial"])
B$stanza <- factor(B$stanza, levels = c("0", "1", "2", "3", "4", "5+"))
B$species <- factor(B$species, levels = c("Gag", "Red grouper"))
symlog <- scales::trans_new("symlog", function(x) sign(x) * log10(1 + abs(x) / 10), function(y) sign(y) * 10 * (10^abs(y) - 1))
pB <- ggplot(B, aes(stanza, delta, fill = measure)) + geom_hline(yintercept = 0) +
  geom_col(position = position_dodge2(width = 0.8, preserve = "total"), width = 0.8) +
  facet_grid(. ~ species, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = fill_all[names(fill_all) != "Spatial"]) +
  scale_y_continuous(trans = symlog, breaks = c(-1000, -100, -10, 0, 10), labels = c("-1,000", "-100", "-10", "0", "10")) +
  labs(x = "Stanza (age)", y = "Change in NLL") + th +
  theme(strip.text = element_text(face = "bold", hjust = 0, size = 15), axis.text.x = element_text(size = 12, colour = "black"), legend.position = "none")

#---- C: focal spatial maps -----------------------------------------------------------------------------------
C <- spm[!is.na(spm$best), ]; C$delta <- C$best - C$baseline; C$pct <- 100 * C$delta / C$baseline
C$lab <- factor(C$map, levels = c("gag 2-4", "gag 5+", "red grouper 3-4", "red grouper 5+"),
                labels = c("Gag\n2-4", "Gag\n5+", "Red\ngrouper\n3-4", "Red\ngrouper\n5+"))
C$txt <- paste0(sprintf("%+.0f", C$pct), "%\n(", sprintf("%+.1f", C$delta), ")")
pC <- ggplot(C, aes(lab, pct, fill = pct < 0)) + geom_hline(yintercept = 0) + geom_col(width = 0.65) +
  geom_text(aes(label = txt, vjust = ifelse(pct > 0, -0.2, 1.1)), size = 4.6, lineheight = 0.9) +
  scale_fill_manual(values = c("TRUE" = "#009E73", "FALSE" = "#CC79A7"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  labs(x = NULL, y = "Change in spatial NLL (%)") + th

#---- D: largest series across members ------------------------------------------------------------------------
nice <- c(GAG_age6_catch = "Gag 5+ C", GAG_age6_F = "Gag 5+ F", GAG_age6_biomass = "Gag 5+ B",
          REDGRP_age6_F = "Red grouper 5+ F", REDGRP_age6_catch = "Red grouper 5+ C",
          GAG_SEDAR105_AGE0_TPR = "Gag 0 B, TPR", GAG_SEDAR105_AGE0_TBR = "Gag 0 B, TBR",
          GAG_SEDAR105_AGE0_CHR = "Gag 0 B, CHR", GAG_SEDAR105_AGE0_FWCNMFS = "Gag 0 B, FWC-NMFS",
          GAG_SEDAR105_AGE0_SAR = "Gag 0 B, SAR", GAG_SEDAR105_AGE0_MBB = "Gag 0 B, MBB",
          REDGRP_SEDAR88_NMFS_BLL = "Red grouper 5+ B, BLL", REDGRP_SEDAR88_CmbVid = "Red grouper 5+ B, video",
          REDGRP_SEDAR88_Rec = "Red grouper 3-4 B, rec.", GAG_SEDAR72SRFS_Srv_SEAMAP_VIDEO_9 = "Gag B, SEAMAP video",
          GAG_SEDAR72SRFS_Rec_HBT_3 = "Gag B, HBT rec.", GAG_SEDAR105_GFISHER = "Gag B, GFISHER")
mem$id <- sub("\\|.*$", "", mem$series)
D <- mem[order(-mem$member_mean), ][seq_len(N_TOP), ]
D$label <- ifelse(D$id %in% names(nice), nice[D$id], D$id)
D$label <- vapply(D$label, function(x) paste(strwrap(x, width = 100), collapse = "\n"), "")
D$label <- factor(D$label, levels = rev(unique(D$label)))
D$best_series <- ser$best[match(D$id, ser$Title)]
pD <- ggplot(D, aes(y = label)) +
  geom_linerange(aes(xmin = member_min, xmax = member_max, colour = "Ensemble range"), linewidth = 2.2, alpha = 0.45) +
  geom_point(aes(x = member_mean, colour = "Ensemble mean"), size = 3) +
  geom_point(aes(x = best_series, colour = "Best model"), shape = 18, size = 5) +
  geom_point(aes(x = baseline, colour = "Baseline model"), shape = 18, size = 5) +
  scale_colour_manual(values = c("Ensemble range" = col_mem, "Ensemble mean" = col_mean, "Best model" = col_best, "Baseline model" = col_base),
                      breaks = c("Baseline model", "Best model", "Ensemble mean", "Ensemble range"),
                      guide = guide_legend(ncol = 1, override.aes = list(linetype = c(0, 0, 0, 1), shape = c(18, 18, 16, NA),
                                                                          linewidth = c(0, 0, 0, 2.2), alpha = c(1, 1, 1, 0.45)))) +
  scale_x_log10() + labs(x = "NLL of the series (log scale)", y = NULL) + th +
  theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1),
        legend.key.height = unit(1.1, "lines"), legend.background = element_blank())

#---- combine -------------------------------------------------------------------------------------------------

#---- E, F: contribution of parameter types to the NLL ---------------------------------------------------------
# Random forest of log10(NLL) on the 98 parameters over the 11,471 GA evaluations; grouped permutation importance.
# Evaluations are not independent (the GA samples near good regions), so the shares are descriptive. Needs ranger.
library(ranger)
par_tab <- read.csv(file.path(RUN, "results", "parvec_ga01.csv"), check.names = FALSE); names(par_tab)[1] <- "par"
e <- read.csv(file.path(RUN, "results", "ga_evals_ga01.csv"), check.names = FALSE)
types <- c("Vulnerabilities", "Red tide curves", "Environmental curves", "Fleet", "Dispersal")
ptype <- ifelse(grepl("^vul_", par_tab$par), types[1], ifelse(grepl("^rt", par_tab$par), types[2],
         ifelse(grepl("^env", par_tab$par), types[3], ifelse(grepl("^flt_", par_tab$par), types[4], types[5])))); names(ptype) <- par_tab$par
X <- e[, par_tab$par]; ok <- complete.cases(X) & is.finite(e$fitness); X <- X[ok, ]; y <- log10(e$fitness[ok])
for(j in names(X)) if(min(X[[j]]) > 0 && max(X[[j]]) / min(X[[j]]) > 20) X[[j]] <- log10(X[[j]])
set.seed(1); te <- sample(seq_along(y), round(0.25 * length(y))); tr <- setdiff(seq_along(y), te)
fit <- ranger(x = X[tr, ], y = y[tr], num.trees = 500, min.node.size = 5, seed = 1)
r2 <- 1 - sum((y[te] - predict(fit, X[te, ])$predictions)^2) / sum((y[te] - mean(y[te]))^2)
mse0 <- mean((y[te] - predict(fit, X[te, ])$predictions)^2)
perm <- function(cols, nrep = 10){ mean(replicate(nrep, { Xp <- X[te, ]; Xp[, cols] <- Xp[sample(length(te)), cols]
  mean((y[te] - predict(fit, Xp)$predictions)^2) - mse0 })) }
grp <- ptype[colnames(X)]
G <- data.frame(type = types, n = as.numeric(table(factor(grp, levels = types))))
G$inc <- sapply(types, function(t) perm(names(grp)[grp == t])); G$share <- 100 * G$inc / sum(G$inc)
G$per_par <- G$share / G$n
P <- data.frame(par = colnames(X), type = grp, inc = sapply(colnames(X), function(j) perm(j, 3)), row.names = NULL)
P$share <- 100 * P$inc / sum(G$inc)
message("held-out R2 = ", round(r2, 2)); print(G)

cols <- c("Vulnerabilities" = "#1B4F72", "Red tide curves" = "#B5367A", "Environmental curves" = "#009E73", "Fleet" = "#D55E00", "Dispersal" = "#E69F00")
th_p <- theme_classic(base_size = 15) + theme(strip.background = element_blank(), axis.title = element_text(face = "bold"),
        axis.text = element_text(colour = "black"), legend.position = "none")
G$type <- factor(G$type, levels = rev(G$type[order(-G$share)]))
G$lab <- paste0(sprintf("%.0f", G$share), "%")
G$tcol <- as.character(G$type); lev <- levels(G$type); G$type <- factor(paste0(G$type, " (", G$n, ")"), levels = paste0(lev, " (", G$n[match(lev, G$type)], ")"))
pE <- ggplot(G, aes(share, type, fill = tcol)) + geom_col(width = 0.65) +
  geom_text(aes(label = lab), hjust = -0.06, size = 4.4) +
  scale_fill_manual(values = cols) + scale_x_continuous(limits = c(0, 56), breaks = c(0, 20, 40), expand = expansion(mult = c(0, 0))) +
  labs(x = "Share of NLL variation (%)", y = NULL) + th_p
nice_p <- function(x){ x <- sub("^vul_(\\d+)_(\\d+)$", "Vulnerability \\1-\\2", x)
  x <- sub("^flt_(\\d+)_(mult|pow)$", "Fleet \\1 \\2", x)
  x <- sub("^rt(\\d+)_type\\d+_(\\w+)\\.adj$", "Red tide \\1 \\2", x)
  x <- sub("^env(\\d+)_type\\d+_(\\w+)\\.adj$", "Environment \\1 \\2", x)
  sub("^disp_(\\d+)$", "Dispersal \\1", x) }
Pt <- P[order(-P$share), ][1:10, ]; Pt$lab <- factor(nice_p(Pt$par), levels = rev(nice_p(Pt$par)))
pF <- ggplot(Pt, aes(share, lab, fill = type)) + geom_col(width = 0.65) +
  scale_fill_manual(values = cols, breaks = types) + scale_x_continuous(breaks = c(0, 10, 20), expand = expansion(mult = c(0, 0.04))) +
  labs(x = "Share of NLL variation (%)", y = NULL) + th_p +
  theme(legend.position = c(0.98, 0.05), legend.justification = c(1, 0), legend.title = element_blank(), legend.background = element_blank())

gl <- ggplotGrob(pA + theme(legend.position = "right")); leg <- wrap_elements(full = gl$grobs[[which(sapply(gl$grobs, function(g) g$name) == "guide-box")[1]]])
fig <- (pA + leg + pB + pD + pC + pE + pF + plot_layout(design = "ABC\nD#E\nF#G", widths = c(1.22, 0.4, 1))) + plot_annotation(tag_levels = list(c("A", "", "B", "C", "D", "E", "F"))) & theme(plot.tag = element_text(face = "bold", size = 22))
dir.create(file.path(pr, "ts"), showWarnings = FALSE, recursive = TRUE)
tag <- if(EXCLUDE_STANZA_BIOMASS) "" else "_all58"
ggsave(file.path(pr, "ts", paste0("Fig_contribution_of_components", tag, ".png")), fig, width = 16, height = 15.5, dpi = 300, bg = "white")
ggsave(file.path(pr, "ts", paste0("Fig_contribution_of_components", tag, ".pdf")), fig, width = 16, height = 15.5)

write.csv(rbind(Ad), file.path(pr, "ts", paste0("Fig_contribution_A_change", tag, ".csv")), row.names = FALSE)
write.csv(A, file.path(pr, "ts", paste0("Fig_contribution_A_partition", tag, ".csv")), row.names = FALSE)
write.csv(B, file.path(pr, "ts", paste0("Fig_contribution_B_stanza", tag, ".csv")),    row.names = FALSE)
write.csv(C[, c("map", "baseline", "best", "delta", "pct")], file.path(pr, "ts", paste0("Fig_contribution_C_maps", tag, ".csv")), row.names = FALSE)
write.csv(D[, c("id", "baseline", "best_series", "member_min", "member_mean", "member_max")], file.path(pr, "ts", paste0("Fig_contribution_D_series", tag, ".csv")), row.names = FALSE)
print(A)
write.csv(G, file.path(pr, "ts", "Fig_parameter_type_E.csv"), row.names = FALSE)
write.csv(P[order(-P$share), ], file.path(pr, "ts", "Fig_parameter_type_F_all_parameters.csv"), row.names = FALSE)
