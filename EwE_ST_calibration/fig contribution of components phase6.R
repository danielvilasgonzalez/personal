#---------------------------------------------------------------------------------------------------------------
# Figure: contribution of components to the fit (phase6 ga01)
#   A  change in temporal and spatial NLL, best minus baseline (all groups, gag, red grouper)
#   B  change in NLL by stanza, catch and fishing mortality (gag, red grouper)
#   C  ten largest temporal series, range across the ensemble members
#   D  change in spatial NLL for the four focal maps
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
if(EXCLUDE_STANZA_BIOMASS){
  ser <- ser[!is_stanza_bio(ser$Title), ]
  mem <- mem[!is_stanza_bio(mem$series), ]
}
ser$base <- ser$loglik.baseline; ser$best <- ser$loglik.best; ser$delta <- ser$best - ser$base
message("series used: ", nrow(ser), " | temporal NLL baseline ", round(sum(ser$base), 1), ", best ", round(sum(ser$best), 1))

# palette taken from the manuscript figures (st_calibration_functions.R): baseline = black, best = turquoise,
# ensemble mean = dark magenta, ensemble range = magenta (members), temporal NLL = dark blue (as in Fig. 2)
col_base <- "black"; col_best <- "#20B2AA"; col_mem <- "#B5367A"; col_mean <- "#6E1F4D"
col_t <- "#1B4F72"; col_s <- "#E69F00"; col_b <- "#0072B2"; col_c <- "#D55E00"; col_f <- "#999999"
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
Ad <- do.call(rbind, lapply(split(A, A$group), function(d){
  b <- d[d$model == "Baseline", ]; e <- d[d$model == "Best", ]
  data.frame(group = b$group, part = c("Temporal", "Spatial"),
             delta = c(e$temporal - b$temporal, e$spatial - b$spatial),
             pct = c(100 * (e$temporal - b$temporal) / b$temporal, 100 * (e$spatial - b$spatial) / b$spatial))}))
Ad$part <- factor(Ad$part, levels = c("Temporal", "Spatial"))
pc  <- ifelse(abs(Ad$pct) < 1, sprintf("%+.1f", Ad$pct), sprintf("%+.0f", Ad$pct))
Ad$lab <- paste0(pc, "%\n(", ifelse(Ad$delta > 0, "+", "-"), formatC(abs(round(Ad$delta)), format = "d", big.mark = ","), ")")
pA <- ggplot(Ad, aes(part, pct, fill = part)) + geom_hline(yintercept = 0) + geom_col(width = 0.65) +
  geom_text(aes(label = lab, vjust = ifelse(pct > 0, -0.2, 1.1)), size = 4.6, lineheight = 0.9) +
  facet_wrap(~group, nrow = 1) +
  scale_fill_manual(values = c("Temporal" = col_t, "Spatial" = col_s)) +
  scale_y_continuous(limits = c(-68, 22), breaks = seq(-60, 20, 20)) +
  labs(x = NULL, y = "Change in NLL (%)") + th + theme(legend.position = "none")

#---- B: change by stanza (catch and F only) ------------------------------------------------------------------
get_d <- function(title) { v <- ser$delta[ser$Title == title]; if(length(v)) v[1] else NA_real_ }
rows <- list()
for(sp in c("Gag", "Red grouper")){
  pre <- if(sp == "Gag") "GAG" else "REDGRP"
  for(a in 1:6){
    ct <- if(sp == "Red grouper" && a < 6) paste0(pre, "_age", a, "_relCatch") else paste0(pre, "_age", a, "_catch")
    rows[[length(rows) + 1]] <- data.frame(species = sp, stanza = c("0", "1", "2", "3", "4", "5+")[a],
      measure = c("Catch", "Fishing mortality"), delta = c(get_d(ct), get_d(paste0(pre, "_age", a, "_F"))))
    if(!EXCLUDE_STANZA_BIOMASS)
      rows[[length(rows) + 1]] <- data.frame(species = sp, stanza = c("0", "1", "2", "3", "4", "5+")[a],
        measure = "Biomass", delta = get_d(paste0(pre, "_age", a, "_biomass")))
  }
}
B <- do.call(rbind, rows); B$stanza <- factor(B$stanza, levels = c("0", "1", "2", "3", "4", "5+"))
B$species <- factor(B$species, levels = c("Gag", "Red grouper"))
symlog <- scales::trans_new("symlog", function(x) sign(x) * log10(1 + abs(x) / 10), function(y) sign(y) * 10 * (10^abs(y) - 1))
pB <- ggplot(B, aes(stanza, delta, fill = measure)) + geom_hline(yintercept = 0) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~species, nrow = 1) +
  scale_fill_manual(values = c("Biomass" = col_b, "Catch" = col_c, "Fishing mortality" = col_f)) +
  scale_y_continuous(trans = symlog, breaks = c(-1000, -100, -10, 0, 10), labels = c("-1,000", "-100", "-10", "0", "10")) +
  labs(x = "Stanza (age)", y = "Change in NLL") + th +
  theme(legend.position = c(0.53, 0.03), legend.justification = c(0, 0), legend.direction = "vertical",
        legend.key.height = unit(1.1, "lines"), legend.background = element_blank())

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
fig <- (pA + pB + pD + pC + plot_layout(ncol = 2, widths = c(1.3, 1))) + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold", size = 22))
dir.create(file.path(pr, "ts"), showWarnings = FALSE, recursive = TRUE)
tag <- if(EXCLUDE_STANZA_BIOMASS) "" else "_all58"
ggsave(file.path(pr, "ts", paste0("Fig_contribution_of_components", tag, ".png")), fig, width = 13.5, height = 10.5, dpi = 300, bg = "white")
ggsave(file.path(pr, "ts", paste0("Fig_contribution_of_components", tag, ".pdf")), fig, width = 13.5, height = 10.5)

write.csv(Ad, file.path(pr, "ts", paste0("Fig_contribution_A_change", tag, ".csv")), row.names = FALSE)
write.csv(A, file.path(pr, "ts", paste0("Fig_contribution_A_partition", tag, ".csv")), row.names = FALSE)
write.csv(B, file.path(pr, "ts", paste0("Fig_contribution_B_stanza", tag, ".csv")),    row.names = FALSE)
write.csv(C[, c("map", "baseline", "best", "delta", "pct")], file.path(pr, "ts", paste0("Fig_contribution_C_maps", tag, ".csv")), row.names = FALSE)
write.csv(D[, c("id", "baseline", "best_series", "member_min", "member_mean", "member_max")], file.path(pr, "ts", paste0("Fig_contribution_D_series", tag, ".csv")), row.names = FALSE)
print(A)
