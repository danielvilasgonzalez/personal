# Which NLL cut-off for the ensemble? Coverage of the fitted observations, band width and error of the ensemble mean, for cut-offs from the best model alone to all models.
# Reads plots_R/ts/Ensemble_cutoff_assessment.csv (section 3.7 of "st results phase6.R"); writes Fig_S_ensemble_cutoff_assessment.png/.pdf
suppressMessages({library(ggplot2); library(patchwork)})
if(!exists("RUN")) RUN <- Sys.getenv("EWE_RUN", unset = "..")
if(!exists("ENS_PROP")) ENS_PROP <- 0.80
OUT <- file.path(RUN, "plots_R", "ts")
d <- read.csv(file.path(OUT, "Ensemble_cutoff_assessment.csv"), stringsAsFactors = FALSE)
TXT <- 10.8; TTL <- 12
all <- d[d$group == "All series", ]; typ <- d[d$group != "All series", ]
lab <- c(BiomassRel = "Biomass and indices", Catches = "Catch (abs.)", CatchesRel = "Catch (rel.)", FishingMortalityRef = "Fishing mortality")
typ$group <- ifelse(typ$group %in% names(lab), lab[typ$group], typ$group)
tcol <- c("Biomass and indices" = "#0072B2", "Catch (abs.)" = "#D55E00", "Catch (rel.)" = "#E69F00", "Fishing mortality" = "#009E73")
th <- theme_bw(base_size = 12) + theme(panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 0.55), panel.grid.minor = element_blank(),
        axis.text = element_text(size = TXT, colour = "black"), axis.title = element_text(size = TTL, face = "bold"), legend.position = "bottom", legend.text = element_text(size = TXT))
mk <- function(y, ylab, ylim = NULL){
  ggplot() +
    geom_vline(xintercept = 100 * ENS_PROP, linetype = "22", colour = "grey35", linewidth = 0.6) +
    geom_line(data = typ, aes(100 * cutoff, .data[[y]], colour = group), linewidth = 0.6, alpha = 0.8) +
    geom_line(data = all, aes(100 * cutoff, .data[[y]], colour = "All series"), linewidth = 1.5) +
    geom_point(data = all, aes(100 * cutoff, .data[[y]], colour = "All series"), size = 2.4) +
    scale_colour_manual(values = c("All series" = "black", tcol), name = NULL) +
    scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, 100)) +
    scale_y_continuous(limits = c(ylim[1], ylim[2]), expand = expansion(mult = c(0.02, 0.05))) +
    labs(x = "NLL cut-off (percentile, 0 = best model)", y = ylab) + th }
p <- (mk("coverage", "Observations inside the band", c(0, 1)) + mk("rel_width", "Band width / mean observation", c(0, NA)) +
      mk("w_rrmse_mean", "RRMSE of the ensemble mean x weight", c(0, NA))) +
  plot_layout(guides = "collect", nrow = 1) & theme(legend.position = "bottom")
ggsave(file.path(OUT, "Fig_S_ensemble_cutoff_assessment.png"), p, width = 15, height = 5.6, dpi = 300, bg = "white")
ggsave(file.path(OUT, "Fig_S_ensemble_cutoff_assessment.pdf"), p, width = 15, height = 5.6, device = cairo_pdf)
write.csv(cbind(all[, c("cutoff", "n_members", "coverage", "rel_width", "rrmse_mean", "w_rrmse_mean")]), file.path(OUT, "Table_S_ensemble_cutoff_assessment.csv"), row.names = FALSE)
message("saved Fig_S_ensemble_cutoff_assessment (dashed line = ", 100 * ENS_PROP, "% cut-off)")
