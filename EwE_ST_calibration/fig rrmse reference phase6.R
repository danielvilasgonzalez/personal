# Fit to the observed stanza biomass (reference series): relative RMSE of the baseline, the best model and the ensemble.
# Reads plots_R/ts/Reference_stanza_biomass_rrmse.csv (section 3.5 of "st results phase6.R"); writes the supplementary table and figure.
suppressMessages(library(ggplot2))
if(!exists("RUN")) RUN <- Sys.getenv("EWE_RUN", unset = "..")
OUT <- file.path(RUN, "plots_R", "ts")
d <- read.csv(file.path(OUT, "Reference_stanza_biomass_rrmse.csv"), stringsAsFactors = FALSE)
n_ens <- if("n_members" %in% names(d)) d$n_members[1] else NA
d$stanza_lab <- factor(ifelse(d$stanza == 5, "5+", as.character(d$stanza)), levels = c("0", "1", "2", "3", "4", "5+"))
d$species <- factor(d$species, levels = c("Gag", "Red grouper"))
d$change_best_pct <- 100 * (d$rrmse_best - d$rrmse_baseline) / d$rrmse_baseline
d$change_ens_mean_pct <- 100 * (d$rrmse_ens_mean - d$rrmse_baseline) / d$rrmse_baseline
tab <- d[, c("species", "stanza_lab", "series", "rrmse_baseline", "rrmse_best", "rrmse_ens_mean", "rrmse_ens_min", "rrmse_ens_max", "change_best_pct", "change_ens_mean_pct")]
names(tab)[2] <- "stanza"
tab[, -(1:3)] <- round(tab[, -(1:3)], 3); tab$change_best_pct <- round(tab$change_best_pct, 1); tab$change_ens_mean_pct <- round(tab$change_ens_mean_pct, 1)
write.csv(tab, file.path(OUT, "Table_S_rrmse_reference_stanza.csv"), row.names = FALSE)

TXT <- 10.8; TTL <- 12
long <- rbind(data.frame(d[, c("species", "stanza_lab")], model = "Baseline", y = d$rrmse_baseline, lo = NA, hi = NA),
              data.frame(d[, c("species", "stanza_lab")], model = "Ensemble mean", y = d$rrmse_ens_mean, lo = d$rrmse_ens_min, hi = d$rrmse_ens_max),
              data.frame(d[, c("species", "stanza_lab")], model = "Best model", y = d$rrmse_best, lo = NA, hi = NA))
long$model <- factor(long$model, levels = c("Baseline", "Ensemble mean", "Best model"))
cols <- c(Baseline = "black", `Ensemble mean` = "grey45", `Best model` = "#20B2AA"); shp <- c(Baseline = 21, `Ensemble mean` = 22, `Best model` = 21)
dodge <- position_dodge(width = 0.55)
p <- ggplot(long, aes(stanza_lab, y, colour = model, fill = model, shape = model, group = model)) +
  geom_linerange(aes(ymin = lo, ymax = hi), position = dodge, linewidth = 0.9, na.rm = TRUE) +
  geom_point(position = dodge, size = 3, stroke = 0.9) +
  facet_wrap(~ species, nrow = 1) +
  scale_colour_manual(values = cols, name = NULL) + scale_fill_manual(values = c(Baseline = "white", `Ensemble mean` = "grey45", `Best model` = "#20B2AA"), name = NULL) +
  scale_shape_manual(values = shp, name = NULL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Age stanza", y = "Relative RMSE against observed biomass") +
  theme_bw(base_size = 12) +
  theme(panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 0.55), panel.grid.minor = element_blank(),
        axis.text = element_text(size = TXT, colour = "black"), axis.title = element_text(size = TTL, face = "bold"),
        strip.text = element_text(size = TTL, face = "bold"), strip.background = element_rect(fill = "grey92", colour = "grey20", linewidth = 0.55),
        legend.position = "bottom", legend.text = element_text(size = TXT), aspect.ratio = 1)
ggsave(file.path(OUT, "Fig_S_rrmse_reference_stanza.png"), p, width = 9, height = 5, dpi = 300, bg = "white")
ggsave(file.path(OUT, "Fig_S_rrmse_reference_stanza.pdf"), p, width = 9, height = 5, device = pdf_device())
message("saved Fig_S_rrmse_reference_stanza and Table_S_rrmse_reference_stanza", if(is.na(n_ens)) "" else paste0(" (ensemble of ", n_ens, " models)"))
print(tab)
