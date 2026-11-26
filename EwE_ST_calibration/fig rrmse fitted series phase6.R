# Relative RMSE of every fitted series (survey indices by region, biomass, catch, F): baseline, best model and ensemble.
# Reads plots_R/ts/Fitted_series_rrmse.csv (section 3.6 of "st results phase6.R"); writes the supplementary figure and table.
suppressMessages({library(ggplot2); library(patchwork)})
if(!exists("RUN")) RUN <- Sys.getenv("EWE_RUN", unset = "..")
OUT <- file.path(RUN, "plots_R", "ts")
d <- read.csv(file.path(OUT, "Fitted_series_rrmse.csv"), stringsAsFactors = FALSE)
n_ens <- d$n_members[1]
lab <- c(BiomassRel = "Biomass and survey indices (relative)", Biomass = "Biomass (absolute)", Catches = "Catch (absolute)", CatchesRel = "Catch (relative)", FishingMortalityRef = "Fishing mortality")
d$type_lab <- factor(ifelse(d$type %in% names(lab), lab[d$type], d$type), levels = unique(c(lab, d$type))); levels(d$type_lab) <- vapply(strwrap(levels(d$type_lab), 16, simplify = FALSE), paste, character(1), collapse = "\n")
d$series_lab <- paste0(d$series, ifelse(d$region > 0, paste0(" (R", d$region, ")"), ""))
d <- d[order(d$type_lab, d$rrmse_baseline), ]; d$series_lab <- factor(d$series_lab, levels = rev(unique(d$series_lab)))
d$change_best_pct <- round(100 * (d$rrmse_best - d$rrmse_baseline) / d$rrmse_baseline, 1)
d$change_ens_mean_pct <- round(100 * (d$rrmse_ens_mean - d$rrmse_baseline) / d$rrmse_baseline, 1)
# weight = 1 / assumed observation CV in the objective, so rRMSE x weight = misfit in units of the assumed observation CV (1 = as large as the assumed error)
for(v in c("baseline", "best", "ens_mean", "ens_min", "ens_max")) d[[paste0("w_rrmse_", v)]] <- d[[paste0("rrmse_", v)]] * d$weight
wcols <- paste0("w_rrmse_", c("baseline", "best", "ens_mean", "ens_min", "ens_max"))
write.csv(cbind(d[, c("species", "type", "series", "region", "weight")], round(d[, c("rrmse_baseline", "rrmse_best", "rrmse_ens_mean", "rrmse_ens_min", "rrmse_ens_max")], 3), d[, c("change_best_pct", "change_ens_mean_pct")], round(d[, wcols], 3)),
          file.path(OUT, "Table_S_rrmse_fitted_series.csv"), row.names = FALSE)

TXT <- 10.8; TTL <- 12
cols <- c(Baseline = "black", `Ensemble mean` = "grey45", `Best model` = "#20B2AA")
mk <- function(pref){
  l <- rbind(data.frame(d[, c("series_lab", "type_lab")], model = "Baseline", y = d[[paste0(pref, "baseline")]], lo = NA, hi = NA),
             data.frame(d[, c("series_lab", "type_lab")], model = "Ensemble mean", y = d[[paste0(pref, "ens_mean")]], lo = d[[paste0(pref, "ens_min")]], hi = d[[paste0(pref, "ens_max")]]),
             data.frame(d[, c("series_lab", "type_lab")], model = "Best model", y = d[[paste0(pref, "best")]], lo = NA, hi = NA))
  l$model <- factor(l$model, levels = names(cols)); l }
dodge <- position_dodge(width = 0.7)
mkp <- function(pref, xlab, last){
  ggplot(mk(pref), aes(y, series_lab, colour = model, fill = model, shape = model)) +
    geom_linerange(aes(xmin = lo, xmax = hi), position = dodge, linewidth = 0.8, na.rm = TRUE) +
    geom_point(position = dodge, size = 2.2, stroke = 0.8) +
    facet_grid(type_lab ~ ., scales = "free_y", space = "free_y") +
    scale_colour_manual(values = cols, name = NULL) + scale_fill_manual(values = c(Baseline = "white", `Ensemble mean` = "grey45", `Best model` = "#20B2AA"), name = NULL) +
    scale_shape_manual(values = c(Baseline = 21, `Ensemble mean` = 22, `Best model` = 21), name = NULL) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = xlab, y = NULL) +
    theme_bw(base_size = 12) +
    theme(panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 0.55), panel.grid.minor = element_blank(),
          axis.text = element_text(size = 8.5, colour = "black"), axis.title = element_text(size = TTL, face = "bold"),
          legend.position = "bottom", legend.text = element_text(size = TXT),
          axis.text.y = if(last) element_blank() else element_text(size = 8.5, colour = "black"),
          axis.ticks.y = if(last) element_blank() else element_line(colour = "grey20"),
          strip.text.y = if(last) element_text(size = 9, face = "bold", angle = 0) else element_blank(),
          strip.background.y = if(last) element_rect(fill = "grey92", colour = "grey20", linewidth = 0.55) else element_blank())
}
p <- (mkp("rrmse_", "Relative RMSE against the fitted data", FALSE) + mkp("w_rrmse_", "Relative RMSE x weight (in units of the assumed observation CV)", TRUE)) +
  patchwork::plot_layout(guides = "collect", widths = c(1.15, 1)) & theme(legend.position = "bottom")
h <- max(5, 1.6 + 0.2 * nrow(d))
ggsave(file.path(OUT, "Fig_S_rrmse_fitted_series.png"), p, width = 14, height = h, dpi = 300, bg = "white")
ggsave(file.path(OUT, "Fig_S_rrmse_fitted_series.pdf"), p, width = 14, height = h, device = cairo_pdf)
message("saved Fig_S_rrmse_fitted_series and Table_S_rrmse_fitted_series (ensemble of ", n_ens, " models)")
