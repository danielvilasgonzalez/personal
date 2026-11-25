# Sensitivity of the ensemble band to the NLL-quantile cut-off (supplement figure)
# Domain-wide annual mean biomass of gag 0 from every final-generation model; the 5th-95th percentile band is recomputed
# for ensembles made of the models with NLL at or below the 70th / 80th (ENS_PROP) / 90th / 100th percentile of the final-generation NLL distribution.
suppressMessages({library(ggplot2)})
if(!exists("RUN"))      RUN   <- Sys.getenv("EWE_RUN", unset = "..")   # phase folder (contains results/ and output/); defined by the run script when sourced from it
if(!exists("LABEL"))    LABEL <- Sys.getenv("EWE_LABEL", unset = "ga01")
if(!exists("ENS_PROP")) ENS_PROP <- 0.8                                 # the cut used for every ensemble figure
GROUP <- Sys.getenv("EWE_GROUP", unset = "gag 0")
START_YEAR <- 1985
PLOTS <- file.path(RUN, "plots_R", "ts")
csv_data <- file.path(PLOTS, "Fig_S_ensemble_threshold_gag0_data.csv")

if(!file.exists(csv_data)){   # one row per model and year: Ecospace_Annual_Average_Biomass.csv of every final-generation run
  runs <- read.csv(file.path(RUN, "results", paste0("final_ga_runs_", LABEL, ".csv")), stringsAsFactors = FALSE)
  runs$run <- basename(dirname(gsub("\\\\", "/", runs$cmd_file)))
  dirs <- list.dirs(file.path(RUN, "output"), recursive = TRUE, full.names = TRUE)
  dirs <- dirs[grepl("^run_", basename(dirs))]; names(dirs) <- basename(dirs)
  res <- lapply(seq_len(nrow(runs)), function(i){
    d <- dirs[runs$run[i]]; if(is.na(d)) return(NULL)
    L <- readLines(file.path(d, "Ecospace_Annual_Average_Biomass.csv"), warn = FALSE); h <- grep("^Year,", L)[1]
    x <- read.csv(text = L[h:length(L)], check.names = FALSE, stringsAsFactors = FALSE)
    x <- x[grepl("^[0-9.]+$", x$Year), ]
    data.frame(run = runs$run[i], fitness = runs$fitness[i], year = as.integer(x$Year), biomass = as.numeric(x[[GROUP]]))
  })
  write.csv(do.call(rbind, res), csv_data, row.names = FALSE)
}
d <- read.csv(csv_data)
d$year <- START_YEAR - 1 + d$year
fit <- unique(d[, c("run", "fitness")])
cuts <- sort(unique(c(1, 0.9, ENS_PROP, 0.7)), decreasing = TRUE)
lab  <- paste0(cuts * 100, "% (n = ", sapply(cuts, function(q) sum(fit$fitness <= quantile(fit$fitness, q))), ")")
band <- do.call(rbind, lapply(seq_along(cuts), function(k){
  keep <- fit$run[fit$fitness <= quantile(fit$fitness, cuts[k])]
  a <- aggregate(biomass ~ year, d[d$run %in% keep, ], function(v) quantile(v, c(0.05, 0.95)))
  data.frame(year = a$year, lo = a$biomass[, 1], hi = a$biomass[, 2], cut = lab[k])
}))
band$cut <- factor(band$cut, levels = lab)
best <- d[d$run == fit$run[which.min(fit$fitness)], ]
is_main <- which(cuts == ENS_PROP)
cols <- setNames(rep(c("grey70", "#1B9E77", "#7570B3"), length.out = length(cuts)), lab)
cols[is_main] <- "black"
band_long <- rbind(transform(band, y = lo, edge = "lo"), transform(band, y = hi, edge = "hi"))
band_long$main <- band_long$cut == lab[is_main]
p <- ggplot() +
  geom_ribbon(data = band[band$cut == lab[is_main], ], aes(year, ymin = lo, ymax = hi), fill = "grey60", alpha = 0.35) +
  geom_line(data = band_long, aes(year, y, colour = cut, group = interaction(cut, edge), linewidth = main)) +
  geom_line(data = best, aes(year, biomass), colour = "#20B2AA", linewidth = 0.9, linetype = "22") +
  scale_colour_manual(values = cols, name = "Models retained (NLL at or below\nthis percentile of the final generation)") +
  scale_linewidth_manual(values = c(`FALSE` = 0.6, `TRUE` = 1.1), guide = "none") +
  labs(x = "Year", y = paste0("Annual mean biomass, ", GROUP, " (domain-wide)")) +
  theme_bw(base_size = 12) + theme(panel.grid.minor = element_blank(), legend.position = "right")
ggsave(file.path(PLOTS, "Fig_S_ensemble_threshold_sensitivity.png"), p, width = 9, height = 4.2, dpi = 300)
w <- aggregate(hi - lo ~ cut, band, mean); names(w)[2] <- "mean_band_width"
w$relative_to_100pct <- w$mean_band_width / w$mean_band_width[1]
write.csv(w, file.path(PLOTS, "Table_S_ensemble_threshold_widths.csv"), row.names = FALSE)
print(w)
