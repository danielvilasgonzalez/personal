U <- "/mnt/user-data/uploads/ST_calibration/phase6_ga01"; W <- "/home/claude/rescore"; N_ENS <- 37; options(warn = 1)
for (f in c("globals.R", "utils.R", "input_output.R", "Ecospace_objective_functions.R", "Spatial_likelihood.R")) suppressMessages(source(file.path(W, "r4", "R", f)))
system.file <- function(..., package = "") if (package == "R4EwE") file.path(W, "r4", "inst", ...) else base::system.file(..., package = package)
runs <- read.csv(file.path(U, "results", "final_ga_runs_ga01.csv")); runs <- runs[order(runs$fitness), ]
inp <- file.path(U, "inputs")
vuls <- read.csv(file.path(inp, "WFS-MICE EwE 2.13 v67020-Vulnerabilities.csv"), row.names = 1); group.names <- gsub(" ", "_", vuls[, 1])
obs.ts <- fn.read_ecosim_timeseries(file.path(inp, "ts_mice_v5_discards_ecospace_regions_sedar105.csv"))
ri <- read.csv(file.path(U, "run_info.csv")); g <- function(k) ri$value[ri$field == k]
rel <- as.numeric(strsplit(g("setting.relative.types"), " ")[[1]]); obs.ts$ts.head$Absolute[obs.ts$ts.head$Type %in% rel] <- FALSE
sg <- as.numeric(strsplit(g("setting.stanza.groups"), " ")[[1]]); ld <- as.numeric(strsplit(g("setting.lead.stanza"), " ")[[1]])
obs.ts$ts.head$Absolute[obs.ts$ts.head$Type == 104 & obs.ts$ts.head$Poolcode %in% setdiff(sg, ld)] <- FALSE
th <- obs.ts$ts.head; j_all <- which(grepl("_age[1-6]_biomass$", th$Title))
print(th[j_all, intersect(c("Title", "Type", "Poolcode", "Region", "Weight", "Absolute"), names(th))])
member_dir <- function(cmd) { key <- sub("^(run_g[0-9]+_i[0-9]+).*", "\\1", basename(dirname(gsub("\\\\", "/", cmd)))); list.files(file.path(W, "output/gen30_small"), pattern = paste0("^", key), full.names = TRUE)[1] }
dirs <- c(base = file.path(U, "output/run_base"), setNames(vapply(runs$cmd_file[seq_len(N_ENS)], member_dir, character(1)), paste0("m", seq_len(N_ENS))))
obs_yrs <- as.numeric(rownames(obs.ts$ts)); styear <- min(obs_yrs)
norm_name <- function(x) tolower(gsub("[^a-z0-9]", "", tolower(x)))
rr <- function(d){
  regs <- .detect_ecospace_regions(d, "annual")
  arr <- fn.read_pred_ecospace_wide(d, "Biomass", "annual", regs, styear, aggregate_by_group = FALSE)
  ri0 <- if(0L %in% as.integer(sub("R", "", dimnames(arr)[[3]]))) which(as.integer(sub("R", "", dimnames(arr)[[3]])) == 0L) else 1L
  common <- match(obs_yrs, as.numeric(dimnames(arr)[[1]]))
  sapply(j_all, function(j){
    col <- match(norm_name(group.names[th$Poolcode[j]]), norm_name(dimnames(arr)[[2]]))
    p <- arr[, col, ri0, 1][common]; o <- obs.ts$ts[, j]; o[o <= 0] <- NA
    ok <- !is.na(o) & !is.na(p) & p > 0; q <- 1
    if(!isTRUE(th$Absolute[j])) q <- mean(p[ok]) / mean(o[ok])
    sqrt(mean((o[ok] - p[ok] / q)^2)) / mean(o[ok]) })
}
R <- sapply(dirs, rr); rownames(R) <- th$Title[j_all]
mem <- R[, -1]
out <- data.frame(series = th$Title[j_all], rrmse_baseline = R[, "base"], rrmse_best = R[, "m1"], rrmse_ens_mean = rowMeans(mem), rrmse_ens_min = apply(mem, 1, min), rrmse_ens_max = apply(mem, 1, max))
out$species <- ifelse(grepl("^GAG", out$series), "Gag", "Red grouper"); out$stanza <- as.integer(sub(".*_age([1-6])_.*", "\\1", out$series)) - 1
out <- out[order(out$species, out$stanza), ]; print(round(out[, 2:6], 3)); 
agg <- function(k) c(baseline = mean(out$rrmse_baseline[k]), best = mean(out$rrmse_best[k]), ens_mean = mean(out$rrmse_ens_mean[k]), ens_min = min(colMeans(mem[out$series[k], , drop = FALSE])), ens_max = max(colMeans(mem[out$series[k], , drop = FALSE])))
print(round(rbind(Gag = agg(out$species == "Gag"), Red = agg(out$species != "Gag"), All = agg(rep(TRUE, nrow(out))), `Gag no stanza5+` = agg(out$species == "Gag" & out$stanza < 5)), 3))
write.csv(out, file.path(U, "plots_R/ts/Reference_stanza_biomass_rrmse.csv"), row.names = FALSE)
