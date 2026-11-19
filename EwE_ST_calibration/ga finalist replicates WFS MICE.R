#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ga finalist replicates WFS MICE.R   (WINDOWS, needs the Ecospace console)
# Re-runs the 37 ensemble members N_REP times each, with exactly the command file the GA used, and
# scores every run with the same objective (temporal + spatial). Gives:
#   * the run-to-run noise of the finalists (the baseline SD of 21 NLL comes from the base run only),
#   * how optimistic the GA score of each member was (winner's curse),
#   * whether the ranking inside the ensemble means anything.
# Output: finalist_replicates.csv with columns id, rep, nll, fitness_ga. Copy it to <RUN>/sens1/ on the
# Mac and run section 9 of ga_discussion_checks.R.
# Cost: 37 x N_REP runs (185 for N_REP = 5), about one hour with 24 workers.
#
# SELF-CONTAINED: it does not need 'setup cal WFS MICE.R'. It only needs a copy of the run folder
# (RUN) with run_info.csv, inputs/ and results/final_ga_runs_ga01.csv, and the GFISHER maps and the
# region areas file that run_info.csv points to (override the paths below if they differ on this PC).
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list = ls()); graphics.off(); gc()

#USER INPUT------------------------------------------------------------------------------------
RUN     <- "C:/path/to/phase6_ga01"             # copy of the run folder (run_info.csv, inputs/, results/)
LABEL   <- "ga01"
N_ENS   <- 37                                   # best 10% of the final population
N_REP   <- 5                                    # replicates per member (3 is the minimum worth running)
workers <- 24
SCRATCH <- "O:/WFS MICE/phase6/ga01/finalist_reps"       # new output folders go here
OUT_CSV <- file.path(RUN, "sens1", "finalist_replicates.csv")
ALT_DRIVES <- c("O:", "E:")                     # the GA wrote to O:; the archive is on E:
OBSMAPS_OVERRIDE      <- NULL                   # e.g. "C:/.../maps/GFISHER/5min/maxn_sdmTMB/mice" if run_info's path is not valid here
REGION_AREAS_OVERRIDE <- NULL                   # same for combined_regions_5min_attributes.csv
CONSOLE_OVERRIDE      <- NULL                   # same for EwEClientConsole.exe

#PACKAGES (the same R4EwE commit the GA used)---------------------------------------------------
ri <- read.csv(file.path(RUN, "run_info.csv")); g <- function(k) ri$value[ri$field == k]
remotes::install_github(paste0("dchagaris/R4EwE@", g("r4ewe_commit")), upgrade = FALSE)
library(R4EwE); library(foreach); library(parallel); library(snow); library(doSNOW)

#SET-UP, rebuilt from run_info.csv and inputs/ (mirrors 'setup cal WFS MICE.R')-----------------------
inp <- file.path(RUN, "inputs")
file.console     <- if (!is.null(CONSOLE_OVERRIDE)) CONSOLE_OVERRIDE else g("ref.console")
styear           <- as.numeric(g("setting.styear")); enyear <- as.numeric(g("setting.enyear"))
nyrs             <- enyear - styear + 1; model_styear <- styear
spatial.weight   <- as.numeric(g("setting.spatial.weight"))
expected_regions <- as.numeric(strsplit(g("setting.expected_regions"), " ")[[1]])
obj.fxn          <- as.numeric(g("setting.obj.fxn")); fit.abs.catch <- identical(g("setting.fit.abs.catch"), "TRUE")

vuls <- read.csv(file.path(inp, g("input.basevul")), row.names = 1)
group.names <- gsub(" ", "_", vuls[, 1])
fleet.names <- trimws(gsub("(t/km^2/year)", "", as.character(read.csv(file.path(inp, g("input.catch")), header = FALSE, nrows = 1)[, -c(1:2)]), fixed = TRUE))
fleet.names <- fleet.names[1:(length(fleet.names) - 1)]

obs.ts <- fn.read_ecosim_timeseries(file.path(inp, g("input.obsts")))
rel <- as.numeric(strsplit(g("setting.relative.types"), " ")[[1]])
obs.ts$ts.head$Absolute[obs.ts$ts.head$Type %in% rel] <- FALSE
sg <- as.numeric(strsplit(g("setting.stanza.groups"), " ")[[1]]); ld <- as.numeric(strsplit(g("setting.lead.stanza"), " ")[[1]])
obs.ts$ts.head$Absolute[obs.ts$ts.head$Type == 104 & obs.ts$ts.head$Poolcode %in% setdiff(sg, ld)] <- FALSE

obsmaps <- if (!is.null(OBSMAPS_OVERRIDE)) OBSMAPS_OVERRIDE else g("ref.obsmaps")
spatial.maps <- strsplit(g("setting.spatial.maps"), " ")[[1]]
spatial.obs <- fn.load_gfisher_sdmTMB(sdm_dir = obsmaps, maps = spatial.maps,
                                      styear = as.numeric(g("setting.spatial.styear")), endyear = as.numeric(g("setting.spatial.endyear")),
                                      cv.weight = identical(g("setting.spatial.cv.weight"), "TRUE"))
region.areas <- fn.read_region_areas(if (!is.null(REGION_AREAS_OVERRIDE)) REGION_AREAS_OVERRIDE else g("ref.region.areas"))

#MEMBERS---------------------------------------------------------------------------------------
runs <- read.csv(file.path(RUN, "results", paste0("final_ga_runs_", LABEL, ".csv")))
runs <- runs[order(runs$fitness), ][seq_len(N_ENS), ]
runs$id <- sub("^(run_g[0-9]+_i[0-9]+).*", "\\1", basename(dirname(gsub("\\\\", "/", runs$cmd_file))))
find_cmd <- function(p) {                       # the GA command file, on O: or on the archive drive
  for (d in ALT_DRIVES) { q <- sub("^[A-Za-z]:", d, p); if (file.exists(q)) return(q) }
  NA_character_
}
runs$cmd_src <- vapply(runs$cmd_file, find_cmd, character(1))
if (anyNA(runs$cmd_src)) stop("command file not found for: ", paste(runs$id[is.na(runs$cmd_src)], collapse = ", "))

#RUN LIST: copy each command file and point the output folder at a new replicate folder------------
runlist <- do.call(rbind, lapply(seq_len(nrow(runs)), function(i) data.frame(
  id = runs$id[i], rep = seq_len(N_REP), fitness_ga = runs$fitness[i], cmd_src = runs$cmd_src[i])))
runlist$dir.out  <- file.path(SCRATCH, sprintf("%s_rep%02d", runlist$id, runlist$rep))
runlist$cmd_file <- file.path(runlist$dir.out, "cmd.txt")
for (i in seq_len(nrow(runlist))) {
  dir.create(runlist$dir.out[i], recursive = TRUE, showWarnings = FALSE)
  x <- readLines(runlist$cmd_src[i], warn = FALSE)
  k <- grep("^<ECOSPACE_OUTPUT_DIR>", x)
  stopifnot(length(k) == 1)
  x[k] <- sprintf("<ECOSPACE_OUTPUT_DIR>, %s, System.String, Updated", gsub("/", "\\\\", runlist$dir.out[i]))
  writeLines(x, runlist$cmd_file[i])
}

#RUN AND SCORE (same call as the sensitivity script)-----------------------------------------------
runlist.LL <- fn.runEwE.parallel(runlist = runlist, obj.fxn = obj.fxn,
                                 cl.export = list("runlist", "obs.ts"),
                                 fit.abs.catch = fit.abs.catch, n_cores = workers)
out <- data.frame(id = runlist.LL$id, rep = runlist.LL$rep, nll = runlist.LL$LL.total, fitness_ga = runlist.LL$fitness_ga)
dir.create(dirname(OUT_CSV), recursive = TRUE, showWarnings = FALSE)
write.csv(out, OUT_CSV, row.names = FALSE)
print(aggregate(nll ~ id, out, function(v) c(mean = mean(v), sd = sd(v))))
cat("median within-member SD:", round(median(tapply(out$nll, out$id, sd)), 1), "(baseline SD 21.2, stop tolerance 32.3)\n")
cat("written:", OUT_CSV, "\n")
