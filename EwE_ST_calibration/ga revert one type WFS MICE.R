#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ga revert one type WFS MICE.R   (WINDOWS, needs the Ecospace console)
# Real, signed effect of each parameter type on the NLL (panel B of the contribution figure), with no
# surrogate model. For each of the best N_MEM GA members it runs Ecospace with the member's own command
# file, changed in one of these ways:
#   baseline   all parameter tags removed          -> the baseline model (NLL 9,984 on the gag + red grouper basis)
#   full       command file as the GA wrote it     -> the fitted member (control, should reproduce its GA NLL)
#   rev_<type> only that type's tags removed       -> "fitted model, but this type back at baseline"
#   only_<type> only that type's tags kept         -> "baseline model, but this type fitted"
# effect of a type = NLL(rev_<type>) - NLL(full)    (what is lost without it; > 0 means the type helps)
#                    NLL(baseline) - NLL(only_<type>) (what it gains alone)
# If the two differ, the types interact; report both. Types: vul, redtide, env, flt, disp (as in panel B).
#
# HOW THE TAGS ARE FOUND: fn.parvec2cmd writes the command file as cmd_base followed by the parameter tags in
# the order  vulnerabilities, environmental, red tide, dispersal, mediation, fleet.  The number of tag lines of each
# type follows from parvec_ga01.csv (one line per vulnerability, dispersal and fleet parameter, one per
# environmental or red tide parameter group). The script checks every tag prefix before it runs anything.
#
# Output: revert_one_type.csv (id, member, variant, nll, dir.out). Copy it to <RUN>/sens1/ on the Mac. Keep the
# output folders in SCRATCH: the gag + red grouper NLL of each run is re-scored from them with the same code
# as the 37-member matrix (rescore_members_temporal.R), because LL.total also holds the other groups.
# Cost: (2 + 2 x 5) x N_MEM runs. N_MEM = 37 gives 444 runs, about 2.5 h with 24 workers. Set N_MEM <- 1 for a
# first check (12 runs).
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list = ls()); graphics.off(); gc()

#USER INPUT------------------------------------------------------------------------------------
RUN     <- "C:/path/to/phase6_ga01"             # copy of the run folder (run_info.csv, inputs/, results/)
LABEL   <- "ga01"
N_MEM   <- 37                                   # best members to test (1 = best model only)
workers <- 24
SCRATCH <- "O:/WFS MICE/phase6/ga01/revert_one_type"
OUT_CSV <- file.path(RUN, "sens1", "revert_one_type.csv")
ALT_DRIVES <- c("O:", "E:")
OBSMAPS_OVERRIDE <- NULL; REGION_AREAS_OVERRIDE <- NULL; CONSOLE_OVERRIDE <- NULL

#PACKAGES AND SET-UP (identical to 'ga finalist replicates WFS MICE.R')--------------------------------
ri <- read.csv(file.path(RUN, "run_info.csv")); g <- function(k) ri$value[ri$field == k]
remotes::install_github(paste0("dchagaris/R4EwE@", g("r4ewe_commit")), upgrade = FALSE)
library(R4EwE); library(foreach); library(parallel); library(snow); library(doSNOW)

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

#TAG BLOCKS: how many command-file lines each parameter type owns--------------------------------------
pv <- read.csv(file.path(RUN, "results", paste0("parvec_", LABEL, ".csv")), check.names = FALSE); names(pv)[1] <- "par"
type_of <- ifelse(grepl("^vul_", pv$par), "vul", ifelse(grepl("^rt", pv$par), "redtide",
           ifelse(grepl("^env", pv$par), "env", ifelse(grepl("^flt_", pv$par), "flt", "disp"))))
if (any(grepl("^med", pv$par))) stop("mediation parameters present: add a 'med' block to the order below")
n_lines <- c(vul     = sum(type_of == "vul"),
             env     = length(unique(pv$par.groups[type_of == "env"])),
             redtide = length(unique(pv$par.groups[type_of == "redtide"])),
             disp    = sum(type_of == "disp"),
             flt     = sum(type_of == "flt"))
n_lines <- n_lines[c("vul", "env", "redtide", "disp", "flt")]           # order written by fn.parvec2cmd
prefix <- c(vul = "<ECOSIM_VULNERABILITIES_INDEXED>", env = "<ECOSPACE_ENVIRONMENTAL_RESPONSE_INDEXED>",
            redtide = "<ECOSPACE_ENVIRONMENTAL_RESPONSE_INDEXED>", disp = "<ECOSPACE_DISPERSAL_RATE_INDEXED>",
            flt = "<ECOSPACE_FLEET_")                                  # EFFECTIVEPOWER / TOTAL_EFFORTMULT
print(n_lines)
n_tag <- sum(n_lines)

#MEMBERS-----------------------------------------------------------------------------------------
runs <- read.csv(file.path(RUN, "results", paste0("final_ga_runs_", LABEL, ".csv")))
runs <- runs[order(runs$fitness), ][seq_len(N_MEM), ]
runs$id <- sub("^(run_g[0-9]+_i[0-9]+).*", "\\1", basename(dirname(gsub("\\\\", "/", runs$cmd_file))))
find_cmd <- function(p) { for (d in ALT_DRIVES) { q <- sub("^[A-Za-z]:", d, p); if (file.exists(q)) return(q) }; NA_character_ }
runs$cmd_src <- vapply(runs$cmd_file, find_cmd, character(1))
if (anyNA(runs$cmd_src)) stop("command file not found for: ", paste(runs$id[is.na(runs$cmd_src)], collapse = ", "))

#BUILD THE COMMAND FILES-------------------------------------------------------------------------
rows <- list()
for (i in seq_len(nrow(runs))) {
  x <- readLines(runs$cmd_src[i], warn = FALSE)
  nb <- length(x) - n_tag; stopifnot(nb > 0)
  base <- x[seq_len(nb)]; tags <- x[(nb + 1):length(x)]
  blk <- rep(names(n_lines), n_lines)                                  # type of each tag line
  # check: every tag line starts with the prefix of the type it is assigned to
  ok <- mapply(function(l, b) startsWith(l, prefix[[b]]), tags, blk)
  if (!all(ok)) stop("tag order does not match for ", runs$id[i], ": line(s) ", paste(which(!ok), collapse = ","))
  mk <- function(keep_types){ c(base, tags[blk %in% keep_types]) }
  variants <- c(list(baseline = mk(character()), full = mk(names(n_lines))),
                setNames(lapply(names(n_lines), function(t) mk(setdiff(names(n_lines), t))), paste0("rev_", names(n_lines))),
                setNames(lapply(names(n_lines), function(t) mk(t)), paste0("only_", names(n_lines))))
  for (v in names(variants)) {
    id  <- paste0(runs$id[i], "__", v)
    out <- file.path(SCRATCH, id); dir.create(out, recursive = TRUE, showWarnings = FALSE)
    y <- variants[[v]]; k <- grep("^<ECOSPACE_OUTPUT_DIR>", y); stopifnot(length(k) == 1)
    y[k] <- sprintf("<ECOSPACE_OUTPUT_DIR>, %s, System.String, Updated", gsub("/", "\\\\", out))
    writeLines(y, file.path(out, "cmd.txt"))
    rows[[length(rows) + 1]] <- data.frame(id = id, rep = 1, fitness_ga = runs$fitness[i], member = runs$id[i], variant = v,
                                           dir.out = out, cmd_file = file.path(out, "cmd.txt"))
  }
}
runlist <- do.call(rbind, rows)
message(nrow(runlist), " runs: ", length(unique(runlist$member)), " members x ", length(unique(runlist$variant)), " variants")

#RUN AND SCORE-----------------------------------------------------------------------------------
runlist.LL <- fn.runEwE.parallel(runlist = runlist, obj.fxn = obj.fxn, cl.export = list("runlist", "obs.ts"),
                                 fit.abs.catch = fit.abs.catch, n_cores = workers)
out <- merge(runlist[, c("id", "member", "variant", "dir.out", "fitness_ga")],
             data.frame(id = runlist.LL$id, nll = runlist.LL$LL.total), by = "id")
dir.create(dirname(OUT_CSV), recursive = TRUE, showWarnings = FALSE)
write.csv(out, OUT_CSV, row.names = FALSE)
print(aggregate(nll ~ variant, out, mean))
cat("control: mean |NLL(full) - GA fitness| =", round(mean(abs(out$nll[out$variant == "full"] - out$fitness_ga[out$variant == "full"])), 1),
    "(about the run-to-run noise, SD 21)\nwritten:", OUT_CSV, "\n")
