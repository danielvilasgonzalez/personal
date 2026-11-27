#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# check setup phase6.R -- run this FIRST on a new computer. Base R only, takes seconds, changes nothing.
#   source("<repo folder>/check setup phase6.R")
# It lists every folder and file the phase6 scripts read, says which are missing and which environment variable moves each
# folder, and checks the R packages. Where each folder sits on THIS computer is set by environment variables (put them in
# ~/.Renviron, one per line, e.g. EWE_RUN=/Volumes/data/ST_calibration/phase6_ga01), or defaults to folders under your home folder:
#   EWE_RUN   run folder (inputs/, output/, results/, sens1/)   default ~/Work/ST_calibration/phase6_ga01
#   EWE_REPO  this repo folder                                  default ~/Documents/GitHub/personal/EwE_ST_calibration
#   EWE_BASE  folder that holds maxn/ (observed rasters)        default ~/Work/ST_calibration
#   EWE_MAXN  the maxn/ folder itself                           default <EWE_BASE>/maxn
#   EWE_R4    R4EwE source folder (or install the package)      default ~/Documents/GitHub/R4EwE
#   EWE_SDM   sdmTMB maps used by the GA (GFISHER repo)         default ~/Documents/GitHub/GFISHER/output/maps/GFISHER/5min/maxn_sdmTMB/mice
#   EWE_CORES parallel cores                                    default 3
# Returns invisibly the number of missing REQUIRED items; 'run all phase6.R' stops when it is not 0.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
local({
  hm   <- path.expand("~")
  RUN  <- Sys.getenv("EWE_RUN",  file.path(hm, "Work/ST_calibration/phase6_ga01"))
  REPO <- Sys.getenv("EWE_REPO", file.path(hm, "Documents/GitHub/personal/EwE_ST_calibration"))
  BASE <- Sys.getenv("EWE_BASE", file.path(hm, "Work/ST_calibration"))
  MAXN <- Sys.getenv("EWE_MAXN", file.path(BASE, "maxn"))
  R4   <- Sys.getenv("EWE_R4",   file.path(hm, "Documents/GitHub/R4EwE"))
  SDM  <- Sys.getenv("EWE_SDM",  file.path(hm, "Documents/GitHub/GFISHER/output/maps/GFISHER/5min/maxn_sdmTMB/mice"))
  L <- "ga01"
  items <- data.frame(stringsAsFactors = FALSE, need = character(), path = character(), var = character(), what = character())
  add <- function(need, path, var, what) items[nrow(items) + 1, ] <<- list(need, path, var, what)

  # repo (scripts and masks)
  for(f in c("st_calibration_functions.R", "st calibration run phase6.R", "st results phase6.R", "run all phase6.R",
             "fig contribution and parameters phase6.R", "fig convergence and contribution phase6.R", "fig ensemble threshold sensitivity phase6.R",
             "fig rrmse fitted series phase6.R", "fig rrmse reference phase6.R", "fig ensemble cutoff assessment phase6.R", "table nll composition phase6.R",
             "combined_regions_5min.asc", "combined_regions_5min_attributes.csv", "age0_survey_regions_5min_mod.asc"))
    add("required", file.path(REPO, f), "EWE_REPO", "repo file")
  # run folder
  for(f in c("inputs/ts_mice_v5_discards_ecospace_regions_sedar105.csv", "inputs/WFS-MICE EwE 2.13 v67020-Catch.csv",
             "inputs/WFS-MICE EwE 2.13 v67020-Vulnerabilities.csv", "inputs/WFS-MICE EwE 2.13 v67020-Dispersal.csv",
             "inputs/WFS-MICE env and red tide response shape pars.csv", "run_info.csv",
             paste0("results/", c(paste0("final_ga_runs_", L, ".csv"), paste0("final_ga_pop", L, ".csv"), paste0("parvec_", L, ".csv"),
                                  paste0("ga_results_", L, ".csv"), paste0("ga_evals_", L, ".csv"))),
             "sens1/LL_rescored.xlsx", "output/run_base/Ecospace_Annual_Average_Biomass.csv"))
    add("required", file.path(RUN, f), "EWE_RUN", "run folder")
  add("required", file.path(RUN, "output/gen30_small"), "EWE_RUN", "folder with the 370 candidate model outputs")
  add("optional", file.path(RUN, "sens1/finalist_replicates.csv"), "EWE_RUN", "section 5 of the results script (made on the Windows machine)")
  add("optional", file.path(RUN, "sens1/revert_one_type.csv"), "EWE_RUN", "parameter-type effects, if made on the Windows machine")
  # outside the run folder
  add("required", MAXN, "EWE_MAXN", "observed gag / red grouper rasters (.asc)")
  add("required", SDM,  "EWE_SDM",  "sdmTMB maps (GFISHER repo), used when re-scoring the spatial NLL")
  for(f in c("globals.R", "utils.R", "input_output.R", "Ecospace_objective_functions.R", "Spatial_likelihood.R", "fn.ecospace_gif.R"))
    add(if(requireNamespace("R4EwE", quietly = TRUE)) "optional" else "required", file.path(R4, "R", f), "EWE_R4", "R4EwE source (not needed if the package is installed)")

  items$exists <- file.exists(items$path)
  cat("\n==== phase6 setup check on", Sys.info()[["nodename"]], "(", R.version.string, ") ====\n")
  cat("home folder:", hm, "\n\n")
  cat(sprintf("%-8s %-9s %s\n", "status", "need", "path"), sep = "")
  for(i in seq_len(nrow(items))) cat(sprintf("%-8s %-9s %s\n", if(items$exists[i]) "ok" else "MISSING", items$need[i], items$path[i]), sep = "")

  # contents of the big folders
  if(dir.exists(file.path(RUN, "output/gen30_small"))){
    n <- length(list.dirs(file.path(RUN, "output/gen30_small"), recursive = FALSE))
    cat("\ncandidate folders in output/gen30_small:", n, if(n == 370) "(expected 370: ok)\n" else "(expected 370: CHECK)\n")
  }
  if(dir.exists(MAXN)) cat("rasters in", MAXN, ":", length(list.files(MAXN, pattern = "\\.asc$")), "\n")
  if(dir.exists(SDM))  cat("map files in", SDM, ":", length(list.files(SDM, recursive = TRUE)), "\n")

  # R packages
  pk <- c(required = "data.table", required = "ggplot2", required = "patchwork", required = "scales", required = "readxl",
          required = "terra", required = "ranger", optional = "RColorBrewer", optional = "pdftools", optional = "R4EwE")
  have <- vapply(pk, requireNamespace, logical(1), quietly = TRUE)
  cat("\nR packages:\n"); for(i in seq_along(pk)) cat(sprintf("%-8s %-9s %s\n", if(have[i]) "ok" else "MISSING", names(pk)[i], pk[i]), sep = "")
  if(any(!have & names(pk) == "required")) cat("install with: install.packages(c(", paste0('"', pk[!have & names(pk) == "required"], '"', collapse = ", "), "))\n")

  # run_info.csv holds paths written by the machine that made the run: show the ones that do not exist here
  ri_f <- file.path(RUN, "run_info.csv")
  if(file.exists(ri_f)){
    ri <- tryCatch(read.csv(ri_f, stringsAsFactors = FALSE), error = function(e) NULL)
    if(!is.null(ri) && all(c("field", "value") %in% names(ri))){
      v <- ri$value[grepl("^([A-Za-z]:[/\\\\]|/)", ri$value)]
      f <- ri$field[match(v, ri$value)]; bad <- !file.exists(v)
      if(any(bad)){ cat("\nrun_info.csv entries that point to paths missing on this computer (check they are not read by the scripts you run):\n")
        for(k in which(bad)) cat("  ", f[k], "->", v[k], "\n") } else cat("\nrun_info.csv: every path it lists exists here.\n")
    }
  }
  nmiss <- sum(!items$exists & items$need == "required") + sum(!have & names(pk) == "required")
  cat("\n", if(nmiss == 0) "READY: nothing required is missing." else paste0(nmiss, " required item(s) missing: set the EWE_* variable named in the table, or copy the folder, then run this check again."), "\n", sep = "")
  assign(".phase6_missing", nmiss, envir = globalenv())
  invisible(nmiss)
})
