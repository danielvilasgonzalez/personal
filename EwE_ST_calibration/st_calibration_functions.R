#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# st_calibration_functions.R
#
# FUNCTION LIBRARY ONLY -- no config, no file paths, nothing gets run just by sourcing
# this file. Every plotting/reading/aggregation function used by the WFS-MICE GA
# calibration workflow lives here.
#
# Usage from a separate run script:
#   source("/Users/daniel/Downloads/st_calibration_functions.R")
#   ... your config (paths, ga_runs, etc.) and actual function calls go in that
#   separate script, not here -- see st_calibration_run.R for a complete example.
#
# Required packages: ggplot2, patchwork, data.table, scales. Install any missing ones
# with install.packages(c("ggplot2","patchwork","data.table","scales")) before sourcing.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# FIGURES PLAN
#
# - Did model calibration improve the fit to the data?
#     Figure 1: Objective function (or total residual error) before and after calibration.
#     -> PART 3 (GA convergence: min/max NLL by generation) partly covers this; the
#        before/after framing itself would need a pre-calibration baseline run to
#        compare against, which isn't wired in yet.
#
# - TEMPORAL. Does the calibrated model reproduce observed time series?
#     Figure 2: O vs. P biomass time series (red and gag grouper).
#       -> plot_obs_vs_pred() / plot_ga_ensemble_faceted() / plot_ga_generation_best()
#     Figure 3: O vs. P landings and fishing mortality.
#       -> plot_obs_vs_pred(vars = "F") etc. (Catch is wired up; Landings is not yet --
#          see the VAR_TYPE_MAP note in PART 1 on why fleet-disaggregated types need a
#          different predicted-file shape to match correctly)
#     Figure X: RRMSE (relative RMSE) summary across groups/regions.
#       -> not yet built; a natural next addition once the O-vs-P panels above look right
#
# - SPATIAL. Does the model reproduce observed spatial distributions?
#     Figure 4: O vs. P biomass maps (selected years).
#     Figure 5: Spatial residual maps (Observed - Predicted) -- where does the model
#       perform well, and where are the remaining mismatches?
#       -> blocked on Ecospace ASCII grid exports (one per year), which aren't
#          available yet; will build once those exist (see PART 5 placeholder below)
#
# - RT. What are the estimated impacts of red tide on grouper mortality?
#     Figure 6: Estimated red tide-associated natural mortality through time.
#       -> not yet started
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# st_calibration_plots.R  (consolidated -- no source() needed)
#
# Everything in one file: the shared Ecospace obs-vs-pred helpers, the GA-ensemble
# helpers, and your own GA convergence / vulnerability-distribution diagnostic plots.
# Just run this whole script top to bottom (or source it) -- no other files needed.
#
# Sections:
#   PART 1 -- shared helpers + plot_obs_vs_pred()      (was plot_ecospace_obs_vs_pred.R)
#   PART 2 -- GA-ensemble helpers + plot_ga_ensemble() + plot_ga_ensemble_faceted() +
#             plot_ga_generation_best()                 (was plot_ga_ensemble.R)
#   PART 3 -- your calibration diagnostic plots (GA convergence, vulnerability spread)
#   PART 4 -- example / actual run calls
#   PART 5 -- spatial O-vs-P maps (Figures 4/5) -- BLOCKED, see note below
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# GA convergence diagnostic helper (used by PART 3's min/median/max NLL multiplot)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @title One GA-convergence panel: a primary NLL statistic + SD NLL on a secondary axis.
#' @description Builds one ggplot panel with `primary_col` (e.g. "min_LL", "median_LL",
#'   "max_LL") plotted against generation, plus `sd_col` overlaid on a secondary axis.
#'   `sd_max` maps directly to the secondary axis's displayed top (0 maps to the
#'   primary axis's bottom) -- so the secondary axis always shows exactly [0, sd_max],
#'   not some multiple of it. When the actual SD data is small relative to `sd_max`,
#'   the SD line naturally reads as a compressed band near the bottom, without needing
#'   a separate "band fraction" setting. Used three times (min/median/max) to build the
#'   side-by-side GA convergence multiplot.
#' @param ga2 data.frame with columns `gen`, `primary_col`, `sd_col`.
#' @param primary_col Column name for the primary NLL statistic (e.g. "min_LL").
#' @param primary_label Legend label / axis title for the primary series, e.g. "minimum NLL".
#' @param primary_color Line/point color for the primary series.
#' @param sd_col Column name for the SD series. Default "sd_LL".
#' @param sd_label Legend label for the SD series. Default "sd NLL".
#' @param sd_color Line/point color for the SD series. Default "#C0392B".
#' @param sd_max Value that maps to the TOP of the secondary (SD) axis -- the axis will
#'   display exactly [0, sd_max]. Default 150000. Every sd_col value is still plotted
#'   at its exact proportional position (nothing is capped/dropped as long as it's
#'   below sd_max -- see the message this function prints if any value exceeds it).
#'   Pass NULL to fall back to the data's own max (so the SD line's own highest point
#'   sits exactly at the axis top).
#' @param gen_breaks Optional numeric vector of x-axis breaks. NULL (default) = ggplot's
#'   own default breaks.
#' @param primary_min Lower bound for the primary axis. Default 0 (NLL values are never
#'   negative, so 0 is the sensible default floor). Pass a higher value (e.g. 4000) to
#'   zoom into a narrower window around the data when the series varies over a small
#'   range relative to its own magnitude (e.g. minimum NLL declining from ~5700 to
#'   ~4700 across generations reads as barely-visible detail on a 0-6000 axis, but is
#'   clearly visible zoomed to ~4000-6000).
#' @param legend_position,legend_justification Passed straight to theme(); default
#'   places the legend inside the panel, top-right.
#' @return A ggplot object.
#' @export
plot_nll_convergence_panel <- function(ga2, primary_col, primary_label, primary_color,
                                       sd_col = "sd_LL", sd_label = "sd NLL", sd_color = "#C0392B",
                                       sd_max = 150000, gen_breaks = NULL,
                                       primary_min = 0,
                                       legend_position = c(0.95, 0.95),
                                       legend_justification = c(1, 1)){
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
  
  primary_vals <- ga2[[primary_col]]
  sd_vals <- ga2[[sd_col]]
  primary_range <- range(primary_vals, na.rm = TRUE)
  if(primary_min > primary_range[1])
    message("plot_nll_convergence_panel(): primary_min (", primary_min, ") is ABOVE the actual ",
            "minimum of '", primary_col, "' (", signif(primary_range[1], 6), ") -- the lowest ",
            "point(s) will be clipped from view (zoomed past), not dropped from the data.")
  sd_max_actual <- if(is.null(sd_max)) max(sd_vals, na.rm = TRUE) else sd_max
  if(!is.null(sd_max) && sd_max < max(sd_vals, na.rm = TRUE))
    message("plot_nll_convergence_panel(): sd_max (", sd_max, ") is below the actual max of '",
            sd_col, "' (", signif(max(sd_vals, na.rm = TRUE), 6), ") -- the highest point(s) ",
            "will be drawn ABOVE the top of the panel (not clipped/dropped from the data, but off ",
            "the visible plot). Raise sd_max, or pass sd_max = NULL to use the data's own max.")
  
  # Primary axis spans [primary_min, primary_top] -- default primary_min=0 (NLL values
  # are never negative), can be raised to zoom into a narrower window.
  #
  # sd_max now maps DIRECTLY to primary_top (the axis TOP), and 0 maps to primary_min --
  # this is the actual fix for "sd_max axis limit not applied": the previous version
  # mapped sd_max to band_top (only band_frac of the way up the primary axis), but
  # sec_axis() ALWAYS displays the range corresponding to the FULL primary axis
  # [primary_min, primary_top], not just the "band" portion -- so the displayed top came
  # out to sd_max/band_frac (e.g. 150000/0.3 = 500000), not sd_max itself. Mapping
  # sd_max straight to primary_top makes the secondary axis show EXACTLY [0, sd_max],
  # no band_frac math involved. The SD line still reads as visually confined near the
  # bottom whenever the actual data is small relative to sd_max -- that falls out
  # naturally from this same linear map, no separate band parameter needed.
  primary_top <- primary_range[2] * 1.05
  sf   <- (primary_top - primary_min) / sd_max_actual
  offs <- primary_min
  
  df <- ga2
  df$.primary   <- primary_vals
  df$.sd_scaled <- sd_vals * sf + offs
  
  colors <- stats::setNames(c(primary_color, sd_color), c(primary_label, sd_label))
  y_limits <- c(primary_min, primary_top)
  
  # since we're back to scale_y_continuous(limits=) (needed for sec_axis to respect
  # primary_min/sd_max -- see the comment on that call below), explicitly check for
  # any point that would actually be dropped, rather than letting it happen silently
  dropped_primary <- sum(primary_vals < y_limits[1] | primary_vals > y_limits[2], na.rm = TRUE)
  dropped_sd <- sum(df$.sd_scaled < y_limits[1] | df$.sd_scaled > y_limits[2], na.rm = TRUE)
  if(dropped_primary > 0 || dropped_sd > 0)
    message("plot_nll_convergence_panel(): ", dropped_primary, " '", primary_col, "' point(s) and ",
            dropped_sd, " '", sd_col, "' point(s) fall outside [", signif(y_limits[1], 6), ", ",
            signif(y_limits[2], 6), "] and will be DROPPED (not just clipped from view) -- ",
            "widen primary_min/primary_top or sd_max if this matters.")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = gen)) +
    ggplot2::geom_line(ggplot2::aes(y = .primary, colour = primary_label),
                       linewidth = 1, alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(y = .primary, colour = primary_label),
                        size = 2, alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = .sd_scaled, colour = sd_label),
                       linewidth = 1, linetype = "22", alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(y = .sd_scaled, colour = sd_label),
                        shape = 17, size = 2, alpha = 0.7) +
    ggplot2::scale_colour_manual(values = colors, name = NULL) +
    # scale_y_continuous(limits=), NOT coord_cartesian() -- this is what makes sec_axis()
    # actually respect primary_min/sd_max. coord_cartesian() only zooms the VIEW; it
    # does not change what range sec_axis() computes its own displayed ticks from, so
    # the secondary axis was silently ignoring sd_max even though sd_max correctly
    # affected where the SD line itself got drawn (that transform happens earlier, in
    # the sf/offs calculation above, independent of which zoom mechanism is used here).
    # scale_y_continuous(limits=) DOES tie sec_axis to the same range, at the cost of
    # dropping any point outside it -- safe here because primary_min/sd_max are chosen
    # generously enough (see the run script's own comments) that nothing should
    # actually fall outside [primary_min, primary_top] in the first place.
    ggplot2::scale_y_continuous(
      name = primary_label,
      limits = y_limits,
      sec.axis = ggplot2::sec_axis(~ (. - offs) / sf, name = sd_label)
    ) +
    ggplot2::labs(x = "generation") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face = "bold"),
      axis.title.y.right = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(color = "black"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = legend_position,
      legend.justification = legend_justification,
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.width = grid::unit(1.2, "cm"),
      legend.background = ggplot2::element_blank(),
      aspect.ratio = 1
    ) +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(linewidth = 1, size = 3)))
  
  if(!is.null(gen_breaks)) p <- p + ggplot2::scale_x_continuous(breaks = gen_breaks)
  
  p
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PART 1: shared Ecospace obs-vs-pred helpers + plot_obs_vs_pred()
#
# Self-contained observed-vs-predicted plotting for an EwE Ecospace run (no R4EwE
# dependency). Reads: (1) "Ecospace_Annual_Average_<Var>.csv" (whole-domain, Year x
# group) and its per-region siblings "Ecospace_Annual_Average_Region_<n>_<Var>.csv",
# and (2) one "ts_*.csv" reference-timeseries file holding observed series for all
# variables, distinguished by its "Type" column.
#
# VARIABLE ROUTING: VAR_TYPE_MAP maps each predicted variable to its observed "Type"
# codes and Pool-code column. Only Biomass (Type 0/1) and Catch (Type 6/61/-6) are
# wired up; F is derived as Catch/Biomass. Landings/discards would need a
# fleet-disaggregated predicted file to match correctly.
#
# REGION HANDLING: each observed series carries a Region code ("0" = whole domain, or
# a comma-list). For each distinct (Pool code, Region), loads the matching predicted
# file and sums across regions for multi-region series. Predicted files are cached
# after first read.
#
# MULTIPLE RUNS: plot_obs_vs_pred() takes a named `runs` vector of directories and
# overlays one predicted line per run per panel. The first run is the rescaling
# reference for relative obs series.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Which observed Type codes belong to which predicted variable, which Pool code column
#' identifies the species/group for that type, and the y-axis label to use.
#' group_via = "Poolcode1" -> use the (possibly multi-value) Pool code 1 list column.
#' group_via = "Poolcode2" -> use the single-value Pool code 2 column (e.g. for
#'   fleet x group series where Pool code 1 is the fleet) -- not currently used by the
#'   default entries below, but supported by build_group_id() for future variables.
VAR_TYPE_MAP <- list(
  Biomass = list(types = c(0L, 1L),        group_via = "Poolcode1",
                 y_lab = "Biomass (t/km\u00b2)"),
  Catch   = list(types = c(6L, 61L, -6L),  group_via = "Poolcode1",
                 y_lab = "Catch (t/km\u00b2)"),
  # F has no "Ecospace_Annual_Average_F.csv" export -- EwE convention (matching the
  # original R4EwE code) is F = Catch / Biomass, computed from the two files above
  # rather than read from its own file (see build_predicted_vector_generic()).
  F       = list(types = c(4L, 104L),      group_via = "Poolcode1",
                 y_lab = "Fishing mortality F (Catch / Biomass, yr\u207b\u00b9)",
                 derived_from = c("Catch", "Biomass"))
)


#' @title Read an Ecospace "Annual Average <Var>" export (domain-wide or per-region).
#' @description Skips the variable-length metadata header block (everything before the
#'   line starting with "Year,"), reads the Year x group data table, pulls StartYear out
#'   of the metadata so relative model Year (1, 2, 3, ...) can be converted to calendar
#'   year, and returns a data.frame with a `year` column (calendar year) plus one column
#'   per group, in group-number order. EwE writes Ecospace columns in Ecopath
#'   group-number order with no gaps, so column position doubles as the pool code:
#'   `group_names[pc + 1]` (see below) gives the column name for pool code `pc`.
#' @param path Path to an Ecospace_Annual_Average[_Region_<n>]_<Var>.csv file.
#' @return list(data = data.frame(year, <group columns...>), start_year = integer,
#'   group_names = character vector indexed by pool code + 1, i.e. group_names[pc + 1]
#'   is the column name for pool code pc; group_names[1] is an unused "" placeholder).
read_ecospace_output <- function(path){
  
  raw <- readLines(path, warn = FALSE)
  
  sy_line <- grep("^StartYear,", raw, value = TRUE)
  if(length(sy_line) == 0)
    stop("Could not find 'StartYear' in metadata header of ", path)
  start_year <- as.integer(gsub("^StartYear,", "", sy_line[1]))
  
  hdr_idx <- grep("^Year,", raw)
  if(length(hdr_idx) == 0)
    stop("Could not find the 'Year,...' data header row in ", path)
  hdr_idx <- hdr_idx[1]
  
  dat <- utils::read.csv(path, skip = hdr_idx - 1, check.names = FALSE,
                         stringsAsFactors = FALSE)
  dat$year <- start_year + dat$Year - 1L
  dat$Year <- NULL
  dat <- dat[, c("year", setdiff(names(dat), "year"))]
  
  group_cols  <- setdiff(names(dat), "year")
  group_names <- c("", group_cols)   # 1-indexed: group_names[pc + 1] = name of pool code pc
  names(group_names) <- NULL
  
  message("read_ecospace_output(): ", basename(path), " -- ", length(group_cols),
          " groups, years ", min(dat$year), "-", max(dat$year))
  
  list(data = dat, start_year = start_year, group_names = group_names)
}
read_ecospace_biomass <- read_ecospace_output  # backward-compatible alias


#' @title Read an EwE reference-timeseries ("ts_*.csv") file.
#' @description Parses the standard EwE timeseries format: a "Title" column-name row,
#'   then "Weight" / "Pool code 1" / "Pool code 2" / "Type" / "Region" header rows, then
#'   Year x series data. Pool code / Region cells may hold a comma-separated list (e.g.
#'   "5,6,7,8,9") for a series that aggregates multiple pools/regions; these are parsed
#'   into list-columns. Holds series for every variable together (Type distinguishes
#'   them; see VAR_TYPE_MAP).
#' @param path Path to the ts_*.csv file.
#' @return list(ts.head = data.frame(one row per series: Title, Weight, Poolcode1 (raw
#'   string), Poolcodes (list), Poolcode2, Type, Region (raw string), Regions (list),
#'   Absolute), ts = data.frame(year, <series columns...>)).
read_ewe_timeseries <- function(path){
  
  raw <- utils::read.csv(path, header = FALSE, check.names = FALSE,
                         stringsAsFactors = FALSE, colClasses = "character")
  
  row_label <- trimws(raw[[1]])
  hdr_rows  <- c(Title = which(row_label == "Title")[1],
                 Weight = which(row_label == "Weight")[1],
                 Pool1  = which(row_label == "Pool code 1")[1],
                 Pool2  = which(row_label == "Pool code 2")[1],
                 Type   = which(row_label == "Type")[1],
                 Region = which(row_label == "Region")[1])
  if(any(is.na(hdr_rows)))
    stop("Missing expected header row(s) in ", path, ": ",
         paste(names(hdr_rows)[is.na(hdr_rows)], collapse = ", "))
  
  data_start <- max(hdr_rows) + 1L
  series_cols <- 2:ncol(raw)  # column 1 is the row label
  
  split_codes <- function(x) lapply(strsplit(x, ","), function(v) as.integer(trimws(v)))
  
  ts.head <- data.frame(
    Title     = as.character(raw[hdr_rows["Title"],  series_cols]),
    Weight    = suppressWarnings(as.numeric(raw[hdr_rows["Weight"], series_cols])),
    Poolcode1 = as.character(raw[hdr_rows["Pool1"],  series_cols]),
    Poolcode2 = suppressWarnings(as.integer(raw[hdr_rows["Pool2"], series_cols])),
    Type      = suppressWarnings(as.integer(raw[hdr_rows["Type"],  series_cols])),
    Region    = as.character(raw[hdr_rows["Region"], series_cols]),
    stringsAsFactors = FALSE
  )
  ts.head$Poolcodes <- split_codes(ts.head$Poolcode1)
  ts.head$Regions    <- split_codes(ts.head$Region)
  # "Absolute" only really disambiguates Type 0 vs 1 (relative vs absolute biomass);
  # for other variables (catch, etc.) obs are treated as relative (rescaled to pred)
  # unless you extend this rule for a newly-added variable.
  ts.head$Absolute   <- ts.head$Type %in% c(1L)
  
  yrs <- suppressWarnings(as.numeric(raw[[1]][data_start:nrow(raw)]))
  keep <- !is.na(yrs)
  vals <- as.data.frame(lapply(raw[data_start:nrow(raw), series_cols][keep, , drop = FALSE],
                               function(x) suppressWarnings(as.numeric(x))))
  names(vals) <- ts.head$Title
  ts <- cbind(year = yrs[keep], vals)
  
  message("read_ewe_timeseries(): ", nrow(ts.head), " series, years ",
          min(ts$year), "-", max(ts$year))
  
  list(ts.head = ts.head, ts = ts)
}


#' @title Get (and cache) the predicted Ecospace output file for one variable + region.
#' @description Region 0 resolves to
#'   `file.path(pred_dir, sprintf("Ecospace_Annual_Average_%s.csv", var))`; any other
#'   region `<n>` resolves to
#'   `file.path(pred_dir, sprintf("Ecospace_Annual_Average_Region_%d_%s.csv", n, var))`.
#'   Falls back to a case/zero-padding-insensitive filename match if the exact name isn't
#'   found. BOTH successful reads and misses are cached in `cache` (an environment, keyed
#'   by "<var>__<region>", using a FALSE sentinel for misses so a missing file is only
#'   checked -- and logged -- once, not once per panel that happens to reference it).
#' @param region Integer region code (0 = whole domain).
#' @param var Variable name, e.g. "Biomass" or "Catch" (must match the file's <Var> token).
#' @param pred_dir Directory containing the whole-domain file and per-region siblings.
#' @param cache An environment used as a memoization cache across calls.
#' @return The list returned by read_ecospace_output(), or NULL (silently, after the
#'   first check/log) if the expected file does not exist on disk.
get_region_output <- function(region, var, pred_dir, cache){
  key <- paste0(var, "__", region)
  
  if(exists(key, envir = cache, inherits = FALSE)){
    val <- get(key, envir = cache, inherits = FALSE)
    if(identical(val, FALSE)) return(NULL)  # cached miss -- no re-check, no re-log
    return(val)
  }
  
  path <- if(region == 0L){
    file.path(pred_dir, sprintf("Ecospace_Annual_Average_%s.csv", var))
  } else {
    file.path(pred_dir, sprintf("Ecospace_Annual_Average_Region_%d_%s.csv", region, var))
  }
  
  if(!file.exists(path)){
    # exact name missing -- try a looser match in case of zero-padding / case
    # differences (e.g. "Region_06_" or "region_6_") before giving up
    pat <- if(region == 0L)
      sprintf("^Ecospace_Annual_Average_%s\\.csv$", var)
    else
      sprintf("Region_0*%d_%s\\.csv$", region, var)
    candidates <- list.files(pred_dir, pattern = pat, full.names = TRUE, ignore.case = TRUE)
    if(length(candidates) == 1) path <- candidates[1]
  }
  
  if(!file.exists(path)){
    nearby <- tryCatch(
      list.files(pred_dir, pattern = sprintf("Ecospace_Annual_Average.*%s\\.csv$", var),
                 ignore.case = TRUE),
      error = function(e) character(0))
    message("  [missing] predicted ", var, " file for region ", region, " not found: ", path)
    if(!dir.exists(pred_dir)){
      message("    pred_dir does not exist: ", pred_dir)
    } else if(length(nearby) == 0){
      message("    no Ecospace_Annual_Average*_", var, ".csv files found in pred_dir at all: ", pred_dir)
    } else {
      message("    files matching that pattern in pred_dir: ", paste(nearby, collapse = ", "))
    }
    assign(key, FALSE, envir = cache)  # cache the miss so this candidate/var/region is
    # only ever checked and logged once
    return(NULL)
  }
  
  pr <- read_ecospace_output(path)
  assign(key, pr, envir = cache)
  pr
}
get_region_pred <- get_region_output  # backward-compatible alias (old 3-arg signature
# dropped -- var is now required; see below)


#' @title Build the predicted vector for a set of pool codes summed over regions.
#' @description For each region in `region_codes`, loads (or reuses from cache) that
#'   region's predicted file for `var`, extracts the columns matching `pool_codes`
#'   (`group_names[pool_codes + 1]`), sums across pool codes, then sums the resulting
#'   per-region vectors together (values are additive across space). Region 0 alone
#'   means "whole domain" and uses the domain-average file directly.
#' @return list(years, vec, used_regions (regions actually contributing),
#'   missing_regions (regions whose file was not found or lacked the needed columns)).
build_predicted_vector <- function(pool_codes, region_codes, var, pred_dir,
                                   cache, group_names_lookup){
  if(length(region_codes) == 0 || all(is.na(region_codes))) region_codes <- 0L
  region_codes <- unique(region_codes)
  
  years <- NULL
  total_vec <- NULL
  used_regions <- integer(0)
  missing_regions <- integer(0)
  
  for(r in region_codes){
    pr <- get_region_output(r, var, pred_dir, cache)
    if(is.null(pr)){ missing_regions <- c(missing_regions, r); next }
    
    pred_cols <- group_names_lookup(pr, pool_codes)
    miss_cols <- setdiff(pred_cols, names(pr$data))
    if(length(miss_cols) > 0){
      message("  [missing] region ", r, " predicted ", var, " file has no column(s): ",
              paste(miss_cols, collapse = ", "))
      missing_regions <- c(missing_regions, r)
      next
    }
    
    v <- if(length(pred_cols) == 1) pr$data[[pred_cols]] else
      rowSums(pr$data[, pred_cols, drop = FALSE], na.rm = TRUE)
    
    if(is.null(total_vec)){
      years <- pr$data$year
      total_vec <- v
    } else {
      # align on year in case a region file has a different year span
      m <- match(years, pr$data$year)
      v_aligned <- ifelse(is.na(m), NA_real_, v[m])
      total_vec <- total_vec + v_aligned
    }
    used_regions <- c(used_regions, r)
  }
  
  list(years = years, vec = total_vec,
       used_regions = used_regions, missing_regions = missing_regions)
}


#' @title Like build_predicted_vector(), but resolves derived variables (e.g. F).
#' @description If `var`'s VAR_TYPE_MAP entry has a `derived_from` field (currently only
#'   "F", derived as Catch / Biomass), recursively builds the component predicted
#'   vectors and combines them; otherwise delegates straight to
#'   build_predicted_vector(). Adding a new derived variable = adding a `derived_from`
#'   entry to VAR_TYPE_MAP plus a case here for how to combine the components (division
#'   is the only combiner implemented so far, since F is the only derived variable in
#'   use; extend the `if` below if a future variable needs e.g. a sum or a difference).
#' @return Same shape as build_predicted_vector(): list(years, vec, used_regions,
#'   missing_regions). missing_regions is the union across all component variables.
build_predicted_vector_generic <- function(pool_codes, region_codes, var, pred_dir,
                                           cache, group_names_lookup){
  info <- VAR_TYPE_MAP[[var]]
  
  if(is.null(info$derived_from))
    return(build_predicted_vector(pool_codes, region_codes, var, pred_dir,
                                  cache, group_names_lookup))
  
  if(!identical(info$derived_from, c("Catch", "Biomass")))
    stop("build_predicted_vector_generic(): don't know how to combine derived_from = ",
         paste(info$derived_from, collapse = ", "), " for variable '", var, "'. ",
         "Only the Catch/Biomass ratio (F) is implemented -- extend this function for ",
         "any newly-added derived variable.")
  
  catch_res <- build_predicted_vector(pool_codes, region_codes, "Catch",   pred_dir, cache, group_names_lookup)
  bio_res   <- build_predicted_vector(pool_codes, region_codes, "Biomass", pred_dir, cache, group_names_lookup)
  missing_regions <- union(catch_res$missing_regions, bio_res$missing_regions)
  
  if(is.null(catch_res$vec) || is.null(bio_res$vec) ||
     all(is.na(catch_res$vec)) || all(is.na(bio_res$vec)))
    return(list(years = NULL, vec = NULL, used_regions = integer(0),
                missing_regions = missing_regions))
  
  years <- catch_res$years
  m <- match(years, bio_res$years)
  bio_aligned <- ifelse(is.na(m), NA_real_, bio_res$vec[m])
  # F undefined (NA, not 0) where biomass is zero/negative -- avoids a spurious
  # divide-by-zero spike rather than a genuine fishing-mortality signal
  vec <- ifelse(is.na(bio_aligned) | bio_aligned <= 0, NA_real_, catch_res$vec / bio_aligned)
  
  list(years = years, vec = vec,
       used_regions = union(catch_res$used_regions, bio_res$used_regions),
       missing_regions = missing_regions)
}


#' @keywords internal
#' @noRd
# Pull the group-id pool code(s) + a stable string key for one obs row, per group_via.
build_group_id <- function(th, row, group_via){
  if(group_via == "Poolcode2"){
    list(pool_codes = th$Poolcode2[row], key = as.character(th$Poolcode2[row]))
  } else {
    list(pool_codes = th$Poolcodes[[row]], key = th$Poolcode1[row])
  }
}


#' @title Plot observed vs. predicted (ggplot2), one PNG per (Variable, group, region).
#' @description For each variable in `vars` (see VAR_TYPE_MAP), selects the matching
#'   observed Type codes from `obs`, groups them by the exact (Pool code, Region)
#'   combination -- so every panel is compared against the correctly-matched predicted
#'   values -- and overlays, for EVERY run in `runs`, that run's predicted line (whole-
#'   domain file for Region 0, or the summed Ecospace_Annual_Average_Region_<n>_<Var>.csv
#'   file(s) otherwise) plus all observed series sharing that combination, as a ggplot2
#'   figure. Predicted lines are colored by run (see `run_colors`); observed points are
#'   black, distinguished by shape only (so the run colors stay the salient comparison).
#'   Type-0-equivalent (relative) series are rescaled to the REFERENCE run's units
#'   (the first run in `runs` that has data for that group) via
#'   q = mean(obs on overlapping years) / mean(reference pred on overlapping years), so
#'   every run's line sits on the same obs-anchored scale; Type-1 (absolute biomass)
#'   series are plotted as-is. Writes one PNG per panel to `plots_dir` via
#'   ggplot2::ggsave().
#' @param obs Output of read_ewe_timeseries().
#' @param runs A NAMED character vector/list of directories, one per Ecospace run to
#'   overlay, e.g. `c(baseline = "/path/to/sp03_5min_phase3_init/",
#'   best_fit = "/path/to/sp03_5min_phase3_init/run_g027_i0044_pid27288_.../")`. Each
#'   directory must contain "Ecospace_Annual_Average_<Var>.csv" and its per-region
#'   siblings "Ecospace_Annual_Average_Region_<n>_<Var>.csv", for every variable in
#'   `vars`. The first name is treated as the reference run for obs rescaling (see
#'   above) and, unless overridden in `run_colors`, is drawn in black.
#' @param run_colors Optional named character vector of colors, same names as `runs`
#'   (e.g. `c(baseline = "black", best_fit = "blue")`). Default: black for a run literally
#'   named "baseline" (or the first run if none is so named), then blue/red/darkgreen/...
#'   for the rest.
#' @param vars Character vector of variable names to plot; each must be a key in
#'   VAR_TYPE_MAP (default: c("Biomass", "Catch")).
#' @param plots_dir Output folder for PNGs (created if it doesn't exist). Default "plots".
#' @param width,height,dpi ggsave() figure dimensions (inches) and resolution (dpi).
#' @return Invisibly, a data.frame summarizing what was plotted: one row per (variable,
#'   group, region) panel with variable, pool_codes, region_codes, group_label,
#'   n_obs_series, runs_plotted, png_file, missing_by_run.
#' @export
plot_obs_vs_pred <- function(obs,
                             runs,
                             run_colors = NULL,
                             vars       = c("Biomass", "Catch"),
                             plots_dir  = "plots",
                             width = 7.5, height = 5, dpi = 150){
  
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required for plot_obs_vs_pred(). Install it with ",
         "install.packages('ggplot2').")
  
  run_names <- names(runs)
  if(is.null(run_names) || any(run_names == "") || length(runs) == 0)
    stop("`runs` must be a NAMED vector/list of directories, e.g. ",
         "c(baseline = '/path/to/run1', best_fit = '/path/to/run2').")
  
  default_palette <- c("blue", "red", "darkgreen", "purple", "orange", "brown", "magenta")
  if(is.null(run_colors)){
    run_colors <- setNames(rep(NA_character_, length(run_names)), run_names)
    baseline_nm <- if("baseline" %in% run_names) "baseline" else run_names[1]
    run_colors[baseline_nm] <- "black"
    other_nms <- setdiff(run_names, baseline_nm)
    if(length(other_nms) > 0)
      run_colors[other_nms] <- rep(default_palette, length.out = length(other_nms))
  } else {
    missing_c <- setdiff(run_names, names(run_colors))
    if(length(missing_c) > 0)
      stop("run_colors is missing an entry for run(s): ", paste(missing_c, collapse = ", "))
  }
  ref_run <- run_names[1]  # used to anchor obs rescaling when this group has data for it
  
  unknown_vars <- setdiff(vars, names(VAR_TYPE_MAP))
  if(length(unknown_vars) > 0)
    stop("Unrecognized variable(s) in `vars`: ", paste(unknown_vars, collapse = ", "),
         ". Known variables: ", paste(names(VAR_TYPE_MAP), collapse = ", "),
         " -- add an entry to VAR_TYPE_MAP to support more.")
  
  if(!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  
  th <- obs$ts.head
  safe_name <- function(s) gsub("[^A-Za-z0-9._-]+", "_", s)
  
  # one cache per run, so files with the same var/region name in different run folders
  # never collide
  caches <- setNames(lapply(run_names, function(x) new.env(parent = emptyenv())), run_names)
  summary_rows <- list()
  
  for(var in vars){
    info <- VAR_TYPE_MAP[[var]]
    var_rows <- which(th$Type %in% info$types)
    if(length(var_rows) == 0){
      message("[", var, "] no observed series with Type in {", paste(info$types, collapse = ","),
              "}; skipping this variable.")
      next
    }
    message("[", var, "] ", length(var_rows), " observed series found.")
    
    grp_id_all <- lapply(var_rows, build_group_id, th = th, group_via = info$group_via)
    group_key  <- paste0(vapply(grp_id_all, `[[`, "", "key"), "__R", th$Region[var_rows])
    groups     <- split(var_rows, group_key)
    
    group_names_lookup <- function(pr, pool_codes) pr$group_names[pool_codes + 1L]
    
    # for group_label text: any run's whole-domain file for this var supplies the
    # pool-code -> group-name lookup (Ecopath group order is identical across runs,
    # domain, and every region file for a given var). Derived variables (e.g. F) have
    # no file of their own -- fall back to one of their component variables' files.
    label_var <- if(!is.null(info$derived_from)) info$derived_from[1] else var
    label_ref <- NULL
    for(rn in run_names){
      label_ref <- get_region_output(0L, label_var, runs[[rn]], caches[[rn]])
      if(!is.null(label_ref)) break
    }
    
    for(gi in seq_along(groups)){
      
      rows <- groups[[gi]]
      gid <- build_group_id(th, rows[1], info$group_via)
      pool_codes   <- gid$pool_codes
      region_codes <- th$Regions[[rows[1]]]
      
      pred_by_run <- list()
      missing_by_run <- list()
      for(rn in run_names){
        pr <- build_predicted_vector_generic(pool_codes, region_codes, var, runs[[rn]],
                                             caches[[rn]], group_names_lookup)
        if(is.null(pr$vec) || all(is.na(pr$vec))){
          missing_by_run[[rn]] <- pr$missing_regions
          next
        }
        pred_by_run[[rn]] <- pr
      }
      
      if(length(pred_by_run) == 0){
        message("  [skip] ", var, " group '", names(groups)[gi], "': no predicted data ",
                "available for ANY run.")
        next
      }
      
      pred_cols_lbl <- if(!is.null(label_ref)) group_names_lookup(label_ref, pool_codes) else
        paste0("pool_", pool_codes)
      region_lbl <- if(identical(region_codes, 0L))
        "domain avg" else paste0("Region ", paste(region_codes, collapse = "+"))
      group_label <- paste0(var, ": ", paste(pred_cols_lbl, collapse = " + "),
                            "  (", region_lbl, ")")
      
      pred_df <- do.call(rbind, lapply(names(pred_by_run), function(rn)
        data.frame(year = pred_by_run[[rn]]$years, value = pred_by_run[[rn]]$vec,
                   run = rn, stringsAsFactors = FALSE)))
      pred_df$run <- factor(pred_df$run, levels = run_names[run_names %in% names(pred_by_run)])
      
      # anchor obs rescaling to the reference run if it has data for this group,
      # otherwise fall back to whichever run did resolve (keeps every panel usable
      # even when the reference run's region file happens to be missing)
      anchor_rn <- if(ref_run %in% names(pred_by_run)) ref_run else names(pred_by_run)[1]
      anchor_years <- pred_by_run[[anchor_rn]]$years
      anchor_vec   <- pred_by_run[[anchor_rn]]$vec
      
      obs_df_list <- list()
      for(j in seq_along(rows)){
        r <- rows[j]
        ov <- obs$ts[[th$Title[r]]]
        oy <- obs$ts$year
        keep <- !is.na(ov)
        if(!any(keep)) next
        
        common <- intersect(oy[keep], anchor_years)
        if(length(common) == 0){
          message("  [note] series '", th$Title[r], "': no overlapping years with prediction; skipping.")
          next
        }
        
        q <- 1
        if(!th$Absolute[r]){
          mo <- mean(ov[keep][oy[keep] %in% common], na.rm = TRUE)
          mp <- mean(anchor_vec[anchor_years %in% common], na.rm = TRUE)
          if(is.finite(mo) && is.finite(mp) && mp > 0) q <- mo / mp
        }
        obs_scaled <- ov / q
        
        obs_df_list[[length(obs_df_list) + 1]] <- data.frame(
          year   = oy[keep],
          value  = obs_scaled[keep],
          series = th$Title[r],
          stringsAsFactors = FALSE
        )
      }
      
      obs_df <- if(length(obs_df_list) > 0) do.call(rbind, obs_df_list) else
        data.frame(year = numeric(0), value = numeric(0), series = character(0))
      if(nrow(obs_df) > 0)
        obs_df$series <- factor(obs_df$series, levels = unique(obs_df$series))
      
      p <- ggplot2::ggplot() +
        ggplot2::geom_line(data = pred_df, ggplot2::aes(x = year, y = value, color = run),
                           linewidth = 1) +
        ggplot2::scale_color_manual(values = run_colors[levels(pred_df$run)], name = "Run") +
        { if(nrow(obs_df) > 0)
          ggplot2::geom_point(data = obs_df,
                              ggplot2::aes(x = year, y = value, shape = series),
                              color = "black", size = 2.2)
        } +
        ggplot2::labs(title = group_label, x = "Year", y = info$y_lab,
                      shape = "Observed series") +
        ggplot2::theme_bw(base_size = 11) +
        ggplot2::theme(legend.position = "bottom",
                       legend.text = ggplot2::element_text(size = 7),
                       plot.title = ggplot2::element_text(size = 11, face = "bold"),
                       aspect.ratio = 1) +
        ggplot2::guides(color = ggplot2::guide_legend(nrow = 1), shape = ggplot2::guide_legend(nrow = 2)) +
        { if(nrow(obs_df) > 0)
          # ggplot2's default shape palette only has 6 values and errors past that
          ggplot2::scale_shape_manual(values = rep(0:25, length.out = nlevels(obs_df$series)))
        }
      
      png_file <- file.path(plots_dir, paste0(var, "_", safe_name(names(groups)[gi]), ".png"))
      ggplot2::ggsave(png_file, plot = p, width = width, height = height, dpi = dpi, units = "in")
      
      missing_str <- paste(vapply(names(missing_by_run), function(rn)
        sprintf("%s:%s", rn, paste(missing_by_run[[rn]], collapse = ",")),
        character(1)), collapse = "; ")
      
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        variable        = var,
        pool_codes      = paste(pool_codes, collapse = ","),
        region_codes    = paste(region_codes, collapse = ","),
        group_label     = group_label,
        n_obs_series    = length(unique(obs_df$series)),
        runs_plotted    = paste(names(pred_by_run), collapse = ","),
        png_file        = png_file,
        missing_by_run  = missing_str,
        stringsAsFactors = FALSE
      )
    }
  }
  
  message("Wrote ", length(summary_rows), " PNG(s) to ", normalizePath(plots_dir, mustWork = FALSE))
  invisible(do.call(rbind, summary_rows))
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PART 2: GA-ensemble helpers + plot_ga_ensemble()
#
# Visualize how predicted Biomass / Catch / F time series vary across an entire GA
# calibration generation, colored by each candidate's fitness (likelihood) -- the
# suggested alternative to replaying per-generation history (which wasn't saved): use the
# gen-30 population, color by NLL.
#
# INPUTS:
#   1) final_ga_runs_<timestamp>.csv -- columns gapop_row, role, cmd_file, fitness. The
#      candidate's OUTPUT FOLDER NAME is parsed out of cmd_file (the path segment right
#      before "cmd.txt"), e.g. cmd_file = ".../gen30/run_g027_i0044_..._fD8kg1XW/cmd.txt"
#      -> run_folder = "run_g027_i0044_..._fD8kg1XW". This is what has to match a
#      subfolder on disk -- gapop_row is just a table row index, not a folder name.
#   2) gen_dir -- a directory with one subfolder per candidate (matching run_folder),
#      each containing that candidate's own Ecospace_Annual_Average_<Var>.csv (domain-
#      average only -- GA candidates generally aren't run per-region).
#
# KNOWN MISMATCH: your fitness table may have fewer valid rows than output folders on
# disk (e.g. 906 vs 910). resolve_run_dirs() only keeps candidates present in BOTH (a
# fitness value AND an existing folder) and reports counts on both sides of the mismatch
# so you can see what got dropped and why.
#
# OUTPUT: one PNG per (Variable, matched group) panel, all candidates' lines colored on a
# continuous log10 scale by fitness (viridis palette), the single best (min-fitness)
# candidate redrawn on top as a thick black line, written to <plots_dir>/.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @title Parse the GA run/fitness table and extract each candidate's output-folder name.
#' @description Reads final_ga_runs_<timestamp>.csv (columns: gapop_row, role, cmd_file,
#'   fitness) and extracts the run's output-folder name from `cmd_file` -- the path
#'   segment immediately before "cmd.txt" (Windows-style backslashes are normalized to
#'   "/" first). This folder name is what matches a subfolder under `gen_dir` on disk
#'   (e.g. output/gen30_small/<folder>/) -- NOT gapop_row, which is only a table row
#'   index and doesn't by itself identify which run folder produced a given fitness.
#' @param path Path to final_ga_runs_<timestamp>.csv.
#' @return data.frame(gapop_row, role, cmd_file, fitness, run_folder).
read_ga_runs <- function(path){
  df <- utils::read.csv(path, stringsAsFactors = FALSE)
  req <- c("gapop_row", "cmd_file", "fitness")
  miss <- setdiff(req, names(df))
  if(length(miss) > 0)
    stop("read_ga_runs(): missing expected column(s) in ", path, ": ", paste(miss, collapse = ", "))
  
  extract_folder <- function(p){
    p <- gsub("\\\\", "/", p)
    parts <- strsplit(p, "/")[[1]]
    parts <- parts[nzchar(parts)]
    if(length(parts) < 2) return(NA_character_)
    parts[length(parts) - 1]
  }
  df$run_folder <- vapply(df$cmd_file, extract_folder, character(1))
  
  # generation number, parsed from the leading "run_g<NN>_" in the folder name (NA if
  # the folder name doesn't follow that convention)
  gen_str <- sub("^run_g(\\d+)_.*$", "\\1", df$run_folder)
  df$generation <- suppressWarnings(as.integer(ifelse(gen_str == df$run_folder, NA, gen_str)))
  
  n_bad <- sum(is.na(df$run_folder))
  if(n_bad > 0)
    message("read_ga_runs(): could not parse a run folder out of cmd_file for ", n_bad, " row(s).")
  n_no_gen <- sum(is.na(df$generation))
  if(n_no_gen > 0)
    message("read_ga_runs(): could not parse a generation number (expected 'run_g<NN>_...') for ",
            n_no_gen, " row(s).")
  
  message("read_ga_runs(): ", nrow(df), " candidate(s) with a fitness value, spanning ",
          length(unique(stats::na.omit(df$generation))), " generation(s).")
  df
}


#' @title Attach each candidate's on-disk output directory and flag missing folders.
#' @description Some candidates in the fitness table may not have a corresponding output
#'   folder on disk, and some output folders may have no matching fitness row (e.g. a
#'   failed run that never got scored). This function reports both directions but only
#'   KEEPS candidates present in the fitness table; use the message output (or diff
#'   `list.files(gen_dir)` against `ga_runs$run_folder` yourself) to see the extra
#'   on-disk folders it excluded.
#' @param ga_runs Output of read_ga_runs().
#' @param gen_dir Directory containing one subfolder per candidate (e.g. "output/gen30_small").
#' @return `ga_runs` with two extra columns: run_dir (full path), exists (logical).
resolve_run_dirs <- function(ga_runs, gen_dir){
  if(!dir.exists(gen_dir))
    message("resolve_run_dirs(): WARNING -- gen_dir itself does not exist: ", gen_dir,
            ". Every candidate will show as missing below, but that's because this ",
            "directory is wrong/unreachable (moved, typo, unmounted drive, etc.), not ",
            "because 906 individual candidate folders are absent. Check the path (e.g. ",
            "list.files(dirname(gen_dir)) to see what's actually there) before re-running.")
  
  ga_runs$run_dir <- file.path(gen_dir, ga_runs$run_folder)
  ga_runs$exists  <- !is.na(ga_runs$run_folder) & dir.exists(ga_runs$run_dir)
  
  n_missing <- sum(!ga_runs$exists)
  if(n_missing > 0)
    message("resolve_run_dirs(): ", n_missing, " of ", nrow(ga_runs),
            " candidate folder(s) listed in the fitness table were NOT found under ",
            gen_dir, " (excluded from the ensemble).")
  
  on_disk <- tryCatch(list.files(gen_dir), error = function(e) character(0))
  extra <- setdiff(on_disk, ga_runs$run_folder)
  if(length(extra) > 0)
    message("resolve_run_dirs(): ", length(extra), " folder(s) under ", gen_dir,
            " have NO matching row in the fitness table (e.g. failed/unscored runs) -- ",
            "excluded from the ensemble.")
  
  message("resolve_run_dirs(): ", sum(ga_runs$exists), " of ", nrow(ga_runs),
          " candidates have both a fitness value AND an on-disk folder -- these are ",
          "what plot_ga_ensemble() will use.")
  
  ga_runs
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Ensemble candidate selection: three modes, shared everywhere an "ensemble" is plotted
#
# Matches the workflow in your own get_red_tide_mortality_ga_ensemble.R: (1) "quantile"
# -- keep the best X% by fitness (the convention already used throughout this script);
# (2) "topN" -- keep exactly the N best candidates by rank; (3) "aic" -- Akaike-style
# weights over the FULL population, with the weight-decay scale tuned so the effective
# sample size (ESS = 1/sum(w^2)) hits a target (your code targets ESS=100 to match the
# topN=100 comparison). "aic" mode returns WEIGHTS alongside the (full) candidate set,
# rather than a hard include/exclude subset -- weighted means/CIs use every candidate,
# just down-weighted by fit quality, rather than a sharp cutoff.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @title Akaike-style weights from a fitness (NLL) vector.
#' @description `w_i = exp(-(fitness_i - min(fitness)) / scale)`, normalized to sum to
#'   1. Standard Akaike-weight form generalized with a tunable `scale` (classic AIC
#'   weights correspond to `scale = 2` when `fitness` is itself an AIC-like quantity);
#'   larger `scale` spreads weight across more candidates, smaller `scale` concentrates
#'   it on the best few. Matches `fn.akaike_weights()` in your own R4EwE codebase.
#' @param fitness Numeric vector of fitness (NLL) values, lower = better.
#' @param scale Positive scalar controlling weight decay. Default 2.
#' @return Numeric vector of weights, same length as `fitness`, summing to 1.
compute_akaike_weights <- function(fitness, scale = 2){
  d <- fitness - min(fitness, na.rm = TRUE)
  w <- exp(-d / scale)
  w[is.na(fitness)] <- 0
  w / sum(w)
}


#' @title Solve for the Akaike weight scale that hits a target effective sample size.
#' @description Effective sample size (ESS) = `1 / sum(weights^2)` -- this finds the
#'   `scale` (via `uniroot()`) such that `compute_akaike_weights(fitness, scale)` has
#'   that ESS. Matches the `uniroot(...)` step in your
#'   get_red_tide_mortality_ga_ensemble.R (there targeting ESS = 100 to make the
#'   AIC-weighted ensemble roughly comparable in "effective" size to a top-100 ensemble).
#' @param fitness Numeric vector of fitness (NLL) values.
#' @param target_ess Desired effective sample size. Default 100.
#' @param interval Search interval passed to `uniroot()`. Default c(1, 1e6).
#' @return The solved scale value (numeric scalar).
solve_akaike_scale <- function(fitness, target_ess = 100, interval = c(1, 1e6)){
  fitness <- fitness[!is.na(fitness)]
  n <- length(fitness)
  if(target_ess >= n){
    warning("solve_akaike_scale(): target_ess (", target_ess, ") >= number of candidates (",
            n, ") -- returning a very large scale (near-uniform weights).")
    return(interval[2])
  }
  f <- function(s) 1 / sum(compute_akaike_weights(fitness, scale = s)^2) - target_ess
  res <- tryCatch(stats::uniroot(f, interval = interval), error = function(e){
    stop("solve_akaike_scale(): uniroot() failed to find a scale for target_ess = ", target_ess,
         " in interval [", interval[1], ", ", interval[2], "]: ", conditionMessage(e))
  })
  message("solve_akaike_scale(): scale = ", round(res$root, 2), " gives ESS ~= ",
          round(1 / sum(compute_akaike_weights(fitness, scale = res$root)^2), 1))
  res$root
}


#' @title Select ensemble candidates by one of three modes -- the shared selection logic
#'   used across every ensemble plot in this script.
#' @description
#' \itemize{
#'   \item `"quantile"` (default): keep candidates with fitness at or below the
#'     `keep_fitness_quantile` quantile (e.g. 0.9 = best 90%). Hard subset, no weights.
#'   \item `"topN"`: keep exactly the `top_n` best candidates by fitness rank. Hard
#'     subset, no weights. NOTE: not actually used as a standalone ensemble mode in this
#'     project's own run script anymore -- with no weighting, an unweighted top-N subset
#'     doesn't add much over "quantile", so "topN" was dropped from run.R in favor of
#'     "aic" everywhere. `top_n` itself is still meaningful, though: it's also what
#'     `restrict_to_top_n` defaults to for `mode = "aic"`'s pool-restriction step below.
#'   \item `"aic"`: Akaike-style weights -- BY DEFAULT restricted to the same `top_n`
#'     (100) best candidates first (see `restrict_to_top_n`), then weighted within that
#'     pool. Pass `restrict_to_top_n = NULL` for the original behavior (weights computed
#'     over the FULL population, effectively down-weighting rather than excluding poor
#'     fits). `ens_weight` column added either way; scale tuned (via
#'     solve_akaike_scale()) so the weights' effective sample size hits `target_ess`
#'     WITHIN whichever pool is used.
#' }
#' @param ga_runs Output of resolve_run_dirs() (or any data.frame with a `fitness` column
#'   and one row per candidate).
#' @param mode One of "quantile", "topN", "aic". Default "quantile".
#' @param keep_fitness_quantile Used when `mode = "quantile"`. Default 0.9.
#' @param top_n Used when `mode = "topN"`, and as the default `restrict_to_top_n` for
#'   `mode = "aic"`. Default 100. Ignored if `top_n_prop` is supplied (see below).
#' @param top_n_prop Optional proportion in (0, 1]; if supplied, OVERRIDES `top_n` by
#'   computing `round(top_n_prop * nrow(ga_runs))` at call time, so the ensemble size
#'   scales automatically with however many total candidates are actually in `ga_runs`
#'   (e.g. if a future GA run archives a different total population size, the kept
#'   count adjusts proportionally instead of staying a fixed number that may no longer
#'   make sense as a fraction of the pool). NULL (default) = use `top_n` as a fixed
#'   count, unchanged from previous behavior. For THIS project's current pool of 910
#'   candidates, `top_n_prop = 100/910` (~0.11, i.e. ~11%) reproduces exactly the
#'   fixed `top_n = 100` this script has always used.
#' @param restrict_to_top_n Used when `mode = "aic"`: restricts to this many best
#'   candidates by fitness rank BEFORE computing weights. Defaults to `top_n` (100, or
#'   the `top_n_prop`-derived count if that was supplied). Set to NULL to weight over
#'   the full population instead.
#' @param target_ess Used when `mode = "aic"`. Default 100.
#' @param aic_scale Used when `mode = "aic"`; if supplied, skips solve_akaike_scale() and
#'   uses this scale directly. NULL (default) = solve for `target_ess`.
#' @return The input data.frame (subset for "quantile"/"topN"; the (possibly
#'   top_n-restricted) subset with an added `ens_weight` column, for "aic"), plus an
#'   attribute `"ensemble_mode"` recording which mode was used (retrievable via
#'   `attr(x, "ensemble_mode")`) so downstream plotting functions can adapt their
#'   aggregation and labeling automatically.
#' @export
select_ensemble_candidates <- function(ga_runs,
                                       mode = c("quantile", "topN", "aic"),
                                       keep_fitness_quantile = 0.9,
                                       top_n = 100,
                                       top_n_prop = NULL,
                                       restrict_to_top_n = top_n,
                                       target_ess = 100,
                                       aic_scale = NULL){
  mode <- match.arg(mode)
  if(!"fitness" %in% names(ga_runs)) stop("ga_runs must have a 'fitness' column.")
  
  if(!is.null(top_n_prop)){
    if(top_n_prop <= 0 || top_n_prop > 1)
      stop("select_ensemble_candidates(): top_n_prop must be in (0, 1], got ", top_n_prop, ".")
    top_n_new <- round(top_n_prop * nrow(ga_runs))
    # restrict_to_top_n's default (top_n) was captured BEFORE top_n was possibly
    # overridden here -- if the caller left restrict_to_top_n at its default (i.e. it
    # still equals the OLD top_n), re-derive it from the new, proportion-based top_n
    # too, so top_n_prop actually affects the aic pool-restriction step as well, not
    # just the (rarely-used standalone) "topN" mode.
    if(identical(restrict_to_top_n, top_n)) restrict_to_top_n <- top_n_new
    message("select_ensemble_candidates(): top_n_prop = ", top_n_prop, " -> top_n = ", top_n_new,
            " (", round(top_n_prop * 100, 1), "% of ", nrow(ga_runs), " candidate(s)).")
    top_n <- top_n_new
  }
  
  if(mode == "quantile"){
    cutoff <- stats::quantile(ga_runs$fitness, probs = keep_fitness_quantile, na.rm = TRUE)
    out <- ga_runs[ga_runs$fitness <= cutoff, , drop = FALSE]
    message("select_ensemble_candidates(): mode='quantile' (", round(keep_fitness_quantile * 100),
            "%) -> ", nrow(out), " of ", nrow(ga_runs), " candidate(s).")
  } else if(mode == "topN"){
    ord <- rank(ga_runs$fitness, ties.method = "first")
    out <- ga_runs[ord <= top_n, , drop = FALSE]
    message("select_ensemble_candidates(): mode='topN' (N=", top_n, ") -> ",
            nrow(out), " of ", nrow(ga_runs), " candidate(s).")
  } else {
    pool <- ga_runs
    if(!is.null(restrict_to_top_n)){
      ord <- rank(pool$fitness, ties.method = "first")
      pool <- pool[ord <= restrict_to_top_n, , drop = FALSE]
    }
    scale <- if(is.null(aic_scale)) solve_akaike_scale(pool$fitness, target_ess) else aic_scale
    out <- pool
    out$ens_weight <- compute_akaike_weights(out$fitness, scale = scale)
    message("select_ensemble_candidates(): mode='aic' (",
            if(!is.null(restrict_to_top_n)) paste0("restricted to top ", restrict_to_top_n, " first, ") else "full population, ",
            "target ESS=", target_ess, ", scale=", round(scale, 2), ") -> ",
            nrow(out), " of ", nrow(ga_runs), " candidate(s), weighted.")
  }
  attr(out, "ensemble_mode") <- mode
  out
}


#' @keywords internal
#' @noRd
# Pool codes (and their group names) whose Ecopath group name matches any of `patterns`
# (case-insensitive substring), read from one reference group_names vector (from any
# candidate's domain Biomass file -- Ecopath group order/names are the same across all).
find_pool_codes_by_name <- function(group_names, patterns){
  hit <- vapply(group_names, function(nm)
    nzchar(nm) && any(vapply(patterns, function(p) grepl(p, nm, ignore.case = TRUE), logical(1))),
    logical(1))
  idx <- which(hit)  # 1-indexed positions in group_names
  data.frame(pool_code = idx - 1L, group_name = unname(group_names[idx]), stringsAsFactors = FALSE)
}


#' @title Diagnose how many GA candidate folders actually contain predicted output files.
#' @description For every candidate with `exists == TRUE` in `ga_runs`, checks whether
#'   each variable in `vars` has an "Ecospace_Annual_Average_<Var>.csv" file directly
#'   inside its run_dir (does NOT check per-region siblings -- this is about whether the
#'   candidate was archived with output at all, not region coverage). Useful for
#'   explaining a plot_ga_ensemble() run that plotted far fewer lines than the number of
#'   candidates that passed filtering -- if only a fraction of candidates were fully
#'   archived (e.g. a disk-space-limited GA run that only saved full Ecospace output for
#'   some candidates), that's an upstream data-availability fact, not a bug in the
#'   plotting code, and this tells you the actual number to expect. Also prints a
#'   representativeness check comparing the fully-archived (plottable) subset's role
#'   breakdown and fitness quantiles against the full candidate pool, flagging (and
#'   listing) any outlier(s) inside the plottable set specifically.
#' @param ga_runs Output of resolve_run_dirs() (uses the rows where exists == TRUE).
#' @param vars Character vector of variable names to check for, e.g. c("Biomass", "Catch").
#' @return Invisibly, data.frame(run_folder, fitness, <one logical column per var>,
#'   n_vars_present), after printing a per-variable and combined summary.
#' @export
diagnose_candidate_outputs <- function(ga_runs, vars = c("Biomass", "Catch")){
  valid <- ga_runs[ga_runs$exists, , drop = FALSE]
  if(nrow(valid) == 0) stop("No candidate folders found on disk -- check ga_runs$exists.")
  
  has_var <- function(run_dir, var)
    file.exists(file.path(run_dir, sprintf("Ecospace_Annual_Average_%s.csv", var)))
  
  flags <- as.data.frame(lapply(vars, function(v)
    vapply(valid$run_dir, has_var, logical(1), var = v)))
  names(flags) <- vars
  out <- cbind(run_folder = valid$run_folder, fitness = valid$fitness, flags,
               n_vars_present = rowSums(flags))
  
  message("diagnose_candidate_outputs(): checked ", nrow(valid), " candidate folder(s) ",
          "(those with exists == TRUE).")
  for(v in vars){
    n_present <- sum(flags[[v]])
    message("  ", v, ": ", n_present, " of ", nrow(valid), " candidate(s) have ",
            sprintf("Ecospace_Annual_Average_%s.csv", v), " (",
            round(100 * n_present / nrow(valid), 1), "%).")
  }
  n_all <- sum(out$n_vars_present == length(vars))
  message("  Candidates with ALL of {", paste(vars, collapse = ", "), "} present: ",
          n_all, " of ", nrow(valid), ".")
  message("  (plot_ga_ensemble() can only draw a line for a candidate if its predicted ",
          "file for that variable AND that specific pool code/region combination reads ",
          "successfully -- this count is the ceiling on how many lines any panel can show.)")
  
  # representativeness check: is the fully-archived (plottable) subset a fair sample of
  # the full candidate pool, or skewed (e.g. only elites, or biased toward worse fits)?
  archived  <- out$run_folder[out$n_vars_present == length(vars)]
  arch_fit  <- valid$fitness[valid$run_folder %in% archived]
  role_full <- if("role" %in% names(valid)) table(valid$role) else NULL
  role_arch <- if("role" %in% names(valid)) table(valid$role[valid$run_folder %in% archived]) else NULL
  
  if(length(arch_fit) > 0){
    q <- function(x) round(stats::quantile(x, probs = c(0, .25, .5, .75, 1), na.rm = TRUE), 1)
    full_q <- q(valid$fitness)
    arch_q <- q(arch_fit)
    
    message("")
    message("  --- representativeness: archived (plottable) subset vs. full candidate pool ---")
    if(!is.null(role_full))
      message("  role breakdown -- full: ",
              paste(names(role_full), role_full, sep = "=", collapse = ", "),
              " | archived: ", paste(names(role_arch), role_arch, sep = "=", collapse = ", "))
    message("  fitness quantiles (0/25/50/75/100%)")
    message("    full:     ", paste(full_q, collapse = " / "))
    message("    archived: ", paste(arch_q, collapse = " / "))
    
    arch_mean <- mean(arch_fit, na.rm = TRUE)
    arch_median <- arch_q[3]
    if(is.finite(arch_mean) && is.finite(arch_median) && arch_median > 0 &&
       arch_mean > 1.3 * arch_median){
      worst <- valid[valid$run_folder %in% archived, c("run_folder", "fitness")]
      worst <- worst[order(-worst$fitness), , drop = FALSE]
      top_n <- min(5, nrow(worst))
      message("  NOTE: archived-subset mean (", round(arch_mean, 1),
              ") is notably higher than its median (", round(arch_median, 1),
              ") -- pulled up by outlier(s) INSIDE the plottable set. Worst ", top_n,
              " archived candidate(s) by fitness:")
      for(i in seq_len(top_n))
        message("    ", worst$run_folder[i], "  fitness = ", round(worst$fitness[i], 1))
      message("  Pass these to plot_ga_ensemble(exclude_run_folders = ...) to drop them ",
              "from the visualization, or use max_fitness / keep_fitness_quantile.")
    } else {
      message("  Archived-subset mean and median are close -- no single outlier obviously ",
              "dominating the plottable set.")
    }
  }
  
  invisible(out)
}


#' @title Plot predicted time series across a whole GA generation, colored by fitness.
#' @description For every group whose Ecopath name matches one of `species_patterns`
#'   (case-insensitive substring -- e.g. "gag" matches every "gag 0" .. "gag 5+" stanza),
#'   and for every variable in `vars`, draws one predicted line per candidate (domain-
#'   average only, Region 0 -- GA candidates generally aren't run per-region) colored on a
#'   continuous log10 scale by that candidate's fitness (LOWER fitness = better, per your
#'   GA setup). The single best (minimum-fitness) candidate is redrawn on top as a thick
#'   black line so it's identifiable regardless of how the color scale reads. Observed
#'   series matching each plotted pool code at Region 0 are optionally overlaid (single-
#'   pool-code obs only -- an aggregated multi-stanza obs series like "5,6,7,8,9" is NOT
#'   split out here the way plot_obs_vs_pred() does it), rescaled against the best-fit
#'   candidate's line.
#' @param obs Output of read_ewe_timeseries() (only used if overlay_obs = TRUE).
#' @param ga_runs Output of resolve_run_dirs().
#' @param init_run_dir Optional baseline (pre-calibration) run folder -- if supplied,
#'   its predicted line is overlaid in black dashed, alongside the ensemble (colored by
#'   NLL, plasma scale) and the best-fit line (turquoise solid).
#' @param vars Character vector of variable names (keys of VAR_TYPE_MAP), e.g.
#'   c("Biomass", "Catch", "F").
#' @param species_patterns Character vector of case-insensitive substrings to match
#'   against Ecopath group names, e.g. c("gag", "red grouper").
#' @param overlay_obs Logical; overlay observed series matching each plotted pool code at
#'   Region 0. Default TRUE.
#' @param max_fitness Optional numeric cutoff; candidates with fitness ABOVE this are
#'   excluded before plotting (fitness = NLL here, lower = better, so this drops the
#'   worst-fitting outliers). NULL (default) = no cutoff. Run once with NULL first and
#'   check the printed fitness range / worst-5 list to pick a sensible value.
#' @param keep_fitness_quantile Optional value in (0, 1]; keeps only the best-fitting
#'   fraction of candidates by fitness, e.g. 0.9 keeps the best 90% (drops the worst 10%
#'   by NLL). Computed AFTER exclude_run_folders/max_fitness have already been applied, so
#'   it's relative to whatever set remains at that point. NULL (default) = no quantile
#'   filter. If both this and max_fitness are given, both are applied (whichever is more
#'   restrictive for a given candidate wins).
#' @param exclude_run_folders Optional character vector of specific `run_folder` names
#'   (as they appear in `ga_runs$run_folder` / plot legends) to exclude regardless of
#'   fitness -- use this once you've identified a specific outlier candidate by name.
#' @param max_candidates Safety cap on candidate lines drawn per panel; if more than this
#'   many valid candidates exist, a random subsample is drawn (the best-fit candidate is
#'   always kept). Default 1000 (effectively "no cap" for a 906-candidate generation).
#' @param plots_dir Output folder for PNGs (created if it doesn't exist). Default "plots".
#' @param width,height,dpi ggsave() figure dimensions (inches) and resolution (dpi).
#' @return Invisibly, a data.frame summarizing what was plotted: one row per (variable,
#'   pool_code) panel with variable, pool_code, group_name, n_candidates_plotted,
#'   best_fitness, best_run_folder, png_file.
#' @export
plot_ga_ensemble <- function(obs = NULL,
                             ga_runs,
                             init_run_dir          = NULL,
                             vars                  = c("Biomass", "Catch", "F"),
                             species_patterns      = c("gag", "red grouper"),
                             overlay_obs           = TRUE,
                             max_fitness           = NULL,
                             keep_fitness_quantile = NULL,
                             exclude_run_folders   = NULL,
                             max_candidates        = 1000,
                             plots_dir             = "plots",
                             width = 7.5, height = 5, dpi = 150){
  
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
  if(overlay_obs && is.null(obs))
    stop("overlay_obs = TRUE requires `obs` (output of read_ewe_timeseries()).")
  if(!is.null(keep_fitness_quantile) &&
     (keep_fitness_quantile <= 0 || keep_fitness_quantile > 1))
    stop("keep_fitness_quantile must be in (0, 1], e.g. 0.9 for the best 90%.")
  
  unknown_vars <- setdiff(vars, names(VAR_TYPE_MAP))
  if(length(unknown_vars) > 0)
    stop("Unrecognized variable(s) in `vars`: ", paste(unknown_vars, collapse = ", "),
         ". Known variables: ", paste(names(VAR_TYPE_MAP), collapse = ", "))
  
  if(!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  
  valid <- ga_runs[ga_runs$exists, , drop = FALSE]
  if(nrow(valid) == 0)
    stop("No candidate folders found on disk -- check gen_dir / resolve_run_dirs() output.")
  
  if(!is.null(exclude_run_folders)){
    n_before <- nrow(valid)
    dropped  <- intersect(valid$run_folder, exclude_run_folders)
    valid <- valid[!(valid$run_folder %in% exclude_run_folders), , drop = FALSE]
    message("plot_ga_ensemble(): exclude_run_folders removed ", length(dropped),
            " of ", n_before, " candidate(s)",
            if(length(setdiff(exclude_run_folders, dropped)) > 0)
              paste0(" (", length(setdiff(exclude_run_folders, dropped)),
                     " name(s) in exclude_run_folders didn't match any candidate)") else "",
            ".")
  }
  
  if(!is.null(max_fitness)){
    n_before <- nrow(valid)
    valid <- valid[valid$fitness <= max_fitness, , drop = FALSE]
    message("plot_ga_ensemble(): max_fitness = ", max_fitness, " excluded ",
            n_before - nrow(valid), " of ", n_before, " candidate(s); ",
            nrow(valid), " remaining.")
  }
  
  if(!is.null(keep_fitness_quantile)){
    n_before <- nrow(valid)
    cutoff <- stats::quantile(valid$fitness, probs = keep_fitness_quantile, na.rm = TRUE)
    valid <- valid[valid$fitness <= cutoff, , drop = FALSE]
    message("plot_ga_ensemble(): keep_fitness_quantile = ", keep_fitness_quantile,
            " -> cutoff fitness = ", signif(cutoff, 6), "; excluded ",
            n_before - nrow(valid), " of ", n_before, " candidate(s) (worst ",
            round((1 - keep_fitness_quantile) * 100, 1), "% by NLL); ",
            nrow(valid), " remaining.")
  }
  
  if(nrow(valid) == 0)
    stop("No candidates left after filtering -- loosen max_fitness / keep_fitness_quantile / exclude_run_folders.")
  
  worst5 <- paste(round(sort(valid$fitness, decreasing = TRUE)[seq_len(min(5, nrow(valid)))], 1),
                  collapse = ", ")
  message("plot_ga_ensemble(): fitness range among included candidates: ",
          signif(min(valid$fitness), 6), " - ", signif(max(valid$fitness), 6),
          " (worst 5: ", worst5, "). If a plot still looks skewed by one line, note its ",
          "run_folder from the legend/summary and pass it via exclude_run_folders next time.")
  
  if(nrow(valid) > max_candidates){
    best_idx_pre <- which.min(valid$fitness)
    keep <- unique(c(best_idx_pre, sample(seq_len(nrow(valid)), max_candidates - 1)))
    message("plot_ga_ensemble(): subsampling ", max_candidates, " of ", nrow(valid),
            " candidates (best-fit candidate always kept).")
    valid <- valid[keep, , drop = FALSE]
  }
  
  best_row <- valid[which.min(valid$fitness), , drop = FALSE]
  message("plot_ga_ensemble(): ", nrow(valid), " candidates plotted; best fitness = ",
          signif(best_row$fitness, 6), " (", best_row$run_folder, ").")
  
  # one cache PER CANDIDATE, so a given candidate's Biomass/Catch CSV is read once and
  # reused across every species/variable panel (not once per panel)
  caches <- setNames(lapply(seq_len(nrow(valid)), function(i) new.env(parent = emptyenv())),
                     valid$run_folder)
  init_cache <- if(!is.null(init_run_dir)) new.env(parent = emptyenv()) else NULL
  
  group_names_lookup <- function(pr, pool_codes) pr$group_names[pool_codes + 1L]
  
  # reference group-name list, from whichever candidate's domain Biomass file reads first
  ref <- NULL
  for(i in seq_len(nrow(valid))){
    ref <- get_region_output(0L, "Biomass", valid$run_dir[i], caches[[valid$run_folder[i]]])
    if(!is.null(ref)) break
  }
  if(is.null(ref))
    stop("Could not read a domain-average Biomass file from ANY candidate folder -- ",
         "check that gen_dir/<run_folder>/Ecospace_Annual_Average_Biomass.csv exists.")
  
  target_groups <- find_pool_codes_by_name(ref$group_names, species_patterns)
  if(nrow(target_groups) == 0)
    stop("No group names matched species_patterns = ", paste(species_patterns, collapse = ", "))
  message("plot_ga_ensemble(): matched ", nrow(target_groups), " group(s): ",
          paste(target_groups$group_name, collapse = ", "))
  
  safe_name <- function(s) gsub("[^A-Za-z0-9._-]+", "_", s)
  summary_rows <- list()
  
  for(var in vars){
    info <- VAR_TYPE_MAP[[var]]
    
    for(gi in seq_len(nrow(target_groups))){
      pool_code  <- target_groups$pool_code[gi]
      group_name <- target_groups$group_name[gi]
      
      line_rows <- list()
      for(i in seq_len(nrow(valid))){
        pr <- build_predicted_vector_generic(pool_code, 0L, var, valid$run_dir[i],
                                             caches[[valid$run_folder[i]]], group_names_lookup)
        if(is.null(pr$vec) || all(is.na(pr$vec))) next
        line_rows[[length(line_rows) + 1]] <- data.frame(
          year = pr$years, value = pr$vec,
          candidate = valid$run_folder[i], fitness = valid$fitness[i],
          stringsAsFactors = FALSE
        )
      }
      
      if(length(line_rows) == 0){
        message("  [skip] ", var, " '", group_name, "': no candidate produced predicted data.")
        next
      }
      pred_df <- do.call(rbind, line_rows)
      best_df <- pred_df[pred_df$candidate == best_row$run_folder, , drop = FALSE]
      
      init_df <- NULL
      if(!is.null(init_run_dir)){
        pr_init <- build_predicted_vector_generic(pool_code, 0L, var, init_run_dir,
                                                  init_cache, group_names_lookup)
        if(!is.null(pr_init$vec) && !all(is.na(pr_init$vec)))
          init_df <- data.frame(year = pr_init$years, value = pr_init$vec, stringsAsFactors = FALSE)
      }
      
      # obs overlay, rescaled against the best-fit candidate's line (Region 0 only;
      # single-pool-code obs only -- see @description)
      obs_df <- data.frame(year = numeric(0), value = numeric(0), series = character(0))
      if(overlay_obs && nrow(best_df) > 0){
        th <- obs$ts.head
        obs_rows <- which(th$Type %in% info$types & th$Poolcode1 == as.character(pool_code) &
                            th$Region == "0")
        if(length(obs_rows) > 0){
          obs_list <- list()
          for(r in obs_rows){
            ov <- obs$ts[[th$Title[r]]]; oy <- obs$ts$year
            keep <- !is.na(ov)
            if(!any(keep)) next
            common <- intersect(oy[keep], best_df$year)
            if(length(common) == 0) next
            q <- 1
            if(!th$Absolute[r]){
              mo <- mean(ov[keep][oy[keep] %in% common], na.rm = TRUE)
              mp <- mean(best_df$value[best_df$year %in% common], na.rm = TRUE)
              if(is.finite(mo) && is.finite(mp) && mp > 0) q <- mo / mp
            }
            obs_list[[length(obs_list) + 1]] <- data.frame(
              year = oy[keep], value = (ov / q)[keep], series = th$Title[r],
              stringsAsFactors = FALSE)
          }
          if(length(obs_list) > 0){
            obs_df <- do.call(rbind, obs_list)
            obs_df$series <- factor(obs_df$series, levels = unique(obs_df$series))
          }
        }
      }
      
      p <- ggplot2::ggplot() +
        ggplot2::geom_line(data = pred_df,
                           ggplot2::aes(x = year, y = value, group = candidate, color = fitness),
                           linewidth = 0.35, alpha = 0.65) +
        ggplot2::scale_color_viridis_c(option = "plasma", direction = -1, trans = "log10", name = "NLL") +
        { if(!is.null(init_df))
          ggplot2::geom_line(data = init_df, ggplot2::aes(x = year, y = value),
                             color = "black", linewidth = 1, linetype = "22")
        } +
        ggplot2::geom_line(data = best_df, ggplot2::aes(x = year, y = value),
                           color = "#20B2AA", linewidth = 1.1) +
        { if(nrow(obs_df) > 0)
          ggplot2::geom_point(data = obs_df, ggplot2::aes(x = year, y = value, shape = series),
                              color = "black", size = 2.2)
        } +
        ggplot2::labs(title = paste0(var, ": ", group_name),
                      x = "Year", y = info$y_lab, shape = "Observed series") +
        ggplot2::theme_bw(base_size = 11) +
        ggplot2::theme(legend.position = "right",
                       legend.text = ggplot2::element_text(size = 7),
                       plot.title = ggplot2::element_text(size = 11, face = "bold"),
                       aspect.ratio = 1) +
        { if(nrow(obs_df) > 0)
          ggplot2::scale_shape_manual(values = rep(0:25, length.out = nlevels(obs_df$series)))
        }
      
      png_file <- file.path(plots_dir, paste0(var, "_ensemble_", safe_name(group_name), ".png"))
      ggplot2::ggsave(png_file, plot = p, width = width, height = height, dpi = dpi, units = "in")
      
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        variable             = var,
        pool_code            = pool_code,
        group_name           = group_name,
        n_candidates_plotted = length(unique(pred_df$candidate)),
        best_fitness         = best_row$fitness,
        best_run_folder      = best_row$run_folder,
        png_file             = png_file,
        stringsAsFactors     = FALSE
      )
    }
  }
  
  message("Wrote ", length(summary_rows), " PNG(s) to ", normalizePath(plots_dir, mustWork = FALSE))
  invisible(do.call(rbind, summary_rows))
}


#' @title Faceted version of plot_ga_ensemble(): all stanzas of one species in one PNG.
#' @description Same data/filtering pipeline as plot_ga_ensemble(), but one PNG per
#'   (variable, species pattern) with every matching stanza as a facet. Every candidate
#'   drawn with the same uniform line width/transparency, colored by fitness (plasma
#'   scale, yellow=low/best NLL, blue/purple=high/worst NLL); the single best-fitness
#'   candidate redrawn on top as a solid turquoise line.
#' @param obs Output of read_ewe_timeseries() (only used if overlay_obs = TRUE).
#' @param ga_runs Output of resolve_run_dirs().
#' @param init_run_dir Optional baseline (pre-calibration) run folder -- if supplied,
#'   its predicted line is overlaid in black dashed in every panel.
#' @param vars Character vector of variable names (keys of VAR_TYPE_MAP).
#' @param species_patterns Character vector of case-insensitive substrings, e.g.
#'   c("gag", "red grouper"). Each pattern becomes its own faceted PNG per variable.
#' @param ylim_from_obs Logical, default TRUE. Y-clips each stanza's panel (via
#'   coord_cartesian) to its own observed range + `ylim_pad`, so wild simulations can't
#'   force the axis out past where the actual fit is readable.
#' @param ylim_pad Fractional padding on the observed range when `ylim_from_obs = TRUE`.
#'   Default 0.15.
#' @param line_width,alpha Linewidth/alpha applied uniformly to every candidate's line.
#'   Defaults (0.35/0.7) keep worse-fit candidates visible rather than washed out.
#' @param overlay_obs Same as plot_ga_ensemble().
#' @param ens_mode One of "quantile" (default, best X% by fitness), "topN" (exactly N
#'   best by rank), or "aic" (Akaike-weighted, restricted to the `top_n` best by
#'   fitness first -- see select_ensemble_candidates()). All individual candidate
#'   lines are drawn regardless of mode; "aic" additionally draws a dashed maroon line
#'   showing the AIC-weighted mean of the ensemble's predicted values at each year, a
#'   genuine weighted average rather than just a plain subset of individual lines.
#' @param keep_fitness_quantile,top_n,target_ess Passed to select_ensemble_candidates().
#' @param max_fitness,exclude_run_folders,max_candidates Same as plot_ga_ensemble().
#' @param facet_ncol Number of panel columns. Default 3.
#' @param plots_dir Output folder for PNGs. Default "plots".
#' @param width,height,dpi ggsave() figure dimensions (inches) and resolution (dpi).
#' @return Invisibly, a data.frame: one row per (variable, species pattern) panel with
#'   variable, species, n_groups, n_candidates_plotted, best_fitness, best_run_folder, png_file.
#' @export
plot_ga_ensemble_faceted <- function(obs = NULL,
                                     ga_runs,
                                     init_run_dir          = NULL,
                                     vars                  = c("Biomass", "Catch", "F"),
                                     species_patterns      = c("gag", "red grouper"),
                                     ylim_from_obs         = TRUE,
                                     ylim_pad              = 0.15,
                                     line_width = 0.35, alpha = 0.60,
                                     overlay_obs           = TRUE,
                                     ens_mode              = c("quantile", "topN", "aic"),
                                     keep_fitness_quantile = 0.9,
                                     top_n                 = 100,
                                     top_n_prop            = NULL,
                                     target_ess            = 100,
                                     max_fitness           = NULL,
                                     exclude_run_folders   = NULL,
                                     max_candidates        = 1000,
                                     facet_ncol            = 3,
                                     plots_dir             = "plots",
                                     width = 10, height = 7, dpi = 150){
  
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
  if(!requireNamespace("patchwork", quietly = TRUE))
    stop("Package 'patchwork' is required for plot_ga_ensemble_faceted() (used to give ",
         "each stanza its own y-axis range). Install it with install.packages('patchwork').")
  if(overlay_obs && is.null(obs))
    stop("overlay_obs = TRUE requires `obs` (output of read_ewe_timeseries()).")
  ens_mode <- match.arg(ens_mode)
  
  unknown_vars <- setdiff(vars, names(VAR_TYPE_MAP))
  if(length(unknown_vars) > 0)
    stop("Unrecognized variable(s) in `vars`: ", paste(unknown_vars, collapse = ", "))
  
  if(!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  
  valid <- ga_runs[ga_runs$exists, , drop = FALSE]
  if(nrow(valid) == 0)
    stop("No candidate folders found on disk -- check gen_dir / resolve_run_dirs() output.")
  
  if(!is.null(exclude_run_folders))
    valid <- valid[!(valid$run_folder %in% exclude_run_folders), , drop = FALSE]
  if(!is.null(max_fitness))
    valid <- valid[valid$fitness <= max_fitness, , drop = FALSE]
  # ens_mode selection: "quantile"/"topN" are hard subsets via the shared selection
  # function, no weights. "aic" uses real Akaike-style weights via
  # select_ensemble_candidates(mode="aic") (restricted to the top_n best by fitness
  # first, by that function's own default) -- individual candidate lines are still all
  # drawn, but ens_weight now carries real weights used below to add an actual
  # AIC-weighted mean line to each panel, not just a plain lowest-top_n-by-fitness subset.
  valid <- select_ensemble_candidates(valid, mode = ens_mode,
                                      keep_fitness_quantile = keep_fitness_quantile,
                                      top_n = top_n, top_n_prop = top_n_prop, target_ess = target_ess)
  if(nrow(valid) == 0)
    stop("No candidates left after filtering -- loosen max_fitness / ens_mode settings / exclude_run_folders.")
  if(nrow(valid) > max_candidates){
    best_idx_pre <- which.min(valid$fitness)
    keep <- unique(c(best_idx_pre, sample(seq_len(nrow(valid)), max_candidates - 1)))
    valid <- valid[keep, , drop = FALSE]
  }
  
  best_row <- valid[which.min(valid$fitness), , drop = FALSE]
  message("plot_ga_ensemble_faceted(): ", nrow(valid), " candidates (ens_mode = '", ens_mode,
          if(ens_mode == "aic") paste0("', AIC-weighted, target ESS=", target_ess) else "'",
          "); best fitness = ", signif(best_row$fitness, 6), " (", best_row$run_folder, ").")
  
  caches <- setNames(lapply(seq_len(nrow(valid)), function(i) new.env(parent = emptyenv())),
                     valid$run_folder)
  init_cache <- if(!is.null(init_run_dir)) new.env(parent = emptyenv()) else NULL
  group_names_lookup <- function(pr, pool_codes) pr$group_names[pool_codes + 1L]
  
  ref <- NULL
  for(i in seq_len(nrow(valid))){
    ref <- get_region_output(0L, "Biomass", valid$run_dir[i], caches[[valid$run_folder[i]]])
    if(!is.null(ref)) break
  }
  if(is.null(ref))
    stop("Could not read a domain-average Biomass file from ANY candidate folder.")
  
  target_groups <- find_pool_codes_by_name(ref$group_names, species_patterns)
  if(nrow(target_groups) == 0)
    stop("No group names matched species_patterns = ", paste(species_patterns, collapse = ", "))
  
  # assign each matched group to whichever pattern it matches first (patterns listed
  # earlier win if a name matches more than one)
  target_groups$species <- vapply(target_groups$group_name, function(nm){
    hit <- species_patterns[vapply(species_patterns, function(p) grepl(p, nm, ignore.case = TRUE), logical(1))]
    if(length(hit) == 0) NA_character_ else hit[1]
  }, character(1))
  target_groups <- target_groups[!is.na(target_groups$species), , drop = FALSE]
  
  safe_name <- function(s) gsub("[^A-Za-z0-9._-]+", "_", s)
  summary_rows <- list()
  
  for(var in vars){
    info <- VAR_TYPE_MAP[[var]]
    
    for(sp in unique(target_groups$species)){
      grp_rows <- target_groups[target_groups$species == sp, , drop = FALSE]
      
      pred_list <- list()
      obs_list  <- list()
      init_list <- list()
      
      for(gi in seq_len(nrow(grp_rows))){
        pool_code  <- grp_rows$pool_code[gi]
        group_name <- grp_rows$group_name[gi]
        
        for(i in seq_len(nrow(valid))){
          pr <- build_predicted_vector_generic(pool_code, 0L, var, valid$run_dir[i],
                                               caches[[valid$run_folder[i]]], group_names_lookup)
          if(is.null(pr$vec) || all(is.na(pr$vec))) next
          pred_list[[length(pred_list) + 1]] <- data.frame(
            year = pr$years, value = pr$vec, group_name = group_name,
            candidate = valid$run_folder[i], fitness = valid$fitness[i],
            weight = if("ens_weight" %in% names(valid)) valid$ens_weight[i] else NA_real_,
            stringsAsFactors = FALSE
          )
        }
        
        if(!is.null(init_run_dir)){
          pr_init <- build_predicted_vector_generic(pool_code, 0L, var, init_run_dir,
                                                    init_cache, group_names_lookup)
          if(!is.null(pr_init$vec) && !all(is.na(pr_init$vec)))
            init_list[[length(init_list) + 1]] <- data.frame(
              year = pr_init$years, value = pr_init$vec, group_name = group_name,
              stringsAsFactors = FALSE)
        }
        
        if(overlay_obs){
          th <- obs$ts.head
          obs_rows <- which(th$Type %in% info$types & th$Poolcode1 == as.character(pool_code) &
                              th$Region == "0")
          for(r in obs_rows){
            ov <- obs$ts[[th$Title[r]]]; oy <- obs$ts$year
            keep <- !is.na(ov)
            if(!any(keep)) next
            obs_list[[length(obs_list) + 1]] <- data.frame(
              year = oy[keep], value_raw = ov[keep], series = th$Title[r],
              group_name = group_name, absolute = th$Absolute[r], stringsAsFactors = FALSE
            )
          }
        }
      }
      
      if(length(pred_list) == 0){
        message("  [skip] ", var, " '", sp, "': no candidate produced predicted data for any stanza.")
        next
      }
      pred_df <- do.call(rbind, pred_list)
      pred_df$group_name <- factor(pred_df$group_name, levels = grp_rows$group_name)
      best_df <- pred_df[pred_df$candidate == best_row$run_folder, , drop = FALSE]
      
      # Order candidates from highest NLL to lowest NLL so that the lowest-NLL
      # candidates are drawn last and appear on top. Sorting pred_df's ROWS alone
      # (the previous approach) does NOT control draw order here: ggplot2 draws a
      # grouped geom_line() by the grouping variable's factor level order, not by
      # data-frame row order -- so `candidate` must be set as an explicit factor with
      # levels in the desired draw order. With plasma + direction = -1, the lowest
      # NLL candidates are yellow, so this puts the best-supported lines visually on
      # top of the worse-supported ones.
      candidate_order <- unique(pred_df[order(pred_df$fitness, decreasing = TRUE), "candidate"])
      pred_df$candidate <- factor(pred_df$candidate, levels = candidate_order)
      
      init_df <- if(length(init_list) > 0) do.call(rbind, init_list) else NULL
      if(!is.null(init_df)) init_df$group_name <- factor(init_df$group_name, levels = grp_rows$group_name)
      
      # rescale obs per stanza against that stanza's best-fit line, then combine
      obs_df <- data.frame(year = numeric(0), value = numeric(0), series = character(0),
                           group_name = character(0))
      if(length(obs_list) > 0){
        raw <- do.call(rbind, obs_list)
        scaled_list <- list()
        for(gn in unique(raw$group_name)){
          sub <- raw[raw$group_name == gn, , drop = FALSE]
          bd  <- best_df[best_df$group_name == gn, , drop = FALSE]
          for(sr in unique(sub$series)){
            s2 <- sub[sub$series == sr, , drop = FALSE]
            common <- intersect(s2$year, bd$year)
            q <- 1
            if(!isTRUE(s2$absolute[1]) && length(common) > 0){
              mo <- mean(s2$value_raw[s2$year %in% common], na.rm = TRUE)
              mp <- mean(bd$value[bd$year %in% common], na.rm = TRUE)
              if(is.finite(mo) && is.finite(mp) && mp > 0) q <- mo / mp
            }
            scaled_list[[length(scaled_list) + 1]] <- data.frame(
              year = s2$year, value = s2$value_raw / q, series = sr,
              group_name = gn, stringsAsFactors = FALSE)
          }
        }
        obs_df <- do.call(rbind, scaled_list)
        obs_df$group_name <- factor(obs_df$group_name, levels = grp_rows$group_name)
        obs_df$series <- factor(obs_df$series, levels = unique(obs_df$series))
      }
      
      # ---- custom in-panel observed-series legend (points + text), top-left inside
      # the panel -- title now sits OUTSIDE the panel (standard ggplot title, see
      # below), so there's no collision risk keeping this legend at the top.
      make_obs_legend <- function(sub_obs, y_min, y_max, x_min, x_range){
        series_levels <- unique(as.character(sub_obs$series))
        n_series <- length(series_levels)
        y_range <- y_max - y_min
        legend_x <- x_min + x_range * 0.043
        legend_spacing <- y_range * 0.045
        legend_y <- y_max - y_range * 0.035 - seq(0, n_series - 1) * legend_spacing
        data.frame(year = legend_x, value = legend_y, series = series_levels)
      }
      
      # ---- fixed grid position per panel: show_y_axis only in the first column,
      # show_x_axis only at the bottom of whichever column a panel falls in (so an
      # incomplete last row still gets correct x-axes on the panels that are actually
      # bottommost in their own column) ----
      group_levels <- levels(pred_df$group_name)
      n_groups <- length(group_levels)
      fitness_limits <- range(valid$fitness, na.rm = TRUE)
      
      panel_list <- list()
      for(idx in seq_along(group_levels)){
        gn <- group_levels[idx]
        col_pos <- ((idx - 1) %% facet_ncol) + 1
        show_y_axis <- (col_pos == 1)
        panels_in_this_col <- seq(col_pos, n_groups, by = facet_ncol)
        show_x_axis <- (idx == max(panels_in_this_col))
        
        sub_pred <- pred_df[pred_df$group_name == gn, , drop = FALSE]
        sub_best <- best_df[best_df$group_name == gn, , drop = FALSE]
        sub_init <- if(!is.null(init_df)) init_df[init_df$group_name == gn, , drop = FALSE] else NULL
        sub_obs  <- if(nrow(obs_df) > 0) obs_df[obs_df$group_name == gn, , drop = FALSE] else obs_df
        if(nrow(sub_pred) == 0) next
        sub_obs$series <- droplevels(sub_obs$series)
        
        # AIC-weighted mean time series, per year, across this panel's own candidates --
        # only meaningful when ens_mode=="aic" (weight is NA for quantile/topN, which
        # have no weights at all). This is a genuine weighted average of the ensemble's
        # predicted values, not just a plain subset of individual lines.
        sub_wmean <- NULL
        if(ens_mode == "aic" && "weight" %in% names(sub_pred) && !all(is.na(sub_pred$weight))){
          wm_dt <- data.table::as.data.table(sub_pred)
          sub_wmean <- as.data.frame(wm_dt[, .(value = stats::weighted.mean(value, weight, na.rm = TRUE)),
                                           by = year])
        }
        
        # include sub_init's own values in the range, not just sub_obs -- otherwise
        # coord_cartesian(ylim=) below silently clips the init (dashed) line out of
        # view whenever its magnitude falls outside the observed data's own range,
        # which happens often for F/Catch where the uncalibrated init model can differ
        # substantially from what calibration converged to. The ensemble candidates
        # (sub_pred) are deliberately NOT included here -- that's the actual point of
        # ylim_from_obs, keeping the axis anchored to the observed fit rather than
        # letting a wild simulation blow it out -- but init is a specific, named
        # reference line the user explicitly wants visible regardless.
        obs_and_init_vals <- c(sub_obs$value, if(!is.null(sub_init)) sub_init$value else NULL,
                               if(!is.null(sub_wmean)) sub_wmean$value else NULL)
        if(ylim_from_obs && length(obs_and_init_vals) > 0){
          rng <- range(obs_and_init_vals, na.rm = TRUE)
        } else {
          rng <- range(sub_pred$value, na.rm = TRUE)
        }
        pad <- diff(rng) * ylim_pad
        if(!is.finite(pad) || pad <= 0) pad <- max(abs(rng), 1) * ylim_pad
        # extra headroom at the top (14%) reserved for the custom in-panel legend --
        # the title no longer needs panel space since it now sits outside the panel
        y_range_base <- rng[2] - rng[1]
        y_min <- max(0, rng[1] - pad)
        y_max_plot <- rng[2] + pad + y_range_base * 0.14
        
        x_rng <- range(sub_pred$year, na.rm = TRUE)
        obs_legend <- if(nrow(sub_obs) > 0)
          make_obs_legend(sub_obs, y_min, y_max_plot, x_rng[1], diff(x_rng)) else NULL
        # if this panel has more than one observed series, they need to be visually
        # distinguishable -- map shape to series (not a fixed shape=21 for everyone),
        # both on the actual data points and on the matching legend key points
        n_series_here <- nlevels(sub_obs$series)
        
        pp <- ggplot2::ggplot() +
          ggplot2::geom_line(data = sub_pred,
                             ggplot2::aes(x = year, y = value, group = candidate, color = fitness),
                             linewidth = line_width, alpha = alpha)
        if(!is.null(sub_init) && nrow(sub_init) > 0)
          pp <- pp + ggplot2::geom_line(data = sub_init, ggplot2::aes(x = year, y = value),
                                        color = "black", linewidth = 1.25, linetype = "22")
        pp <- pp + ggplot2::geom_line(data = sub_best, ggplot2::aes(x = year, y = value),
                                      color = "#20B2AA", linewidth = 1.25, linetype = "22")
        if(!is.null(sub_wmean))
          pp <- pp + ggplot2::geom_line(data = sub_wmean, ggplot2::aes(x = year, y = value),
                                        color = "grey30", linewidth = 1.25, linetype = "solid")
        if(nrow(sub_obs) > 0)
          pp <- pp + ggplot2::geom_point(data = sub_obs, ggplot2::aes(x = year, y = value, shape = series),
                                         fill = "white", color = "black", size = 1.5, stroke = 0.7)
        if(!is.null(obs_legend)){
          pp <- pp +
            ggplot2::geom_point(data = obs_legend, ggplot2::aes(x = year, y = value, shape = series),
                                fill = "white", color = "black", size = 1.9, stroke = 0.7,
                                inherit.aes = FALSE, show.legend = FALSE) +
            ggplot2::geom_text(data = obs_legend,
                               ggplot2::aes(x = year + diff(x_rng) * 0.026, y = value, label = series),
                               hjust = 0, vjust = 0.5, size = 2.6, color = "black", inherit.aes = FALSE)
        }
        if(nrow(sub_obs) > 0)
          pp <- pp + ggplot2::scale_shape_manual(values = rep(21:25, length.out = n_series_here), guide = "none")
        pp <- pp +
          ggplot2::scale_color_viridis_c(option = "plasma", direction = -1, trans = "log10",
                                         name = "NLL", limits = fitness_limits, guide = "none") +
          ggplot2::scale_x_continuous(expand = c(0, 0)) +
          ggplot2::scale_y_continuous(expand = c(0, 0)) +
          # coord_cartesian(), not scale_y_continuous(limits=) -- the latter DROPS any
          # point outside the range entirely, which is why several ensemble candidate
          # lines were missing from the plot (their values extended beyond the
          # observed-data-derived y-range). coord_cartesian() just clips the visible
          # window, keeping every line's full data intact.
          ggplot2::coord_cartesian(ylim = c(y_min, y_max_plot)) +
          ggplot2::labs(title = gn, x = "Year", y = info$y_lab) +
          ggplot2::theme_bw(base_size = 10) +
          ggplot2::theme(
            aspect.ratio = 1,
            panel.grid.major = ggplot2::element_line(colour = scales::alpha("grey25", 0.35),
                                                     linetype = "dashed", linewidth = 0.2),
            panel.grid.minor = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5,
                                               margin = ggplot2::margin(0, 0, 4, 0)),
            axis.title.x = if(show_x_axis) ggplot2::element_text(size = 11) else ggplot2::element_blank(),
            axis.text.x  = if(show_x_axis) ggplot2::element_text(size = 9)  else ggplot2::element_blank(),
            axis.ticks.x = if(show_x_axis) ggplot2::element_line(linewidth = 0.5) else ggplot2::element_blank(),
            axis.title.y = if(show_y_axis) ggplot2::element_text(size = 11) else ggplot2::element_blank(),
            axis.text.y  = if(show_y_axis) ggplot2::element_text(size = 9)  else ggplot2::element_blank(),
            axis.ticks.y = if(show_y_axis) ggplot2::element_line(linewidth = 0.5) else ggplot2::element_blank(),
            legend.position = "none",
            plot.margin = ggplot2::margin(4, 2, 4, 2)
          )
        
        panel_list[[gn]] <- pp
      }
      
      if(length(panel_list) == 0){
        message("  [skip] ", var, " '", sp, "': no panels to plot.")
        next
      }
      
      # ---- dedicated bottom legend for the three FIXED-color reference lines (init,
      # best-fit, ensemble mean) -- these are drawn via separate geom_line() calls with
      # hardcoded colors, not mapped to any aesthetic, so ggplot2 never generates a
      # legend for them on its own. Same technique as the NLL colorbar above: build a
      # tiny standalone ggplot with a manual discrete color scale, extract its legend
      # grob, and place it below the combined figure. No legend title (name = NULL),
      # single horizontal row. Only includes whichever lines this call actually draws.
      ref_line_labels <- character(0)
      ref_line_colors <- character(0)
      ref_line_linetypes <- character(0)
      if(!is.null(init_run_dir)){
        ref_line_labels <- c(ref_line_labels, "Baseline model")
        ref_line_colors <- c(ref_line_colors, "Baseline model" = "black")
        ref_line_linetypes <- c(ref_line_linetypes, "Baseline model" = "22")
      }
      ref_line_labels <- c(ref_line_labels, "Best model")
      ref_line_colors <- c(ref_line_colors, "Best model" = "#20B2AA")
      ref_line_linetypes <- c(ref_line_linetypes, "Best model" = "22")
      if(ens_mode == "aic"){
        ref_line_labels <- c(ref_line_labels, "Ensemble model")
        ref_line_colors <- c(ref_line_colors, "Ensemble model" = "grey30")
        ref_line_linetypes <- c(ref_line_linetypes, "Ensemble model" = "solid")
      }
      ref_legend_df <- expand.grid(x = c(1, 2),
                                   line = factor(ref_line_labels, levels = ref_line_labels))
      ref_legend_df$y <- 1
      # linetype mapped alongside color, matching each label's ACTUAL panel style
      # (baseline/best are dashed "22", ensemble is solid) -- previously linetype was
      # never mapped in this dummy legend at all, so every key showed the same default
      # style regardless of what the real panel lines used. ggplot2 merges color and
      # linetype into ONE combined guide automatically since both share the same
      # grouping variable (line) and labels.
      ref_legend_source <- ggplot2::ggplot(ref_legend_df,
                                           ggplot2::aes(x = x, y = y, color = line, linetype = line)) +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::scale_color_manual(values = ref_line_colors, name = NULL) +
        ggplot2::scale_linetype_manual(values = ref_line_linetypes, name = NULL) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "bottom",
                       legend.text = ggplot2::element_text(size = 10),
                       legend.margin = ggplot2::margin(0, 0, 0, 0),
                       plot.margin = ggplot2::margin(0, 0, 0, 0),
                       # widened so a dashed key actually shows multiple dash segments
                       # rather than looking like one short, ambiguous stroke
                       legend.key.width = grid::unit(1.6, "cm")) +
        ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 1.5)),
                        linetype = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 1.5)))
      
      ref_grob <- ggplot2::ggplotGrob(ref_legend_source)
      ref_guide_index <- which(vapply(ref_grob$grobs, function(x)
        inherits(x, "gtable") && x$name == "guide-box", logical(1)))
      ref_legend_plot <- if(length(ref_guide_index) > 0)
        patchwork::wrap_elements(full = ref_grob$grobs[[ref_guide_index[1]]], clip = FALSE) else NULL
      
      # ---- dedicated, separately-built NLL colorbar, extracted as a grob and placed
      # beside the panel grid via patchwork -- gives full, direct control over its own
      # size/position, rather than trying to derive one from per-panel legends via
      # guides="collect" (which produced a legend that didn't reliably span the whole
      # figure the way this explicit approach does).
      # This now matches ensemble_facet_example.R's exact technique -- earlier
      # attempts (unit(1,"npc") for barheight, then a negative plot.margin) didn't
      # actually achieve the alignment; the example's real mechanism is a
      # plot_spacer() with a NEGATIVE height stacked ABOVE the legend via `/` (see
      # nll_legend_plot below), which shifts the legend upward to compensate for the
      # panels' own title space -- not a margin tweak on the legend plot itself.
      nll_legend_source <- ggplot2::ggplot(valid, ggplot2::aes(x = 1, y = fitness, colour = fitness)) +
        ggplot2::geom_point(alpha = 0) +
        ggplot2::scale_colour_viridis_c(option = "plasma", direction = -1, trans = "log10",
                                        limits = fitness_limits, name = "NLL",
                                        guide = ggplot2::guide_colorbar(
                                          title.position = "top", title.hjust = 0,
                                          barheight = grid::unit(14.65, "cm"),
                                          barwidth = grid::unit(0.55, "cm"),
                                          frame.colour = "black", frame.linewidth = 0.5,
                                          ticks = TRUE, ticks.colour = "black")) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "right", legend.justification = "left",
                       legend.title = ggplot2::element_text(size = 11, hjust = 0),
                       legend.text = ggplot2::element_text(size = 10),
                       legend.margin = ggplot2::margin(0, 0, 0, 0),
                       plot.margin = ggplot2::margin(0, 0, 0, 0))
      
      nll_grob <- ggplot2::ggplotGrob(nll_legend_source)
      guide_index <- which(vapply(nll_grob$grobs, function(x)
        inherits(x, "gtable") && x$name == "guide-box", logical(1)))
      
      n_rows_actual <- ceiling(length(panel_list) / facet_ncol)
      panel_grid <- patchwork::wrap_plots(panel_list, ncol = facet_ncol, nrow = n_rows_actual)
      
      if(length(guide_index) > 0){
        nll_legend_grob <- nll_grob$grobs[[guide_index[1]]]
        # plot_spacer() with a NEGATIVE height, stacked ABOVE the legend -- this is
        # the actual alignment mechanism, not a margin on the legend plot itself
        nll_legend_plot <- (patchwork::plot_spacer() /
                              patchwork::wrap_elements(full = nll_legend_grob, clip = FALSE)) +
          patchwork::plot_layout(heights = c(-0.11, 0.98))
        p <- (panel_grid | nll_legend_plot) + patchwork::plot_layout(widths = c(1, 0.075))
      } else {
        message("  [note] could not extract NLL colorbar grob -- combined figure will have no NLL legend.")
        p <- panel_grid
      }
      
      # stack the reference-line legend below the whole figure (panel grid + NLL bar)
      if(!is.null(ref_legend_plot)){
        p <- (p / ref_legend_plot) + patchwork::plot_layout(heights = c(1, 0.04))
      } else {
        message("  [note] could not extract reference-line legend grob -- combined figure will ",
                "have no bottom legend for init/best-fit/ensemble-mean lines.")
      }
      
      png_file <- file.path(plots_dir, paste0(var, "_ensemble_facet_", safe_name(sp), "_", ens_mode, ".png"))
      ggplot2::ggsave(png_file, plot = p, width = width, height = height, dpi = dpi, units = "in")
      
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        variable             = var,
        species              = sp,
        ens_mode             = ens_mode,
        n_groups             = length(panel_list),
        n_candidates_plotted = length(unique(pred_df$candidate)),
        best_fitness         = best_row$fitness,
        best_run_folder      = best_row$run_folder,
        png_file             = png_file,
        stringsAsFactors     = FALSE
      )
    }
  }
  
  message("Wrote ", length(summary_rows), " PNG(s) to ", normalizePath(plots_dir, mustWork = FALSE))
  invisible(do.call(rbind, summary_rows))
}


#' @title Plot EVERY archived candidate from the final GA population, colored by fitness.
#' @description IMPORTANT FRAMING NOTE: all 910 folders on disk are the generation-30
#'   population itself -- there is no separately-archived output from generations 1-29
#'   (per your colleague: "I didn't save those outputs"). Each candidate's folder name
#'   also carries a `g<NN>` token recording which generation originally DISCOVERED that
#'   individual's parameter set (elites survive and get carried forward by the GA's
#'   elitism, so a gen-30 population member can carry an earlier origin tag like "g027"
#'   if it's a persisting elite) -- that origin-generation info is still returned in the
#'   summary table (`generations_plotted`), but is NOT used for coloring: since 891 of
#'   906 candidates (98%) are literally generation 30, coloring by that label conveyed
#'   almost no visual information and was actively misleading. Every archived candidate
#'   is plotted individually, colored continuously by FITNESS (same convention as
#'   plot_ga_ensemble_faceted() and every other ensemble plot in this script), with the
#'   single best-fitness candidate overall redrawn as a bold black line, faceted by
#'   stanza (one PNG per variable x species) via the same patchwork/y-clipping-to-
#'   observed-range approach as plot_ga_ensemble_faceted().
#' @param obs Output of read_ewe_timeseries() (only used if overlay_obs = TRUE).
#' @param ga_runs Output of resolve_run_dirs() -- must have a `generation` column
#'   (added automatically by the current read_ga_runs()).
#' @param vars Character vector of variable names (keys of VAR_TYPE_MAP).
#' @param species_patterns Character vector of case-insensitive substrings, e.g.
#'   c("gag", "red grouper").
#' @param palette "viridis" (default) or "plasma" -- which continuous palette to use for
#'   the fitness color scale.
#' @param overlay_obs Logical; overlay observed series matching each plotted pool code at
#'   Region 0. Default TRUE.
#' @param ylim_from_obs,ylim_pad Same as plot_ga_ensemble_faceted() -- clip each panel's
#'   y-axis to that stanza's observed range (+ padding) so lines that stray don't blow
#'   out the axis and hide the fit near the data.
#' @param facet_ncol Number of panel columns. Default 3.
#' @param plots_dir Output folder for PNGs (created if it doesn't exist). Default "plots".
#' @param width,height,dpi ggsave() figure dimensions (inches) and resolution (dpi).
#' @return Invisibly, a data.frame summarizing what was plotted: one row per (variable,
#'   species) panel with variable, species, n_candidates_plotted, n_generations_plotted,
#'   generations_plotted, best_fitness, best_run_folder, best_generation, png_file.
#' @export
plot_ga_generation_best <- function(obs = NULL,
                                    ga_runs,
                                    vars              = c("Biomass", "Catch", "F"),
                                    species_patterns  = c("gag", "red grouper"),
                                    palette           = c("viridis", "plasma"),
                                    overlay_obs       = TRUE,
                                    ylim_from_obs     = TRUE,
                                    ylim_pad          = 0.15,
                                    facet_ncol        = 3,
                                    plots_dir         = "plots",
                                    width = 10, height = 7, dpi = 150){
  
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
  if(!requireNamespace("patchwork", quietly = TRUE))
    stop("Package 'patchwork' is required. Install it with install.packages('patchwork').")
  if(!"generation" %in% names(ga_runs))
    stop("ga_runs has no 'generation' column -- re-run ga_runs <- read_ga_runs(...) with the ",
         "current version of this script (it parses generation from the run folder name).")
  palette <- match.arg(palette)
  
  unknown_vars <- setdiff(vars, names(VAR_TYPE_MAP))
  if(length(unknown_vars) > 0)
    stop("Unrecognized variable(s) in `vars`: ", paste(unknown_vars, collapse = ", "))
  
  if(!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  
  valid <- ga_runs[ga_runs$exists & !is.na(ga_runs$generation), , drop = FALSE]
  if(nrow(valid) == 0)
    stop("No candidates with both an on-disk folder and a parseable generation number.")
  
  n_gen_total <- length(unique(ga_runs$generation[!is.na(ga_runs$generation)]))
  n_gen_ondisk <- length(unique(valid$generation))
  message("plot_ga_generation_best(): ", n_gen_ondisk, " of ", n_gen_total,
          " generation(s) in the fitness table have at least one on-disk candidate folder.")
  
  caches <- setNames(lapply(seq_len(nrow(valid)), function(i) new.env(parent = emptyenv())),
                     valid$run_folder)
  group_names_lookup <- function(pr, pool_codes) pr$group_names[pool_codes + 1L]
  
  ref <- NULL
  for(i in seq_len(nrow(valid))){
    ref <- get_region_output(0L, "Biomass", valid$run_dir[i], caches[[valid$run_folder[i]]])
    if(!is.null(ref)) break
  }
  if(is.null(ref)) stop("Could not read a domain-average Biomass file from ANY candidate folder.")
  
  target_groups <- find_pool_codes_by_name(ref$group_names, species_patterns)
  if(nrow(target_groups) == 0)
    stop("No group names matched species_patterns = ", paste(species_patterns, collapse = ", "))
  target_groups$species <- vapply(target_groups$group_name, function(nm){
    hit <- species_patterns[vapply(species_patterns, function(p) grepl(p, nm, ignore.case = TRUE), logical(1))]
    if(length(hit) == 0) NA_character_ else hit[1]
  }, character(1))
  target_groups <- target_groups[!is.na(target_groups$species), , drop = FALSE]
  
  safe_name <- function(s) gsub("[^A-Za-z0-9._-]+", "_", s)
  summary_rows <- list()
  # Colored by FITNESS (continuous), same convention as every other ensemble plot in this
  # script -- not by origin-generation label. An earlier version colored by generation,
  # but since 891 of 906 candidates (98%) ARE generation 30, that coloring conveyed almost
  # no information (nearly every line was the same label) and was actively misleading.
  # Fitness is the informative axis here regardless of which generation an individual was
  # originally discovered in.
  
  for(var in vars){
    info <- VAR_TYPE_MAP[[var]]
    
    # for THIS variable, restrict to candidates that actually have archived data for it,
    # then take the min-fitness candidate per generation among those -- a generation's
    # true best may not be archived, so we can only show its best ARCHIVED candidate
    has_var_file <- vapply(valid$run_dir, function(d)
      file.exists(file.path(d, sprintf("Ecospace_Annual_Average_%s.csv", var))) ||
        (!is.null(VAR_TYPE_MAP[[var]]$derived_from) &&
           all(vapply(VAR_TYPE_MAP[[var]]$derived_from, function(v)
             file.exists(file.path(d, sprintf("Ecospace_Annual_Average_%s.csv", v))), logical(1)))),
      logical(1))
    var_valid <- valid[has_var_file, , drop = FALSE]
    if(nrow(var_valid) == 0){
      message("[", var, "] no archived candidate has this variable's file(s); skipping.")
      next
    }
    var_valid <- valid[has_var_file, , drop = FALSE]
    if(nrow(var_valid) == 0){
      message("[", var, "] no archived candidate has this variable's file(s); skipping.")
      next
    }
    var_valid <- var_valid[order(var_valid$generation), ]
    n_labels <- length(unique(var_valid$generation))
    message("[", var, "] ", nrow(var_valid), " archived candidate(s) across ", n_labels,
            " origin-generation label(s) (", paste(sort(unique(var_valid$generation)), collapse = ","),
            ") -- ALL are plotted individually, colored by origin-generation label ",
            "(see function docs: these are all generation-30 population members, not ",
            "separately-archived sequential generations).")
    
    overall_best <- var_valid[which.min(var_valid$fitness), ]
    
    for(sp in unique(target_groups$species)){
      grp_rows <- target_groups[target_groups$species == sp, , drop = FALSE]
      
      pred_list <- list()
      obs_list  <- list()
      
      for(gi in seq_len(nrow(grp_rows))){
        pool_code  <- grp_rows$pool_code[gi]
        group_name <- grp_rows$group_name[gi]
        
        for(i in seq_len(nrow(var_valid))){
          pr <- build_predicted_vector_generic(pool_code, 0L, var, var_valid$run_dir[i],
                                               caches[[var_valid$run_folder[i]]], group_names_lookup)
          if(is.null(pr$vec) || all(is.na(pr$vec))) next
          pred_list[[length(pred_list) + 1]] <- data.frame(
            year = pr$years, value = pr$vec, group_name = group_name,
            candidate = var_valid$run_folder[i], generation = var_valid$generation[i],
            fitness = var_valid$fitness[i], stringsAsFactors = FALSE
          )
        }
        
        if(overlay_obs){
          th <- obs$ts.head
          obs_rows <- which(th$Type %in% info$types & th$Poolcode1 == as.character(pool_code) &
                              th$Region == "0")
          for(r in obs_rows){
            ov <- obs$ts[[th$Title[r]]]; oy <- obs$ts$year
            keep <- !is.na(ov)
            if(!any(keep)) next
            obs_list[[length(obs_list) + 1]] <- data.frame(
              year = oy[keep], value_raw = ov[keep], series = th$Title[r],
              group_name = group_name, absolute = th$Absolute[r], stringsAsFactors = FALSE
            )
          }
        }
      }
      
      if(length(pred_list) == 0){
        message("  [skip] ", var, " '", sp, "': no candidate produced predicted data.")
        next
      }
      pred_df <- do.call(rbind, pred_list)
      pred_df$group_name <- factor(pred_df$group_name, levels = grp_rows$group_name)
      pred_df$generation <- factor(pred_df$generation, levels = sort(unique(pred_df$generation)))
      best_df <- pred_df[pred_df$candidate == overall_best$run_folder, , drop = FALSE]
      
      # Order candidates from highest NLL to lowest NLL so the lowest-NLL (best, yellow
      # with plasma+direction=-1) candidates draw last and appear on top -- matching
      # plot_ga_ensemble_faceted()'s fix. The previous "sort by generation" comment/
      # logic here was stale (left over from before this function was changed to color
      # by continuous fitness rather than origin-generation label -- see the function's
      # own docs), and even for its original purpose, sorting pred_df's ROWS alone does
      # NOT control draw order for a grouped geom_line(): ggplot2 draws by the grouping
      # variable's factor level order, not row order, so `candidate` must be an
      # explicit factor with levels in the desired draw order.
      candidate_order <- unique(pred_df[order(pred_df$fitness, decreasing = TRUE), "candidate"])
      pred_df$candidate <- factor(pred_df$candidate, levels = candidate_order)
      
      obs_df <- data.frame(year = numeric(0), value = numeric(0), series = character(0),
                           group_name = character(0))
      if(length(obs_list) > 0){
        raw <- do.call(rbind, obs_list)
        scaled_list <- list()
        for(gn in unique(raw$group_name)){
          sub <- raw[raw$group_name == gn, , drop = FALSE]
          bd  <- best_df[best_df$group_name == gn, , drop = FALSE]
          for(sr in unique(sub$series)){
            s2 <- sub[sub$series == sr, , drop = FALSE]
            common <- intersect(s2$year, bd$year)
            q <- 1
            if(!isTRUE(s2$absolute[1]) && length(common) > 0){
              mo <- mean(s2$value_raw[s2$year %in% common], na.rm = TRUE)
              mp <- mean(bd$value[bd$year %in% common], na.rm = TRUE)
              if(is.finite(mo) && is.finite(mp) && mp > 0) q <- mo / mp
            }
            scaled_list[[length(scaled_list) + 1]] <- data.frame(
              year = s2$year, value = s2$value_raw / q, series = sr,
              group_name = gn, stringsAsFactors = FALSE)
          }
        }
        obs_df <- do.call(rbind, scaled_list)
        obs_df$group_name <- factor(obs_df$group_name, levels = grp_rows$group_name)
        obs_df$series <- factor(obs_df$series, levels = unique(obs_df$series))
      }
      
      # ---- custom in-panel observed-series legend (points + text), top-left inside
      # the panel -- same convention as plot_ga_ensemble_faceted() (item 3: this
      # function should match that one's configuration)
      make_obs_legend <- function(sub_obs, y_min, y_max, x_min, x_range){
        series_levels <- unique(as.character(sub_obs$series))
        n_series <- length(series_levels)
        y_range <- y_max - y_min
        legend_x <- x_min + x_range * 0.043
        legend_spacing <- y_range * 0.045
        legend_y <- y_max - y_range * 0.035 - seq(0, n_series - 1) * legend_spacing
        data.frame(year = legend_x, value = legend_y, series = series_levels)
      }
      
      # ---- fixed grid position per panel: y-axis only in first column, x-axis only
      # at the bottom of each column ----
      group_levels <- levels(pred_df$group_name)
      n_groups_here <- length(group_levels)
      fitness_limits <- range(var_valid$fitness, na.rm = TRUE)
      
      panel_list <- list()
      for(idx in seq_along(group_levels)){
        gn <- group_levels[idx]
        col_pos <- ((idx - 1) %% facet_ncol) + 1
        show_y_axis <- (col_pos == 1)
        panels_in_this_col <- seq(col_pos, n_groups_here, by = facet_ncol)
        show_x_axis <- (idx == max(panels_in_this_col))
        
        sub_pred <- pred_df[pred_df$group_name == gn, , drop = FALSE]
        sub_best <- best_df[best_df$group_name == gn, , drop = FALSE]
        sub_obs  <- if(nrow(obs_df) > 0) obs_df[obs_df$group_name == gn, , drop = FALSE] else obs_df
        if(nrow(sub_pred) == 0) next
        sub_obs$series <- droplevels(sub_obs$series)
        
        if(ylim_from_obs && nrow(sub_obs) > 0){
          rng <- range(sub_obs$value, na.rm = TRUE)
        } else {
          rng <- range(sub_pred$value, na.rm = TRUE)
        }
        pad <- diff(rng) * ylim_pad
        if(!is.finite(pad) || pad <= 0) pad <- max(abs(rng), 1) * ylim_pad
        # extra headroom at the top (14%) reserved for the custom in-panel legend --
        # the title sits outside the panel (standard ggplot title), so it doesn't
        # need panel space
        y_range_base <- rng[2] - rng[1]
        y_min <- max(0, rng[1] - pad)
        y_max_plot <- rng[2] + pad + y_range_base * 0.14
        
        x_rng <- range(sub_pred$year, na.rm = TRUE)
        obs_legend <- if(nrow(sub_obs) > 0)
          make_obs_legend(sub_obs, y_min, y_max_plot, x_rng[1], diff(x_rng)) else NULL
        n_series_here <- nlevels(sub_obs$series)
        
        pp <- ggplot2::ggplot() +
          ggplot2::geom_line(data = sub_pred,
                             ggplot2::aes(x = year, y = value, group = candidate, color = fitness),
                             linewidth = 0.35, alpha = 0.35)
        pp <- pp + ggplot2::geom_line(data = sub_best, ggplot2::aes(x = year, y = value),
                                      color = "#20B2AA", linewidth = 1.25)
        if(nrow(sub_obs) > 0)
          pp <- pp + ggplot2::geom_point(data = sub_obs, ggplot2::aes(x = year, y = value, shape = series),
                                         fill = "white", color = "black", size = 2.0, stroke = 0.8)
        if(!is.null(obs_legend)){
          pp <- pp +
            ggplot2::geom_point(data = obs_legend, ggplot2::aes(x = year, y = value, shape = series),
                                fill = "white", color = "black", size = 1.9, stroke = 0.7,
                                inherit.aes = FALSE, show.legend = FALSE) +
            ggplot2::geom_text(data = obs_legend,
                               ggplot2::aes(x = year + diff(x_rng) * 0.026, y = value, label = series),
                               hjust = 0, vjust = 0.5, size = 2.6, color = "black", inherit.aes = FALSE)
        }
        if(nrow(sub_obs) > 0)
          pp <- pp + ggplot2::scale_shape_manual(values = rep(21:25, length.out = n_series_here), guide = "none")
        pp <- pp +
          (if(palette == "plasma")
            ggplot2::scale_color_viridis_c(option = "plasma", direction = -1, trans = "log10",
                                           name = "NLL", limits = fitness_limits, guide = "none")
           else
             ggplot2::scale_color_viridis_c(trans = "log10", name = "NLL", limits = fitness_limits, guide = "none")) +
          ggplot2::scale_x_continuous(expand = c(0, 0)) +
          ggplot2::scale_y_continuous(expand = c(0, 0)) +
          ggplot2::coord_cartesian(ylim = c(y_min, y_max_plot)) +
          ggplot2::labs(title = gn, x = "Year", y = info$y_lab) +
          ggplot2::theme_bw(base_size = 10) +
          ggplot2::theme(
            aspect.ratio = 1,
            panel.grid.major = ggplot2::element_line(colour = scales::alpha("grey25", 0.35),
                                                     linetype = "dashed", linewidth = 0.2),
            panel.grid.minor = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(size = 11, face = "bold", hjust = 0.5,
                                               margin = ggplot2::margin(0, 0, 4, 0)),
            axis.title.x = if(show_x_axis) ggplot2::element_text(size = 11) else ggplot2::element_blank(),
            axis.text.x  = if(show_x_axis) ggplot2::element_text(size = 9)  else ggplot2::element_blank(),
            axis.ticks.x = if(show_x_axis) ggplot2::element_line(linewidth = 0.5) else ggplot2::element_blank(),
            axis.title.y = if(show_y_axis) ggplot2::element_text(size = 11) else ggplot2::element_blank(),
            axis.text.y  = if(show_y_axis) ggplot2::element_text(size = 9)  else ggplot2::element_blank(),
            axis.ticks.y = if(show_y_axis) ggplot2::element_line(linewidth = 0.5) else ggplot2::element_blank(),
            legend.position = "none",
            plot.margin = ggplot2::margin(4, 2, 4, 2)
          )
        
        panel_list[[gn]] <- pp
      }
      
      if(length(panel_list) == 0){
        message("  [skip] ", var, " '", sp, "': no panels to plot.")
        next
      }
      
      # dedicated NLL colorbar, built separately and placed beside the panel grid --
      # same mechanism as plot_ga_ensemble_faceted()
      nll_legend_source <- ggplot2::ggplot(var_valid, ggplot2::aes(x = 1, y = fitness, colour = fitness)) +
        ggplot2::geom_point(alpha = 0) +
        (if(palette == "plasma")
          ggplot2::scale_colour_viridis_c(option = "plasma", direction = -1, trans = "log10",
                                          limits = fitness_limits, name = "NLL",
                                          guide = ggplot2::guide_colorbar(
                                            title.position = "top", title.hjust = 0,
                                            barheight = grid::unit(0.832 * height, "in"),
                                            barwidth = grid::unit(0.55, "cm"),
                                            frame.colour = "black", frame.linewidth = 0.5,
                                            ticks = TRUE, ticks.colour = "black"))
         else
           ggplot2::scale_colour_viridis_c(trans = "log10", limits = fitness_limits, name = "NLL",
                                           guide = ggplot2::guide_colorbar(
                                             title.position = "top", title.hjust = 0,
                                             barheight = grid::unit(0.832 * height, "in"),
                                             barwidth = grid::unit(0.55, "cm"),
                                             frame.colour = "black", frame.linewidth = 0.5,
                                             ticks = TRUE, ticks.colour = "black"))) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "right", legend.justification = "left",
                       legend.title = ggplot2::element_text(size = 11, hjust = 0),
                       legend.text = ggplot2::element_text(size = 9),
                       legend.margin = ggplot2::margin(0, 0, 0, 0),
                       plot.margin = ggplot2::margin(0, 0, 0, 0))
      
      nll_grob <- ggplot2::ggplotGrob(nll_legend_source)
      guide_index <- which(vapply(nll_grob$grobs, function(x)
        inherits(x, "gtable") && x$name == "guide-box", logical(1)))
      
      n_rows_actual <- ceiling(length(panel_list) / facet_ncol)
      panel_grid <- patchwork::wrap_plots(panel_list, ncol = facet_ncol, nrow = n_rows_actual)
      
      if(length(guide_index) > 0){
        nll_legend_grob <- nll_grob$grobs[[guide_index[1]]]
        nll_legend_plot <- patchwork::wrap_elements(full = nll_legend_grob, clip = FALSE)
        p <- (panel_grid | nll_legend_plot) + patchwork::plot_layout(widths = c(1, 0.075))
      } else {
        message("  [note] could not extract NLL colorbar grob -- combined figure will have no NLL legend.")
        p <- panel_grid
      }
      
      png_file <- file.path(plots_dir, paste0(var, "_gen_best_", safe_name(sp), ".png"))
      ggplot2::ggsave(png_file, plot = p, width = width, height = height, dpi = dpi, units = "in")
      
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        variable               = var,
        species                = sp,
        n_candidates_plotted   = nrow(var_valid),
        n_generations_plotted  = n_labels,
        generations_plotted    = paste(sort(unique(var_valid$generation)), collapse = ","),
        best_fitness           = overall_best$fitness,
        best_run_folder        = overall_best$run_folder,
        best_generation        = overall_best$generation,
        png_file               = png_file,
        stringsAsFactors       = FALSE
      )
    }
  }
  
  message("Wrote ", length(summary_rows), " PNG(s) to ", normalizePath(plots_dir, mustWork = FALSE))
  invisible(do.call(rbind, summary_rows))
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# RRMSE per predicted time series, vs. candidate fitness
#
# Does this make sense, and does lower RRMSE mean lower (better) fitness/LL? Generally
# yes, but not guaranteed for any single series: `fitness` is a WEIGHTED SUM across
# every fitted observed series (many biomass/catch/F series across many stanzas), so a
# candidate can have good overall fitness while fitting one PARTICULAR series only
# middlingly (compensated by fitting others very well), especially for lower-weight
# series. Plotting per-series RRMSE against overall fitness is a genuinely useful
# diagnostic for exactly this: it shows whether the objective's aggregate improvement
# is broadly shared across series, or concentrated in a few high-weight ones while
# others don't improve much as fitness gets better. Expect a real but noisy negative
# correlation (lower fitness = lower RRMSE), not a clean 1:1 relationship.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @title Compute RRMSE per (candidate, observed series), for correlation with fitness.
#' @description For every candidate and every observed series matching `vars` x
#'   `species_patterns` (same matching logic as plot_ga_ensemble()), computes RRMSE =
#'   RMSE / mean(|observed|) over the years common to both series. Non-absolute
#'   (index-type) observed series are rescaled to the candidate's own predicted scale by
#'   mean-matching over the common period first (same convention as the obs overlay in
#'   plot_ga_ensemble()), so RRMSE is computed on a comparable scale for every candidate
#'   -- NOT rescaled against the single best-fit candidate, since that would bias the
#'   apparent error of every other candidate.
#' @param obs Output of read_ewe_timeseries().
#' @param ga_runs Output of resolve_run_dirs().
#' @param vars Character vector of variable names (keys of VAR_TYPE_MAP).
#' @param species_patterns Character vector of case-insensitive substrings, e.g.
#'   c("gag", "red grouper").
#' @param min_common_years Minimum number of overlapping (observed, predicted) years
#'   required to compute RRMSE for a (candidate, series) pair. Default 3 (a 1-2 point
#'   RRMSE is not meaningful).
#' @param max_candidates Safety cap; a random subsample is used if more candidates exist
#'   with archived output (best-fit candidate always kept). Default 1000.
#' @return data.frame(run_folder, fitness, variable, group_name, series, n_years, rrmse)
#'   -- one row per (candidate, series) pair with enough overlapping data.
#' @export
compute_rrmse_by_candidate <- function(obs, ga_runs,
                                       vars              = c("Biomass", "Catch", "F"),
                                       species_patterns  = c("gag", "red grouper"),
                                       min_common_years  = 3,
                                       max_candidates    = 1000){
  valid <- ga_runs[ga_runs$exists, , drop = FALSE]
  if(nrow(valid) == 0)
    stop("No candidate folders found on disk -- check gen_dir / resolve_run_dirs() output.")
  if(nrow(valid) > max_candidates){
    best_idx_pre <- which.min(valid$fitness)
    keep <- unique(c(best_idx_pre, sample(seq_len(nrow(valid)), max_candidates - 1)))
    valid <- valid[keep, , drop = FALSE]
    message("compute_rrmse_by_candidate(): subsampling ", max_candidates,
            " candidate(s) (best-fit always kept).")
  }
  
  caches <- setNames(lapply(seq_len(nrow(valid)), function(i) new.env(parent = emptyenv())),
                     valid$run_folder)
  group_names_lookup <- function(pr, pool_codes) pr$group_names[pool_codes + 1L]
  
  ref <- NULL
  for(i in seq_len(nrow(valid))){
    ref <- get_region_output(0L, "Biomass", valid$run_dir[i], caches[[valid$run_folder[i]]])
    if(!is.null(ref)) break
  }
  if(is.null(ref)) stop("Could not read a domain-average Biomass file from ANY candidate folder.")
  
  target_groups <- find_pool_codes_by_name(ref$group_names, species_patterns)
  if(nrow(target_groups) == 0)
    stop("No group names matched species_patterns = ", paste(species_patterns, collapse = ", "))
  
  rows <- list()
  for(var in vars){
    info <- VAR_TYPE_MAP[[var]]
    for(gi in seq_len(nrow(target_groups))){
      pool_code  <- target_groups$pool_code[gi]
      group_name <- target_groups$group_name[gi]
      
      th <- obs$ts.head
      obs_rows <- which(th$Type %in% info$types & th$Poolcode1 == as.character(pool_code) &
                          th$Region == "0")
      if(length(obs_rows) == 0) next
      
      for(i in seq_len(nrow(valid))){
        pr <- build_predicted_vector_generic(pool_code, 0L, var, valid$run_dir[i],
                                             caches[[valid$run_folder[i]]], group_names_lookup)
        if(is.null(pr$vec) || all(is.na(pr$vec))) next
        pred_df_i <- data.frame(year = pr$years, pred = pr$vec)
        
        for(r in obs_rows){
          ov <- obs$ts[[th$Title[r]]]; oy <- obs$ts$year
          keep <- !is.na(ov)
          if(!any(keep)) next
          obs_df_r <- data.frame(year = oy[keep], obs = ov[keep])
          
          m <- merge(obs_df_r, pred_df_i, by = "year")
          m <- m[is.finite(m$obs) & is.finite(m$pred), , drop = FALSE]
          if(nrow(m) < min_common_years) next
          
          q <- 1
          if(!th$Absolute[r]){
            mo <- mean(m$obs, na.rm = TRUE); mp <- mean(m$pred, na.rm = TRUE)
            if(is.finite(mo) && is.finite(mp) && mp > 0) q <- mo / mp
          }
          pred_scaled <- m$pred * q
          resid <- m$obs - pred_scaled
          rmse  <- sqrt(mean(resid^2, na.rm = TRUE))
          denom <- mean(abs(m$obs), na.rm = TRUE)
          if(!is.finite(denom) || denom <= 0) next
          rrmse <- rmse / denom
          
          rows[[length(rows) + 1]] <- data.frame(
            run_folder = valid$run_folder[i], fitness = valid$fitness[i],
            variable = var, group_name = group_name, series = th$Title[r],
            n_years = nrow(m), rrmse = rrmse, stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  
  out <- do.call(rbind, rows)
  if(is.null(out) || nrow(out) == 0){
    warning("compute_rrmse_by_candidate(): no (candidate, series) pair had enough overlapping data.")
    return(invisible(NULL))
  }
  message("compute_rrmse_by_candidate(): ", nrow(out), " (candidate, series) RRMSE value(s) computed ",
          "across ", length(unique(out$run_folder)), " candidate(s) and ",
          length(unique(out$series)), " observed series.")
  out
}


#' @title Plot RRMSE vs. fitness, colored by fitness, with per-facet correlation reported.
#' @description Visualizes whether per-series fit quality (RRMSE) tracks overall
#'   candidate fitness -- see the section note above compute_rrmse_by_candidate() for
#'   what to expect (a real but noisy negative relationship, not a clean 1:1 mapping).
#'   Prints the Spearman correlation between RRMSE and fitness for each facet, since
#'   that's the actual quantitative answer to "does lower RRMSE mean lower NLL" -- the
#'   plot alone can be ambiguous to eyeball when points are dense.
#' @param rrmse_df Output of compute_rrmse_by_candidate().
#' @param facet_by One of "series" (default), "group_name", or "variable" -- what to
#'   facet panels by.
#' @param facet_ncol Number of panel columns. Default 4.
#' @param plots_dir Output folder for the PNG. Default "plots".
#' @param png_file Output filename. Default "rrmse_vs_fitness.png".
#' @param width,height,dpi ggsave() figure dimensions (inches) and resolution (dpi).
#' @return Invisibly, a data.frame with one row per facet: facet, n, spearman_rho,
#'   spearman_p.
#' @export
plot_rrmse_vs_fitness <- function(rrmse_df,
                                  facet_by   = c("series", "group_name", "variable"),
                                  facet_ncol = 4,
                                  plots_dir  = "plots",
                                  png_file   = "rrmse_vs_fitness.png",
                                  width = 12, height = 8, dpi = 150){
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
  facet_by <- match.arg(facet_by)
  
  facet_vals <- unique(rrmse_df[[facet_by]])
  cor_rows <- list()
  for(fv in facet_vals){
    sub <- rrmse_df[rrmse_df[[facet_by]] == fv, , drop = FALSE]
    if(nrow(sub) < 4){
      cor_rows[[length(cor_rows) + 1]] <- data.frame(facet = fv, n = nrow(sub),
                                                     spearman_rho = NA_real_, spearman_p = NA_real_,
                                                     stringsAsFactors = FALSE)
      next
    }
    ct <- suppressWarnings(stats::cor.test(sub$fitness, sub$rrmse, method = "spearman"))
    cor_rows[[length(cor_rows) + 1]] <- data.frame(facet = fv, n = nrow(sub),
                                                   spearman_rho = unname(ct$estimate),
                                                   spearman_p = ct$p.value, stringsAsFactors = FALSE)
  }
  cor_df <- do.call(rbind, cor_rows)
  cor_df <- cor_df[order(cor_df$spearman_rho), ]
  message("plot_rrmse_vs_fitness(): Spearman correlation (fitness vs. RRMSE) per ", facet_by, ":")
  for(i in seq_len(nrow(cor_df)))
    message(sprintf("  %-30s n=%3d  rho=%6.3f  p=%.4f%s", cor_df$facet[i], cor_df$n[i],
                    cor_df$spearman_rho[i], cor_df$spearman_p[i],
                    if(is.na(cor_df$spearman_rho[i])) "  (too few points)"
                    else if(cor_df$spearman_rho[i] > 0) "  <- WARNING: positive (worse fitness, worse RRMSE as expected) is normal; NEGATIVE would be unexpected"
                    else ""))
  # NOTE: rho > 0 here is the EXPECTED direction (both "fitness" and "rrmse" are
  # cost-like: lower = better for fitness, and RRMSE is always >= 0 with lower = better
  # fit) -- so a POSITIVE correlation (worse fitness co-occurs with higher RRMSE) is
  # the sign that "lower RRMSE corresponds to lower/better fitness" holds. A near-zero
  # or negative rho for a given series means that series' fit doesn't track overall
  # fitness well -- worth a closer look at that series' weight and data quality.
  
  p <- ggplot2::ggplot(rrmse_df, ggplot2::aes(x = fitness, y = rrmse, color = fitness)) +
    ggplot2::geom_point(alpha = 0.6, size = 1.3) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_color_viridis_c(trans = "log10", name = "NLL") +
    ggplot2::facet_wrap(stats::as.formula(paste0("~", facet_by)), scales = "free_y", ncol = facet_ncol) +
    ggplot2::labs(x = "Fitness (NLL, log scale)", y = "RRMSE") +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 7, face = "bold"), aspect.ratio = 1)
  
  if(!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(plots_dir, png_file)
  ggplot2::ggsave(out_path, plot = p, width = width, height = height, dpi = dpi, units = "in")
  message("Wrote ", out_path)
  
  invisible(cor_df)
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PART 4: time series evaluation -- observed vs. predicted (uses PART 1 + PART 2 above)
#
# O - ts_mice_v5_discards_ecospace_regions_sedar105.csv
# P - sp03_5min_phase3_init / gen30_small candidate folders
#
# (Runnable example code for this section lives in the single combined `if(FALSE){...}`
# block at the very end of this file, alongside PART 5's example code.)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PART 5: spatial maps (mean / variability) + red-tide M0 mortality time series
#
# Format confirmed from actual uploaded files:
#   "EcospaceMap<Var>-<group_token>.csv" -- e.g. EcospaceMapBiomass-red_grouper_0.csv.
#   Header block (MapRows, MapCols, MapCellSize, MapTopLeftLat, MapTopLeftLon,
#   StartYear, ...) ends at a literal '"<HEADER end/>"' line, followed by "Variable,..."
#   and a group-name line (labeled "Contaminant Concentrations" in EwE's own export --
#   not an actual contaminant field, just how EwE names that row). Then repeating blocks
#   of "Step,<n>" / "Year,<fractional elapsed years>" / MapRows data rows of MapCols
#   comma-separated values (-9999 = no data), one block per CALENDAR YEAR (Step
#   increments by 12 between blocks -- confirmed against StartYear=1985..2024 in the
#   sample file). calendar_year = StartYear + floor(Year).
#
# STILL BLOCKED: true point-vs-cell O-vs-P maps and residual maps (Figure 4/5 as
# originally scoped) need observed data at point/haul resolution with coordinates; the
# current ts_*.csv is Region-aggregated only. What IS buildable now, and is what this
# section does, is P-only spatial summaries -- mean and temporal variability (SD/CV) per
# cell -- which is exactly "where is biomass concentrated, and where is it most
# variable" per your request. If/when point-level observed coordinates become
# available, the O-vs-P and residual map versions are a straightforward extension of
# read_ecospace_map() below (same grid, just overlay observed points and difference).
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @title Read one "EcospaceMap<Var>-<group>.csv" spatial time series export.
#' @description Parses the metadata header (grid dimensions, cell size, top-left corner,
#'   start year), then every Step/Year block into a long-format data.frame. -9999 is
#'   converted to NA. All data rows across all year-blocks are parsed in a single
#'   data.table::fread() call rather than one strsplit()/as.numeric() call per row, which
#'   matters a lot when this is called thousands of times across an ensemble of
#'   candidate folders.
#' @param path Path to an EcospaceMap<Var>-<group_token>.csv file.
#' @param verbose Logical, default FALSE. TRUE prints a one-line summary after reading
#'   (group/variable/grid size/years) -- useful when reading a single file by hand, but
#'   left off by default since bulk ensemble functions call this thousands of times and
#'   per-file messages there just look like console spam (and, since the message only
#'   shows the filename not the folder, can look identical across different candidates
#'   reading the same-named file -- see compute_ensemble_map_stats() for the progress
#'   reporting used during bulk reads instead).
#' @return list(meta = list(rows, cols, cellsize, top_lat, top_lon, start_year,
#'   variable, group), data = data.frame(year, row, col, lat, lon, value) -- one row per
#'   active-or-inactive cell per year; inactive/no-data cells have value = NA).


#' @keywords internal
#' @noRd
# Generates every (row, col, lat, lon) cell in the confirmed model domain -- used as a
# grey base/land layer in plot_ecospace_map() and plot_spatial_residuals() so cells not
# covered by the actual data (land, or simply outside a raster's footprint) read as
# background instead of blank white space. Uses the SAME fixed reference constants as
# read_ecospace_map()/read_esri_ascii() throughout this script, confirmed for this
# model's domain -- not derived from any particular file, so this works even when the
# caller doesn't have a land/water shapefile handy.
.full_domain_grid <- function(top_lat = 30.5, top_lon = -87.5, cellsize = 0.08333334,
                              nrows = 66, ncols = 78){
  row_idx <- rep(seq_len(nrows), times = ncols)
  col_idx <- rep(seq_len(ncols), each = nrows)
  data.frame(row = row_idx, col = col_idx,
             lat = top_lat - (row_idx - 1) * cellsize,
             lon = top_lon + (col_idx - 1) * cellsize)
}


#' @keywords internal
#' @noRd
# Extracts ONLY true land cells from a region/land mask (e.g. read_esri_ascii() on
# combined_regions_5min.asc), for use as a grey background layer that's actually
# accurate -- unlike .full_domain_grid() above (which draws the ENTIRE grid grey,
# lumping true land together with water cells that simply have no data for whatever
# specific group/year is being plotted). Land cells are identified as NODATA in the
# source file (read_esri_ascii() converts NODATA_value, -9999 in this project's masks,
# to NA) -- distinct from water cells coded 0 ("unsampled water"), which are real,
# non-NA values and correctly stay OUT of this land-only set.
.land_cells_from_mask <- function(land_mask){
  land_mask[is.na(land_mask$value), c("row", "col", "lat", "lon"), drop = FALSE]
}


#' @title Read a depth raster and extract depth at every cell in the model grid.
#' @description Reads `depth_file` via terra::rast() and extracts the depth value at
#'   every (lon, lat) cell of the model's 66x78 grid via nearest-neighbor lookup. Used
#'   for the shallow/deep split in the paired ensemble maps and faceted residual maps
#'   (cells deeper than `depth_threshold` are shown plain white, not colored by
#'   value/CV/residual, since deep-water predictions in this model are typically much
#'   less reliable/meaningful than shelf predictions).
#' @param depth_file Path to the depth .asc raster. Must have exactly 66 rows x 78
#'   columns (this model's confirmed grid) -- errors with the actual dimensions found
#'   if not, rather than silently misaligning.
#' @param top_lat,top_lon,cellsize,nrows,ncols Grid reference constants, matching
#'   .full_domain_grid()'s defaults (this model's confirmed domain) -- override only if
#'   you have a genuinely different grid.
#' @return data.frame(row, col, lat, lon, depth) -- one row per grid cell.
#' @export
read_depth_grid <- function(depth_file, top_lat = 30.5, top_lon = -87.5, cellsize = 0.08333334,
                            nrows = 66, ncols = 78){
  if(!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required for read_depth_grid(). Install it with install.packages('terra').")
  if(!file.exists(depth_file))
    stop("read_depth_grid(): file not found: ", depth_file)
  
  depth_rast <- terra::rast(depth_file)
  if(terra::nrow(depth_rast) != nrows || terra::ncol(depth_rast) != ncols)
    stop("read_depth_grid(): depth raster dimensions (", terra::nrow(depth_rast), " x ",
         terra::ncol(depth_rast), ") don't match the expected grid (", nrows, " x ", ncols,
         ") -- check depth_file, or pass matching nrows/ncols if this is genuinely a ",
         "different grid.")
  
  grid <- .full_domain_grid(top_lat, top_lon, cellsize, nrows, ncols)
  grid_pts <- terra::vect(grid, geom = c("lon", "lat"), crs = "EPSG:4326")
  depth_values <- terra::extract(depth_rast, grid_pts, method = "near")
  grid$depth <- depth_values[, 2]
  grid
}


#' @title Read a generic ESRI ASCII grid (.asc) file.
#' @description Parses the standard 6-line header (NCOLS/NROWS/XLLCORNER/YLLCORNER/
#'   CELLSIZE/NODATA_value) plus the data matrix. Uses the SAME row/col -> lat/lon
#'   convention as read_ecospace_map() (fixed reference constants MapTopLeftLat=30.5,
#'   MapTopLeftLon=-87.5, CellSize~0.0833334, confirmed for this model's domain), NOT the
#'   file's own XLLCORNER/YLLCORNER -- this is intentional: cross-checked empirically
#'   against a real EcospaceMapBiomass export (every biomass-active cell is confirmed
#'   non-land at the SAME row/col index, zero offset, zero violations), so using the
#'   same fixed reference guarantees pixel-perfect overlay with every other map in this
#'   script rather than trusting two independently-labeled coordinate systems to agree
#'   to the sub-cell level. `NODATA_value` cells become `NA`.
#' @param path Path to the .asc file.
#' @param top_lat,top_lon,cellsize Reference constants for the row/col -> lat/lon
#'   mapping. Defaults match this model's confirmed domain (see above) -- override only
#'   if you're using a genuinely different grid.
#' @return data.frame(row, col, lat, lon, value) -- one row per cell, `value` is
#'   whatever the raster encodes (region ID, survey count, etc.), NA for NODATA cells.
read_esri_ascii <- function(path, top_lat = 30.5, top_lon = -87.5, cellsize = 0.08333334){
  raw <- readLines(path, warn = FALSE)
  hdr <- toupper(trimws(raw[1:6]))
  gv <- function(key){
    ln <- grep(paste0("^", key), hdr, value = TRUE)
    if(length(ln) == 0) stop("read_esri_ascii(): missing header field '", key, "' in ", path)
    as.numeric(trimws(sub(paste0("^", key), "", ln[1])))
  }
  ncols <- as.integer(gv("NCOLS"))
  nrows <- as.integer(gv("NROWS"))
  nodata <- gv("NODATA_VALUE")
  
  data_lines <- raw[7:(6 + nrows)]
  mat <- do.call(rbind, lapply(data_lines, function(l) as.numeric(strsplit(trimws(l), "\\s+")[[1]])))
  if(!identical(dim(mat), c(nrows, ncols)))
    stop("read_esri_ascii(): expected ", nrows, " x ", ncols, " data matrix in ", path,
         " but got ", paste(dim(mat), collapse = " x "))
  
  row_idx <- rep(seq_len(nrows), times = ncols)
  col_idx <- rep(seq_len(ncols), each = nrows)
  lat <- top_lat - (row_idx - 1) * cellsize
  lon <- top_lon + (col_idx - 1) * cellsize
  vals <- as.vector(mat)  # column-major flatten == row-fastest, matches row_idx/col_idx
  vals[vals == nodata] <- NA_real_
  
  message("read_esri_ascii(): ", basename(path), " -- ", nrows, "x", ncols, " grid, ",
          sum(!is.na(vals)), " non-NODATA cell(s).")
  data.frame(row = row_idx, col = col_idx, lat = lat, lon = lon, value = vals)
}


#' @title Compute spatial residuals: observed survey raster vs. ensemble predicted mean.
#' @description Joins an observed raster (e.g. a maxN survey count from
#'   read_esri_ascii(), like "GFISHER_maxn_mod21_gag-1_5min_66x78.asc") against the
#'   ensemble's predicted mean biomass map (from compute_ensemble_map_stats(), same
#'   confirmed grid) by row/col, restricted to cells where the observed raster actually
#'   has data (survey rasters are typically NA/NODATA outside the sampled footprint --
#'   comparing there would just be comparing "no data" against a real prediction, not a
#'   real residual). Since observed counts (maxN) and predicted biomass (t/km^2) are on
#'   different scales, observed is q-rescaled to the predicted scale first (same
#'   mean-matching convention used for non-absolute time series elsewhere in this
#'   script) before computing the residual.
#' @param obs_raster Output of read_esri_ascii() for one observed FG raster.
#' @param pred_mean_df A data.frame(row, col, lat, lon, value) of predicted mean biomass
#'   -- e.g. `compute_ensemble_map_stats(run_folders, "Biomass", tok)$agg` reshaped via
#'   `as.data.frame(agg[, .(row, col, lat, lon, value = mean_val)])`, or the `mean_df`
#'   object built inside plot_ecospace_ensemble_maps_paired()/faceted().
#' @return data.frame(row, col, lat, lon, obs, obs_scaled, pred, residual,
#'   residual_std) -- one row per cell where BOTH observed and predicted have data.
#'   `residual = obs_scaled - pred`; `residual_std` is that divided by the residual SD
#'   (a simple standardization, not a formal statistical residual test).
compute_spatial_residuals <- function(obs_raster, pred_mean_df){
  m <- merge(obs_raster[, c("row", "col", "lat", "lon", "value")],
             pred_mean_df[, c("row", "col", "value")],
             by = c("row", "col"), suffixes = c("_obs", "_pred"))
  m <- m[!is.na(m$value_obs) & !is.na(m$value_pred), , drop = FALSE]
  if(nrow(m) == 0)
    stop("compute_spatial_residuals(): no cells with both observed and predicted data -- ",
         "check that obs_raster and pred_mean_df share the same grid (see read_esri_ascii() docs).")
  
  mo <- mean(m$value_obs, na.rm = TRUE)
  mp <- mean(m$value_pred, na.rm = TRUE)
  q <- if(is.finite(mo) && is.finite(mp) && mo > 0) mp / mo else 1
  m$obs_scaled <- m$value_obs * q
  m$residual <- m$obs_scaled - m$value_pred
  sd_r <- stats::sd(m$residual, na.rm = TRUE)
  m$residual_std <- if(is.finite(sd_r) && sd_r > 0) m$residual / sd_r else NA_real_
  
  message("compute_spatial_residuals(): ", nrow(m), " cell(s) with both observed and ",
          "predicted data (q-rescale factor = ", signif(q, 4), ").")
  
  data.frame(row = m$row, col = m$col, lat = m$lat, lon = m$lon,
             obs = m$value_obs, obs_scaled = m$obs_scaled, pred = m$value_pred,
             residual = m$residual, residual_std = m$residual_std)
}


#' @title Plot a spatial residual map (observed survey vs. predicted biomass).
#' @description Diverging color scale centered at 0 -- blue/red (or your chosen
#'   palette) shows under/over-prediction by cell. Uses `residual_std` by default
#'   (standardized, comparable across FGs with different scales); pass `value_col =
#'   "residual"` for the raw (rescaled-observed-units) residual instead.
#' @param residual_df Output of compute_spatial_residuals().
#' @param title Plot title. Default "".
#' @param value_col Which column to map to color: "residual_std" (default) or
#'   "residual".
#' @param show_land Logical, default TRUE. Draws a grey background layer for cells not
#'   covered by `residual_df`.
#' @param land_mask Optional data.frame from read_esri_ascii() identifying TRUE land
#'   cells -- see plot_ecospace_map()'s own docs for why this matters (only real land
#'   shaded grey, not land lumped together with water that just lacks data for this
#'   specific raster). NULL (default) falls back to the whole-domain-grey behavior.
#' @param depth_grid Optional data.frame from read_depth_grid() (row, col, lat, lon,
#'   depth). When supplied, cells deeper than `depth_threshold` are shown plain white
#'   instead of colored by residual -- deep-water predictions are typically far less
#'   reliable than shelf predictions in this model. NULL (default) colors every cell.
#' @param depth_threshold Depth (m) cutoff for the shallow/deep split. Default 500.
#' @param show_x_axis,show_y_axis Logical, default TRUE. FALSE hides that axis's
#'   title/text/ticks entirely -- for shared-axis faceted layouts.
#' @param lon_breaks,lat_breaks Optional explicit numeric vectors for dashed gridlines
#'   at fixed breaks, replacing theme_bw()'s default grid. NULL (default) = ggplot's
#'   own automatic breaks, no explicit gridlines.
#' @param inside_legend Logical, default FALSE. TRUE moves the fill legend inside the
#'   panel (bottom-left by default), framed, with the title above the bar.
#' @param legend_position Only used when `inside_legend = TRUE`. c(x, y) in [0,1],
#'   default c(0.02, 0.02) (bottom-left).
#' @param barheight,barwidth Only used when `inside_legend = TRUE`. Colorbar dimensions
#'   in cm. Defaults 1.5/0.35.
#' @param base_size Base font size (pt) -- controls axis/title text size relative to
#'   the plot. Default 16 (larger than ggplot2's own default of 11, which read too
#'   small for these maps in practice).
#' @param fixed_aspect Logical, default TRUE. TRUE sets aspect.ratio=1, forcing a
#'   square panel. FALSE skips this, letting coord_quickmap() (already correct for
#'   geographic aspect on its own) determine the panel's shape and letting it fill
#'   whatever cell width is allocated to it -- needed when combining panels via
#'   patchwork and controlling spacing with plot.margin: a fixed aspect.ratio forces
#'   the panel to a specific size independent of its cell, and ggplot2 centers that
#'   fixed-size panel within the cell, which silently absorbs any plot.margin change.
#'   See plot_ecospace_map()'s equivalent parameter for the full explanation.
#' @return A ggplot object.
#' @export
plot_spatial_residuals <- function(residual_df, title = "", value_col = c("residual_std", "residual"),
                                   show_land = TRUE, land_mask = NULL,
                                   depth_grid = NULL, depth_threshold = 500,
                                   show_x_axis = TRUE, show_y_axis = TRUE,
                                   lon_breaks = NULL, lat_breaks = NULL,
                                   inside_legend = FALSE, legend_position = c(0.02, 0.02),
                                   barheight = 2.8, barwidth = 0.45, base_size = 11,
                                   title_inside = TRUE, fixed_aspect = TRUE){
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
  value_col <- match.arg(value_col)
  residual_df$plot_value <- residual_df[[value_col]]
  
  data_deep <- NULL
  if(!is.null(depth_grid)){
    if(!all(c("row", "col") %in% names(residual_df)))
      stop("plot_spatial_residuals(): depth_grid was supplied but residual_df has no ",
           "row/col columns to merge on.")
    merged <- merge(residual_df, depth_grid[, c("row", "col", "depth")], by = c("row", "col"), all.x = TRUE)
    data_deep <- merged[!is.na(merged$depth) & merged$depth > depth_threshold, , drop = FALSE]
    residual_df <- merged[is.na(merged$depth) | merged$depth <= depth_threshold, , drop = FALSE]
  }
  lims <- max(abs(residual_df$plot_value), na.rm = TRUE)
  
  p <- ggplot2::ggplot()
  land_df <- NULL
  if(show_land){
    land_df <- if(!is.null(land_mask)) .land_cells_from_mask(land_mask) else .full_domain_grid()
    p <- p + ggplot2::geom_raster(data = land_df, ggplot2::aes(x = lon, y = lat), fill = "grey65")
  }
  p <- p + ggplot2::geom_raster(data = residual_df, ggplot2::aes(x = lon, y = lat, fill = plot_value))
  if(!is.null(data_deep) && nrow(data_deep) > 0)
    p <- p + ggplot2::geom_raster(data = data_deep, ggplot2::aes(x = lon, y = lat), fill = "white")
  
  if(!is.null(lon_breaks))
    p <- p + ggplot2::geom_vline(xintercept = lon_breaks, colour = scales::alpha("grey25", 0.40),
                                 linetype = "dashed", linewidth = 0.35)
  if(!is.null(lat_breaks))
    p <- p + ggplot2::geom_hline(yintercept = lat_breaks, colour = scales::alpha("grey25", 0.40),
                                 linetype = "dashed", linewidth = 0.35)
  
  fill_guide <- if(inside_legend){
    ggplot2::guide_colorbar(position = "inside", frame.colour = "black", frame.linewidth = 0.6,
                            ticks.colour = "black", title.position = "top",
                            barheight = grid::unit(barheight, "cm"), barwidth = grid::unit(barwidth, "cm"))
  } else {
    ggplot2::guide_colorbar()
  }
  
  p <- p +
    ggplot2::scale_fill_gradient2(name = if(value_col == "residual_std") "Std.\nresidual" else "Residual",
                                  low = "#B2182B", mid = "white", high = "#2166AC",
                                  midpoint = 0, limits = c(-lims, lims), na.value = "grey90",
                                  oob = scales::squish, guide = fill_guide) +
    # expand=c(0,0) on both axes -- removes ggplot's default 5% padding around the
    # data, so the map fills its panel completely instead of leaving blank margin
    # inside the plot frame (same fix already applied to plot_region_map()).
    ggplot2::scale_x_continuous(breaks = lon_breaks, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = lat_breaks, expand = c(0, 0)) +
    ggplot2::coord_quickmap() +
    ggplot2::labs(title = if(title_inside) NULL else title, x = "Longitude", y = "Latitude") +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      # same recalibrated constants as plot_ecospace_map() -- see that function's
      # comment for why (previously mismatched defaults, 11 vs 16, made residuals and
      # paired maps look genuinely different despite similar-looking code)
      plot.title = ggplot2::element_text(size = base_size * 0.909, face = "bold", hjust = 0.5),
      aspect.ratio = if(fixed_aspect) 1 else NULL,
      axis.title.x = if(show_x_axis) ggplot2::element_text() else ggplot2::element_blank(),
      axis.text.x  = if(show_x_axis) ggplot2::element_text() else ggplot2::element_blank(),
      axis.ticks.x = if(show_x_axis) ggplot2::element_line() else ggplot2::element_blank(),
      axis.title.y = if(show_y_axis) ggplot2::element_text() else ggplot2::element_blank(),
      axis.text.y  = if(show_y_axis) ggplot2::element_text() else ggplot2::element_blank(),
      axis.ticks.y = if(show_y_axis) ggplot2::element_line() else ggplot2::element_blank()
    )
  if(inside_legend)
    p <- p + ggplot2::theme(
      legend.position.inside = legend_position,
      legend.justification.inside = if(legend_position[2] < 0.5) c(0, 0) else c(0, 1),
      legend.title = ggplot2::element_text(hjust = 0, size = base_size * 0.818),
      legend.background = ggplot2::element_rect(fill = scales::alpha("white", 0.5), colour = NA),
      legend.margin = ggplot2::margin(1, 1, 1, 1)
    )
  if(title_inside && nzchar(title)){
    lon_all <- c(residual_df$lon, if(!is.null(land_df)) land_df$lon else NULL)
    lat_all <- c(residual_df$lat, if(!is.null(land_df)) land_df$lat else NULL)
    x_center <- mean(range(lon_all, na.rm = TRUE))
    y_top <- max(lat_all, na.rm = TRUE)
    p <- p + ggplot2::annotate("text", x = x_center, y = y_top, label = title,
                               hjust = 0.5, vjust = 1, size = base_size * 0.364, fontface = "bold")
  }
  p
}


#' @title Batch spatial residuals: every observed survey raster in a folder, vs. predicted.
#' @description Scans `raster_dir` for observed FG rasters (e.g. maxN survey files like
#'   "GFISHER_maxn_mod21_gag-1_5min_66x78.asc"), extracts a species+stanza guess from
#'   each filename, matches it against the REAL EcospaceMap token via
#'   detect_map_group_tokens(), computes the predicted mean biomass map for the
#'   best-fit candidate, computes the residual (see compute_spatial_residuals()), and
#'   SAVES one PNG per raster -- rather than returning ggplot objects you'd have to
#'   save yourself one at a time.
#' @param raster_dir Directory containing the observed raster files.
#' @param ga_runs Output of resolve_run_dirs().
#' @param species_patterns Character vector of case-insensitive substrings identifying
#'   which species to include, e.g. c("gag", "red grouper"). A raster whose filename
#'   doesn't match ANY of these is skipped (reported in the summary, not silently).
#' @param ens_mode One of "quantile" (default, best X% by fitness -- see
#'   `keep_fitness_quantile`), "topN" (exactly the N best by rank -- see `top_n`), or
#'   "aic" (Akaike-selected subset -- see `target_ess`). For "aic" mode, the predicted
#'   mean map is a true AIC-weighted average: each candidate's own per-cell mean
#'   (collapsed across that candidate's own years first) is weighted by its AIC weight
#'   via compute_ensemble_map_stats()'s `weights` argument, not pooled unweighted
#'   across candidates. See select_ensemble_candidates() for the shared selection
#'   logic used by every ensemble function in this script.
#' @param keep_fitness_quantile,top_n,target_ess Passed to select_ensemble_candidates().
#' @param raster_pattern Regex for which files in `raster_dir` count as rasters to
#'   process. Default "\\\\.asc$" (every .asc file).
#' @param manual_tokens Optional named character vector, names = raster file BASENAME
#'   (as returned by basename(), including extension), values = the EcospaceMap token to
#'   use for that specific file -- overrides auto-detection for files where the filename
#'   parsing guesses wrong (e.g. an unusual naming convention). NULL (default) = auto
#'   for every file.
#' @param max_folders Safety cap on candidate folders scanned per group when computing
#'   the predicted mean map, applied AFTER ensemble selection (i.e. a subsample of the
#'   already-selected `ens_mode` candidates, not of the full run set). Default 200.
#' @param show_land Logical, default TRUE. Passed through to plot_spatial_residuals()
#'   for every raster in this batch -- draws land as a grey background layer so it
#'   reads as background instead of blank white space.
#' @param land_mask Optional data.frame from read_esri_ascii() (e.g. on
#'   combined_regions_5min.asc) identifying TRUE land cells -- passed through to
#'   plot_spatial_residuals() so only actual land is shaded grey, and water cells that
#'   simply lack data for a given raster stay white. NULL (default) falls back to the
#'   older whole-domain-grey behavior.
#' @param depth_grid,depth_threshold Optional data.frame(row, col, depth) and a depth
#'   threshold (default 500m) -- passed through to plot_spatial_residuals() so cells
#'   deeper than the threshold are drawn plain white instead of colored by residual
#'   value. NULL (default) skips this split entirely.
#' @param lon_breaks,lat_breaks Optional numeric vectors of longitude/latitude values
#'   for explicit dashed gridlines (replacing theme_bw()'s default grid) and axis tick
#'   positions. NULL (default) uses ggplot2's own automatic breaks/no explicit grid.
#' @param barheight,barwidth Gradient legend colorbar size (cm), passed through to
#'   plot_spatial_residuals(). Defaults 2.5/0.4.
#' @param facet_ncol Number of columns in the combined figure. Default 3.
#' @param base_size Base font size (pt), passed through to every plot_spatial_residuals()
#'   call in this batch. Default 11.
#' @param plots_dir Output folder. Default "plots".
#' @param width,height,dpi ggsave() figure dimensions (inches) and resolution (dpi).
#'   NULL (default, for width/height) auto-computes from facet_ncol/n_rows using the
#'   same formula as plot_ecospace_ensemble_maps_paired()'s no-variability case
#'   (max(4, facet_ncol*4.0) / max(3.5, n_rows*3.6)), so the two functions' output
#'   looks the same scale. Pass an explicit number to override.
#' @return Invisibly, a data.frame summarizing what happened: one row per raster file,
#'   with columns file, species_matched, group_token, status ("ok" or "skipped"),
#'   skip_reason, png_file (always NA per-file -- individual files always get combined
#'   into a single faceted PNG per species, "residuals_faceted_<species>.png", rather
#'   than saved separately).
#' @export
plot_spatial_residuals_batch <- function(raster_dir, ga_runs,
                                         ens_mode         = c("quantile", "topN", "aic"),
                                         keep_fitness_quantile = 0.9,
                                         top_n            = 100,
                                         top_n_prop       = NULL,
                                         target_ess       = 100,
                                         species_patterns = c("gag", "red grouper"),
                                         stanza_patterns  = NULL,
                                         raster_pattern = "\\.asc$",
                                         manual_tokens = NULL,
                                         max_folders = 200,
                                         show_land = TRUE,
                                         land_mask = NULL,
                                         depth_grid = NULL,
                                         depth_threshold = 500,
                                         lon_breaks = NULL,
                                         lat_breaks = NULL,
                                         barheight = 2.8,
                                         barwidth = 0.45,
                                         facet_ncol = 3,
                                         base_size = 11,
                                         plots_dir = "plots",
                                         width = NULL, height = NULL, dpi = 250){
  if(!requireNamespace("patchwork", quietly = TRUE))
    stop("Package 'patchwork' is required. Install it with install.packages('patchwork').")
  ens_mode <- match.arg(ens_mode)
  raster_files <- list.files(raster_dir, pattern = raster_pattern, full.names = TRUE)
  if(length(raster_files) == 0)
    stop("plot_spatial_residuals_batch(): no files matching '", raster_pattern, "' found in ", raster_dir)
  message("plot_spatial_residuals_batch(): ", length(raster_files), " raster file(s) found in ", raster_dir, ".")
  
  # ensemble candidate selection -- same mechanism as plot_ecospace_ensemble_maps_paired()
  # and plot_ecospace_ensemble_m0(): select_ensemble_candidates() decides WHICH
  # candidates count as "the ensemble" for this ens_mode, then only those feed the
  # predicted-mean map computation below. For "aic" mode, weights_by_folder carries
  # each candidate's own AIC weight through to compute_ensemble_map_stats(), which
  # weight-averages each candidate's own per-cell mean rather than pooling every
  # (candidate, year) observation equally -- true AIC-weighted spatial aggregation,
  # not the unweighted approximation this used to fall back to.
  valid <- ga_runs[ga_runs$exists, , drop = FALSE]
  sel <- select_ensemble_candidates(valid, mode = ens_mode, keep_fitness_quantile = keep_fitness_quantile,
                                    top_n = top_n, top_n_prop = top_n_prop, target_ess = target_ess)
  valid <- valid[valid$run_folder %in% sel$run_folder, , drop = FALSE]
  weights_by_folder <- if(ens_mode == "aic") stats::setNames(sel$ens_weight, sel$run_folder) else NULL
  if(nrow(valid) == 0)
    stop("No candidates left after ensemble selection (ens_mode = '", ens_mode, "').")
  message("plot_spatial_residuals_batch(): ", nrow(valid), " candidate(s) selected (ens_mode = '",
          ens_mode, "').")
  
  if(!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  safe_name <- function(s) gsub("[^A-Za-z0-9._-]+", "_", s)
  
  # extracts a "<species> <stanza>" guess from a filename, e.g.
  # "GFISHER_maxn_mod21_gag-1_5min_66x78.asc" -> "gag 1", and
  # "GFISHER_maxn_mod31_red-grouper-5-_5min_66x78.asc" -> "red grouper 5+" (a trailing
  # "-" after the stanza number is treated as "+", since that's the actual convention
  # observed in these filenames -- "+" itself can't appear in a filename on most
  # systems, so it gets written as a trailing dash instead). Handles gag/red grouper
  # with -, _, or space as separators throughout, case-insensitive. Doesn't try to be
  # exhaustive about every possible naming convention -- files that don't parse cleanly
  # should use `manual_tokens` instead of fighting the regex.
  guess_species_stanza <- function(fname){
    m <- regmatches(fname, regexpr("(?i)(gag|red[-_ ]?grouper)[-_ ]?(\\d+)(\\+|-(?![0-9]))?",
                                   fname, perl = TRUE))
    if(length(m) == 0 || !nzchar(m)) return(NA_character_)
    parts <- regmatches(m, regexec("(?i)(gag|red[-_ ]?grouper)[-_ ]?(\\d+)(\\+|-(?![0-9]))?", m, perl = TRUE))[[1]]
    sp <- tolower(gsub("[-_]", " ", parts[2]))
    sp <- trimws(gsub("\\s+", " ", sp))
    plus <- if(nzchar(parts[4])) "+" else ""
    paste0(sp, " ", parts[3], plus)
  }
  
  # normalizes -, _, and multiple spaces to a single space before comparing, so a
  # pattern written as "red grouper" still matches a filename spelled "red-grouper" or
  # "red_grouper" -- this was the actual cause of every red grouper file being skipped
  # at this very first check, before even reaching the stanza-parsing regex above
  .normalize_seps <- function(x) trimws(gsub("[-_]+", " ", gsub("\\s+", " ", x)))
  
  rows <- list()
  facet_panels <- list()
  for(rf in raster_files){
    fname <- basename(rf)
    fname_norm <- .normalize_seps(fname)
    matched_sp <- species_patterns[vapply(species_patterns, function(p)
      grepl(.normalize_seps(p), fname_norm, ignore.case = TRUE), logical(1))]
    if(length(matched_sp) == 0){
      rows[[length(rows) + 1]] <- data.frame(file = fname, species_matched = NA_character_,
                                             group_token = NA_character_, status = "skipped",
                                             skip_reason = "filename matched no species_patterns",
                                             png_file = NA_character_, stringsAsFactors = FALSE)
      next
    }
    
    if(!is.null(manual_tokens) && fname %in% names(manual_tokens)){
      group_token <- manual_tokens[[fname]]
      species_stanza <- NA_character_
    } else {
      species_stanza <- guess_species_stanza(fname)
      if(is.na(species_stanza)){
        rows[[length(rows) + 1]] <- data.frame(file = fname, species_matched = matched_sp[1],
                                               group_token = NA_character_, status = "skipped",
                                               skip_reason = "could not parse species+stanza from filename -- try manual_tokens",
                                               png_file = NA_character_, stringsAsFactors = FALSE)
        next
      }
      tok <- tryCatch(detect_map_group_tokens(valid$run_dir, "Biomass", species_patterns = species_stanza),
                      error = function(e) NULL)
      if(is.null(tok) || nrow(tok) == 0){
        rows[[length(rows) + 1]] <- data.frame(file = fname, species_matched = matched_sp[1],
                                               group_token = NA_character_, status = "skipped",
                                               skip_reason = paste0("no EcospaceMap token found matching '",
                                                                    species_stanza, "' -- try manual_tokens"),
                                               png_file = NA_character_, stringsAsFactors = FALSE)
        next
      }
      exact <- tok[tolower(tok$display) == species_stanza, , drop = FALSE]
      group_token <- if(nrow(exact) == 1) exact$token[1] else tok$token[1]
    }
    
    if(!is.null(stanza_patterns)){
      group_display <- gsub("_", " ", group_token)
      stanza_hit <- any(vapply(stanza_patterns, function(p) grepl(p, group_display, ignore.case = TRUE),
                               logical(1)))
      if(!stanza_hit){
        rows[[length(rows) + 1]] <- data.frame(file = fname, species_matched = matched_sp[1],
                                               group_token = group_token, status = "skipped",
                                               skip_reason = paste0("stanza '", group_display,
                                                                    "' not in stanza_patterns"),
                                               png_file = NA_character_, stringsAsFactors = FALSE)
        next
      }
    }
    
    pred_stats <- tryCatch(compute_ensemble_map_stats(valid$run_dir[seq_len(min(nrow(valid), max_folders))],
                                                      "Biomass", group_token, weights = weights_by_folder),
                           error = function(e) NULL)
    if(is.null(pred_stats) || is.null(pred_stats$agg)){
      rows[[length(rows) + 1]] <- data.frame(file = fname, species_matched = matched_sp[1],
                                             group_token = group_token, status = "skipped",
                                             skip_reason = "no candidate had this group's Biomass map archived",
                                             png_file = NA_character_, stringsAsFactors = FALSE)
      next
    }
    pred_mean <- as.data.frame(pred_stats$agg[, list(row, col, lat, lon, value = mean_val)])
    
    obs_raster <- tryCatch(read_esri_ascii(rf), error = function(e) NULL)
    if(is.null(obs_raster)){
      rows[[length(rows) + 1]] <- data.frame(file = fname, species_matched = matched_sp[1],
                                             group_token = group_token, status = "skipped",
                                             skip_reason = "read_esri_ascii() failed on this file",
                                             png_file = NA_character_, stringsAsFactors = FALSE)
      next
    }
    
    resid <- tryCatch(compute_spatial_residuals(obs_raster, pred_mean), error = function(e) NULL)
    if(is.null(resid)){
      rows[[length(rows) + 1]] <- data.frame(file = fname, species_matched = matched_sp[1],
                                             group_token = group_token, status = "skipped",
                                             skip_reason = "no overlapping observed/predicted cells",
                                             png_file = NA_character_, stringsAsFactors = FALSE)
      next
    }
    
    # defer actual panel-building until the full species group is known (below) --
    # show_x_axis/show_y_axis depend on each panel's FINAL position in the grid,
    # which we can't know until every raster in this species group has been scanned
    facet_panels[[length(facet_panels) + 1]] <- list(species = matched_sp[1], group_token = group_token,
                                                     resid = resid)
    rows[[length(rows) + 1]] <- data.frame(file = fname, species_matched = matched_sp[1],
                                           group_token = group_token, status = "ok",
                                           skip_reason = NA_character_, png_file = NA_character_,
                                           stringsAsFactors = FALSE)
  }
  
  if(length(facet_panels) > 0){
    species_groups <- unique(vapply(facet_panels, function(x) x$species, character(1)))
    for(sp in species_groups){
      sp_data <- Filter(function(x) identical(x$species, sp), facet_panels)
      n_this_group <- length(sp_data)
      n_rows_this_group <- ceiling(n_this_group / facet_ncol)
      
      sp_panels <- vector("list", n_this_group)
      for(idx in seq_len(n_this_group)){
        col_pos <- ((idx - 1) %% facet_ncol) + 1
        show_y_axis <- (col_pos == 1)
        panels_in_this_col <- seq(col_pos, n_this_group, by = facet_ncol)
        show_x_axis <- (idx == max(panels_in_this_col))
        
        # title="" + fixed_aspect=FALSE on the panel itself, with a separate title
        # built as its own text element and stacked above via `/` -- this is the
        # EXACT same mechanism plot_ecospace_ensemble_maps_paired() uses for its
        # variability=NULL path (same heights ratio, same margin), so the two
        # functions' arrangement is now genuinely structurally identical rather than
        # just superficially similar (previously this used a standard ggplot title
        # via labs(title=), a fundamentally different code path)
        resid_panel <- plot_spatial_residuals(
          sp_data[[idx]]$resid, title = "",
          show_land = show_land, land_mask = land_mask,
          depth_grid = depth_grid, depth_threshold = depth_threshold,
          show_x_axis = show_x_axis, show_y_axis = show_y_axis,
          lon_breaks = lon_breaks, lat_breaks = lat_breaks,
          barheight = barheight, barwidth = barwidth,
          inside_legend = TRUE, base_size = base_size,
          fixed_aspect = FALSE)
        
        title_grob <- patchwork::wrap_elements(
          grid::textGrob(gsub("_", " ", sp_data[[idx]]$group_token),
                         x = grid::unit(0.5, "npc"), y = grid::unit(0.40, "npc"),
                         gp = grid::gpar(fontsize = 16, fontface = "bold")),
          clip = FALSE)
        
        sp_panels[[idx]] <- (title_grob / resid_panel) +
          patchwork::plot_layout(heights = c(0.09, 1)) &
          ggplot2::theme(plot.margin = ggplot2::margin(2, 2, 2, 2))
      }
      combined <- patchwork::wrap_plots(sp_panels, ncol = facet_ncol)
      png_file <- file.path(plots_dir, paste0("residuals_faceted_", safe_name(sp), "_", ens_mode, ".png"))
      # same fix as plot_ecospace_ensemble_maps_paired()'s no-variability case: 3.45
      # (not 4.0) in/panel matches this model's actual coord_quickmap() aspect
      # (~1.046:1 width:height, from the 66x78 grid's lat/lon extent at ~27.75N mean
      # latitude) given the title_grob's 0.09-of-total-height allocation -- see that
      # function's own width-formula comment for the full derivation. The previous
      # 4.0in/panel left ~0.55in of excess width per panel, centered as whitespace on
      # both sides of the map, which read as extra space between columns specifically
      # (not rows) since only the horizontal dimension had this excess.
      w <- if(is.null(width)) max(3.5, facet_ncol * 3.45) else width
      h <- if(is.null(height)) max(3.5, n_rows_this_group * 3.6) else height
      ggplot2::ggsave(png_file, plot = combined, width = w, height = h, dpi = dpi, units = "in")
      message("  [ok] faceted '", sp, "' (", n_this_group, " panel(s)) -> ", png_file)
    }
  }
  
  out <- do.call(rbind, rows)
  message("plot_spatial_residuals_batch(): ", sum(out$status == "ok"), " of ", nrow(out),
          " raster(s) plotted (faceted by species).")
  invisible(out)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region-linked spatial + time series (age0 gag survey regions)
#
# Map centered, each region's time series panel placed as a directional inset
# (patchwork::inset_element()) with a black arrow drawn from that region's centroid to
# the map's edge in that direction -- see plot_region_timeseries_grid()'s own docs for
# how directions are assigned (angular order, collision-avoided). This is a genuine
# panel-per-region radial layout with real connector arrows, not an approximation.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' @title Aggregate a group's predicted biomass to per-region annual totals.
#' @description Reads one candidate's EcospaceMapBiomass export for `group_token`, sums
#'   biomass across every cell belonging to each region (per `region_mask`, e.g. from
#'   read_esri_ascii() on age0_survey_regions_5min_mod.asc), for every year. Region IDs
#'   <= 0 (land, unsampled water) are excluded.
#'
#'   IMPORTANT UNIT NOTE: EcospaceMap cell values are DENSITY (t/km^2), not total
#'   biomass -- summing density directly across cells (as an earlier version of this
#'   function did) produces a number with no real meaning, not a total. This function
#'   multiplies the per-cell density by `cell_area_km2` (the confirmed per-cell area for
#'   this model's 5-arcmin grid, ~9.26108 km per side) before summing, so the result is
#'   genuine total biomass (t) for the region -- comparable to EwE's own native
#'   Ecospace_Annual_Average_Region_<n>_Biomass.csv exports (used for the init/baseline
#'   run elsewhere in this script), which already report a properly-scaled quantity.
#'   Every cell in this grid is the same size, so multiplying the SUMMED density by one
#'   constant cell area is equivalent to summing each cell's own (density x area) --
#'   no need to look up a per-cell area.
#' @param run_dir Path to one candidate's output folder (containing
#'   EcospaceMapBiomass-<group_token>.csv).
#' @param group_token The group token as it appears in the Ecospace map filename (e.g.
#'   "gag_0" for EcospaceMapBiomass-gag_0.csv -- use detect_map_group_tokens() if unsure).
#' @param region_mask Output of read_esri_ascii() for a region-ID raster (e.g.
#'   age0_survey_regions_5min_mod.asc) -- a data.frame(row, col, value) where `value` is
#'   the region ID per cell.
#' @param cell_area_km2 Area of one grid cell in km^2. Default 9.26108^2 (this model's
#'   confirmed 5-arcmin cell size, ~85.77 km^2) -- override only if you know this
#'   specific region_mask uses a genuinely different grid resolution.
#' @return data.frame(year, region_id, biomass_sum) -- one row per (year, region)
#'   combination with at least one contributing cell. `biomass_sum` is now true total
#'   biomass (t), not a sum of densities.
#' @keywords internal
#' @noRd
# Reads one candidate's EcospaceMapBiomass export ONCE and derives BOTH the per-region
# aggregation (same result as aggregate_biomass_by_region()) and the domain-wide total
# (sum across EVERY active cell, not restricted to any region -- same method
# .compute_domain_wide_data() uses for the best-fit run) from that single read. Used by
# plot_region_timeseries_grid()'s ensemble loop so a candidate's map file is only read
# once even though both the per-region panels AND the WFS (domain-wide) panel need an
# ensemble mean/CI computed from the same underlying candidate set.
#' @return list(by_region = data.frame(year, region_id, biomass_sum), domain =
#'   data.frame(year, biomass_sum)) -- both already converted from density to total
#'   biomass (t) via cell_area_km2.
.aggregate_biomass_by_region_and_domain <- function(run_dir, group_token, region_mask,
                                                    cell_area_km2 = 9.26108^2){
  path <- file.path(run_dir, paste0("EcospaceMapBiomass-", group_token, ".csv"))
  if(!file.exists(path)) stop(".aggregate_biomass_by_region_and_domain(): file not found: ", path)
  m <- read_ecospace_map(path)
  
  rmask <- region_mask[!is.na(region_mask$value) & region_mask$value > 0, c("row", "col", "value")]
  names(rmask)[3] <- "region_id"
  
  joined <- merge(m$data, rmask, by = c("row", "col"))
  joined <- joined[!is.na(joined$value), , drop = FALSE]
  by_region <- NULL
  if(nrow(joined) > 0){
    by_region <- stats::aggregate(value ~ year + region_id, data = joined, FUN = sum, na.rm = TRUE)
    names(by_region)[3] <- "biomass_sum"
    by_region$biomass_sum <- by_region$biomass_sum * cell_area_km2
  }
  
  cell_data <- m$data[!is.na(m$data$value), , drop = FALSE]
  domain <- NULL
  if(nrow(cell_data) > 0){
    domain <- stats::aggregate(value ~ year, data = cell_data, FUN = sum, na.rm = TRUE)
    names(domain)[2] <- "biomass_sum"
    domain$biomass_sum <- domain$biomass_sum * cell_area_km2
  }
  
  list(by_region = by_region, domain = domain)
}


aggregate_biomass_by_region <- function(run_dir, group_token, region_mask, cell_area_km2 = 9.26108^2){
  path <- file.path(run_dir, paste0("EcospaceMapBiomass-", group_token, ".csv"))
  if(!file.exists(path)) stop("aggregate_biomass_by_region(): file not found: ", path)
  m <- read_ecospace_map(path)
  
  rmask <- region_mask[!is.na(region_mask$value) & region_mask$value > 0, c("row", "col", "value")]
  names(rmask)[3] <- "region_id"
  
  joined <- merge(m$data, rmask, by = c("row", "col"))
  joined <- joined[!is.na(joined$value), , drop = FALSE]
  if(nrow(joined) == 0)
    stop("aggregate_biomass_by_region(): no cells with both map data and a positive region ID -- ",
         "check that region_mask uses the SAME grid (read_esri_ascii() confirms this for this model's domain).")
  
  agg <- stats::aggregate(value ~ year + region_id, data = joined, FUN = sum, na.rm = TRUE)
  names(agg)[3] <- "biomass_sum"
  agg$biomass_sum <- agg$biomass_sum * cell_area_km2  # density (t/km^2) -> total biomass (t)
  message("aggregate_biomass_by_region(): ", nrow(agg), " (year, region) combination(s) across ",
          length(unique(agg$region_id)), " region(s) (density converted to total biomass using ",
          signif(cell_area_km2, 4), " km^2/cell).")
  agg
}


#' @keywords internal
#' @noRd
# Shared region_label -> color lookup, so the map and the time series line colors use
# EXACTLY the same palette (computed once, reused by both plot_region_map() and
# plot_region_timeseries_grid()) rather than two independently-generated color sets
# that might not match.
#
# Default palette is a HAND-PICKED set (not a named RColorBrewer palette), because
# Set1 and Dark2 both produced colors that read as confusable in practice (Set1's red
# vs orange, Dark2's teal-green vs olive-green). This set is based on Okabe-Ito (a
# widely-used colorblind-safe palette), extended to 9 colors with purple/brown, chosen
# specifically to avoid: red/orange adjacency, two similar greens, and blue/purple
# adjacency -- the three confusions reported so far.
.CUSTOM_REGION_PALETTE <- c(
  "#0072B2",  # blue
  "#E69F00",  # orange
  "#009E73",  # green (only one green in this set)
  "#CC79A7",  # pink/magenta
  "#D55E00",  # vermillion (red-leaning, kept visually distinct from orange above)
  "#F0E442",  # yellow
  "#6A3D9A",  # purple (kept visually distinct from blue above)
  "#8B4513",  # brown
  "#999999"   # grey
)

.region_color_palette <- function(region_levels, palette = "custom"){
  if(identical(palette, "custom")){
    n <- length(region_levels)
    if(n > length(.CUSTOM_REGION_PALETTE))
      stop(".region_color_palette(): 'custom' palette only has ", length(.CUSTOM_REGION_PALETTE),
           " colors but ", n, " region(s) need one -- pass a different `palette` ",
           "(e.g. a RColorBrewer palette name, or \"viridis\") for more than that many regions.")
    stats::setNames(.CUSTOM_REGION_PALETTE[seq_len(n)], region_levels)
  } else if(identical(palette, "viridis")){
    if(!requireNamespace("scales", quietly = TRUE))
      stop("Package 'scales' is required for palette = 'viridis'.")
    stats::setNames(scales::viridis_pal()(length(region_levels)), region_levels)
  } else {
    if(!requireNamespace("RColorBrewer", quietly = TRUE))
      stop("Package 'RColorBrewer' is required for palette = '", palette,
           "'. Install it with install.packages('RColorBrewer'), or pass palette = 'viridis'.")
    stats::setNames(RColorBrewer::brewer.pal(max(3, length(region_levels)), palette)[seq_along(region_levels)],
                    region_levels)
  }
}


#' @title Plot a region-ID raster, colored/labeled by region name.
#' @param region_mask Output of read_esri_ascii() (e.g. on age0_survey_regions_5min_mod.asc
#'   or combined_regions_5min.asc).
#' @param region_attributes Optional data.frame with columns ID and NAME (e.g. read
#'   directly from combined_regions_5min_attributes.csv) to label regions by name
#'   instead of bare ID. NULL (default) labels by numeric ID.
#' @param title Plot title. Default "".
#' @param palette Discrete fill palette for regions. Default "Set3". Pass any
#'   RColorBrewer palette name, or "viridis".
#' @param region_colors Optional pre-computed named vector (region_label -> color), from
#'   .region_color_palette() -- pass this to guarantee the SAME colors as a companion
#'   time series plot. NULL (default) computes fresh from `palette`.
#' @param show_land Logical, default TRUE. Shows land and unsampled-water cells as
#'   distinct grey shades for geographic context, but they are DELIBERATELY EXCLUDED
#'   from the legend (via `breaks =`) -- only the actual named regions get a legend
#'   entry, since land/water aren't really "regions" in the sense the legend is about.
#' @param fixed_aspect Logical, default FALSE. TRUE uses coord_quickmap() (correct
#'   geographic aspect ratio). FALSE (default) uses coord_cartesian() with no aspect
#'   lock, letting the panel fill whatever shape it's given -- combine with an explicit
#'   square width=height in ggsave()/patchwork sizing (see plot_region_timeseries_grid())
#'   to get an actually-square map panel.
#' @return A ggplot object.
#' @export
plot_region_map <- function(region_mask, region_attributes = NULL, title = "",
                            palette = "custom", region_colors = NULL,
                            show_land = TRUE, fixed_aspect = FALSE, base_size = 11){
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
  
  df <- region_mask
  df$region_label <- NA_character_
  is_region <- !is.na(df$value) & df$value > 0
  if(any(is_region)){
    if(!is.null(region_attributes)){
      lab <- stats::setNames(as.character(region_attributes$NAME), as.character(region_attributes$ID))
      df$region_label[is_region] <- ifelse(as.character(df$value[is_region]) %in% names(lab),
                                           lab[as.character(df$value[is_region])],
                                           as.character(df$value[is_region]))
    } else {
      df$region_label[is_region] <- as.character(df$value[is_region])
    }
  }
  if(show_land){
    df$region_label[is.na(df$value)] <- "Land"
    df$region_label[!is.na(df$value) & df$value == 0] <- "Water (unsampled)"
  } else {
    df <- df[is_region, , drop = FALSE]
  }
  region_levels <- sort(unique(df$region_label[!(df$region_label %in% c("Land", "Water (unsampled)"))]))
  df$region_label <- factor(df$region_label, levels = c(region_levels, "Water (unsampled)", "Land"))
  
  if(is.null(region_colors)) region_colors <- .region_color_palette(region_levels, palette)
  manual_extra <- c("Water (unsampled)" = "white", "Land" = "grey65")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = lon, y = lat, fill = region_label)) +
    ggplot2::geom_raster() +
    ggplot2::labs(title = title, x = "Longitude", y = "Latitude", fill = "Region") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   # size/margin/vjust now MATCH the region panel title convention
                   # (previously 1.4x here vs 1.05x there) -- that size mismatch is the
                   # most likely cause of row 1 (which shares its row with this map,
                   # since the map spans both rows) getting more vertical headroom than
                   # row 2, since patchwork allocates row height based on the tallest
                   # content sharing that row
                   plot.title = ggplot2::element_text(size = base_size * 1.05, face = "bold", hjust = 0.5,
                                                      margin = ggplot2::margin(b = 0), vjust = 0.2),
                   aspect.ratio = 1)
  
  if(show_land){
    # breaks = region_levels ONLY -- Land/Water still render (their colors are in
    # `values`) but get NO legend entry, since breaks controls what appears in the guide
    p <- p + ggplot2::scale_fill_manual(values = c(region_colors, manual_extra), name = "Region",
                                        breaks = region_levels, na.value = "white")
  } else {
    p <- p + ggplot2::scale_fill_manual(values = region_colors, name = "Region", na.value = "white")
  }
  
  p <- p + if(fixed_aspect) ggplot2::coord_quickmap() else ggplot2::coord_cartesian(expand = FALSE)
  p
}


#' @title Region map + per-region time series, in a plain grid layout.
#' @description Map occupies a 2x2 block on the left of a 5-column x 2-row grid; the
#'   remaining 3x2 block holds up to 6 region panels (row 1 left-to-right, then row 2).
#'   Only regions with at least one MATCHED observed series are plotted.
#' @param run_dir Best-fit candidate's output folder. If NULL, supply `ga_runs` +
#'   `species_pattern` instead to auto-detect the best candidate that actually has this
#'   file archived.
#' @param init_run_dir Optional baseline (pre-calibration) run folder -- if supplied,
#'   plots init alongside best-fit in every panel (dashed vs. solid).
#' @param group_token Group token for the Ecospace map filename (e.g. "gag_0"). Auto-
#'   detected via `ga_runs`/`species_pattern` if NULL.
#' @param region_mask Raw output of read_esri_ascii() (not pre-filtered).
#' @param ga_runs Output of resolve_run_dirs(). Required if `run_dir` is NULL, and also
#'   required (regardless of `run_dir`) if `show_ensemble = TRUE`.
#' @param species_pattern Case-insensitive pattern, e.g. "gag 0". Required if `run_dir`
#'   is NULL; be specific enough to match exactly one stanza.
#' @param show_ensemble Logical, default FALSE. TRUE adds an AIC-weighted ensemble mean
#'   line (grey) plus a semi-transparent 95% CI ribbon to every region panel (and the
#'   domain-wide WFS panel), computed the same way as every other ensemble function in
#'   this script: select_ensemble_candidates(mode="aic", ...) picks the weighted pool,
#'   aggregate_biomass_by_region() is run for EACH selected candidate (this is the
#'   expensive part -- one EcospaceMapBiomass CSV read per candidate, same cost as
#'   compute_ensemble_map_stats() elsewhere), then weighted.mean()/weighted_quantile()
#'   (2.5th/97.5th percentile) are computed per (region, year) across candidates.
#'   Requires `ga_runs` regardless of whether `run_dir` was supplied explicitly.
#' @param ens_mode,keep_fitness_quantile,top_n,top_n_prop,target_ess Passed to
#'   select_ensemble_candidates() when `show_ensemble = TRUE`. Same defaults as every
#'   other ensemble function in this script (ens_mode="aic" is the only mode this
#'   function actually computes a weighted mean/CI for -- "quantile"/"topN" have no
#'   weights, so requesting those here just uses an unweighted mean/95% range instead).
#' @param ensemble_max_folders Safety cap on how many selected candidates to actually
#'   read (a random subsample is used if more are selected, matching the `max_folders`
#'   convention elsewhere). Default 200.
#' @param region_attributes Optional data.frame(ID, NAME) for region labels.
#' @param obs Output of read_ewe_timeseries() -- REQUIRED, since regions without a
#'   matched observed series are excluded. Non-absolute series are q-rescaled to the
#'   best-fit predicted scale before plotting.
#' @param facet_ncol Columns for any panels beyond the first 8. Default 3.
#' @param map_palette,map_show_land,map_fixed_aspect Passed to plot_region_map().
#' @param plots_dir,png_file Output folder/filename.
#' @param width,height Nominal figure size (inches) -- actual saved figure is square,
#'   using max(width, height).
#' @param dpi ggsave() resolution.
#' @param base_size Base font size (pt); this, not dpi, controls text size relative to
#'   the plot. Default 22.
#' @param include_domain_wide Logical, default TRUE. Adds a 6th panel: the group's
#'   total biomass across the whole domain (see plot_domain_wide_biomass()).
#' @param domain_obs_pattern Used only when `include_domain_wide = TRUE` -- filters
#'   which Region-0 observed series to show (e.g. "FWCNMFS").
#' @param cell_area_km2 Used only when `include_domain_wide = TRUE`. Default 9.26108^2.
#' @return Invisibly, the per-region aggregated data.frame, with attr(., "run_dir")/
#'   attr(., "init_run_dir")/attr(., "group_token")/attr(., "n_regions_total").
#' @export
plot_region_timeseries_grid <- function(run_dir = NULL, init_run_dir = NULL, group_token = NULL,
                                        region_mask,
                                        ga_runs = NULL, species_pattern = NULL,
                                        show_ensemble = FALSE,
                                        ens_mode = c("aic", "quantile", "topN"),
                                        keep_fitness_quantile = 0.9,
                                        top_n = 100, top_n_prop = NULL, target_ess = 100,
                                        ensemble_max_folders = 200,
                                        region_attributes = NULL, obs,
                                        base_size = 36,
                                        include_domain_wide = TRUE,
                                        domain_obs_pattern = NULL,
                                        cell_area_km2 = 9.26108^2,
                                        map_palette = "custom", map_show_land = TRUE,
                                        map_fixed_aspect = FALSE,
                                        plots_dir = "plots",
                                        png_file = NULL,
                                        width = 16, height = 16, dpi = 400){
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
  if(!requireNamespace("patchwork", quietly = TRUE))
    stop("Package 'patchwork' is required. Install it with install.packages('patchwork').")
  if(missing(obs) || is.null(obs))
    stop("plot_region_timeseries_grid(): `obs` is required -- only regions with a matched ",
         "observed series are plotted (see description).")
  ens_mode <- match.arg(ens_mode)
  if(show_ensemble && is.null(ga_runs))
    stop("plot_region_timeseries_grid(): show_ensemble = TRUE requires `ga_runs`, ",
         "regardless of whether `run_dir` was supplied explicitly.")
  
  if(is.null(run_dir)){
    if(is.null(ga_runs) || is.null(species_pattern))
      stop("plot_region_timeseries_grid(): either supply `run_dir` + `group_token` directly, ",
           "or supply `ga_runs` + `species_pattern` for automatic detection/selection.")
    valid <- ga_runs[ga_runs$exists, , drop = FALSE]
    
    if(is.null(group_token)){
      tok <- detect_map_group_tokens(valid$run_dir, "Biomass", species_patterns = species_pattern)
      exact <- tok[tolower(tok$display) == tolower(species_pattern), , drop = FALSE]
      if(nrow(exact) == 1){
        group_token <- exact$token[1]
      } else if(nrow(tok) == 1){
        group_token <- tok$token[1]
      } else {
        stop("plot_region_timeseries_grid(): species_pattern = '", species_pattern, "' matched ",
             nrow(tok), " token(s) (", paste(tok$display, collapse = ", "), ") -- ",
             "be more specific (e.g. 'gag 0' not 'gag'), or pass group_token explicitly.")
      }
      message("plot_region_timeseries_grid(): auto-detected group_token = '", group_token, "'.")
    }
    
    has_file <- vapply(valid$run_dir, function(d)
      file.exists(file.path(d, paste0("EcospaceMapBiomass-", group_token, ".csv"))), logical(1))
    if(!any(has_file))
      stop("plot_region_timeseries_grid(): NO candidate has EcospaceMapBiomass-", group_token,
           ".csv archived -- this group's spatial output may not have been saved for any candidate.")
    candidates_with_file <- valid[has_file, , drop = FALSE]
    best_with_file <- candidates_with_file[which.min(candidates_with_file$fitness), ]
    run_dir <- best_with_file$run_dir
    message("plot_region_timeseries_grid(): ", sum(has_file), " of ", nrow(valid),
            " candidate(s) have this file archived -- using the best-fitting one (fitness = ",
            signif(best_with_file$fitness, 6), ", ", best_with_file$run_folder, ").")
  } else if(is.null(group_token)){
    stop("plot_region_timeseries_grid(): group_token is required when run_dir is supplied explicitly.")
  }
  
  agg <- aggregate_biomass_by_region(run_dir, group_token, region_mask)
  agg$run <- "best-fit"
  if(!is.null(init_run_dir)){
    # init_run_dir has NO archived spatial EcospaceMapBiomass export -- only EwE's own
    # per-region annual-average files, one per region:
    # Ecospace_Annual_Average_Region_<n>_Biomass.csv. Read these DIRECTLY via
    # get_region_output() (the same reader used elsewhere in this script for domain/
    # region-level, non-spatial predicted output) rather than trying to reconstruct a
    # per-region sum from cells the way aggregate_biomass_by_region() does for the
    # best-fit run.
    #
    # SAME UNIT FIX AS aggregate_biomass_by_region(): this file also reports DENSITY
    # (t/km^2), one value per region per year, not total biomass -- confirmed
    # empirically (init was showing near-zero/flat relative to both best-fit and the
    # observed points once best-fit was correctly converted to total biomass). Since
    # this is already a per-REGION value (not per-cell), convert using that region's
    # TOTAL area (cell count x cell_area_km2), not a single cell's area.
    cell_area_km2 <- 9.26108^2
    region_cell_counts <- table(region_mask$value[!is.na(region_mask$value) & region_mask$value > 0])
    region_area_km2 <- as.numeric(region_cell_counts) * cell_area_km2
    names(region_area_km2) <- names(region_cell_counts)
    
    init_cache <- new.env(parent = emptyenv())
    match_pattern <- if(!is.null(species_pattern)) species_pattern else group_token
    init_rows <- list()
    for(rid in unique(agg$region_id)){
      pr <- get_region_output(rid, "Biomass", init_run_dir, init_cache)
      if(is.null(pr)) next
      group_cols <- setdiff(names(pr$data), "year")
      col_match <- group_cols[tolower(group_cols) == tolower(match_pattern)]
      if(length(col_match) == 0)
        col_match <- group_cols[grepl(match_pattern, group_cols, ignore.case = TRUE)]
      if(length(col_match) == 0){
        message("  [note] region ", rid, "'s init Biomass file has no column matching '",
                match_pattern, "' (columns present: ", paste(group_cols, collapse = ", "), ")")
        next
      }
      this_area <- region_area_km2[as.character(rid)]
      if(is.na(this_area)){
        message("  [note] region ", rid, " not found in region_mask's cell counts -- ",
                "cannot convert init density to total biomass for this region, skipping.")
        next
      }
      init_rows[[length(init_rows) + 1]] <- data.frame(
        year = pr$data$year, region_id = rid, biomass_sum = pr$data[[col_match[1]]] * this_area,
        stringsAsFactors = FALSE)
    }
    if(length(init_rows) > 0){
      agg_init <- do.call(rbind, init_rows)
      agg_init$run <- "init"
      agg <- rbind(agg, agg_init)
      message("  init: found region-level Biomass data for ", length(init_rows), " of ",
              length(unique(agg$region_id[agg$run == "best-fit"])), " region(s) in init_run_dir ",
              "(via Ecospace_Annual_Average_Region_<n>_Biomass.csv, density converted to total ",
              "biomass using each region's own total area).")
    } else {
      message("  [note] init_run_dir has no matching per-region Biomass export for '",
              match_pattern, "' in ANY region -- showing best-fit only.")
    }
  }
  
  # ---- optional AIC-weighted ensemble mean + 95% CI ribbon, per region AND domain-wide ----
  # Same selection mechanism as every other ensemble function in this script
  # (select_ensemble_candidates()), but this is genuinely more expensive than those:
  # there's no per-cell streaming shortcut here (unlike compute_ensemble_map_stats()),
  # so this runs .aggregate_biomass_by_region_and_domain() -- a full EcospaceMapBiomass
  # CSV read -- once PER SELECTED CANDIDATE. Reported explicitly below so a slow run
  # isn't mistaken for a hang. Each candidate's map is read ONCE and used to derive
  # BOTH the per-region totals and the domain-wide (WFS) total, rather than reading the
  # same file twice.
  ribbon_df <- NULL
  domain_ribbon_df <- NULL
  domain_ens_mean_df <- NULL
  if(show_ensemble){
    valid_ens <- ga_runs[ga_runs$exists, , drop = FALSE]
    sel <- select_ensemble_candidates(valid_ens, mode = ens_mode,
                                      keep_fitness_quantile = keep_fitness_quantile,
                                      top_n = top_n, top_n_prop = top_n_prop, target_ess = target_ess)
    has_file_ens <- vapply(sel$run_dir, function(d)
      file.exists(file.path(d, paste0("EcospaceMapBiomass-", group_token, ".csv"))), logical(1))
    sel <- sel[has_file_ens, , drop = FALSE]
    if(nrow(sel) == 0){
      message("  [note] show_ensemble = TRUE, but NONE of the ", ens_mode, "-selected candidates ",
              "have EcospaceMapBiomass-", group_token, ".csv archived -- skipping the ensemble ",
              "mean/ribbon, showing best-fit (and init, if supplied) only.")
    } else {
      if(nrow(sel) > ensemble_max_folders){
        keep_idx <- unique(c(which.min(sel$fitness), sample(seq_len(nrow(sel)), ensemble_max_folders - 1)))
        sel <- sel[keep_idx, , drop = FALSE]
        message("  ensemble: subsampling to ", ensemble_max_folders, " of the ", ens_mode,
                "-selected candidates (best-fit always kept) for performance.")
      }
      message("  ensemble: reading ", nrow(sel), " candidate(s) for the weighted mean/95% CI ",
              "ribbon (one EcospaceMapBiomass CSV read each, reused for BOTH per-region and ",
              "domain-wide -- this is the slow part)...")
      ens_rows <- list()
      ens_domain_rows <- list()
      n_ens_found <- 0
      for(i in seq_len(nrow(sel))){
        both_i <- tryCatch(.aggregate_biomass_by_region_and_domain(sel$run_dir[i], group_token,
                                                                   region_mask, cell_area_km2),
                           error = function(e) NULL)
        if(is.null(both_i) || is.null(both_i$by_region)) next
        agg_i <- both_i$by_region
        agg_i$run_folder <- sel$run_folder[i]
        agg_i$weight <- if("ens_weight" %in% names(sel)) sel$ens_weight[i] else 1
        ens_rows[[length(ens_rows) + 1]] <- agg_i
        if(!is.null(both_i$domain)){
          dom_i <- both_i$domain
          dom_i$run_folder <- sel$run_folder[i]
          dom_i$weight <- if("ens_weight" %in% names(sel)) sel$ens_weight[i] else 1
          ens_domain_rows[[length(ens_domain_rows) + 1]] <- dom_i
        }
        n_ens_found <- n_ens_found + 1
      }
      if(n_ens_found == 0){
        message("  [note] ensemble candidates were selected, but none could actually be read -- ",
                "skipping the ensemble mean/ribbon.")
      } else {
        ens_long <- do.call(rbind, ens_rows)
        ens_dt <- data.table::as.data.table(ens_long)
        ens_summary <- ens_dt[, {
          wm <- stats::weighted.mean(biomass_sum, weight, na.rm = TRUE)
          qs <- weighted_quantile(biomass_sum, weight, probs = c(0.025, 0.975))
          list(mean_val = wm, lo = qs[1], hi = qs[2])
        }, by = .(year, region_id)]
        ens_summary <- as.data.frame(ens_summary)
        
        agg_ens <- data.frame(year = ens_summary$year, region_id = ens_summary$region_id,
                              biomass_sum = ens_summary$mean_val, run = "ensemble_mean",
                              stringsAsFactors = FALSE)
        agg <- rbind(agg, agg_ens)
        ribbon_df <- ens_summary[, c("year", "region_id", "lo", "hi")]
        message("  ensemble: ", n_ens_found, " of ", nrow(sel), " selected candidate(s) actually read; ",
                "weighted mean + 95% CI ribbon computed for ", length(unique(ens_summary$region_id)),
                " region(s).")
        
        if(length(ens_domain_rows) > 0){
          ens_domain_long <- do.call(rbind, ens_domain_rows)
          ens_domain_dt <- data.table::as.data.table(ens_domain_long)
          domain_summary <- ens_domain_dt[, {
            wm <- stats::weighted.mean(biomass_sum, weight, na.rm = TRUE)
            qs <- weighted_quantile(biomass_sum, weight, probs = c(0.025, 0.975))
            list(mean_val = wm, lo = qs[1], hi = qs[2])
          }, by = .(year)]
          domain_summary <- as.data.frame(domain_summary)
          domain_ens_mean_df <- data.frame(year = domain_summary$year, value = domain_summary$mean_val,
                                           stringsAsFactors = FALSE)
          domain_ribbon_df <- domain_summary[, c("year", "lo", "hi")]
          message("  ensemble: domain-wide (WFS) weighted mean + 95% CI ribbon computed from the ",
                  "same ", length(ens_domain_rows), " candidate read(s).")
        } else {
          message("  [note] per-region ensemble data was computed, but no candidate's map yielded ",
                  "a domain-wide total -- WFS panel will show best-fit (and init) only.")
        }
      }
    }
  }
  
  lab <- if(!is.null(region_attributes))
    stats::setNames(as.character(region_attributes$NAME), as.character(region_attributes$ID)) else NULL
  agg$region_label <- if(!is.null(lab))
    ifelse(as.character(agg$region_id) %in% names(lab), lab[as.character(agg$region_id)],
           as.character(agg$region_id)) else as.character(agg$region_id)
  
  n_regions_total <- length(unique(agg$region_id))
  
  # match observed series by exact Region-code equality, then q-rescale each series to
  # the BEST-FIT predicted scale (mean-matching over years common to both) if it's a
  # non-absolute (index-type) series -- same convention as every other ensemble/obs-vs-
  # pred function in this script
  th <- obs$ts.head
  obs_by_region <- list()
  for(rid in unique(agg$region_id)){
    obs_rows <- which(as.character(th$Region) == as.character(rid))
    if(length(obs_rows) == 0) next
    pred_best <- agg[agg$region_id == rid & agg$run == "best-fit", c("year", "biomass_sum")]
    for(r in obs_rows){
      ov <- obs$ts[[th$Title[r]]]; oy <- obs$ts$year
      keep <- !is.na(ov)
      if(!any(keep)) next
      common <- intersect(oy[keep], pred_best$year)
      q <- 1
      if(!isTRUE(th$Absolute[r]) && length(common) > 0){
        mo <- mean(ov[oy %in% common], na.rm = TRUE)
        mp <- mean(pred_best$biomass_sum[pred_best$year %in% common], na.rm = TRUE)
        if(is.finite(mo) && is.finite(mp) && mo > 0) q <- mo / mp
      }
      obs_by_region[[length(obs_by_region) + 1]] <- data.frame(
        region_id = rid, year = oy[keep], value = ov[keep] / q, series = th$Title[r],
        stringsAsFactors = FALSE)
    }
  }
  obs_df <- if(length(obs_by_region) > 0) do.call(rbind, obs_by_region) else NULL
  if(is.null(obs_df))
    stop("plot_region_timeseries_grid(): no observed series had a Region code matching ANY of ",
         "this raster's region IDs -- nothing to plot (every region would be excluded by the ",
         "observed-only filter). Check obs$ts.head$Region values against region_mask's IDs.")
  
  # observed-only filter: drop any region with no matched observed data
  regions_with_obs <- unique(obs_df$region_id)
  agg <- agg[agg$region_id %in% regions_with_obs, , drop = FALSE]
  message("plot_region_timeseries_grid(): ", length(regions_with_obs), " of ", n_regions_total,
          " region(s) have a matched observed series -- only those are plotted.")
  
  # region colors computed ONCE here, passed to BOTH the map and every time series
  # panel below, so line colors on the panels exactly match that region's color on the
  # map (rather than two independently-generated palettes that might not agree)
  region_levels <- sort(unique(agg$region_label[agg$region_id %in% regions_with_obs]))
  region_colors <- .region_color_palette(region_levels, map_palette)
  # Set3's pale yellow (#FFFFB3) is low-contrast against a white background -- swap it
  # for a more visible amber/gold if it shows up in this palette
  region_colors[region_colors == "#FFFFB3"] <- "#E8A33D"
  
  ts_panels <- list()
  for(rid in sort(regions_with_obs)){
    sub <- agg[agg$region_id == rid, , drop = FALSE]
    sub_best <- sub[sub$run == "best-fit", , drop = FALSE]
    sub_init <- sub[sub$run == "init", , drop = FALSE]
    sub_ens  <- sub[sub$run == "ensemble_mean", , drop = FALSE]
    sub_ribbon <- if(!is.null(ribbon_df)) ribbon_df[ribbon_df$region_id == rid, , drop = FALSE] else NULL
    sub_obs <- obs_df[obs_df$region_id == rid, , drop = FALSE]
    sub_obs$series <- droplevels(factor(sub_obs$series))  # this panel's own series only
    lbl <- sub$region_label[1]
    this_color <- unname(region_colors[lbl])
    
    pp <- ggplot2::ggplot()
    if(show_ensemble){
      # ONLY the AIC-weighted ensemble mean + its 95% CI ribbon -- both in the
      # region's own color, ribbon at low alpha for the uncertainty shading. init and
      # best-fit are deliberately NOT drawn in this mode (per request: only the
      # weighted mean and its surrounding uncertainty, nothing else).
      if(!is.null(sub_ribbon) && nrow(sub_ribbon) > 0)
        pp <- pp + ggplot2::geom_ribbon(data = sub_ribbon,
                                        ggplot2::aes(x = year, ymin = pmax(lo, 0), ymax = hi),
                                        fill = this_color, alpha = 0.25, colour = NA)
      if(nrow(sub_ens) > 0)
        pp <- pp + ggplot2::geom_line(data = sub_ens, ggplot2::aes(x = year, y = biomass_sum),
                                      color = this_color, linewidth = 1.8)
    } else {
      if(nrow(sub_init) > 0)
        pp <- pp + ggplot2::geom_line(data = sub_init, ggplot2::aes(x = year, y = biomass_sum, linetype = "init"),
                                      color = "black", linewidth = 1.4)
      pp <- pp + ggplot2::geom_line(data = sub_best, ggplot2::aes(x = year, y = biomass_sum, linetype = "best-fit"),
                                    color = this_color, linewidth = 1.8)
      pp <- pp + ggplot2::scale_linetype_manual(values = c("init" = "22", "best-fit" = "solid"), name = NULL)
    }
    pp <- pp + ggplot2::geom_point(data = sub_obs, ggplot2::aes(x = year, y = value, shape = series),
                                   color = "black", size = base_size * 0.18, stroke = 1)
    if(nlevels(sub_obs$series) > 0)
      pp <- pp + ggplot2::scale_shape_manual(values = rep(0:25, length.out = nlevels(sub_obs$series)))
    pp <- pp + ggplot2::labs(title = lbl, x = "Year", y = "Biomass (t)", shape = "Observed")
    pp <- pp + ggplot2::theme_bw(base_size = base_size)
    pp <- pp + ggplot2::theme(plot.title = ggplot2::element_text(size = base_size * 1.05, face = "bold",
                                                                 color = "black", margin = ggplot2::margin(b = 0),
                                                                 vjust = 0.2),
                              plot.margin = ggplot2::margin(2, 2, 2, 2),
                              aspect.ratio = 1)
    # observed-series shape legend placed INSIDE the panel, top-left -- matches the WFS
    # (domain-wide) panel's own legend style exactly, rather than the wider "top" row
    # design from an earlier version, which also had the side effect of pushing the
    # panel title further from the plot frame (removing that legend row brings the
    # title back down close to the frame too). linetype legend (init/best-fit)
    # suppressed per-panel and shown once via the LAST panel below when applicable --
    # in show_ensemble mode there's no linetype aesthetic at all, so this guide is a
    # no-op there.
    pp <- pp + ggplot2::guides(shape = ggplot2::guide_legend(position = "inside", title = NULL), linetype = "none")
    pp <- pp + ggplot2::theme(
      legend.position.inside = c(0.02, 0.98),
      legend.justification.inside = c(0, 1),
      legend.key.size = grid::unit(0.5, "cm"),
      legend.text = ggplot2::element_text(size = base_size * 0.55),
      legend.background = ggplot2::element_rect(fill = scales::alpha("white", 0.65), colour = NA),
      legend.margin = ggplot2::margin(1, 2, 1, 2)
    )
    ts_panels[[as.character(rid)]] <- pp
  }
  if(length(ts_panels) == 0) stop("plot_region_timeseries_grid(): no region panels to plot.")
  
  # simple grid layout: map (bigger, 2x2 block) + region panels, plain reading order,
  # NO arrows/direction assignment -- this replaced an earlier radial/compass-arrow
  # design that turned out to be less readable in practice
  map_panel <- plot_region_map(region_mask, region_attributes, title = paste0(group_token, " survey regions"),
                               palette = map_palette, region_colors = region_colors,
                               show_land = map_show_land, fixed_aspect = map_fixed_aspect,
                               base_size = base_size)
  map_panel <- map_panel + ggplot2::guides(fill = ggplot2::guide_legend(position = "inside"))
  map_panel <- map_panel + ggplot2::theme(
    legend.position.inside = c(0.02, 0.02),
    legend.justification.inside = c(0, 0),
    legend.background = ggplot2::element_rect(fill = scales::alpha("white", 0.75), colour = NA),
    # reduced from the previous 2.0cm/base_size*2/base_size*2.1 (too large) -- still
    # clearly the most prominent legend in the figure, just not overwhelming
    legend.key.size = grid::unit(1.0, "cm"),
    legend.text = ggplot2::element_text(size = base_size * 1.15),
    legend.title = ggplot2::element_text(size = base_size * 1.25),
    # explicit larger axis text -- was relying on theme_bw()'s default (base_size*0.8),
    # which read too small for the map's lon/lat tick labels
    axis.text = ggplot2::element_text(size = base_size * 0.9, color = "black"),
    axis.title = ggplot2::element_text(size = base_size),
    aspect.ratio = 1,
    plot.margin = ggplot2::margin(2, 2, 2, 2)
  )
  
  ordered_panels <- ts_panels[as.character(sort(regions_with_obs))]
  
  # domain-wide "WFS" panel fills the bottom-right cell (the 6th slot) instead of
  # leaving it a blank spacer -- reuses the SAME data/panel-building logic as the
  # standalone plot_domain_wide_biomass(), just embedded here rather than saved as its
  # own separate file
  if(include_domain_wide){
    domain_data <- tryCatch(
      .compute_domain_wide_data(run_dir, init_run_dir, group_token, species_pattern,
                                obs, domain_obs_pattern, cell_area_km2),
      error = function(e){
        message("  [note] could not build the domain-wide panel: ", conditionMessage(e),
                " -- leaving that cell blank instead.")
        NULL
      })
    if(!is.null(domain_data)){
      domain_data$ens_mean_df <- domain_ens_mean_df
      domain_data$ribbon_df   <- domain_ribbon_df
      pp_domain <- .build_domain_wide_panel(domain_data, init_run_dir, base_size,
                                            show_ensemble = show_ensemble)
      pp_domain <- pp_domain + ggplot2::theme(plot.margin = ggplot2::margin(2, 2, 2, 2), aspect.ratio = 1)
      ordered_panels <- c(ordered_panels, list("WFS" = pp_domain))
    }
  }
  n_panels <- length(ordered_panels)
  
  # Trim redundant axis titles across the 3x2 grid of time series panels: drop the
  # "Year" x-axis title on the top row (positions 1-3, since the bottom row's x-axis
  # already labels the shared year range), and keep the "Biomass (t)" y-axis title
  # only in the first column (positions 1 and 4) since every panel shares the same
  # y quantity.
  n_show <- min(n_panels, 6)
  for(i in seq_len(n_show)){
    row_i <- ((i - 1) %/% 3) + 1
    col_i <- ((i - 1) %% 3) + 1
    if(row_i == 1) ordered_panels[[i]] <- ordered_panels[[i]] + ggplot2::labs(x = NULL)
    if(col_i != 1) ordered_panels[[i]] <- ordered_panels[[i]] + ggplot2::labs(y = NULL)
  }
  
  # 5 cols x 2 rows: map = area(t=1,l=1,b=2,r=2) (2x2 block, bigger than a single time
  # series panel); the remaining 3x2 block on the right holds up to 6 panels in reading
  # order (row 1 left-to-right, then row 2) -- with include_domain_wide=TRUE, the WFS
  # panel added above lands in the LAST of these slots (bottom-right)
  if(n_panels > 6)
    message("plot_region_timeseries_grid(): grid layout has 6 slots but ", n_panels,
            " panels were built -- extra ones will overflow past the intended layout.")
  panel_cells <- list(
    patchwork::area(1, 3, 1, 3), patchwork::area(1, 4, 1, 4), patchwork::area(1, 5, 1, 5),
    patchwork::area(2, 3, 2, 3), patchwork::area(2, 4, 2, 4), patchwork::area(2, 5, 2, 5)
  )
  map_area <- patchwork::area(1, 1, 2, 2)
  design <- Reduce(c, c(list(map_area), panel_cells[seq_len(min(n_panels, 6))]))
  combined <- patchwork::wrap_plots(c(list(map_panel), ordered_panels[seq_len(min(n_panels, 6))]), design = design)
  
  if(is.null(png_file)) png_file <- paste0("region_timeseries_", gsub("[^A-Za-z0-9._-]+", "_", group_token), ".png")
  if(!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(plots_dir, png_file)
  # 5 cols x 2 rows, map spans 2 cols x 2 rows -- output width/height sized so a
  # single-panel "unit" is consistent between the 2x2 map block and the 1x1 time
  # series cells
  unit_size <- height / 2
  out_width  <- unit_size * 5
  out_height <- unit_size * 2
  ggplot2::ggsave(out_path, plot = combined, width = out_width, height = out_height, dpi = dpi, units = "in")
  message("Wrote ", out_path, " (", n_panels, " panel(s), ",
          round(out_width, 1), "x", round(out_height, 1), "in, 5x2 grid layout, map bigger, no arrows).")
  
  attr(agg, "run_dir") <- run_dir
  attr(agg, "init_run_dir") <- init_run_dir
  attr(agg, "group_token") <- group_token
  attr(agg, "n_regions_total") <- n_regions_total
  invisible(agg)
}



#' @title Plot domain-wide (whole study area) predicted vs. observed biomass.
#' @description Standalone companion to plot_region_timeseries_grid() -- shows the
#'   group's TOTAL biomass across the WHOLE modeled domain (every active cell, not
#'   restricted to any region), for best-fit (and optionally init), with observed
#'   data overlaid.
#'
#'   METHOD: for best-fit, sums density (t/km^2) x cell_area_km2 across every active
#'   cell in EcospaceMapBiomass-<group_token>.csv directly (same method
#'   aggregate_biomass_by_region() uses per-region, just unrestricted). The true
#'   domain area is derived from that same file's active-cell count, then reused to
#'   convert init_run_dir's Region-0 (density-only) export into a comparable total.
#' @param run_dir Best-fit candidate's output folder (must have
#'   EcospaceMapBiomass-<group_token>.csv archived).
#' @param init_run_dir Optional baseline run folder, for a dashed comparison line.
#' @param group_token Group token for the Ecospace map filename (e.g. "gag 0") --
#'   required, since this reads the spatial file directly. Use
#'   detect_map_group_tokens() if unsure.
#' @param species_pattern Used only for matching the group column in init_run_dir's
#'   Region-0 export. Falls back to `group_token` if not supplied.
#' @param obs Output of read_ewe_timeseries().
#' @param domain_obs_pattern Optional substring to filter which Region-0 observed
#'   series to show (e.g. "FWCNMFS"). NULL shows all.
#' @param cell_area_km2 Per-cell area, km^2. Default 9.26108^2.
#' @param base_size Base font size (pt). Default 18.
#' @param plots_dir,png_file,width,height,dpi Output folder/filename/figure dimensions/resolution.
#' @return Invisibly, the data.frame plotted (year, value, run), with
#'   attr(., "total_area_km2") recording the derived domain area (should be
#'   noticeably larger than the ~20472 km^2 covered by just the 9 age0 survey regions).
#' @export
#' @keywords internal
#' @noRd
# Computes domain-wide predicted + observed biomass for one group -- shared by
# plot_domain_wide_biomass() (standalone) and plot_region_timeseries_grid() (embedded).
.compute_domain_wide_data <- function(run_dir, init_run_dir, group_token, species_pattern,
                                      obs, domain_obs_pattern, cell_area_km2){
  path <- file.path(run_dir, paste0("EcospaceMapBiomass-", group_token, ".csv"))
  if(!file.exists(path))
    stop(".compute_domain_wide_data(): file not found: ", path)
  m <- read_ecospace_map(path)
  cell_data <- m$data[!is.na(m$data$value), , drop = FALSE]
  if(nrow(cell_data) == 0)
    stop(".compute_domain_wide_data(): every cell is NA in ", path, " -- nothing to sum.")
  domain_best <- stats::aggregate(value ~ year, data = cell_data, FUN = sum, na.rm = TRUE)
  domain_best$value <- domain_best$value * cell_area_km2
  domain_df <- data.frame(year = domain_best$year, value = domain_best$value,
                          run = "best-fit", stringsAsFactors = FALSE)
  
  active_cells <- length(unique(paste(cell_data$row, cell_data$col)))
  total_area_km2 <- active_cells * cell_area_km2
  message(".compute_domain_wide_data(): true domain area for '", group_token, "' = ",
          round(total_area_km2, 1), " km^2 (", active_cells, " active cell(s) in the ",
          "spatial export) -- compare against the ~20472 km^2 covered by just the 9 ",
          "age0 survey regions; the model domain should be noticeably larger.")
  
  match_pattern <- if(!is.null(species_pattern)) species_pattern else group_token
  if(!is.null(init_run_dir)){
    pr_init <- tryCatch(get_region_output(0, "Biomass", init_run_dir, new.env(parent = emptyenv())),
                        error = function(e) NULL)
    if(!is.null(pr_init)){
      dcols_i <- setdiff(names(pr_init$data), "year")
      dcol_i <- dcols_i[tolower(dcols_i) == tolower(match_pattern)]
      if(length(dcol_i) == 0) dcol_i <- dcols_i[grepl(match_pattern, dcols_i, ignore.case = TRUE)]
      if(length(dcol_i) > 0){
        domain_df <- rbind(domain_df, data.frame(year = pr_init$data$year,
                                                 value = pr_init$data[[dcol_i[1]]] * total_area_km2,
                                                 run = "init", stringsAsFactors = FALSE))
      } else message("  [note] init_run_dir's domain-wide Biomass file has no column matching '",
                     match_pattern, "'.")
    } else message("  [note] init_run_dir has no domain-wide (region 0) Biomass export -- best-fit only.")
  }
  
  domain_obs_rows <- which(as.character(obs$ts.head$Region) == "0")
  if(!is.null(domain_obs_pattern))
    domain_obs_rows <- domain_obs_rows[grepl(domain_obs_pattern, obs$ts.head$Title[domain_obs_rows], ignore.case = TRUE)]
  obs_df <- NULL
  if(length(domain_obs_rows) > 0){
    obs_list <- list()
    best_vals <- domain_df$value[domain_df$run == "best-fit"]
    best_years <- domain_df$year[domain_df$run == "best-fit"]
    for(r in domain_obs_rows){
      ov <- obs$ts[[obs$ts.head$Title[r]]]; oy <- obs$ts$year
      keep <- !is.na(ov)
      if(!any(keep)) next
      common <- intersect(oy[keep], best_years)
      if(length(common) >= 2){
        q <- mean(best_vals[match(common, best_years)]) / mean(ov[keep][match(common, oy[keep])])
        if(is.finite(q) && q > 0) ov <- ov * q
      }
      obs_list[[length(obs_list) + 1]] <- data.frame(year = oy[keep], value = ov[keep],
                                                     series = obs$ts.head$Title[r], stringsAsFactors = FALSE)
    }
    if(length(obs_list) > 0) obs_df <- do.call(rbind, obs_list)
  }
  
  list(domain_df = domain_df, obs_df = obs_df, total_area_km2 = total_area_km2)
}


#' @keywords internal
#' @noRd
# Builds the domain-wide ggplot panel from .compute_domain_wide_data()'s output --
# shared by plot_domain_wide_biomass() (standalone) and plot_region_timeseries_grid()
# (embedded).
.build_domain_wide_panel <- function(domain_data, init_run_dir, base_size, show_ensemble = FALSE){
  domain_df <- domain_data$domain_df
  obs_df <- domain_data$obs_df
  ens_mean_df <- domain_data$ens_mean_df
  ribbon_df   <- domain_data$ribbon_df
  pp <- ggplot2::ggplot()
  sub_init <- domain_df[domain_df$run == "init", , drop = FALSE]
  sub_best <- domain_df[domain_df$run == "best-fit", , drop = FALSE]
  if(show_ensemble && !is.null(ens_mean_df)){
    # ONLY the AIC-weighted domain-wide mean + its 95% CI ribbon -- black, since this
    # panel has no region-specific color the way individual region panels do. init
    # and best-fit are deliberately NOT drawn here, matching the per-region panels'
    # "ensemble mode shows only the weighted mean and its uncertainty" convention.
    if(!is.null(ribbon_df) && nrow(ribbon_df) > 0)
      pp <- pp + ggplot2::geom_ribbon(data = ribbon_df,
                                      ggplot2::aes(x = year, ymin = pmax(lo, 0), ymax = hi),
                                      fill = "black", alpha = 0.20, colour = NA)
    pp <- pp + ggplot2::geom_line(data = ens_mean_df, ggplot2::aes(x = year, y = value),
                                  color = "black", linewidth = 1.8)
  } else {
    if(nrow(sub_init) > 0)
      pp <- pp + ggplot2::geom_line(data = sub_init, ggplot2::aes(x = year, y = value, linetype = "init"),
                                    color = "black", linewidth = 1.4)
    pp <- pp + ggplot2::geom_line(data = sub_best, ggplot2::aes(x = year, y = value, linetype = "best-fit"),
                                  color = "black", linewidth = 1.8)
    pp <- pp + ggplot2::scale_linetype_manual(values = c("init" = "22", "best-fit" = "solid"), name = NULL)
  }
  if(!is.null(obs_df)){
    obs_df$series <- factor(obs_df$series)
    pp <- pp + ggplot2::geom_point(data = obs_df, ggplot2::aes(x = year, y = value, shape = series),
                                   color = "black", size = base_size * 0.18, stroke = 1)
    pp <- pp + ggplot2::scale_shape_manual(values = rep(0:25, length.out = nlevels(obs_df$series)))
  }
  pp <- pp + ggplot2::labs(title = "WFS", x = "Year", y = "Biomass (t)", shape = "Observed")
  pp <- pp + ggplot2::theme_bw(base_size = base_size)
  pp <- pp + ggplot2::theme(plot.title = ggplot2::element_text(size = base_size * 0.95, face = "bold",
                                                               hjust = 0.5, margin = ggplot2::margin(b = 0),
                                                               vjust = 0.2),
                            aspect.ratio = 1)
  # observed-series shape legend placed INSIDE the panel, top-left -- same convention
  # as every region panel in this figure. The init/best-fit LINETYPE legend is dropped
  # entirely here (not just restyled) -- its meaning is identical across every region
  # panel and doesn't need repeating on the domain-wide panel too.
  pp <- pp + ggplot2::guides(
    shape    = if(!is.null(obs_df)) ggplot2::guide_legend(position = "inside", title = NULL) else "none",
    linetype = "none"
  )
  pp <- pp + ggplot2::theme(
    legend.position.inside = c(0.02, 0.98),
    legend.justification.inside = c(0, 1),
    legend.key.size = grid::unit(0.5, "cm"),
    legend.text = ggplot2::element_text(size = base_size * 0.55),
    legend.background = ggplot2::element_rect(fill = scales::alpha("white", 0.65), colour = NA),
    legend.margin = ggplot2::margin(1, 2, 1, 2)
  )
  pp
}


#' @title Plot domain-wide (whole study area) predicted vs. observed biomass.
#' @description Standalone wrapper around .compute_domain_wide_data() +
#'   .build_domain_wide_panel() -- see plot_region_timeseries_grid()'s docs for the
#'   embedded version of this SAME panel (its bottom-right cell), which is generally
#'   preferable to a separate file when you're already building that combined figure.
#'   METHOD: for the best-fit run, sums density (t/km^2) x cell_area_km2 across EVERY
#'   active cell in EcospaceMapBiomass-<group_token>.csv directly -- the SAME reliable
#'   method aggregate_biomass_by_region() uses for individual regions, just not
#'   restricted to any region's cells. The TRUE domain area for THIS group is derived
#'   directly from that same spatial file (count of active cells x cell_area_km2), then
#'   reused to convert init_run_dir's Region-0 (density) export into a comparable total.
#' @param run_dir Best-fit candidate's output folder (must have
#'   EcospaceMapBiomass-<group_token>.csv archived).
#' @param init_run_dir Optional baseline run folder, for a dashed comparison line.
#' @param group_token Group token for the Ecospace map filename (e.g. "gag 0") --
#'   required, since this reads the actual spatial file. Use detect_map_group_tokens()
#'   if unsure, or reuse attr(region_ts, "group_token") from a prior
#'   plot_region_timeseries_grid() call.
#' @param species_pattern Used only for matching the group column in init_run_dir's
#'   Region-0 export. Falls back to `group_token` if not supplied.
#' @param obs Output of read_ewe_timeseries().
#' @param domain_obs_pattern Optional case-insensitive substring to filter WHICH
#'   Region-0 observed series to show (e.g. "FWCNMFS"). NULL (default) shows all.
#' @param cell_area_km2 Per-cell area, km^2. Default 9.26108^2.
#' @param base_size Base font size (pt). Default 18.
#' @param plots_dir,png_file,width,height,dpi Output folder/filename/figure dimensions
#'   (inches)/resolution.
#' @return Invisibly, the data.frame plotted (year, value, run), with
#'   attr(., "total_area_km2") recording the TRUE domain area used.
#' @export
plot_domain_wide_biomass <- function(run_dir, init_run_dir = NULL, group_token,
                                     species_pattern = NULL,
                                     obs, domain_obs_pattern = NULL,
                                     cell_area_km2 = 9.26108^2,
                                     base_size = 18, plots_dir = "plots", png_file = NULL,
                                     width = 8, height = 6, dpi = 300){
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
  if(missing(group_token) || is.null(group_token))
    stop("plot_domain_wide_biomass(): group_token is required (this reads the spatial ",
         "EcospaceMapBiomass-<group_token>.csv file directly) -- e.g. reuse ",
         "attr(region_ts, \"group_token\") from a prior plot_region_timeseries_grid() call.")
  
  domain_data <- .compute_domain_wide_data(run_dir, init_run_dir, group_token, species_pattern,
                                           obs, domain_obs_pattern, cell_area_km2)
  pp <- .build_domain_wide_panel(domain_data, init_run_dir, base_size)
  
  if(is.null(png_file)) png_file <- paste0("domain_wide_", gsub("[^A-Za-z0-9._-]+", "_", group_token), ".png")
  if(!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(plots_dir, png_file)
  ggplot2::ggsave(out_path, plot = pp, width = width, height = height, dpi = dpi, units = "in")
  message("Wrote ", out_path)
  
  attr(domain_data$domain_df, "total_area_km2") <- domain_data$total_area_km2
  invisible(domain_data$domain_df)
}


#' @title Find the real EcospaceMap<Var>-<token>.csv filename token(s) matching species_patterns.
#' @description Scans `run_folders` in order (stopping at the first one with a match) for
#'   files named "EcospaceMap<var>-<token>.csv", converts each token's underscores to
#'   spaces for display (e.g. "gag_0" -> "gag 0"), and returns every token whose display
#'   name matches any of `species_patterns`. Not every candidate folder has full spatial
#'   output archived, so this checks multiple folders rather than assuming the first one
#'   has it.
#' @param run_folders Character vector of candidate directories to check, in order.
#' @param var Variable name as it appears in the filename, e.g. "Biomass".
#' @param species_patterns Character vector of case-insensitive substrings to match
#'   against each token's display name.
#' @return data.frame(token, display) -- one row per matching file found in the first
#'   folder that had any match. Errors if no folder checked has a match.
#' @export
detect_map_group_tokens <- function(run_folders, var, species_patterns){
  for(rf in run_folders){
    files <- list.files(rf, pattern = paste0("^EcospaceMap", var, "-.*\\.csv$"))
    if(length(files) == 0) next
    toks <- sub(paste0("^EcospaceMap", var, "-(.*)\\.csv$"), "\\1", files)
    disp <- gsub("_", " ", toks)
    keep <- vapply(disp, function(nm)
      any(vapply(species_patterns, function(p) grepl(p, nm, ignore.case = TRUE), logical(1))),
      logical(1))
    if(any(keep))
      return(data.frame(token = toks[keep], display = disp[keep], stringsAsFactors = FALSE))
  }
  stop("detect_map_group_tokens(): no EcospaceMap", var, "-*.csv files matching species_patterns ",
       "= ", paste(species_patterns, collapse = ", "), " found in ANY of the ",
       length(run_folders), " folder(s) checked.")
}


#' @title Pool one variable's EcospaceMap file across MANY candidate folders: per-cell mean/sd/cv.
#' @description Scans `run_folders`, reads whichever ones actually have the needed
#'   EcospaceMap file(s) for `tok`. Two aggregation modes:
#'   \itemize{
#'     \item `weights = NULL` (default): per-cell mean/sd/cv POOLED ACROSS EVERY
#'       (candidate, year) combination found equally -- streamed as per-folder (n, sum,
#'       sum-of-squares) rather than holding every candidate's full data in memory at
#'       once, so this scales to hundreds of folders.
#'     \item `weights` supplied (named numeric vector, names = basename(run_folder)):
#'       each candidate's own per-cell mean is first collapsed across that candidate's
#'       own years, THEN these per-candidate means are weight-averaged across
#'       candidates using `weights` (e.g. AIC weights from select_ensemble_candidates()).
#'       This is deliberately different from the pooled default: pooling by (candidate,
#'       year) would implicitly let a candidate with more archived years count more,
#'       which has nothing to do with how well-supported that candidate actually is.
#'   }
#'   `var = "F"` is handled specially either way: reads both Biomass and Catch files and
#'   derives F = Catch/Biomass cell-by-cell (NA where Biomass <= 0), matching the same
#'   convention used elsewhere in this script.
#' @param run_folders Character vector of candidate directories to scan.
#' @param var Variable name as it appears in the filename (e.g. "Biomass"), or "F"
#'   (derived from Biomass + Catch).
#' @param tok The exact filename token, e.g. "gag_0" for EcospaceMapBiomass-gag_0.csv.
#' @param year_range Optional c(min_year, max_year) to restrict which years contribute.
#'   NULL (default) = every year in each file.
#' @param progress_every Print a progress message every N folders when running
#'   sequentially. 0 disables progress messages. Default 100.
#' @param n_cores Number of CPU cores for parallel folder reading (Mac/Linux only, via
#'   parallel::mclapply()). Default 1 (sequential). Falls back to sequential with a
#'   message if unavailable (Windows, or 'parallel' missing).
#' @param weights Optional named numeric vector (names = basename(run_folder), e.g.
#'   from select_ensemble_candidates()'s `ens_weight` column) triggering the weighted
#'   aggregation mode described above. NULL (default) = pooled, unweighted. Every
#'   folder actually read must have a matching entry, or this errors rather than
#'   silently dropping/reweighting candidates.
#' @return list(agg = data.table(row, col, lat, lon, mean_val, sd_val, cv_val), n_found
#'   = number of folders that actually had the needed file(s)). agg is NULL if no
#'   folder had it.
#' @export
compute_ensemble_map_stats <- function(run_folders, var, tok, year_range = NULL,
                                       progress_every = 100, n_cores = 1,
                                       calculate_variability = TRUE,
                                       weights = NULL){
  
  read_one <- function(rf){
    
    dat <- NULL
    
    if(var == "F"){
      
      bio_path <- file.path(
        rf,
        sprintf("EcospaceMapBiomass-%s.csv", tok)
      )
      
      cat_path <- file.path(
        rf,
        sprintf("EcospaceMapCatch-%s.csv", tok)
      )
      
      if(!file.exists(bio_path) || !file.exists(cat_path))
        return(NULL)
      
      mb <- tryCatch(
        read_ecospace_map(bio_path),
        error = function(e) NULL
      )
      
      mc <- tryCatch(
        read_ecospace_map(cat_path),
        error = function(e) NULL
      )
      
      if(is.null(mb) || is.null(mc))
        return(NULL)
      
      merged <- merge(
        mb$data,
        mc$data,
        by = c("year", "row", "col", "lat", "lon"),
        suffixes = c("_bio", "_cat")
      )
      
      merged$value <- ifelse(
        is.na(merged$value_bio) | merged$value_bio <= 0,
        NA_real_,
        merged$value_cat / merged$value_bio
      )
      
      dat <- merged[
        ,
        c("year", "row", "col", "lat", "lon", "value")
      ]
      
    } else {
      
      path <- file.path(
        rf,
        sprintf("EcospaceMap%s-%s.csv", var, tok)
      )
      
      if(!file.exists(path))
        return(NULL)
      
      mm <- tryCatch(
        read_ecospace_map(path),
        error = function(e) NULL
      )
      
      if(is.null(mm))
        return(NULL)
      
      dat <- mm$data
    }
    
    if(!is.null(year_range))
      dat <- dat[
        dat$year >= year_range[1] &
          dat$year <= year_range[2],
        ,
        drop = FALSE
      ]
    
    dt <- data.table::as.data.table(dat)
    
    dt <- dt[!is.na(value)]
    
    if(nrow(dt) == 0)
      return(NULL)
    
    # per-folder per-cell (n, sum, sum-of-squares) -- small (one row per grid cell), so
    # this is cheap to return/collect even for hundreds of folders. run_folder is
    # tagged on every row so weights (if supplied) can be joined in after stacking --
    # needed to weight by CANDIDATE (a per-folder property like AIC weight), not by
    # (folder, year) pair the way the unweighted pooled path implicitly does.
    
    if(calculate_variability){
      
      out <- dt[
        ,
        .(
          n = .N,
          s = sum(value),
          ss = sum(value^2)
        ),
        by = .(row, col, lat, lon)
      ]
      
    } else {
      
      # When no variability is requested, only collect n and sum.
      out <- dt[
        ,
        .(
          n = .N,
          s = sum(value)
        ),
        by = .(row, col, lat, lon)
      ]
    }
    
    out[, run_folder := basename(rf)]
    out
  }
  
  use_parallel <- n_cores > 1 &&
    .Platform$OS.type == "unix" &&
    requireNamespace("parallel", quietly = TRUE)
  
  if(use_parallel){
    
    message(
      "    running ",
      length(run_folders),
      " folder(s) across ",
      n_cores,
      " core(s)..."
    )
    
    per_folder_stats <- parallel::mclapply(
      run_folders,
      read_one,
      mc.cores = n_cores
    )
    
  } else {
    
    if(n_cores > 1)
      message(
        "    n_cores > 1 requested but parallel processing isn't available on this ",
        "platform (Windows, or the 'parallel' package is missing) -- running sequentially."
      )
    
    per_folder_stats <- vector(
      "list",
      length(run_folders)
    )
    
    for(i in seq_along(run_folders)){
      
      if(progress_every > 0 && i %% progress_every == 0)
        message(
          "    ...checked ",
          i,
          " of ",
          length(run_folders),
          " folder(s) so far."
        )
      
      per_folder_stats[[i]] <- read_one(
        run_folders[i]
      )
    }
  }
  
  per_folder_stats <- Filter(
    Negate(is.null),
    per_folder_stats
  )
  
  n_found <- length(per_folder_stats)
  
  if(n_found == 0)
    return(
      list(
        agg = NULL,
        n_found = 0
      )
    )
  
  allstats <- data.table::rbindlist(
    per_folder_stats
  )
  
  if(!is.null(weights)){
    
    # ---- WEIGHTED PATH: weight by CANDIDATE (e.g. AIC weight), not by (folder, year)
    # pair. Each folder's own per-cell mean is collapsed across its own years FIRST
    # (mean_folder = s/n, already what a single candidate "believes" for that cell,
    # regardless of how many years contributed to it), THEN these per-folder means are
    # weighted-averaged across folders using `weights`. This is deliberately different
    # from the unweighted path below, which pools every (folder, year) observation
    # equally -- that would implicitly let a candidate with more archived years count
    # more, which has nothing to do with how well-supported that candidate actually is.
    
    w_lookup <- weights[allstats$run_folder]
    if(any(is.na(w_lookup))){
      missing_folders <- unique(allstats$run_folder[is.na(w_lookup)])
      stop("compute_ensemble_map_stats(): `weights` has no entry for candidate folder(s): ",
           paste(missing_folders, collapse = ", "), ". Every folder actually read must have ",
           "a weight -- pass a complete named vector (names = basename(run_folder)).")
    }
    allstats[, w := w_lookup]
    allstats[, mean_folder := s / n]
    
    agg <- allstats[
      ,
      .(
        mean_val = stats::weighted.mean(mean_folder, w),
        n_candidates = .N
      ),
      by = .(row, col, lat, lon)
    ]
    
    if(calculate_variability){
      # weighted variance of the per-folder means around the weighted mean --
      # normalized so weights need not sum to 1 (standard weighted-variance form)
      merged_w <- merge(allstats, agg[, .(row, col, lat, lon, mean_val)],
                        by = c("row", "col", "lat", "lon"))
      merged_w[, sq_dev := w * (mean_folder - mean_val)^2]
      var_agg <- merged_w[, .(var_val = sum(sq_dev) / sum(w)), by = .(row, col, lat, lon)]
      agg <- merge(agg, var_agg, by = c("row", "col", "lat", "lon"))
      agg[, sd_val := sqrt(pmax(var_val, 0))]
      agg[, cv_val := ifelse(is.finite(mean_val) & mean_val > 0, sd_val / mean_val, NA_real_)]
    }
    
    message("compute_ensemble_map_stats(): AIC-weighted aggregation across ", n_found,
            " candidate(s) (each candidate's own per-cell mean weighted by its AIC weight, ",
            "not pooled by (candidate, year) pair).")
    
    return(list(agg = agg, n_found = n_found))
  }
  
  if(calculate_variability){
    
    agg <- allstats[
      ,
      .(
        n = sum(n),
        s = sum(s),
        ss = sum(ss)
      ),
      by = .(row, col, lat, lon)
    ]
    
    agg[, mean_val := s / n]
    
    agg[
      ,
      var_val := pmax(
        ss / n - mean_val^2,
        0
      )
    ]
    
    agg[
      ,
      sd_val := sqrt(var_val)
    ]
    
    agg[
      ,
      cv_val := ifelse(
        is.finite(mean_val) & mean_val > 0,
        sd_val / mean_val,
        NA_real_
      )
    ]
    
  } else {
    
    agg <- allstats[
      ,
      .(
        n = sum(n),
        s = sum(s)
      ),
      by = .(row, col, lat, lon)
    ]
    
    agg[
      ,
      mean_val := s / n
    ]
  }
  
  list(
    agg = agg,
    n_found = n_found
  )
}


#' @description Parses the metadata header (grid dimensions, cell size, top-left corner,
#'   start year), then every Step/Year block into a long-format data.frame. -9999 is
#'   converted to NA. All data rows across all year-blocks are parsed in a single
#'   data.table::fread() call rather than one strsplit()/as.numeric() call per row, which
#'   matters a lot when this is called thousands of times across an ensemble of
#'   candidate folders.
#' @param path Path to an EcospaceMap<Var>-<group_token>.csv file.
#' @param verbose Logical, default FALSE. TRUE prints a one-line summary after reading
#'   (group/variable/grid size/years) -- useful when reading a single file by hand, but
#'   left off by default since bulk ensemble functions call this thousands of times and
#'   per-file messages there just look like console spam (and, since the message only
#'   shows the filename not the folder, can look identical across different candidates
#'   reading the same-named file -- see compute_ensemble_map_stats() for the progress
#'   reporting used during bulk reads instead).
#' @return list(meta = list(rows, cols, cellsize, top_lat, top_lon, start_year,
#'   variable, group), data = data.frame(year, row, col, lat, lon, value) -- one row per
#'   active-or-inactive cell per year; inactive/no-data cells have value = NA).
read_ecospace_map <- function(path, verbose = FALSE){
  if(!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required. Install it with install.packages('data.table').")
  
  raw <- readLines(path, warn = FALSE)
  
  gv <- function(key){
    ln <- grep(paste0("^", key, ","), raw, value = TRUE)
    if(length(ln) == 0) return(NA_character_)
    sub(paste0("^", key, ","), "", ln[1])
  }
  meta <- list(
    rows       = as.integer(gv("MapRows")),
    cols       = as.integer(gv("MapCols")),
    cellsize   = as.numeric(gv("MapCellSize")),
    top_lat    = as.numeric(gv("MapTopLeftLat")),
    top_lon    = as.numeric(gv("MapTopLeftLon")),
    start_year = as.integer(gv("StartYear"))
  )
  if(any(vapply(meta, function(x) length(x) == 0 || is.na(x), logical(1))))
    stop("read_ecospace_map(): missing one or more required header fields (MapRows/",
         "MapCols/MapCellSize/MapTopLeftLat/MapTopLeftLon/StartYear) in ", path)
  
  var_idx <- grep("^Variable,", raw)
  if(length(var_idx) == 0) stop("read_ecospace_map(): no 'Variable,' line found in ", path)
  meta$variable <- sub("^Variable,", "", raw[var_idx[1]])
  grp_line <- raw[var_idx[1] + 1]
  meta$group <- gsub('^"|"$', "", gsub("^[^,]*,", "", grp_line))
  
  step_idx <- grep("^Step,", raw)
  n_blocks <- length(step_idx)
  if(n_blocks == 0) stop("read_ecospace_map(): no 'Step,' blocks found in ", path)
  
  row_idx <- rep(seq_len(meta$rows), times = meta$cols)
  col_idx <- rep(seq_len(meta$cols), each = meta$rows)
  lat <- meta$top_lat - (row_idx - 1) * meta$cellsize
  lon <- meta$top_lon + (col_idx - 1) * meta$cellsize
  
  yr_frac <- suppressWarnings(as.numeric(sub("^Year,", "", raw[step_idx + 1])))
  calendar_years <- meta$start_year + floor(yr_frac)
  
  # gather every data-row line index across ALL blocks and bulk-parse them in one
  # data.table::fread() call (vectorized C parser) instead of n_blocks separate
  # strsplit()/as.numeric() calls -- this is the dominant cost when reading hundreds of
  # these files across a GA ensemble
  data_starts <- step_idx + 2L
  all_line_idx <- unlist(lapply(data_starts, function(ds) ds:(ds + meta$rows - 1L)))
  all_lines <- raw[all_line_idx]
  big <- data.table::fread(text = paste(all_lines, collapse = "\n"), header = FALSE,
                           sep = ",", showProgress = FALSE)
  if(nrow(big) != n_blocks * meta$rows || ncol(big) != meta$cols)
    stop("read_ecospace_map(): expected ", n_blocks * meta$rows, " x ", meta$cols,
         " total data rows in ", path, " but got ", nrow(big), " x ", ncol(big))
  big_mat <- as.matrix(big)
  storage.mode(big_mat) <- "double"
  
  blocks <- vector("list", n_blocks)
  for(k in seq_len(n_blocks)){
    rr <- ((k - 1L) * meta$rows + 1L):(k * meta$rows)
    vals <- as.vector(big_mat[rr, , drop = FALSE])  # column-major == row-fastest, matches row_idx/col_idx
    vals[vals == -9999] <- NA_real_
    blocks[[k]] <- data.frame(year = calendar_years[k], row = row_idx, col = col_idx,
                              lat = lat, lon = lon, value = vals)
  }
  
  if(verbose)
    message("read_ecospace_map(): ", basename(path), " -- ", meta$group, " (", meta$variable,
            "), ", meta$rows, "x", meta$cols, " grid, ", n_blocks, " year(s) (",
            min(calendar_years), "-", max(calendar_years), ").")
  
  list(meta = meta, data = do.call(rbind, blocks))
}


#' @title Collapse a read_ecospace_map() time series to one value per cell.
#' @description Summarizes across all years present in `map_data` (i.e. across the whole
#'   time span in the file, or whatever subset of years you've already filtered to).
#' @param map_data The `data` element of read_ecospace_map()'s return value (or a
#'   year-filtered subset of it).
#' @param stat "mean", "sd" (temporal standard deviation), or "cv" (coefficient of
#'   variation = sd/mean, undefined -> NA where mean <= 0).
#' @return data.frame(row, col, lat, lon, value) with one row per cell.
summarize_ecospace_map <- function(map_data, stat = c("mean", "sd", "cv")){
  stat <- match.arg(stat)
  dt <- data.table::as.data.table(map_data)
  agg <- dt[, .(mean_val = mean(value, na.rm = TRUE), sd_val = stats::sd(value, na.rm = TRUE)),
            by = .(row, col, lat, lon)]
  agg[, cv_val := ifelse(is.finite(mean_val) & mean_val > 0, sd_val / mean_val, NA_real_)]
  out <- switch(stat,
                mean = agg[, .(row, col, lat, lon, value = mean_val)],
                sd   = agg[, .(row, col, lat, lon, value = sd_val)],
                cv   = agg[, .(row, col, lat, lon, value = cv_val)])
  as.data.frame(out)
}


#' @title Plot one cell-level spatial summary as a raster map.
#' @param map_summary Output of summarize_ecospace_map() (or any data.frame with
#'   lon/lat/value columns, plus row/col if `depth_grid` is supplied).
#' @param title,value_lab Plot title and fill-legend label.
#' @param log_scale Logical, default FALSE. TRUE pre-transforms the plotted values to
#'   log(value + 1) and updates `value_lab` to say so.
#' @param show_land Logical, default TRUE. Draws a grey background layer for cells not
#'   covered by `map_summary`.
#' @param land_mask Optional data.frame from read_esri_ascii() (e.g. on
#'   combined_regions_5min.asc) identifying TRUE land cells specifically -- when
#'   supplied, only actual land is shaded grey, and water cells that simply lack data
#'   for this particular plot stay white. NULL (default) falls back to the older
#'   .full_domain_grid() behavior (whole domain grid shaded grey).
#' @param depth_grid Optional data.frame from read_depth_grid() (row, col, lat, lon,
#'   depth). When supplied, cells deeper than `depth_threshold` are shown plain white
#'   instead of colored by value -- deep-water predictions are typically far less
#'   reliable than shelf predictions in this model. NULL (default) colors every cell.
#' @param depth_threshold Depth (m) cutoff for the shallow/deep split. Default 500.
#' @param show_x_axis,show_y_axis Logical, default TRUE. FALSE hides that axis's
#'   title/text/ticks entirely -- for shared-axis faceted layouts (e.g. only the
#'   leftmost column needs a y-axis, only the bottom row needs an x-axis).
#' @param lon_breaks,lat_breaks Optional explicit numeric vectors of longitude/latitude
#'   break values. When supplied, draws dashed gridlines at exactly these breaks
#'   (replacing theme_bw()'s default grid) instead of ggplot's automatic breaks. NULL
#'   (default) uses ggplot's own automatic breaks with no explicit gridlines.
#' @param inside_legend Logical, default FALSE. TRUE moves the fill legend inside the
#'   panel (bottom-left by default, via `legend_position`), framed, with the title
#'   above the bar -- matching the convention used across this project's other inside
#'   legends. FALSE (default) keeps ggplot2's normal outside-the-panel legend.
#' @param legend_position Only used when `inside_legend = TRUE`. c(x, y) in [0,1],
#'   default c(0.02, 0.02) (bottom-left).
#' @param barheight,barwidth Only used when `inside_legend = TRUE`. Colorbar dimensions
#'   in cm. Defaults 1.7/0.3.
#' @param fixed_aspect Logical, default TRUE. TRUE sets aspect.ratio=1, forcing a
#'   square panel. FALSE skips this, letting coord_quickmap() (already correct for
#'   geographic aspect on its own) determine the panel's shape and letting it fill
#'   whatever cell width is allocated to it -- needed when combining panels side by
#'   side (e.g. via patchwork's `|`) and controlling the gap between them with
#'   plot.margin: a fixed aspect.ratio forces the panel to a specific size independent
#'   of its cell, and ggplot2 centers that fixed-size panel within the cell, which
#'   silently absorbs any plot.margin change since the panel's own size never responds
#'   to it.
#' @return A ggplot object (not saved -- combine/save yourself, or use
#'   plot_ecospace_group_maps() for the save-to-PNG convenience wrapper).
plot_ecospace_map <- function(map_summary, title = "", value_lab = "Value", log_scale = FALSE,
                              show_land = TRUE, land_mask = NULL,
                              depth_grid = NULL, depth_threshold = 500,
                              show_x_axis = TRUE, show_y_axis = TRUE,
                              lon_breaks = NULL, lat_breaks = NULL,
                              inside_legend = FALSE, legend_position = c(0.02, 0.02),
                              barheight = 1.7, barwidth = 0.3,
                              title_inside = TRUE, base_size = 11, fixed_aspect = TRUE){
  
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
  
  if(log_scale){
    map_summary$value <- log(map_summary$value + 1)
    value_lab <- paste0("log(", value_lab, "+1)")
  }
  
  # ------------------------------------------------------------
  # DEPTH-BASED SHALLOW/DEEP SPLIT
  # ------------------------------------------------------------
  
  data_deep <- NULL
  
  if(!is.null(depth_grid)){
    
    if(!all(c("row", "col") %in% names(map_summary)))
      stop(
        "plot_ecospace_map(): depth_grid was supplied but map_summary has no row/col ",
        "columns to merge on."
      )
    
    merged <- merge(
      map_summary,
      depth_grid[, c("row", "col", "depth")],
      by = c("row", "col"),
      all.x = TRUE
    )
    
    data_deep <- merged[
      !is.na(merged$depth) &
        merged$depth > depth_threshold,
      ,
      drop = FALSE
    ]
    
    map_summary <- merged[
      is.na(merged$depth) |
        merged$depth <= depth_threshold,
      ,
      drop = FALSE
    ]
  }
  
  # ------------------------------------------------------------
  # LAND
  # ------------------------------------------------------------
  
  p <- ggplot2::ggplot()
  
  land_df <- NULL
  
  if(show_land){
    
    land_df <- if(!is.null(land_mask))
      .land_cells_from_mask(land_mask)
    else
      .full_domain_grid()
    
    p <- p +
      ggplot2::geom_raster(
        data = land_df,
        ggplot2::aes(
          x = lon,
          y = lat
        ),
        fill = "grey65"
      )
  }
  
  # ------------------------------------------------------------
  # MAP RASTER
  # ------------------------------------------------------------
  
  p <- p +
    ggplot2::geom_raster(
      data = map_summary,
      ggplot2::aes(
        x = lon,
        y = lat,
        fill = value
      )
    )
  
  if(!is.null(data_deep) && nrow(data_deep) > 0)
    p <- p +
    ggplot2::geom_raster(
      data = data_deep,
      ggplot2::aes(
        x = lon,
        y = lat
      ),
      fill = "white"
    )
  
  # ------------------------------------------------------------
  # GRIDLINES
  # Explicit dashed gridlines at fixed breaks
  # ------------------------------------------------------------
  
  if(!is.null(lon_breaks))
    p <- p +
    ggplot2::geom_vline(
      xintercept = lon_breaks,
      colour = scales::alpha("grey25", 0.40),
      linetype = "dashed",
      linewidth = 0.35
    )
  
  if(!is.null(lat_breaks))
    p <- p +
    ggplot2::geom_hline(
      yintercept = lat_breaks,
      colour = scales::alpha("grey25", 0.40),
      linetype = "dashed",
      linewidth = 0.35
    )
  
  # ------------------------------------------------------------
  # FILL LEGEND
  # ------------------------------------------------------------
  
  fill_guide <- if(inside_legend){
    
    ggplot2::guide_colorbar(
      position = "inside",
      frame.colour = "black",
      frame.linewidth = 0.6,
      ticks.colour = "black",
      title.position = "top",
      barheight = grid::unit(
        barheight,
        "cm"
      ),
      barwidth = grid::unit(
        barwidth,
        "cm"
      )
    )
    
  } else {
    
    ggplot2::guide_colorbar()
    
  }
  
  # ------------------------------------------------------------
  # SCALES AND COORDINATES
  #
  # lon_breaks and lat_breaks control only the axis breaks.
  # The actual map extent is determined by the raster itself.
  # ------------------------------------------------------------
  
  p <- p +
    ggplot2::scale_fill_viridis_c(
      name = value_lab,
      na.value = "grey90",
      breaks = if(is.null(lon_breaks))
        scales::pretty_breaks(n = 5)
      else
        ggplot2::waiver(),
      guide = fill_guide
    ) +
    ggplot2::scale_x_continuous(
      breaks = lon_breaks,
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = lat_breaks,
      expand = c(0, 0)
    ) +
    ggplot2::coord_quickmap(
      expand = FALSE
    ) +
    ggplot2::labs(
      title = if(title_inside) NULL else title,
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme_bw(
      base_size = base_size
    ) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      
      # constants recalibrated so base_size=11 exactly reproduces
      # the previous hardcoded values
      plot.title = ggplot2::element_text(
        size = base_size * 0.909,
        face = "bold",
        hjust = 0.5
      ),
      
      aspect.ratio = if(fixed_aspect)
        1
      else
        NULL,
      
      axis.title.x = if(show_x_axis)
        ggplot2::element_text()
      else
        ggplot2::element_blank(),
      
      axis.text.x = if(show_x_axis)
        ggplot2::element_text()
      else
        ggplot2::element_blank(),
      
      axis.ticks.x = if(show_x_axis)
        ggplot2::element_line()
      else
        ggplot2::element_blank(),
      
      axis.title.y = if(show_y_axis)
        ggplot2::element_text()
      else
        ggplot2::element_blank(),
      
      axis.text.y = if(show_y_axis)
        ggplot2::element_text()
      else
        ggplot2::element_blank(),
      
      axis.ticks.y = if(show_y_axis)
        ggplot2::element_line()
      else
        ggplot2::element_blank()
    )
  
  # ------------------------------------------------------------
  # INSIDE LEGEND
  # ------------------------------------------------------------
  
  if(inside_legend)
    p <- p +
    ggplot2::theme(
      legend.position.inside = legend_position,
      legend.justification.inside =
        if(legend_position[2] < 0.5)
          c(0, 0)
      else
        c(0, 1),
      
      legend.title = ggplot2::element_text(
        hjust = 0,
        size = base_size * 0.818
      ),
      
      legend.background =
        ggplot2::element_rect(
          fill = scales::alpha("white", 0.5),
          colour = NA
        ),
      
      legend.margin =
        ggplot2::margin(
          1,
          1,
          1,
          1
        )
    )
  
  # ------------------------------------------------------------
  # TITLE INSIDE THE MAP
  #
  # No title is drawn when title = "".
  # ------------------------------------------------------------
  
  if(title_inside && nzchar(title)){
    
    lon_all <- c(
      map_summary$lon,
      if(!is.null(land_df)) land_df$lon else NULL
    )
    
    lat_all <- c(
      map_summary$lat,
      if(!is.null(land_df)) land_df$lat else NULL
    )
    
    x_center <- mean(
      range(
        lon_all,
        na.rm = TRUE
      )
    )
    
    y_top <- max(
      lat_all,
      na.rm = TRUE
    )
    
    p <- p +
      ggplot2::annotate(
        "text",
        x = x_center,
        y = y_top,
        label = title,
        hjust = 0.5,
        vjust = 1,
        size = base_size * 0.364,
        fontface = "bold"
      )
  }
  
  p
}
#' @description Combines mean|variability pairs (2 columns per stanza) into rows of
#'   `stanzas_per_row` stanzas each (default 2). Each stanza's mean+CV pair shares ONE
#'   title above both panels (e.g. "red grouper 1"), with small "Biomass"/"CV" labels
#'   inset directly in the top-right corner of each individual map. Each panel gets its
#'   own color range (a shared scale across stanzas of very different magnitude would
#'   wash out the smaller ones).
#' @param map_root_dir Directory containing one subfolder per candidate.
#' @param run_folders Optional character vector of specific subfolder paths. NULL
#'   (default) = all immediate subfolders of `map_root_dir`.
#' @param species_patterns Character vector of case-insensitive substrings, e.g.
#'   c("gag", "red grouper"). Each pattern gets its own separate output.
#' @param vars Character vector of variable tokens, e.g. c("Biomass", "Catch", "F").
#' @param variability Either "cv" (default) or "sd" for the second panel.
#' @param year_range Optional c(min_year, max_year) to restrict years. NULL = all.
#' @param max_folders Safety cap on candidate folders scanned per group. Default 200.
#' @param n_cores Number of CPU cores for parallel folder reading (Mac/Linux only, via
#'   parallel::mclapply()). Default 1 (sequential). Falls back to sequential with a
#'   message if unavailable (Windows, or 'parallel' missing).
#' @param land_mask,depth_grid,depth_threshold,lon_breaks,lat_breaks Passed through to
#'   plot_ecospace_map() for each panel.
#' @param stanzas_per_row Number of stanzas (each a mean|variability pair-unit) placed
#'   in each row. Default 2.
#' @param barheight,barwidth Gradient colorbar size (cm), passed to plot_ecospace_map().
#' @param plots_dir Output folder for PNGs. Default "plots".
#' @param width,height,dpi ggsave() figure dimensions (inches) and resolution (dpi).
#'   Both scale automatically with the resulting grid size if left NULL.
#' @return Invisibly, a data.frame summarizing what was plotted: one row per (variable,
#'   species) with variable, species, n_stanzas_plotted, png_file.
#' @export
plot_ecospace_ensemble_maps_paired <- function(map_root_dir,
                                               ga_runs          = NULL,
                                               ens_mode         = c("quantile", "topN", "aic"),
                                               keep_fitness_quantile = 0.9,
                                               top_n            = 100,
                                               top_n_prop       = NULL,
                                               target_ess       = 100,
                                               run_folders      = NULL,
                                               species_patterns = c("gag", "red grouper"),
                                               vars             = c("Biomass", "Catch", "F"),
                                               variability      = NULL,
                                               log_scale        = TRUE,
                                               year_range       = NULL,
                                               max_folders      = 200,
                                               n_cores          = 1,
                                               land_mask        = NULL,
                                               depth_grid       = NULL,
                                               depth_threshold  = 500,
                                               lon_breaks       = NULL,
                                               lat_breaks       = NULL,
                                               stanzas_per_row  = 3,
                                               barheight        = 2.8,
                                               barwidth         = 0.45,
                                               plots_dir        = "plots",
                                               width = NULL, height = NULL, dpi = 150){
  
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
  if(!requireNamespace("patchwork", quietly = TRUE))
    stop("Package 'patchwork' is required. Install it with install.packages('patchwork').")
  if(!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required. Install it with install.packages('data.table').")
  
  ens_mode <- match.arg(ens_mode)
  
  if(!is.null(variability))
    variability <- match.arg(variability, c("cv", "sd"))
  
  if(is.null(run_folders))
    run_folders <- list.dirs(map_root_dir, recursive = FALSE, full.names = TRUE)
  
  if(length(run_folders) == 0)
    stop("No subfolders found under map_root_dir: ", map_root_dir)
  
  message("plot_ecospace_ensemble_maps_paired(): ", length(run_folders),
          " candidate folder(s) found under ", map_root_dir, ".")
  
  # ------------------------------------------------------------
  # ENSEMBLE SELECTION
  # ------------------------------------------------------------
  
  if(is.null(ga_runs))
    stop("ga_runs must be supplied when using ens_mode.")
  
  valid <- ga_runs[ga_runs$exists, , drop = FALSE]
  
  sel <- select_ensemble_candidates(
    valid,
    mode = ens_mode,
    keep_fitness_quantile = keep_fitness_quantile,
    top_n = top_n,
    top_n_prop = top_n_prop,
    target_ess = target_ess
  )
  
  weights_by_folder <-
    if(ens_mode == "aic")
      stats::setNames(sel$ens_weight, sel$run_folder)
  else
    NULL
  
  # restrict to the selected candidates
  # for "aic" mode this is every candidate,
  # with weights applied later if needed
  run_folders <- run_folders[
    basename(run_folders) %in% sel$run_folder
  ]
  
  if(length(run_folders) == 0)
    stop(
      "No subfolders under map_root_dir matched the selected candidates ",
      "(ens_mode = '", ens_mode, "')."
    )
  
  message(
    "plot_ecospace_ensemble_maps_paired(): ",
    length(run_folders),
    " candidate folder(s) selected (ens_mode = '",
    ens_mode,
    "') under ",
    map_root_dir,
    "."
  )
  
  if(length(run_folders) > max_folders){
    run_folders <- sample(run_folders, max_folders)
    message(
      "  subsampling to ",
      max_folders,
      " folder(s) for performance ",
      "(raise max_folders, e.g. to 910, to use more/all of them)."
    )
  }
  
  if(!dir.exists(plots_dir))
    dir.create(
      plots_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  
  safe_name <- function(s)
    gsub("[^A-Za-z0-9._-]+", "_", s)
  
  var_lab_2nd <- if(is.null(variability))
    NULL
  else if(variability == "cv")
    "CV"
  else
    "SD"
  
  bio_tokens <- NULL
  
  if(any(vars %in% c("Biomass", "F")))
    bio_tokens <- detect_map_group_tokens(
      run_folders,
      "Biomass",
      species_patterns
    )
  
  summary_rows <- list()
  
  for(var in vars){
    
    tokens <- if(var %in% c("Biomass", "F"))
      bio_tokens
    else
      detect_map_group_tokens(
        run_folders,
        var,
        species_patterns
      )
    
    tokens$species <- vapply(
      tokens$display,
      function(nm){
        hit <- species_patterns[
          vapply(
            species_patterns,
            function(p)
              grepl(
                p,
                nm,
                ignore.case = TRUE
              ),
            logical(1)
          )
        ]
        
        if(length(hit) == 0)
          NA_character_
        else
          hit[1]
      },
      character(1)
    )
    
    tokens <- tokens[
      !is.na(tokens$species),
      ,
      drop = FALSE
    ]
    
    for(sp in unique(tokens$species)){
      
      grp_rows <- tokens[
        tokens$species == sp,
        ,
        drop = FALSE
      ]
      
      n_stanzas_total <- nrow(grp_rows)
      n_rows_used <- ceiling(
        n_stanzas_total / stanzas_per_row
      )
      
      panels <- list()
      n_used <- integer(0)
      
      for(ti in seq_len(nrow(grp_rows))){
        
        tok  <- grp_rows$token[ti]
        disp <- grp_rows$display[ti]
        
        # stanza's position in the row/col grid -- y-axis only on the first column of
        # each row, x-axis only on the last row
        stanza_row <- ceiling(
          ti / stanzas_per_row
        )
        
        stanza_col_in_row <- ((ti - 1) %% stanzas_per_row) + 1
        
        show_y_axis <- (
          stanza_col_in_row == 1
        )
        
        show_x_axis <- (
          stanza_row == n_rows_used
        )
        
        res <- compute_ensemble_map_stats(
          run_folders,
          var,
          tok,
          year_range,
          n_cores = n_cores,
          weights = weights_by_folder
        )
        
        message(
          "  [",
          var,
          " / ",
          disp,
          "] ",
          res$n_found,
          " of ",
          length(run_folders),
          " folder(s) had the needed file(s)."
        )
        
        if(res$n_found == 0)
          next
        
        agg <- res$agg
        
        n_used <- c(
          n_used,
          res$n_found
        )
        
        mean_df <- as.data.frame(
          agg[, .(
            row,
            col,
            lat,
            lon,
            value = mean_val
          )]
        )
        
        # ------------------------------------------------------------
        # MEAN PANEL
        # ------------------------------------------------------------
        
        mean_panel <- plot_ecospace_map(
          mean_df,
          title = "",
          value_lab = var,
          log_scale = log_scale,
          land_mask = land_mask,
          depth_grid = depth_grid,
          depth_threshold = depth_threshold,
          show_x_axis = show_x_axis,
          show_y_axis = show_y_axis,
          lon_breaks = lon_breaks,
          lat_breaks = lat_breaks,
          inside_legend = TRUE,
          barheight = barheight,
          barwidth = barwidth,
          fixed_aspect = FALSE
        )
        
        # ------------------------------------------------------------
        # TITLE ABOVE THE MAP(S)
        # ------------------------------------------------------------
        
        title_grob <- patchwork::wrap_elements(
          grid::textGrob(
            disp,
            x = grid::unit(
              0.5,
              "npc"
            ),
            y = grid::unit(
              0.40,
              "npc"
            ),
            gp = grid::gpar(
              fontsize = 16,
              fontface = "bold"
            )
          ),
          clip = FALSE
        )
        
        # ------------------------------------------------------------
        # WITH VARIABILITY
        # ------------------------------------------------------------
        
        if(!is.null(variability)){
          
          var_df <- as.data.frame(
            agg[, .(
              row,
              col,
              lat,
              lon,
              value =
                if(variability == "cv")
                  cv_val
              else
                sd_val
            )]
          )
          
          var_panel <- plot_ecospace_map(
            var_df,
            title = "",
            value_lab = var_lab_2nd,
            log_scale = FALSE,
            land_mask = land_mask,
            depth_grid = depth_grid,
            depth_threshold = depth_threshold,
            show_x_axis = show_x_axis,
            show_y_axis = FALSE,
            lon_breaks = lon_breaks,
            lat_breaks = lat_breaks,
            inside_legend = TRUE,
            barheight = barheight,
            barwidth = barwidth,
            fixed_aspect = FALSE
          )
          
          # ------------------------------------------------------------
          # REDUCE SPACE BETWEEN THE TWO MAPS
          # ------------------------------------------------------------
          
          mean_panel <- mean_panel +
            ggplot2::theme(
              plot.margin =
                ggplot2::margin(
                  4,
                  0,
                  4,
                  8
                )
            )
          
          var_panel <- var_panel +
            ggplot2::theme(
              plot.margin =
                ggplot2::margin(
                  4,
                  8,
                  4,
                  0
                )
            )
          
          # ------------------------------------------------------------
          # VARIABLE LABELS INSIDE EACH MAP
          # ------------------------------------------------------------
          
          mean_label <- grid::textGrob(
            var,
            x = grid::unit(
              0.80,
              "npc"
            ),
            y = grid::unit(
              0.90,
              "npc"
            ),
            hjust = 0.5,
            vjust = 0.5,
            gp = grid::gpar(
              fontsize = 11,
              fontface = "bold"
            )
          )
          
          var_label <- grid::textGrob(
            var_lab_2nd,
            x = grid::unit(
              0.80,
              "npc"
            ),
            y = grid::unit(
              0.90,
              "npc"
            ),
            hjust = 0.5,
            vjust = 0.5,
            gp = grid::gpar(
              fontsize = 11,
              fontface = "bold"
            )
          )
          
          mean_panel <- (
            mean_panel +
              patchwork::inset_element(
                mean_label,
                left = 0.72,
                bottom = 0.855,
                right = 0.88,
                top = 0.945,
                align_to = "panel",
                clip = FALSE
              )
          ) &
            ggplot2::theme(
              panel.background =
                ggplot2::element_rect(
                  fill = "transparent",
                  colour = NA
                ),
              plot.background =
                ggplot2::element_rect(
                  fill = "transparent",
                  colour = NA
                )
            )
          
          var_panel <- (
            var_panel +
              patchwork::inset_element(
                var_label,
                left = 0.72,
                bottom = 0.855,
                right = 0.88,
                top = 0.945,
                align_to = "panel",
                clip = FALSE
              )
          ) &
            ggplot2::theme(
              panel.background =
                ggplot2::element_rect(
                  fill = "transparent",
                  colour = NA
                ),
              plot.background =
                ggplot2::element_rect(
                  fill = "transparent",
                  colour = NA
                )
            )
          
          # ------------------------------------------------------------
          # COMBINE TITLE + MEAN + VARIABILITY
          # ------------------------------------------------------------
          
          pair_unit <- (
            title_grob /
              (mean_panel | var_panel)
          ) +
            patchwork::plot_layout(
              heights = c(
                0.09,
                1
              )
            ) &
            ggplot2::theme(
              plot.margin =
                ggplot2::margin(
                  0,
                  0,
                  1,
                  0
                )
            )
          
        } else {
          
          # ------------------------------------------------------------
          # NO VARIABILITY
          #
          # Only the mean map is plotted. The stanza title is retained
          # above the map. Margin is now symmetric (2 on every side) --
          # the previous asymmetric margin(0,-6,4,-6) was a workaround for
          # the width formula leaving extra horizontal whitespace per
          # panel (see the width calculation above); now that panel width
          # matches the map's actual geographic aspect, that whitespace
          # excess is gone, so a small, equal margin on all sides gives
          # equal visual spacing between columns and between rows,
          # rather than needing an uneven margin to compensate for it.
          # ------------------------------------------------------------
          
          pair_unit <- (
            title_grob /
              mean_panel
          ) +
            patchwork::plot_layout(
              heights = c(
                0.09,
                1
              )
            ) &
            ggplot2::theme(
              plot.margin =
                ggplot2::margin(
                  2,
                  2,
                  2,
                  2
                )
            )
        }
        
        panels[[length(panels) + 1]] <- pair_unit
      }
      
      if(length(panels) == 0){
        message(
          "  [skip] ",
          var,
          " '",
          sp,
          "': no stanza produced data."
        )
        next
      }
      
      # panels is now ONE pair-unit per stanza.
      n_stanzas <- length(panels)
      rows_used <- ceiling(
        n_stanzas / stanzas_per_row
      )
      
      # With variability, each stanza contains two maps and therefore
      # requires approximately twice the width of the no-variability case.
      #
      # NO-VARIABILITY WIDTH: 3.45 (not 4.0) is derived from this model's actual
      # geographic aspect, not a visual guess -- coord_quickmap()'s enforced aspect for
      # this 66x78 grid (lat_range=5.5deg, lon_range=6.5deg, mean_lat~27.75N) gives a
      # map width:height ratio of ~1.046:1. With title_grob taking 0.09 of the pair
      # unit's total height (map gets 1/1.09 of it), a 3.6in-tall unit gives the map
      # itself ~3.30in of height, so it needs ~3.30*1.046 = 3.45in of width to exactly
      # fill its cell with no leftover whitespace. The previous 4.0in left ~0.55in of
      # excess width per panel, centered as whitespace on both sides of the map --
      # this, not an inter-panel spacing setting, is what was making the gap BETWEEN
      # columns look wider than the gap between rows.
      if(is.null(width)){
        
        w <- if(is.null(variability))
          max(
            3.5,
            stanzas_per_row * 3.45
          )
        else
          max(
            7,
            stanzas_per_row * 7.2
          )
        
      } else {
        w <- width
      }
      
      h <- if(is.null(height))
        max(
          3.5,
          rows_used * 3.6
        )
      else
        height
      
      p <- patchwork::wrap_plots(
        panels,
        ncol = stanzas_per_row
      )
      
      png_file <- file.path(
        plots_dir,
        paste0(
          var,
          "_ensemble_paired_",
          if(!is.null(variability))
            variability
          else
            "mean",
          "_",
          safe_name(sp),
          "_",
          ens_mode,
          ".png"
        )
      )
      
      ggplot2::ggsave(
        png_file,
        plot = p,
        width = w,
        height = h,
        dpi = dpi,
        units = "in"
      )
      
      summary_rows[[length(summary_rows) + 1]] <-
        data.frame(
          variable = var,
          species = sp,
          n_stanzas_plotted = n_stanzas,
          n_candidates = if(length(n_used) > 0)
            max(n_used)
          else
            0,
          ens_mode = ens_mode,
          png_file = png_file,
          stringsAsFactors = FALSE
        )
    }
  }
  
  message(
    "Wrote ",
    length(summary_rows),
    " PNG(s) to ",
    normalizePath(
      plots_dir,
      mustWork = FALSE
    )
  )
  
  invisible(
    do.call(
      rbind,
      summary_rows
    )
  )
}
#' @title Mean-only ensemble maps (no CV/SD panel), in a fixed number of rows, all in a
#'   single PNG.
#' @description Same pipeline as plot_ecospace_ensemble_maps_paired() (same file
#'   discovery, same compute_ensemble_map_stats() call, same plot_ecospace_map() styling
#'   -- including fixed_aspect = FALSE and the same axis/legend logic), but drops the
#'   variability (CV/SD) panel entirely: each stanza gets ONE map (the mean), not a
#'   mean|CV pair, so the grid is `stanzas_per_row` single maps per row instead of
#'   `stanzas_per_row` pair-units. Use this when you only want Biomass/Catch/F mean
#'   maps and don't need the variability panel next to them.
#' @param map_root_dir Directory containing one subfolder per candidate.
#' @param run_folders Optional character vector of specific subfolder paths. NULL
#'   (default) = all immediate subfolders of `map_root_dir`.
#' @param species_patterns Character vector of case-insensitive substrings, e.g.
#'   c("gag", "red grouper"). Each pattern gets its own separate output.
#' @param vars Character vector of variable tokens, e.g. c("Biomass", "Catch", "F").
#' @param log_scale Passed to plot_ecospace_map() for the mean panel.
#' @param year_range Optional c(min_year, max_year) to restrict years. NULL = all.
#' @param max_folders Safety cap on candidate folders scanned per group. Default 200.
#' @param n_cores Number of CPU cores for parallel folder reading (Mac/Linux only, via
#'   parallel::mclapply()). Default 1 (sequential). Falls back to sequential with a
#'   message if unavailable (Windows, or 'parallel' missing).
#' @param land_mask,depth_grid,depth_threshold,lon_breaks,lat_breaks Passed through to
#'   plot_ecospace_map() for each panel.
#' @param stanzas_per_row Number of single-map stanzas placed in each row. Default 3
#'   (a single map is about half the width of a mean|CV pair-unit, so a wider default
#'   than the paired function's 2 keeps the same rough total figure width).
#' @param barheight,barwidth Gradient colorbar size (cm), passed to plot_ecospace_map().
#' @param plots_dir Output folder for PNGs. Default "plots".
#' @param width,height,dpi ggsave() figure dimensions (inches) and resolution (dpi).
#'   Both scale automatically with the resulting grid size if left NULL.
#' @return Invisibly, a data.frame summarizing what was plotted: one row per (variable,
#'   species) with variable, species, n_stanzas_plotted, png_file.
#' @export
# plot_ecospace_ensemble_maps_single <- function(map_root_dir,
#                                                run_folders      = NULL,
#                                                species_patterns = c("gag", "red grouper"),
#                                                vars             = c("Biomass", "Catch", "F"),
#                                                log_scale        = TRUE,
#                                                year_range       = NULL,
#                                                max_folders      = 200,
#                                                n_cores          = 1,
#                                                land_mask        = NULL,
#                                                depth_grid       = NULL,
#                                                depth_threshold  = 500,
#                                                lon_breaks       = NULL,
#                                                lat_breaks       = NULL,
#                                                stanzas_per_row  = 3,
#                                                barheight        = 2.8,
#                                                barwidth         = 0.45,
#                                                plots_dir        = "plots",
#                                                width = NULL, height = NULL, dpi = 150){
#   
#   if(!requireNamespace("ggplot2", quietly = TRUE))
#     stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
#   if(!requireNamespace("patchwork", quietly = TRUE))
#     stop("Package 'patchwork' is required. Install it with install.packages('patchwork').")
#   if(!requireNamespace("data.table", quietly = TRUE))
#     stop("Package 'data.table' is required. Install it with install.packages('data.table').")
#   
#   if(is.null(run_folders))
#     run_folders <- list.dirs(map_root_dir, recursive = FALSE, full.names = TRUE)
#   if(length(run_folders) == 0)
#     stop("No subfolders found under map_root_dir: ", map_root_dir)
#   message("plot_ecospace_ensemble_maps_single(): ", length(run_folders),
#           " candidate folder(s) found under ", map_root_dir, ".")
#   
#   if(length(run_folders) > max_folders){
#     run_folders <- sample(run_folders, max_folders)
#     message("  subsampling to ", max_folders, " folder(s) for performance ",
#             "(raise max_folders, e.g. to 910, to use more/all of them).")
#   }
#   
#   if(!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
#   safe_name <- function(s) gsub("[^A-Za-z0-9._-]+", "_", s)
#   
#   bio_tokens <- NULL
#   if(any(vars %in% c("Biomass", "F")))
#     bio_tokens <- detect_map_group_tokens(run_folders, "Biomass", species_patterns)
#   
#   summary_rows <- list()
#   
#   for(var in vars){
#     tokens <- if(var %in% c("Biomass", "F")) bio_tokens else
#       detect_map_group_tokens(run_folders, var, species_patterns)
#     
#     tokens$species <- vapply(tokens$display, function(nm){
#       hit <- species_patterns[vapply(species_patterns, function(p) grepl(p, nm, ignore.case = TRUE), logical(1))]
#       if(length(hit) == 0) NA_character_ else hit[1]
#     }, character(1))
#     tokens <- tokens[!is.na(tokens$species), , drop = FALSE]
#     
#     for(sp in unique(tokens$species)){
#       grp_rows <- tokens[tokens$species == sp, , drop = FALSE]
#       n_stanzas_total <- nrow(grp_rows)
#       n_rows_used <- ceiling(n_stanzas_total / stanzas_per_row)
#       
#       panels <- list()
#       n_used <- integer(0)
#       
#       for(ti in seq_len(nrow(grp_rows))){
#         tok  <- grp_rows$token[ti]
#         disp <- grp_rows$display[ti]
#         
#         # stanza's position in the row/col grid -- y-axis only on the first column of
#         # each row, x-axis only on the last row (same logic as the paired function)
#         stanza_row <- ceiling(ti / stanzas_per_row)
#         stanza_col_in_row <- ((ti - 1) %% stanzas_per_row) + 1
#         show_y_axis <- (stanza_col_in_row == 1)
#         show_x_axis <- (stanza_row == n_rows_used)
#         
#         res <- compute_ensemble_map_stats(run_folders, var, tok, year_range, n_cores = n_cores)
#         message("  [", var, " / ", disp, "] ", res$n_found, " of ", length(run_folders),
#                 " folder(s) had the needed file(s).")
#         if(res$n_found == 0) next
#         agg <- res$agg
#         n_used <- c(n_used, res$n_found)
#         
#         mean_df <- as.data.frame(agg[, .(row, col, lat, lon, value = mean_val)])
#         
#         # same fixed_aspect = FALSE fix as the paired maps -- see the note in
#         # plot_ecospace_ensemble_maps_paired() above for why this (rather than
#         # aspect.ratio=1) is what actually lets each panel fill its allocated cell.
#         mean_panel <- plot_ecospace_map(
#           mean_df, title = "", value_lab = "", log_scale = log_scale,
#           land_mask = land_mask, depth_grid = depth_grid, depth_threshold = depth_threshold,
#           show_x_axis = show_x_axis, show_y_axis = show_y_axis,
#           lon_breaks = lon_breaks, lat_breaks = lat_breaks, inside_legend = TRUE,
#           barheight = barheight, barwidth = barwidth, fixed_aspect = FALSE)
#         
#         mean_panel <- mean_panel + ggplot2::theme(
#           plot.margin = ggplot2::margin(4, 8, 4, 8)
#         )
#         
#         # variable label inside the map (e.g. "Biomass"), same placement/style as the
#         # paired function's mean-panel label
#         mean_label <- grid::textGrob(
#           var,
#           x = grid::unit(0.80, "npc"),
#           y = grid::unit(0.90, "npc"),
#           hjust = 0.5,
#           vjust = 0.5,
#           gp = grid::gpar(
#             fontsize = 11,
#             fontface = "bold"
#           )
#         )
#         
#         mean_panel <- (mean_panel +
#                          patchwork::inset_element(
#                            mean_label,
#                            left = 0.72,
#                            bottom = 0.855,
#                            right = 0.88,
#                            top = 0.945,
#                            align_to = "panel",
#                            clip = FALSE
#                          )) & ggplot2::theme(panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
#                                              plot.background  = ggplot2::element_rect(fill = "transparent", colour = NA))
#         
#         # single-map "stanza unit" with its own title above it (same wrap_elements()
#         # approach as the paired function, for the same plot_annotation()-gets-dropped
#         # reason noted there)
#         title_grob <- patchwork::wrap_elements(
#           grid::textGrob(
#             disp,
#             x = grid::unit(0.5, "npc"),
#             y = grid::unit(0.35, "npc"),
#             gp = grid::gpar(
#               fontsize = 16,
#               fontface = "bold"
#             )
#           ),
#           clip = FALSE
#         )
#         
#         stanza_unit <- (title_grob / mean_panel) +
#           patchwork::plot_layout(
#             heights = c(0.06, 1)
#           )
#         
#         panels[[length(panels) + 1]] <- stanza_unit
#       }
#       
#       if(length(panels) == 0){
#         message("  [skip] ", var, " '", sp, "': no stanza produced data.")
#         next
#       }
#       
#       # panels is now ONE single-map unit per stanza -- grid uses stanzas_per_row directly
#       n_stanzas <- length(panels)
#       rows_used <- ceiling(n_stanzas / stanzas_per_row)
#       
#       # each stanza unit is roughly the width of a single map plus its margin (about
#       # half the width of a mean|CV pair-unit in the paired function)
#       h <- if(is.null(height)) max(3.5, rows_used * 3.6) else height
#       w <- if(is.null(width)) max(4, stanzas_per_row * 3.8) else width
#       
#       p <- patchwork::wrap_plots(panels, ncol = stanzas_per_row)
#       
#       png_file <- file.path(plots_dir, paste0(var, "_ensemble_mean_",
#                                               safe_name(sp), ".png"))
#       ggplot2::ggsave(png_file, plot = p, width = w, height = h, dpi = dpi, units = "in")
#       
#       summary_rows[[length(summary_rows) + 1]] <- data.frame(
#         variable = var, species = sp, n_stanzas_plotted = n_stanzas,
#         png_file = png_file, stringsAsFactors = FALSE)
#     }
#   }
#   
#   message("Wrote ", length(summary_rows), " PNG(s) to ", normalizePath(plots_dir, mustWork = FALSE))
#   invisible(do.call(rbind, summary_rows))
# }


#' @title Read an "M0_loss_rate.csv" (red-tide / other-mortality) export.
#' @description Different header layout from the Ecospace_Annual_Average_<Var>.csv
#'   files: metadata block ends at '"<HEADER end/>"', then a blank line, then a plain
#'   "Year,<group1>,<group2>,..." header row, then rows numbered 1..N (elapsed years
#'   since StartYear, NOT calendar year -- converted here).
#' @param path Path to an M0_loss_rate.csv file.
#' @return data.frame(year, <group columns...>) -- calendar year, one column per group
#'   (including any aggregate columns the export contains, e.g. "gag total").
read_m0_loss_rate <- function(path){
  raw <- readLines(path, warn = FALSE)
  
  hdr_end <- grep('"<HEADER end/>"', raw, fixed = TRUE)
  if(length(hdr_end) == 0) stop("read_m0_loss_rate(): no '<HEADER end/>' marker found in ", path)
  sy_line <- grep("^StartYear,", raw, value = TRUE)
  if(length(sy_line) == 0) stop("read_m0_loss_rate(): no 'StartYear' found in ", path)
  start_year <- as.integer(gsub("^StartYear,", "", sy_line[1]))
  
  after <- raw[(hdr_end[1] + 1):length(raw)]
  after <- after[nzchar(trimws(after))]  # drop blank lines
  if(length(after) < 2) stop("read_m0_loss_rate(): no data rows found after header in ", path)
  
  df <- utils::read.csv(text = paste(after, collapse = "\n"), stringsAsFactors = FALSE, check.names = FALSE)
  if(!"Year" %in% names(df))
    stop("read_m0_loss_rate(): expected a 'Year' column in ", path, ", got: ",
         paste(names(df), collapse = ", "))
  df$year <- start_year + df$Year - 1L
  df$Year <- NULL
  df <- df[, c("year", setdiff(names(df), "year"))]
  
  message("read_m0_loss_rate(): ", basename(path), " -- ", ncol(df) - 1, " group(s), years ",
          min(df$year), "-", max(df$year), ".")
  df
}


#' @keywords internal
#' @noRd
# Weighted quantile (type-4-ish linear interpolation on the weighted CDF) -- used for
# the "aic" ensemble mode's weighted 5th/50th/95th percentile bands, matching the
# weighted-percentile approach in fn.weighted_mrt_summary() from your R4EwE codebase.
weighted_quantile <- function(x, w, probs){
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if(length(x) == 0) return(rep(NA_real_, length(probs)))
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1]], numeric(1))
}


#' @title Pool M0_loss_rate.csv across MANY candidate folders: ensemble mean +/- SD ribbon.
#' @description Like plot_ecospace_ensemble_maps() but for the (non-spatial) M0 export:
#'   scans every subfolder of `map_root_dir`, reads whichever ones have
#'   `M0_loss_rate.csv` (once, shared across species), and for every (year, group)
#'   computes the mean and SD ACROSS CANDIDATES -- i.e. how much does the calibration
#'   ensemble agree on red-tide/other mortality in a given year, and how does that evolve
#'   over time. Writes ONE SEPARATE PNG PER SPECIES (matched via `species_patterns`),
#'   each a +/-1 SD ribbon + mean line (no individual candidate lines), one manually-
#'   built panel per group, arranged in a fixed `facet_ncol`-column grid -- rather than
#'   mixing gag and red grouper stanzas into a single figure.
#' @param map_root_dir Directory containing one subfolder per candidate, each with its
#'   own `M0_loss_rate.csv`.
#' @param ga_runs Output of resolve_run_dirs() -- required for candidate selection
#'   (`ens_mode`), since that needs each candidate's fitness.
#' @param ens_mode One of "quantile" (default, best X% by fitness -- see
#'   `keep_fitness_quantile`), "topN" (exactly the N best by rank -- see `top_n`), or
#'   "aic" (Akaike-weighted, using ALL candidates but down-weighting worse fits -- see
#'   `target_ess`). See select_ensemble_candidates() for the shared selection logic used
#'   by every ensemble function in this script.
#' @param keep_fitness_quantile,top_n,target_ess Passed to select_ensemble_candidates().
#' @param run_folders Optional character vector of specific subfolder paths to use
#'   instead of every subfolder of `map_root_dir`. NULL (default) = all immediate
#'   subfolders.
#' @param species_patterns Character vector of case-insensitive substrings to match
#'   against M0 column names, e.g. c("gag", "red grouper"). Each pattern gets its own
#'   PNG; a column matching more than one pattern is assigned to whichever pattern is
#'   listed first. Columns matching neither pattern are skipped (reported in a message).
#' @param groups Character vector of column names (as they appear in M0_loss_rate.csv)
#'   to consider. NULL (default) = every group column found in the first
#'   successfully-read file.
#' @param max_folders Safety cap on how many candidate folders to read; a random
#'   subsample is used if more are found. Default 200.
#' @param facet_ncol Number of columns in the grid. Default 4. If the number of groups
#'   for a species isn't an exact multiple of this, blank spacer cells are inserted
#'   right after the first (n_groups %% facet_ncol) real panels -- NOT appended at the
#'   end -- so the incomplete row comes first (padded with blanks) and the complete
#'   row comes last. E.g. for 7 groups (gag 0-5+ plus "gag total") at facet_ncol=4:
#'   row 1 = gag 0, gag 1, gag 2, BLANK; row 2 = gag 3, gag 4, gag 5+, gag total.
#'   Ignored when `single_plot = TRUE`.
#' @param single_plot Logical, default FALSE. FALSE (original behavior) produces the
#'   faceted grid described under `facet_ncol`, one independently-scaled panel per
#'   group. TRUE instead produces ONE combined plot with every group (including any
#'   "total" aggregate) drawn as its own colored line, all on the SAME shared y-axis
#'   scale, so magnitudes across ages are directly comparable at a glance --
#'   "M0_ensemble_single_<species>_<ens_mode>.png" instead of
#'   "M0_ensemble_<species>_<ens_mode>.png".
#' @param single_plot_uncertainty Logical, default FALSE. Only used when
#'   `single_plot = TRUE`. FALSE draws mean lines only -- with several groups sharing
#'   one panel, overlapping +/-1 SD ribbons get visually unreadable fast. TRUE draws
#'   them anyway (semi-transparent, no separate fill legend) if you want them despite
#'   the clutter.
#' @param plots_dir Output folder for PNGs (created if it doesn't exist). Default "plots".
#' @param width,height,dpi ggsave() figure dimensions (inches) and resolution (dpi).
#'   When `single_plot = TRUE`, height is scaled to 70% of this value (a single wide
#'   panel needs less vertical space than the multi-row grid).
#' @return Invisibly, a data.frame summarizing what was plotted: one row per species with
#'   species, n_groups, n_candidates, ens_mode, png_file.
#' @export
plot_ecospace_ensemble_m0 <- function(map_root_dir,
                                      ga_runs,
                                      ens_mode         = c("quantile", "topN", "aic"),
                                      keep_fitness_quantile = 0.9,
                                      top_n            = 100,
                                      top_n_prop       = NULL,
                                      target_ess       = 100,
                                      run_folders      = NULL,
                                      species_patterns = c("gag", "red grouper"),
                                      groups           = NULL,
                                      max_folders      = 200,
                                      facet_ncol       = 4,
                                      single_plot      = FALSE,
                                      single_plot_uncertainty = FALSE,
                                      plots_dir        = "plots",
                                      width = 11, height = 8, dpi = 150){
  
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
  
  if(!requireNamespace("data.table", quietly = TRUE))
    stop("Package 'data.table' is required. Install it with install.packages('data.table').")
  
  if(!requireNamespace("patchwork", quietly = TRUE))
    stop("Package 'patchwork' is required. Install it with install.packages('patchwork').")
  
  ens_mode <- match.arg(ens_mode)
  
  valid <- ga_runs[
    ga_runs$exists,
    ,
    drop = FALSE
  ]
  
  sel <- select_ensemble_candidates(
    valid,
    mode = ens_mode,
    keep_fitness_quantile = keep_fitness_quantile,
    top_n = top_n,
    top_n_prop = top_n_prop,
    target_ess = target_ess
  )
  
  weights_by_folder <- if(ens_mode == "aic")
    stats::setNames(
      sel$ens_weight,
      sel$run_folder
    )
  else
    NULL
  
  if(is.null(run_folders))
    run_folders <- list.dirs(
      map_root_dir,
      recursive = FALSE,
      full.names = TRUE
    )
  
  # restrict to the selected candidates (by folder basename matching run_folder) --
  # for "aic" mode this is every candidate (weights applied later), for
  # "quantile"/"topN" it's the hard-selected subset
  run_folders <- run_folders[
    basename(run_folders) %in% sel$run_folder
  ]
  
  if(length(run_folders) == 0)
    stop(
      "No subfolders under map_root_dir matched the selected candidates ",
      "(ens_mode = '", ens_mode, "')."
    )
  
  message(
    "plot_ecospace_ensemble_m0(): ",
    length(run_folders),
    " candidate folder(s) selected (ens_mode = '",
    ens_mode,
    "') under ",
    map_root_dir,
    "."
  )
  
  if(length(run_folders) > max_folders){
    
    run_folders <- sample(
      run_folders,
      max_folders
    )
    
    message(
      "  subsampling to ",
      max_folders,
      " folder(s) for performance ",
      "(raise max_folders to use more/all of them)."
    )
  }
  
  all_list <- list()
  n_found <- 0
  
  for(rf in run_folders){
    
    path <- file.path(
      rf,
      "M0_loss_rate.csv"
    )
    
    if(!file.exists(path))
      next
    
    m0 <- tryCatch(
      read_m0_loss_rate(path),
      error = function(e) NULL
    )
    
    if(is.null(m0))
      next
    
    m0$candidate <- basename(rf)
    
    all_list[[length(all_list) + 1]] <- m0
    
    n_found <- n_found + 1
  }
  
  message(
    "plot_ecospace_ensemble_m0(): ",
    n_found,
    " of ",
    length(run_folders),
    " folder(s) had M0_loss_rate.csv."
  )
  
  if(n_found == 0)
    stop(
      "No M0_loss_rate.csv files found in any checked folder."
    )
  
  all_df <- do.call(
    rbind,
    all_list
  )
  
  if(is.null(groups))
    groups <- setdiff(
      names(all_df),
      c(
        "year",
        "candidate"
      )
    )
  
  missing_g <- setdiff(
    groups,
    names(all_df)
  )
  
  if(length(missing_g) > 0)
    stop(
      "plot_ecospace_ensemble_m0(): group(s) not found: ",
      paste(
        missing_g,
        collapse = ", "
      )
    )
  
  # assign each group column to whichever species pattern it matches first
  species_of <- vapply(
    groups,
    function(g){
      
      hit <- species_patterns[
        vapply(
          species_patterns,
          function(p)
            grepl(
              p,
              g,
              ignore.case = TRUE
            ),
          logical(1)
        )
      ]
      
      if(length(hit) == 0)
        NA_character_
      else
        hit[1]
    },
    character(1)
  )
  
  ungrouped <- groups[
    is.na(species_of)
  ]
  
  if(length(ungrouped) > 0)
    message(
      "plot_ecospace_ensemble_m0(): ",
      length(ungrouped),
      " column(s) matched no species_patterns, skipped: ",
      paste(
        ungrouped,
        collapse = ", "
      )
    )
  
  safe_name <- function(s)
    gsub(
      "[^A-Za-z0-9._-]+",
      "_",
      s
    )
  
  if(!dir.exists(plots_dir))
    dir.create(
      plots_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  
  summary_rows <- list()
  
  for(sp in unique(species_patterns)){
    
    sp_groups <- groups[
      !is.na(species_of) &
        species_of == sp
    ]
    
    if(length(sp_groups) == 0){
      
      message(
        "  [skip] '",
        sp,
        "': no matching column(s) in M0_loss_rate.csv."
      )
      
      next
    }
    
    dt <- data.table::as.data.table(
      all_df
    )
    
    long <- data.table::melt(
      dt,
      id.vars = c(
        "year",
        "candidate"
      ),
      measure.vars = sp_groups,
      variable.name = "group",
      value.name = "value"
    )
    
    if(ens_mode == "aic"){
      
      long$w <- weights_by_folder[
        long$candidate
      ]
      
      agg <- long[, {
        
        wm <- stats::weighted.mean(
          value,
          w,
          na.rm = TRUE
        )
        
        qs <- weighted_quantile(
          value,
          w,
          probs = c(
            0.05,
            0.95
          )
        )
        
        .(
          mean_val = wm,
          lo = qs[1],
          hi = qs[2],
          n = sum(
            !is.na(value)
          )
        )
        
      }, by = .(
        year,
        group
      )]
      
    } else {
      
      agg <- long[, .(
        mean_val = mean(
          value,
          na.rm = TRUE
        ),
        sd_val = stats::sd(
          value,
          na.rm = TRUE
        ),
        n = sum(
          !is.na(value)
        )
      ), by = .(
        year,
        group
      )]
      
      agg[, `:=`(
        lo = mean_val - sd_val,
        hi = mean_val + sd_val
      )]
    }
    
    agg_df <- as.data.frame(
      agg
    )
    
    agg_df$group <- factor(
      agg_df$group,
      levels = sp_groups
    )
    
    ens_label <- switch(
      ens_mode,
      
      quantile = paste0(
        sp,
        " (best ",
        round(
          keep_fitness_quantile * 100
        ),
        "%, n=",
        n_found,
        ")"
      ),
      
      topN = paste0(
        sp,
        " (top ",
        top_n,
        ", n=",
        n_found,
        ")"
      ),
      
      aic = paste0(
        sp,
        " (AIC-weighted, ESS~",
        target_ess,
        "; lines = lowest ",
        top_n,
        ", n=",
        n_found,
        ")"
      )
    )
    
    if(single_plot){
      
      # ---- single combined plot: total is removed completely.
      # Only age/group lines are shown in the plot and legend.
      
      group_names <- tolower(
        trimws(
          as.character(
            agg_df$group
          )
        )
      )
      
      is_total <- grepl(
        "total",
        group_names
      )
      
      agg_single <- agg_df[
        !is_total,
        ,
        drop = FALSE
      ]
      
      # Re-factor after removing total so the legend contains
      # only the remaining groups.
      
      agg_single$group <- droplevels(
        agg_single$group
      )
      
      p <- ggplot2::ggplot(
        agg_single,
        ggplot2::aes(
          x = year,
          y = mean_val,
          color = group
        )
      )
      
      # --------------------------------------------------------
      # Uncertainty
      # --------------------------------------------------------
      
      if(single_plot_uncertainty &&
         nrow(agg_single) > 0){
        
        p <- p +
          ggplot2::geom_ribbon(
            data = agg_single,
            ggplot2::aes(
              x = year,
              ymin = pmax(
                lo,
                0
              ),
              ymax = hi,
              fill = group
            ),
            inherit.aes = FALSE,
            alpha = 0.15,
            colour = NA,
            show.legend = FALSE
          )
      }
      
      # --------------------------------------------------------
      # Mean lines
      # --------------------------------------------------------
      
      if(nrow(agg_single) > 0){
        
        p <- p +
          ggplot2::geom_line(
            data = agg_single,
            ggplot2::aes(
              x = year,
              y = mean_val,
              color = group
            ),
            linewidth = 0.7,
            alpha = 0.75
          )
      }
      
      # --------------------------------------------------------
      # Scales and theme
      # --------------------------------------------------------
      
      p <- p +
        ggplot2::scale_x_continuous(
          breaks = scales::pretty_breaks(
            n = 10
          ),
          minor_breaks = seq(
            floor(
              min(
                agg_single$year,
                na.rm = TRUE
              )
            ),
            ceiling(
              max(
                agg_single$year,
                na.rm = TRUE
              )
            ),
            by = 1
          )
        ) +
        ggplot2::scale_color_viridis_d(
          name = NULL,
          direction = -1,
          end = 0.9
        ) +
        ggplot2::scale_fill_viridis_d(
          name = NULL,
          direction = -1,
          end = 0.9
        ) +
        ggplot2::labs(
          title = sp,
          x = "Year",
          y = expression(
            M0~(year^{-1})
          )
        ) +
        ggplot2::theme_bw(
          base_size = 12
        ) +
        ggplot2::theme(
          
          plot.title = ggplot2::element_text(
            size = 13,
            face = "bold",
            hjust = 0.5,
            margin = ggplot2::margin(
              0,
              0,
              6,
              0
            )
          ),
          
          # Major grid
          panel.grid.major = ggplot2::element_line(
            colour = scales::alpha(
              "grey25",
              0.35
            ),
            linetype = "dashed",
            linewidth = 0.35
          ),
          
          # Minor grid, with a vertical line at every year
          panel.grid.minor = ggplot2::element_line(
            colour = scales::alpha(
              "grey35",
              0.20
            ),
            linetype = "dashed",
            linewidth = 0.20
          ),
          
          legend.position = "right"
        )
      
      out_path <- file.path(
        plots_dir,
        paste0(
          "M0_ensemble_single_",
          safe_name(sp),
          "_",
          ens_mode,
          ".png"
        )
      )
      
      print(p)
      
      ggplot2::ggsave(
        out_path,
        plot = p,
        width = width,
        height = height * 0.7,
        dpi = dpi,
        units = "in"
      )
      
      message(
        "Wrote ",
        out_path,
        " (",
        ens_label,
        ")"
      )
      
      summary_rows[[length(summary_rows) + 1]] <-
        data.frame(
          species = sp,
          n_groups = length(sp_groups),
          n_candidates = n_found,
          ens_mode = ens_mode,
          png_file = out_path,
          stringsAsFactors = FALSE
        )
      
      next
    }
    
    # manually-built panels (not facet_wrap()) arranged in a fixed 4-column grid,
    # with an explicit blank cell after the 3rd group -- e.g. for gag's 7 groups
    # (0-5+ plus "total"): row 1 = gag 0, gag 1, gag 2, BLANK;
    # row 2 = gag 3, gag 4, gag 5+, gag total.
    #
    # Grid layout is pre-computed BEFORE the panel loop (it only depends on
    # length(sp_groups), known upfront) so each panel can determine its own FINAL
    # position in the grid -- accounting for where the blank cell(s) get inserted --
    # and show axis titles only where they're needed: x-axis title only on the bottom
    # row, y-axis title only in the first column. Blanks are always inserted into the
    # first (incomplete) row, never the last, so the last row is always fully real
    # panels -- meaning "is this panel in the last row" is a safe, simple check for
    # "is this panel bottommost in its column" without needing per-column tracking.
    n_panels_grid <- length(sp_groups)
    n_cells_grid <- ceiling(n_panels_grid / facet_ncol) * facet_ncol
    n_blanks_grid <- n_cells_grid - n_panels_grid
    insert_at_grid <- n_panels_grid %% facet_ncol
    if(insert_at_grid == 0) insert_at_grid <- n_panels_grid
    n_rows_total <- n_cells_grid / facet_ncol
    
    panel_list <- list()
    
    for(gi in seq_along(sp_groups)){
      gname <- sp_groups[gi]
      
      sub <- agg_df[
        agg_df$group == gname,
        ,
        drop = FALSE
      ]
      
      # this panel's FINAL position in the grid, after blank insertion (see the
      # layout note above) -- panels at or before insert_at_grid keep their original
      # position; panels after it shift right by n_blanks_grid to make room for the
      # blank cell(s) inserted in between
      final_pos <- if(gi <= insert_at_grid) gi else gi + n_blanks_grid
      row_pos <- ceiling(final_pos / facet_ncol)
      col_pos <- ((final_pos - 1) %% facet_ncol) + 1
      show_x_axis <- (row_pos == n_rows_total)
      show_y_axis <- (col_pos == 1)
      
      pp <- ggplot2::ggplot(
        sub,
        ggplot2::aes(
          x = year,
          y = mean_val
        )
      ) +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin = pmax(
              lo,
              0
            ),
            ymax = hi
          ),
          fill = "firebrick",
          alpha = 0.25
        ) +
        ggplot2::geom_line(
          color = "firebrick",
          linewidth = 0.7
        ) +
        ggplot2::labs(
          title = gname,
          x = "Year",
          y = expression(
            M0~(year^{-1})
          )
        ) +
        ggplot2::theme_bw(
          base_size = 11
        ) +
        ggplot2::theme(
          
          plot.title = ggplot2::element_text(
            size = 11,
            face = "bold",
            hjust = 0.5,
            margin = ggplot2::margin(
              0,
              0,
              4,
              0
            )
          ),
          
          panel.grid.major = ggplot2::element_line(
            colour = scales::alpha(
              "grey25",
              0.35
            ),
            linetype = "dashed",
            linewidth = 0.35
          ),
          
          panel.grid.minor = ggplot2::element_line(
            colour = scales::alpha(
              "grey35",
              0.20
            ),
            linetype = "dashed",
            linewidth = 0.20
          ),
          
          axis.title.x = if(show_x_axis) ggplot2::element_text() else ggplot2::element_blank(),
          axis.text.x  = if(show_x_axis) ggplot2::element_text() else ggplot2::element_blank(),
          axis.ticks.x = if(show_x_axis) ggplot2::element_line() else ggplot2::element_blank(),
          axis.title.y = if(show_y_axis) ggplot2::element_text() else ggplot2::element_blank(),
          axis.text.y  = if(show_y_axis) ggplot2::element_text() else ggplot2::element_blank(),
          axis.ticks.y = if(show_y_axis) ggplot2::element_line() else ggplot2::element_blank(),
          
          aspect.ratio = 1
        )
      
      panel_list[[gname]] <- pp
    }
    
    # reuse the grid dimensions already computed before the loop (n_panels_grid ==
    # length(panel_list) always, since every sp_groups entry produced exactly one
    # panel) -- no need to recompute them
    blanks <- stats::setNames(
      replicate(
        n_blanks_grid,
        patchwork::plot_spacer(),
        simplify = FALSE
      ),
      paste0(
        "__blank",
        seq_len(n_blanks_grid)
      )
    )
    
    full_list <-
      if(n_blanks_grid > 0)
        c(
          panel_list[
            seq_len(insert_at_grid)
          ],
          blanks,
          panel_list[
            seq_len(
              n_panels_grid - insert_at_grid
            ) +
              insert_at_grid
          ]
        )
    else
      panel_list
    
    p <- patchwork::wrap_plots(
      full_list,
      ncol = facet_ncol
    )
    
    out_path <- file.path(
      plots_dir,
      paste0(
        "M0_ensemble_",
        safe_name(sp),
        "_",
        ens_mode,
        ".png"
      )
    )
    
    ggplot2::ggsave(
      out_path,
      plot = p,
      width = width,
      height = height,
      dpi = dpi,
      units = "in"
    )
    
    message(
      "Wrote ",
      out_path,
      " (",
      ens_label,
      ")"
    )
    
    summary_rows[[length(summary_rows) + 1]] <-
      data.frame(
        species = sp,
        n_groups = length(sp_groups),
        n_candidates = n_found,
        ens_mode = ens_mode,
        png_file = out_path,
        stringsAsFactors = FALSE
      )
  }
  
  invisible(
    do.call(
      rbind,
      summary_rows
    )
  )
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# cmd.txt: environmental response function parameters, colored by fitness
#
# Each candidate's cmd.txt has lines like:
#   <ECOSPACE_ENVIRONMENTAL_RESPONSE_INDEXED>(4), 10 9.144 132.59 18.9209 0 -0.1563 0.991, Indexed.Single[]
# (group index, then shape-type code + that shape's numeric parameters). Lines starting
# with "//" are commented-out format examples, not real values, and are skipped.
#
# SHAPE_FUNCTION_TYPES is copied verbatim from cmd.txt's own header enum. Confirmed
# parameter names/order: Sigmoid (type 10, 6 params: x min, x max, x mid, x opt, steep,
# y max) and Trapezoid (type 9, 4 params: X1-X4, tolerance function) via cmd.txt's
# header, the EwE User Guide, and this project's own GA tag-generation code. Normal
# (type 6, 5 params) only has position 5 ("Max") confirmed; positions 1-4 stay generic.
# All other shape types have no confirmed parameter names and stay generically labeled.
#
# plot_ga_response_curves() evaluates and draws the actual response curve for Sigmoid,
# Trapezoid, Normal, and Logistic4Params -- see eval_sigmoid_response()/
# eval_trapezoid_response()/eval_normal_response()/eval_logistic4params_response().
#
# Shape type itself determines the driver category (confirmed via this project's own
# calibration code, fn.makeparvec()): shape 11 (Logistic4Params) is ALWAYS red tide (20
# of 31 active response functions); shapes 6/9/10 are habitat (SST/SBT/depth/PP-derived,
# 11 of 31). SHAPE_DRIVER_CATEGORY below encodes this and is the default driver_label
# for plot_ga_response_curves() when none is supplied.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Verbatim from cmd.txt's own "Public Enum eShapeFunctionType As Long" block.
SHAPE_FUNCTION_TYPES <- c(
  `0` = "NotSet", `1` = "Linear", `2` = "Sigmoid_Legacy", `3` = "Hyperbolic",
  `4` = "Exponential", `5` = "Betapdf", `6` = "Normal", `7` = "RightShoulder",
  `8` = "LeftShoulder", `9` = "Trapezoid", `10` = "Sigmoid", `11` = "Logistic4Params"
)

# CONFIRMED driver category per shape type -- sourced directly from your own R4EwE
# calibration code (fn.makeparvec()), which states explicitly: "All red tide responses
# are shape 11 (logistic4params)" and gives shape 11 the GA parameter names
# inflection.adj/slope.adj under the `redtide` parameter block specifically (as opposed
# to shapes 6/9/10, which appear under the general `env` block with names like
# mean.adj/width.adj/xmid.adj -- the GAM-derived habitat-capacity responses). This is a
# real rule from the calibration source, not inferred from parameter magnitude: in this
# model's 31 environmental response functions, shape 11 is ALWAYS red tide (20 of 31),
# and shapes 6/9/10 are ALWAYS GAM-derived habitat responses (11 of 31 -- SST, SBT,
# depth, or primary production per Vilas et al. 2023's methodology, but WHICH of those
# four for a given group_index is still not resolved without WFS_5min_STconfig.xml or
# the model database -- see plot_ga_response_by_driver_map() to supply that once known).
SHAPE_DRIVER_CATEGORY <- c(
  `6`  = "Habitat (GAM-derived: SST/SBT/depth/PP)",
  `9`  = "Habitat (GAM-derived: SST/SBT/depth/PP)",
  `10` = "Habitat (GAM-derived: SST/SBT/depth/PP)",
  `11` = "Red tide"
)

#' @title Parse "<TAG>(index), val1 val2 ..., Type" lines out of an EwE cmd.txt file.
#' @description Generic reader for any `<TAG>_INDEXED` parameter block in a cmd.txt
#'   (works for ECOSPACE_ENVIRONMENTAL_RESPONSE_INDEXED, ECOSIM_VULNERABILITIES_INDEXED,
#'   ECOSPACE_DISPERSAL_RATE_INDEXED, etc. -- anything of the form
#'   `<TAG>(idx), space-separated numbers, Type`). Lines beginning with `//` (cmd.txt's
#'   own commented-out format-documentation examples) are skipped -- only active,
#'   uncommented parameter lines are returned.
#' @param path Path to a cmd.txt file.
#' @param tag The tag name without angle brackets, e.g. "ECOSPACE_ENVIRONMENTAL_RESPONSE_INDEXED".
#' @return data.frame(index, param_string) plus a list-column `params` (numeric vector
#'   per row, the space-separated values parsed out of param_string). Zero rows if the
#'   tag doesn't appear (active, uncommented) in the file.
read_cmd_indexed_params <- function(path, tag){
  raw <- readLines(path, warn = FALSE)
  raw <- raw[!grepl("^\\s*//", raw)]  # drop commented-out format-example lines
  
  pat <- paste0("^<", tag, ">\\((\\d+)\\)\\s*,\\s*([^,]+),")
  hit_lines <- raw[grepl(pat, raw)]
  if(length(hit_lines) == 0)
    return(data.frame(index = integer(0), param_string = character(0), stringsAsFactors = FALSE))
  
  m <- regmatches(hit_lines, regexec(pat, hit_lines))
  idx <- vapply(m, function(x) as.integer(x[2]), integer(1))
  pstr <- vapply(m, function(x) trimws(x[3]), character(1))
  params <- lapply(strsplit(pstr, "\\s+"), function(v) suppressWarnings(as.numeric(v)))
  
  out <- data.frame(index = idx, param_string = pstr, stringsAsFactors = FALSE)
  out$params <- params
  out
}




#' @title Evaluate a Sigmoid response curve (shape type 10).
#' @description Standard logistic-style sigmoid, scaled to [0, ymax], with `xmid` as the
#'   inflection point and `steep` controlling growth rate. RECONSTRUCTED FROM STANDARD
#'   SIGMOID CONVENTIONS, NOT SOURCE-CODE-VERIFIED against Ecospace's own implementation
#'   -- see the shape-function section note above plot_ga_response_curves(). `xopt` is
#'   accepted (matching cmd.txt's own 6-parameter Sigmoid block) but not used in this
#'   reconstructed formula; it's undocumented in the confirmed parameter list beyond its
#'   name.
#' @param x Numeric vector of driver values to evaluate at.
#' @param xmin,xmax Domain bounds (used by the caller to build `x`, not by this formula
#'   directly).
#' @param xmid Inflection point (x value at 50% of ymax).
#' @param xopt Accepted for signature completeness; unused (see @description).
#' @param steep Logistic growth rate.
#' @param ymax Upper asymptote / peak response value.
#' @return Numeric vector the same length as `x`.
eval_sigmoid_response <- function(x, xmin, xmax, xmid, xopt, steep, ymax){
  ymax / (1 + exp(-steep * (x - xmid)))
}

#' @title Evaluate a Trapezoid response curve (shape type 9).
#' @description Standard trapezoidal tolerance/membership function: 0 below x1, rises
#'   linearly from x1 to x2, flat at 1 from x2 to x3, falls linearly from x3 to x4, 0
#'   above x4. This is the shape with the STRONGEST supporting evidence in this project
#'   (corroborated by the tested sensitivity-analysis code, per the shape-function
#'   section note above plot_ga_response_curves()), though the exact formula here is
#'   still a standard-convention reconstruction, not copied from Ecospace's own source.
#' @param x Numeric vector of driver values to evaluate at.
#' @param x1,x2,x3,x4 The four breakpoints (x1 <= x2 <= x3 <= x4).
#' @return Numeric vector in [0, 1], the same length as `x`.
eval_trapezoid_response <- function(x, x1, x2, x3, x4){
  y <- numeric(length(x))
  y[x <= x1 | x >= x4] <- 0
  rising  <- x > x1 & x < x2
  plateau <- x >= x2 & x <= x3
  falling <- x > x3 & x < x4
  y[rising]  <- (x[rising]  - x1) / (x2 - x1)
  y[plateau] <- 1
  y[falling] <- (x4 - x[falling]) / (x4 - x3)
  y
}

#' @title Evaluate a Normal (asymmetric Gaussian) response curve (shape type 6).
#' @description Gaussian peak scaled to `max_i`, with independent standard deviations on
#'   either side of the mean (a "split normal" -- steeper falloff on whichever side has
#'   the smaller SD). RECONSTRUCTED FROM STANDARD CONVENTIONS, NOT SOURCE-CODE-VERIFIED
#'   -- see the shape-function section note above plot_ga_response_curves().
#' @param x Numeric vector of driver values to evaluate at.
#' @param mean_i Peak location.
#' @param sdleft,sdright Standard deviation used for x < mean_i and x >= mean_i respectively.
#' @param max_i Peak response value.
#' @return Numeric vector the same length as `x`.
eval_normal_response <- function(x, mean_i, sdleft, sdright, max_i){
  sd_i <- ifelse(x < mean_i, sdleft, sdright)
  sd_i[sd_i <= 0] <- .Machine$double.eps  # guard against a degenerate zero-width side
  max_i * exp(-((x - mean_i)^2) / (2 * sd_i^2))
}

#' @title Evaluate a Logistic4Params response curve (shape type 11 -- always red tide).
#' @description Standard 2-parameter-shape logistic scaled to [0, 1] (xmin/xmax here are
#'   the x-DOMAIN bounds used by the caller to build `x`, not response-value asymptotes).
#'   RECONSTRUCTED FROM STANDARD CONVENTIONS, NOT SOURCE-CODE-VERIFIED -- see the
#'   shape-function section note above plot_ga_response_curves().
#' @param x Numeric vector of driver values to evaluate at.
#' @param xmin,xmax Domain bounds (used by the caller to build `x`, not by this formula
#'   directly).
#' @param inflection Inflection point (x value at y = 0.5).
#' @param slope Logistic growth rate.
#' @return Numeric vector in [0, 1], the same length as `x`.
eval_logistic4params_response <- function(x, xmin, xmax, inflection, slope){
  1 / (1 + exp(-slope * (x - inflection)))
}


#' @title Evaluate a single static response curve directly from a catalog row's own parameters.
#' @description For a FIXED (non-GA-tuned) response function -- one that never appears
#'   active in any candidate's cmd.txt, so there's no per-candidate data to read -- this
#'   evaluates ONE curve from the catalog's own shape_type/par1..par6 columns instead.
#'   Mirrors plot_ga_response_curves()'s own per-shape logic exactly (same eval_*
#'   functions, same domain construction), just reading parameters from a catalog row
#'   instead of a cmd.txt line.
#' @param catalog_row A single-row data.frame from read_env_response_catalog()'s output
#'   (i.e. `catalog[gi, ]`).
#' @param n_points Number of x points to evaluate at. Default 200.
#' @param x_floor Optional numeric lower bound to clip the domain to. NULL (default) = none.
#' @return data.frame(shape_type, x, y), or NULL if the row's shape_type isn't one of the
#'   four handled types (Sigmoid/Trapezoid/Normal/Logistic4Params).
.eval_catalog_response_curve <- function(catalog_row, n_points = 200, x_floor = NULL){
  shp <- catalog_row$shape_type[1]
  p <- c(catalog_row$par1[1], catalog_row$par2[1], catalog_row$par3[1],
         catalog_row$par4[1], catalog_row$par5[1], catalog_row$par6[1])
  
  if(!is.na(shp) && shp == 10 && !anyNA(p[1:5])){                  # Sigmoid
    xmin <- p[1]; xmax <- p[2]; xmid <- p[3]; xopt <- p[4]; steep <- p[5]; ymax <- p[6]
    if(is.na(ymax)) ymax <- 1
    lo <- if(!is.null(x_floor)) max(xmin, x_floor) else xmin
    x_seq <- seq(lo, xmax, length.out = n_points)
    y_seq <- eval_sigmoid_response(x_seq, xmin, xmax, xmid, xopt, steep, ymax)
  } else if(!is.na(shp) && shp == 9 && !anyNA(p[1:4])){             # Trapezoid
    x1 <- p[1]; x2 <- p[2]; x3 <- p[3]; x4 <- p[4]
    lo <- if(!is.null(x_floor)) max(x1, x_floor) else x1
    x_seq <- seq(lo, x4, length.out = n_points)
    y_seq <- eval_trapezoid_response(x_seq, x1, x2, x3, x4)
  } else if(!is.na(shp) && shp == 6 && !anyNA(p[c(1,3,4,5)])){      # Normal
    sdleft <- p[1]; sdright <- p[3]; mean_i <- p[4]; max_i <- p[5]  # p[2] = redundant auto DataWidth
    datawidth <- 5 * sdleft + 5 * sdright
    lo <- mean_i - datawidth / 2
    if(!is.null(x_floor)) lo <- max(lo, x_floor)
    x_seq <- seq(lo, mean_i + datawidth / 2, length.out = n_points)
    y_seq <- eval_normal_response(x_seq, mean_i, sdleft, sdright, max_i)
  } else if(!is.na(shp) && shp == 11 && !anyNA(p[1:4])){            # Logistic4Params
    xmin <- p[1]; xmax <- p[2]; inflection <- p[3]; slope <- p[4]
    lo <- if(!is.null(x_floor)) max(xmin, x_floor) else xmin
    x_seq <- seq(lo, xmax, length.out = n_points)
    y_seq <- eval_logistic4params_response(x_seq, xmin, xmax, inflection, slope)
  } else {
    return(NULL)
  }
  
  data.frame(shape_type = shp, x = x_seq, y = y_seq, stringsAsFactors = FALSE)
}


#' @title Plot environmental response CURVES across the ensemble, colored by fitness.
#' @description For one Ecopath group index, evaluates each candidate's actual response
#'   curve (y in [0, 1] roughly, vs. the environmental driver value x, over that
#'   candidate's own domain) and plots them all together colored on the same continuous
#'   fitness scale used elsewhere in this script -- "does the shape of the environmental
#'   preference curve itself converge as fitness improves." Handles shape type 10
#'   (Sigmoid, via eval_sigmoid_response()) and type 9 (Trapezoid, via
#'   eval_trapezoid_response()); candidates using a different/unhandled shape type for
#'   this group are skipped with a message. RECONSTRUCTED FORMULAS, NOT SOURCE-CODE-
#'   VERIFIED -- see the shape-function section note above and the two eval_*
#'   function docs (Trapezoid has stronger supporting evidence than Sigmoid, since it's
#'   corroborated by your own tested sensitivity-analysis code rather than just
#'   documentation). If you can screenshot Ecospace's own "Change shape form" dialog for
#'   one of these drivers, that would let this be checked directly against ground truth.
#' @param ga_runs Output of resolve_run_dirs().
#' @param group_index Ecopath group pool code (integer) whose environmental response to
#'   plot. The real group name is looked up automatically from any candidate's Biomass
#'   export and used in the plot/filename instead of the bare number.
#' @param driver_label Label for the x-axis (the environmental driver's own units/name)
#'   -- NOT stored anywhere in cmd.txt that this script parses, so you'll need to supply
#'   it yourself if you know which physical driver corresponds to this group_index (e.g.
#'   "Depth (m)", "SST (\u00b0C)"). NULL (default) labels it generically as "Environmental variable".
#' @param n_points Number of x points to evaluate each curve at. Default 200.
#' @param x_floor Optional numeric lower bound to clip the evaluated x-domain to (e.g.
#'   0 for a driver like depth that's never negative). Only the Normal shape's domain is
#'   actually computed rather than read from cmd.txt (`mean - datawidth/2`, which CAN go
#'   negative even for a driver like depth), so this matters most for that shape;
#'   applied to every shape's domain uniformly for consistency. NULL (default) = no
#'   clipping.
#' @param keep_fitness_quantile Optional fraction in (0, 1]; if supplied, restricts to
#'   candidates with fitness at or below this quantile (e.g. 0.9 = drop the worst 10% by
#'   NLL) BEFORE plotting -- same convention as the ensemble time series functions.
#'   NULL (default) = use every candidate.
#' @param max_folders Safety cap on candidate folders read; a random subsample is used
#'   if more are found. Default 200.
#' @param plots_dir Output folder for the PNG (created if it doesn't exist). Default "plots".
#' @param png_file Output filename. Default "response_curves_<group_name>.png".
#' @param width,height,dpi ggsave() figure dimensions (inches) and resolution (dpi).
#' @return Invisibly, the long-format data.frame that was plotted (run_folder, fitness,
#'   shape_type, x, y).
#' @export
plot_ga_response_curves <- function(ga_runs, group_index,
                                    driver_label = NULL,
                                    n_points    = 200,
                                    x_floor     = NULL,
                                    keep_fitness_quantile = NULL,
                                    max_folders = 200,
                                    plots_dir   = "plots",
                                    png_file    = NULL,
                                    width = 8, height = 5.5, dpi = 150){
  if(!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required. Install it with install.packages('ggplot2').")
  
  valid <- ga_runs[ga_runs$exists, , drop = FALSE]
  if(nrow(valid) == 0) stop("No candidate folders found on disk -- check ga_runs$exists.")
  if(!is.null(keep_fitness_quantile)){
    cutoff <- stats::quantile(valid$fitness, probs = keep_fitness_quantile, na.rm = TRUE)
    valid  <- valid[valid$fitness <= cutoff, , drop = FALSE]
    if(nrow(valid) == 0) stop("No candidates left after keep_fitness_quantile filtering.")
  }
  if(nrow(valid) > max_folders){
    valid <- valid[sample(seq_len(nrow(valid)), max_folders), , drop = FALSE]
    message("plot_ga_response_curves(): subsampling to ", max_folders, " candidate(s) for performance.")
  }
  
  # look up the real functional group name (e.g. "gag 0") from any candidate's own
  # Biomass export, instead of showing the bare pool-code number
  group_display <- paste0("group ", group_index)
  for(i in seq_len(nrow(valid))){
    ref <- tryCatch(get_region_output(0L, "Biomass", valid$run_dir[i], new.env(parent = emptyenv())),
                    error = function(e) NULL)
    if(!is.null(ref) && (group_index + 1L) <= length(ref$group_names) && nzchar(ref$group_names[group_index + 1L])){
      group_display <- ref$group_names[group_index + 1L]
      break
    }
  }
  
  rows <- list()
  n_found <- 0
  n_wrong_shape <- 0
  for(i in seq_len(nrow(valid))){
    cmd_path <- file.path(valid$run_dir[i], "cmd.txt")
    if(!file.exists(cmd_path)) next
    tab <- tryCatch(read_cmd_indexed_params(cmd_path, "ECOSPACE_ENVIRONMENTAL_RESPONSE_INDEXED"),
                    error = function(e) NULL)
    if(is.null(tab) || nrow(tab) == 0) next
    row <- tab[tab$index == group_index, , drop = FALSE]
    if(nrow(row) == 0) next
    p <- row$params[[1]]
    shp <- if(length(p) >= 1) p[1] else NA_real_
    
    if(length(p) == 7 && shp == 10){          # type + 6 Sigmoid params
      xmin <- p[2]; xmax <- p[3]; xmid <- p[4]; xopt <- p[5]; steep <- p[6]; ymax <- p[7]
      lo <- if(!is.null(x_floor)) max(xmin, x_floor) else xmin
      x_seq <- seq(lo, xmax, length.out = n_points)
      y_seq <- eval_sigmoid_response(x_seq, xmin, xmax, xmid, xopt, steep, ymax)
    } else if(length(p) == 5 && shp == 9){    # type + 4 Trapezoid params
      x1 <- p[2]; x2 <- p[3]; x3 <- p[4]; x4 <- p[5]
      lo <- if(!is.null(x_floor)) max(x1, x_floor) else x1
      x_seq <- seq(lo, x4, length.out = n_points)
      y_seq <- eval_trapezoid_response(x_seq, x1, x2, x3, x4)
    } else if(length(p) == 6 && shp == 6){    # type + 5 Normal params
      sdleft <- p[2]; sdright <- p[4]; mean_i <- p[5]; max_i <- p[6]  # p[3] = redundant auto DataWidth
      datawidth <- 5 * sdleft + 5 * sdright
      lo <- mean_i - datawidth / 2
      if(!is.null(x_floor)) lo <- max(lo, x_floor)
      x_seq <- seq(lo, mean_i + datawidth / 2, length.out = n_points)
      y_seq <- eval_normal_response(x_seq, mean_i, sdleft, sdright, max_i)
    } else if(length(p) == 5 && shp == 11){   # type + 4 Logistic4Params params
      xmin <- p[2]; xmax <- p[3]; inflection <- p[4]; slope <- p[5]
      lo <- if(!is.null(x_floor)) max(xmin, x_floor) else xmin
      x_seq <- seq(lo, xmax, length.out = n_points)
      y_seq <- eval_logistic4params_response(x_seq, xmin, xmax, inflection, slope)
    } else {
      n_wrong_shape <- n_wrong_shape + 1
      next
    }
    
    rows[[length(rows) + 1]] <- data.frame(
      run_folder = valid$run_folder[i], fitness = valid$fitness[i], shape_type = shp,
      x = x_seq, y = y_seq, stringsAsFactors = FALSE
    )
    n_found <- n_found + 1
  }
  message("plot_ga_response_curves(): ", group_display, " (index ", group_index, ") -- ", n_found,
          " of ", nrow(valid), " candidate(s) had a plottable response here (Sigmoid, Trapezoid, ",
          "Normal, or Logistic4Params)",
          if(n_wrong_shape > 0) paste0(" (", n_wrong_shape, " used a different/unhandled shape type, skipped)") else "",
          ".")
  if(n_found == 0)
    stop("No candidate had a Sigmoid- or Trapezoid-type ECOSPACE_ENVIRONMENTAL_RESPONSE_INDEXED(",
         group_index, ") set in cmd.txt among the folders checked.")
  
  df <- do.call(rbind, rows)
  shapes_seen <- sort(unique(df$shape_type))
  shape_names_seen <- SHAPE_FUNCTION_TYPES[as.character(shapes_seen)]
  message("plot_ga_response_curves(): shape(s) drawn = ", paste(shape_names_seen, collapse = "/"),
          " -- RECONSTRUCTED formula, not source-verified (see function docs).")
  best_folder <- valid$run_folder[which.min(valid$fitness)]
  best_df <- df[df$run_folder == best_folder, , drop = FALSE]
  df <- df[order(-df$fitness), ]  # worst first, best drawn on top
  
  # confirmed default driver label from SHAPE_DRIVER_CATEGORY (see its definition for
  # sourcing) -- note this function only ever draws shapes 9/10, both "Habitat"
  # category, since no curve formula is implemented for shape 11 (red tide) yet
  driver_txt <- if(!is.null(driver_label)) driver_label else {
    cats <- unique(SHAPE_DRIVER_CATEGORY[as.character(shapes_seen)])
    cats <- cats[!is.na(cats)]
    if(length(cats) == 1) cats else if(length(cats) > 1) paste(cats, collapse = " / ") else "Environmental variable (unrecognized shape)"
  }
  safe_name <- function(s) gsub("[^A-Za-z0-9._-]+", "_", s)
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = df, ggplot2::aes(x = x, y = y, group = run_folder, color = fitness),
                       linewidth = 0.35, alpha = 0.45) +
    ggplot2::scale_color_viridis_c(trans = "log10", name = "NLL") +
    { if(nrow(best_df) > 0)
      ggplot2::geom_line(data = best_df, ggplot2::aes(x = x, y = y), color = "black", linewidth = 1.1)
    } +
    ggplot2::labs(title = paste0(group_display, " -- ", driver_txt),
                  x = driver_txt, y = "Relative response") +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 11, face = "bold"), aspect.ratio = 1)
  
  if(is.null(png_file)) png_file <- paste0("response_curves_", safe_name(group_display), ".png")
  if(!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(plots_dir, png_file)
  ggplot2::ggsave(out_path, plot = p, width = width, height = height, dpi = dpi, units = "in")
  message("Wrote ", out_path)
  
  invisible(df)
}


#' @keywords internal
#' @noRd
# Shared group-name lookup, used by every response-function function below. Reads the
# CONFIRMED 26-name list straight off any candidate's own Biomass export (positions
# verified directly against read output: index 4 = "gag 0", index 11 = "red grouper 1",
# etc. -- exact, not inferred). Indices beyond the tracked-group count (26 in this
# model) do NOT correspond to any of these names -- they're something else (fleets, or
# other internal pool codes not in the tracked Biomass output) that can't be resolved
# from any file available to this script. Returns a list with `name` (display string)
# and `resolved` (TRUE/FALSE) so callers can tell confirmed names from fallbacks.
.lookup_env_group_name <- function(valid, group_index){
  for(i in seq_len(nrow(valid))){
    ref <- tryCatch(get_region_output(0L, "Biomass", valid$run_dir[i], new.env(parent = emptyenv())),
                    error = function(e) NULL)
    if(!is.null(ref)){
      if((group_index + 1L) <= length(ref$group_names) && nzchar(ref$group_names[group_index + 1L])){
        return(list(name = ref$group_names[group_index + 1L], resolved = TRUE))
      } else {
        return(list(name = paste0("group ", group_index, " (beyond tracked group list)"), resolved = FALSE))
      }
    }
  }
  list(name = paste0("group ", group_index), resolved = FALSE)
}


#' @keywords internal
#' @noRd
# Converts a driver_map data.frame (group_index, driver, ...) -- e.g. from
# build_driver_map_from_catalog() -- into the named-character-vector format
# (names = group_index as character, values = driver) used as `driver_labels`
# throughout this section. Lets every response-function plotting function accept a
# `driver_map` argument and automatically get real, catalog-confirmed names instead of
# requiring the caller to hand-build a driver_labels vector. Prefers the `driver_display`
# column (e.g. "depth (gfisher)", includes the catalog's source) when present, since
# that's more informative than the bare driver name used for matching/filtering
# elsewhere (plot_ga_response_by_driver_map()'s `driver_name` argument still matches
# against the plain `driver` column, not this display version).
.driver_labels_from_map <- function(driver_map){
  if(is.null(driver_map)) return(NULL)
  req <- c("group_index", "driver")
  miss <- setdiff(req, names(driver_map))
  if(length(miss) > 0)
    stop("driver_map is missing required column(s): ", paste(miss, collapse = ", "))
  vals <- if("driver_display" %in% names(driver_map)) driver_map$driver_display else driver_map$driver
  stats::setNames(vals, as.character(driver_map$group_index))
}


#' @title Find the environmental-response group_index(es) matching a functional group name.
#' @description Reverse lookup: given a functional group name (or pattern), finds its
#'   position in the CONFIRMED group name list (from any candidate's Biomass export) and
#'   returns that index IF it actually has an environmental response function set in
#'   cmd.txt (not every group does). Only searches within the tracked group list (26
#'   names in this model) -- it cannot find anything for indices beyond that range, since
#'   those aren't resolvable to a name at all (see .lookup_env_group_name()).
#' @param ga_runs Output of resolve_run_dirs().
#' @param group_name_pattern Case-insensitive substring to match against group names,
#'   e.g. "red grouper 1" (exact) or "red grouper" (matches all red grouper stanzas).
#' @param max_check How many candidate cmd.txt files to scan for response-function
#'   presence. Default 5 (same rationale as discover_response_group_indices()).
#' @return data.frame(group_index, group_name) -- one row per matching, response-
#'   function-bearing group. Zero rows (with a message) if nothing matched.
#' @export
find_response_group_index <- function(ga_runs, group_name_pattern, max_check = 5){
  valid <- ga_runs[ga_runs$exists, , drop = FALSE]
  if(nrow(valid) == 0) stop("No candidate folders found on disk -- check ga_runs$exists.")
  
  ref <- NULL
  for(i in seq_len(nrow(valid))){
    ref <- tryCatch(get_region_output(0L, "Biomass", valid$run_dir[i], new.env(parent = emptyenv())),
                    error = function(e) NULL)
    if(!is.null(ref)) break
  }
  if(is.null(ref)) stop("Could not read a domain-average Biomass file from ANY candidate folder.")
  
  hit_idx <- which(grepl(group_name_pattern, ref$group_names, ignore.case = TRUE))
  if(length(hit_idx) == 0){
    message("find_response_group_index(): no group name matched '", group_name_pattern, "'.")
    return(data.frame(group_index = integer(0), group_name = character(0)))
  }
  # group_names[k] corresponds to group_index = k - 1 (see .lookup_env_group_name())
  candidate_indices <- hit_idx - 1L
  
  has_response <- discover_response_group_indices(ga_runs, max_check)
  found <- candidate_indices[candidate_indices %in% has_response]
  if(length(found) == 0){
    message("find_response_group_index(): '", group_name_pattern, "' matched ",
            length(candidate_indices), " group name(s) but NONE have an environmental ",
            "response function set in cmd.txt.")
    return(data.frame(group_index = integer(0), group_name = character(0)))
  }
  
  data.frame(group_index = found, group_name = ref$group_names[found + 1L], stringsAsFactors = FALSE)
}


#' @keywords internal
#' @noRd
# Shared implementation for plot_ga_response_curves_for_group() and
# plot_ga_response_curves_by_indices(): builds one panel per group_index (skipping any
# with no plottable Sigmoid/Trapezoid curve, with a message) and combines via patchwork.
.plot_curves_multi <- function(ga_runs, group_indices, panel_names, driver_labels,
                               facet_ncol, plots_dir, png_file, width, height, dpi,
                               panel_title = NULL, is_fixed = NULL, catalog = NULL, ...){
  panels <- list()
  panel_is_fixed <- logical(0)  # parallel to panels, tracks which are fixed (no color aesthetic)
  all_df <- list()
  has_any_varying <- FALSE
  # NA panel_names (unresolved group) can't be used as a display title -- give it a
  # safe, honest fallback string instead
  panel_names <- ifelse(is.na(panel_names), paste0("group ", group_indices, " (unresolved)"), panel_names)
  # panels/panel_is_fixed are keyed by group_index (always unique), NOT panel_names --
  # panel_names CAN genuinely collide (e.g. "red grouper 1+" is the parsed group_name
  # for BOTH its mortality and foraging red tide functions, confirmed against the
  # sensitivity xlsx catalog: every gag/red grouper red tide group has exactly this
  # pair). Keying by name caused the second entry to silently overwrite the first in
  # the list, leaving has_any_varying=TRUE while the actual panel list ended up with
  # no varying entries -- producing "attempt to select less than one element" when
  # tail(varying_names, 1) returned character(0) below.
  panel_keys <- as.character(group_indices)
  for(k in seq_along(group_indices)){
    gi <- group_indices[k]
    key <- panel_keys[k]
    lab <- if(!is.null(driver_labels) && as.character(gi) %in% names(driver_labels))
      driver_labels[[as.character(gi)]] else NULL
    this_fixed <- !is.null(is_fixed) && isTRUE(is_fixed[k])
    
    if(this_fixed){
      # FIXED (non-GA-tuned) response function -- no per-candidate data exists in any
      # cmd.txt, so evaluate ONE static curve directly from the catalog's own
      # parameters instead of trying (and failing) to read candidate files.
      if(is.null(catalog) || gi > nrow(catalog) || gi < 1){
        message("  [skip] ", panel_names[k], " (idx ", gi, "): marked fixed but no catalog row available.")
        next
      }
      res <- tryCatch(.eval_catalog_response_curve(catalog[gi, , drop = FALSE], x_floor = NULL),
                      error = function(e) NULL)
      if(is.null(res)){
        message("  [skip] ", panel_names[k], " (idx ", gi, "): fixed, but shape_type not ",
                "one of the four handled types.")
        next
      }
      res$run_folder <- "(fixed)"
      res$fitness <- NA_real_
      res$group_index <- gi
      res$group_name <- panel_names[k]
      all_df[[length(all_df) + 1]] <- res
      
      shapes_seen_k <- unique(res$shape_type)
      driver_txt <- if(!is.null(lab)) lab else {
        cats <- unique(SHAPE_DRIVER_CATEGORY[as.character(shapes_seen_k)])
        cats <- cats[!is.na(cats)]
        if(length(cats) >= 1) paste(cats, collapse = " / ") else "Environmental variable (unrecognized shape)"
      }
      pp <- ggplot2::ggplot() +
        ggplot2::geom_line(data = res, ggplot2::aes(x = x, y = y), color = "black", linewidth = 1.1) +
        ggplot2::labs(title = paste0(panel_names[k], " -- ", driver_txt, " (fixed)"),
                      x = driver_txt, y = "Relative response") +
        ggplot2::theme_bw(base_size = 11) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"), aspect.ratio = 1)
      panels[[key]] <- pp
      panel_is_fixed[key] <- TRUE
      next
    }
    
    has_any_varying <- TRUE
    res <- tryCatch(plot_ga_response_curves(ga_runs, group_index = gi, driver_label = lab,
                                            plots_dir = tempdir(),
                                            png_file = paste0(".tmp_curve_", gi, ".png"), ...),
                    error = function(e){
                      message("  [skip] ", panel_names[k], " (idx ", gi, "): ", conditionMessage(e))
                      NULL
                    })
    if(is.null(res)) next
    res$group_index <- gi
    res$group_name <- panel_names[k]
    all_df[[length(all_df) + 1]] <- res
    
    shapes_seen_k <- unique(res$shape_type)
    driver_txt <- if(!is.null(lab)) lab else {
      cats <- unique(SHAPE_DRIVER_CATEGORY[as.character(shapes_seen_k)])
      cats <- cats[!is.na(cats)]
      if(length(cats) >= 1) paste(cats, collapse = " / ") else "Environmental variable (unrecognized shape)"
    }
    best_df <- res[res$fitness == min(res$fitness), , drop = FALSE]
    pp <- ggplot2::ggplot() +
      ggplot2::geom_line(data = res, ggplot2::aes(x = x, y = y, group = run_folder, color = fitness),
                         linewidth = 0.35, alpha = 0.45) +
      ggplot2::scale_color_viridis_c(trans = "log10", name = "NLL") +
      ggplot2::geom_line(data = best_df, ggplot2::aes(x = x, y = y), color = "black", linewidth = 1.1) +
      ggplot2::labs(title = paste0(panel_names[k], " -- ", driver_txt),
                    x = driver_txt, y = "Relative response") +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"), aspect.ratio = 1) +
      # suppressed on every panel by default -- re-enabled on just the LAST panel
      # below, so the NLL legend appears exactly once for the whole combined figure
      # instead of once per panel
      ggplot2::guides(color = "none")
    panels[[key]] <- pp
    panel_is_fixed[key] <- FALSE
  }
  
  if(length(panels) == 0){
    warning(".plot_curves_multi(): nothing plottable among the requested group_indices.")
    return(invisible(NULL))
  }
  
  # re-enable the NLL colorbar on just the last VARYING panel (not necessarily the
  # absolute last panel in the list -- a "fixed" panel has no color aesthetic at all,
  # so targeting it would silently drop the colorbar request instead of erroring,
  # since ggplot2 ignores guides() for aesthetics that were never mapped)
  if(has_any_varying && any(!panel_is_fixed)){
    varying_keys <- names(panel_is_fixed)[!panel_is_fixed]
    last_varying <- tail(varying_keys, 1)
    panels[[last_varying]] <- panels[[last_varying]] +
      ggplot2::guides(color = ggplot2::guide_colorbar(
        position = "right", barheight = grid::unit(height / 3, "in"), barwidth = grid::unit(0.35, "cm")))
  }
  
  p <- patchwork::wrap_plots(panels, ncol = facet_ncol)
  if(!is.null(panel_title))
    p <- p + patchwork::plot_annotation(title = panel_title,
                                        theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 12, face = "bold")))
  
  if(!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(plots_dir, png_file)
  ggplot2::ggsave(out_path, plot = p, width = width, height = height, dpi = dpi, units = "in")
  message("Wrote ", out_path)
  
  invisible(do.call(rbind, all_df))
}


#' @title Plot response curves grouped by DRIVER, using a confirmed group<->driver mapping.
#' @description This is the function that actually does "all depth response functions
#'   for red grouper groups" -- needs a real mapping table as input, since this script
#'   cannot determine which group_index corresponds to which physical driver on its own
#'   (see the shape-function section note above). Once you have that mapping
#'   -- from build_driver_map_from_catalog(), WFS_5min_STconfig.xml, or Ecospace's own
#'   "Environmental Responses" screen in the GUI -- pass it here. IMPORTANT: if
#'   `species_patterns` has more than one element (e.g. c("gag", "red grouper")), each
#'   species gets its OWN SEPARATE output -- they are NEVER combined into one figure,
#'   even though a single call covers both. This matters because a combined figure
#'   would mix functional groups from different species together under one driver,
#'   which is misleading (there are ~26 functional groups but up to 66 response
#'   functions in this model -- multiple response functions can belong to one species
#'   across several drivers, but a figure should never merge two different species'
#'   groups together).
#' @param ga_runs Output of resolve_run_dirs().
#' @param driver_map A data.frame with columns `group_index` (integer, matching cmd.txt's
#'   ECOSPACE_ENVIRONMENTAL_RESPONSE_INDEXED index) and `driver` (character, the physical
#'   variable name, e.g. "depth", "red tide"). An optional `group_name` column can be
#'   included; if omitted, names are looked up automatically via .lookup_env_group_name().
#' @param driver_name The driver to filter to, matched case-insensitively as a substring
#'   against `driver_map$driver`, e.g. "depth" or "red tide" -- match exactly what's in
#'   your own table (build_driver_map_from_catalog() uses the catalog's own `var` values,
#'   e.g. "depth", "temp", "DO", "salinity", "red tide").
#' @param species_patterns Optional character vector of case-insensitive substrings, one
#'   PER SPECIES -- e.g. c("gag", "red grouper") produces two separate outputs, one per
#'   species, each containing only that species' matching groups. NULL (default) = no
#'   species restriction at all, everything matching the driver goes into ONE output
#'   (only appropriate when you actually want all species combined, e.g. a
#'   model-structure overview -- not the default recommendation).
#' @param keep_fitness_quantile Optional fraction in (0, 1]; passed through to
#'   plot_ga_response_curves() to drop the worst-fitting candidates before plotting
#'   (e.g. 0.9 = drop the worst 10% by NLL).
#' @param facet_ncol Number of panel columns. Default 3.
#' @param plots_dir Output folder. Default "plots".
#' @param width,height,dpi ggsave() figure dimensions.
#' @param ... Passed through to the underlying plot_ga_response_curves() calls (e.g.
#'   max_folders).
#' @return Invisibly, a named list keyed by species label (or "all" if
#'   species_patterns is NULL) -- each element is that species' combined long-format
#'   data.frame.
#' @export
plot_ga_response_by_driver_map <- function(ga_runs, driver_map, driver_name,
                                           species_patterns = NULL,
                                           keep_fitness_quantile = NULL,
                                           x_floor = "auto",
                                           facet_ncol = 3,
                                           plots_dir = "plots",
                                           catalog = NULL,
                                           width = 10, height = 6, dpi = 150, ...){
  req_cols <- c("group_index", "driver")
  miss <- setdiff(req_cols, names(driver_map))
  if(length(miss) > 0)
    stop("driver_map is missing required column(s): ", paste(miss, collapse = ", "))
  
  # "auto" floors known-nonnegative physical drivers (currently just depth) at 0;
  # pass an explicit number to override, or NULL for no clipping at all
  if(identical(x_floor, "auto"))
    x_floor <- if(grepl("depth", driver_name, ignore.case = TRUE)) 0 else NULL
  
  base_sel <- driver_map[grepl(driver_name, driver_map$driver, ignore.case = TRUE), , drop = FALSE]
  if(nrow(base_sel) == 0)
    stop("No rows in driver_map matched driver_name = '", driver_name, "'.")
  if(!"is_fixed" %in% names(base_sel)) base_sel$is_fixed <- FALSE
  
  valid <- ga_runs[ga_runs$exists, , drop = FALSE]
  if(!"group_name" %in% names(base_sel))
    base_sel$group_name <- vapply(base_sel$group_index, function(gi) .lookup_env_group_name(valid, gi)$name, character(1))
  
  # one species per iteration, NEVER combined -- see @description
  species_list <- if(is.null(species_patterns)) list(all = NULL) else stats::setNames(as.list(species_patterns), species_patterns)
  
  safe_name <- function(s) gsub("[^A-Za-z0-9._-]+", "_", s)
  out <- list()
  
  for(sp_label in names(species_list)){
    sp_pattern <- species_list[[sp_label]]
    sel <- base_sel
    
    if(!is.null(sp_pattern)){
      # NA group_name (unresolved catalog entries) can never match a species pattern --
      # coerce to "" first so grepl() returns a clean FALSE instead of propagating NA
      # into the row-selection logical vector (which R would otherwise keep as an NA row).
      nm_safe <- ifelse(is.na(sel$group_name), "", sel$group_name)
      keep <- grepl(sp_pattern, nm_safe, ignore.case = TRUE)
      sel <- sel[keep, , drop = FALSE]
    }
    if(nrow(sel) == 0){
      message("plot_ga_response_by_driver_map(): driver '", driver_name, "', species '",
              sp_label, "' -- no matching response functions, skipped.")
      next
    }
    
    n_fixed_here <- sum(sel$is_fixed, na.rm = TRUE)
    message("plot_ga_response_by_driver_map(): driver '", driver_name, "', species '", sp_label,
            "' -> ", nrow(sel), " response function(s) (", n_fixed_here, " fixed): ",
            paste0(sel$group_name, " (idx ", sel$group_index, ifelse(sel$is_fixed, ", fixed", ""), ")",
                   collapse = ", "))
    
    driver_labels <- stats::setNames(sel$driver, as.character(sel$group_index))
    
    png_file <- paste0("response_curves_", safe_name(driver_name), "_", safe_name(sp_label), ".png")
    out[[sp_label]] <- .plot_curves_multi(
      ga_runs, sel$group_index, sel$group_name, driver_labels, facet_ncol,
      plots_dir, png_file, width, height, dpi,
      panel_title = paste0(driver_name, " responses -- ", sp_label),
      is_fixed = sel$is_fixed, catalog = catalog,
      keep_fitness_quantile = keep_fitness_quantile, x_floor = x_floor, ...)
  }
  
  invisible(out)
}


#' @title Read the WFS-MICE environmental/red-tide response function CATALOG.
#' @description Parses "WFS-MICE_env_and_red_tide_response_shape_pars...csv". Each row's
#'   1-indexed POSITION in the file is what cmd.txt's
#'   ECOSPACE_ENVIRONMENTAL_RESPONSE_INDEXED(N) index actually refers to -- confirmed
#'   directly: row N's shape type matches the active shape type at index N in a real
#'   candidate's cmd.txt for all 31 active indices in this model, zero mismatches. This
#'   is NOT the same as the number embedded in the "Function name" text itself (e.g.
#'   "1_sharks_depth_gfisher") -- that leading number is just that row's own
#'   functional-group catalog number, which repeats across the file's later
#'   variable-specific rows (DO/temp/salinity/red tide) and does NOT track row position
#'   past the first pass through the tracked groups. Function name format is
#'   "<fg catalog number>_<fg name>_<var>_<source>", though some rows (e.g.
#'   "gag 1+_red_tide_M0") have no leading number at all.
#' @param path Path to the catalog CSV.
#' @return data.frame(row_num, group_name, var, source, shape_type, par1..par6) -- one
#'   row per catalog entry, in file order. `row_num` is the 1-indexed position that
#'   matches cmd.txt's index directly.
#' @title Read response-function definitions directly from the sensitivity analysis xlsx.
#' @description An alternative to read_env_response_catalog() + build_driver_map_from_catalog()
#'   that is structurally more robust: this file has an EXPLICIT `fxn_num` column giving
#'   each response function's real cmd.txt group_index directly, rather than relying on
#'   row position matching group_index (which silently breaks if the CSV catalog is
#'   ever missing a row, has an extra row, or is reordered -- any single misalignment
#'   cascades into wrong group_index for everything after that point). One row per
#'   unique response function (56 in the version this was verified against): group name
#'   parsing handles both "N_name_driver_shapename" (e.g. "4_gag 0_depth_sigmoid") and
#'   the no-leading-number, driver-shared style used for some red tide/temp functions
#'   (e.g. "gag 1+_red_tide_M0" applies to gag ages 1 through 5+ collectively -- NOT 5
#'   separate per-age functions; "4_gag_temp_normal" applies to every gag age at once).
#'   Confirmed against sensitivity_sp03_5min_phase3_init_2026-07-23.xlsx: gag/red grouper
#'   have the FULL 6-per-species set for depth (ages 0-5+ each separately), but only 2
#'   per species for red tide (age-0-specific + one shared "1+" for ages 1-5+, for both
#'   the mortality and foraging pathways), and just 1 shared function per species for
#'   temp -- this is genuine model structure, not missing data.
#' @param path Path to the sensitivity .xlsx file.
#' @param sheet Sheet name or index. Default 1 (first sheet).
#' @return data.frame(group_index, group_name, driver, resp_type, shape_type, par1..par6)
#'   -- one row per response function found (tag.type == "env response off" rows only;
#'   vulnerability/dispersal rows are excluded since they have no group_index/shape here).
#' @export
read_sensitivity_response_catalog <- function(path, sheet = 1){
  if(!requireNamespace("readxl", quietly = TRUE))
    stop("Package 'readxl' is required. Install it with install.packages('readxl').")
  
  df <- as.data.frame(readxl::read_excel(path, sheet = sheet))
  req <- c("tag.type", "fxn_num", "Function.name", "shape", "resp_type", "var",
           "Param.1", "Param.2", "Param.3", "Param.4", "Param.5", "Param.6")
  miss <- setdiff(req, names(df))
  if(length(miss) > 0)
    stop("read_sensitivity_response_catalog(): missing expected column(s): ", paste(miss, collapse = ", "))
  
  df <- df[df$tag.type == "env response off" & !is.na(df$fxn_num), , drop = FALSE]
  if(nrow(df) == 0)
    stop("read_sensitivity_response_catalog(): no rows with tag.type == 'env response off' found.")
  
  # a given group_index should appear exactly once -- if the sheet has duplicates
  # (e.g. accidentally re-tested), keep the first and warn, rather than silently
  # picking an arbitrary one
  dup <- duplicated(df$fxn_num)
  if(any(dup)){
    message("read_sensitivity_response_catalog(): ", sum(dup), " duplicate fxn_num value(s) found ",
            "(keeping the first occurrence of each): ", paste(unique(df$fxn_num[dup]), collapse = ", "))
    df <- df[!dup, , drop = FALSE]
  }
  
  # group_name: strip an optional leading "<num>_", then locate the driver name (var,
  # with spaces converted to underscores, e.g. "red tide" -> "red_tide") within the
  # remaining string and take everything before it -- same technique as
  # read_env_response_catalog(), verified against every row in the reference file.
  raw_name <- df[["Function.name"]]
  has_num  <- grepl("^\\d+_", raw_name)
  rest     <- ifelse(has_num, sub("^\\d+_", "", raw_name), raw_name)
  df$group_name <- vapply(seq_len(nrow(df)), function(i){
    var_us <- gsub(" ", "_", df$var[i])
    idx <- regexpr(paste0("_", var_us), rest[i], fixed = TRUE)
    if(idx == -1) idx <- regexpr(var_us, rest[i], fixed = TRUE)
    if(idx > 1) substr(rest[i], 1, idx - 1) else rest[i]
  }, character(1))
  
  out <- data.frame(
    group_index = as.integer(df$fxn_num),
    group_name  = df$group_name,
    driver      = df$var,
    resp_type   = df$resp_type,
    shape_type  = suppressWarnings(as.integer(df$shape)),
    par1 = suppressWarnings(as.numeric(df$Param.1)),
    par2 = suppressWarnings(as.numeric(df$Param.2)),
    par3 = suppressWarnings(as.numeric(df$Param.3)),
    par4 = suppressWarnings(as.numeric(df$Param.4)),
    par5 = suppressWarnings(as.numeric(df$Param.5)),
    par6 = suppressWarnings(as.numeric(df$Param.6)),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$group_index), , drop = FALSE]
  rownames(out) <- NULL
  
  message("read_sensitivity_response_catalog(): ", nrow(out), " response function(s) read, ",
          "variables present: ", paste(sort(unique(out$driver)), collapse = ", "), ".")
  out
}


#' @title Build a group_index -> driver map from the sensitivity xlsx catalog.
#' @description Same purpose as build_driver_map_from_catalog(), but for
#'   read_sensitivity_response_catalog()'s output, which has group_index EXPLICIT (no
#'   row-position assumption needed). Every row in the catalog is included; is_fixed
#'   marks the ones discover_response_group_indices() did NOT find active in any
#'   candidate's cmd.txt (fixed parameters, not GA-tuned) -- these are still plotted,
#'   just from the catalog's own static parameters via .eval_catalog_response_curve()
#'   instead of per-candidate cmd.txt data that doesn't exist for them.
#'
#'   ALSO recovers active cmd.txt group_index(es) that have NO row in the sensitivity
#'   catalog at all -- confirmed to happen for per-stanza red tide functions (e.g. gag
#'   ages 1-5+ each have their OWN active, GA-tuned response function in cmd.txt, but
#'   the sensitivity analysis this catalog comes from apparently only tested one
#'   representative "gag 1+" case rather than every stanza individually, so those other
#'   stanzas' functions are genuinely active in the model but simply undocumented in
#'   this specific file). Recovered via shape type + the confirmed SHAPE_DRIVER_CATEGORY
#'   rule (shape 11 is always red tide; 6/9/10 always habitat), which is enough to
#'   assign a driver without needing a catalog row at all; group_name is independently
#'   resolved via .lookup_env_group_name() (reads the Biomass export directly). These
#'   recovered entries get is_fixed = FALSE (they ARE GA-tuned, just uncatalogued) and
#'   route through the normal per-candidate plot_ga_response_curves() path.
#' @param ga_runs Output of resolve_run_dirs().
#' @param catalog Output of read_sensitivity_response_catalog().
#' @param max_check Passed to discover_response_group_indices() and
#'   discover_response_group_shapes().
#' @return data.frame(group_index, group_name, driver, driver_display, source, is_fixed)
#'   -- same shape as build_driver_map_from_catalog()'s output, so both are drop-in
#'   compatible with plot_ga_response_by_driver_map()'s `driver_map`/`catalog` arguments.
#' @export
build_driver_map_from_sensitivity <- function(ga_runs, catalog, max_check = 5){
  active_idx <- discover_response_group_indices(ga_runs, max_check)
  
  is_fixed <- !(catalog$group_index %in% active_idx)
  out <- data.frame(
    group_index    = catalog$group_index,
    group_name     = catalog$group_name,
    driver         = catalog$driver,
    driver_display = catalog$driver,
    source         = NA_character_,
    is_fixed       = is_fixed,
    stringsAsFactors = FALSE
  )
  
  # active cmd.txt index(es) the catalog has no row for at all -- this is exactly the
  # case that was silently dropping per-stanza red tide functions (e.g. gag 1-5+ each
  # having their OWN active response function in cmd.txt, but the sensitivity xlsx
  # apparently only tested a "gag 1+" representative rather than every stanza
  # individually). Recovered here via shape type + SHAPE_DRIVER_CATEGORY (shape 11 is
  # CONFIRMED always red tide; 6/9/10 always habitat -- see that constant's own
  # definition), which is enough to assign a driver WITHOUT needing a catalog row.
  # group_name is independently resolvable via .lookup_env_group_name() (reads the
  # Biomass export directly), so these entries end up fully usable, not just partially.
  missing_active <- setdiff(active_idx, catalog$group_index)
  if(length(missing_active) > 0){
    shapes <- discover_response_group_shapes(ga_runs, max_check)
    valid <- ga_runs[ga_runs$exists, , drop = FALSE]
    
    extra_rows <- list()
    n_recovered <- 0
    for(gi in missing_active){
      shp <- shapes$shape_type[shapes$group_index == gi]
      shp <- if(length(shp) > 0) shp[1] else NA_integer_
      drv <- unname(SHAPE_DRIVER_CATEGORY[as.character(shp)])
      nm  <- .lookup_env_group_name(valid, gi)$name
      if(!is.na(drv)) n_recovered <- n_recovered + 1
      extra_rows[[length(extra_rows) + 1]] <- data.frame(
        group_index = gi, group_name = nm, driver = if(is.na(drv)) NA_character_ else drv,
        driver_display = if(is.na(drv)) NA_character_ else paste0(drv, " (shape ", shp, ", uncatalogued)"),
        source = NA_character_, is_fixed = FALSE, stringsAsFactors = FALSE)
    }
    extra <- do.call(rbind, extra_rows)
    out <- rbind(out, extra)
    out <- out[order(out$group_index), , drop = FALSE]
    message("build_driver_map_from_sensitivity(): ", length(missing_active), " active cmd.txt ",
            "index(es) had no row in the sensitivity catalog -- recovered ", n_recovered,
            " of them via shape type (driver assigned from SHAPE_DRIVER_CATEGORY); ",
            length(missing_active) - n_recovered, " had an unrecognized shape type and remain ",
            "unresolved (driver = NA).")
  }
  
  n_named <- sum(!is.na(out$group_name))
  n_fixed <- sum(out$is_fixed, na.rm = TRUE)
  message("build_driver_map_from_sensitivity(): ", nrow(out), " response function(s) total -- ",
          n_named, " resolved, ", nrow(out) - n_named, " unresolved (group_name = NA), ",
          n_fixed, " of them FIXED (not GA-tuned).")
  out
}


read_env_response_catalog <- function(path){
  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  req <- c("Function name", "Function type", "Param 1", "Param 2", "Param 3",
           "Param 4", "Param 5", "Param 6", "var")
  miss <- setdiff(req, names(df))
  if(length(miss) > 0)
    stop("read_env_response_catalog(): missing expected column(s): ", paste(miss, collapse = ", "))
  
  # row_num = 1-indexed position in the file -- see @description for why this, not the
  # leading number in "Function name", is what actually matches cmd.txt's index.
  df$row_num <- seq_len(nrow(df))
  
  # group_name: strip an optional leading "<num>_" (some rows, e.g. "gag 1+_red_tide_M0"
  # template-style rows, don't have one), then strip the TRAILING (n_var_tokens + 1)
  # "_"-delimited tokens (the var name itself, however many underscore-tokens it
  # occupies once spaces become underscores, plus one more for source/shape-name),
  # keeping whatever's left as the FG name. Using the real `var` column's own token
  # count (rather than assuming a fixed trailing-token count) matters because
  # multi-word variables like "red tide" occupy 2 tokens ("red_tide"), not 1.
  raw_name <- df[["Function name"]]
  has_num  <- grepl("^\\d+_", raw_name)
  rest     <- ifelse(has_num, sub("^\\d+_", "", raw_name), raw_name)
  df$group_name <- vapply(seq_len(nrow(df)), function(i){
    parts <- strsplit(rest[i], "_", fixed = TRUE)[[1]]
    var_i <- df[["var"]][i]
    var_ntok <- if(is.na(var_i) || !nzchar(var_i)) 1L
    else length(strsplit(gsub(" ", "_", var_i), "_", fixed = TRUE)[[1]])
    strip_n <- var_ntok + 1L
    if(length(parts) > strip_n) paste(parts[seq_len(length(parts) - strip_n)], collapse = "_") else rest[i]
  }, character(1))
  
  df$shape_type <- suppressWarnings(as.integer(df[["Function type"]]))
  df$par1 <- suppressWarnings(as.numeric(df[["Param 1"]]))
  df$par2 <- suppressWarnings(as.numeric(df[["Param 2"]]))
  df$par3 <- suppressWarnings(as.numeric(df[["Param 3"]]))
  df$par4 <- suppressWarnings(as.numeric(df[["Param 4"]]))
  df$par5 <- suppressWarnings(as.numeric(df[["Param 5"]]))
  df$par6 <- suppressWarnings(as.numeric(df[["Param 6"]]))
  
  message("read_env_response_catalog(): ", nrow(df), " catalog row(s) read (row position ",
          "1-", nrow(df), " maps directly to cmd.txt's ECOSPACE_ENVIRONMENTAL_RESPONSE_INDEXED ",
          "index), variables present: ", paste(sort(unique(df$var)), collapse = ", "), ".")
  df
}


#' @title Build a confirmed group_index -> driver map from the catalog, by row position.
#' @description For each group_index active in cmd.txt (via
#'   discover_response_group_indices()), takes catalog row N directly (N = the index
#'   itself) -- see read_env_response_catalog() for why row position, not the number
#'   embedded in "Function name", is the correct match.
#'   ALSO includes every OTHER catalog row (marked is_fixed = TRUE) that
#'   discover_response_group_indices() did NOT find active in cmd.txt -- these are
#'   response functions with a FIXED (non-GA-tuned) parameter set. They still belong
#'   to the model structure and should still be plotted, just from the catalog's own
#'   static parameter values rather than per-candidate cmd.txt data (which doesn't
#'   exist for them, since the GA never touches them -- see
#'   plot_ga_response_curves()'s `fixed_params` argument and
#'   .eval_catalog_response_curve() for how these are evaluated). Any index beyond the
#'   catalog's row count still has no possible match and is reported as NA rather than
#'   guessed.
#' @param ga_runs Output of resolve_run_dirs().
#' @param catalog Output of read_env_response_catalog().
#' @param max_check Passed to discover_response_group_indices().
#' @return data.frame(group_index, group_name, driver, driver_display, source,
#'   is_fixed) -- one row per catalog entry PLUS any active cmd.txt index beyond the
#'   catalog's row count (those have group_name/driver/source = NA). is_fixed = TRUE
#'   for rows never found active in cmd.txt (fixed parameters, not GA-tuned).
#' @export
build_driver_map_from_catalog <- function(ga_runs, catalog, max_check = 5){
  active_idx <- discover_response_group_indices(ga_runs, max_check)
  
  # every catalog row, whether or not it's in active_idx -- this is what makes fixed
  # (non-GA-tuned) response functions show up at all, rather than being silently
  # dropped because they never appear in any candidate's cmd.txt tunable-parameter block
  all_idx <- sort(union(active_idx, seq_len(nrow(catalog))))
  
  rows <- list()
  for(gi in all_idx){
    if(gi > nrow(catalog) || gi < 1){
      rows[[length(rows) + 1]] <- data.frame(
        group_index = gi, group_name = NA_character_, driver = NA_character_,
        driver_display = NA_character_, source = NA_character_,
        is_fixed = !(gi %in% active_idx), stringsAsFactors = FALSE)
      next
    }
    row  <- catalog[gi, ]
    disp <- if(!is.na(row$source) && nzchar(row$source))
      paste0(row$var, " (", row$source, ")") else row$var
    rows[[length(rows) + 1]] <- data.frame(
      group_index = gi, group_name = row$group_name, driver = row$var,
      driver_display = disp, source = row$source,
      is_fixed = !(gi %in% active_idx), stringsAsFactors = FALSE)
  }
  
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  n_named <- sum(!is.na(out$group_name))
  n_fixed <- sum(out$is_fixed, na.rm = TRUE)
  message("build_driver_map_from_catalog(): ", nrow(out), " response function(s) total -- ",
          n_named, " resolved, ", nrow(out) - n_named, " unresolved (group_name = NA), ",
          n_fixed, " of them FIXED (not GA-tuned, not found active in cmd.txt -- plotted from ",
          "the catalog's own static parameters instead of per-candidate values).")
  out
}


#' @title Discover every Ecopath group index that has an environmental response function set.
#' @description Scans a handful of candidates' cmd.txt files (not all of them -- the set
#'   of response functions defined is generally the same across every candidate in a run,
#'   since it's part of the model structure rather than something the GA tunes per
#'   candidate, so checking a few is enough) and returns every group index seen in an
#'   active (non-commented) `ECOSPACE_ENVIRONMENTAL_RESPONSE_INDEXED` line.
#' @param ga_runs Output of resolve_run_dirs().
#' @param max_check How many candidate folders to scan before stopping. Default 5.
#' @return Sorted integer vector of group indices found.
#' @title Like discover_response_group_indices(), but also returns each index's shape type.
#' @description Same cmd.txt scanning as discover_response_group_indices(), but keeps
#'   each active group_index's shape type (the first value in its
#'   ECOSPACE_ENVIRONMENTAL_RESPONSE_INDEXED parameter list) alongside it, rather than
#'   discarding that information. Used to recover group_index values that are ACTIVE in
#'   cmd.txt but have no row in a response-function catalog (CSV or sensitivity xlsx) --
#'   for those, shape type alone is enough to confidently assign a driver category via
#'   the confirmed SHAPE_DRIVER_CATEGORY rule (shape 11 is ALWAYS red tide; 6/9/10 are
#'   ALWAYS GAM-derived habitat responses -- see that constant's own definition for the
#'   sourcing), even without knowing which specific catalog entry it corresponds to.
#' @param ga_runs Output of resolve_run_dirs().
#' @param max_check How many candidate folders to scan before stopping. Default 5.
#' @return data.frame(group_index, shape_type) -- one row per distinct active index
#'   found. If an index's shape type is inconsistent across the candidates checked
#'   (shouldn't normally happen, since response-function structure is part of the model
#'   rather than something the GA varies), the FIRST shape type seen for that index is
#'   kept and a warning is issued.
discover_response_group_shapes <- function(ga_runs, max_check = 5){
  valid <- ga_runs[ga_runs$exists, , drop = FALSE]
  if(nrow(valid) == 0) stop("No candidate folders found on disk -- check ga_runs$exists.")
  
  seen <- list()  # group_index (as character) -> shape_type
  inconsistent <- character(0)
  n_checked <- 0
  for(i in seq_len(nrow(valid))){
    if(n_checked >= max_check) break
    cmd_path <- file.path(valid$run_dir[i], "cmd.txt")
    if(!file.exists(cmd_path)) next
    tab <- tryCatch(read_cmd_indexed_params(cmd_path, "ECOSPACE_ENVIRONMENTAL_RESPONSE_INDEXED"),
                    error = function(e) NULL)
    if(is.null(tab) || nrow(tab) == 0) next
    for(r in seq_len(nrow(tab))){
      idx_chr <- as.character(tab$index[r])
      shp <- if(length(tab$params[[r]]) >= 1) tab$params[[r]][1] else NA_real_
      if(idx_chr %in% names(seen)){
        if(!identical(seen[[idx_chr]], shp)) inconsistent <- union(inconsistent, idx_chr)
      } else {
        seen[[idx_chr]] <- shp
      }
    }
    n_checked <- n_checked + 1
  }
  if(length(inconsistent) > 0)
    warning("discover_response_group_shapes(): shape type was NOT consistent across candidates ",
            "for group_index(es): ", paste(inconsistent, collapse = ", "), " -- keeping the first ",
            "value seen for each. This is unexpected (response-function structure should be part ",
            "of the model, not GA-varied) and worth investigating directly.")
  
  out <- data.frame(group_index = as.integer(names(seen)),
                    shape_type = as.integer(unlist(seen)), stringsAsFactors = FALSE)
  out <- out[order(out$group_index), , drop = FALSE]
  message("discover_response_group_shapes(): found ", nrow(out), " active group index(es) with shape type, ",
          "from ", n_checked, " candidate(s) checked.")
  out
}


discover_response_group_indices <- function(ga_runs, max_check = 5){
  valid <- ga_runs[ga_runs$exists, , drop = FALSE]
  if(nrow(valid) == 0) stop("No candidate folders found on disk -- check ga_runs$exists.")
  
  idx_all <- integer(0)
  n_checked <- 0
  for(i in seq_len(nrow(valid))){
    if(n_checked >= max_check) break
    cmd_path <- file.path(valid$run_dir[i], "cmd.txt")
    if(!file.exists(cmd_path)) next
    tab <- tryCatch(read_cmd_indexed_params(cmd_path, "ECOSPACE_ENVIRONMENTAL_RESPONSE_INDEXED"),
                    error = function(e) NULL)
    if(is.null(tab) || nrow(tab) == 0) next
    idx_all <- union(idx_all, tab$index)
    n_checked <- n_checked + 1
  }
  idx_all <- sort(idx_all)
  message("discover_response_group_indices(): found ", length(idx_all), " group index(es) with an ",
          "environmental response function, from ", n_checked, " candidate(s) checked: ",
          paste(idx_all, collapse = ", "))
  idx_all
}

#' @title Spatial residuals for the OLDER age stanzas only (ages 3, 4, 5+).
#' @description Thin wrapper around plot_spatial_residuals_batch(), restricted via its
#'   `stanza_patterns` argument to just the older age-3/4/5+ stanzas for gag and red
#'   grouper -- everything else (ensemble selection, predicted-mean computation, panel
#'   spacing/layout, output naming) is identical to calling
#'   plot_spatial_residuals_batch() directly with the same `stanza_patterns`. Exists as
#'   its own function purely for convenience/readability at the call site, since "older
#'   stanzas only" is a specific, recurring comparison (younger vs. older age classes
#'   tend to show different spatial-fit patterns -- see the paper's discussion notes on
#'   ontogenetic shifts) rather than a one-off filter worth re-typing every time.
#' @inheritParams plot_spatial_residuals_batch
#' @param species_patterns Character vector of case-insensitive substrings identifying
#'   which species' older stanzas to include. Default c("gag", "red grouper") -- the
#'   only two species this model tracks by stanza, so there should rarely be a reason
#'   to override this.
#' @return Same as plot_spatial_residuals_batch(): invisibly, a data.frame summarizing
#'   what was plotted/skipped, one row per raster file scanned (including every
#'   younger-stanza raster, correctly reported as skipped via `stanza_patterns` rather
#'   than silently dropped).
#' @export
plot_spatial_residuals_older_stanzas <- function(raster_dir, ga_runs,
                                                 ens_mode         = c("quantile", "topN", "aic"),
                                                 keep_fitness_quantile = 0.9,
                                                 top_n            = 100,
                                                 top_n_prop       = NULL,
                                                 target_ess       = 100,
                                                 species_patterns = c("gag", "red grouper"),
                                                 raster_pattern = "\\.asc$",
                                                 manual_tokens = NULL,
                                                 max_folders = 200,
                                                 show_land = TRUE,
                                                 land_mask = NULL,
                                                 depth_grid = NULL,
                                                 depth_threshold = 500,
                                                 lon_breaks = NULL,
                                                 lat_breaks = NULL,
                                                 barheight = 2.8,
                                                 barwidth = 0.45,
                                                 facet_ncol = 3,
                                                 base_size = 11,
                                                 plots_dir = "plots",
                                                 width = NULL, height = NULL, dpi = 250){
  ens_mode <- match.arg(ens_mode)
  
  older_stanza_patterns <- c("gag 3", "gag 4", "gag 5+", "red grouper 3", "red grouper 4",
                             "red grouper 5+")
  
  plot_spatial_residuals_batch(
    raster_dir = raster_dir, ga_runs = ga_runs, ens_mode = ens_mode,
    keep_fitness_quantile = keep_fitness_quantile, top_n = top_n, top_n_prop = top_n_prop,
    target_ess = target_ess, species_patterns = species_patterns,
    stanza_patterns = older_stanza_patterns, raster_pattern = raster_pattern,
    manual_tokens = manual_tokens, max_folders = max_folders, show_land = show_land,
    land_mask = land_mask, depth_grid = depth_grid, depth_threshold = depth_threshold,
    lon_breaks = lon_breaks, lat_breaks = lat_breaks, barheight = barheight,
    barwidth = barwidth, facet_ncol = facet_ncol, base_size = base_size,
    plots_dir = plots_dir, width = width, height = height, dpi = dpi)
}
