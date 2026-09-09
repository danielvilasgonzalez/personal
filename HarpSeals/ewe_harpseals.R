library(data.table)
library(ggplot2)

#=========================================================
# Settings
#=========================================================

## FIXED (root cause of "Error in all_dirs[sapply(all_dirs, ...)] :
## invalid subscript type 'list'"): both lines below are macOS-style
## paths ("/Users/..."), but R sessions running on Windows (R's own
## startup banner literally says "Platform: x86_64-w64-mingw32/x64")
## can't have a directory at that path -- `list.dirs(root_dir, ...)`
## then correctly finds ZERO directories. That triggers a base-R
## gotcha completely unrelated to anything else in this script:
## `sapply()` over a zero-length input returns `list()` (an empty
## LIST, not an empty logical vector), and indexing a vector with an
## empty list throws exactly "invalid subscript type 'list'" -- a
## cryptic low-level error that gives no hint the real problem is just
## "root_dir points nowhere on this machine". EDIT root_dir below to
## the actual Windows path to your outputs\sim folder, e.g. something
## like "C:/Users/daniel/Desktop/HarpSeals/outputs/sim/" (forward
## slashes work fine in R on Windows too) -- then re-run. The
## dir.exists() check right after this now fails loudly and clearly
## instead of producing that cryptic error if the path is still wrong.
root_dir <- "/Users/daniel/Desktop/HarpSeals/outputs/sim/"

if(!dir.exists(root_dir)){
  stop(
    "root_dir does not exist on this machine: '", root_dir, "'. ",
    "This is almost always just the wrong path for the computer ",
    "you're currently running R on (e.g. a macOS path left in place ",
    "while running on Windows, or vice versa) -- edit root_dir near ",
    "the top of this script to point at your actual outputs/sim ",
    "folder on THIS machine before re-running."
  )
}

# Pattern used to identify the harp seal group
harp_pattern <- "(?i)(harp.*seal|seal.*harp)"

#=========================================================
# North/South sub-model combination (area-weighted average)
#=========================================================
## The 2018-2020 EwE models are run as separate North/South sub-region
## models, which get combined here into single composite estimates via
## an AREA-WEIGHTED AVERAGE (not a simple mean), using the Wudrick et
## al. ecosystem-model area table:
##   Newfoundland & Labrador Shelf (2J3K): 237,600 km^2 -- 48.0%
##   Grand Banks (3LNO):                   257,400 km^2 -- 52.0%
##   Total (2J3KLNO):                      495,000 km^2 -- 100%
##
## ASSUMPTION (please confirm before trusting combined output): "N"
## sub-models (N50, N80) correspond to Newfoundland & Labrador Shelf,
## "S" sub-models (S50, S20) correspond to Grand Banks -- inferred from
## the N/S letters, NOT independently verified against each model's
## actual NAFO division boundaries. If backwards, swap the two weights
## below.
##
## Two combined Simulations result: "2018-2020_5050" (from N50 + S50)
## and "2018-2020_8020" (from N80 + S20). After combination, `Simulation`
## should only ever contain: "1985-1988", "2013-2015", "2018-2020_8020",
## "2018-2020_5050" -- the raw N50/S50/N80/S20 labels are retired.

area_weight_N <- 237600 / 495000  # Newfoundland & Labrador Shelf, 2J3K (48.0%)
area_weight_S <- 257400 / 495000  # Grand Banks, 3LNO (52.0%)

combined_simulation_map <- list(
  "2018-2020_5050" = c(N = "2018-2020_N50", S = "2018-2020_S50"),
  "2018-2020_8020" = c(N = "2018-2020_N80", S = "2018-2020_S20")
)

## Final Simulation labels, after the N/S rename/combine above --
## defined here (not later, near biomass_results, where it used to
## live) because `check_all_simulations()` needs it right after
## `results` is built, well before biomass_results exists. A static
## constant with no data dependency, so there's no reason it can't
## live here.

simulation_levels <- c(
  "1985-1988",
  "2013-2015",
  "2018-2020_8020",
  "2018-2020_5050"
)

## Shared colour palette for `Simulation`, used consistently on EVERY
## figure that colours or fills by it (previously p5/p7 had their own
## explicit scale_colour/fill_manual with these same values, but some
## other figures had no explicit scale at all and fell back to
## ggplot's default hue palette -- different colours for the same
## Simulation on different figures. Defining it once here and reusing
## it everywhere avoids that drift recurring as more figures get added.

simulation_colors <- c(
  "1985-1988"      = "firebrick",
  "2013-2015"      = "darkorange",
  "2018-2020_8020" = "forestgreen",
  "2018-2020_5050" = "darkorchid"
)

## Shared ggplot theme, used by several figures (D4 Fig 1, D4 Fig 5,
## D3 Fig 2, D3 Fig 3). MOVED here from being defined separately,
## twice, much later in the script (once inside D4 Fig 5's block,
## once inside D3 Fig 2/3's block -- identical content in both, just
## duplicated) -- D4 Fig 1 uses `common_theme` earlier than either of
## those definitions ever ran, which is exactly why it errored with
## "object 'common_theme' not found": R runs top to bottom, and at the
## point D4 Fig 1 referenced it, nothing had defined it yet anywhere
## in the script. Same class of bug as the earlier model_levels_all/
## sim_to_period fixes -- a single definition up front, used
## everywhere, removes the execution-order dependency entirely.

common_theme <- theme_bw(base_size = 13) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold", size = 14),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "bottom",
    legend.title = element_text(face = "plain", size = 13),
    legend.text = element_text(size = 11),
    legend.box = "vertical",
    legend.justification = "left",
    legend.box.just = "left",
    legend.spacing.y = unit(0, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.key.width = unit(1.1, "cm")
  )

## Distinct colour palette for `Model` (the qb-XX/baseline/diet-XX
## sensitivity variants), for figures that colour by Model rather than
## by Simulation -- ordered low-to-high Q/B (blue -> orange), baseline
## in black, diet variants in green/purple so they're visually distinct
## from the Q/B gradient. Not currently used by any figure (D3 Fig 1
## colours by Simulation, not Model) -- kept in case that's wanted
## again later.

sensitivity_model_colors <- c(
  "qb-30"         = "#08306b",
  "qb-15"         = "#4292c6",
  "baseline"      = "black",
  "qb+15"         = "#fd8d3c",
  "qb+30"         = "#a63603",
  "diet-20"       = "#238b45",
  "diet-20pisciv" = "#88419d"
)

## Single authoritative list of every Model variant this script knows
## about, in display order. IMPORTANT: this exists as a standalone
## constant -- not derived from `levels(results$Model)` -- because
## several figures (e.g. the D4 Fig 6 combination-method comparison)
## are built INSIDE the Q/Y section, which runs BEFORE `results$Model`
## is ever converted to a factor later in the script. At that point
## `results$Model` is still plain character, so `levels(results$Model)`
## returns NULL -- and passing `levels = NULL` explicitly to factor()
## is NOT the same as omitting it: R does not fall back to
## auto-detecting levels from the data, it uses the literal empty
## level set, silently turning EVERY value to NA. That was the exact
## cause of the "all points collapsed under an NA x-axis label" bug.
## Using this single constant everywhere sidesteps that execution-order
## dependency entirely -- add any new Model variant here once, and
## every figure that references model_levels_all picks it up.

model_levels_all <- c(
  "baseline",
  "qb-30",
  "qb-15",
  "qb+15",
  "qb+30",
  "diet-20",
  "diet-20pisciv",
  "b-30",
  "b-15",
  "b+15",
  "b+30",
  "PB-50",
  "PB-25",
  "PB+25",
  "PB+50"
)

## Maps every combined Simulation onto one of THREE display periods --
## MOVED here (from much later in the script, near the NCAM section)
## for the same reason `model_levels_all` was moved earlier: several
## time-series figures need this well before line ~3100, and defining
## it only where it was first used meant those earlier figures would
## have hit the same "referenced before it exists" trap.
##
## "2018-2020_8020" and "2018-2020_5050" both map to "2018-2020" --
## they're two population-split SCENARIOS for the same period, not
## sub-regions (the N/S sub-region combination already happened
## upstream), so pooling them here is comparing scenarios, not
## re-doing the area weighting.

sim_to_period <- c(
  "1985-1988"      = "1985-1988",
  "2013-2015"      = "2013-2015",
  "2018-2020_8020" = "2018-2020",
  "2018-2020_5050" = "2018-2020"
)

period_levels <- c("1985-1988", "2013-2015", "2018-2020")

## Model display order for time-series figures that colour by Model:
## baseline listed first (per explicit request), then Q/B low-to-high,
## then diet variants. Distinct from `qb_axis_order` (used by figures
## that put Model on the x-axis, where baseline sits in the middle of
## the Q/B range) -- this one is for the *colour* legend specifically.

model_order_baseline_first <- c(
  "baseline", "qb-30", "qb-15", "qb+15", "qb+30", "diet-20", "diet-20pisciv"
)

## ---- Helper: build a 3-category Period time series from a table
## keyed by raw (4-level) Simulation. Collapses "2018-2020_8020" and
## "2018-2020_5050" into a single "2018-2020" x-position via their
## mean (returned as `mean_line`), while ALSO returning the two raw
## sub-scenario values separately (`raw_points`) so both can be shown
## at that same x-position rather than hiding the spread behind a
## single averaged point. ----

build_period_timeseries <- function(dt, value_col, extra_group_cols = character(0)){
  
  dt <- copy(dt)
  dt[, Period := sim_to_period[as.character(Simulation)]]
  
  by_cols <- c(extra_group_cols, "Period", "Model")
  
  mean_line <- dt[
    !is.na(Period),
    .(Value = mean(get(value_col), na.rm = TRUE)),
    by = by_cols
  ]
  mean_line[, Period := factor(Period, levels = period_levels)]
  
  raw_2020 <- dt[Simulation %in% c("2018-2020_5050", "2018-2020_8020")]
  keep_cols <- c(extra_group_cols, "Simulation", "Model", value_col)
  raw_points <- raw_2020[, keep_cols, with = FALSE]
  setnames(raw_points, value_col, "Value")
  raw_points[, Period := factor("2018-2020", levels = period_levels)]
  
  list(mean_line = mean_line, raw_points = raw_points)
  
}

## Generic combiner: given a data.table with a `Simulation` column,
## some grouping/id columns, and some numeric value columns, replaces
## each N/S pair defined in combined_simulation_map with ONE
## area-weighted-average row. Rows for Simulations not involved in any
## combination (e.g. "1985_1988"/"1985-1988", "2013-2015") pass through
## unchanged except for the 1985_1988 -> 1985-1988 rename.
##
## IMPORTANT: this combines RAW quantities (e.g. Predator_M2, Fishing,
## Biomass, QB, DietFraction), not ratios. If a table also has derived
## ratio columns (e.g. Predator_vs_Fishing = Predator_M2/Fishing),
## don't pass those in value_cols -- recompute them AFTER combining
## from the combined raw components instead. Averaging two ratios
## directly is not the same as (and is less defensible than) the ratio
## of two area-weighted-averaged components.

combine_NS_simulations <- function(dt, id_cols, value_cols){
  
  dt <- copy(dt)
  dt[Simulation == "1985_1988", Simulation := "1985-1988"]
  
  passthrough <- dt[!Simulation %in% unlist(combined_simulation_map)]
  
  ## IMPORTANT FIX: GroupID is NOT used as a join key here, even if
  ## passed in id_cols. GroupID is a model-internal index and is NOT
  ## guaranteed to be consistent across different regional sub-model
  ## EwE files (N50 vs S50 vs N80 vs S20 can legitimately number the
  ## same named group differently) -- or even within the same
  ## Simulation, between a Mortality file's numbering and a Diet
  ## composition file's numbering. Joining on GroupID silently dropped
  ## any group whose IDs didn't happen to line up between N and S,
  ## which is what caused entire species (and, for Q/Y specifically,
  ## some sensitivity Models for the SAME species) to disappear from
  ## combined output with no visible error. The join key is now the
  ## standardized Group NAME plus whatever other id_cols were
  ## requested (Model, Predator, etc.); GroupID, if present in
  ## id_cols, is carried through afterward from the N side as a
  ## reference-only column, not used for matching.
  
  join_cols <- setdiff(id_cols, "GroupID")
  keep_group_id <- "GroupID" %in% id_cols
  
  combined_list <- lapply(names(combined_simulation_map), function(combo_name){
    
    pair <- combined_simulation_map[[combo_name]]
    
    dt_N <- dt[Simulation == pair[["N"]]]
    dt_S <- dt[Simulation == pair[["S"]]]
    
    if(nrow(dt_N) == 0 || nrow(dt_S) == 0){
      cat(
        "\nSkipping combined Simulation '", combo_name, "' -- one or",
        " both of its sub-models ('", pair[["N"]], "', '", pair[["S"]],
        "') not found in this table. Expected until both folders",
        " exist on disk with matching output files.\n", sep = ""
      )
      return(NULL)
    }
    
    dt_N <- copy(dt_N)
    dt_S <- copy(dt_S)
    
    ## Diagnostic: Group names present in only one sub-model. This is
    ## exactly the signal for a naming mismatch (typo, spacing, or
    ## naming-convention difference between the N and S sub-model
    ## files) as opposed to a genuine ecological absence -- surfaced
    ## here so it can be checked directly rather than just showing up
    ## as unexplained blank cells three figures downstream.
    
    groups_N <- unique(dt_N$Group)
    groups_S <- unique(dt_S$Group)
    only_N <- setdiff(groups_N, groups_S)
    only_S <- setdiff(groups_S, groups_N)
    
    if(length(only_N) > 0 || length(only_S) > 0){
      cat(
        "\n", combo_name, ": Group names present in only ONE of the two",
        " sub-models (check for naming mismatches vs. genuine absence",
        " before assuming these species just aren't modeled there):\n",
        "  Only in ", pair[["N"]], ": ",
        if(length(only_N) > 0) paste(only_N, collapse = ", ") else "(none)",
        "\n  Only in ", pair[["S"]], ": ",
        if(length(only_S) > 0) paste(only_S, collapse = ", ") else "(none)",
        "\n", sep = ""
      )
    }
    
    if(keep_group_id) groupid_ref <- unique(dt_N[, .(Group, GroupID)])
    
    setnames(dt_N, value_cols, paste0(value_cols, "_N"))
    setnames(dt_S, value_cols, paste0(value_cols, "_S"))
    
    merged <- merge(
      dt_N[, c(join_cols, paste0(value_cols, "_N")), with = FALSE],
      dt_S[, c(join_cols, paste0(value_cols, "_S")), with = FALSE],
      by = join_cols,
      all = TRUE
    )
    
    check_col <- paste0(value_cols[1], "_N")
    check_col_S <- paste0(value_cols[1], "_S")
    
    merged[, Missing_N := is.na(get(check_col))]
    merged[, Missing_S := is.na(get(check_col_S))]
    
    n_mismatched <- sum(merged$Missing_N | merged$Missing_S)
    
    if(n_mismatched > 0){
      warning(
        combo_name, ": ", n_mismatched, " row(s) exist in only one of",
        " the two sub-models being combined (matched by Group name --",
        " GroupID is no longer used for this join). FIXED (per",
        " explicit request): these are now KEPT, not dropped -- the",
        " MISSING side's contribution is treated as ZERO for every",
        " value column, and the normal area-weighted average is",
        " computed from that (e.g. Haddock, present only in the South",
        " sub-model, gets area_weight_N * 0 + area_weight_S *",
        " Haddock_S). This is an ECOLOGICAL ASSUMPTION -- that a",
        " species absent from one region's own model structure",
        " genuinely has negligible/zero biomass and mortality there,",
        " not just missing data -- not a neutral default. Check the",
        " Missing_N/Missing_S columns on the output (or",
        " HarpSeal_NS_StructuralPresence_Baseline.csv) before trusting",
        " this for any specific species where that assumption might",
        " not hold."
      )
    }
    
    for(col in value_cols){
      n_col <- paste0(col, "_N")
      s_col <- paste0(col, "_S")
      merged[is.na(get(n_col)), (n_col) := 0]
      merged[is.na(get(s_col)), (s_col) := 0]
    }
    
    for(col in value_cols){
      merged[[col]] <- area_weight_N * merged[[paste0(col, "_N")]] +
        area_weight_S * merged[[paste0(col, "_S")]]
      merged[[paste0(col, "_N")]] <- NULL
      merged[[paste0(col, "_S")]] <- NULL
    }
    
    if(keep_group_id){
      merged <- merge(merged, groupid_ref, by = "Group", all.x = TRUE)
    }
    
    merged[, Simulation := combo_name]
    merged
    
  })
  
  combined <- rbindlist(combined_list, fill = TRUE)
  
  rbindlist(list(passthrough, combined), fill = TRUE)
  
}

## Reusable check: warn (loudly, in the console -- not silently) if any
## of the four expected Simulations is missing from a given figure's
## plotting data, rather than a plot that just quietly shows 2 or 3
## models with no explanation. Doesn't stop execution -- the figure
## still renders with whatever Simulations ARE available.

check_all_simulations <- function(sim_present, figure_label){
  
  missing <- setdiff(simulation_levels, unique(as.character(sim_present)))
  
  if(length(missing) > 0){
    warning(
      figure_label, ": missing Simulation(s) ",
      paste(missing, collapse = ", "),
      " -- this figure shows fewer than the expected four models.",
      " For 2018-2020_8020 specifically, this is most likely because",
      " seal_qb_diet_lookup has no QB/diet entries yet for the N80/S20",
      " sub-models (see the GAP note near that table) -- add them",
      " there once available."
    )
  }
  
}

## Species categorization for SOW deliverable 1, which asks for results
## "at the species level, including seal prey species and exploited
## seal prey species" -- two overlapping-but-distinct categories.
##
## THESE ARE PLACEHOLDER VALUES ONLY, used if the Diet composition
## files can't be read for some reason. Once baseline_diet_cache is
## built (in the Q/Y section below), both vectors are OVERWRITTEN with
## values derived directly from your actual diet matrix + fishing
## mortality data -- see "Data-driven species categorization" further
## down in this script. Don't rely on the two lists below for the
## report; they're a guess, not a data-driven result.
##
## NOTE: "Greenland cod" removed -- per HarpSealTEST.xlsx it is NOT its
## own functional group in this model; it's folded into "Other
## piscivorous fish" (see the internal-inconsistency check further
## down). Listing "Greenland cod" here would silently match zero rows.

seal_prey_species <- c(
  "Cod adult", "Cod juvenile", "Arctic cod",
  "Herring", "Capelin", "Greenland halibut"
  # placeholder only -- see note above, this gets overwritten with
  # data-driven values later in the script
)

exploited_species <- c(
  "Cod adult", "Cod juvenile", "Herring", "Capelin", "Greenland halibut"
  # placeholder only -- see note above, this gets overwritten with
  # data-driven values later in the script
)

#=========================================================
# Clean group names
#=========================================================

clean_group_names <- function(x){
  
  y <- trimws(x)
  
  #=========================================================
  # Harp seal
  #=========================================================
  
  i <- grepl(
    "(?i)^seal\\s*harp$|^harp\\s*seal$",
    y,
    perl = TRUE
  )
  y[i] <- "Harp seal"
  
  #=========================================================
  # Adult cod
  #=========================================================
  
  ## Cod >35 cm
  i <- grepl(
    "(?i)^cod\\s*>\\s*35\\s*cm$",
    y,
    perl = TRUE
  )
  y[i] <- "Cod adult"
  
  ## Cod > 35cm
  i <- grepl(
    "(?i)^cod\\s*>\\s*35cm$",
    y,
    perl = TRUE
  )
  y[i] <- "Cod adult"
  
  ## Cod adult
  i <- grepl(
    "(?i)^cod\\s*adult$",
    y,
    perl = TRUE
  )
  y[i] <- "Cod adult"
  
  ## Adult cod
  i <- grepl(
    "(?i)^adult\\s*cod$",
    y,
    perl = TRUE
  )
  y[i] <- "Cod adult"
  
  ## Atlantic cod greater than 35cm
  i <- grepl(
    "(?i)^atlantic\\s*cod\\s*(greater\\s*than|>)\\s*35\\s*cm$",
    y,
    perl = TRUE
  )
  y[i] <- "Cod adult"
  
  ## Cod>35 (no spaces, no "cm")
  i <- grepl(
    "(?i)^cod\\s*>\\s*35$",
    y,
    perl = TRUE
  )
  y[i] <- "Cod adult"
  
  #=========================================================
  # Juvenile cod
  #=========================================================
  
  ## Cod <=35 cm
  i <- grepl(
    "(?i)^cod\\s*<=\\s*35\\s*cm$",
    y,
    perl = TRUE
  )
  y[i] <- "Cod juvenile"
  
  ## Cod <35 cm
  i <- grepl(
    "(?i)^cod\\s*<\\s*35\\s*cm$",
    y,
    perl = TRUE
  )
  y[i] <- "Cod juvenile"
  
  ## Cod juvenile
  i <- grepl(
    "(?i)^cod\\s*juvenile$",
    y,
    perl = TRUE
  )
  y[i] <- "Cod juvenile"
  
  ## Juvenile cod
  i <- grepl(
    "(?i)^juvenile\\s*cod$",
    y,
    perl = TRUE
  )
  y[i] <- "Cod juvenile"
  
  ## Atlantic cod less than 35cm
  i <- grepl(
    "(?i)^atlantic\\s*cod\\s*(less\\s*than|<)\\s*35\\s*cm$",
    y,
    perl = TRUE
  )
  y[i] <- "Cod juvenile"
  
  ## Cod<=35 (no spaces, no "cm")
  i <- grepl(
    "(?i)^cod\\s*<=\\s*35$",
    y,
    perl = TRUE
  )
  y[i] <- "Cod juvenile"
  
  #=========================================================
  # Leave Arctic cod unchanged
  #=========================================================
  
  i <- grepl(
    "(?i)^arctic\\s*cod$",
    y,
    perl = TRUE
  )
  y[i] <- "Arctic cod"
  
  #=========================================================
  # Greenland cod
  #=========================================================
  ## NOTE: this is the group at the centre of the internal-inconsistency
  ## issue in the SOW: stomach-content data cannot distinguish Atlantic
  ## from Greenland cod, but if the Ecopath model carries Greenland cod
  ## as its own functional group, it may receive zero harp seal
  ## predation by construction. Standardizing the name here (instead of
  ## letting it fall through unrenamed, or accidentally merging into
  ## "Cod adult"/"Cod juvenile" via a looser regex) is what lets the
  ## check block below report on it explicitly.
  
  i <- grepl(
    "(?i)^greenland\\s*cod$",
    y,
    perl = TRUE
  )
  y[i] <- "Greenland cod"
  
  #=========================================================
  # Herring
  #=========================================================
  
  i <- grepl(
    "(?i).*herring.*",
    y,
    perl = TRUE
  )
  y[i] <- "Herring"
  
  #=========================================================
  # Capelin
  #=========================================================
  
  i <- grepl(
    "(?i).*capelin.*",
    y,
    perl = TRUE
  )
  y[i] <- "Capelin"
  
  #=========================================================
  # Greenland halibut
  #=========================================================
  
  i <- grepl(
    "(?i).*greenland.*halibut.*",
    y,
    perl = TRUE
  )
  y[i] <- "Greenland halibut"
  
  #=========================================================
  # Silver hake / Saithe / Pollock -- confirmed to be the SAME
  # functional group in this model, just named differently across
  # different periods' EwE exports (e.g. one period's raw CSV calls it
  # "Silver hake", another calls it "Saithe", another "Pollock", or a
  # combined "Silver hake/Saithe"). Without this, they were treated as
  # entirely separate species -- losing continuity across periods, so
  # there was no way to see this group's values for 2013-2015 AND
  # 2018-2020 side by side, since neither period's raw file used the
  # same name for it. Canonical name spells out the equivalence
  # explicitly (rather than picking just one of the three names) so
  # the report documents this FG-identity assumption directly, the
  # same way "Other piscivorous fish" documents the Greenland cod
  # case above.
  #=========================================================
  ## FIXED: the previous pattern was ANCHORED (^...$) to a small set of
  ## exact phrasings ("silver hake", "silver hake/saithe",
  ## "saithe/pollock", "pollock") -- a raw label using a DIFFERENT
  ## connector, e.g. "Silver hake and Pollock" (the word "and" instead
  ## of "/"), matched NONE of those alternatives and passed through
  ## uncleaned. That's exactly why "Silver hake/Saithe/Pollock" (the
  ## rows that DID match) and "Silver hake and pollock" (the raw label
  ## that didn't) showed up as two separate groups instead of merging.
  ## Switched to an UNANCHORED "contains any of these terms" match --
  ## the same `.*term.*` style already used for Herring/Capelin/
  ## Greenland halibut just above -- so any raw label containing
  ## "silver hake", "saithe", or "pollock" anywhere in it merges here,
  ## regardless of connector word, ordering, or how many of the three
  ## names are combined.
  i <- grepl(
    "(?i).*(silver\\s*hake|saithe|pollock).*",
    y,
    perl = TRUE
  )
  y[i] <- "Silver hake/Saithe/Pollock"
  
  #=========================================================
  # Other piscivorous fish
  #=========================================================
  i <- grepl( "(?i)^other\\s+piscivorous\\s+fish$", y, perl = TRUE ) 
  y[i] <- "Other piscivorous fish" 
  
  #=========================================================
  # Adult American plaice
  #=========================================================
  ## FIXED: previously only matched the symbol form WITHOUT "cm"
  ## (e.g. "amplaice>35") or the fully spelled-out words form WITH
  ## "cm" ("american plaice greater than 35cm") -- there was no
  ## alternative covering the symbol form WITH "cm"
  ## ("American Plaice > 35 cm"), even though that's exactly the
  ## convention Cod's own regex above supports (both
  ## "cod>35cm" AND "cod>35" are matched for Cod). If a regional
  ## sub-model's raw CSV used the symbol+cm form for this species,
  ## it fell through uncleaned -- which then breaks the N/S join in
  ## combine_NS_simulations() (joined on cleaned Group name), making
  ## the species silently vanish from the combined 2018-2020 output
  ## as "present in only one sub-model", not from any real data gap.
  ## `(\\s*cm)?` makes "cm" optional after the symbol form so both
  ## variants match.
  i <- grepl( "(?i)^(amplaice\\s*>\\s*35(\\s*cm)?|american\\s+plaice\\s*>\\s*35(\\s*cm)?|american\\s+plaice\\s+greater\\s+than\\s+35\\s*cm)$", y, perl = TRUE )
  y[i] <- "American Plaice adult"
  
  #=========================================================
  # Juvenile American plaice
  #=========================================================
  ## FIXED: same gap as the adult pattern above, mirrored for "<=".
  i <- grepl( "(?i)^(amplaice\\s*<=\\s*35(\\s*cm)?|american\\s+plaice\\s*<=\\s*35(\\s*cm)?|american\\s+plaice\\s+less\\s+than\\s+35\\s*cm)$", y, perl = TRUE )
  y[i] <- "American Plaice juvenile"
  
  #=========================================================
  # Small benthivorous fish
  #=========================================================
  i <- grepl(
    "(?i)^(small\\s+benthivorous\\s+fish|small\\s+benthivourous\\s+fish|other\\s+small\\s+benthivorous\\s+fish|other\\s+s\\s+benthivorous\\s+fish)$",
    y,
    perl = TRUE
  )
  y[i] <- "Small benthivorous fish"
  
  
  #=========================================================
  # Medium benthivorous fish
  #=========================================================
  i <- grepl(
    "(?i)^(medium\\s+benthivorous\\s+fish|medium\\s+benthivourous\\s+fish|other\\s+medium\\s+benthivorous\\s+fish|other\\s+medium\\s+benthivourous\\s+fish|other\\s+m\\s+benthivorous\\s+fish|other\\s+m\\s+benthivourous\\s+fish)$",
    y,
    perl = TRUE
  )
  y[i] <- "Medium benthivorous fish"
  
  
  #=========================================================
  # Large benthivorous fish
  #=========================================================
  i <- grepl(
    "(?i)^(large\\s+benthivorous\\s+fish|large\\s+benthivourous\\s+fish|other\\s+large\\s+benthivorous\\s+fish|other\\s+large\\s+benthivourous\\s+fish|other\\s+l\\s+benthivorous\\s+fish|other\\s+l\\s+benthivourous\\s+fish)$",
    y,
    perl = TRUE
  )
  y[i] <- "Large benthivorous fish"
  
  return(y) 
}
#=========================================================
# Compare fishing vs harp seal predation
#=========================================================

compare_predator_vs_fishing <- function(mort, pred){
  
  names(mort)[1] <- "GroupID"
  
  names(pred)[1] <- "GroupID"
  names(pred)[2] <- "Group"
  
  mort <- as.data.table(mort)
  pred <- as.data.table(pred)
  
  mort[, GroupID := as.integer(GroupID)]
  pred[, GroupID := as.integer(GroupID)]
  
  ## Standardize names
  
  mort[, `Group name` := clean_group_names(`Group name`)]
  pred[, Group := clean_group_names(Group)]
  
  ## Check duplicated names after cleaning
  
  dup <- mort[, .N, by = `Group name`][N > 1]
  
  if(nrow(dup) > 0){
    
    warning("Duplicated functional groups after cleaning:")
    
    print(dup)
    
  }
  
  ## Find harp seal automatically
  
  predator_row <- mort[
    grepl(
      harp_pattern,
      `Group name`,
      perl = TRUE
    )
  ]
  
  if(nrow(predator_row) == 0)
    stop("No harp seal group found.")
  
  if(nrow(predator_row) > 1)
    warning("Multiple harp seal groups found. Using first match.")
  
  predator_row <- predator_row[1]
  
  predator_id <- as.character(predator_row$GroupID)
  
  predator_name <- predator_row$`Group name`
  
  if(!predator_id %in% names(pred))
    stop("Predator column ", predator_id, " not found.")
  
  
  cat(
    "Predator:",
    predator_name,
    "(GroupID =", predator_id, ")\n"
  )
  
  ## Extract harp seal predation
  
  out <- pred[, .(
    GroupID,
    Group,
    Predator_M2 = get(predator_id)
  )]
  
  ## Merge mortality
  ## NOTE: "Prod/biom or Z" is total mortality Z (at Ecopath equilibrium,
  ## Z = P/B). "+ Other mort. rate (/year)" is residual natural
  ## mortality not otherwise accounted for (M0). Both added here to
  ## support the F / M2 / Z proportion plot (Figure 10) -- they weren't
  ## needed for the M2/F ratio alone, which is why they weren't pulled
  ## in originally.
  
  out <- merge(
    out,
    mort[, .(
      GroupID,
      Fishing = `= Fishing mort. rate`,
      Total_M2 = `+ Predation mort. rate (/year)`,
      Z = `Prod/biom or Z`,
      OtherMortality = `+ Other mort. rate (/year)`
    )],
    by = "GroupID",
    all.x = TRUE
  )
  
  out[is.na(Predator_M2), Predator_M2 := 0]
  out[is.na(Fishing), Fishing := 0]
  out[is.na(Total_M2), Total_M2 := 0]
  out[is.na(Z), Z := 0]
  out[is.na(OtherMortality), OtherMortality := 0]
  
  ## FIXED: previously `out <- out[Fishing > 0]` dropped every Group
  ## with zero fishing mortality OUT OF `results` ENTIRELY, this early
  ## and this permanently -- before combine_NS_simulations() even ran,
  ## before Figure 10's mortality-composition chart ever got a chance
  ## to see it. That's the right call for M2/F specifically (dividing
  ## by zero fishing is undefined), but WRONG for anything that uses
  ## Total_M2/OtherMortality independently of fishing -- a species can
  ## have real predation mortality and zero fishing and still belong
  ## on Figure 10 (F + Total_M2 + OtherMortality proportions) with a
  ## correct 0% fishing segment. It also silently caused species to
  ## vanish from combined 2018-2020 Simulations whenever they had
  ## Fishing == 0 in just ONE of the two N/S regional sub-models,
  ## since combine_NS_simulations() then saw the group present in only
  ## one side and dropped it as unmatched -- not a naming problem, not
  ## a missing-data problem, just this filter running upstream of the
  ## combination step.
  ##
  ## Every row is now KEPT. Predator_vs_Fishing is NA (not silently
  ## missing along with the whole row) wherever Fishing == 0, so M2/F-
  ## specific tables/figures still correctly show no ratio there --
  ## they just do it via an explicit NA on a real row, rather than the
  ## row not existing at all. Anything that specifically needs
  ## Fishing > 0 (e.g. the "exploited species" definition further
  ## down, which means ACTUALLY fished, not just theoretically able to
  ## be) now filters on that explicitly, at its own point of use,
  ## instead of inheriting it implicitly from this having already
  ## happened here.
  
  out[, Predator_vs_Fishing := fifelse(
    Fishing > 0,
    Predator_M2 / Fishing,
    NA_real_
  )]
  
  out[, Predator_fraction_M2 :=
        fifelse(
          Total_M2 > 0,
          Predator_M2 / Total_M2,
          NA_real_
        )]
  
  out[, Predator := predator_name]
  
  return(out)
  
}

#=========================================================
# Find EwE models
#=========================================================

all_dirs <- list.dirs(
  root_dir,
  recursive = TRUE,
  full.names = TRUE
)

## FIXED: `sapply()` over a zero-length `all_dirs` returns `list()`
## (an empty LIST, not an empty logical vector) -- indexing with that
## then throws "invalid subscript type 'list'", a cryptic error with
## no hint that the real problem is upstream (root_dir pointing
## nowhere on this machine -- see the dir.exists() check added near
## root_dir's definition, which should now catch that case earlier
## and more clearly). `vapply(..., logical(1))` is used here instead
## of `sapply()` as a second, independent safety net: unlike sapply,
## vapply ALWAYS returns a plain logical vector of the declared
## length -- including length 0 when `all_dirs` is empty -- so this
## line itself can no longer produce that specific crash even if
## something upstream still lets an empty/malformed all_dirs through.
model_dirs <- all_dirs[
  vapply(all_dirs, function(x){
    
    mort <- list.files(
      x,
      pattern = "Mortalit.*\\.csv$",
      ignore.case = TRUE
    )
    
    pred <- list.files(
      x,
      pattern = "Predation.*mortality.*\\.csv$",
      ignore.case = TRUE
    )
    
    length(mort) > 0 &&
      length(pred) > 0
    
  }, logical(1))
]

cat(length(model_dirs), "simulation folders found\n")

#=========================================================
# Process models
#=========================================================

results <- rbindlist(
  
  lapply(model_dirs, function(model){
    cat("\nProcessing:", model, "\n")
    
    mort_file <- list.files(
      model,
      pattern = "Mortalit.*\\.csv$",
      full.names = TRUE,
      ignore.case = TRUE
    )[1]
    
    pred_file <- list.files(
      model,
      pattern = "Predation.*mortality.*\\.csv$",
      full.names = TRUE,
      ignore.case = TRUE
    )[1]
    
    mort <- read.csv(
      mort_file,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      dec = ","
    )
    
    pred <- read.csv(
      pred_file,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      dec = ","
    )
    
    mort <- as.data.table(mort)
    pred <- as.data.table(pred)
    
    setnames(mort, 1, "GroupID")
    setnames(pred, 1, "GroupID")
    setnames(pred, 2, "Group")
    
    cat("\nChecking group IDs...\n")
    
    tmp <- merge(
      mort[, .(GroupID, MortName = `Group name`)],
      pred[, .(GroupID, PredName = Group)],
      by = "GroupID",
      all = TRUE
    )
    
    print(tmp)
    
    out <- compare_predator_vs_fishing(
      mort,
      pred
    )
    
    ## Folder structure:
    ## sim/1985_1988/baseline
    
    out[, Simulation := basename(dirname(model))]
    out[, Model := basename(model)]
    
    out
    
  }),
  
  fill = TRUE
  
)

#=========================================================
# Combine N/S sub-models (area-weighted) + rename 1985_1988
#=========================================================
## IMPORTANT: `results_raw` is a snapshot taken BEFORE the rename/
## combine step below, keeping the RAW folder-derived Simulation
## labels (e.g. "1985_1988", "2018-2020_N50", "2018-2020_S50",
## "2018-2020_N80", "2018-2020_S20"). The Q/Y section further down
## needs this: it iterates over model_dirs directly and looks things
## up by `sim_name <- basename(dirname(model))`, which is always the
## RAW folder name, never the renamed/combined one. Looking those up
## against the renamed `results` table below (instead of this raw
## snapshot) was a real bug: it silently returned zero rows for every
## Simulation except "2013-2015" (the one label the rename/combine
## step never touches), which is exactly why Q/Y, the Q/B sensitivity
## trend, and the M2/F-vs-Q/Y agreement plot were only ever showing
## 2013-2015 -- not a data-availability gap, a stale-lookup bug.

results_raw <- copy(results)

#=========================================================
# Diagnostic: Predator_M2 > Total_M2 (mortality composition
# shortfall check)
#=========================================================
## Total_M2 (all-predator predation mortality) and Predator_M2 (harp-
## seal-specific predation mortality) come from TWO SEPARATE EwE
## output files -- Total_M2 from Mortalities.csv's own "+ Predation
## mort. rate (/year)" column, Predator_M2 from the harp seal's column
## in Predation_mortality_rates.csv (see compare_predator_vs_fishing()
## above). Nothing in the code guarantees Total_M2 >= Predator_M2 --
## it's an assumption Figure 10 (mortality composition) relies on via
## `pmax(Total_M2 - Predator_M2, 0)`. If EwE's own two files disagree
## for a species (rounding/export precision, or a genuine reporting
## quirk), Predator_M2 can end up marginally LARGER than Total_M2,
## which clips Figure 10's "other predation" slice to zero instead of
## going negative -- the four stacked fractions then sum to slightly
## MORE than 1, not less. On the plot, that used to make the topmost
## (Fishing) segment vanish entirely rather than visibly overshoot,
## because ggplot's default scale behavior drops out-of-range stacked
## values instead of clipping them -- now fixed via `oob =
## scales::squish` on Figure 10's y-axis, so affected bars correctly
## render at 100% with every segment still visible, instead of falling
## short with a slice missing. This checks BOTH the raw (pre-combine,
## per raw Simulation) data and will be checked again after combining,
## so a mismatch can be traced to its actual source: already present
## in a single raw model folder's own files (an EwE/source-data
## precision issue, not fixable in this script), vs. only appearing
## after the N/S combination (which WOULD point to a bug in
## combine_NS_simulations itself, since linear averaging should
## preserve the inequality if it holds in both raw inputs).

check_predator_vs_total_m2 <- function(dt, label){
  
  bad <- dt[Predator_M2 > Total_M2 & Total_M2 > 0]
  
  if(nrow(bad) > 0){
    
    bad[, Excess := Predator_M2 - Total_M2]
    bad[, ExcessPct := 100 * Excess / Total_M2]
    
    cat("\n===== DIAGNOSTIC: Predator_M2 > Total_M2 (", label, ") =====\n", sep = "")
    cat("These rows push Figure 10's stacked mortality-composition",
        "fractions to sum to slightly MORE than 100% (the 'other",
        "predation' slice gets clipped to zero instead of going",
        "negative). Total_M2 and Predator_M2 come from two separate",
        "EwE files, so this reflects a genuine inconsistency in the",
        "source model output, not a merge bug in this script --",
        "small ExcessPct values are likely rounding/export precision;",
        "large ones are worth checking directly against the raw",
        "Mortalities.csv / Predation_mortality_rates.csv files.\n\n")
    print(bad[order(-ExcessPct), .(
      Simulation, Model, Group, Predator_M2, Total_M2, Excess, ExcessPct
    )])
    cat("=================================================================\n")
    
  }
  
  invisible(bad)
  
}

check_predator_vs_total_m2(results_raw, "raw, pre-combine")

#=========================================================
# Diagnostic: why Q/B and biomass sensitivity runs match so closely
#=========================================================
## CORRECTED explanation (an earlier version of this comment
## incorrectly suggested this might mean duplicated/copied EwE output
## files -- it does not; the b-XX and qb-XX runs genuinely use
## different harp seal biomass and Q/B inputs respectively, confirmed
## directly against Mortalities.csv). The near-identical M2/F and Q/Y
## values between matched qb-XX/b-XX pairs (qb-30 \u2248 b-30, etc.) are a
## MATHEMATICAL CONSEQUENCE of how Ecopath computes predation
## mortality, not a data or script problem:
##
##   M2_i = sum_j (Q_j * DC_ji),  where Q_j = (Q/B)_j * B_j
##
## M2 on a prey species depends only on the predator's TOTAL
## consumption Q_j -- the product of Q/B and Biomass -- never on the
## two factors individually. Since b-XX and qb-XX both use the SAME
## percentage range (\u00b115%, \u00b130%) applied to whichever single factor
## each is designed to test, the resulting product Q_seal is
## necessarily the same either way:
##   qb-30: (QB_baseline x 0.70) x B_baseline
##   b-30:   QB_baseline x (B_baseline x 0.70)
## These are algebraically identical. This block confirms that
## numerically for Cod adult (any residual difference should be pure
## rounding, from HarpSeal_biomass.csv's stored precision, not a real
## distinction), across every raw Simulation.
##
## PRACTICAL IMPLICATION: testing Q/B and biomass sensitivity at
## matching percentage ranges cannot reveal information the other
## axis hasn't already shown, since Ecopath is blind to which factor
## changed. If the two axes are meant to carry genuinely different
## information (as their separate pedigree-uncertainty categories in
## the SOW imply), each should use a percentage range sized to that
## specific parameter's own real-world uncertainty, not the same
## arbitrary range for both.

qb_b_pairs <- list(
  c(qb = "qb-30", b = "b-30"),
  c(qb = "qb-15", b = "b-15"),
  c(qb = "qb+15", b = "b+15"),
  c(qb = "qb+30", b = "b+30")
)

qb_b_compare <- rbindlist(
  lapply(qb_b_pairs, function(pair){
    
    qb_rows <- results_raw[
      Group == "Cod adult" & Model == pair[["qb"]],
      .(Simulation, Predator_M2, Fishing)
    ]
    b_rows <- results_raw[
      Group == "Cod adult" & Model == pair[["b"]],
      .(Simulation, Predator_M2, Fishing)
    ]
    
    if(nrow(qb_rows) == 0 || nrow(b_rows) == 0) return(NULL)
    
    merged <- merge(qb_rows, b_rows, by = "Simulation", suffixes = c("_qb", "_b"))
    merged[, `:=`(
      QB_Model = pair[["qb"]],
      B_Model  = pair[["b"]],
      M2_Identical = abs(Predator_M2_qb - Predator_M2_b) < 1e-9,
      F_Identical  = abs(Fishing_qb - Fishing_b) < 1e-9
    )]
    merged
    
  }),
  fill = TRUE
)

if(!is.null(qb_b_compare) && nrow(qb_b_compare) > 0){
  
  cat("\n===== DIAGNOSTIC: Q/B vs biomass sensitivity -- same result, and why =====\n")
  cat("Compares Cod adult's RAW Predator_M2 and Fishing, read directly\n")
  cat("from each model folder's OWN Mortalities.csv / Predation_mortality_\n")
  cat("rates.csv, between paired qb-XX and b-XX folders, per raw Simulation.\n")
  cat("M2_Identical/F_Identical = TRUE is EXPECTED here, not a data problem:\n")
  cat("M2 on a prey species depends only on the predator's total consumption\n")
  cat("(Q/B x Biomass), never on the two factors separately -- so scaling\n")
  cat("either one by the same percentage produces the same downstream M2.\n")
  cat("Any tiny (non-zero but negligible) difference below reflects\n")
  cat("HarpSeal_biomass.csv's stored rounding precision, not a real\n")
  cat("distinction between the two sensitivity axes.\n\n")
  print(qb_b_compare[order(Simulation, QB_Model)])
  
  near_identical <- qb_b_compare[
    abs(Predator_M2_qb - Predator_M2_b) > 1e-9 &
      abs(Predator_M2_qb - Predator_M2_b) / pmax(abs(Predator_M2_qb), 1e-9) > 0.01
  ]
  
  if(nrow(near_identical) > 0){
    warning(
      "Some qb-XX/b-XX pairs differ by MORE than rounding error (>1%) --",
      " this is unexpected given the shared-percentage-range argument",
      " above, and is worth checking directly (e.g. a mismatched",
      " multiplier, or the two axes not actually using the same",
      " percentage range for that particular Simulation/Model pair)."
    )
    print(near_identical)
  }
  
  cat("===============================================================================\n")
  
} else {
  
  cat("\nSkipping qb-XX vs b-XX diagnostic -- no matching Model pairs found",
      "in results_raw for Cod adult yet (expected before all sensitivity",
      "models have been run).\n")
  
}

## Combine RAW components only (Predator_M2, Fishing, Total_M2, Z,
## OtherMortality) via area-weighted average, then RECOMPUTE the
## derived ratio columns (Predator_vs_Fishing, Predator_fraction_M2)
## from those combined components -- averaging the ratios directly
## instead would be a different (less defensible) calculation.

results <- combine_NS_simulations(
  results,
  id_cols    = c("GroupID", "Group", "Model"),
  value_cols = c("Predator_M2", "Fishing", "Total_M2", "Z", "OtherMortality")
)

## Same fix as inside compare_predator_vs_fishing() above: NA where
## Fishing == 0 (undefined ratio), not silently propagating a
## divide-by-zero and not requiring the row to have been dropped
## earlier to avoid one.
results[, Predator_vs_Fishing := fifelse(
  Fishing > 0,
  Predator_M2 / Fishing,
  NA_real_
)]
results[, Predator_fraction_M2 := fifelse(
  Total_M2 > 0,
  Predator_M2 / Total_M2,
  NA_real_
)]

## ---- Targeted diagnostic for Haddock, per explicit request: user
## reports Haddock has genuine harp seal predation AND fishing
## mortality in the South baseline models (2018-2020_S50 and
## 2018-2020_S20), but the combined 2018-2020_5050/8020 M2/F in Table 2
## was coming out as zero. NOTE on the underlying math, so the output
## below is easy to interpret: when a Group is zero-filled on the
## North side for BOTH Predator_M2 and Fishing (either because it's
## genuinely absent from the North's own model structure, or present
## there with real zeros), the area weight on the North side multiplies
## 0/0 and cancels out of the ratio entirely --
##   (area_weight_N*0 + area_weight_S*M2_S) /
##   (area_weight_N*0 + area_weight_S*Fishing_S)
##     = (area_weight_S*M2_S) / (area_weight_S*Fishing_S) = M2_S/Fishing_S
## -- i.e. the combined M2/F for a South-only species should equal
## EXACTLY the raw South ratio, not a diluted or zeroed-out version of
## it. If the printed "combined" ratio below is 0 or NA despite the
## "raw" block showing a real nonzero Predator_M2 and Fishing for
## 2018-2020_S50/S20, that points to something more specific breaking
## the merge for this species in particular (a Group-name mismatch
## between the North/South Mortality files after clean_group_names(),
## or a GroupID mismatch between Mortality.csv and
## Predation_mortality_rates.csv WITHIN the South model itself) rather
## than the zero-fill logic -- share this printed output to pin it
## down further.
if(exists("results_raw")){
  cat("\n===== Haddock diagnostic: RAW per-region values (before N/S",
      "combination) feeding the 2018-2020_5050/8020 area-weighted",
      "average =====\n")
  print(
    results_raw[
      Group == "Haddock" & Model == "baseline",
      .(Simulation, Model, Fishing, Predator_M2)
    ]
  )
}
cat("\n===== Haddock diagnostic: COMBINED (post-N/S-average) values --",
    "these are exactly what Table 2 uses for the 2018-2020_5050/8020",
    "columns =====\n")
print(
  results[
    Group == "Haddock" & Model == "baseline" &
      Simulation %in% c("2018-2020_5050", "2018-2020_8020"),
    .(Simulation, Fishing, Predator_M2, Predator_vs_Fishing, Missing_N, Missing_S)
  ]
)

check_predator_vs_total_m2(results, "combined, post-N/S-average")

check_all_simulations(results$Simulation, "results (feeds Figures D1-1/2/3/4, D2-1, D4-5)")

## Diagnostic: which Group names appear in every Simulation vs. only
## some. A group present in 1985-1988/2013-2015 but absent from
## 2018-2020_8020/5050 (or vice versa) is EXACTLY what produces the
## sparse/blank cells in the species-level heatmaps (Figures D1-4,
## D1-5) -- this print-out lets you check by eye whether that's a real
## naming mismatch between the different regional EwE model files
## (e.g. a typo or spacing difference) or a genuine ecological absence
## (species not modeled in that region), rather than discovering it
## only as unexplained blanks several figures downstream.

group_by_sim <- split(results$Group, results$Simulation)
all_groups   <- sort(unique(results$Group))

group_coverage <- data.table(
  Group = all_groups
)
for(sim in names(group_by_sim)){
  group_coverage[[sim]] <- all_groups %in% unique(group_by_sim[[sim]])
}

incomplete_groups <- group_coverage[
  rowSums(group_coverage[, -1, with = FALSE]) < length(group_by_sim)
]

if(nrow(incomplete_groups) > 0){
  cat(
    "\nGroups NOT present in every Simulation (check for naming",
    "mismatches between regional model files vs. genuine absence):\n"
  )
  print(incomplete_groups)
} else {
  cat("\nEvery Group name is present in all", length(group_by_sim), "Simulations -- no naming mismatches detected.\n")
}

setorder(
  results,
  Simulation,
  Model,
  Group
)

## Check duplicates

dup <- results[
  ,
  .N,
  by = .(
    Simulation,
    Model,
    Group
  )
][N > 1]

if(nrow(dup) > 0){
  
  cat("\nDuplicated groups detected:\n")
  
  print(dup)
  
}

## Check standardized names

cat("\nGroups found:\n")
print(sort(unique(results$Group)))

## MOVED here (from much later in the script, right before the old
## "Prepare plotting data" section) so `results$Model`/`results$Simulation`
## become real factors immediately after `results` itself is renamed/
## combined, instead of staying plain character for ~1,650 intervening
## lines. That gap was a live risk: it's the exact same execution-order
## trap that caused the `model_levels_all`/`sim_to_period` NA-collapse
## bugs found and fixed earlier (levels() on a non-factor column
## returns NULL, and passing that NULL explicitly to factor() silently
## turns every value to NA). Moving this here removes that window
## entirely for `results` -- any diagnostic or figure from this point
## onward can safely call levels(results$Model)/levels(results$Simulation).

#=========================================================
# Set factor order
#=========================================================

## UPDATED: b-30/b-15/b+15/b+30 are the REAL standing-biomass
## sensitivity axis now that HarpSeal_biomass.csv exists -- these
## supersede the PB+50/PB-50/PB+25/PB-25 P/B-proxy placeholders
## (`seal_pb_reference`, still documented above but no longer the
## active biomass leg). Kept PB-XX in the level list too in case those
## runs get produced later, but the coverage check below now looks for
## b-XX specifically. Uses `model_levels_all` (defined once near the
## top of this script) rather than a second hardcoded copy of the same
## list, so the two can't silently drift apart again.

results[, Model := factor(Model, levels = model_levels_all)]

## Fail loudly if the biomass sensitivity runs haven't actually been
## produced yet -- don't let the plots/tables below silently show only
## diet and Q/B sensitivity while claiming full pedigree-informed
## coverage.

biomass_models_found <- intersect(
  c("b-30", "b-15", "b+15", "b+30"),
  unique(as.character(results$Model))
)

if(length(biomass_models_found) == 0){
  warning(
    "No 'b-30'/'b-15'/'b+15'/'b+30' Model folders found under ",
    "root_dir. The Q/B and diet sensitivity legs are covered, but the ",
    "biomass leg required by the SOW (pedigree-informed biomass ",
    "uncertainty) is currently missing -- these EwE runs still need to ",
    "be produced before the sensitivity section of the report can be ",
    "called complete."
  )
} else if(length(biomass_models_found) < 4){
  warning(
    "Only found biomass-sensitivity Model(s): ",
    paste(biomass_models_found, collapse = ", "),
    " -- expected all four (b-30, b-15, b+15, b+30). Check the",
    " remaining folders exist under root_dir with their own",
    " Mortality/Predation-mortality csvs."
  )
}

results[, Simulation := factor(
  Simulation,
  levels = simulation_levels
)]


#=========================================================
# Internal model inconsistency check: Greenland cod / Other piscivorous fish
#=========================================================
## SOW deliverable 2, CONFIRMED (per Daniel's own note in
## HarpSealTEST.xlsx, "input runs" sheet): Greenland cod is NOT its own
## functional group in this model. Stomach content data lumps Atlantic
## and Greenland cod together as Gadus spp. (otolith ID can't separate
## them), but in the Ecopath model Greenland cod instead falls inside
## the "Other piscivorous fish" functional group -- and THAT group has
## zero diet content assigned to harp seals. That means the model
## implicitly attributes 100% of the Gadus spp. consumption in the
## stomach data to Atlantic cod, which the raw data can't actually
## support. (Also per the same note: the harp seal diet column doesn't
## sum to exactly 1 -- it's 0.999 -- which is immaterial to the results
## and not flagged further here; the tolerance check on diet columns
## elsewhere in this script already accounts for that.)
##
## This block confirms that numerically from the predation-mortality
## data rather than asserting it: Predator_M2 for "Other piscivorous
## fish" should be zero (or the diet fraction feeding it should be, if
## you're checking the diet composition file directly) in every
## Simulation/Model where this holds.

other_pisciv_check <- results[Group == "Other piscivorous fish"]

if(nrow(other_pisciv_check) == 0){
  
  cat(
    "\nNo group named 'Other piscivorous fish' found -- check",
    "clean_group_names() / the raw group name spelling in this",
    "model's Mortalities.csv before relying on this check.\n"
  )
  
} else {
  
  cat("\n'Other piscivorous fish' (the group Greenland cod belongs to):",
      "harp seal predation mortality (Predator_M2) and total predation",
      "mortality (Total_M2) by Simulation/Model:\n")
  
  print(
    other_pisciv_check[
      ,
      .(Simulation, Model, Predator_M2, Total_M2, Fishing)
    ]
  )
  
  if(all(other_pisciv_check$Predator_M2 == 0)){
    cat(
      "\n-> Predator_M2 is exactly zero for 'Other piscivorous fish' in",
      "every model/period checked here, confirming the internal",
      "inconsistency: stomach content data can't separate Atlantic from",
      "Greenland cod, but the model assigns Greenland cod's functional",
      "group zero harp seal predation, so all Gadus spp. consumption in",
      "the diet data is implicitly (and unsupportably) attributed to",
      "Atlantic cod alone.\n"
    )
  } else {
    cat(
      "\n-> Predator_M2 is NOT uniformly zero for 'Other piscivorous",
      "fish' in this data -- re-check against Daniel's original note",
      "and the functional group definitions before citing the",
      "inconsistency as stated in the SOW.\n"
    )
  }
  
}

#=========================================================
# Save results
#=========================================================

fwrite(
  results,
  file.path(root_dir,
            "HarpSeal_Predation_vs_Fishing_AllModels.csv")
)

#=========================================================
# Read biomass ("Basic estimates") per model folder
#=========================================================
## NOTE: per Daniel, each Simulation's "baseline" Model subfolder
## contains a "Basic estimates" csv (Ecopath baseline Biomass, t/km^2).
## Non-baseline qb-XX/diet-XX folders have no MEANINGFULLY DIFFERENT
## match here since Biomass is a fixed Ecopath input under those
## perturbations, not a sensitivity-varied output -- that's not an
## error, so those model folders are silently skipped in the "by
## period" comparisons below (which filter on Model == "baseline"). If
## a Simulation's "baseline" folder specifically is missing the file,
## that IS flagged below.
##
## CORRECTED: per HarpSealTEST.xlsx, the SOW's "biomass" sensitivity
## axis is actually implemented as harp seal P/B perturbation
## (PB+50/-50/+25/-25, see `seal_pb_reference` in the Q/Y section
## below), not standing-biomass perturbation -- and those runs don't
## exist as EwE output folders yet (confirmed: Basic estimates is
## currently only produced for baseline). Once you run them, add
## "PB+50"/"PB-50"/"PB+25"/"PB-25" to the Model factor levels below and
## this loop will pick up their Basic-estimates files automatically
## (no code change needed here) -- but note a P/B change wouldn't, by
## itself, change *this file's* Biomass column, only its own P/B
## column; if those runs also perturb standing biomass, that would
## legitimately be a new Biomass value worth NOT silently pooling with
## baseline in `biomass_by_period` further down.
##
## File layout notes (from CoArc_1985_newmodel-Basic_estimates.csv):
##   - decimal comma ("3,576"), same as Mortality/Predation csvs -> dec=","
##   - section header rows exist (e.g. ",cod,,,,,,,,,,,") with a blank
##     first column -> dropped after converting GroupID to integer
##   - biomass column used: "Biomass (t/km^2)" (habitat-area-normalized
##     biomass; NOT "Biomass in habitat area (t/km^2)")

biomass_results <- rbindlist(
  
  lapply(model_dirs, function(model){
    
    biomass_file <- list.files(
      model,
      pattern = "Basic.*estimates.*\\.csv$",
      full.names = TRUE,
      ignore.case = TRUE
    )[1]
    
    if(is.na(biomass_file)) return(NULL)
    
    cat("\nReading biomass:", biomass_file, "\n")
    
    bio <- read.csv(
      biomass_file,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      dec = ","
    )
    
    bio <- as.data.table(bio)
    setnames(bio, 1, "GroupID")
    
    ## Drop section-header rows (blank GroupID, e.g. ",cod,,,,,,,,,,,")
    bio[, GroupID := suppressWarnings(as.integer(GroupID))]
    bio <- bio[!is.na(GroupID)]
    
    if(!"Biomass (t/km^2)" %in% names(bio))
      stop(
        "Column 'Biomass (t/km^2)' not found in ", biomass_file,
        " -- check the header spelling/encoding."
      )
    
    bio[, Group := clean_group_names(`Group name`)]
    bio[, Biomass := as.numeric(`Biomass (t/km^2)`)]
    
    out <- bio[, .(GroupID, Group, Biomass)]
    out[, Simulation := basename(dirname(model))]
    out[, Model := basename(model)]
    
    out
    
  }),
  
  fill = TRUE
  
)

if(is.null(biomass_results) || nrow(biomass_results) == 0)
  stop("No 'Basic estimates' biomass files were found in any model folder.")

## Combine N/S sub-models (area-weighted) + rename 1985_1988, same
## treatment as `results` above.

## Raw (pre-combination) copy, same purpose as results_raw above --
## keeps the per-region N/S Biomass values available for diagnostics
## further down, since biomass_results itself gets overwritten with
## the combined version on the next line.

biomass_results_raw <- copy(biomass_results)

biomass_results <- combine_NS_simulations(
  biomass_results,
  id_cols    = c("GroupID", "Group", "Model"),
  value_cols = c("Biomass")
)

## `simulation_levels` is now defined near the top of the script
## (Settings section, alongside combined_simulation_map) -- it's a
## static constant with no data dependency, so it needs to exist
## before `check_all_simulations()` is first called (right after
## `results` is built), not here. Moved after an "object
## 'simulation_levels' not found" error surfaced from that ordering.

biomass_results[, Simulation := factor(
  Simulation,
  levels = simulation_levels
)]

## Explicit check: warn if any Simulation's baseline folder specifically
## is missing a biomass file, rather than silently having gaps in the
## downstream comparison

sims_with_baseline_biomass <- unique(
  biomass_results[Model == "baseline"]$Simulation
)

missing_biomass_sims <- setdiff(
  simulation_levels,
  as.character(sims_with_baseline_biomass)
)

if(length(missing_biomass_sims) > 0){
  
  warning(
    "No baseline 'Basic estimates' biomass file found for Simulation(s): ",
    paste(missing_biomass_sims, collapse = ", ")
  )
  
}

fwrite(
  biomass_results,
  file.path(root_dir,
            "HarpSeal_Biomass_BasicEstimates_AllModels.csv")
)

#=========================================================
# Q/Y ratio: harp seal consumption vs fishery yield, by species
#=========================================================
## SOW deliverable 1 asks for BOTH ratios reported in West et al.
## (2025):
##   - M2/F (predation mortality rate / fishing mortality rate) --
##     already computed above as `results$Predator_vs_Fishing`.
##   - Q/Y  (harp seal consumption of a species, biomass/year, divided
##     by that species' fishery yield, biomass/year) -- NOT previously
##     computed anywhere in this script. This section adds it.
##
## REVISED against the real files (CoArc_1985_newmodel-*.csv) AND
## HarpSealTEST.xlsx ("sim" / "input runs" sheets):
##   - There is no Catch/Landings column anywhere in Basic_estimates,
##     and no separate Catch file. Yield is derived instead as
##     Y_species = Fishing_species * Biomass_species (F from
##     Mortalities.csv, already in `results$Fishing`; Biomass from
##     Basic_estimates). THIS IS AN ASSUMPTION, not a neutral fact: it
##     relies on Ecopath having defined "Fishing mort. rate" internally
##     as Catch / Biomass in the first place, so multiplying back out
##     exactly recovers the Catch value that went in. That's standard
##     Ecopath practice, but if your version/parametrization computes F
##     differently (e.g. a Baranov-type equation, or catch reported
##     independently of the Biomass in this file), F * Biomass would
##     NOT equal true yield and this whole Q/Y leg would need a real
##     Catch source instead. Worth a quick sanity check against any
##     landings figure you already trust for at least one species/year
##     before this goes in the report.
##   - Basic_estimates ONLY exists in the baseline Model folder (P/B and
##     B don't vary across the qb-XX/diet-XX sensitivity variants
##     currently run). Biomass is read ONCE per Simulation, from that
##     Simulation's baseline folder, and reused for every Model variant
##     in that Simulation.
##   - The Q/B column header in the real file is "Consumption / biomass
##     (/year)", not literally "Q/B" -- find_col()'s regex covers both.
##   - Harp seal Q/B and the Cod-adult diet fraction, per
##     Simulation x Model, are now taken directly from
##     `seal_qb_diet_lookup` below (hardcoded from the "sim" sheet of
##     HarpSealTEST.xlsx) -- these are your own confirmed values, not
##     guessed percentages. This replaces the earlier guessed
##     qb_variant_multiplier entirely.
##   - The SOW's "biomass" sensitivity axis appears to actually be P/B
##     perturbation (PB+50/-50/+25/-25), not standing-biomass
##     perturbation, and per your note those runs don't exist as EwE
##     output folders yet. `seal_pb_reference` below records the exact
##     P/B values for when you do run them; it isn't wired into Q/Y or
##     M2/F yet because there's no Mortality/Predation-mortality data
##     to compute a ratio against.
##
## STILL NEEDED: a full "Diet composition" csv, for species beyond Cod
## adult (herring, capelin, Greenland halibut, etc.). Until you provide
## one (or confirm it doesn't exist / isn't produced by your EwE runs),
## this block computes Q/Y for Cod adult only, using the confirmed
## hardcoded diet fraction, and says so rather than stopping entirely --
## Cod adult is the species the SOW's headline ratios (17x/24x) are
## actually about, so this isn't blocking the main comparison.

find_col <- function(dt, pattern, file_label){
  
  hit <- grep(
    pattern,
    names(dt),
    perl = TRUE,
    ignore.case = TRUE,
    value = TRUE
  )
  
  if(length(hit) == 0){
    stop(
      "No column matching /", pattern, "/ found in ", file_label,
      ". Available columns: ", paste(names(dt), collapse = ", "),
      " -- update the regex passed to find_col() to match your actual",
      " header text."
    )
  }
  
  if(length(hit) > 1){
    warning(
      "Multiple columns matched /", pattern, "/ in ", file_label, ": ",
      paste(hit, collapse = ", "), ". Using the first: ", hit[1]
    )
  }
  
  hit[1]
  
}

## ---- Hardcoded reference: harp seal Q/B and the Cod-adult diet
## fraction, by Simulation x Model, taken directly from
## HarpSealTEST.xlsx ("sim" sheet). NOTE: diet-20 and diet-20pisciv
## currently show the SAME cod-adult diet fraction (baseline x 0.8) --
## consistent with both being "reduce cod fraction by 20%" variants
## that differ only in where the removed 20% is redistributed among
## other prey (all prey vs. piscivorous fish specifically), not in the
## cod fraction itself. Extend this table if more model variants get
## run (e.g. once PB+50/PB-50/PB+25/PB-25 exist).

## IMPORTANT: Simulation values here are RAW folder names (matching the
## actual disk folder, e.g. "1985_1988" with an underscore) -- this
## table is looked up inside the per-raw-folder Q/Y loop using
## `sim_name <- basename(dirname(model))`, BEFORE the N/S area-weighted
## combination happens. Do NOT rename "1985_1988" to "1985-1988" here;
## that rename only applies to the final combined output tables
## (results, biomass_results, qy_results), not to this raw lookup.
##
## GAP: no entries yet for "2018-2020_N80"/"2018-2020_S20" (the two new
## sub-models mentioned for the 80/20 split). Without QB_seal and
## DietFraction_CodAdult values for those two, the per-model Q/Y loop
## will skip them (graceful warning, not a crash), and the combined
## "2018-2020_8020" Simulation won't be computable until both rows are
## added below -- add them here once you have the equivalent
## HarpSealTEST.xlsx-style values for the N80/S20 runs.

seal_qb_diet_lookup <- data.table(
  Simulation = rep(
    c("1985_1988", "2013-2015", "2018-2020_N50", "2018-2020_S50"),
    times = 7
  ),
  Model = rep(
    c("baseline", "qb+15", "qb+30", "qb-15", "qb-30", "diet-20", "diet-20pisciv"),
    each = 4
  ),
  QB_seal = c(
    16.78, 17.64, 17.64, 17.64,
    19.297, 20.286, 20.286, 20.286,
    21.814, 22.932, 22.932, 22.932,
    14.263, 14.994, 14.994, 14.994,
    11.746, 12.348, 12.348, 12.348,
    16.78, 17.64, 17.64, 17.64,
    16.78, 17.64, 17.64, 17.64
  ),
  DietFraction_CodAdult = c(
    0.06, 0.024, 0.134, 0.04,
    0.06, 0.024, 0.134, 0.04,
    0.06, 0.024, 0.134, 0.04,
    0.06, 0.024, 0.134, 0.04,
    0.06, 0.024, 0.134, 0.04,
    0.048, 0.0192, 0.1072, 0.032,
    0.048, 0.0192, 0.1072, 0.032
  )
)

## ---- Q/B sensitivity multiplier, GENERALIZED from seal_qb_diet_lookup ----
## Cross-checked against every one of the 4 hardcoded Simulations
## above: qb+15 = baseline x 1.15, qb+30 = baseline x 1.30, qb-15 =
## baseline x 0.85, qb-30 = baseline x 0.70 EXACTLY, in every single
## one of them (e.g. 1985_1988: 16.78 x 1.15 = 19.297; 2013-2015:
## 17.64 x 1.15 = 20.286 -- both match seal_qb_diet_lookup to the
## digit). Since this multiplier is consistent across every Simulation
## with known data, it's used as a GENERAL rule for ANY Simulation
## (including 2018-2020_N80/S20, which have no xlsx-sourced values of
## their own) -- QB_seal for a qb-XX model is computed as that
## Simulation's OWN baseline Q/B (read directly from its
## Basic_estimates.csv, in `biomass_baseline`) times this multiplier,
## rather than requiring a pre-supplied hardcoded value for every new
## Simulation added. diet-20/diet-20pisciv keep Q/B unchanged from
## baseline (multiplier 1.0), matching the confirmed xlsx data.

qb_multiplier <- c(
  "baseline"      = 1.00,
  "qb-30"         = 0.70,
  "qb-15"         = 0.85,
  "qb+15"         = 1.15,
  "qb+30"         = 1.30,
  "diet-20"       = 1.00,
  "diet-20pisciv" = 1.00
)

## Reference only -- P/B perturbation runs (PB+50/-50/+25/-25) still
## don't exist as EwE output folders as far as this script knows. Kept
## for documentation; SUPERSEDED as the "biomass" SOW deliverable by
## seal_biomass_csv_lookup directly below, which is the real thing --
## actual standing-biomass perturbation, not a P/B proxy for it.

seal_pb_reference <- c(
  "PB+50" = 0.2235,
  "PB-50" = 0.0745,
  "PB+25" = 0.18625,
  "PB-25" = 0.11175
)

#=========================================================
# Harp seal biomass: authoritative source (HarpSeal_biomass.csv)
#=========================================================
## Per-request: ALL harp seal biomass values used anywhere in this
## script -- for every Simulation and every Model variant, including
## baseline/qb-XX/diet-XX AND the new b-30/b-15/b+15/b+30 biomass-
## sensitivity variants -- come from this file, not from
## Basic_estimates.csv's single baseline value. This is what finally
## fulfills the SOW's "biomass" pedigree-uncertainty axis with real
## standing-biomass perturbation (b-30/-15/+15/+30), rather than the
## P/B proxy documented above, which still has no matching EwE runs.
##
## File format (as provided): one row per raw Simulation ("model"
## column), one column per Model variant, values as decimal-comma
## strings (European format, matching every other file this script
## reads with dec=","). Values are harp seal biomass density (same
## units as Basic_estimates.csv's Biomass column) -- NOT a percentage
## or multiplier.
##
## IMPORTANT: the raw CSV's Simulation labels use a different
## direction/number order than every folder name elsewhere in this
## script ("2020_80N" here vs. "2018-2020_N80" as a disk folder) --
## this mapping is inferred from the shared "80N"/"50N"/"50S"/"20S"
## substrings and should be visually confirmed against the printed
## table below before trusting the results.

harp_biomass_csv_map <- c(
  "1985"     = "1985_1988",
  "2013"     = "2013-2015",
  "2020_50N" = "2018-2020_N50",
  "2020_50S" = "2018-2020_S50",
  "2020_80N" = "2018-2020_N80",
  "2020_20S" = "2018-2020_S20"
)

## Robust file search, same pattern-search style used throughout this
## script, rather than a single hardcoded path -- searches root_dir
## (and one level down, in case it's placed alongside the Simulation
## folders rather than directly in root_dir) for a CSV whose name
## contains both "harp" and "biomass".

harp_biomass_file <- list.files(
  root_dir,
  pattern = "(?i)harp.*seal.*biomass.*\\.csv$|harpseal.*biomass.*\\.csv$",
  full.names = TRUE,
  recursive = FALSE
)[1]

if(is.na(harp_biomass_file)){
  harp_biomass_file <- list.files(
    root_dir,
    pattern = "(?i)harp.*biomass.*\\.csv$",
    full.names = TRUE,
    recursive = TRUE
  )[1]
}

if(is.na(harp_biomass_file)){
  
  warning(
    "No HarpSeal biomass csv found under root_dir (searched for a",
    " filename containing 'harp'/'seal' and 'biomass'). The b-30/b-15/",
    "b+15/b+30 biomass-sensitivity Models will have no authoritative",
    " harp seal biomass source and Q/Y will fall back to the baseline-",
    "only value from Basic_estimates.csv for them, which is WRONG for",
    " those specific Models (they exist precisely to vary this value)."
  )
  seal_biomass_csv_lookup <- data.table(
    Simulation = character(0), Model = character(0), Biomass_Seal_CSV = numeric(0)
  )
  
} else {
  
  cat("\nReading harp seal biomass from:", harp_biomass_file, "\n")
  
  harp_biomass_raw <- read.csv(
    harp_biomass_file,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  ## First column is the raw Simulation label ("model" in the file);
  ## remaining columns are Model variants. Values are decimal-comma
  ## strings -- convert via gsub before as.numeric (dec="," in
  ## read.csv only affects numeric-typed columns, and these arrive as
  ## character because of the comma).
  
  setnames(harp_biomass_raw, 1, "Simulation_raw")
  harp_biomass_raw <- as.data.table(harp_biomass_raw)
  
  value_cols <- setdiff(names(harp_biomass_raw), "Simulation_raw")
  for(col in value_cols){
    harp_biomass_raw[[col]] <- as.numeric(gsub(",", ".", harp_biomass_raw[[col]], fixed = TRUE))
  }
  
  seal_biomass_csv_lookup <- melt(
    harp_biomass_raw,
    id.vars = "Simulation_raw",
    variable.name = "Model",
    value.name = "Biomass_Seal_CSV"
  )
  
  seal_biomass_csv_lookup[, Model := as.character(Model)]
  
  unmapped_sims <- setdiff(unique(seal_biomass_csv_lookup$Simulation_raw), names(harp_biomass_csv_map))
  if(length(unmapped_sims) > 0){
    warning(
      "harp_biomass_csv_map has no entry for: ",
      paste(unmapped_sims, collapse = ", "),
      " -- these rows in the biomass CSV will be dropped. Add them to",
      " harp_biomass_csv_map if they correspond to real Simulations."
    )
  }
  
  seal_biomass_csv_lookup[, Simulation := harp_biomass_csv_map[Simulation_raw]]
  seal_biomass_csv_lookup <- seal_biomass_csv_lookup[!is.na(Simulation)]
  seal_biomass_csv_lookup[, Simulation_raw := NULL]
  
  cat("\nHarp seal biomass lookup, as parsed and mapped (verify Simulation",
      "labels against the raw CSV's row names before trusting this):\n")
  print(seal_biomass_csv_lookup[order(Simulation, Model)])
  
}

## ---- Step 1: read Biomass ONCE per Simulation, from that
## Simulation's baseline folder (Basic_estimates only exists there),
## with a consistency check against seal_qb_diet_lookup's baseline row ----

baseline_dirs <- model_dirs[basename(model_dirs) == "baseline"]

biomass_baseline <- rbindlist(
  
  lapply(baseline_dirs, function(model){
    
    basic_file <- list.files(
      model,
      pattern = "Basic.*estimates.*\\.csv$",
      full.names = TRUE,
      ignore.case = TRUE
    )[1]
    
    if(is.na(basic_file)) return(NULL)
    
    basic <- read.csv(
      basic_file,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      dec = ","
    )
    basic <- as.data.table(basic)
    setnames(basic, 1, "GroupID")
    basic[, GroupID := suppressWarnings(as.integer(GroupID))]
    basic <- basic[!is.na(GroupID)]
    basic[, Group := clean_group_names(`Group name`)]
    basic[, Biomass := as.numeric(`Biomass (t/km^2)`)]
    
    qb_col <- find_col(
      basic,
      "consumption\\s*/?\\s*biomass|q\\.?/?b",
      paste0(basic_file, " (Q/B)")
    )
    basic[, QB := as.numeric(get(qb_col))]
    
    sim_name <- basename(dirname(model))
    
    seal_qb_file   <- basic[grepl(harp_pattern, Group, perl = TRUE)][1]$QB
    seal_qb_lookup <- seal_qb_diet_lookup[
      Simulation == sim_name & Model == "baseline"
    ]$QB_seal
    
    if(length(seal_qb_lookup) == 1 && !is.na(seal_qb_file) &&
       abs(seal_qb_file - seal_qb_lookup) > 0.01){
      warning(
        "Baseline Q/B for harp seal in ", basic_file, " (", seal_qb_file,
        ") doesn't match the hardcoded value from HarpSealTEST.xlsx (",
        seal_qb_lookup, ") for Simulation '", sim_name, "' -- check",
        " which is current before trusting Q/Y."
      )
    }
    
    basic[, Simulation := sim_name]
    basic[, .(GroupID, Group, Biomass, QB, Simulation)]
    
  }),
  
  fill = TRUE
  
)

if(is.null(biomass_baseline) || nrow(biomass_baseline) == 0)
  stop(
    "No baseline 'Basic estimates' file could be read for any ",
    "Simulation -- Q/Y can't be computed without it."
  )

#=========================================================
# Harp seal Q/B: consolidated table across ALL models/simulations
#=========================================================
## NEW: a standalone, exportable table giving harp seal Q/B for every
## (raw Simulation, Model) combination. Previously Q/B was only ever
## computed inline, on the fly, inside the Q/Y per-model loop further
## down, and never materialized as its own object -- unlike biomass
## (HarpSeal_biomass.csv / seal_biomass_csv_lookup) or diet
## (baseline_diet_cache / qy_results$DietFraction), there was no
## single place to inspect or export harp seal Q/B directly. This
## mirrors those: Q/B_Seal = baseline Q/B (from Basic_estimates.csv,
## via biomass_baseline above) x the qb_multiplier for that Model
## variant (1.0 for baseline/diet-XX/b-XX, since none of those perturb
## Q/B -- only the qb-XX variants do).

seal_qb_baseline <- biomass_baseline[
  grepl(harp_pattern, Group, perl = TRUE),
  .(Simulation, QB_Baseline = QB)
]

seal_qb_all_models <- rbindlist(lapply(model_levels_all, function(m){
  mult <- if(m %in% names(qb_multiplier)) qb_multiplier[[m]] else 1.00
  dt <- copy(seal_qb_baseline)
  dt[, Model := m]
  dt[, QB_Multiplier := mult]
  dt[, QB_Seal := QB_Baseline * mult]
  dt
}))

setorder(seal_qb_all_models, Simulation, Model)

cat("\n===== Harp seal Q/B, consolidated across all Simulations x Models =====\n")
print(seal_qb_all_models)
cat("=========================================================================\n")

fwrite(
  seal_qb_all_models,
  file.path(root_dir, "HarpSeal_QB_AllModels.csv")
)

## ---- Step 1b: read & cache each Simulation's BASELINE Diet
## composition file ONCE. Rationale: only diet-20/diet-20pisciv are
## expected to have their OWN diet matrix (diet is exactly what those
## variants perturb); qb-30/qb-15/qb+15/qb+30 use the SAME diet as
## baseline -- only total consumption (via Q/B) changes for those. So
## a qb-XX model folder legitimately has no Diet composition file of
## its own, and should reuse the Simulation's baseline diet matrix for
## EVERY species, not just Cod adult. (Previously this fell all the way
## back to a Cod-adult-only hardcoded value whenever a model's own
## folder lacked a diet file, which is why the qb-XX rows in the Q/Y
## heatmap were mostly NA for herring/capelin/halibut -- fixed here.)

read_diet_file <- function(diet_file){
  
  diet <- read.csv(
    diet_file,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    dec = ","
  )
  diet <- as.data.table(diet)
  setnames(diet, 1, "GroupID")
  setnames(diet, 2, "Group")
  diet[, GroupID := suppressWarnings(as.integer(GroupID))]
  diet <- diet[!is.na(GroupID)]
  diet[, Group := clean_group_names(Group)]
  
  ## FIXED (per explicit warning: "numbering on diet and FG are not
  ## the same for each model"): this function used to take `seal_id`
  ## as an argument SUPPLIED FROM A DIFFERENT FILE -- the Simulation's
  ## baseline Basic_estimates.csv/Mortalities.csv GroupID for the harp
  ## seal -- and used that cross-file ID to pick a column out of THIS
  ## diet file. That's only safe if both files share the exact same
  ## internal ID numbering. Nothing guarantees that: EwE's internal ID
  ## assignment for a given functional group is NOT guaranteed
  ## consistent across separately exported files, or across different
  ## Model variants of the same Simulation (e.g. diet-20's own
  ## separately-exported Diet composition file could number species
  ## differently than that Simulation's baseline Basic_estimates.csv).
  ## If the two didn't actually match, this would silently select the
  ## WRONG predator's column -- no error, just wrong DietFraction
  ## values for every species in that file, unless the mismatched ID
  ## happened not to exist as a column at all (triggering the stop()
  ## below) or happened to belong to some other real predator (no
  ## error at all, just silently wrong).
  ##
  ## Fixed by finding the harp seal's row WITHIN THIS SAME diet file
  ## and using ITS OWN GroupID to select the column -- within a single
  ## Ecopath export, the row ID list and the column ID list are the
  ## same internally-consistent numbering (a group's row ID IS its
  ## column ID within that one file), so this is safe even when
  ## numbering differs across separate model/Model exports. No
  ## cross-file dependency at all any more.
  
  seal_row <- diet[grepl(harp_pattern, Group, perl = TRUE)]
  
  if(nrow(seal_row) == 0){
    stop(
      "No harp seal row found in ", diet_file, " (checked against",
      " clean_group_names()'d Group names) -- can't determine which",
      " column is the harp seal's diet without it."
    )
  }
  if(nrow(seal_row) > 1){
    warning(
      "Multiple harp seal rows found in ", diet_file,
      " -- using the first match."
    )
  }
  
  seal_id <- as.character(seal_row$GroupID[1])
  
  if(!seal_id %in% names(diet)){
    stop(
      "Harp seal GroupID (", seal_id, "), determined from THIS diet",
      " file's own rows, was not found as a column in ", diet_file,
      ". Available columns: ", paste(names(diet), collapse = ", "),
      " -- this model's diet matrix may not include the harp seal as",
      " a predator column at all."
    )
  }
  
  diet_frac  <- as.numeric(diet[[seal_id]])
  
  ## FIX: blank cells in EwE diet composition csvs conventionally mean
  ## "zero diet content", not "missing data" -- read.csv/as.numeric()
  ## turns a blank cell into NA, not 0. Without this conversion, every
  ## species the harp seal DOESN'T eat gets DietFraction = NA instead
  ## of 0, which then propagates as NA through Q_species and Q/Y for
  ## that species -- this was silently inflating the NA count in the
  ## Q/Y heatmap (on top of the earlier missing-diet-file issue that
  ## was already fixed), and is exactly what caused
  ## `all(pisciv_diet_check$DietFraction == 0)` to error rather than
  ## return TRUE for "Other piscivorous fish" (NA == 0 is NA, and
  ## all() with an NA in it is neither TRUE nor FALSE, not a genuine
  ## data problem).
  ##
  ## total_frac (the ~1 sum check below) is computed BEFORE this
  ## conversion via na.rm=TRUE, so it isn't affected either way -- this
  ## only changes what gets stored per-species afterward.
  
  n_blank <- sum(is.na(diet_frac))
  
  if(n_blank > 0){
    cat(
      "  ", n_blank, "blank diet cell(s) in", basename(diet_file),
      "treated as 0 (standard EwE convention for zero diet content).\n"
    )
  }
  
  diet_frac[is.na(diet_frac)] <- 0
  
  total_frac <- sum(diet_frac, na.rm = TRUE)
  
  if(total_frac > 0 && abs(total_frac - 1) > 0.02){
    warning(
      "Harp seal diet column in ", diet_file, " sums to ",
      round(total_frac, 3), " (expected ~1). DietFraction below uses",
      " the raw fractions as given -- re-check the diet file if this",
      " isn't close to 1."
    )
  }
  
  data.table(
    GroupID      = diet$GroupID,
    Group        = diet$Group,
    DietFraction = diet_frac
  )
  
}

baseline_diet_cache <- rbindlist(
  
  lapply(baseline_dirs, function(model){
    
    sim_name <- basename(dirname(model))
    
    ## Broadened to also match "matrix" naming (not just "composition"),
    ## since diet files are sometimes named things like "DietMatrix.csv"
    ## rather than "Diet composition.csv".
    
    diet_file <- list.files(
      model,
      pattern = "Diet.*(comp|matrix).*\\.csv$",
      full.names = TRUE,
      ignore.case = TRUE
    )[1]
    
    if(is.na(diet_file)){
      
      all_csv <- list.files(model, pattern = "\\.csv$", full.names = FALSE, ignore.case = TRUE)
      
      cat(
        "\nNo baseline Diet composition/matrix file found for Simulation '",
        sim_name, "' (folder: ", model, ") -- qb-XX variants for this",
        " Simulation will fall back to Cod-adult-only Q/Y.\n",
        "  CSV files actually present in this folder: ",
        if(length(all_csv) > 0) paste(all_csv, collapse = ", ") else "(none)",
        "\n  If a diet file IS there under a different name, update the",
        " search pattern above to match it.\n", sep = ""
      )
      return(NULL)
    }
    
    dt <- tryCatch(
      read_diet_file(diet_file),
      error = function(e){
        cat(
          "\nCouldn't read baseline diet file for Simulation '", sim_name,
          "' (", diet_file, "): ", conditionMessage(e),
          " -- skipping.\n", sep = ""
        )
        NULL
      }
    )
    if(is.null(dt)) return(NULL)
    dt[, Simulation := sim_name]
    dt
    
  }),
  
  fill = TRUE
  
)

## ---- Supplementary check for SOW deliverable 2, now that real diet
## data exists: confirm zero diet fraction directly, not just zero
## downstream predation mortality. The earlier check (near the top of
## this script, before Diet composition files were available) could
## only look at Predator_M2, which is a CONSEQUENCE of the diet
## fraction being zero -- this checks the actual cause.

if(exists("baseline_diet_cache") && nrow(baseline_diet_cache) > 0){
  
  pisciv_diet_check <- baseline_diet_cache[Group == "Other piscivorous fish"]
  
  if(nrow(pisciv_diet_check) == 0){
    
    cat(
      "\nSupplementary diet-fraction check: 'Other piscivorous fish'",
      "not found in baseline_diet_cache -- check group name spelling.\n"
    )
    
  } else {
    
    cat(
      "\nSupplementary diet-fraction check (SOW deliverable 2):",
      "harp seal DIET FRACTION on 'Other piscivorous fish', by",
      "Simulation (direct confirmation, not inferred from M2):\n"
    )
    print(pisciv_diet_check[, .(Simulation, DietFraction)])
    
    if(all(pisciv_diet_check$DietFraction == 0)){
      cat(
        "-> Confirmed directly from the diet input: 0% of harp seal's",
        "diet is assigned to 'Other piscivorous fish' in every",
        "Simulation checked -- this is the root cause of the zero",
        "Predator_M2 already confirmed earlier in this script, not",
        "just a correlated downstream effect.\n"
      )
    } else {
      cat(
        "-> DietFraction is NOT uniformly zero for 'Other piscivorous",
        "fish' -- this contradicts the earlier Predator_M2-based",
        "check. Re-investigate before citing the inconsistency.\n"
      )
    }
    
  }
  
}

#=========================================================
# Diagnostic: WHY is a Group missing from a given raw Simulation --
# not modeled there at all (NOT_IN_STRUCTURE), zero fishing mortality
# (ZERO_FISHING -- silently dropped by `out <- out[Fishing > 0]`
# inside compare_predator_vs_fishing() above, since M2/F is undefined
# without a nonzero denominator), or zero harp seal diet fraction
# (ZERO_PREDATION)?
#=========================================================
## Added per request: a species can vanish from a combined 2018-2020
## figure for at least THREE distinct, unrelated reasons -- a naming
## mismatch between the N/S regional CSVs (already checked elsewhere
## in this script, and the root cause fixed for American Plaice
## above), the group not existing as its own functional group in one
## region's model structure at all, or that region's Mortality file
## genuinely recording zero fishing mortality for it (in which case
## dropping it from the M2/F-based figures is CORRECT behaviour, not
## a bug -- there's no fishery pressure to form a ratio against).
## Rather than inferring which of these applies from downstream
## symptoms, this reads the RAW per-folder Mortality file directly
## (independent of the already Fishing>0-filtered `results` table) and
## reports the answer plainly, per Simulation/Model.
diagnose_group_gap <- function(group_name, model_dirs_subset = model_dirs){
  
  rbindlist(lapply(model_dirs_subset, function(model){
    
    sim_name   <- basename(dirname(model))
    model_name <- basename(model)
    
    mort_file <- list.files(
      model, pattern = "Mortalit.*\\.csv$",
      full.names = TRUE, ignore.case = TRUE
    )[1]
    if(is.na(mort_file)) return(NULL)
    
    mort <- read.csv(
      mort_file, check.names = FALSE,
      stringsAsFactors = FALSE, dec = ","
    )
    mort <- as.data.table(mort)
    setnames(mort, 1, "GroupID")
    mort[, `Group name` := clean_group_names(`Group name`)]
    
    row <- mort[`Group name` == group_name]
    
    if(nrow(row) == 0){
      return(data.table(
        Simulation = sim_name, Model = model_name,
        Status = "NOT_IN_STRUCTURE",
        Fishing = NA_real_, DietFraction = NA_real_
      ))
    }
    
    fishing_val <- suppressWarnings(as.numeric(row$`= Fishing mort. rate`[1]))
    
    diet_val <- if(exists("baseline_diet_cache") && nrow(baseline_diet_cache) > 0){
      dv <- baseline_diet_cache[
        Simulation == sim_name & Group == group_name
      ]$DietFraction
      if(length(dv) == 0) NA_real_ else dv[1]
    } else {
      NA_real_
    }
    
    status <- if(is.na(fishing_val) || fishing_val <= 0){
      "ZERO_FISHING"
    } else if(!is.na(diet_val) && diet_val == 0){
      "ZERO_PREDATION"
    } else {
      "PRESENT"
    }
    
    data.table(
      Simulation = sim_name, Model = model_name,
      Status = status,
      Fishing = fishing_val, DietFraction = diet_val
    )
    
  }), fill = TRUE)
  
}

cat("\n===== Gap diagnosis: American Plaice juvenile =====\n")
cat("(Status meanings -- NOT_IN_STRUCTURE: this Group isn't a modeled",
    "functional group in that folder's own Mortality file at all;",
    "ZERO_FISHING: the Group exists but Fishing mort. rate == 0, which",
    "is why compare_predator_vs_fishing() drops it from `results` for",
    "that Simulation/Model (M2/F is undefined without fishing);",
    "ZERO_PREDATION: Fishing > 0 but the harp seal diet fraction on it",
    "is 0; PRESENT: none of the above, it should show up normally.)\n\n")
print(diagnose_group_gap("American Plaice juvenile")[order(Simulation, Model)])

cat("\n===== Gap diagnosis: Medium benthivorous fish =====\n")
print(diagnose_group_gap("Medium benthivorous fish")[order(Simulation, Model)])
cat("=================================================================\n")

#=========================================================
# Full status table: EVERY Group x every raw Simulation x Model --
# OK / ZERO_FISHING / ZERO_PREDATION / NOT_IN_STRUCTURE
#=========================================================
## Generalizes diagnose_group_gap() above from "check one named group"
## to "check every group, everywhere, automatically" -- one row per
## (Group, Simulation, Model), so instead of guessing which species/
## period to investigate next, or calling diagnose_group_gap() one
## species at a time, this gives the complete picture in a single
## table you can filter/sort/pivot however you like.
##
## Universe of Group names = every cleaned Group name that appears in
## ANY raw Mortality file, across ANY Simulation/Model -- NOT just the
## groups already in `results`, since `results` has already dropped
## every Fishing == 0 row (that's exactly one of the statuses this
## table exists to surface, so building the universe from `results`
## would hide it).
##
## Status meanings:
##   NOT_IN_STRUCTURE -- this Group isn't a modeled functional group
##     in that folder's own Mortality file at all (a genuine
##     regional/model-structure difference, not a data problem).
##   ZERO_FISHING -- the Group exists with Fishing mort. rate == 0.
##     This is exactly what `out <- out[Fishing > 0]` inside
##     compare_predator_vs_fishing() drops from `results` for that
##     Simulation/Model -- correctly, since M2/F is undefined without
##     a nonzero fishing denominator.
##   ZERO_PREDATION -- Fishing > 0, but the harp seal diet fraction on
##     it is 0 (no predation link) -- uses THIS Model's own diet file
##     if it has one (diet-20/diet-20pisciv), otherwise its
##     Simulation's baseline diet matrix (same tiering logic as the
##     Q/Y section's Tier 1/Tier 2 fallback).
##   OK_NO_DIET_DATA -- Fishing > 0, but no diet file/lookup was
##     available at all to check predation against (distinct from
##     ZERO_PREDATION, where a diet fraction WAS found and it was 0).
##   OK -- none of the above; this Group/Simulation/Model should
##     appear normally with a real M2/F ratio.
all_known_groups <- sort(unique(unlist(lapply(model_dirs, function(model){
  
  mort_file <- list.files(
    model, pattern = "Mortalit.*\\.csv$",
    full.names = TRUE, ignore.case = TRUE
  )[1]
  if(is.na(mort_file)) return(character(0))
  
  mort <- read.csv(
    mort_file, check.names = FALSE,
    stringsAsFactors = FALSE, dec = ","
  )
  clean_group_names(mort[[2]])  # "Group name" is always the 2nd column
  
}))))

group_status_table <- rbindlist(lapply(model_dirs, function(model){
  
  sim_name   <- basename(dirname(model))
  model_name <- basename(model)
  
  mort_file <- list.files(
    model, pattern = "Mortalit.*\\.csv$",
    full.names = TRUE, ignore.case = TRUE
  )[1]
  if(is.na(mort_file)) return(NULL)
  
  mort <- read.csv(
    mort_file, check.names = FALSE,
    stringsAsFactors = FALSE, dec = ","
  )
  mort <- as.data.table(mort)
  setnames(mort, 1, "GroupID")
  mort[, `Group name` := clean_group_names(`Group name`)]
  
  ## This Model's own diet file if it has one (diet-20/diet-20pisciv),
  ## else fall back to its Simulation's baseline diet matrix -- same
  ## Tier 1/Tier 2 fallback the Q/Y section itself uses, so
  ## DietFraction here reflects whatever diet is actually in force for
  ## THIS Model, not always baseline's.
  diet_file <- list.files(
    model, pattern = "Diet.*(comp|matrix).*\\.csv$",
    full.names = TRUE, ignore.case = TRUE
  )[1]
  
  diet_lookup <- if(!is.na(diet_file)){
    tryCatch(
      read_diet_file(diet_file),
      error = function(e) NULL
    )
  } else if(
    exists("baseline_diet_cache") &&
    nrow(baseline_diet_cache[Simulation == sim_name]) > 0
  ){
    baseline_diet_cache[Simulation == sim_name]
  } else {
    NULL
  }
  
  rbindlist(lapply(all_known_groups, function(g){
    
    row <- mort[`Group name` == g]
    
    if(nrow(row) == 0){
      return(data.table(
        Simulation = sim_name, Model = model_name, Group = g,
        Status = "NOT_IN_STRUCTURE",
        Fishing = NA_real_, DietFraction = NA_real_
      ))
    }
    
    fishing_val <- suppressWarnings(as.numeric(row$`= Fishing mort. rate`[1]))
    
    diet_val <- if(!is.null(diet_lookup)){
      dv <- diet_lookup[Group == g]$DietFraction
      if(length(dv) == 0) NA_real_ else dv[1]
    } else {
      NA_real_
    }
    
    status <- if(is.na(fishing_val) || fishing_val <= 0){
      "ZERO_FISHING"
    } else if(!is.na(diet_val) && diet_val == 0){
      "ZERO_PREDATION"
    } else if(is.na(diet_val)){
      "OK_NO_DIET_DATA"
    } else {
      "OK"
    }
    
    data.table(
      Simulation = sim_name, Model = model_name, Group = g,
      Status = status, Fishing = fishing_val, DietFraction = diet_val
    )
    
  }))
  
}), fill = TRUE)

setorder(group_status_table, Group, Simulation, Model)

fwrite(
  group_status_table,
  file.path(root_dir, "HarpSeal_GroupStatus_AllModels.csv")
)

cat("\n===== Group status: counts by Status (all Groups x Simulations x Models) =====\n")
print(group_status_table[, .N, by = Status][order(-N)])

## Wide, one-row-per-Group pivot (Simulation_Model columns hold the
## Status string) -- easier to eyeball a specific species' full
## pattern across every model/period at a glance than scrolling the
## long table. Saved as its own CSV; not printed to console by default
## since it can run wide (one column per Simulation x Model combo).
group_status_wide <- dcast(
  group_status_table,
  Group ~ Simulation + Model,
  value.var = "Status"
)

fwrite(
  group_status_wide,
  file.path(root_dir, "HarpSeal_GroupStatus_AllModels_Wide.csv")
)

cat("\nFull long-format table saved to HarpSeal_GroupStatus_AllModels.csv",
    "(one row per Group x Simulation x Model). Wide pivot (one row per",
    "Group, one column per Simulation x Model) saved to",
    "HarpSeal_GroupStatus_AllModels_Wide.csv -- open that one directly",
    "to see a species' full status pattern at a glance.\n")

#=========================================================
# N/S structural presence check: WHY does a Group drop out of a
# combined 2018-2020 Simulation? (per explicit request: name it as
# NOT_PRESENT_IN_N / NOT_PRESENT_IN_S specifically, not just "missing")
#=========================================================
## Now that compare_predator_vs_fishing() keeps Fishing == 0 rows (see
## the earlier fix) and Figure 10 uses `seal_prey_species` instead of
## `exploited_species` (also fixed earlier), zero fishing mortality is
## NO LONGER a reason for a blank cell anywhere in this pipeline. The
## ONLY thing that can still make a Group's bar blank for one specific
## combined Simulation (2018-2020_5050 or 2018-2020_8020) is
## combine_NS_simulations() dropping it -- which it does whenever the
## Group doesn't exist as a modeled row in BOTH of the two regional
## N/S sub-models. This block checks that directly, per raw N/S pair,
## and names which SIDE is missing it -- rather than leaving that as
## an unexplained blank in the figure or a generic "present in only
## one sub-model" console warning.
##
## Restricted to Model == "baseline", since that's what Figure 10 (and
## most of the report's headline comparisons) actually uses; the same
## approach generalizes to other Models if needed later.
##
## Uses read_diet_file(diet_file) with NO seal_id argument -- see the
## fix just above: the harp seal's column is now determined from
## EACH region's OWN diet file, not borrowed from a different file, so
## a genuine N/S difference in DietFraction_N vs DietFraction_S below
## reflects a real regional difference, not a numbering artifact.
read_baseline_group_structure <- function(sim_name){
  
  model_dir <- model_dirs[
    basename(dirname(model_dirs)) == sim_name & basename(model_dirs) == "baseline"
  ]
  if(length(model_dir) == 0) return(NULL)
  model_dir <- model_dir[1]
  
  mort_file <- list.files(
    model_dir, pattern = "Mortalit.*\\.csv$",
    full.names = TRUE, ignore.case = TRUE
  )[1]
  if(is.na(mort_file)) return(NULL)
  
  mort <- read.csv(mort_file, check.names = FALSE, stringsAsFactors = FALSE, dec = ",")
  mort <- as.data.table(mort)
  setnames(mort, 1, "GroupID")
  mort[, `Group name` := clean_group_names(`Group name`)]
  
  diet_file <- list.files(
    model_dir, pattern = "Diet.*(comp|matrix).*\\.csv$",
    full.names = TRUE, ignore.case = TRUE
  )[1]
  
  diet_lookup <- if(!is.na(diet_file)){
    tryCatch(read_diet_file(diet_file), error = function(e) NULL)
  } else {
    NULL
  }
  
  list(mort = mort, diet = diet_lookup)
  
}

ns_structure_check <- rbindlist(lapply(names(combined_simulation_map), function(combo_name){
  
  pair  <- combined_simulation_map[[combo_name]]
  n_sim <- pair[["N"]]
  s_sim <- pair[["S"]]
  
  n_data <- read_baseline_group_structure(n_sim)
  s_data <- read_baseline_group_structure(s_sim)
  
  all_groups_this_pair <- sort(unique(c(
    if(!is.null(n_data)) n_data$mort$`Group name` else character(0),
    if(!is.null(s_data)) s_data$mort$`Group name` else character(0)
  )))
  
  rbindlist(lapply(all_groups_this_pair, function(g){
    
    n_row <- if(!is.null(n_data)) n_data$mort[`Group name` == g] else data.table()
    s_row <- if(!is.null(s_data)) s_data$mort[`Group name` == g] else data.table()
    
    present_N <- nrow(n_row) > 0
    present_S <- nrow(s_row) > 0
    
    fishing_N <- if(present_N) suppressWarnings(as.numeric(n_row$`= Fishing mort. rate`[1])) else NA_real_
    fishing_S <- if(present_S) suppressWarnings(as.numeric(s_row$`= Fishing mort. rate`[1])) else NA_real_
    
    diet_N <- if(present_N && !is.null(n_data$diet)){
      dv <- n_data$diet[Group == g]$DietFraction
      if(length(dv) == 0) NA_real_ else dv[1]
    } else NA_real_
    
    diet_S <- if(present_S && !is.null(s_data$diet)){
      dv <- s_data$diet[Group == g]$DietFraction
      if(length(dv) == 0) NA_real_ else dv[1]
    } else NA_real_
    
    reason <- if(!present_N && !present_S){
      "NOT_IN_EITHER_REGION"
    } else if(!present_N){
      "NOT_PRESENT_IN_N"
    } else if(!present_S){
      "NOT_PRESENT_IN_S"
    } else {
      "PRESENT_IN_BOTH"
    }
    
    data.table(
      CombinedSimulation = combo_name,
      N_Simulation = n_sim, S_Simulation = s_sim,
      Group = g,
      Present_in_N = present_N, Present_in_S = present_S,
      Fishing_N = fishing_N, Fishing_S = fishing_S,
      DietFraction_N = diet_N, DietFraction_S = diet_S,
      Reason = reason
    )
    
  }))
  
}), fill = TRUE)

setorder(ns_structure_check, CombinedSimulation, Reason, Group)

fwrite(
  ns_structure_check,
  file.path(root_dir, "HarpSeal_NS_StructuralPresence_Baseline.csv")
)

cat("\n===== N/S structural presence check (baseline model): why does a",
    "Group drop out of a combined 2018-2020 Simulation? =====\n")
cat("(NOT_PRESENT_IN_N / NOT_PRESENT_IN_S: the Group exists as a modeled",
    "functional group in only ONE of the two regional sub-models, so",
    "combine_NS_simulations() can't area-weight it and drops it from that",
    "combined Simulation entirely -- this is now the ONLY reason a cell",
    "should be blank in Figure 10, since zero fishing mortality alone no",
    "longer causes a drop (see the earlier fixes). NOT_IN_EITHER_REGION:",
    "absent from both regions for this combined Simulation.",
    "PRESENT_IN_BOTH: structurally fine in both regions -- any remaining",
    "differences are in the VALUES (Fishing_N vs Fishing_S,",
    "DietFraction_N vs DietFraction_S), not presence, and are shown here",
    "for comparison, not flagged as a problem.)\n\n")

print(ns_structure_check[Reason != "PRESENT_IN_BOTH"])

cat("\nFull table -- including PRESENT_IN_BOTH rows with each region's own",
    "Fishing/DietFraction values side by side for comparison -- saved to",
    "HarpSeal_NS_StructuralPresence_Baseline.csv.\n")

#=========================================================
# Are ALL West et al. (2025) named diet species correctly represented?
#=========================================================
## West et al. (2025) explicitly names harp seal diet species: Arctic
## cod, Atlantic and Greenland cod (Gadus spp.), capelin, Atlantic
## herring, and shrimp (per DFO Fs97-6-3328, the stomach-content source
## cited alongside this check). The Greenland-cod case is already
## confirmed above: it's folded into "Other piscivorous fish", which
## gets ZERO diet fraction -- exactly the internal inconsistency SOW
## deliverable 2 asks about. This block generalizes that same check to
## EVERY named species, rather than relying on having spotted this one
## case by hand -- if another named species is also mapped to a
## functional group with zero (or missing) diet fraction, it'll show
## up here instead of being missed.
##
## Mapping from West et al.'s species names to this model's functional
## group names -- VERIFY these against your own group definitions,
## since a wrong mapping here would hide a real inconsistency instead
## of flagging it. Atlantic cod is split into two size-based groups in
## this model (Cod adult / Cod juvenile), so both are checked.

west_et_al_species_map <- data.table(
  WestEtAl_Species = c(
    "Arctic cod",
    "Atlantic cod (adult)",
    "Atlantic cod (juvenile)",
    "Greenland cod",
    "Capelin",
    "Atlantic herring",
    "Shrimp"
  ),
  Mapped_FunctionalGroup = c(
    "Arctic cod",
    "Cod adult",
    "Cod juvenile",
    "Other piscivorous fish",
    "Capelin",
    "Herring",
    "Shrimp"
  )
)

if(exists("baseline_diet_cache") && nrow(baseline_diet_cache) > 0){
  
  west_et_al_check <- merge(
    west_et_al_species_map,
    baseline_diet_cache[, .(Simulation, Group, DietFraction)],
    by.x = "Mapped_FunctionalGroup",
    by.y = "Group",
    all.x = TRUE,
    allow.cartesian = TRUE
  )
  
  setorder(west_et_al_check, WestEtAl_Species, Simulation)
  
  cat("\n===== West et al. (2025) named diet species vs. this model's",
      "functional groups =====\n")
  cat("Source for the named species list: DFO Fs97-6-3328 (stomach",
      "content report). Cross-referenced against baseline_diet_cache",
      "(this model's own diet matrix) -- confirms whether EVERY named",
      "species is actually represented with nonzero seal-diet content,",
      "not just the Greenland cod case already found.\n\n")
  print(west_et_al_check)
  
  flagged <- west_et_al_check[is.na(DietFraction) | DietFraction == 0]
  
  if(nrow(flagged) > 0){
    cat("\n-> FLAGGED: the following named West et al. diet species",
        "show ZERO or MISSING diet fraction in this model, despite",
        "being explicitly named as harp seal prey in the source paper.",
        "NA means the mapped functional group wasn't found in",
        "baseline_diet_cache at all for that Simulation (check the",
        "mapping/group name); 0 means the group exists but gets no",
        "seal diet, the same class of inconsistency already confirmed",
        "for Greenland cod:\n")
    print(flagged)
  } else {
    cat("\n-> All named West et al. diet species show nonzero diet",
        "fraction in every Simulation checked -- no further internal",
        "inconsistencies of this type detected among the named",
        "species.\n")
  }
  
  cat("========================================================================================\n")
  
  fwrite(
    west_et_al_check,
    file.path(root_dir, "HarpSeal_WestEtAl_DietSpecies_Check.csv")
  )
  
} else {
  
  cat("\nSkipping West et al. named-species check -- baseline_diet_cache",
      "isn't available.\n")
  
}

## ---- Step 2: per model folder, compute Q_species and Y_species.
## Uses the model's OWN Diet composition file if present (diet-20/
## diet-20pisciv); otherwise falls back to that Simulation's BASELINE
## diet matrix (qb-XX variants); otherwise Cod-adult only, as a last
## resort, if even the baseline diet file is missing for that
## Simulation ----

## Compact diagnostic log: one row per model, recording exactly which
## diet-data tier was used and (when none was found) what CSV files
## actually exist in that folder. Printed as ONE table after the loop
## completes, instead of scattered messages buried in a long console
## log -- meant to directly answer "why is Simulation X sparse" without
## another round of guessing at file names.

diet_search_log <- list()
qy_results <- rbindlist(
  
  lapply(model_dirs, function(model){
    
    tryCatch({
      
      cat("\nProcessing Q/Y for:", model, "\n")
      
      sim_name   <- basename(dirname(model))
      model_name <- basename(model)
      
      bio <- biomass_baseline[Simulation == sim_name]
      
      if(nrow(bio) == 0){
        warning(
          "No baseline Biomass available for Simulation '",
          sim_name, "' -- skipping Q/Y for ", model
        )
        return(NULL)
      }
      
      #=========================================================
      # Harp seal
      #=========================================================
      
      seal_row <- bio[grepl(harp_pattern, Group, perl = TRUE)]
      
      if(nrow(seal_row) == 0)
        stop(
          "No harp seal group found in baseline Basic estimates for ",
          sim_name
        )
      
      seal_row <- seal_row[1]
      seal_id  <- as.character(seal_row$GroupID)
      
      
      #=========================================================
      # Harp seal biomass
      #=========================================================
      
      biomass_csv_row <- seal_biomass_csv_lookup[
        Simulation == sim_name & Model == model_name
      ]
      
      if(nrow(biomass_csv_row) == 1){
        
        seal_row$Biomass <- biomass_csv_row$Biomass_Seal_CSV
        
      } else {
        
        is_biomass_variant <- model_name %in% 
          c("b-30", "b-15", "b+15", "b+30")
        
        warning(
          "No entry in HarpSeal_biomass.csv for Simulation '",
          sim_name,
          "', Model '", model_name,
          "' -- falling back to baseline Basic_estimates value."
        )
        
        if(is_biomass_variant){
          warning(
            "This is a biomass-sensitivity Model. Check ",
            "HarpSeal_biomass.csv."
          )
        }
        
      }
      
      
      #=========================================================
      # Harp seal Q/B and Q
      #=========================================================
      
      mult <- if(
        model_name %in% names(qb_multiplier)
      ) {
        qb_multiplier[[model_name]]
      } else {
        1.00
      }
      
      Q_seal <- seal_row$QB * mult * seal_row$Biomass
      
      
      #=========================================================
      # Find diet composition file
      #=========================================================
      
      diet_file <- list.files(
        model,
        pattern = "Diet.*(comp|matrix).*\\.csv$",
        full.names = TRUE,
        ignore.case = TRUE
      )[1]
      
      if(is.na(diet_file)){
        
        all_csv_this_model <- list.files(
          model,
          pattern = "\\.csv$",
          full.names = FALSE,
          ignore.case = TRUE
        )
        
        cat(
          "  No diet file matched in ", model,
          " -- CSV files actually present: ",
          if(length(all_csv_this_model) > 0)
            paste(all_csv_this_model, collapse = ", ")
          else
            "(none)",
          "\n",
          sep = ""
        )
        
      } else {
        
        all_csv_this_model <- character(0)
        
      }
      
      
      #=========================================================
      # Resolve DietFraction
      #
      # IMPORTANT:
      # DietFraction comes from the model diet matrix.
      # No area weighting.
      # No hardcoded DietFraction override.
      #=========================================================
      
      tier_used <- NA_character_
      
      if(!is.na(diet_file)){
        
        #-------------------------------------------------------
        # Tier 1:
        # Use this model's own diet matrix
        #-------------------------------------------------------
        
        tier_used <- paste0(
          "Tier 1 (own file: ",
          basename(diet_file),
          ")"
        )
        
        out <- read_diet_file(
          diet_file
        )
        
      } else if(
        exists("baseline_diet_cache") &&
        nrow(
          baseline_diet_cache[Simulation == sim_name]
        ) > 0
      ){
        
        #-------------------------------------------------------
        # Tier 2:
        # Reuse the Simulation's baseline diet matrix
        #
        # Appropriate for models where Q/B or biomass changes,
        # but diet composition does not.
        #-------------------------------------------------------
        
        tier_used <- 
          "Tier 2 (reused baseline Simulation's diet file)"
        
        cat(
          "  No diet file in ", model,
          " -- reusing ", sim_name,
          " baseline diet matrix.\n",
          sep = ""
        )
        
        out <- copy(
          baseline_diet_cache[
            Simulation == sim_name
          ]
        )
        
        out[, Simulation := NULL]
        
      } else {
        
        #-------------------------------------------------------
        # Tier 3:
        # No diet matrix available.
        # Only Cod adult can be calculated if a lookup exists.
        #
        # This is a fallback only.
        #-------------------------------------------------------
        
        tier_used <- 
          "Tier 3 (Cod adult only, hardcoded lookup)"
        
        cat(
          "  No diet file for ", model,
          " or its Simulation's baseline.",
          " Computing Q/Y for Cod adult only.\n",
          sep = ""
        )
        
        cod_id <- results_raw[
          Simulation == sim_name &
            Model == model_name &
            Group == "Cod adult"
        ]$GroupID[1]
        
        if(is.na(cod_id)){
          
          warning(
            "Couldn't find 'Cod adult' GroupID for ",
            model,
            " -- skipping Q/Y entirely for this model."
          )
          
          return(NULL)
        }
        
        # Only use the hardcoded lookup as a last-resort fallback.
        lookup_row <- seal_qb_diet_lookup[
          Simulation == sim_name &
            Model == model_name
        ]
        
        if(nrow(lookup_row) == 0){
          
          warning(
            "No diet file and no fallback DietFraction available ",
            "for Cod adult in ",
            model,
            " -- skipping."
          )
          
          return(NULL)
        }
        
        out <- data.table(
          GroupID      = cod_id,
          Group        = "Cod adult",
          DietFraction = lookup_row$DietFraction_CodAdult
        )
        
      }
      
      
      #=========================================================
      # Check diet fractions
      #=========================================================
      
      if(nrow(out) == 0){
        warning(
          "No diet fractions found for ",
          sim_name, "/", model_name
        )
        return(NULL)
      }
      
      diet_search_log[[length(diet_search_log) + 1]] <<- data.table(
        Simulation      = sim_name,
        Model           = model_name,
        TierUsed        = tier_used,
        SpeciesInOut    = nrow(out),
        OtherCSVsIfNone = if(
          length(all_csv_this_model) > 0
        ) {
          paste(
            all_csv_this_model,
            collapse = "; "
          )
        } else {
          ""
        }
      )
      
      
      #=========================================================
      # Calculate Q_species
      #=========================================================
      
      out[, Q_species := DietFraction * Q_seal]
      
      
      #=========================================================
      # Fishing mortality and biomass
      #=========================================================
      
      fishing_this_model <- results_raw[
        Simulation == sim_name &
          Model == model_name,
        .(
          GroupID,
          Fishing
        )
      ]
      
      out <- merge(
        out,
        fishing_this_model,
        by = "GroupID",
        all.x = TRUE
      )
      
      out <- merge(
        out,
        bio[, .(
          GroupID,
          Biomass
        )],
        by = "GroupID",
        all.x = TRUE
      )
      
      
      #=========================================================
      # Calculate Y_species and Q/Y
      #=========================================================
      
      out[, Y_species := Fishing * Biomass]
      
      out[, Q_over_Y := fifelse(
        Y_species > 0,
        Q_species / Y_species,
        NA_real_
      )]
      
      
      #=========================================================
      # Remove seal self-consumption and invalid Y
      #=========================================================
      
      out <- out[
        GroupID != as.integer(seal_id)
      ]
      
      # out <- out[
      #   !is.na(Y_species) &
      #     Y_species > 0
      # ]
      
      
      #=========================================================
      # Add identifiers
      #=========================================================
      
      out[, Simulation := sim_name]
      out[, Model := model_name]
      out[, Predator := seal_row$Group]
      
      
      #=========================================================
      # Return
      #=========================================================
      
      out[, .(
        GroupID,
        Group,
        DietFraction,
        Q_species,
        Y_species,
        Q_over_Y,
        Simulation,
        Model,
        Predator
      )]
      
    }, error = function(e){
      
      warning(
        "Q/Y failed for ",
        model,
        ": ",
        conditionMessage(e),
        " -- skipping this model/Simulation, continuing with the rest."
      )
      
      NULL
      
    })
    
  }),
  
  fill = TRUE
  
)

qy_results_diet <- copy(qy_results)
## Print the diet-source summary table -- this is THE thing to check
## first if any Simulation looks sparse in the Q/Y heatmap: it shows,
## per Simulation/Model, whether a diet file was found and which tier
## was used, or (if none was found at all) exactly what CSV files DO
## exist in that folder so the real filename can be matched against
## the search pattern above.

diet_search_summary <- rbindlist(diet_search_log, fill = TRUE)
setorder(diet_search_summary, Simulation, Model)

cat("\n===== Diet-source summary (one row per model) =====\n")
print(diet_search_summary, nrows = 200)
cat("=====================================================\n")
if(is.null(qy_results) || nrow(qy_results) == 0){
  
  warning(
    "Q/Y ratio table came back empty -- check seal_qb_diet_lookup",
    " covers every Simulation/Model you actually ran."
  )
  
} else {
  
  #=========================================================
  # Save DietFraction before combining N/S simulations
  #=========================================================
  
  diet_fraction_raw <- copy(
    qy_results[
      ,
      .(
        Simulation,
        Model,
        GroupID,
        Group,
        DietFraction
      )
    ]
  )
  
  #=========================================================
  # Combine N/S simulations
  #=========================================================
  
  ## Raw (pre-combination) copy, same purpose as results_raw /
  ## biomass_results_raw above -- keeps DietFraction and the per-
  ## region N/S Q_species/Y_species values available for diagnostics
  ## further down.
  
  qy_results_raw <- copy(qy_results)
  
  qy_results <- combine_NS_simulations(
    qy_results,
    id_cols    = c("GroupID", "Group", "Model", "Predator"),
    value_cols = c("Q_species", "Y_species")
  )
  
  qy_results[, Q_over_Y := fifelse(
    Y_species > 0,
    Q_species / Y_species,
    NA_real_
  )]
  
  
  
  # Restore the original DietFraction values
  qy_results[
    diet_fraction_raw,
    on = .(
      Simulation,
      Model,
      GroupID,
      Group
    ),
    DietFraction := i.DietFraction
  ]
  
  
  #=========================================================
  # IMPORTANT:
  #
  # The two combined simulations, 2018-2020_5050 and
  # 2018-2020_8020, do not have an original single
  # DietFraction because they are created from two different
  # sub-models.
  #
  # We leave those as NA rather than inventing a weighting.
  # This does NOT affect Q_species, Y_species or Q_over_Y.
  #=========================================================
  
  
  setorder(
    qy_results,
    Simulation,
    Model,
    Group
  )
  
  
  fwrite(
    qy_results,
    file.path(
      root_dir,
      "HarpSeal_ConsumptionYield_QY_AllModels.csv"
    )
  )
  
  
  cat("\nQ/Y results (head):\n")
  print(head(qy_results))
  
  
  #=========================================================
  # Diagnostic: Ecopath-implied vs reconstructed consumption
  #=========================================================
  
  biomass_baseline_combined <- combine_NS_simulations(
    biomass_baseline,
    id_cols    = c("Group"),
    value_cols = c("Biomass", "QB")
  )
  
  
  seal_biomass_qb <- biomass_baseline_combined[
    grepl(harp_pattern, Group, perl = TRUE),
    .(
      Simulation,
      Biomass_Seal = Biomass,
      QB_Seal_Baseline = QB
    )
  ]
  
  
  cod_ecopath_side <- results[
    Group == "Cod adult",
    .(
      Simulation = as.character(Simulation),
      Model = as.character(Model),
      Predator_M2,
      Fishing
    )
  ]
  
  
  cod_biomass <- biomass_baseline_combined[
    Group == "Cod adult",
    .(
      Simulation,
      Biomass_Cod = Biomass
    )
  ]
  
  
  cod_reconstructed_side <- qy_results[
    Group == "Cod adult",
    .(
      Simulation,
      Model,
      DietFraction,
      Q_species,
      Y_species,
      Q_over_Y
    )
  ]
  
  
  cod_check <- merge(
    cod_ecopath_side,
    cod_reconstructed_side,
    by = c("Simulation", "Model"),
    all = TRUE
  )
  
  
  cod_check <- merge(
    cod_check,
    cod_biomass,
    by = "Simulation",
    all.x = TRUE
  )
  
  
  cod_check <- merge(
    cod_check,
    seal_biomass_qb,
    by = "Simulation",
    all.x = TRUE
  )
  
  
  cod_check[
    ,
    QB_Multiplier_Used := sapply(
      Model,
      function(m)
        if(m %in% names(qb_multiplier))
          qb_multiplier[[m]]
      else
        1.00
    )
  ]
  
  
  cod_check[
    ,
    QB_Seal_Used :=
      QB_Seal_Baseline * QB_Multiplier_Used
  ]
  
  
  # Ecopath implied consumption
  cod_check[
    ,
    EcopathImpliedConsumption :=
      Predator_M2 * Biomass_Cod
  ]
  
  
  setnames(
    cod_check,
    "Q_species",
    "ReconstructedConsumption"
  )
  
  
  cod_check[
    ,
    PctDiff :=
      100 *
      (
        ReconstructedConsumption -
          EcopathImpliedConsumption
      ) /
      EcopathImpliedConsumption
  ]
  
  
  cat(
    "\n===== Cod adult: Ecopath-implied vs reconstructed seal consumption =====\n"
  )
  
  
  print(
    cod_check[order(Simulation, Model)]
  )
  
  
  cat(
    "===========================================================================\n"
  )
  
  
  #=========================================================
  # Area-weighted vs biomass-weighted N/S combination
  # Diagnostic only
  #=========================================================
  
  cod_biomass_raw <- biomass_baseline[
    Group == "Cod adult",
    .(
      Simulation,
      Biomass_Cod_Raw = Biomass
    )
  ]
  
  
  cod_raw_data <- results_raw[
    Group == "Cod adult",
    .(
      Simulation,
      Model,
      Predator_M2,
      Fishing
    )
  ]
  
  
  cod_raw_data <- merge(
    cod_raw_data,
    cod_biomass_raw,
    by = "Simulation",
    all.x = TRUE
  )
  
  
  combo_compare <- rbindlist(
    
    lapply(
      names(combined_simulation_map),
      
      function(combo_name){
        
        pair <- combined_simulation_map[[combo_name]]
        
        dt_N <- cod_raw_data[
          Simulation == pair[["N"]]
        ]
        
        dt_S <- cod_raw_data[
          Simulation == pair[["S"]]
        ]
        
        if(nrow(dt_N) == 0 || nrow(dt_S) == 0)
          return(NULL)
        
        
        merged <- merge(
          dt_N,
          dt_S,
          by = "Model",
          suffixes = c("_N", "_S")
        )
        
        
        # Current area-weighted method
        merged[
          ,
          M2_AreaWeighted :=
            area_weight_N * Predator_M2_N +
            area_weight_S * Predator_M2_S
        ]
        
        
        merged[
          ,
          F_AreaWeighted :=
            area_weight_N * Fishing_N +
            area_weight_S * Fishing_S
        ]
        
        
        merged[
          ,
          M2_over_F_AreaWeighted :=
            M2_AreaWeighted / F_AreaWeighted
        ]
        
        
        # Biomass-weighted diagnostic
        merged[
          ,
          M2_BiomassWeighted :=
            (
              area_weight_N *
                Biomass_Cod_Raw_N *
                Predator_M2_N +
                area_weight_S *
                Biomass_Cod_Raw_S *
                Predator_M2_S
            ) /
            (
              area_weight_N *
                Biomass_Cod_Raw_N +
                area_weight_S *
                Biomass_Cod_Raw_S
            )
        ]
        
        
        merged[
          ,
          F_BiomassWeighted :=
            (
              area_weight_N *
                Biomass_Cod_Raw_N *
                Fishing_N +
                area_weight_S *
                Biomass_Cod_Raw_S *
                Fishing_S
            ) /
            (
              area_weight_N *
                Biomass_Cod_Raw_N +
                area_weight_S *
                Biomass_Cod_Raw_S
            )
        ]
        
        
        merged[
          ,
          M2_over_F_BiomassWeighted :=
            M2_BiomassWeighted /
            F_BiomassWeighted
        ]
        
        
        merged[
          ,
          Simulation := combo_name
        ]
        
        
        merged[
          ,
          .(
            Simulation,
            Model,
            Biomass_Cod_N = Biomass_Cod_Raw_N,
            Biomass_Cod_S = Biomass_Cod_Raw_S,
            M2_over_F_AreaWeighted,
            M2_over_F_BiomassWeighted
          )
        ]
        
      }
    ),
    
    fill = TRUE
  )
  
  
  if(nrow(combo_compare) > 0){
    
    combo_compare[
      ,
      PctDiff_MethodChoice :=
        100 *
        (
          M2_over_F_BiomassWeighted -
            M2_over_F_AreaWeighted
        ) /
        M2_over_F_AreaWeighted
    ]
    
    
    cat(
      "\n===== Cod adult M2/F: area-weighted vs biomass-weighted N/S combination =====\n"
    )
    
    
    print(
      combo_compare[order(Simulation, Model)]
    )
    
    
    cat(
      "===============================================================================\n"
    )
    
  }
  
}

#=========================================================
# Data-driven species categorization (REPLACES hardcoded guesses)
#=========================================================
## The `seal_prey_species` / `exploited_species` vectors defined near
## the top of this script were placeholder guesses, written before any
## real diet data was available. Now that baseline_diet_cache exists
## (built above from the Diet composition files you provided), both
## categories can be derived directly from data instead:
##
##   - "Seal prey species": ANY functional group with a nonzero diet
##     fraction in the harp seal's diet, in the baseline Diet
##     composition file, for ANY Simulation (unioned across periods).
##   - "Exploited seal prey species": the subset of the above that ALSO
##     has Fishing > 0 somewhere in `results` -- both conditions
##     required, matching the SOW's "seal prey species AND exploited
##     seal prey species" framing exactly. `results` already only
##     contains rows with Fishing > 0 (filtered inside
##     compare_predator_vs_fishing()), so this is a straightforward
##     intersection.
##
## This OVERWRITES the hardcoded seal_prey_species/exploited_species
## defined at the top of the script. If baseline_diet_cache isn't
## available for some reason, this falls back to the old hardcoded
## lists with a loud warning rather than silently using an empty set.

if(exists("baseline_diet_cache") && nrow(baseline_diet_cache) > 0){
  
  seal_prey_species <- sort(unique(
    baseline_diet_cache[DietFraction > 0]$Group
  ))
  
  ## FIXED: `unique(results$Group)` used to implicitly mean "has
  ## Fishing > 0 somewhere", back when compare_predator_vs_fishing()
  ## dropped every Fishing == 0 row before `results` was ever built.
  ## Now that those rows are kept (see the fix in
  ## compare_predator_vs_fishing() above -- needed so species with
  ## zero fishing but real predation still show up in Figure 10),
  ## `unique(results$Group)` would include essentially every modeled
  ## group whether or not it's actually fished, silently collapsing
  ## "exploited seal prey species" into just "seal prey species".
  ## Filtering on `Fishing > 0` explicitly here restores the original,
  ## correct meaning: actually fished somewhere, not merely present.
  exploited_species <- sort(intersect(
    seal_prey_species,
    unique(results[Fishing > 0]$Group)
  ))
  
  cat(
    "\nData-driven species categorization (overrides the hardcoded",
    "placeholder lists defined near the top of this script):\n",
    "  Seal prey species (", length(seal_prey_species), "): ",
    paste(seal_prey_species, collapse = ", "), "\n",
    "  Exploited seal prey species (", length(exploited_species), "): ",
    paste(exploited_species, collapse = ", "), "\n",
    sep = ""
  )
  
  ## Added per explicit request, for the RATIO plots specifically
  ## (M2/F, Q/Y time series and heatmaps -- NOT Table 2 or Figure 10's
  ## mortality-composition chart, which deliberately want the BROADER
  ## `seal_prey_species` picture, including species with zero fishing
  ## or zero predation, so those cases stay visible rather than being
  ## hidden). `exploited_species` only requires Fishing > 0 SOMEWHERE
  ## and, separately, nonzero diet SOMEWHERE -- it does NOT require
  ## both to be true in the SAME Simulation/Model, so a species could
  ## qualify as "exploited" while never actually having a defined,
  ## plottable M2/F ratio anywhere. `species_with_valid_ratio` is
  ## stricter: a Group only qualifies if there's at least ONE
  ## Simulation/Model row in `results` where Fishing > 0 AND
  ## Predator_M2 > 0 SIMULTANEOUSLY -- i.e. Predator_vs_Fishing is a
  ## real, finite, nonzero ratio somewhere, not just theoretically
  ## computable from unrelated rows.
  species_with_valid_ratio <- sort(unique(
    results[Fishing > 0 & Predator_M2 > 0]$Group
  ))
  
  cat(
    "  Species with at least one valid (nonzero F AND nonzero M2 in",
    " the SAME Simulation/Model) M2/F ratio (",
    length(species_with_valid_ratio), "): ",
    paste(species_with_valid_ratio, collapse = ", "), "\n",
    sep = ""
  )
  
} else {
  
  warning(
    "No baseline_diet_cache available -- falling back to the",
    " hardcoded placeholder seal_prey_species/exploited_species lists",
    " defined near the top of this script. Those are guesses and",
    " should not be trusted for the report until real diet data",
    " confirms them."
  )
  
  ## Fallback so downstream figures referencing species_with_valid_ratio
  ## don't error with "object not found" when this branch is taken --
  ## still computable from `results` alone (doesn't depend on
  ## baseline_diet_cache), so no reason to skip it here.
  species_with_valid_ratio <- sort(unique(
    results[Fishing > 0 & Predator_M2 > 0]$Group
  ))
  
}

#=========================================================
# Combined M2/F and Q/Y ratio comparison (SOW deliverable 1)
#=========================================================
## Puts both ratios side by side, at species level, across every
## Simulation/Model -- the direct analogue of the paper's headline
## "17x" (M2/F) and "24x" (Q/Y) figures, and what deliverables 1 and 4
## (whether these ratios are overstated / which conclusions are robust)
## are built from.

if(exists("qy_results") && nrow(qy_results) > 0){
  
  ## Explicit as.character() on both sides of the join key: `results`
  ## now converts Model/Simulation to factors much earlier in the
  ## script (right after `results` itself is renamed/combined) than it
  ## used to, whereas `qy_results` keeps them as character throughout.
  ## data.table's merge() is expected to coerce a factor-vs-character
  ## join correctly on its own, but forcing both sides to character
  ## explicitly here removes any dependence on that behavior -- safer
  ## than trusting an implicit coercion for a merge this many
  ## downstream figures depend on.
  
  ratio_compare <- merge(
    results[, .(
      Simulation = as.character(Simulation),
      Model = as.character(Model),
      Group,
      M2_over_F = Predator_vs_Fishing
    )],
    qy_results[, .(
      Simulation = as.character(Simulation),
      Model = as.character(Model),
      Group,
      Q_over_Y
    )],
    by = c("Simulation", "Model", "Group"),
    all = TRUE
  )
  
  ratio_compare[, Category := fifelse(
    Group %in% exploited_species, "Exploited seal prey species",
    fifelse(Group %in% seal_prey_species, "Seal prey species (non-exploited)", "Other")
  )]
  
  setorder(ratio_compare, Simulation, Model, Group)
  
  fwrite(
    ratio_compare,
    file.path(root_dir, "HarpSeal_M2F_vs_QY_Ratios_AllModels.csv")
  )
  
  cat("\nM2/F vs Q/Y ratio comparison (head):\n")
  print(head(ratio_compare, 20))
  
  ## Flag species/models where the two ratios disagree sharply in
  ## magnitude -- e.g. one metric says seal impact far exceeds fishing
  ## while the other says they're roughly comparable. Exactly the kind
  ## of check needed for "assessment of conclusions: robust vs
  ## sensitive" (deliverable 4).
  
  ratio_compare[, Ratio_discrepancy := abs(log(M2_over_F / Q_over_Y))]
  
  cat("\nLargest M2/F vs Q/Y discrepancies (higher = ratios disagree more):\n")
  print(head(ratio_compare[order(-Ratio_discrepancy)], 10))
  
  ## Species-level summary restricted to the two SOW-required
  ## categories (drops "Other" groups not flagged as seal prey at all)
  
  species_level_summary <- ratio_compare[
    Category != "Other",
    .(
      Mean_M2_over_F = mean(M2_over_F, na.rm = TRUE),
      Mean_Q_over_Y  = mean(Q_over_Y, na.rm = TRUE)
    ),
    by = .(Group, Category)
  ][order(Category, -Mean_M2_over_F)]
  
  fwrite(
    species_level_summary,
    file.path(root_dir, "HarpSeal_SpeciesLevel_SealPrey_vs_Exploited.csv")
  )
  
  cat("\nSpecies-level summary (seal prey vs exploited seal prey):\n")
  print(species_level_summary)
  
}

#=========================================================
# Table 2: M2/F and Q/Y by functional group, for 1985-1988,
# 2018-2020_5050 and 2018-2020_8020 (baseline model only)
#=========================================================
## Reproduces the shape of the manuscript's Table 2: one row per seal-
## prey functional group (`seal_prey_species`, data-driven -- ANY
## nonzero harp seal diet fraction in ANY period), M2/F and Q/Y side
## by side, for the three periods the table compares (2013-2015 is
## deliberately excluded, matching the manuscript's own Table 2).
##
## Numeric cells come straight from `results`/`qy_results` (baseline
## Model only). Non-numeric cells reproduce the manuscript's own
## conventions for a cell with no ratio to report -- using the SAME
## three-way distinction as `diagnose_group_gap()`/`group_status_table`
## earlier in this script, computed here directly from the already
## COMBINED baseline tables (so it reflects each combined 2018-2020
## Simulation's own N/S-area-weighted values, not a re-derivation from
## the raw N/S folders):
##   "-"           -- FG absent from that Simulation's EwE model
##                    structure entirely (both Fishing and the metric's
##                    own numerator come back NA after merging against
##                    the full Group x Simulation grid below -- i.e.
##                    this Group/Simulation combination never existed
##                    in `results`/`qy_results` at all).
##   "0 (no F)"    -- FG exists and is preyed upon, but Fishing == 0,
##                    so the ratio is undefined (matches "0 no F").
##   "0 (no diet)" -- FG exists and is fished, but has zero predation
##                    on it (Predator_M2 == 0 for M2/F, Q_species == 0
##                    for Q/Y) -- matches "0 no diet".
## M2/F and Q/Y get INDEPENDENT status columns (Status_M2F/Status_QY),
## since the two metrics can differ for the same Group/Simulation (a
## group's Ecopath-native Predator_M2 and this script's reconstructed
## Q_species aren't guaranteed to hit exactly zero at the same time --
## see the earlier Ecopath-implied-vs-reconstructed-consumption check).
##
## NOT auto-relabelled: cells the manuscript instead marks "XX low F"
## rather than reporting a huge, unstable ratio (e.g. American Plaice
## juvenile, Medium/Small benthivorous fish, Sandlance) -- how small a
## Fishing value is "too small to trust" is an editorial judgement
## call, not something to silently hard-code here. Instead,
## `LowFishingFlag` marks any numeric cell whose Fishing is below the
## 5th percentile of all nonzero baseline Fishing values actually
## observed, so you can review those specific cells and decide,
## per-cell, whether to keep the number or write "XX" as the
## manuscript does -- edit `low_fishing_threshold` directly if you'd
## rather use a fixed cutoff instead of a data-driven percentile.
target_sims_table2 <- c("1985-1988", "2018-2020_5050", "2018-2020_8020")

low_fishing_threshold <- quantile(
  results[Model == "baseline" & Fishing > 0]$Fishing,
  probs = 0.05, na.rm = TRUE
)

cat("\nTable 2: flagging numeric cells with Fishing below the 5th",
    "percentile of nonzero baseline Fishing values (",
    round(low_fishing_threshold, 5),
    ") as candidates for the manuscript's own 'XX low F' treatment --",
    "this is a REVIEW FLAG, not an automatic relabel; decide per cell.\n")

## Full Group x Simulation grid, so a Group/Simulation combination
## that never existed in `results`/`qy_results` at all correctly shows
## up as NOT_IN_STRUCTURE ("-") rather than just being absent from the
## output with no explanation.
table2_grid <- CJ(
  Group = seal_prey_species,
  Simulation = target_sims_table2,
  unique = TRUE
)

## Missing_N/Missing_S pulled straight from `results`/`qy_results` --
## set inside combine_NS_simulations() (see the earlier fix there):
## TRUE means that side's contribution was zero-filled because the
## Group wasn't a modeled functional group in that region at all, NOT
## that the value is otherwise untrustworthy. Carried through here so
## Table 2 can flag exactly which cells rest on that assumption,
## rather than looking identical to a cell where both regions agreed.
## NA (not TRUE/FALSE) for 1985-1988, which was never a combined
## Simulation in the first place.
table2_m2f <- results[
  Model == "baseline" & as.character(Simulation) %in% target_sims_table2,
  .(
    Group, Simulation = as.character(Simulation),
    Fishing, Predator_M2, M2_over_F = Predator_vs_Fishing,
    Missing_N_M2F = Missing_N, Missing_S_M2F = Missing_S
  )
]

table2_qy <- qy_results[
  Model == "baseline" & Simulation %in% target_sims_table2,
  .(
    Group, Simulation, Q_species, Y_species, Q_over_Y,
    Missing_N_QY = Missing_N, Missing_S_QY = Missing_S
  )
]

table2_data <- merge(
  table2_grid, table2_m2f, by = c("Group", "Simulation"), all.x = TRUE
)
table2_data <- merge(
  table2_data, table2_qy, by = c("Group", "Simulation"), all.x = TRUE
)

table2_data[, Status_M2F := fifelse(
  is.na(Fishing) & is.na(Predator_M2), "NOT_IN_STRUCTURE",
  fifelse(
    !is.na(Fishing) & Fishing == 0, "ZERO_FISHING",
    fifelse(
      !is.na(Predator_M2) & Predator_M2 == 0, "ZERO_PREDATION",
      "OK"
    )
  )
)]

table2_data[, Status_QY := fifelse(
  is.na(Fishing) & is.na(Q_species), "NOT_IN_STRUCTURE",
  fifelse(
    !is.na(Fishing) & Fishing == 0, "ZERO_FISHING",
    fifelse(
      !is.na(Q_species) & Q_species == 0, "ZERO_PREDATION",
      fifelse(is.na(Q_species), "NO_DIET_DATA", "OK")
    )
  )
)]

table2_data[, LowFishingFlag :=
              !is.na(Fishing) & Fishing > 0 & Fishing < low_fishing_threshold]

## SourceRegionNote: per explicit request -- when a species is present
## in only ONE of the two regional N/S sub-models for a combined
## 2018-2020 Simulation, combine_NS_simulations() (see the earlier
## fix) treats the missing side as contributing ZERO and still
## computes the normal area-weighted average from that, rather than
## dropping the species -- e.g. Haddock, present only in the South,
## gets area_weight_N * 0 + area_weight_S * Haddock_S. That value is
## real and worth keeping, but it rests on an assumption (absence from
## one region's own structure means negligible/zero there) that's
## worth flagging directly in the table rather than letting it look
## identical to a cell where both regions genuinely agreed. Separate
## notes for M2/F and Q/Y since their own N/S combinations
## (Missing_N_M2F/Missing_S_M2F vs Missing_N_QY/Missing_S_QY) could in
## principle differ. NA (no note) for 1985-1988, which was never a
## combined Simulation.
table2_data[, SourceNote_M2F := fifelse(
  is.na(Missing_N_M2F) | is.na(Missing_S_M2F), "",
  fifelse(
    Missing_N_M2F, " [S only]",
    fifelse(Missing_S_M2F, " [N only]", "")
  )
)]

table2_data[, SourceNote_QY := fifelse(
  is.na(Missing_N_QY) | is.na(Missing_S_QY), "",
  fifelse(
    Missing_N_QY, " [S only]",
    fifelse(Missing_S_QY, " [N only]", "")
  )
)]

## Matches the manuscript's own "<0.01" convention for a tiny but
## genuinely nonzero ratio, instead of rounding it down to a
## misleading "0.00".
format_ratio <- function(x){
  fifelse(
    is.na(x), NA_character_,
    fifelse(
      x > 0 & x < 0.01, "<0.01",
      sprintf("%.2f", x)
    )
  )
}

table2_data[, M2F_Display := fifelse(
  Status_M2F == "NOT_IN_STRUCTURE", "-",
  fifelse(
    Status_M2F == "ZERO_FISHING", "0 (no F)",
    fifelse(
      Status_M2F == "ZERO_PREDATION", "0 (no diet)",
      paste0(format_ratio(M2_over_F), ifelse(LowFishingFlag, " [low F]", ""), SourceNote_M2F)
    )
  )
)]

table2_data[, QY_Display := fifelse(
  Status_QY %in% c("NOT_IN_STRUCTURE"), "-",
  fifelse(
    Status_QY == "ZERO_FISHING", "0 (no F)",
    fifelse(
      Status_QY == "ZERO_PREDATION", "0 (no diet)",
      fifelse(
        Status_QY == "NO_DIET_DATA", "NA (no diet data)",
        paste0(format_ratio(Q_over_Y), ifelse(LowFishingFlag, " [low F]", ""), SourceNote_QY)
      )
    )
  )
)]

## Long -> wide, exact Table 2 layout: M2/F columns (one per period)
## followed by Q/Y columns (one per period), one row per FG.
table2_m2f_wide <- dcast(table2_data, Group ~ Simulation, value.var = "M2F_Display")
setnames(table2_m2f_wide, target_sims_table2, paste0("M2F_", target_sims_table2))

table2_qy_wide <- dcast(table2_data, Group ~ Simulation, value.var = "QY_Display")
setnames(table2_qy_wide, target_sims_table2, paste0("QY_", target_sims_table2))

table2_final <- merge(table2_m2f_wide, table2_qy_wide, by = "Group")
setorder(table2_final, Group)
setcolorder(
  table2_final,
  c("Group", paste0("M2F_", target_sims_table2), paste0("QY_", target_sims_table2))
)

cat("\n===== Table 2: M2/F and Q/Y by functional group, baseline model =====\n")
print(table2_final)

fwrite(table2_final, file.path(root_dir, "Table2_M2F_QY_ByFunctionalGroup.csv"))

## West et al. (2025) published only the single aggregate headline
## values per period (M2/F = 1.3, Q/Y = 1.7 for 1985-1988; M2/F = 17,
## Q/Y = 24 for 2018-2020) -- there is no per-FG published series to
## compare against instead, so `AboveWestEtAl` applies those same
## thresholds uniformly across every FG, flagging any OK numeric cell
## that exceeds them. Saved in the companion long-format file below so
## it can drive conditional (red-fill) formatting in Excel/Word.
table2_data[, AboveWestEtAl := fifelse(
  Status_M2F != "OK" & Status_QY != "OK", FALSE,
  fifelse(
    grepl("2018-2020", Simulation),
    (!is.na(M2_over_F) & M2_over_F > 17) | (!is.na(Q_over_Y) & Q_over_Y > 24),
    (!is.na(M2_over_F) & M2_over_F > 1.3) | (!is.na(Q_over_Y) & Q_over_Y > 1.7)
  )
)]

fwrite(table2_data, file.path(root_dir, "Table2_M2F_QY_ByFunctionalGroup_Long.csv"))

cat("\nTable 2 saved: 'Table2_M2F_QY_ByFunctionalGroup.csv' (wide, ready",
    "to paste into the manuscript table) and",
    "'Table2_M2F_QY_ByFunctionalGroup_Long.csv' (one row per FG x",
    "period, with Status_M2F/Status_QY, LowFishingFlag, and",
    "AboveWestEtAl columns for verifying any individual cell or",
    "applying red fill in Excel/Word where AboveWestEtAl == TRUE).\n")

#=========================================================
# All-species check: Ecopath-implied vs reconstructed seal
# consumption (generalizes the Cod-adult-only check above)
#=========================================================
## WHY THIS EXISTS: M2/F uses Ecopath's own native Predator_M2/Fishing
## output. Q/Y uses Q_species = DietFraction * Q_seal, reconstructed
## independently in this script (Q_seal = QB_seal * qb_multiplier *
## Biomass_seal, from HarpSealTEST.xlsx / HarpSeal_biomass.csv), not
## read from Ecopath's own internal consumption accounting. Since
## Predator_M2 (by definition, Ecopath-side) = [consumption of this
## prey by the seal] / [prey Biomass], multiplying Predator_M2 back by
## prey Biomass recovers Ecopath's OWN implied consumption of that
## prey by the seal -- directly comparable to this script's
## independently reconstructed Q_species for the SAME prey. Any gap
## between the two says this script's Q_seal reconstruction doesn't
## exactly match whatever consumption value actually went into
## Ecopath's own model.
##
## This also happens to be exactly the quantity behind the M2/F vs
## Q/Y divergence noticed across FGs: algebraically, (M2/F) / (Q/Y) =
## (Predator_M2 * Biomass) / Q_species -- i.e. EcopathImpliedConsumption
## / ReconstructedConsumption. If that ratio is close to 1 and,
## crucially, close to the SAME value for every species within a
## period, the M2/F vs Q/Y gap is one shared systematic offset (most
## likely in how Q_seal is reconstructed for the seal, which is common
## to every prey species) rather than something genuinely different
## per species. If it fragments across species within a period, the
## cause is more likely species-specific (diet fraction mismatches,
## N/S regional weighting differences, etc.).

if(exists("results") && exists("qy_results") && exists("biomass_results")){
  
  consumption_check <- merge(
    results[, .(
      Simulation = as.character(Simulation),
      Model      = as.character(Model),
      Group,
      Predator_M2
    )],
    biomass_results[, .(
      Simulation = as.character(Simulation),
      Model      = as.character(Model),
      Group,
      Biomass
    )],
    by = c("Simulation", "Model", "Group"),
    all.x = TRUE
  )
  
  consumption_check <- merge(
    consumption_check,
    qy_results[, .(
      Simulation = as.character(Simulation),
      Model      = as.character(Model),
      Group,
      ReconstructedConsumption = Q_species
    )],
    by = c("Simulation", "Model", "Group"),
    all.x = TRUE
  )
  
  consumption_check[
    ,
    EcopathImpliedConsumption := Predator_M2 * Biomass
  ]
  
  ## The exact quantity that (M2/F)/(Q/Y) reduces to algebraically --
  ## see the comment block above. Also express it as a % difference,
  ## which is easier to eyeball for "is this ~0" per row.
  
  consumption_check[
    ,
    ImpliedOverReconstructed := fifelse(
      ReconstructedConsumption > 0,
      EcopathImpliedConsumption / ReconstructedConsumption,
      NA_real_
    )
  ]
  
  consumption_check[
    ,
    PctDiff := fifelse(
      ReconstructedConsumption > 0,
      100 * (ReconstructedConsumption - EcopathImpliedConsumption) /
        EcopathImpliedConsumption,
      NA_real_
    )
  ]
  
  setorder(consumption_check, Simulation, Model, Group)
  
  fwrite(
    consumption_check,
    file.path(root_dir, "HarpSeal_EcopathImplied_vs_Reconstructed_Consumption_AllSpecies.csv")
  )
  
  cat("\n===== All-species check: Ecopath-implied vs reconstructed seal consumption =====\n")
  print(consumption_check)
  
  ## Per-Simulation summary: mean and SD of ImpliedOverReconstructed
  ## across species. A period where this ratio is a genuine shared
  ## constant (as the 1985-1988 M2/F-vs-Q/Y numbers strongly suggest)
  ## should show a SD close to 0 relative to its mean -- i.e. a low
  ## coefficient of variation. A period where the SD is large relative
  ## to the mean confirms the gap has become species-specific in that
  ## period, not a single shared offset.
  
  consumption_check_summary <- consumption_check[
    is.finite(ImpliedOverReconstructed) & ImpliedOverReconstructed > 0,
    .(
      N_species  = .N,
      Mean_Ratio = mean(ImpliedOverReconstructed, na.rm = TRUE),
      SD_Ratio   = sd(ImpliedOverReconstructed, na.rm = TRUE),
      CV_Ratio   = sd(ImpliedOverReconstructed, na.rm = TRUE) /
        mean(ImpliedOverReconstructed, na.rm = TRUE)
    ),
    by = .(Simulation, Model)
  ][order(Simulation, Model)]
  
  fwrite(
    consumption_check_summary,
    file.path(root_dir, "HarpSeal_EcopathImplied_vs_Reconstructed_Consumption_Summary.csv")
  )
  
  cat("\n===== Per-Simulation/Model summary: how CONSTANT is the Ecopath-implied /",
      "reconstructed-consumption ratio across species? =====\n")
  cat("(Low CV_Ratio = one shared offset across species, as seen in 1985-1988's",
      "M2/F-vs-Q/Y numbers. High CV_Ratio = the gap is species-specific in that",
      "Simulation/Model.)\n")
  print(consumption_check_summary)
  
} else {
  
  cat(
    "\nSkipping all-species Ecopath-implied-vs-reconstructed-consumption check --",
    "results/qy_results/biomass_results not all available.\n"
  )
  
}

#=========================================================
# Why does Q/Y grow more than M2/F from 1985-1988 to
# 2018-2020? Decompose the fold-change, per species.
#=========================================================
## SETUP: M2/F = Predator_M2 / Fishing (both native Ecopath output).
## Q/Y = Q_species / Y_species, where Y_species = Fishing * Biomass
## (this script's own construction -- see the Y_species section
## earlier). So the fold-change (2018-2020 value / 1985-1988 value)
## of each ratio decomposes as:
##
##   M2F_fold = Predator_M2_fold / Fishing_fold
##   QY_fold  = Q_species_fold / Y_species_fold
##            = Q_species_fold / (Fishing_fold * Biomass_fold)
##
## Fishing_fold is the SAME term in both (Y_species is literally
## Fishing * Biomass), so it cancels when comparing the two ratios'
## fold-changes directly:
##
##   QY_fold / M2F_fold = Q_species_fold / (Biomass_fold * Predator_M2_fold)
##
## This is the exact question asked: is Q/Y outgrowing M2/F because
## this script's reconstructed Q_species (driven by seal Q/B *
## seal biomass * diet fraction) grew faster from 1985-1988 to
## 2018-2020 than Ecopath's own native predation-mortality-rate times
## prey biomass did? If GrowthRatio (QY_fold / M2F_fold, computed
## two independent ways below as a cross-check) is consistently > 1
## across species, and Q_species_fold is consistently the largest of
## the three component fold-changes, that confirms the reconstructed
## consumption term is the driver, not Biomass or Predator_M2.
##
## Run this AFTER results/qy_results/biomass_results exist (same
## requirement as the consumption_check block above), and requires at
## least two periods -- edit EARLY_PERIOD/LATE_PERIOD below if your
## Simulation labels differ (e.g. if you want 2018-2020_8020 instead
## of 2018-2020_5050, or 2013-2015 instead of 1985-1988).

if(exists("results") && exists("qy_results") && exists("biomass_results")){
  
  EARLY_PERIOD <- "1985-1988"
  LATE_PERIOD  <- "2018-2020_5050"
  
  growth_components <- merge(
    results[
      Simulation %in% c(EARLY_PERIOD, LATE_PERIOD),
      .(
        Simulation = as.character(Simulation),
        Model      = as.character(Model),
        Group,
        Predator_M2,
        Fishing
      )
    ],
    biomass_results[
      Simulation %in% c(EARLY_PERIOD, LATE_PERIOD),
      .(
        Simulation = as.character(Simulation),
        Model      = as.character(Model),
        Group,
        Biomass
      )
    ],
    by = c("Simulation", "Model", "Group"),
    all = TRUE
  )
  
  growth_components <- merge(
    growth_components,
    qy_results[
      Simulation %in% c(EARLY_PERIOD, LATE_PERIOD),
      .(
        Simulation = as.character(Simulation),
        Model      = as.character(Model),
        Group,
        Q_species,
        Y_species,
        Q_over_Y
      )
    ],
    by = c("Simulation", "Model", "Group"),
    all = TRUE
  )
  
  growth_components[, M2_over_F := fifelse(
    Fishing > 0,
    Predator_M2 / Fishing,
    NA_real_
  )]
  
  ## Reshape early vs. late period side by side, per Group x Model
  
  growth_wide <- dcast(
    growth_components,
    Group + Model ~ Simulation,
    value.var = c(
      "Predator_M2", "Fishing", "Biomass",
      "Q_species", "Y_species",
      "M2_over_F", "Q_over_Y"
    )
  )
  
  early_suffix <- paste0("_", EARLY_PERIOD)
  late_suffix  <- paste0("_", LATE_PERIOD)
  
  fold <- function(var){
    early_col <- paste0(var, early_suffix)
    late_col  <- paste0(var, late_suffix)
    if(!early_col %in% names(growth_wide) || !late_col %in% names(growth_wide))
      return(rep(NA_real_, nrow(growth_wide)))
    fifelse(
      growth_wide[[early_col]] > 0,
      growth_wide[[late_col]] / growth_wide[[early_col]],
      NA_real_
    )
  }
  
  growth_wide[, `:=`(
    Predator_M2_fold = fold("Predator_M2"),
    Fishing_fold      = fold("Fishing"),
    Biomass_fold      = fold("Biomass"),
    Q_species_fold    = fold("Q_species"),
    Y_species_fold    = fold("Y_species"),
    M2_over_F_fold    = fold("M2_over_F"),
    Q_over_Y_fold     = fold("Q_over_Y")
  )]
  
  ## GrowthRatio, computed TWO independent ways as a cross-check --
  ## they should match closely (both are just QY_fold / M2F_fold,
  ## algebra vs. taking the ratio of the two already-computed folds
  ## directly). A big mismatch between them signals a data problem
  ## (e.g. a missing Group in one period) rather than a real effect.
  
  growth_wide[, GrowthRatio_direct := Q_over_Y_fold / M2_over_F_fold]
  growth_wide[, GrowthRatio_decomposed := Q_species_fold / (Biomass_fold * Predator_M2_fold)]
  growth_wide[, GrowthRatio_CrossCheckDiff := GrowthRatio_direct - GrowthRatio_decomposed]
  
  ## Which of the three component fold-changes is largest -- i.e.
  ## which term is most responsible for pulling Q/Y's growth ahead of
  ## M2/F's. Compares Q_species_fold directly against
  ## (Biomass_fold * Predator_M2_fold) as a single combined term,
  ## since that's the quantity Q_species_fold is being weighed against
  ## in GrowthRatio_decomposed above.
  
  growth_wide[, EcopathSide_fold := Biomass_fold * Predator_M2_fold]
  growth_wide[, DominantDriver := fifelse(
    is.na(Q_species_fold) | is.na(EcopathSide_fold), NA_character_,
    fifelse(
      Q_species_fold > EcopathSide_fold,
      "Reconstructed Q_species grew faster",
      "Ecopath-side (Biomass x Predator_M2) grew faster"
    )
  )]
  
  growth_wide <- growth_wide[order(Model, Group)]
  
  fwrite(
    growth_wide,
    file.path(root_dir, "HarpSeal_M2F_vs_QY_GrowthDecomposition.csv")
  )
  
  ## IMPORTANT, READ BEFORE INTERPRETING THE PRINTED TABLE: Biomass is
  ## only ever available for Model == "baseline" -- Basic_estimates.csv
  ## (the file Biomass is read from) only exists in each Simulation's
  ## baseline folder, never in the qb-XX/diet-XX/b-XX variant folders
  ## (documented earlier in this script, around the Y_species /
  ## biomass_baseline sections). So Biomass_fold, GrowthRatio_decomposed,
  ## EcopathSide_fold and DominantDriver are correctly NA for every
  ## non-baseline Model -- that's not a bug, the decomposition (as
  ## opposed to M2_over_F_fold/Q_over_Y_fold/GrowthRatio_direct, which
  ## don't need Biomass and ARE available for every Model) is only
  ## meaningful for baseline. The full CSV keeps every Model for
  ## completeness; the console print below is filtered to baseline so
  ## you're not scrolling past all-NA rows for models it doesn't apply to.
  
  cat("\n===== Why Q/Y grows more than M2/F: per-species fold-change decomposition,",
      EARLY_PERIOD, "->", LATE_PERIOD, "(baseline only -- Biomass isn't available",
      "for the other Models, see comment above) =====\n")
  cat("(GrowthRatio > 1 means Q/Y grew faster than M2/F for that species.",
      "GrowthRatio_direct and GrowthRatio_decomposed should closely agree --",
      "large GrowthRatio_CrossCheckDiff flags a data issue, not a real effect.",
      "DominantDriver says whether the reconstructed Q_species term or the",
      "Ecopath-native Biomass x Predator_M2 term grew faster, per the algebra",
      "in the comment block above. Sorted by GrowthRatio_direct, descending.)\n")
  print(
    growth_wide[
      Model == "baseline"
    ][
      order(-GrowthRatio_direct),
      .(
        Group,
        M2_over_F_fold, Q_over_Y_fold,
        GrowthRatio_direct, GrowthRatio_decomposed, GrowthRatio_CrossCheckDiff,
        Predator_M2_fold, Fishing_fold, Biomass_fold,
        Q_species_fold, Y_species_fold,
        DominantDriver
      )
    ]
  )
  
  cat("\n(Full table for every Model -- including the non-baseline rows where",
      "Biomass/GrowthRatio_decomposed/DominantDriver are legitimately NA -- is",
      "in HarpSeal_M2F_vs_QY_GrowthDecomposition.csv.)\n")
  
  ## Summary across species, restricted to baseline (matches the
  ## table already reviewed) -- how many species see Q_species
  ## growing faster vs. the Ecopath-side term growing faster, and
  ## what's the median GrowthRatio. A lopsided count strongly in one
  ## direction is the same kind of signal as the low-CV finding in
  ## the consumption_check block above: one systematic driver, not
  ## scattered species-specific noise.
  
  growth_summary <- growth_wide[
    Model == "baseline" & is.finite(GrowthRatio_direct),
    .(
      N_species = .N,
      N_QSpecies_dominant = sum(DominantDriver == "Reconstructed Q_species grew faster", na.rm = TRUE),
      N_EcopathSide_dominant = sum(DominantDriver == "Ecopath-side (Biomass x Predator_M2) grew faster", na.rm = TRUE),
      Median_GrowthRatio = median(GrowthRatio_direct, na.rm = TRUE),
      Mean_GrowthRatio = mean(GrowthRatio_direct, na.rm = TRUE)
    )
  ]
  
  cat("\n===== Baseline summary: which term dominates the Q/Y-vs-M2/F growth gap? =====\n")
  print(growth_summary)
  
} else {
  
  cat(
    "\nSkipping M2/F vs Q/Y growth decomposition --",
    "results/qy_results/biomass_results not all available.\n"
  )
  
}

#=========================================================
# Does the North/South area-weighting order-of-operations
# explain the GrowthRatio_CrossCheckDiff seen above?
#=========================================================
## WHY THIS EXISTS: GrowthRatio_direct and GrowthRatio_decomposed
## (from the block above) should be algebraically identical -- Fishing
## cancels out of (M2/F)/(Q/Y) by construction. They weren't, for
## several species. The likely reason: Y_species = Fishing * Biomass
## and Q_species = DietFraction * Q_seal are each computed PER RAW
## regional sub-model (e.g. "2018-2020_N50", "2018-2020_S50") BEFORE
## combine_NS_simulations() area-weight-averages them. Predator_M2 and
## Fishing (the M2/F side), by contrast, are atomic values straight
## from EwE's own output -- nothing is multiplied together before
## they're combined.
##
## Averaging a PRODUCT across two regions is not the same as
## multiplying the two regions' AVERAGES:
##   area_weight_N*(F_N*B_N) + area_weight_S*(F_S*B_S)
##     != (area_weight_N*F_N + area_weight_S*F_S) *
##        (area_weight_N*B_N + area_weight_S*B_S)
## unless F and B barely vary between North and South for that
## species. This block computes both sides directly for Y_species
## (JensenGap_Y) and checks whether species with a bigger North/South
## split in Fishing and Biomass are the same species that showed the
## biggest GrowthRatio_CrossCheckDiff above -- if so, that confirms
## this combination-order effect as a real, additional driver of the
## M2/F-vs-Q/Y divergence, separate from the Q_seal-reconstruction
## question the earlier consumption_check block addressed.
##
## Requires the *_raw objects captured just before each
## combine_NS_simulations() call (results_raw already existed;
## biomass_results_raw and qy_results_raw were added alongside this
## block) and the growth_wide table from the block above (for
## GrowthRatio_CrossCheckDiff to compare against).

if(
  exists("results_raw") && exists("biomass_results_raw") &&
  exists("qy_results_raw") && exists("growth_wide") &&
  exists("combined_simulation_map")
){
  
  ## Which raw N/S pair actually feeds LATE_PERIOD (defined in the
  ## block above) -- read from combined_simulation_map instead of
  ## hardcoding, so this stays correct if LATE_PERIOD is edited to
  ## "2018-2020_8020" instead.
  
  ns_pair <- combined_simulation_map[[LATE_PERIOD]]
  
  if(is.null(ns_pair)){
    
    cat(
      "\nSkipping N/S heterogeneity check -- LATE_PERIOD ('", LATE_PERIOD,
      "') is not a combined Simulation in combined_simulation_map.\n",
      sep = ""
    )
    
  } else {
    
    sim_N <- ns_pair[["N"]]
    sim_S <- ns_pair[["S"]]
    
    ## Raw Fishing/Biomass, baseline model, one row per Group x region
    
    fishing_ns <- dcast(
      results_raw[
        Model == "baseline" & Simulation %in% c(sim_N, sim_S),
        .(Simulation, Group, Fishing)
      ],
      Group ~ Simulation,
      value.var = "Fishing"
    )
    setnames(fishing_ns, c(sim_N, sim_S), c("Fishing_N", "Fishing_S"))
    
    biomass_ns <- dcast(
      biomass_results_raw[
        Model == "baseline" & Simulation %in% c(sim_N, sim_S),
        .(Simulation, Group, Biomass)
      ],
      Group ~ Simulation,
      value.var = "Biomass"
    )
    setnames(biomass_ns, c(sim_N, sim_S), c("Biomass_N", "Biomass_S"))
    
    ns_check <- merge(fishing_ns, biomass_ns, by = "Group", all = TRUE)
    
    ## "Official" combined values -- pulled from the already-combined
    ## results/biomass_results/qy_results tables (LATE_PERIOD row),
    ## rather than recomputed here, so this matches exactly what every
    ## figure and every other diagnostic in this script actually used.
    
    ns_check <- merge(
      ns_check,
      results[
        Model == "baseline" & Simulation == LATE_PERIOD,
        .(Group, Fishing_combined = Fishing, Predator_M2_combined = Predator_M2)
      ],
      by = "Group", all.x = TRUE
    )
    
    ns_check <- merge(
      ns_check,
      biomass_results[
        Model == "baseline" & Simulation == LATE_PERIOD,
        .(Group, Biomass_combined = Biomass)
      ],
      by = "Group", all.x = TRUE
    )
    
    ns_check <- merge(
      ns_check,
      qy_results[
        Model == "baseline" & Simulation == LATE_PERIOD,
        .(Group, Y_species_actual = Y_species, Q_species_actual = Q_species)
      ],
      by = "Group", all.x = TRUE
    )
    
    ## The core test: actual (real, area-weighted-product) Y_species
    ## vs. naive (product of the area-weighted components). JensenGap_Y
    ## == 1 means no distortion from this combination step for that
    ## species; further from 1 means a bigger distortion.
    
    ns_check[, Y_species_naive := Fishing_combined * Biomass_combined]
    ns_check[, JensenGap_Y := fifelse(
      Y_species_naive != 0,
      Y_species_actual / Y_species_naive,
      NA_real_
    )]
    
    ## Heterogeneity metrics: how different is North from South, for
    ## the two components that get multiplied together. abs(log-ratio)
    ## so it's symmetric (N double S, or S double N, score the same)
    ## and 0 means "identical between regions".
    
    ns_check[, Fishing_NS_LogRatio := fifelse(
      Fishing_N > 0 & Fishing_S > 0, abs(log(Fishing_N / Fishing_S)), NA_real_
    )]
    ns_check[, Biomass_NS_LogRatio := fifelse(
      Biomass_N > 0 & Biomass_S > 0, abs(log(Biomass_N / Biomass_S)), NA_real_
    )]
    
    ## Bring in GrowthRatio_CrossCheckDiff from the decomposition
    ## block above, baseline model only, to test directly whether
    ## bigger JensenGap_Y deviations line up with bigger cross-check
    ## mismatches.
    
    ns_check <- merge(
      ns_check,
      growth_wide[
        Model == "baseline",
        .(Group, GrowthRatio_direct, GrowthRatio_decomposed, GrowthRatio_CrossCheckDiff)
      ],
      by = "Group", all.x = TRUE
    )
    
    setorder(ns_check, -Fishing_NS_LogRatio)
    
    fwrite(
      ns_check,
      file.path(root_dir, "HarpSeal_NS_AreaWeighting_JensenGap_Check.csv")
    )
    
    cat("\n===== Does North/South heterogeneity explain the GrowthRatio cross-check",
        "mismatch? (", sim_N, "vs", sim_S, ", baseline) =====\n")
    cat("(JensenGap_Y far from 1 = Y_species's own N/S combination distorts it away",
        "from Fishing_combined x Biomass_combined. Fishing_NS_LogRatio / ",
        "Biomass_NS_LogRatio = how different North is from South for that species",
        "(0 = identical). If species with the largest LogRatios also have the",
        "largest |GrowthRatio_CrossCheckDiff|, that confirms this combination-order",
        "effect as a real driver. Sorted by Fishing_NS_LogRatio, descending.)\n")
    print(ns_check)
    
    ## Direct correlation test, across whatever species have all the
    ## needed values -- a strong positive correlation between region
    ## heterogeneity and the cross-check mismatch is the clearest
    ## possible confirmation.
    
    complete_rows <- ns_check[
      is.finite(Fishing_NS_LogRatio) & is.finite(GrowthRatio_CrossCheckDiff)
    ]
    
    if(nrow(complete_rows) >= 3){
      
      cor_fishing <- cor(
        complete_rows$Fishing_NS_LogRatio,
        abs(complete_rows$GrowthRatio_CrossCheckDiff),
        use = "complete.obs"
      )
      
      cor_biomass <- if(sum(is.finite(complete_rows$Biomass_NS_LogRatio)) >= 3){
        cor(
          complete_rows$Biomass_NS_LogRatio,
          abs(complete_rows$GrowthRatio_CrossCheckDiff),
          use = "complete.obs"
        )
      } else {
        NA_real_
      }
      
      cat("\nCorrelation (n =", nrow(complete_rows), "species) between",
          "Fishing_NS_LogRatio and |GrowthRatio_CrossCheckDiff|:",
          round(cor_fishing, 3), "\n")
      cat("Correlation between Biomass_NS_LogRatio and",
          "|GrowthRatio_CrossCheckDiff|:", round(cor_biomass, 3), "\n")
      cat("(Close to +1 = strong confirmation that N/S heterogeneity drives the",
          "cross-check mismatch. Close to 0 = this isn't the explanation, look",
          "elsewhere.)\n")
      
    } else {
      
      cat("\nToo few species with complete data (need >= 3) to compute a",
          "correlation -- inspect the printed table directly instead.\n")
      
    }
    
  }
  
} else {
  
  cat(
    "\nSkipping N/S heterogeneity check -- results_raw/biomass_results_raw/",
    "qy_results_raw/growth_wide/combined_simulation_map not all available.\n"
  )
  
}

#=========================================================
# Prepare plotting data
#=========================================================

species_order <- results[
  ,
  .(
    MeanRatio = mean(
      Predator_vs_Fishing,
      na.rm = TRUE
    )
  ),
  by = Group
][
  order(-MeanRatio)
]$Group

results[
  ,
  Group := factor(
    Group,
    levels = rev(unique(species_order))
  )
]

# #=========================================================
# # Figure 1
# # Fishing mortality vs harp seal predation, over time
# #=========================================================
# ## FIXED: previously covered ALL functional groups in `results`,
# ## including the harp seal's own group -- since `results` is only
# ## filtered on Fishing > 0 (no diet-relevance filter at all), any
# ## group with recorded fishing mortality showed up here, whether or
# ## not harp seal actually eats it. Harp seal itself had nonzero
# ## Fishing (likely historical/incidental harvest recorded in the
# ## model), so it appeared as its own panel, which doesn't make sense
# ## for a "fishing vs. harp seal predation" comparison. Now filtered to
# ## `seal_prey_species` -- every functional group with a nonzero diet
# ## fraction in harp seal's own diet composition matrix (data-derived,
# ## not a hardcoded guess) -- so only genuine prey items are shown.
# 
# plot_dat <- melt(
#   results[Group %in% seal_prey_species],
#   id.vars = c("Simulation", "Model", "Group"),
#   measure.vars = c("Fishing", "Predator_M2"),
#   variable.name = "Mortality",
#   value.name = "Rate"
# )
# 
# plot_dat[, Mortality := factor(
#   Mortality,
#   levels = c("Fishing", "Predator_M2"),
#   labels = c("Fishing mortality", "Harp seal predation")
# )]
# 
# plot_dat[, Model := factor(Model, levels = model_order_baseline_first)]
# plot_dat <- plot_dat[!is.na(Model)]
# 
# d1f1_period_data <- build_period_timeseries(
#   plot_dat,
#   value_col = "Rate",
#   extra_group_cols = c("Group", "Mortality")
# )
# d1f1_period_data$raw_points[, PeriodNum := as.numeric(Period)]
# d1f1_period_data$raw_points[
#   , PeriodNum := PeriodNum + ifelse(Simulation == "2018-2020_5050", -0.12, 0.12)
# ]
# 
# n_groups_d1f1 <- length(unique(plot_dat$Group))
# 
# p1 <- ggplot(
#   d1f1_period_data$mean_line,
#   aes(x = Period, y = Value, colour = Model, linetype = Mortality, group = interaction(Model, Mortality))
# ) +
#   geom_line(linewidth = 0.7) +
#   geom_point(size = 1.8) +
#   geom_point(
#     data = d1f1_period_data$raw_points,
#     aes(x = PeriodNum, y = Value, colour = Model),
#     shape = 1, size = 1.4, stroke = 0.7, alpha = 0.6,
#     inherit.aes = FALSE
#   ) +
#   facet_wrap(~Group, scales = "free_y", ncol = 5) +
#   scale_colour_manual(values = sensitivity_model_colors) +
#   theme_bw(base_size = 11) +
#   theme(
#     axis.text.x = element_text(angle = 30, hjust = 1),
#     legend.position = "bottom"
#   ) +
#   labs(
#     x = NULL,
#     y = expression(Mortality~(year^{-1})),
#     colour = "Model",
#     linetype = NULL
#   )
# 
# print(p1)
# 
# # ## Caption (for the report -- not rendered in the plot itself, since
# # ## this becomes a document caption rather than an in-plot title/subtitle)
# # p1_caption <- paste(
# #   "D1 - Fig 1. Fishing mortality vs. harp seal predation mortality",
# #   "(Predator_M2) over time, all seal prey species (nonzero diet",
# #   "fraction in harp seal's own diet), one panel per species. Line",
# #   "colour = sensitivity Model, line type = mortality source. 2018-2020",
# #   "shows the mean of the 50/50 and 80/20 scenarios (hollow points =",
# #   "individual scenario values)."
# # )
# # cat("\n", p1_caption, "\n", sep = "")
# # 
# # ggsave(
# #   file.path(root_dir, "D1_Fig1_MortalityBySpecies.png"),
# #   p1,
# #   width = 18,
# #   height = max(10, 2 * ceiling(n_groups_d1f1 / 5)),
# #   dpi = 300
# # )

# #=========================================================
# # Figure 2
# # Harp seal predation relative to fishing, over time
# #=========================================================
# ## FIXED: same issue and same fix as Figure 1 above -- filtered to
# ## `seal_prey_species` instead of every functional group in `results`,
# ## which was incorrectly including harp seal's own group (it has
# ## nonzero Fishing, so nothing previously excluded it) and any other
# ## non-prey group with recorded fishing mortality.
# 
# results_d1f2 <- copy(results[Group %in% seal_prey_species])
# results_d1f2[, Model := factor(Model, levels = model_order_baseline_first)]
# results_d1f2 <- results_d1f2[!is.na(Model)]
# 
# d1f2_period_data <- build_period_timeseries(
#   results_d1f2,
#   value_col = "Predator_vs_Fishing",
#   extra_group_cols = "Group"
# )
# d1f2_period_data$raw_points[, PeriodNum := as.numeric(Period)]
# d1f2_period_data$raw_points[
#   , PeriodNum := PeriodNum + ifelse(Simulation == "2018-2020_5050", -0.12, 0.12)
# ]
# 
# n_groups_d1f2 <- length(unique(results_d1f2$Group))
# 
# p2 <- ggplot(
#   d1f2_period_data$mean_line,
#   aes(x = Period, y = Value, colour = Model, group = Model)
# ) +
#   geom_hline(yintercept = 1, linetype = 2, colour = "grey40") +
#   geom_line(linewidth = 0.7) +
#   geom_point(size = 1.8) +
#   geom_point(
#     data = d1f2_period_data$raw_points,
#     aes(x = PeriodNum, y = Value, colour = Model),
#     shape = 1, size = 1.4, stroke = 0.7, alpha = 0.6,
#     inherit.aes = FALSE
#   ) +
#   facet_wrap(~Group, scales = "free_y", ncol = 5) +
#   scale_colour_manual(values = sensitivity_model_colors) +
#   theme_bw(base_size = 11) +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   labs(
#     x = NULL,
#     y = "Predation mortality / Fishing mortality",
#     colour = "Model"
#   )
# 
# print(p2)
# 
# ## Caption (for the report -- not rendered in the plot itself)
# p2_caption <- paste(
#   "D1 - Fig 2. M2/F ratio (harp seal predation / fishing mortality)",
#   "over time, all seal prey species (nonzero diet fraction in harp",
#   "seal's own diet), one panel per species, coloured by sensitivity",
#   "Model. Dashed grey line = ratio of 1 (equal impact). 2018-2020",
#   "shows the mean of the 50/50 and 80/20 scenarios (hollow points =",
#   "individual scenario values)."
# )
# cat("\n", p2_caption, "\n", sep = "")
# 
# ggsave(
#   file.path(root_dir, "D1_Fig2_M2FRatioBySpecies.png"),
#   p2,
#   width = 18,
#   height = max(10, 2 * ceiling(n_groups_d1f2 / 5)),
#   dpi = 300
# )

#=========================================================
# Summary table
#=========================================================
## FIXED: same issue as Figures 1/2 -- filtered to `seal_prey_species`
## instead of every functional group in `results`, which incorrectly
## included harp seal's own group (nonzero Fishing, nothing previously
## excluded it) and any other non-prey group with fishing mortality.

summary_table <- results[
  Group %in% seal_prey_species,
  .(
    Fishing = round(Fishing, 4),
    Predator_M2 = round(Predator_M2, 4),
    Ratio = round(Predator_vs_Fishing, 2),
    Fraction_of_Total_Predation =
      round(Predator_fraction_M2, 2)
  ),
  by = .(
    Simulation,
    Model,
    Group
  )
]

print(summary_table)

fwrite(
  summary_table,
  file.path(
    root_dir,
    "Summary_HarpSeal_Predation_vs_Fishing.csv"
  )
)

# #=========================================================
# # Figure 3
# # Heatmap
# #=========================================================
# ## FIXED: same filter fix as above -- `heat` starts from
# ## `seal_prey_species` now, not every group in `results`. This does
# ## NOT change Figure D1-4 downstream (`heat_small <- heat[Group %in%
# ## exploited_species]`), since exploited_species is already a subset
# ## of seal_prey_species (seal prey AND fished) -- adding this filter
# ## upstream is redundant-but-harmless for that figure, and fixes
# ## Figure 3 itself, which previously showed `heat` unfiltered.
# 
# heat <- copy(results[Group %in% seal_prey_species])
# 
# heat <- heat[
#   is.finite(Predator_vs_Fishing)
# ]
# 
# species_order <- heat[
#   ,
#   .(
#     MeanRatio = mean(
#       Predator_vs_Fishing,
#       na.rm = TRUE
#     )
#   ),
#   by = Group
# ][order(-MeanRatio)]$Group
# 
# heat[, Group := factor(
#   Group,
#   levels = rev(species_order)
# )]
# 
# heat[, RatioClass := cut(
#   Predator_vs_Fishing,
#   breaks = c(0, 1, 2, 5, 10, Inf),
#   labels = c(
#     "0–1",
#     "1–2",
#     "2–5",
#     "5–10",
#     ">10"
#   ),
#   include.lowest = TRUE
# )]
# 
# p3 <- ggplot(
#   heat,
#   aes(
#     Simulation,
#     Group,
#     fill = RatioClass
#   )
# ) +
#   geom_tile(colour = "grey90") +
#   geom_text(
#     aes(
#       label = sprintf(
#         "%.1f",
#         Predator_vs_Fishing
#       )
#     ),
#     size = 3
#   ) +
#   facet_wrap(
#     ~Model,
#     ncol = 3
#   ) +
#   scale_fill_manual(
#     values = c(
#       "0–1"  = "#f7fbff",
#       "1–2"  = "#c6dbef",
#       "2–5"  = "#6baed6",
#       "5–10" = "#2171b5",
#       ">10"  = "#08306b"
#     ),
#     drop = FALSE,
#     name = "Predation /\nFishing"
#   ) +
#   theme_bw(base_size = 12) +
#   theme(
#     panel.grid = element_blank(),
#     axis.text.x = element_text(
#       angle = 45,
#       hjust = 1
#     )
#   ) +
#   labs(
#     x = NULL,
#     y = NULL
#   )
# 
# print(p3)
# 
# ## Caption (for the report -- not rendered in the plot itself)
# p3_caption <- paste(
#   "D1 - Fig 3. M2/F ratio heatmap, all seal prey species (nonzero diet",
#   "fraction in harp seal's own diet), binned by magnitude, faceted by",
#   "Model (sensitivity variant)."
# )
# cat("\n", p3_caption, "\n", sep = "")
# 
# ggsave(
#   file.path(root_dir, "D1_Fig3_M2FHeatmapAllSpecies.png"),
#   p3,
#   width = 14,
#   height = 10,
#   dpi = 300
# )

# #=========================================================
# # Figure D1-4: M2/F over time, faceted by species (data-driven)
# #=========================================================
# ## CONVERTED from a heatmap to a time-series: with three real time
# ## periods in the data, a species x model GRID makes it hard to read
# ## any actual trend -- a small-multiples line plot (one panel per
# ## species, x = Period, coloured by sensitivity Model) shows the same
# ## information as a trend instead of a lookup table. Same species
# ## selection as before: `exploited_species` (predated by harp seal AND
# ## fished, both conditions data-derived).
# 
# heat_small <- heat[Group %in% exploited_species]
# 
# check_all_simulations(heat_small$Simulation, "D1 Fig 4 (M2/F time series, exploited species)")
# 
# heat_small[, Model := factor(Model, levels = model_order_baseline_first)]
# heat_small <- heat_small[!is.na(Model)]  # drop Models not in the baseline-first set (e.g. b-XX/PB-XX)
# 
# d1f4_period_data <- build_period_timeseries(
#   heat_small,
#   value_col = "Predator_vs_Fishing",
#   extra_group_cols = "Group"
# )
# d1f4_period_data$raw_points[, PeriodNum := as.numeric(Period)]
# d1f4_period_data$raw_points[
#   , PeriodNum := PeriodNum + ifelse(Simulation == "2018-2020_5050", -0.12, 0.12)
# ]
# 
# d1f4_ref_line <- 17
# 
# ## Height/width scale with the number of species being faceted, same
# ## reasoning as the old heatmap's dynamic sizing.
# 
# fig4_height <- max(6, 1.3 * ceiling(length(exploited_species) / 4))
# 
# p4 <- ggplot(
#   d1f4_period_data$mean_line,
#   aes(x = Period, y = Value, colour = Model, group = Model)
# ) +
#   geom_hline(
#     yintercept = d1f4_ref_line,
#     linetype = "dashed",
#     colour = "#FFB000",
#     linewidth = 0.7
#   ) +
#   geom_line(linewidth = 0.7) +
#   geom_point(size = 2) +
#   geom_point(
#     data = d1f4_period_data$raw_points,
#     aes(x = PeriodNum, y = Value, colour = Model),
#     shape = 1, size = 1.6, stroke = 0.8, alpha = 0.7,
#     inherit.aes = FALSE
#   ) +
#   facet_wrap(~Group, scales = "free_y", ncol = 4) +
#   scale_colour_manual(values = sensitivity_model_colors) +
#   theme_bw(base_size = 12) +
#   theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#   labs(
#     x = NULL,
#     y = "M2 / F",
#     colour = "Model"
#   )
# 
# print(p4)
# 
# ## Caption (for the report -- not rendered in the plot itself)
# p4_caption <- paste(
#   "D1 - Fig 4. M2/F ratio over time, all exploited seal prey species",
#   "(predated by harp seal AND fished), one panel per species, coloured",
#   "by sensitivity Model (baseline first, then Q/B, then diet variants).",
#   "The 2018-2020 point/line uses the mean of the 50/50 and 80/20",
#   "population-split scenarios; hollow points show the individual",
#   "scenario values. Dashed gold line = West et al. (2025)'s reported",
#   "M2/F \u2248 17x headline."
# )
# cat("\n", p4_caption, "\n", sep = "")
# 
# ggsave(
#   file.path(
#     root_dir,
#     "D1_Fig4_M2FTimeSeriesExploited.png"
#     
#   ),
#   p4,
#   width = 15,
#   height = fig4_height,
#   dpi = 300
# )
###### NCAM

load("/Users/daniel/Desktop/HarpSeals/NCAM/ncam_2019.RData")   # object is named ncam_2019

fit <- ncam_2019          # (this is what the dashboard code calls the object internally)

ages  <- fit$tmb.data$ages
years <- fit$tmb.data$years

F_matrix   <- fit$rep$F_matrix      # fishing mortality at age  (age x year)
#N_matrix   <- fit$rep$N_matrix * 1000   # abundance at age, in numbers (stored in thousands)
#ssb_matrix <- fit$rep$ssb_matrix    # SSB at age
Z_matrix   <- t(fit$rep$Z_matrix)      # total mortality at age
rownames(Z_matrix) <- fit$tmb.data$years
colnames(Z_matrix) <- fit$tmb.data$ages
M_matrix   <- t(fit$rep$Mpe_matrix)    # natural mortality at age
rownames(M_matrix) <- fit$tmb.data$years
colnames(M_matrix) <- fit$tmb.data$ages
B_at_age<-t(fit$rep$biomass_matrix)
rownames(B_at_age) <- c(1983:2019)
colnames(B_at_age) <- c(fit$tmb.data$ages)
F_at_age <- t(fit$rep$F_matrix)
rownames(F_at_age) <- fit$tmb.data$years
colnames(F_at_age) <- fit$tmb.data$ages
head(B_at_age)
head(F_at_age)

# #=========================================================
# # Biomass-weighted F for ages 4+
# #=========================================================
# 
# # Match years
# yrs <- intersect(rownames(B_at_age), rownames(F_at_age))
# 
# B <- B_at_age[yrs, ]
# F <- F_at_age[yrs, ]
# 
# # Biomass proportions at all ages
# B_prop <- B / rowSums(B)
# 
# # Select ages 4+
# ages <- as.character(4:14)
# 
# # Renormalize proportions within selected ages
# weights <- B_prop[, ages]
# weights <- weights / rowSums(weights)
# 
# # Biomass-weighted fishing mortality
# F_4plus <- rowSums(weights * F[, ages])
# 
# # Output
# F_4plus <- data.frame(
#   Year = as.numeric(rownames(B)),
#   F_4plus = F_4plus
# )
# 
# head(F_4plus)

#=========================================================
# Biomass-weighted mortality rates for ages 4+
#=========================================================

# Match years
yrs <- Reduce(intersect, list(
  rownames(B_at_age),
  rownames(F_at_age),
  rownames(Z_matrix),
  rownames(M_matrix)
))

B <- B_at_age[yrs, ]
F <- F_at_age[yrs, ]
Z <- Z_matrix[yrs, ]
M <- M_matrix[yrs, ]

# Biomass proportions at all ages
B_prop <- B / rowSums(B)

# Ages to average
ages <- as.character(4:14)

# Renormalize biomass proportions within selected ages
weights <- B_prop[, ages]
weights <- weights / rowSums(weights)

# Biomass-weighted mortality rates
F_4plus <- rowSums(weights * F[, ages])
Z_4plus <- rowSums(weights * Z[, ages])
M_4plus <- rowSums(weights * M[, ages])

# Combine into a single data frame
mortality_summary <- data.frame(
  Year = as.numeric(yrs),
  F = F_4plus,
  M = M_4plus,
  Z = Z_4plus
)

head(mortality_summary)

library(data.table)

dt <- as.data.table(mortality_summary)

## Period definitions
## NOTE (label mismatch made explicit rather than silently resolved):
##   - EwE Simulation folders are literally: "1985_1988", "2013-2015",
##     "2018-2020_N50", "2018-2020_S50" on disk, but after the N/S
##     area-weighted combination step earlier in this script,
##     `results$Simulation` only ever contains: "1985-1988",
##     "2013-2015", "2018-2020_8020", "2018-2020_5050".
##   - The requested comparison periods were "1985-1986", "2013-2015",
##     "2018-2020" -- but the first EwE run actually spans 1985-1988,
##     not just 1985-1986. To make the NCAM side line up with what the
##     EwE run actually covers, NCAM years 1985:1988 are pooled into the
##     "1985-1988" period below (not just 1985:1986). If you specifically
##     need NCAM restricted to 1985-1986, change the range here.

dt[, Period := fifelse(
  Year %in% 1985:1988, "1985-1988",
  fifelse(Year %in% 2013:2015, "2013-2015",
          fifelse(Year %in% 2018:2020, "2018-2020", NA_character_))
)]

period_means <- dt[!is.na(Period),
                   lapply(.SD, mean),
                   by = Period,
                   .SDcols = c("F", "M", "Z")]

period_means

## Explicit check: warn (don't fail silently) if NCAM has no years
## falling in one of the three expected periods
missing_periods <- setdiff(
  c("1985-1988", "2013-2015", "2018-2020"),
  period_means$Period
)

if(length(missing_periods) > 0){
  
  warning(
    "No NCAM years found for period(s): ",
    paste(missing_periods, collapse = ", "),
    " -- check the `yrs` range used above."
  )
  
}

#=========================================================
# Compare EwE (1985-1988, 2013-2015, 2018-2020) vs NCAM F / M / Z
#=========================================================

## Mean EwE estimate across Model subfolders, Cod adult, per period
## NOTE: M here uses Total_M2 (all predation mortality), as the
## closest analogue to NCAM's total natural mortality M.
## If you'd rather compare against harp-seal-only mortality,
## swap Total_M2 for Predator_M2 below.

## Map each EwE Simulation factor level onto one of the three comparison
## periods. UPDATED: the two 2018-2020 sub-REGIONS (N50/S50, N80/S20)
## were already combined into single area-weighted composites earlier
## in this script (2018-2020_5050, 2018-2020_8020) -- what gets pooled
## here is different: those two combined Simulations represent two
## different population-split SCENARIOS for the same 2018-2020 period,
## not sub-regions, so pooling them via simple mean below is comparing
## scenarios, not re-doing the area weighting.
##
## `sim_to_period`/`period_levels` are now defined once near the top of
## this script (Settings section), not redefined here -- several
## earlier figures (D1 Fig 1/2/4, D3 Fig 1) need them well before this
## point, which is exactly the same execution-order issue already found
## and fixed for `model_levels_all`.

## Fail loudly (not silently) if a Simulation level shows up that isn't
## mapped to a period -- this is exactly the class of bug that caused
## the original filter to silently return zero rows.

unmapped <- setdiff(levels(results$Simulation), names(sim_to_period))

if(length(unmapped) > 0){
  
  stop(
    "Unmapped Simulation level(s): ",
    paste(unmapped, collapse = ", "),
    " -- update sim_to_period to include them."
  )
  
}

results[, Period := factor(
  sim_to_period[as.character(Simulation)],
  levels = period_levels
)]

## UPDATED: grouped by Simulation (4 levels: 1985-1988, 2013-2015,
## 2018-2020_8020, 2018-2020_5050), NOT by Period (3 levels) --
## grouping by Period would silently pool the two 2018-2020 scenario
## Simulations (8020/5050) into a single averaged point, hiding one of
## the four models from every downstream figure. Still averages across
## Model sensitivity variants (baseline, qb-XX, diet-XX) WITHIN each
## Simulation, same as before -- that averaging is intentional (one
## representative point + implicit sensitivity spread per model), only
## the Simulation-vs-Period pooling was the bug.

## FIXED: M here now uses Total_M2 + OtherMortality (ALL non-fishing
## mortality in EwE's own accounting, equivalent to Z - F), not
## Total_M2 alone. This matters: NCAM's own "M" is a single-species
## stock assessment model's UNDECOMPOSED natural-mortality residual --
## it has no internal mechanism separating predation from disease,
## starvation, senescence, or any other non-fishing cause. Comparing
## it against only EwE's predation component (Total_M2) would compare
## NCAM's FULL non-fishing mortality against just PART of EwE's --
## understating the EwE-side total. Total_M2 + OtherMortality is the
## complete, like-for-like non-fishing mortality on the EwE side.
## Predator_M2 (harp-seal-specific predation) is kept as a separate
## column for reference -- it is NOT directly comparable to NCAM's M,
## since NCAM has no seal-specific mortality estimate to compare it to
## (see the deliverable-4 discussion on this point).

ewe_by_period <- results[
  Group == "Cod adult",
  .(
    F = mean(Fishing, na.rm = TRUE),
    M = mean(Total_M2 + OtherMortality, na.rm = TRUE),
    M_PredationOnly = mean(Total_M2, na.rm = TRUE),
    Predation_SealOnly = mean(Predator_M2, na.rm = TRUE)
  ),
  by = Simulation
]

## Explicit check: we expect exactly one row per Simulation (4 total,
## matching simulation_levels). If not, something upstream didn't
## match ("Cod adult" missing from a Simulation, or an N/S combination
## failed) -- surface it instead of silently plotting fewer points
## than expected.

if(nrow(ewe_by_period) != 4){
  
  warning(
    "Expected 4 EwE Simulation estimates for Cod adult (",
    paste(simulation_levels, collapse = ", "), "), got ",
    nrow(ewe_by_period),
    ". Check that 'Cod adult' exists in every Simulation folder and ",
    "that the N/S area-weighted combination succeeded for both ",
    "2018-2020 scenarios (2018-2020_8020 needs N80+S20 data, which",
    " may still be missing -- see seal_qb_diet_lookup notes)."
  )
  
  print(ewe_by_period)
  
}

ewe_by_period[, Z := F + M]
ewe_by_period[, Period := sim_to_period[as.character(Simulation)]]

ewe_by_period

#=========================================================
# EwE biomass (Cod adult, Harp seal) by period
#=========================================================
## Biomass comes from each Simulation's "baseline" Basic-estimates file
## (t/km^2), read into `biomass_results` earlier (already area-weighted
## combined for the 2018-2020 sub-regions). The two 2018-2020 scenario
## Simulations (8020/5050) are pooled (mean) into a single "2018-2020"
## biomass estimate per group here, same pooling logic used for the
## F/M/Z comparison above.

biomass_results[, Period := factor(
  sim_to_period[as.character(Simulation)],
  levels = c("1985-1988", "2013-2015", "2018-2020")
)]

## UPDATED: grouped by Simulation (4 levels), not Period (3 levels) --
## same fix as ewe_by_period above, so 2018-2020_8020 and
## 2018-2020_5050 appear as two distinct biomass estimates rather than
## being pooled into one averaged "2018-2020" value.

biomass_by_period <- biomass_results[
  Model == "baseline" &
    Group %in% c("Cod adult", "Harp seal"),
  .(Biomass = mean(Biomass, na.rm = TRUE)),
  by = .(Group, Simulation)
]

## Explicit check: expect 2 groups x 4 Simulations = 8 rows

if(nrow(biomass_by_period) != 8){
  
  warning(
    "Expected 8 EwE biomass estimates (2 groups x 4 Simulations), got ",
    nrow(biomass_by_period),
    ". Check that 'Cod adult' / 'Harp seal' exist in every Simulation's ",
    "baseline Basic-estimates file (2018-2020_8020 needs both N80 and",
    " S20 baseline files to combine successfully)."
  )
  
  print(biomass_by_period)
  
}

biomass_by_period[, Period := sim_to_period[as.character(Simulation)]]

biomass_by_period

## Long format for the F/M/Z trajectory overlay
## UPDATED: id.vars includes Simulation (not just Period) so
## 2018-2020_8020 and 2018-2020_5050 stay distinguishable as separate
## points/colours in p5, even though they share the same Year/Period
## on the x-axis.

ewe_points <- melt(
  ewe_by_period[, .(Simulation = as.character(Simulation), Period, F, M, Z)],
  id.vars = c("Simulation", "Period"),
  measure.vars = c("F", "M", "Z"),
  variable.name = "Metric",
  value.name = "Value"
)

## Representative Year for each period, for plotting on the NCAM year
## axis (period midpoint)

period_year <- c(
  "1985-1988" = 1986.5,
  "2013-2015" = 2014,
  "2018-2020" = 2019
)

## Simulation-level year, with the two 2018-2020 scenarios (8020/5050)
## nudged slightly apart on the x-axis so they don't plot as a single
## overlapping point in p5 -- they're still both within the 2018-2020
## window, just offset by +/-0.3 years for visibility.

simulation_year <- c(
  "1985-1988"      = 1986.5,
  "2013-2015"      = 2014,
  "2018-2020_8020" = 2018.7,
  "2018-2020_5050" = 2019.3
)

ewe_points[, Year := simulation_year[as.character(Simulation)]]

## Add Cod-adult Biomass as a 4th metric alongside F/M/Z so it appears
## as its own facet panel in p5

ewe_biomass_points <- biomass_by_period[
  Group == "Cod adult",
  .(Simulation = as.character(Simulation), Period, Metric = "Biomass", Value = Biomass)
]
ewe_biomass_points[, Year := simulation_year[as.character(Simulation)]]

ewe_points <- rbind(ewe_points, ewe_biomass_points)

## NCAM trajectory in long format

ncam_traj <- melt(
  dt[, .(Year, F, M, Z)],
  id.vars = "Year",
  variable.name = "Metric",
  value.name = "Value"
)

## NCAM biomass restricted to ages 4+ (same age range already used for
## the F/M/Z 4+ weighting above: `ages <- as.character(4:14)`, `B <-
## B_at_age[yrs, ]`), summed across those ages per year, then converted
## to t/km^2 using the confirmed domain area (495,000 km^2) so it's on
## the same footing as EwE's Biomass (t/km^2). This replaces using
## `fit$rep$biomass` (which is total biomass across ALL ages, not just
## 4+, and so isn't the right comparator for a 4+ EwE age structure).

area_km2 <- 495000

B_4plus <- rowSums(B[, ages])

ncam_biomass_dt <- data.table(
  Year    = as.numeric(yrs),
  Biomass = B_4plus / area_km2
)

ncam_biomass_long <- ncam_biomass_dt[
  , .(Year, Metric = "Biomass", Value = Biomass)
]

ncam_traj <- rbind(ncam_traj, ncam_biomass_long)

## Shared y-axis range for the F, M, Z panels specifically (Biomass
## keeps its own free scale, since it's on a completely different
## unit/magnitude). facet_wrap(scales="free_y") normally gives every
## panel its own independent range; forcing F/M/Z to share one makes
## their magnitudes directly comparable at a glance (e.g. "is M
## actually much bigger than F in this period"), which free scales
## hide. Achieved by adding invisible geom_blank() anchor points that
## extend each of the three panels to the same shared min/max, without
## drawing anything visible themselves.

fmz_values <- c(
  ncam_traj[Metric %in% c("F", "M", "Z")]$Value,
  ewe_points[Metric %in% c("F", "M", "Z")]$Value
)
fmz_range <- range(fmz_values, na.rm = TRUE)

fmz_anchor <- data.table(
  Metric = rep(c("F", "M", "Z"), times = 2),
  Year   = mean(ncam_traj$Year, na.rm = TRUE),
  Value  = rep(fmz_range, each = 3)
)

## F/M composition of Z over time, as a stacked area UNDER the Z line
## (Z panel only, via Metric == "Z" restricting where this data
## appears) -- Z = F + M by definition, so this shows how the split
## between fishing and natural mortality shifts year to year, not just
## the total.

z_composition <- melt(
  dt[!is.na(Z) & !is.na(F) & !is.na(M), .(Year, F, M)],
  id.vars = "Year",
  variable.name = "Component",
  value.name = "Value"
)
z_composition[, Component := factor(
  Component,
  levels = c("F", "M"),
  labels = c("Fishing (F)", "Natural (M)")
)]
z_composition[, Metric := "Z"]

#=========================================================
# Figure 1: NCAM vs EwE
# Cod adult
#=========================================================


#---------------------------------------------------------
# Facet labels
#---------------------------------------------------------

## FIXED: the old (variable, value) two-argument labeller function
## signature is deprecated in current ggplot2 (this is the warning
## about "labellers taking `variable` and `value` arguments"). Modern
## equivalent: a named vector of plotmath-syntax strings (single quotes
## for literal text, ^ for superscript, * to concatenate) passed
## through as_labeller() with default = label_parsed, which parses and
## renders them identically to the original bquote() expressions.

metric_labels_str <- c(
  F       = "'Fishing mortality (F, year'^-1*')'",
  M       = "'Natural mortality (M, year'^-1*')'",
  Z       = "'Total mortality (Z, year'^-1*')'",
  Biomass = "'Biomass (t/km'^2*')'"
)

metric_labeller <- as_labeller(metric_labels_str, default = label_parsed)


#=========================================================
# Plot
#=========================================================

p5 <- ggplot(
  ncam_traj,
  aes(
    x = Year,
    y = Value
  )
) +
  
  #-------------------------------------------------------
# Z composition
#-------------------------------------------------------

geom_area(
  data = z_composition,
  aes(
    x = Year,
    y = Value,
    fill = Component
  ),
  position = "stack",
  alpha = 0.45,
  inherit.aes = FALSE
) +
  
  scale_fill_manual(
    values = c(
      "Fishing (F)" = "#d73027",
      "Natural (M)" = "#4575b4"
    ),
    name = NULL
  ) +
  
  #-------------------------------------------------------
# NCAM trajectory
#-------------------------------------------------------

geom_line(
  aes(
    linetype = "NCAM estimates"
  ),
  colour = "grey40",
  linewidth = 1
) +
  
  #-------------------------------------------------------
# EwE estimates
#-------------------------------------------------------

geom_point(
  data = ewe_points,
  aes(
    x = Year,
    y = Value,
    shape = "EwE estimates"
  ),
  colour = "black",
  size = 4
) +
  
  #-------------------------------------------------------
# NCAM legend
#-------------------------------------------------------

scale_linetype_manual(
  values = c(
    "NCAM estimates" = "solid"
  ),
  name = NULL
) +
  
  #-------------------------------------------------------
# EwE legend
#-------------------------------------------------------

scale_shape_manual(
  values = c(
    "EwE estimates" = 8
  ),
  name = NULL
) +
  
  #-------------------------------------------------------
# Keep required panel ranges
#-------------------------------------------------------

geom_blank(
  data = fmz_anchor,
  aes(
    x = Year,
    y = Value
  )
) +
  
  #-------------------------------------------------------
# Facets
#-------------------------------------------------------

facet_wrap(
  ~ Metric,
  scales = "free_y",
  labeller = metric_labeller
) +
  
  #-------------------------------------------------------
# No axis titles
#-------------------------------------------------------

labs(
  x = NULL,
  y = NULL
) +
  
  #-------------------------------------------------------
# Common theme
#-------------------------------------------------------

common_theme +
  
  theme(
    
    #-----------------------------------------------------
    # One horizontal row for all legends
    #-----------------------------------------------------
    
    legend.position = "bottom",
    
    legend.box = "horizontal",
    
    legend.direction = "horizontal",
    
    legend.justification = "left",
    
    legend.box.just = "left",
    
    legend.spacing.x = unit(
      0.4,
      "cm"
    ),
    
    legend.margin = margin(
      0,
      0,
      0,
      0
    )
  )


#---------------------------------------------------------
# Print
#---------------------------------------------------------

print(p5)


#=========================================================
# Caption
#=========================================================

p5_caption <- paste(
  "D4 - Fig 1. Cod adult: independent NCAM stock-assessment",
  "trajectory (F, M, Z, Biomass) compared with EwE estimates",
  "from the four simulation periods. NCAM estimates are shown",
  "as grey lines and EwE estimates as black asterisks.",
  "F, M and Z are expressed in year^-1, whereas Biomass is",
  "expressed in t/km^2. F, M and Z share the same y-axis scale",
  "for direct comparison, whereas Biomass uses its own scale.",
  "The Z panel shows the fishing (F) and natural (M) components",
  "of total mortality as stacked shaded areas. EwE natural",
  "mortality is defined as Total_M2 + OtherMortality, representing",
  "all non-fishing mortality and providing the closest like-for-like",
  "comparison with NCAM's undecomposed M."
)

cat(
  "\n",
  p5_caption,
  "\n",
  sep = ""
)


#=========================================================
# Save
#=========================================================

ggsave(
  file.path(
    root_dir,
    "D4_Fig1_NCAM_vs_EwE_CodAdult.png"
  ),
  p5,
  width = 8,
  height = 8,
  dpi = 300
)

#=========================================================
# Estimates table (companion to D4 Fig 1)
#=========================================================
## The plotted values themselves, alongside the figure -- EwE side
## (ewe_points: one row per Simulation x Metric) and NCAM side
## (ncam_traj: one row per Year x Metric) as two separate files, since
## they're on different x-axes (Simulation-period vs. continuous Year)
## and shouldn't be forced into one table.

fwrite(
  ewe_points,
  file.path(root_dir, "D4_Fig1_EwE_Estimates.csv")
)

fwrite(
  ncam_traj,
  file.path(root_dir, "D4_Fig1_NCAM_Estimates.csv")
)

# #=========================================================
# # EwE-only: Fishing mortality vs harp seal predation, by Simulation
# # (Cod adult)
# #=========================================================
# ## UPDATED: faceted by Simulation (4 levels), not Period (3 levels) --
# ## same fix as ewe_by_period/biomass_by_period above.
# 
# fp_compare <- melt(
#   ewe_by_period[, .(Simulation, Fishing = F, `Harp seal predation` = Predation_SealOnly)],
#   id.vars = "Simulation",
#   measure.vars = c("Fishing", "Harp seal predation"),
#   variable.name = "Mortality",
#   value.name = "Rate"
# )
# 
# p6 <- ggplot(
#   fp_compare,
#   aes(
#     x = Mortality,
#     y = Rate,
#     fill = Mortality
#   )
# ) +
#   geom_col(width = 0.6) +
#   facet_wrap(~ Simulation, ncol = 4) +
#   theme_bw(base_size = 13) +
#   theme(legend.position = "none") +
#   labs(
#     x = NULL,
#     y = expression(Mortality~(year^{-1}))
#   )
# 
# print(p6)
# 
# ## Caption (for the report -- not rendered in the plot itself)
# p6_caption <- paste(
#   "D4 - Fig 2. Cod adult: EwE-only comparison of fishing mortality vs.",
#   "harp seal predation mortality, across all four Simulations."
# )
# cat("\n", p6_caption, "\n", sep = "")
# 
# ggsave(
#   file.path(
#     root_dir,
#     "D4_Fig2_FishingVsSealPredation_CodAdult.png"
#   ),
#   p6,
#   width = 9,
#   height = 5,
#   dpi = 300
# )

# #=========================================================
# # EwE biomass (Cod adult, Harp seal) by Simulation -- standalone bar chart
# #=========================================================
# ## `biomass_by_period` was already computed above (right before
# ## ewe_points/ncam_traj), where it's also used to add the Cod-adult
# ## Biomass panel into p5. This is just the by-group, by-Simulation bar
# ## chart view of the same numbers. UPDATED: x-axis/fill is now
# ## Simulation (4 levels), not Period (3 levels), same fix as p5/p6.
# 
# p7 <- ggplot(
#   biomass_by_period,
#   aes(
#     x = Simulation,
#     y = Biomass,
#     fill = Simulation
#   )
# ) +
#   geom_col(width = 0.6) +
#   facet_wrap(~ Group, scales = "free_y") +
#   scale_fill_manual(
#     values = simulation_colors
#   ) +
#   theme_bw(base_size = 13) +
#   theme(
#     legend.position = "none",
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   ) +
#   labs(
#     x = NULL,
#     y = expression(Biomass~(t/km^2))
#   )
# 
# print(p7)
# 
# ## Caption (for the report -- not rendered in the plot itself)
# p7_caption <- "D4 - Fig 3. EwE baseline biomass (Cod adult, harp seal), all four Simulations."
# cat("\n", p7_caption, "\n", sep = "")
# 
# ggsave(
#   file.path(root_dir, "D4_Fig3_BaselineBiomass_ByPeriod.png"),
#   p7,
#   width = 8,
#   height = 5,
#   dpi = 300
# )

## Harp seal has no independent NCAM trajectory to compare against
## (ncam_2019 appears to be a single-species/cod assessment -- its
## `biomass`, `F_matrix`, `Z_matrix`, `Mpe_matrix` are all indexed by
## fish age classes, not by seal age/population structure). Unless you
## have a separate independent harp seal abundance/biomass series to
## overlay, the p7 bar chart above (EwE-only, by period) is the harp
## seal comparison.

# #=========================================================
# # Figure D1-5: Q/Y over time, faceted by species -- analogue of Figure D1-4
# #=========================================================
# ## CONVERTED from a heatmap to a time-series, same reasoning as
# ## Figure D1-4: three real time periods read better as a trend than a
# ## species x model grid. Mirrors Figure D1-4's construction exactly,
# ## for the Q/Y ratio instead of M2/F.
# 
# if(exists("ratio_compare") && nrow(ratio_compare) > 0){
#   
#   qy_heat <- copy(ratio_compare)
#   qy_heat <- qy_heat[is.finite(Q_over_Y)]
#   qy_heat <- qy_heat[Group %in% exploited_species]
#   
#   check_all_simulations(qy_heat$Simulation, "D1 Fig 5 (Q/Y time series, exploited species)")
#   
#   qy_heat[, Model := factor(Model, levels = model_order_baseline_first)]
#   qy_heat <- qy_heat[!is.na(Model)]  # drop Models not in the baseline-first set
#   
#   d1f5_period_data <- build_period_timeseries(
#     qy_heat,
#     value_col = "Q_over_Y",
#     extra_group_cols = "Group"
#   )
#   d1f5_period_data$raw_points[, PeriodNum := as.numeric(Period)]
#   d1f5_period_data$raw_points[
#     , PeriodNum := PeriodNum + ifelse(Simulation == "2018-2020_5050", -0.12, 0.12)
#   ]
#   
#   d1f5_ref_line <- 24
#   
#   fig5_height <- max(6, 1.3 * ceiling(length(exploited_species) / 4))
#   
#   p8 <- ggplot(
#     d1f5_period_data$mean_line,
#     aes(x = Period, y = Value, colour = Model, group = Model)
#   ) +
#     geom_hline(
#       yintercept = d1f5_ref_line,
#       linetype = "dashed",
#       colour = "#FFB000",
#       linewidth = 0.7
#     ) +
#     geom_line(linewidth = 0.7) +
#     geom_point(size = 2) +
#     geom_point(
#       data = d1f5_period_data$raw_points,
#       aes(x = PeriodNum, y = Value, colour = Model),
#       shape = 1, size = 1.6, stroke = 0.8, alpha = 0.7,
#       inherit.aes = FALSE
#     ) +
#     facet_wrap(~Group, scales = "free_y", ncol = 4) +
#     scale_colour_manual(values = sensitivity_model_colors) +
#     theme_bw(base_size = 12) +
#     theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#     labs(
#       x = NULL,
#       y = "Q / Y",
#       colour = "Model"
#     )
#   
#   print(p8)
#   
#   ## Caption (for the report -- not rendered in the plot itself)
#   p8_caption <- paste(
#     "D1 - Fig 5. Q/Y ratio over time, all exploited seal prey species,",
#     "one panel per species, coloured by sensitivity Model (baseline",
#     "first, then Q/B, then diet variants). The 2018-2020 point/line",
#     "uses the mean of the 50/50 and 80/20 population-split scenarios;",
#     "hollow points show the individual scenario values. Dashed gold",
#     "line = West et al. (2025)'s reported Q/Y \u2248 24x headline."
#   )
#   cat("\n", p8_caption, "\n", sep = "")
#   
#   ggsave(
#     file.path(root_dir, "D1_Fig5_QYTimeSeriesExploited.png"),
#     p8,
#     width = 15,
#     height = fig5_height,
#     dpi = 300
#   )
#   
# } else {
#   
#   cat(
#     "\nSkipping Figure 8 (Q/Y time series) -- `ratio_compare` isn't",
#     "available, which means the Q/Y block above didn't find the",
#     "expected Diet composition / Catch files. Fix those file-pattern",
#     "assumptions first.\n"
#   )
#   
# }


# #=========================================================
# # Figure 9: M2/F vs Q/Y agreement check (verifies Ratio_discrepancy)
# #=========================================================
# ## This is the plot that actually LETS YOU SEE the "discrepancy flag"
# ## described in the status memo, rather than just trusting a number in
# ## a csv. Logic:
# ##   - If M2/F and Q/Y agreed perfectly, every point would fall exactly
# ##     on the dashed 1:1 line (log-log scale, since ratios like these
# ##     are naturally multiplicative -- a point twice as far above the
# ##     line as another represents twice the disagreement, not an
# ##     arbitrary distance).
# ##   - Points ABOVE the line: Q/Y says harp seal impact is larger,
# ##     relative to fishing, than M2/F says.
# ##   - Points BELOW the line: M2/F says the impact is larger.
# ##   - Point size/label = Ratio_discrepancy (the same log-ratio-of-
# ##     ratios column already saved in
# ##     HarpSeal_M2F_vs_QY_Ratios_AllModels.csv) -- so the plot and the
# ##     csv are showing exactly the same underlying number, just one
# ##     visually and one as text. You can cross-check any single point
# ##     against that csv directly by Simulation/Model/Group.
# ##
# ## NOTE: until the Diet composition file is provided, `ratio_compare`
# ## only has BOTH ratios for Cod adult (see Figure 8 section) -- every
# ## other group will show M2/F but NA for Q/Y, and therefore won't
# ## appear on this plot at all (it can only plot rows where both ratios
# ## exist). That's not a bug in this plot; it's the same open item
# ## already flagged in the status memo. Once more species have Q/Y, they
# ## show up here automatically -- no code change needed.
# 
# if(exists("ratio_compare") && nrow(ratio_compare) > 0){
#   
#   agreement_data <- ratio_compare[
#     is.finite(M2_over_F) & is.finite(Q_over_Y) & M2_over_F > 0 & Q_over_Y > 0
#   ]
#   
#   if(nrow(agreement_data) > 0){
#     check_all_simulations(agreement_data$Simulation, "D4 Fig 4 (M2/F vs Q/Y agreement)")
#   }
#   
#   if(nrow(agreement_data) == 0){
#     
#     cat(
#       "\nSkipping Figure 9 (M2/F vs Q/Y agreement) -- no rows currently",
#       "have both ratios populated. This will fill in once Q/Y is",
#       "computed for more species (needs the Diet composition file).\n"
#     )
#     
#   } else {
#     
#     axis_limits <- range(
#       c(agreement_data$M2_over_F, agreement_data$Q_over_Y),
#       na.rm = TRUE
#     )
#     
#     p9 <- ggplot(
#       agreement_data,
#       aes(x = M2_over_F, y = Q_over_Y)
#     ) +
#       geom_abline(
#         slope = 1,
#         intercept = 0,
#         linetype = "dashed",
#         colour = "grey50"
#       ) +
#       ## Published benchmark from the SOW itself: West et al. (2025)
#       ## reported M2/F ~= 17x and Q/Y ~= 24x as their headline figures.
#       ## Plotting it as a fixed reference point lets you see directly
#       ## whether the pipeline's own recomputed values, at the species/
#       ## model/period level, sit near, above, or below what the paper
#       ## claims -- which is literally what SOW deliverable 1 asks
#       ## ("whether the ratios accurately reflect... using data reported
#       ## in West et al. (2025)"). This is a single point because the
#       ## paper reports it as a single headline number, not broken out
#       ## by species/period/model the way this pipeline's own results
#       ## are -- so treat closeness to this point as a rough check, not
#       ## a like-for-like comparison at the same level of granularity.
#       annotate(
#         "point",
#         x = 17,
#         y = 24,
#         shape = 8,
#         size = 5,
#         colour = "black"
#       ) +
#       annotate(
#         "text",
#         x = 17,
#         y = 24,
#         label = "West et al. (2025)\nheadline: 17x / 24x",
#         vjust = -1,
#         hjust = 0.5,
#         size = 3.2,
#         fontface = "italic"
#       ) +
#       geom_point(
#         aes(colour = Simulation, size = Ratio_discrepancy),
#         alpha = 0.8
#       ) +
#       { if(requireNamespace("ggrepel", quietly = TRUE)){
#         ggrepel::geom_text_repel(
#           aes(label = paste(Group, Model, sep = " / ")),
#           size = 3,
#           max.overlaps = 20
#         )
#       } else {
#         geom_text(
#           aes(label = paste(Group, Model, sep = " / ")),
#           size = 3,
#           vjust = -1,
#           check_overlap = TRUE
#         )
#       }
#       } +
#       scale_x_log10() +
#       scale_y_log10() +
#       scale_colour_manual(values = simulation_colors, name = "Simulation") +
#       scale_size_continuous(
#         name = "Discrepancy\n(|log ratio-of-ratios|)",
#         range = c(2, 8)
#       ) +
#       coord_fixed() +
#       theme_bw(base_size = 13) +
#       labs(
#         x = "M2 / F (predation mortality / fishing mortality)",
#         y = "Q / Y (harp seal consumption / fishery yield)"
#       )
#     
#     print(p9)
#     
#       ## Caption (for the report -- not rendered in the plot itself)
#       p9_caption <- paste(
#         "D4 - Fig 4. Agreement check between M2/F and Q/Y: points on the",
#         "dashed 1:1 line mean the two ratios agree; off the line means",
#         "they disagree. Point size = discrepancy magnitude. Star =",
#         "West et al. (2025)'s published headline (17x / 24x)."
#       )
#       cat("\n", p9_caption, "\n", sep = "")
#       
#       ggsave(
#         file.path(root_dir, "D4_Fig4_M2F_vs_QY_Agreement.png"),
#         p9,
#         width = 9,
#         height = 8,
#         dpi = 300
#       )
#     
#     cat(
#       "\nFigure D4-4 saved. Cross-check any point against",
#       "HarpSeal_M2F_vs_QY_Ratios_AllModels.csv by matching its",
#       "Simulation/Model/Group label to the Ratio_discrepancy column.\n"
#     )
#     
#   }
#   
# } else {
#   
#   cat(
#     "\nSkipping Figure 9 -- `ratio_compare` isn't available (Q/Y block",
#     "above didn't run successfully).\n"
#   )
#   
# }

########## ADDED DV
#=========================================================
# Figure 1
# Baseline model only
#
# M2/F = Predation mortality / Fishing mortality
# Q/Y  = Consumption / Fisheries yield
#=========================================================
metric_cols <- c(
  "M2 / F" = "#762A83",
  "Q / Y"  = "#B8860B"
)
#=========================================================
# Prepare data
#=========================================================

results_m2fqy <- copy(
  ratio_compare[
    Model == "baseline" &
      Group %in% seal_prey_species
  ]
)

#---------------------------------------------------------
# Create period variable
#---------------------------------------------------------

results_m2fqy[
  ,
  Period := fifelse(
    grepl("^2018-2020", Simulation),
    "2018-2020",
    Simulation
  )
]

results_m2fqy[
  ,
  Period := factor(
    Period,
    levels = c(
      "1985-1988",
      "2013-2015",
      "2018-2020"
    )
  )
]

## FIXED (per explicit request): `Category == "Exploited seal prey
## species"` only means Fishing > 0 SOMEWHERE and nonzero diet
## SOMEWHERE (not necessarily both true in the SAME Simulation/Model)
## -- switched to `species_with_valid_ratio` (defined earlier,
## data-driven from `results`), which requires at least one
## Simulation/Model row where Fishing > 0 AND Predator_M2 > 0
## SIMULTANEOUSLY, i.e. a real, finite, nonzero M2/F ratio somewhere.
## This is specifically for RATIO plots like this one -- Table 2 and
## Figure 10 deliberately keep the broader picture instead, since they
## exist to show the zero/absent cases explicitly rather than hide them.
results_m2fqy_exploited <- results_m2fqy[
  Group %in% species_with_valid_ratio
]

#=========================================================
# Long format
#=========================================================

m2fqy_long <- melt(
  results_m2fqy_exploited,
  id.vars = c(
    "Simulation",
    "Model",
    "Group",
    "Category",
    "Ratio_discrepancy",
    "Period"
  ),
  measure.vars = c(
    "M2_over_F",
    "Q_over_Y"
  ),
  variable.name = "Metric",
  value.name = "Value"
)

m2fqy_long[
  Metric == "M2_over_F",
  Metric := "M2 / F"
]

m2fqy_long[
  Metric == "Q_over_Y",
  Metric := "Q / Y"
]

m2fqy_long[
  ,
  Metric := factor(
    Metric,
    levels = c(
      "M2 / F",
      "Q / Y"
    )
  )
]

#=========================================================
# Mean baseline values
#=========================================================

m2fqy_mean <- m2fqy_long[
  ,
  .(
    Value = mean(Value, na.rm = TRUE)
  ),
  by = .(
    Period,
    Group,
    Metric
  )
]

m2fqy_mean[
  ,
  PeriodNum := as.numeric(Period)
]

m2fqy_long[
  ,
  PeriodNum := as.numeric(Period)
]

## Uncertainty band (replaces the old hollow-point layer, per review
## feedback: the stacked, semi-transparent circles at 2018-2020 were
## easy to misread; a shaded band reads more clearly as "range").
## m2fqy_long has one row per baseline Simulation per period -- a
## single row at 1985-1988 and 2013-2015, and two rows (the 50/50 and
## 80/20 population-split composites) at 2018-2020 -- so the band is
## only non-degenerate at 2018-2020 and collapses onto the trend line
## elsewhere, which is an accurate reflection of where the underlying
## spread actually comes from.

m2fqy_range <- m2fqy_long[
  ,
  .(
    ymin = min(Value, na.rm = TRUE),
    ymax = max(Value, na.rm = TRUE)
  ),
  by = .(Group, Metric, PeriodNum)
]

#=========================================================
# Published estimates
#=========================================================

reported <- data.table(
  PeriodNum = c(
    1, 1,
    3, 3
  ),
  
  Ratio = c(
    1.3,
    1.7,
    17,
    24
  ),
  
  Metric = c(
    "M2 / F",
    "Q / Y",
    "M2 / F",
    "Q / Y"
  ),
  
  Published = c(
    "M2/F = 1.3 (1985-1988)",
    "Q/Y = 1.7 (1985-1988)",
    "M2/F = 17 (2018-2020)",
    "Q/Y = 24 (2018-2020)"
  )
)

reported[
  ,
  Published := factor(
    Published,
    levels = c(
      "M2/F = 1.3 (1985-1988)",
      "Q/Y = 1.7 (1985-1988)",
      "M2/F = 17 (2018-2020)",
      "Q/Y = 24 (2018-2020)"
    )
  )
]

#=========================================================
# Plot
#=========================================================
#=========================================================
# Figure 1
#=========================================================

pfig1 <- ggplot() +
  
  #-------------------------------------------------------
# 1. Uncertainty band -- range across baseline simulations per
# period (only non-degenerate at 2018-2020; see m2fqy_range above)
#-------------------------------------------------------

geom_ribbon(
  data = m2fqy_range,
  aes(
    x = PeriodNum,
    ymin = ymin,
    ymax = ymax,
    fill = Metric,
    group = Metric
  ),
  colour = NA,
  alpha = 0.22
) +
  
  #-------------------------------------------------------
# 2. Model trend lines
#-------------------------------------------------------

geom_line(
  data = m2fqy_mean,
  aes(
    x = PeriodNum,
    y = Value,
    colour = Metric,
    group = Metric
  ),
  linewidth = 0.7
) +
  
  #-------------------------------------------------------
# 3. Mean baseline estimates
#-------------------------------------------------------

geom_point(
  data = m2fqy_mean,
  aes(
    x = PeriodNum,
    y = Value,
    fill = Metric
  ),
  shape = 21,
  colour = "black",
  size = 2.6,
  stroke = 0.4
) +
  
  #-------------------------------------------------------
# 4. Published estimates
# FIXED: moved from first layer to last -- drawn first (behind
# model lines and points) meant the asterisk markers could be
# covered wherever a trend line or the mean point sat at the same
# spot. Drawing them last puts the published-value asterisks on
# top of everything else, so they stay visible.
#-------------------------------------------------------

geom_point(
  data = reported,
  aes(
    x = PeriodNum,
    y = Ratio,
    colour = Metric,
    shape = Published
  ),
  size = 2,
  stroke = 1
) +
  
  #-------------------------------------------------------
# Colour
#-------------------------------------------------------

scale_colour_manual(
  values = metric_cols,
  name = "Baseline EwE models"
) +
  
  #-------------------------------------------------------
# Fill
#-------------------------------------------------------

scale_fill_manual(
  values = metric_cols,
  name = NULL,
  guide = "none"
) +
  
  #-------------------------------------------------------
# Published asterisks
#-------------------------------------------------------

scale_shape_manual(
  values = c(
    "M2/F = 1.3 (1985-1988)" = 8,
    "Q/Y = 1.7 (1985-1988)"  = 8,
    "M2/F = 17 (2018-2020)"   = 8,
    "Q/Y = 24 (2018-2020)"    = 8
  ),
  name = "Published in West et al. (2025)"
) +
  
  #-------------------------------------------------------
# Facets
#-------------------------------------------------------

facet_wrap(
  ~Group,
  scales = "free_y",
  ncol = 5
) +
  
  #-------------------------------------------------------
# X axis
#-------------------------------------------------------

scale_x_continuous(
  breaks = 1:3,
  labels = c(
    "1985-1988",
    "2013-2015",
    "2018-2020"
  ),
  limits = c(
    0.75,
    3.25
  )
) +
  
  #-------------------------------------------------------
# Legends
#-------------------------------------------------------

guides(
  
  # Baseline EwE models
  colour = guide_legend(
    order = 1,
    override.aes = list(
      shape = 21,
      fill = c(
        metric_cols["M2 / F"],
        metric_cols["Q / Y"]
      ),
      size = 2.6,
      linewidth = 0.7
    )
  ),
  
  # Published estimates
  shape = guide_legend(
    order = 2,
    nrow = 2,
    byrow = TRUE,
    override.aes = list(
      shape = 8,
      colour = c(
        metric_cols["M2 / F"],
        metric_cols["Q / Y"],
        metric_cols["M2 / F"],
        metric_cols["Q / Y"]
      ),
      size = 2.8
    )
  )
) +
  
  #-------------------------------------------------------
# Theme
#-------------------------------------------------------

theme_bw(base_size = 11) +
  
  theme(
    #aspect.ratio = 1,
    
    # Remove facet title background
    strip.background = element_blank(),
    
    # Left-align facet titles and adjust vertical position
    strip.text = element_text(
      hjust = 0,
      vjust = 0.01,
      face = "plain"
    ),
    
    # X-axis labels
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    
    # No Y-axis title
    axis.title.y = element_blank(),
    
    # Legends at bottom
    legend.position = "bottom",
    
    # Keep legend titles visible
    legend.title = element_text(
      face = "plain"
    ),
    
    # Two legend rows
    legend.box = "vertical",
    
    # Left-justify both legend rows
    legend.justification = "left",
    legend.box.just = "left",
    
    # Minimal space between legend rows
    legend.spacing.y = unit(
      0,
      "cm"
    ),
    
    legend.margin = margin(
      0,
      0,
      0,
      0
    ),
    
    legend.key.width = unit(
      1.1,
      "cm"
    )
  ) +
  
  labs(
    x = NULL,
    y = NULL
  )

pfig1

## Caption (for the report -- not rendered in the plot itself)

pfig1_caption <- paste(
  "D1 - Fig 1. Temporal comparison of harp seal predation mortality",
  "relative to fishing mortality (M2/F) and harp seal consumption",
  "relative to fisheries yield (Q/Y) for exploited seal prey species",
  "under the baseline model. Filled circles show the period mean and",
  "lines show the temporal trend. The shaded band around each line",
  "shows the range across baseline simulations for that period; for",
  "2018-2020 this reflects the 50/50 and 80/20 population-split",
  "composites, both carried forward as separate baseline simulations,",
  "whereas 1985-1988 and 2013-2015 each have only one baseline",
  "simulation, so the band collapses onto the line there. Asterisks",
  "show the published estimates reported by West et al. (2025):",
  "M2/F = 1.3 and Q/Y = 1.7 for 1985-1988, and M2/F = 17 and Q/Y = 24",
  "for 2018-2020."
)

cat("\n", pfig1_caption, "\n", sep = "")

ggsave(
  file.path(
    root_dir,
    "D1_Fig1_M2F_QY_baseline.png"
  ),
  pfig1,
  width = 13,
  height = 9,
  dpi = 300
)

#=========================================================
# Estimates table (companion to D1 Fig 1)
#=========================================================
## Per-model estimates (m2fqy_long: individual points behind the mean
## line) and the plotted period means (m2fqy_mean) as two files -- the
## mean is what's most directly readable off the figure, but the
## individual-model spread behind it is worth keeping too.

fwrite(
  m2fqy_long,
  file.path(root_dir, "D1_Fig1_Estimates_PerModel.csv")
)

fwrite(
  m2fqy_mean,
  file.path(root_dir, "D1_Fig1_Estimates_PeriodMeans.csv")
)

#=========================================================
# Figure 10: F, harp seal predation, and other mortality as a
# proportion of TOTAL REALIZED MORTALITY (not Z)
#=========================================================
## Answers a different question than M2/F or Q/Y: not "how does seal
## predation compare to fishing" in isolation, but "how much of this
## species' mortality is fishing vs. harp seal predation vs. everything
## else". Useful context for deliverable 4 -- a high M2/F ratio matters
## a lot less if both F and seal predation are tiny slivers of total
## mortality, and a lot more if together they explain most of it.
##
## FIXED (previous version used Z, "Prod/biom or Z", as the
## denominator -- wrong). Ecopath's own additive breakdown is:
##   Z = F + M2(total) + M0(other mortality) + BiomassAccum + NetMigration
## BiomassAccum and NetMigration are NOT mortality -- they're stock
## growth/movement terms, and can be positive OR negative. Using Z as
## the denominator meant:
##   - Species with strong POSITIVE biomass accumulation (population
##     growing) showed an inflated grey "other/residual" slice, because
##     that growth term was being lumped in and mislabeled as "other
##     mortality" -- this is the "too much other mortality" you saw.
##   - Species with strong NEGATIVE biomass accumulation could push
##     F/Z + M2/Z above 1, and the old pmax(...,0) clipping threw away
##     that negative remainder instead of showing it -- this is why
##     Greenland halibut's bar didn't reach 100%.
## Both symptoms had the same root cause: Z is a mass-balance quantity
## that includes non-mortality terms, and a "what causes this species'
## mortality" plot shouldn't use it as the 100% baseline.
##
## FIX: denominator is now F + Total_M2 + OtherMortality only (the
## three genuine mortality sources), with NO biomass accumulation or
## migration term included at all. These three fractions ALWAYS sum to
## exactly 1 by construction -- no pmax clipping needed, and every bar
## reaches exactly 100%.
##
## FIXED: this panel used to silently exclude any species with zero
## fishing mortality, because `results` itself had already dropped
## every Fishing == 0 row upstream, inside compare_predator_vs_fishing()
## -- appropriate for the M2/F ratio (undefined without fishing), but
## WRONG here: a species with real predation mortality and zero
## fishing (e.g. American Plaice in some 2018-2020 sub-models) should
## still appear on this chart, just with its Fishing segment correctly
## at 0%, not be missing from it entirely. Now that
## compare_predator_vs_fishing() keeps every row (Predator_vs_Fishing
## is NA there instead of the whole row vanishing), those species flow
## through to here as long as MortalitySum > 0 -- i.e. as long as they
## have SOME nonzero mortality from fishing, predation, or M0, which
## for a real functional group with nonzero Total_M2 they will. Only a
## group with F = M2 = M0 = 0 everywhere (no mortality of any kind
## recorded) is excluded by the `MortalitySum > 0` filter below, which
## is correct -- there's nothing to show a mortality composition of.
##
## Defaults to baseline only, across all four periods, for readability.
## Change `results[Model == "baseline"]` below to compare sensitivity
## variants for a specific period instead.

z_prop_data <- results[Model == "baseline"]
z_prop_data[, MortalitySum := Fishing + Total_M2 + OtherMortality]
z_prop_data <- z_prop_data[MortalitySum > 0]

z_prop_data[, `:=`(
  Frac_Fishing         = Fishing / MortalitySum,
  Frac_SealPredation   = Predator_M2 / MortalitySum,
  Frac_OtherPredation  = pmax(Total_M2 - Predator_M2, 0) / MortalitySum,
  Frac_OtherMortality  = OtherMortality / MortalitySum
)]

## Sanity check: these four should sum to ~1.00 for every row now, by
## construction (not by clipping) -- UNLESS Predator_M2 > Total_M2 for
## that row, in which case Frac_OtherPredation gets clipped to 0
## instead of going negative, and the sum EXCEEDS 1 (not falls short --
## an earlier version of this comment had the direction backwards).
## That's a real (if usually small) inconsistency between EwE's two
## separate output files (Mortalities.csv vs.
## Predation_mortality_rates.csv), already flagged in detail by
## check_predator_vs_total_m2() earlier in this script -- run that
## against `results` for the exact Group/Simulation/Model rows
## responsible for any excess reported here. On the PLOT itself, that
## excess used to make the topmost (Fishing) segment vanish entirely
## rather than visually exceed 100%, because ggplot's default oob
## handling on a fixed-limit scale drops out-of-range values instead
## of clipping them -- fixed via `oob = scales::squish` on Figure 10's
## y-axis below, so the bar now correctly renders at 100% with every
## segment still visible. A sum BELOW 1, by contrast, is not explained
## by this mechanism and would need separate investigation (e.g. an
## NA slipping through one of the four Frac_ columns).

z_prop_check <- z_prop_data[
  ,
  .(
    Simulation, Model, Group,
    total = Frac_Fishing + Frac_SealPredation + Frac_OtherPredation + Frac_OtherMortality
  )
]

z_prop_bad <- z_prop_check[abs(total - 1) > 0.01]

if(nrow(z_prop_bad) > 0){
  
  warning(
    "Figure 10: mortality fractions don't sum to ~1 for ", nrow(z_prop_bad),
    " row(s). Values ABOVE 1 are almost certainly the Predator_M2 > ",
    "Total_M2 issue (see check_predator_vs_total_m2() output above) -- ",
    "the plot itself clips this via oob = scales::squish, so it will ",
    "still render correctly at 100%. Values BELOW 1 are NOT explained ",
    "by that mechanism and need separate investigation."
  )
  print(z_prop_bad[order(total)])
  
}

z_prop_long <- melt(
  z_prop_data,
  id.vars = c("Simulation", "Model", "Group"),
  measure.vars = c(
    "Frac_Fishing", "Frac_SealPredation",
    "Frac_OtherPredation", "Frac_OtherMortality"
  ),
  variable.name = "Source",
  value.name = "Fraction"
)

z_prop_long[, Source := factor(
  Source,
  levels = c(
    "Frac_Fishing", "Frac_SealPredation",
    "Frac_OtherPredation", "Frac_OtherMortality"
  ),
  labels = c(
    "Fishing", "Harp seal predation",
    "Other predation", "Other mortality (M0)"
  )
)]

## REVISED again (per follow-up clarification): briefly switched to
## `seal_prey_species` (ANY nonzero harp seal diet fraction, no
## fishing requirement at all) so a species like Haddock or American
## Plaice -- genuinely fished in SOME periods/regions, just not the
## specific one being drawn -- wouldn't be excluded just because that
## one row has Fishing == 0. But `seal_prey_species` turned out too
## broad: it also pulled in species like Arctic cod, which has
## Fishing == 0 (or unrecorded) in EVERY SINGLE Simulation/Model, not
## just some -- confirmed directly via diagnose_group_gap("Arctic
## cod"), which shows ZERO_FISHING across all 66 rows. A species never
## fished ANYWHERE contributes nothing to a chart whose whole point is
## comparing fishing pressure against predation pressure, so it
## shouldn't be here at all -- unlike Haddock/American Plaice, which
## DO have real fishing pressure somewhere and legitimately earn a
## 0%-fishing bar in the periods where they don't.
##
## Settled on `exploited_species` (fished somewhere AND preyed upon
## somewhere -- each condition independently, NOT required
## simultaneously the way `species_with_valid_ratio` requires for the
## ratio plots, which would be too strict here and would also exclude
## legitimate 0%-fishing bars). This correctly: keeps Haddock/
## American Plaice/Yellowtail flounder (real fishing pressure exists
## somewhere), and drops Arctic cod (never fished, anywhere, in any
## model).
##
## A given Group/Simulation cell still renders BLANK exactly when that
## Group genuinely doesn't exist in that Simulation's model structure
## (dropped upstream because it wasn't a modeled group in one of the
## two regional N/S sub-models, or wasn't modeled in the raw 1985-1988
## run) -- `Simulation` is a factor with all 4 levels always present
## on the facet's x-axis, so a missing row for one Simulation leaves a
## gap at that x-position within that Group's panel, while the
## Group's OTHER Simulations (where it IS present) still plot
## normally. Zero fishing mortality in one SPECIFIC period, for a
## species that IS fished somewhere else, is no longer a reason for a
## blank cell (see compare_predator_vs_fishing()'s earlier fix) --
## those render as a real bar with a 0%-height Fishing segment.

z_prop_long <- z_prop_long[Group %in% exploited_species]

if(nrow(z_prop_long) == 0){
  
  cat(
    "\nSkipping Figure 10 -- no matching groups with F+M2+M0 > 0 after",
    "filtering. Check that `results` was built successfully before",
    "this point.\n"
  )
  
} else {
  
  #=========================================================
  # Mortality composition through time
  # Baseline models
  #=========================================================
  
  
  #---------------------------------------------------------
  # Remove completely empty row
  #---------------------------------------------------------
  
  z_prop_long <- z_prop_long[
    !is.na(Simulation) &
      !is.na(Model) &
      !is.na(Group) &
      !is.na(Source)
  ]
  
  
  #---------------------------------------------------------
  # Check for remaining empty rows
  #---------------------------------------------------------
  
  z_prop_long[
    is.na(Simulation) |
      is.na(Model) |
      is.na(Group) |
      is.na(Source)
  ]
  
  
  #---------------------------------------------------------
  # Check available simulations / periods
  #---------------------------------------------------------
  
  unique(z_prop_long$Simulation)
  
  
  #---------------------------------------------------------
  # Order periods chronologically
  #---------------------------------------------------------
  ## FIXED: "2000-2002" isn't a real Simulation anywhere in this
  ## project (the four are 1985-1988, 2013-2015, 2018-2020_8020,
  ## 2018-2020_5050) -- removed. Also reordered to match
  ## `simulation_levels`, defined once near the top of this script,
  ## rather than a separately hand-typed list that could drift from it.
  
  z_prop_long[
    ,
    Simulation := factor(
      Simulation,
      levels = simulation_levels
    )
  ]
  
  
  #---------------------------------------------------------
  # Order mortality components
  #---------------------------------------------------------
  
  z_prop_long[
    ,
    Source := factor(
      Source,
      levels = c(
        "Fishing",
        "Harp seal predation",
        "Other predation",
        "Other mortality (M0)"
      )
    )
  ]
  
  
  #---------------------------------------------------------
  # Keep groups in their existing order
  #---------------------------------------------------------
  
  z_prop_long[
    ,
    Group := factor(
      Group,
      levels = unique(Group)
    )
  ]
  
  
  ## common_theme now defined once near the top of this script
  ## (Settings section) -- removed the duplicate definition that used
  ## to be here (identical content, just redundant).
  
  
  #=========================================================
  # Plot
  #=========================================================
  
  p10 <- ggplot(
    z_prop_long,
    aes(
      x = Simulation,
      y = Fraction,
      fill = Source
    )
  ) +
    
    #-------------------------------------------------------
  # Stacked mortality components
  #-------------------------------------------------------
  
  geom_col(
    position = "stack",
    colour = "white",
    linewidth = 0.2
  ) +
    
    #-------------------------------------------------------
  # Five columns
  #-------------------------------------------------------
  
  facet_wrap(
    ~ Group,
    ncol = 5
  ) +
    
    #-------------------------------------------------------
  # Y axis
  #-------------------------------------------------------
  ## FIXED: ggplot's default `oob` (out-of-bounds) handling for a
  ## fixed-limit scale is `scales::censor`, which converts ANY value
  ## outside the limits to NA -- for a stacked bar, that doesn't clip
  ## the overflow, it silently DROPS the entire segment that pushes
  ## the stack past the limit (the topmost one, Fishing, since it's
  ## plotted last). That's exactly why witch flounder/snow crab's
  ## 2018-2020_8020 bars were missing their red slice and falling
  ## short of 100%, rather than being visually clipped at 100% --
  ## their fractions genuinely sum to slightly over 1 (see the
  ## Predator_M2 > Total_M2 diagnostic elsewhere in this script for
  ## why). `oob = scales::squish` clips out-of-range values to the
  ## nearest limit instead of discarding them, so a bar that should
  ## slightly exceed 100% now correctly renders AT 100%, with every
  ## segment still visible, rather than one vanishing.
  
  scale_y_continuous(
    labels = scales::percent_format(
      accuracy = 1
    ),
    limits = c(0, 1),
    oob = scales::squish,
    expand = expansion(
      mult = c(0, 0.02)
    )
  ) +
    
    #-------------------------------------------------------
  # Mortality colours
  #-------------------------------------------------------
  
  scale_fill_manual(
    values = c(
      "Fishing"              = "#d73027",
      "Harp seal predation"  = "#4575b4",
      "Other predation"      = "#91bfdb",
      "Other mortality (M0)" = "grey80"
    ),
    name = NULL
  ) +
    
    #-------------------------------------------------------
  # Labels
  #-------------------------------------------------------
  
  labs(
    x = NULL,
    y = "Proportion of total realized mortality\n(F + M2 + M0)"
  ) +
    
    #-------------------------------------------------------
  # Apply common theme
  #-------------------------------------------------------
  
  common_theme +
    
    #-------------------------------------------------------
  # Larger bottom legend -- per explicit request. Overridden HERE,
  # on top of common_theme, rather than changing common_theme itself,
  # since common_theme is shared with D4 Fig 1, D3 Fig 2/3 and D3
  # Fig 1 -- enlarging it there would resize every one of those
  # figures' legends too, not just this one.
  #-------------------------------------------------------
  
  theme(
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 17, face = "plain"),
    legend.key.size = unit(1, "cm"),
    legend.key.width = unit(1.4, "cm")
  )
  
  
  #---------------------------------------------------------
  # Print plot
  #---------------------------------------------------------
  
  print(p10)
  
  
  #=========================================================
  # Caption
  #=========================================================
  
  p10_caption <- paste(
    "D4 - Fig 5. Changes through time in the composition of realized",
    "mortality for the baseline models. Mortality is partitioned into",
    "fishing, harp seal predation, other predation, and other mortality",
    "(M0), expressed as a proportion of total realized mortality",
    "(F + total predation + M0). Biomass accumulation and net migration",
    "are excluded because they are not mortality."
  )
  
  cat(
    "\n",
    p10_caption,
    "\n",
    sep = ""
  )
  
  
  #=========================================================
  # Save figure
  #=========================================================
  
  ggsave(
    file.path(
      root_dir,
      "D4_Fig5_MortalityComponents_Proportion.png"
    ),
    p10,
    width = 18,
    height = 12,
    dpi = 300
  )
  
  #=========================================================
  # Estimates table (companion to D4 Fig 5)
  #=========================================================
  
  fwrite(
    z_prop_long,
    file.path(root_dir, "D4_Fig5_Estimates.csv")
  )
  
}

########## ADDED DV

#=========================================================
# Figure 12: internal inconsistency, visualized
# Diet fraction of harp seal (SOW deliverable 2)
#=========================================================

#---------------------------------------------------------
# Ecopath / FOC reference table
#---------------------------------------------------------
prey_order <- c(
  "Arctic cod", "Atlantic herring", "Capelin", "Cephalopod",
  "Flounder", "Gadus sp.", "Other fish", "Other invertebrates",
  "Redfish", "Sand lance", "Shrimp", "Zooplankton"
)

ecopath_long <- rbindlist(list(
  data.table(
    Prey = prey_order, Period = "1985-1987",
    Ecopath = c(0.211, 0.0239, 0.4, 0, 0.405, 0.07, 0.091, 0.014, 0.0028, 0.0486, 0.085, 0.0141),
    FOC     = c(73.5, 6.33, 14.8, 3.97, 4.68, 12.1, 11.2, 3.85, 0.43, 0.89, 18.3, 5.9)
  ),
  data.table(
    Prey = prey_order, Period = "2013-2015",
    Ecopath = c(0.074, 0.15, 0.16, 0, 0.06, 0.025, 0.027, 0.048, 0.0362, 0.009, 0.384, 0.026),
    FOC     = c(38.1, 42, 32.3, 4.48, 8.88, 37.1, 17.1, 3.44, 3.15, 3.97, 13.8, 3.17)
  ),
  data.table(
    Prey = prey_order, Period = "2018-2020 NL Shelf",
    Ecopath = c(0.193, 0.19, 0.09, 0.0005, 0.071, 0.13, 0.11, 0.11, 0.015, 0.1, 0.0005, 0.06),
    FOC     = c(48.9, 48, 11.4, 6.83, 2.78, 24.3, 13.2, 6.84, 2.16, 3.78, 17.2, 8.74)
  ),
  data.table(
    Prey = prey_order, Period = "2018-2020 Grand Banks",
    Ecopath = c(0.07, 0.195, 0.11, 0.07, 0.085, 0.041, 0.087, 0.0745, 0.015, 0.201, 0, 0.05),
    FOC     = c(75, 12.5, 0, 0, 0, 12.5, 25, 0, 0, 25, 0, 12.5)
  )
))

#---------------------------------------------------------
# ASSUMPTION -- confirm before trusting this figure:
# "1985-1987" == model's "1985-1988" (naming drift only).
# "2018-2020 NL Shelf" applied to both N-coded scenarios
# (N50, N80); "2018-2020 Grand Banks" to both S-coded
# scenarios (S50, S20).
#---------------------------------------------------------
period_to_simulation <- data.table(
  Period = c(
    "1985-1987",
    "2013-2015",
    "2018-2020 NL Shelf", "2018-2020 NL Shelf",
    "2018-2020 Grand Banks", "2018-2020 Grand Banks"
  ),
  Simulation = c(
    "1985-1988",
    "2013-2015",
    "2018-2020_N50", "2018-2020_N80",
    "2018-2020_S50", "2018-2020_S20"
  )
)

#---------------------------------------------------------
# Prey -> Group crosswalk, rebuilt from the screenshot.
#
# Cells marked CONFIRM were truncated by the column width
# in the screenshot -- the text after "..." is my best
# guess at the completion, not a transcription. Verify
# each against seal_prey_species / qy_results_diet$Group
# before trusting this figure.
#
# NOTE: this table has Gadus sp. -> "Cod adult" and
# Gadus sp. -> "Other piscivorous fish". That's different
# from what was said earlier (cod adult/juvenile/Greenland
# cod) -- flagging again in case the screenshot itself is
# incomplete or mis-cropped.
#---------------------------------------------------------
prey_group_crosswalk <- data.table(
  Prey = c(
    "Arctic cod",
    "Atlantic herring",
    "Capelin",
    "Cephalopod",
    "Flounder",
    "Gadus sp.",
    "Other fish",
    "Other invertebrates",
    "Redfish",
    "Sand lance",
    "Shrimp",
    "Zooplankton",
    "Flounder",
    "Flounder",
    "Flounder",
    "Flounder",
    "Other fish",
    "Other fish",
    "Other fish",
    "Other invertebrates",
    "Other invertebrates",
    "Gadus sp.",
    "Gadus sp."
  ),
  Group = c(
    "Arctic cod",
    "Herring",
    "Capelin",
    "Squid",
    "American Plaice adult",             # CONFIRM: truncated "American plaice a..."
    "Cod adult",
    "Large benthivorous fish",           # CONFIRM: truncated "Large bentivorous..."
    "Predatory invertebrates",           # CONFIRM: truncated "Predatory inverteb..."
    "Redfish",
    "Sandlance",
    "Shrimp",
    "Macrozooplankton",
    "American Plaice juvenile",          # CONFIRM: truncated "American plaice ju..."
    "Greenland halibut",
    "Yellowtail flounder",
    "Witch flounder",
    "Small benthivorous fish",           # CONFIRM: truncated "Small bentivorous..."
    "Other planktivorous fish",          # CONFIRM: truncated "Other planktivorous..."
    "Medium benthivorous fish",          # CONFIRM: truncated "Medium bentivoro..."
    "Deposit feeding invertebrates",     # CONFIRM: truncated "Deposit feeding in..."
    "Suspension feeding invertebrates",  # CONFIRM: truncated "Suspension feedin..."
    "Other piscivorous fish",             # CONFIRM: truncated + conflicts with earlier statement
    "Cod juvenile"
  )
)

inconsistency_data <- qy_results_diet[
  Model == "baseline" &
    Predator == "Harp seal" &
    (
      Group %in% seal_prey_species |
        Group == "Other piscivorous fish"
    )
]
#-------------------------------------------------------
# FIX: normalize known Simulation label drift
#-------------------------------------------------------
inconsistency_data[
  Simulation == "1985_1988",
  Simulation := "1985-1988"
]
if(nrow(inconsistency_data) == 0){
  cat(
    "\nSkipping Figure 12 -- no seal-prey groups or",
    " 'Other piscivorous fish' found in baseline Q/Y results.",
    " Check group name spelling.\n"
  )
} else {
  #-------------------------------------------------------
  # Make sure ALL expected groups are present
  #-------------------------------------------------------
  expected_groups <- unique(
    c(
      seal_prey_species,
      "Other piscivorous fish"
    )
  )
  simulation_levels_diet <- c(
    "1985-1988",
    "2013-2015",
    "2018-2020_N50",
    "2018-2020_S50",
    "2018-2020_N80",
    "2018-2020_S20"
  )
  #-------------------------------------------------------
  # NEW: catch Simulation naming drift
  #-------------------------------------------------------
  unexpected_sims <- setdiff(
    unique(inconsistency_data$Simulation),
    simulation_levels_diet
  )
  if(length(unexpected_sims) > 0){
    cat(
      "\nD2 Fig 2: Simulation value(s) found in baseline data",
      " but not in simulation_levels_diet -- these rows will",
      " be dropped: ",
      paste(unexpected_sims, collapse = ", "),
      "\n",
      sep = ""
    )
  }
  #-------------------------------------------------------
  # NEW: sanity-check the Ecopath crosswalk BEFORE using it
  #-------------------------------------------------------
  unmatched_groups <- setdiff(
    unique(prey_group_crosswalk$Group),
    expected_groups
  )
  if(length(unmatched_groups) > 0){
    cat(
      "\nD2 Fig 2: prey_group_crosswalk references Group name(s)",
      " not found in seal_prey_species -- fix the spelling: ",
      paste(unmatched_groups, collapse = ", "),
      "\n",
      sep = ""
    )
  }
  unmatched_periods <- setdiff(
    unique(period_to_simulation$Simulation),
    simulation_levels_diet
  )
  if(length(unmatched_periods) > 0){
    cat(
      "\nD2 Fig 2: period_to_simulation references Simulation",
      " value(s) not in simulation_levels_diet: ",
      paste(unmatched_periods, collapse = ", "),
      "\n",
      sep = ""
    )
  }
  expected_grid <- CJ(
    Simulation = simulation_levels_diet,
    Group = expected_groups,
    unique = TRUE
  )
  #-------------------------------------------------------
  # NEW: flag duplicate baseline rows before collapsing
  #-------------------------------------------------------
  dup_counts <- inconsistency_data[
    ,
    .N,
    by = .(Simulation, Group)
  ][N > 1]
  if(nrow(dup_counts) > 0){
    cat(
      "\nD2 Fig 2: ",
      nrow(dup_counts),
      " Simulation x Group combination(s) have more than one",
      " baseline row; the maximum DietFraction is used for",
      " each. Check for duplicate entries:\n",
      sep = ""
    )
    print(dup_counts)
  }
  #-------------------------------------------------------
  # Keep one baseline row per Simulation and Group
  #-------------------------------------------------------
  inconsistency_data <- inconsistency_data[
    ,
    .(
      DietFraction = if(
        all(is.na(DietFraction))
      ) {
        0
      } else {
        max(DietFraction, na.rm = TRUE)
      }
    ),
    by = .(Simulation, Group)
  ]
  #-------------------------------------------------------
  # Add groups that are completely missing
  #-------------------------------------------------------
  inconsistency_data <- merge(
    expected_grid,
    inconsistency_data,
    by = c("Simulation", "Group"),
    all.x = TRUE
  )
  #-------------------------------------------------------
  # Replace remaining NA diet fractions with zero
  #-------------------------------------------------------
  inconsistency_data[
    is.na(DietFraction),
    DietFraction := 0
  ]
  #-------------------------------------------------------
  # NEW: attach the Ecopath reference value
  #-------------------------------------------------------
  ecopath_by_sim <- merge(
    ecopath_long,
    period_to_simulation,
    by = "Period",
    allow.cartesian = TRUE
  )
  ecopath_points <- merge(
    ecopath_by_sim,
    prey_group_crosswalk,
    by = "Prey",
    allow.cartesian = TRUE
  )[
    ,
    .(Simulation, Group, EcopathValue = Ecopath)
  ]
  inconsistency_data <- merge(
    inconsistency_data,
    ecopath_points,
    by = c("Simulation", "Group"),
    all.x = TRUE
  )
  #-------------------------------------------------------
  # Restore the intended Simulation order
  #-------------------------------------------------------
  inconsistency_data[
    ,
    Simulation := factor(
      Simulation,
      levels = simulation_levels_diet
    )
  ]
  #-------------------------------------------------------
  # Number of groups
  #-------------------------------------------------------
  n_groups_d2 <- length(
    unique(inconsistency_data$Group)
  )
  #-------------------------------------------------------
  # Sort groups by highest diet fraction
  #-------------------------------------------------------
  group_order <- inconsistency_data[
    Group != "Other piscivorous fish",
    .(
      MaxDietFraction = max(
        DietFraction,
        na.rm = TRUE
      )
    ),
    by = Group
  ][
    order(-MaxDietFraction),
    Group
  ]
  group_order <- c(
    group_order,
    "Other piscivorous fish"
  )
  inconsistency_data[
    ,
    Group := factor(
      Group,
      levels = rev(group_order)
    )
  ]
  #-------------------------------------------------------
  # Label position
  #-------------------------------------------------------
  inconsistency_data[
    ,
    LabelPos := 0.015
  ]
  #-------------------------------------------------------
  # Check for values above plotting limit
  #-------------------------------------------------------
  n_clipped <- sum(
    inconsistency_data$DietFraction > 0.5,
    na.rm = TRUE
  )
  if(n_clipped > 0){
    cat(
      "\nD2 Fig 2: ",
      n_clipped,
      " group(s) have DietFraction > 0.5 and will be ",
      "visually clipped at the fixed 0-0.5 axis limit.",
      " The printed number label still shows the true value.",
      "\n",
      sep = ""
    )
  }
  #-------------------------------------------------------
  # NEW: sum of the harp seal's published Ecopath diet fraction
  # across all prey, per raw Simulation (for the facet strip labels)
  #-------------------------------------------------------
  ## IMPORTANT: summed from `ecopath_long` grouped by Period, BEFORE
  ## the Prey -> Group expansion via prey_group_crosswalk (i.e. NOT
  ## from `ecopath_points`/`ecopath_by_sim` post-crosswalk-join) --
  ## summing after that expansion would double-, triple-, or
  ## quintuple-count any Prey category that maps to multiple Groups
  ## (e.g. "Flounder" maps to 5 Groups), since the same published
  ## value would appear once per Group it was split into. This sum is
  ## meant to answer "does West et al.'s own Ecopath diet vector sum to
  ## ~1, as it should for a properly balanced predator diet", which
  ## only makes sense computed at the ORIGINAL West et al. prey-category
  ## resolution, not after re-expanding to our model's finer Groups.
  
  ecopath_sum_by_sim <- merge(
    ecopath_long[, .(EcopathSum = sum(Ecopath, na.rm = TRUE)), by = Period],
    period_to_simulation,
    by = "Period",
    allow.cartesian = TRUE
  )[, .(Simulation, EcopathSum)]
  
  cat("\n===== D2 Fig 2: sum of published Ecopath diet fraction, by raw Simulation =====\n")
  cat("(Should be close to 1.00 for a properly mass-balanced diet vector;",
      "see West et al. (2025) Table S1 / Harris (2026) review for whichever",
      "periods deviate meaningfully.)\n\n")
  print(ecopath_sum_by_sim[order(Simulation)])
  cat("================================================================================\n")
  
  ecopath_sum_lookup <- setNames(
    round(ecopath_sum_by_sim$EcopathSum, 3),
    ecopath_sum_by_sim$Simulation
  )
  
  d2fig2_strip_labels <- setNames(
    paste0(
      names(ecopath_sum_lookup),
      "\n(\u03a3 Ecopath = ", ecopath_sum_lookup, ")"
    ),
    names(ecopath_sum_lookup)
  )
  
  #-------------------------------------------------------
  # NEW: Groups whose published West et al. prey category was split
  # across multiple Groups in this model (e.g. "Flounder" ->
  # American Plaice adult/juvenile, Greenland halibut, Yellowtail
  # flounder, Witch flounder) -- flagged with a standalone asterisk
  # positioned just left of the 0% axis line (not appended to the
  # axis text), since their black "Published" tick represents a
  # shared, not FG-specific, published value. One row per
  # (Simulation, Group) so the mark appears on every facet panel.
  #-------------------------------------------------------
  
  split_prey <- prey_group_crosswalk[, .N, by = Prey][N > 1, Prey]
  split_groups <- unique(prey_group_crosswalk[Prey %in% split_prey, Group])
  
  cat("\nD2 Fig 2: Groups flagged with an asterisk (published prey category",
      "split across multiple Groups):", paste(split_groups, collapse = ", "), "\n")
  
  ## Positioned next to each bar's OWN "Published" tick mark (at
  ## y = EcopathValue, the same value the geom_segment tick below uses)
  ## rather than a fixed axis-edge position -- the tick's y-position
  ## varies per (Simulation, Group), so the asterisk has to track it
  ## individually rather than using one shared offset for every panel.
  
  split_group_marks <- inconsistency_data[
    Group %in% split_groups & !is.na(EcopathValue)
  ]
  split_group_marks[, AsteriskPos := EcopathValue - 0.025]
  
  cat("\nD2 Fig 2: computed asterisk positions (verify these track each",
      "bar's own published value, not a fixed spot):\n")
  print(split_group_marks[order(Simulation, Group), .(Simulation, Group, EcopathValue, AsteriskPos)])
  
  #-------------------------------------------------------
  # Plot
  #-------------------------------------------------------
  #---------------------------------------------------------
  # Packages
  #---------------------------------------------------------
  
  library(ggplot2)
  library(data.table)
  library(grid)
  
  #---------------------------------------------------------
  # Custom legend key
  # Vertical line
  #---------------------------------------------------------
  
  draw_key_vertical_line <- function(data, params, size) {
    
    grid::segmentsGrob(
      x0 = 0.5,
      x1 = 0.5,
      y0 = 0.10,
      y1 = 0.90,
      gp = grid::gpar(
        col = "black",
        lwd = 1 * .pt
      )
    )
  }
  
  #---------------------------------------------------------
  # Label position
  #---------------------------------------------------------
  
  inconsistency_data[
    ,
    LabelPos := 0.58
  ]
  
  #---------------------------------------------------------
  # Dummy data for legend
  #---------------------------------------------------------
  ## FIXED: the swap was caused by override.aes vectors (in
  ## scale_shape_manual below) being matched POSITIONALLY to the
  ## legend's break order -- and that order defaults to ALPHABETICAL
  ## for a plain character "Legend" column, not the order things were
  ## written in `values=`. "Diet item shared by multiple FGs" (D)
  ## sorts before "EwE values" (E), so the override.aes vectors (which
  ## assumed "EwE values" came first) were applied to the wrong entry.
  ## Both dummy tables' Legend column is now an explicit factor with a
  ## fixed level order ("EwE values" first, "Diet item..." second),
  ## matching scale_shape_manual's values= and override.aes vectors
  ## below exactly -- removes any dependence on alphabetical sorting.
  
  shape_legend_levels <- c(
    "EwE models",
    "Diet item shared by multiple FGs"
  )
  
  legend_bar <- data.table(
    Group = inconsistency_data$Group[1],
    DietFraction = 0,
    Legend = factor("EwE models", levels = shape_legend_levels)
  )
  
  legend_published <- data.table(
    Group = inconsistency_data$Group[1],
    y = 0,
    Legend = "Published in West et al. (2025)"
  )
  
  ## Third legend entry, for the split-prey asterisk marks (see
  ## split_group_marks above) -- explains what the "*" next to some
  ## bars' published tick means. Uses the STANDARD point key glyph,
  ## with shape = 8, which is an actual built-in asterisk point shape
  ## in R/ggplot2's shape palette -- no custom drawing code needed.
  
  legend_asterisk <- data.table(
    Group = inconsistency_data$Group[1],
    y = 0,
    Legend = factor("Diet item shared by multiple FGs", levels = shape_legend_levels)
  )
  
  #---------------------------------------------------------
  # Plot
  #---------------------------------------------------------
  
  p_d2fig2 <- ggplot(
    inconsistency_data,
    aes(
      x = Group,
      y = DietFraction,
      fill = Group == "Other piscivorous fish"
    )
  ) +
    
    #-------------------------------------------------------
  # Diet fraction bars
  #-------------------------------------------------------
  geom_col() +
    
    #-------------------------------------------------------
  # Ecopath reference value
  #-------------------------------------------------------
  geom_segment(
    data = inconsistency_data[
      !is.na(EcopathValue)
    ],
    aes(
      x = as.numeric(Group) - 0.35,
      xend = as.numeric(Group) + 0.35,
      y = EcopathValue,
      yend = EcopathValue
    ),
    inherit.aes = FALSE,
    linewidth = 1.2,
    colour = "black"
  ) +
    
    #-------------------------------------------------------
  # Diet fraction labels
  #-------------------------------------------------------
  geom_text(
    aes(
      y = LabelPos,
      label = ifelse(
        DietFraction == 0,
        "0",
        sprintf("%.4f", DietFraction)
      ),
      colour = DietFraction == 0
    ),
    hjust = 1,
    size = 3
  ) +
    
    #-------------------------------------------------------
  # EwE legend
  #-------------------------------------------------------
  geom_point(
    data = legend_bar,
    aes(
      x = Group,
      y = DietFraction,
      shape = Legend
    ),
    inherit.aes = FALSE,
    colour = "#2E7D32",
    fill = "#2E7D32",
    size = 4,
    alpha = 0,
    key_glyph = draw_key_point
  ) +
    
    #-------------------------------------------------------
  # Published legend
  # REAL line layer
  #-------------------------------------------------------
  geom_segment(
    data = legend_published,
    aes(
      x = Group,
      xend = Group,
      y = y,
      yend = y + 0.01,
      linetype = Legend
    ),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = 1.2,
    alpha = 0,
    key_glyph = draw_key_vertical_line
  ) +
    
    #-------------------------------------------------------
  # Asterisk legend
  #-------------------------------------------------------
  ## Shares the `shape` aesthetic with the "EwE values" entry above
  ## (both merge into ONE shape-based legend guide, order=1). FIXED:
  ## no custom key_glyph -- shape 8 IS an asterisk in ggplot2's
  ## built-in point shape palette, so the standard point glyph renders
  ## it correctly on its own, as long as scale_shape_manual's
  ## override.aes is given per-level vectors (fixed below) rather than
  ## single scalars that get applied to every level uniformly.
  geom_point(
    data = legend_asterisk,
    aes(
      x = Group,
      y = y,
      shape = Legend
    ),
    inherit.aes = FALSE,
    colour = "black",
    size = 4,
    alpha = 0
  ) +
    
    #-------------------------------------------------------
  # Plot limits
  #-------------------------------------------------------
  coord_flip(
    ylim = c(-0.03, 0.57)
  ) +
    
    scale_y_continuous(
      breaks = c(
        0,
        0.1,
        0.2,
        0.3,
        0.4,
        0.5
      )
    ) +
    
    #-------------------------------------------------------
  # Split-prey asterisk marks
  #-------------------------------------------------------
  ## Positioned next to each bar's own "Published" tick (at
  ## y = EcopathValue, same as the geom_segment tick below) -- see
  ## split_group_marks above. FIXED: dropped hjust for offsetting --
  ## hjust/vjust in geom_text is a known source of unexpected behaviour
  ## under coord_flip() (justification is computed in the text's own
  ## pre-flip frame, so hjust=1 doesn't reliably mean "left of this
  ## point" once the whole plot is flipped). Centered (default hjust)
  ## on the already-offset AsteriskPos instead, so placement depends
  ## only on the numeric y-position, not on justification direction.
  geom_text(
    data = split_group_marks,
    aes(x = Group, y = AsteriskPos, label = "*"),
    inherit.aes = FALSE,
    colour = "black",
    size = 4.5,
    fontface = "bold"
  ) +
    
    #-------------------------------------------------------
  # Facets
  #-------------------------------------------------------
  ## Strip labels now include the sum of the published Ecopath diet
  ## fraction across all prey for that raw Simulation (see
  ## ecopath_sum_by_sim above).
  facet_wrap(
    ~Simulation,
    nrow = 1,
    labeller = as_labeller(d2fig2_strip_labels)
  ) +
    
    #-------------------------------------------------------
  # Bar colours
  #-------------------------------------------------------
  scale_fill_manual(
    values = c(
      `TRUE` = "#d73027",
      `FALSE` = "#2E7D32"
    ),
    guide = "none"
  ) +
    
    #-------------------------------------------------------
  # Number colours
  #-------------------------------------------------------
  scale_colour_manual(
    values = c(
      `TRUE` = "#d73027",
      `FALSE` = "#2E7D32"
    ),
    guide = "none"
  ) +
    
    #-------------------------------------------------------
  # EwE legend
  #-------------------------------------------------------
  ## "Diet item shared by multiple FGs" shares this shape scale with
  ## "EwE values" so both merge into one legend guide.
  ##
  ## Two separate bugs were found and fixed here in sequence:
  ## (1) override.aes originally gave single scalar values (shape=15,
  ## colour="#2E7D32", ...), which guide_legend applies to EVERY key in
  ## the guide -- fixed by giving one value PER LEVEL, as vectors.
  ## (2) Those per-level vectors are matched POSITIONALLY to the
  ## guide's break order, and that order defaults to ALPHABETICAL for
  ## a plain character Legend column -- "Diet item..." (D) sorts before
  ## "EwE values" (E), so the vectors below (written assuming "EwE
  ## values" came first) were silently applied to the wrong keys,
  ## swapping which glyph/colour each label got. Fixed at the source:
  ## `legend_bar`/`legend_asterisk`'s Legend column is now an explicit
  ## factor with levels = shape_legend_levels (defined just above them),
  ## so the break order is guaranteed to match the vectors below
  ## regardless of alphabetical sorting.
  ##
  ## THIRD bug, this round: the name here must match shape_legend_levels/
  ## legend_bar$Legend EXACTLY -- it was "EwE model" (singular) here vs.
  ## "EwE models" (plural) everywhere else, so ggplot couldn't match this
  ## level to anything, silently dropping it from the guide down to a
  ## single row -- which is why override.aes's 2-length vectors then
  ## failed with "replacement has 2 rows, data has 1".
  scale_shape_manual(
    name = NULL,
    values = c(
      "EwE models" = 15,
      "Diet item shared by multiple FGs" = 8
    ),
    guide = guide_legend(
      order = 1,
      override.aes = list(
        shape  = c(15, 8),
        colour = c("#2E7D32", "black"),
        fill   = c("#2E7D32", NA),
        size   = c(4, 4),
        alpha  = c(1, 1)
      )
    )
  ) +
    
    #-------------------------------------------------------
  # Published legend
  #-------------------------------------------------------
  scale_linetype_manual(
    name = NULL,
    values = c(
      "Published in West et al. (2025)" = "solid"
    ),
    guide = guide_legend(
      order = 2,
      override.aes = list(
        colour = "black",
        linewidth = 1.2,
        alpha = 1
      )
    )
  ) +
    
    #-------------------------------------------------------
  # Theme
  #-------------------------------------------------------
  theme_bw(
    base_size = 13
  ) +
    
    theme(
      strip.background = element_blank(),
      strip.text = element_text(
        colour = "black"
      ),
      strip.placement = "outside",
      legend.position = "bottom",
      legend.text = element_text(
        size = 11
      )
    ) +
    
    labs(
      x = NULL,
      y = "Harp seal diet fraction"
    )
  
  #---------------------------------------------------------
  # Display
  #---------------------------------------------------------
  
  print(p_d2fig2)
  
  #-------------------------------------------------------
  # Caption
  #-------------------------------------------------------
  p_d2fig2_caption <- paste(
    "D2 - Fig 2. Harp seal diet fraction across every functional",
    "group identified as a harp seal prey species (blue), plus",
    "'Other piscivorous fish' for contrast (red). Groups are",
    "ordered from highest to lowest diet fraction, with",
    "'Other piscivorous fish' shown at the bottom. Groups with",
    "no recorded diet fraction are treated as zero. The black tick",
    "mark on each bar shows the corresponding published Ecopath",
    "diet-fraction value where available. Asterisks mark functional",
    "groups that jointly represent a single West et al. (2025) diet",
    "item mapped to multiple EwE functional groups (e.g., Flounder",
    "is represented jointly by American Plaice adult/juvenile,",
    "Greenland halibut, and Witch flounder), rather than a diet item",
    "mapped one-to-one to a single group."
  )
  cat(
    "\n",
    p_d2fig2_caption,
    "\n",
    sep = ""
  )
  #-------------------------------------------------------
  # Save
  #-------------------------------------------------------
  ggsave(
    file.path(
      root_dir,
      "D2_Fig2_InternalInconsistency_HarpSealDietFraction.png"
    ),
    p_d2fig2,
    width = max(
      12,
      1 * length(simulation_levels_diet)
    ),
    height = max(
      6,
      0.3 * n_groups_d2
    ),
    dpi = 300
  )
  
  #=========================================================
  # Estimates table (companion to D2 Fig 2)
  #=========================================================
  
  fwrite(
    inconsistency_data,
    file.path(root_dir, "D2_Fig2_Estimates.csv")
  )
  
}

#=========================================================
# Figure 12: sensitivity trend for Cod adult (SOW deliverable 3)
#=========================================================
## The heatmaps (Figures 4 and 8) show sensitivity as a grid; this
## shows it as a TREND, which is what "evaluation of how ratios change
## under plausible parameter variation" is really asking for -- does
## the ratio move a little or a lot as Q/B and diet move across their
## plausible range? Cod adult only, since it's the one species with
## full Q/Y coverage right now (see the open item on the Diet
## composition file elsewhere in this script).
##
## Q/B axis ordered by actual perturbation magnitude (qb-30 ... qb+30),
## not alphabetically -- alphabetical order would scramble the trend
## and make a real effect look like noise.

if(exists("ratio_compare") && nrow(ratio_compare) > 0){
  
  
  #=========================================================
  # D3 - Figure 1
  # Cod adult: Q/B and diet sensitivity
  #=========================================================
  
  #=========================================================
  # Settings
  #=========================================================
  
  metric_levels <- c(
    "M2 / F",
    "Q / Y"
  )
  
  period_levels <- c(
    "1985-1988",
    "2013-2015",
    "2018-2020"
  )
  
  #=========================================================
  # Explicit simulation order
  #
  # Baseline
  # Q/B sensitivities
  # Diet sensitivities
  #=========================================================
  
  simulation_order <- c(
    "baseline",
    "qb-30",
    "qb-15",
    "qb+15",
    "qb+30",
    "diet-20",
    "diet-20pisciv"
  )
  
  #=========================================================
  # Prepare data
  #=========================================================
  
  ## MISSING before this fix: qb_trend was referenced by the melt()
  ## below but never actually built anywhere in the script, causing
  ## "object 'qb_trend' not found". Built here from `ratio_compare`
  ## (the table this whole block's exists()/nrow() guard above already
  ## checks for), filtered to Cod adult and the sensitivity models this
  ## figure covers (baseline + Q/B + diet, no biomass -- matching
  ## `simulation_order` just above).
  
  qb_trend <- ratio_compare[
    Group == "Cod adult" & Model %in% simulation_order,
    .(Simulation, Model, M2_over_F, Q_over_Y)
  ]
  
  if(nrow(qb_trend) == 0){
    warning(
      "qb_trend is empty after filtering ratio_compare to Group == ",
      "'Cod adult' and Model %in% simulation_order -- check that both",
      " conditions actually match rows in ratio_compare (e.g. exact",
      " spelling of 'Cod adult', and that these Model names exist)."
    )
  }
  
  qb_trend_long <- melt(
    qb_trend,
    id.vars = c(
      "Simulation",
      "Model"
    ),
    measure.vars = c(
      "M2_over_F",
      "Q_over_Y"
    ),
    variable.name = "Metric",
    value.name = "Ratio"
  )
  
  qb_trend_long[
    ,
    Metric := factor(
      Metric,
      levels = c(
        "M2_over_F",
        "Q_over_Y"
      ),
      labels = metric_levels
    )
  ]
  
  # Explicit model order
  qb_trend_long[
    ,
    Model := factor(
      Model,
      levels = simulation_order
    )
  ]
  
  #=========================================================
  # Period summaries
  #=========================================================
  
  qb_period_data <- build_period_timeseries(
    qb_trend_long,
    value_col = "Ratio",
    extra_group_cols = "Metric"
  )
  
  # Make sure the factor order is retained
  qb_period_data$mean_line[
    ,
    Model := factor(
      Model,
      levels = simulation_order
    )
  ]
  
  qb_period_data$raw_points[
    ,
    Model := factor(
      Model,
      levels = simulation_order
    )
  ]
  
  #=========================================================
  # Published estimates
  # West et al. (2025)
  #=========================================================
  
  qb_published <- data.table(
    
    Period = factor(
      c(
        "1985-1988",
        "1985-1988",
        "2018-2020",
        "2018-2020"
      ),
      levels = period_levels
    ),
    
    Ratio = c(
      1.3,
      1.7,
      17,
      24
    ),
    
    Metric = factor(
      c(
        "M2 / F",
        "Q / Y",
        "M2 / F",
        "Q / Y"
      ),
      levels = metric_levels
    ),
    
    Published = factor(
      c(
        "M2/F = 1.3 (1985-1988)",
        "Q/Y = 1.7 (1985-1988)",
        "M2/F = 17 (2018-2020)",
        "Q/Y = 24 (2018-2020)"
      ),
      levels = c(
        "M2/F = 1.3 (1985-1988)",
        "Q/Y = 1.7 (1985-1988)",
        "M2/F = 17 (2018-2020)",
        "Q/Y = 24 (2018-2020)"
      )
    )
  )
  
  #=========================================================
  # Model colours
  #
  # Baseline = black (redrawn thicker and solid on top of the
  # other lines -- see the geom_line overlay below -- rather than
  # distinguished by linetype, which got lost against the teal/
  # orange gradient where lines overlap closely)
  #
  # Q/B sensitivity = teal gradient
  # Low Q/B  -> light teal
  # High Q/B -> dark teal
  #
  # Diet sensitivity = orange gradient
  #=========================================================
  
  model_cols <- c(
    
    "baseline" = "black",
    
    # Q/B sensitivity
    "qb-30" = "#A6DAD3",
    "qb-15" = "#66C2B7",
    "qb+15" = "#1F9E89",
    "qb+30" = "#006D5B",
    
    # Diet sensitivity
    "diet-20" = "#FDD49E",
    "diet-20pisciv" = "#E66101"
  )
  
  # Reorder colours to exactly match simulation order
  model_cols <- model_cols[
    simulation_order
  ]
  
  #=========================================================
  # Plot
  #=========================================================
  #=========================================================
  # Plot
  #=========================================================
  
  ## Uncertainty band (replaces the old jittered raw-point layer, same
  ## treatment as D3 Fig 2/3 and D1 Fig 1). `qb_period_data$raw_points`
  ## only carries the two 2018-2020 population-split composites (see
  ## build_period_timeseries), so min/max is only non-degenerate at
  ## that period; 1985-1988 and 2013-2015 have a single simulation
  ## each and fall back to the mean, so the band collapses onto the
  ## trend line there.
  
  qb_period_data$mean_line[
    ,
    PeriodNum := as.numeric(Period)
  ]
  
  qb_raw_range <- qb_period_data$raw_points[
    ,
    .(
      ymin = min(Value, na.rm = TRUE),
      ymax = max(Value, na.rm = TRUE)
    ),
    by = .(Metric, Model, Period)
  ]
  
  qb_range <- merge(
    qb_period_data$mean_line[, .(Metric, Model, Period, PeriodNum, Value)],
    qb_raw_range,
    by = c("Metric", "Model", "Period"),
    all.x = TRUE
  )
  
  qb_range[is.na(ymin), ymin := Value]
  qb_range[is.na(ymax), ymax := Value]
  
  p_d3fig1 <- ggplot(
    qb_period_data$mean_line,
    aes(
      x = PeriodNum,
      y = Value,
      colour = Model,
      group = Model
    )
  ) +
    
    #-------------------------------------------------------
  # 1. Uncertainty band (min-max across 2018-2020 population-split
  # composites; degenerates to the trend line where only one
  # simulation exists)
  #
  # Draw FIRST so that the model lines and mean points
  # are plotted on top of it.
  #-------------------------------------------------------
  
  geom_ribbon(
    data = qb_range,
    aes(
      x = PeriodNum,
      ymin = ymin,
      ymax = ymax,
      fill = Model,
      group = Model
    ),
    colour = NA,
    alpha = 0.22,
    inherit.aes = FALSE
  ) +
    
    #-------------------------------------------------------
  # 2. Simulation trends
  #-------------------------------------------------------
  
  geom_line(
    linewidth = 0.75
  ) +
    
    #-------------------------------------------------------
  # 2b. Baseline overlay -- "halo" effect: a wider white stroke
  # underneath the black line so baseline visually punches through
  # any colour it crosses, not just a thicker line in the same
  # colour family. The white halo stays solid (dashing both strokes
  # made them read as two misaligned dashed lines, since ggplot/grid
  # scales a linetype's dash length by the line's own linewidth, so
  # a 2.6pt white dash and a 1.3pt black dash never line up). Only
  # the black line on top is dashed, so whatever is directly behind
  # baseline shows through the gaps in the black dashes, while the
  # solid white halo still makes baseline pop against the sensitivity
  # colours. See the matching comment in D3 Fig 2/3.
  #-------------------------------------------------------
  
  geom_line(
    data = qb_period_data$mean_line[Model == "baseline"],
    linewidth = 2.6,
    colour = "white",
    alpha = 0.6
  ) +
    
    geom_line(
      data = qb_period_data$mean_line[Model == "baseline"],
      linewidth = 1.3,
      colour = "black",
      linetype = "dashed"
    ) +
    
    #-------------------------------------------------------
  # 3. Mean simulation estimates
  #
  # Draw so the mean is clearly visible above the
  # raw scenario estimates.
  #-------------------------------------------------------
  
  geom_point(
    aes(
      fill = Model
    ),
    shape = 21,
    colour = "black",
    size = 2.8,
    stroke = 0.45
  ) +
    
    #-------------------------------------------------------
  # 4. Published West et al. (2025) estimates
  # FIXED: moved from layer 2 to last -- drawn earlier, the
  # asterisk markers could be hidden under a simulation trend line,
  # the baseline overlay, or a mean-estimate point that landed at
  # the same spot. Drawing them last puts the published-value
  # asterisks on top of every other layer, so they stay visible.
  #-------------------------------------------------------
  
  geom_point(
    data = qb_published,
    aes(
      x = as.numeric(Period),
      y = Ratio,
      shape = Published
    ),
    colour = "black",
    size = 3,
    stroke = 0.8,
    inherit.aes = FALSE
  ) +
    
    #=======================================================
  # Simulation colours
  #=======================================================
  
  scale_colour_manual(
    values = model_cols,
    breaks = simulation_order,
    labels = simulation_order,
    name = "Simulations"
  ) +
    
    scale_fill_manual(
      values = model_cols,
      breaks = simulation_order,
      guide = "none"
    ) +
    
    #=======================================================
  # Published estimates legend
  #=======================================================
  
  scale_shape_manual(
    values = c(
      "M2/F = 1.3 (1985-1988)" = 8,
      "Q/Y = 1.7 (1985-1988)" = 8,
      "M2/F = 17 (2018-2020)" = 8,
      "Q/Y = 24 (2018-2020)" = 8
    ),
    name = "Published in West et al. (2025)"
  ) +
    
    #=======================================================
  # X axis
  #=======================================================
  
  scale_x_continuous(
    breaks = 1:length(period_levels),
    labels = period_levels
  ) +
    
    #=======================================================
  # Facets
  #=======================================================
  
  facet_wrap(
    ~Metric,
    scales = "free_y",
    ncol = 2
  ) +
    
    #=======================================================
  # Log scale
  # Same rationale as D3 Fig 2/3: values here can span more than
  # one order of magnitude between 1985-1988/2013-2015 and
  # 2018-2020, and a linear axis compresses the early, smaller
  # values against the 2018-2020 max. Zero-mortality points (F or
  # M2 parameterized as zero, so the ratio is 0 or undefined)
  # become non-finite under log10 and are dropped by ggplot with a
  # "non-finite values removed" warning -- expected, and covered by
  # the caption below.
  #=======================================================
  
  scale_y_log10() +
    
    #=======================================================
  # Legends
  #=======================================================
  
  guides(
    
    colour = guide_legend(
      order = 1,
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        shape = 21,
        fill = model_cols,
        size = 2.8,
        linewidth = 0.8
      )
    ),
    
    shape = guide_legend(
      order = 2,
      nrow = 2,
      ncol = 2,
      byrow = TRUE,
      override.aes = list(
        shape = 8,
        colour = "black",
        size = 3
      )
    )
  )+
    
    #=======================================================
  # Theme
  #=======================================================
  
  theme_bw(
    base_size = 13
  ) +
    
    theme(
      
      #-------------------------------------------------------
      # Square panels
      #-------------------------------------------------------
      
      aspect.ratio = 1,
      
      #-------------------------------------------------------
      # Facet titles
      #-------------------------------------------------------
      
      strip.background = element_blank(),
      
      strip.text = element_text(
        hjust = 0,
        face = "bold",
        size = 16
      ),
      
      #-------------------------------------------------------
      # Axis text
      #-------------------------------------------------------
      
      axis.text.x = element_text(
        angle = 30,
        hjust = 1,
        size = 13
      ),
      
      axis.text.y = element_text(
        size = 13
      ),
      
      #-------------------------------------------------------
      # Axis titles
      #-------------------------------------------------------
      
      axis.title.x = element_text(
        size = 14
      ),
      
      axis.title.y = element_text(
        size = 14
      ),
      
      #-------------------------------------------------------
      # Legends
      #-------------------------------------------------------
      
      legend.position = "bottom",
      
      legend.title = element_text(
        face = "plain",
        size = 13
      ),
      
      legend.text = element_text(
        size = 11
      ),
      
      legend.box = "vertical",
      
      legend.justification = "left",
      
      legend.box.just = "left",
      
      legend.spacing.y = unit(
        0,
        "cm"
      ),
      
      legend.margin = margin(
        0,
        0,
        0,
        0
      ),
      
      legend.key.width = unit(
        1.1,
        "cm"
      )
    )+
    
    labs(
      x = NULL,
      y = ""
    )
  
  #=========================================================
  # Print
  #=========================================================
  
  print(p_d3fig1)
  
  #=========================================================
  # Caption
  #=========================================================
  
  p_d3fig1_caption <- paste(
    "D3 - Fig 1. Cod adult: sensitivity of M2/F and Q/Y to Q/B",
    "and diet assumptions across three periods. The y axis is",
    "shown on a log scale, since values span more than one order",
    "of magnitude between 1985-1988/2013-2015 and 2018-2020; a few",
    "points where F, M2, Q or Y is parameterized as zero (so the",
    "ratio is 0 or undefined) are not shown, since they cannot be",
    "represented on a log scale. Baseline is shown",
    "as a dashed black line with a solid white outline, drawn on",
    "top of the sensitivity variants so it remains visible where",
    "lines overlap, while the dashes let the underlying line show",
    "through. Q/B sensitivity variants are shown",
    "using a teal gradient, with darker colours representing",
    "higher Q/B values. Diet variants are shown using an orange",
    "gradient. Filled points represent period means and lines",
    "show the trend across periods. The shaded band around each",
    "line shows the range across the 2018-2020 population-split",
    "scenarios (50/50 and 80/20); at 1985-1988 and 2013-2015,",
    "where only one simulation exists per period, the band",
    "collapses onto the line. Asterisks represent published",
    "estimates in West et al. (2025): M2/F = 1.3 and Q/Y = 1.7",
    "for 1985-1988, and M2/F = 17 and Q/Y = 24 for 2018-2020."
  )
  
  cat(
    "\n",
    p_d3fig1_caption,
    "\n",
    sep = ""
  )
  
  #=========================================================
  # Save
  #=========================================================
  
  ggsave(
    file.path(
      root_dir,
      "D3_Fig1_QBSensitivityTrend_CodAdult.png"
    ),
    p_d3fig1,
    width = 10,
    height = 8,
    dpi = 300
  )
  
  #=========================================================
  # Estimates table (companion to D3 Fig 1)
  #=========================================================
  ## Period means (the plotted line/points) and the underlying raw
  ## per-Model per-Simulation values (qb_trend_long, before the
  ## Period-collapse) as two files -- the raw table lets you see the
  ## individual 50/50 vs. 80/20 values behind the 2018-2020 mean point.
  
  fwrite(
    qb_period_data$mean_line,
    file.path(root_dir, "D3_Fig1_Estimates_PeriodMeans.csv")
  )
  
  fwrite(
    qb_trend_long,
    file.path(root_dir, "D3_Fig1_Estimates_Raw.csv")
  )
  
  #=========================================================
  # D3 - Figure 1b
  # Same as D3 - Fig 1 above, on the original (untransformed)
  # ratio scale -- no log10 transform. Per request: alongside the
  # log-scale version, also produce this figure on a plain linear
  # axis. Same data, same colours, same baseline overlay, same
  # facets, published-estimate asterisks, and uncertainty band;
  # the only difference is the absence of scale_y_log10() below.
  #=========================================================
  
  p_d3fig1_ratio <- ggplot(
    qb_period_data$mean_line,
    aes(
      x = PeriodNum,
      y = Value,
      colour = Model,
      group = Model
    )
  ) +
    
    geom_ribbon(
      data = qb_range,
      aes(
        x = PeriodNum,
        ymin = ymin,
        ymax = ymax,
        fill = Model,
        group = Model
      ),
      colour = NA,
      alpha = 0.22,
      inherit.aes = FALSE
    ) +
    
    geom_line(
      linewidth = 0.75
    ) +
    
    geom_line(
      data = qb_period_data$mean_line[Model == "baseline"],
      linewidth = 2.6,
      colour = "white",
      alpha = 0.6
    ) +
    
    geom_line(
      data = qb_period_data$mean_line[Model == "baseline"],
      linewidth = 1.3,
      colour = "black",
      linetype = "dashed"
    ) +
    
    geom_point(
      aes(
        fill = Model
      ),
      shape = 21,
      colour = "black",
      size = 2.8,
      stroke = 0.45
    ) +
    
    # Published estimates drawn last so the asterisks stay on top
    # of the simulation lines/points -- see the matching fix in
    # D3 Fig 1 above.
    geom_point(
      data = qb_published,
      aes(
        x = as.numeric(Period),
        y = Ratio,
        shape = Published
      ),
      colour = "black",
      size = 3,
      stroke = 0.8,
      inherit.aes = FALSE
    ) +
    
    scale_colour_manual(
      values = model_cols,
      breaks = simulation_order,
      labels = simulation_order,
      name = "Simulations"
    ) +
    
    scale_fill_manual(
      values = model_cols,
      breaks = simulation_order,
      guide = "none"
    ) +
    
    scale_shape_manual(
      values = c(
        "M2/F = 1.3 (1985-1988)" = 8,
        "Q/Y = 1.7 (1985-1988)" = 8,
        "M2/F = 17 (2018-2020)" = 8,
        "Q/Y = 24 (2018-2020)" = 8
      ),
      name = "Published in West et al. (2025)"
    ) +
    
    scale_x_continuous(
      breaks = 1:length(period_levels),
      labels = period_levels
    ) +
    
    facet_wrap(
      ~Metric,
      scales = "free_y",
      ncol = 2
    ) +
    
    # No scale_y_log10() here -- plain linear axis, unlike D3 Fig 1
    # above. Zero-mortality points (F or M2 parameterized as zero)
    # remain visible on this scale instead of being dropped as
    # non-finite, unlike on the log10 version.
    
    guides(
      
      colour = guide_legend(
        order = 1,
        nrow = 1,
        byrow = TRUE,
        override.aes = list(
          shape = 21,
          fill = model_cols,
          size = 2.8,
          linewidth = 0.8
        )
      ),
      
      shape = guide_legend(
        order = 2,
        nrow = 2,
        ncol = 2,
        byrow = TRUE,
        override.aes = list(
          shape = 8,
          colour = "black",
          size = 3
        )
      )
    )+
    
    theme_bw(
      base_size = 13
    ) +
    
    theme(
      
      aspect.ratio = 1,
      
      strip.background = element_blank(),
      
      strip.text = element_text(
        hjust = 0,
        face = "bold",
        size = 16
      ),
      
      axis.text.x = element_text(
        angle = 30,
        hjust = 1,
        size = 13
      ),
      
      axis.text.y = element_text(
        size = 13
      ),
      
      axis.title.x = element_text(
        size = 14
      ),
      
      axis.title.y = element_text(
        size = 14
      ),
      
      legend.position = "bottom",
      
      legend.title = element_text(
        face = "plain",
        size = 13
      ),
      
      legend.text = element_text(
        size = 11
      ),
      
      legend.box = "vertical",
      
      legend.justification = "left",
      
      legend.box.just = "left",
      
      legend.spacing.y = unit(
        0,
        "cm"
      ),
      
      legend.margin = margin(
        0,
        0,
        0,
        0
      ),
      
      legend.key.width = unit(
        1.1,
        "cm"
      )
    )+
    
    labs(
      x = NULL,
      y = ""
    )
  
  print(p_d3fig1_ratio)
  
  p_d3fig1_ratio_caption <- paste(
    "D3 - Fig 1b. Cod adult: sensitivity of M2/F and Q/Y to Q/B",
    "and diet assumptions across three periods, on the original",
    "(untransformed) ratio scale -- the same figure as D3 - Fig 1",
    "above, without the log10 transform. Panels use independent",
    "y-axis ranges (scales = \"free_y\"), so the 2018-2020 values",
    "do not compress the 1985-1988/2013-2015 values the way a",
    "shared linear axis would, but the relative change across",
    "periods is harder to read here than on the log-scale version",
    "when values span multiple orders of magnitude. All other",
    "elements (baseline overlay, uncertainty band, colours,",
    "published asterisks) are identical to D3 - Fig 1."
  )
  
  cat(
    "\n",
    p_d3fig1_ratio_caption,
    "\n",
    sep = ""
  )
  
  ggsave(
    file.path(
      root_dir,
      "D3_Fig1_QBSensitivityTrend_CodAdult_RatioScale.png"
    ),
    p_d3fig1_ratio,
    width = 10,
    height = 8,
    dpi = 300
  )
  
} else {
  
  cat("\nSkipping Figure D3-1 -- `ratio_compare` isn't available.\n")
  
}


#=========================================================
# D3 - Figure 2 and 3
# Multiple groups
#
# Separate figures:
#   1. M2/F
#   2. Q/Y
#
# Facet layout:
#   5 columns x 5 rows
#=========================================================


#=========================================================
# Settings
#=========================================================

period_levels <- c(
  "1985-1988",
  "2013-2015",
  "2018-2020"
)


#=========================================================
# Groups
#=========================================================

## FIXED (per explicit request): same reasoning as D1 Fig 1 above --
## `exploited_species` doesn't require Fishing > 0 and nonzero
## predation to occur in the SAME Simulation/Model, so it could
## include a species with no real M2/F or Q/Y ratio to actually plot.
## `species_with_valid_ratio` requires both simultaneously in at least
## one row, so every species shown across D3 Fig 2/3 below has at
## least one genuine, finite, nonzero ratio somewhere.
group_order <- species_with_valid_ratio


#=========================================================
# Simulation order
#
# Baseline
# Q/B sensitivities
# Diet sensitivities
#=========================================================

simulation_order <- c(
  "baseline",
  "qb-30",
  "qb-15",
  "qb+15",
  "qb+30",
  "diet-20",
  "diet-20pisciv"
)


#=========================================================
# Prepare data
#=========================================================

multi_group_data <- copy(
  ratio_compare
)


#---------------------------------------------------------
# Keep desired groups
#---------------------------------------------------------

multi_group_data <- multi_group_data[
  Group %in% group_order
]


#---------------------------------------------------------
# Keep finite values
#
# Keep a row if either metric is available.
#---------------------------------------------------------

multi_group_data <- multi_group_data[
  is.finite(M2_over_F) |
    is.finite(Q_over_Y)
]


#=========================================================
# Check simulations
#=========================================================

check_all_simulations(
  multi_group_data$Simulation,
  "D3 Fig 1 multiple groups"
)


#=========================================================
# Standardize model names
#
# ratio_compare contains b+15 etc.
# Convert them to the qb names used in the figure.
#=========================================================

multi_group_data[
  Model == "b-30",
  Model := "qb-30"
]

multi_group_data[
  Model == "b-15",
  Model := "qb-15"
]

multi_group_data[
  Model == "b+15",
  Model := "qb+15"
]

multi_group_data[
  Model == "b+30",
  Model := "qb+30"
]


#=========================================================
# Model order
#=========================================================

multi_group_data[
  ,
  Model := factor(
    Model,
    levels = simulation_order
  )
]


#---------------------------------------------------------
# Remove models not in the desired order
#---------------------------------------------------------

multi_group_data <- multi_group_data[
  !is.na(Model)
]


#=========================================================
# Group order
#=========================================================

multi_group_data[
  ,
  Group := factor(
    Group,
    levels = group_order
  )
]


#---------------------------------------------------------
# Remove groups not in group_order
#---------------------------------------------------------

multi_group_data <- multi_group_data[
  !is.na(Group)
]


#=========================================================
# Convert metrics to long format
#=========================================================

multi_group_long <- melt(
  multi_group_data,
  
  id.vars = c(
    "Simulation",
    "Model",
    "Group"
  ),
  
  measure.vars = c(
    "M2_over_F",
    "Q_over_Y"
  ),
  
  variable.name = "Metric",
  value.name = "Ratio"
)


#=========================================================
# Period summaries
#=========================================================

multi_period_data <- build_period_timeseries(
  multi_group_long,
  value_col = "Ratio",
  extra_group_cols = c(
    "Group",
    "Metric"
  )
)


#=========================================================
# Restore factor ordering
#=========================================================

multi_period_data$mean_line[
  ,
  Model := factor(
    Model,
    levels = simulation_order
  )
]

multi_period_data$raw_points[
  ,
  Model := factor(
    Model,
    levels = simulation_order
  )
]


multi_period_data$mean_line[
  ,
  Group := factor(
    Group,
    levels = group_order
  )
]

multi_period_data$raw_points[
  ,
  Group := factor(
    Group,
    levels = group_order
  )
]


#=========================================================
# Published estimates
# West et al. (2025)
#
# Currently available published values are for Cod adult.
#=========================================================

## NOTE: no `Group` column here, deliberately. West et al. (2025) only
## published these four headline ratios for Cod adult -- there is no
## per-FG published dataset anywhere in this script (checked). Per
## review feedback, these headline values are shown as a common
## reference in every facet of D3 Fig 2/3, not as a claim that each
## FG has its own published estimate -- omitting the `Group` column
## is what makes ggplot replicate this layer across every panel of
## `facet_wrap(~Group)` instead of only the Cod adult one (a layer
## whose data lacks the facetting variable is drawn in every panel).
## If per-FG published values become available later, add a `Group`
## column back and this will revert to per-panel matching instead.

qb_published <- data.table(
  
  Period = factor(
    c(
      "1985-1988",
      "1985-1988",
      "2018-2020",
      "2018-2020"
    ),
    levels = period_levels
  ),
  
  Ratio = c(
    1.3,
    1.7,
    17,
    24
  ),
  
  Metric = c(
    "M2_over_F",
    "Q_over_Y",
    "M2_over_F",
    "Q_over_Y"
  ),
  
  Published = c(
    "M2/F = 1.3 (1985-1988)",
    "Q/Y = 1.7 (1985-1988)",
    "M2/F = 17 (2018-2020)",
    "Q/Y = 24 (2018-2020)"
  )
)


#=========================================================
# Model colours
#=========================================================

model_cols <- c(
  
  #-------------------------------------------------------
  # Baseline
  # Black (redrawn thicker and solid, on top of the other lines --
  # see the geom_line overlay below -- rather than distinguished by
  # linetype, which got lost against the teal/orange gradient where
  # lines overlap closely) rather than the previous solid dark
  # grey, so it reads as a distinct reference line rather than just
  # another member of the Q/B/diet gradient.
  #-------------------------------------------------------
  
  "baseline" = "black",
  
  #-------------------------------------------------------
  # Q/B sensitivity
  #-------------------------------------------------------
  
  "qb-30" = "#A6DAD3",
  "qb-15" = "#66C2B7",
  "qb+15" = "#1F9E89",
  "qb+30" = "#006D5B",
  
  #-------------------------------------------------------
  # Diet sensitivity
  #-------------------------------------------------------
  
  "diet-20" = "#FDD49E",
  "diet-20pisciv" = "#E66101"
)


# Reorder colours to match simulation order

model_cols <- model_cols[
  simulation_order
]


## common_theme now defined once near the top of this script
## (Settings section) -- removed the duplicate definition that used
## to be here (identical content, just redundant).

#=========================================================
# M2/F DATA
#=========================================================

m2f_data <- multi_period_data$mean_line[
  Metric == "M2_over_F"
]

m2f_data[
  ,
  PeriodNum := as.numeric(Period)
]

m2f_raw <- multi_period_data$raw_points[
  Metric == "M2_over_F"
]

m2f_published <- qb_published[
  Metric == "M2_over_F"
]

## Uncertainty band (replaces the old jittered raw-point layer, per
## review feedback: hollow overlapping points at 2018-2020 were hard
## to read once several Model colours stacked up in the same facet;
## a semi-transparent ribbon reads more clearly as "spread", especially
## now that these panels are log-scaled). `raw_points` only carries the
## two 2018-2020 population-split composites (see
## build_period_timeseries), so min/max is only non-degenerate at that
## period; 1985-1988 and 2013-2015 have a single simulation each, and
## fall back to the mean (ymin = ymax = Value), so the ribbon tapers to
## the trend line there rather than implying spread that isn't in the
## data.

m2f_raw_range <- m2f_raw[
  ,
  .(
    ymin = min(Value, na.rm = TRUE),
    ymax = max(Value, na.rm = TRUE)
  ),
  by = .(Group, Model, Period)
]

m2f_range <- merge(
  m2f_data[, .(Group, Model, Period, PeriodNum, Value)],
  m2f_raw_range,
  by = c("Group", "Model", "Period"),
  all.x = TRUE
)

m2f_range[is.na(ymin), ymin := Value]
m2f_range[is.na(ymax), ymax := Value]


#=========================================================
# Plot log(ratio + 1) directly, not via a scale transform
#=========================================================
## Per request: plot the actual log(ratio + 1) values on a plain
## linear axis, rather than using scale_y_continuous(trans = "log1p")
## (which transforms the axis but re-labels the breaks back in
## original-ratio units). Transforming the data itself here means the
## axis just shows plain log(ratio + 1) numbers -- no back-transformed
## breaks.
##
## IMPORTANT: this adds NEW columns for plotting rather than
## overwriting Value/ymin/ymax/Ratio in place -- m2f_data is also
## fwrite()'d as-is to "D3_Fig2_Estimates.csv" below (the companion
## estimates table for this figure), which needs to keep the real
## M2/F ratio, not the log-transformed plotting value. log1p is
## monotonic, so it doesn't matter whether this happens before or
## after computing m2f_range's min/max -- transforming the
## already-computed ymin/ymax gives the same result as taking
## min/max of already-transformed values.

m2f_data[, ValuePlot := log1p(Value)]
m2f_range[, `:=`(
  yminPlot = log1p(ymin),
  ymaxPlot = log1p(ymax)
)]
m2f_published[, RatioPlot := log1p(Ratio)]


#=========================================================
# M2/F PLOT
#=========================================================

p_m2f <- ggplot(
  m2f_data,
  aes(
    x = PeriodNum,
    y = ValuePlot,
    colour = Model,
    group = Model
  )
) +
  
  #-------------------------------------------------------
# 1. Uncertainty band (min-max across 2018-2020 population-split
# composites; degenerates to the trend line where only one
# simulation exists)
#-------------------------------------------------------

geom_ribbon(
  data = m2f_range,
  aes(
    x = PeriodNum,
    ymin = yminPlot,
    ymax = ymaxPlot,
    fill = Model,
    group = Model
  ),
  colour = NA,
  alpha = 0.22,
  inherit.aes = FALSE
) +
  
  #-------------------------------------------------------
# 2. Simulation trends
#-------------------------------------------------------

geom_line(
  linewidth = 0.75
) +
  
  #-------------------------------------------------------
# 2b. Baseline overlay
# Dashed lines alone still got lost against the teal/orange
# gradient where they overlap the baseline closely (per review
# feedback), and a plain thicker black line still blended in
# wherever it crossed a dark teal/orange segment. A "halo" -- a
# wider white stroke drawn first, black on top -- makes baseline
# visually punch through any colour it crosses. The white halo
# stays solid (dashing both strokes made them read as two
# misaligned dashed lines, since ggplot/grid scales a linetype's
# dash length by the line's own linewidth, so a 2.6pt white dash
# and a 1.3pt black dash never line up). Only the black line on top
# is dashed, so whatever is directly behind baseline shows through
# the gaps in the black dashes, while the solid white halo still
# makes baseline pop against the sensitivity colours.
#-------------------------------------------------------

geom_line(
  data = m2f_data[Model == "baseline"],
  linewidth = 2.6,
  colour = "white",
  alpha = 0.6
) +
  
  geom_line(
    data = m2f_data[Model == "baseline"],
    linewidth = 1.3,
    colour = "black",
    linetype = "dashed"
  ) +
  
  #-------------------------------------------------------
# 3. Mean simulation estimates
#-------------------------------------------------------

geom_point(
  aes(
    fill = Model
  ),
  shape = 21,
  colour = "black",
  size = 2.8,
  stroke = 0.45
) +
  
  #-------------------------------------------------------
# 4. Published estimates
# FIXED: moved from layer 2 to last -- drawn earlier, the
# asterisk markers could be hidden under a simulation trend line,
# the baseline overlay, or a mean-estimate point landing at the
# same spot. Drawing them last puts the published-value asterisks
# on top of every other layer, so they stay visible.
#-------------------------------------------------------

geom_point(
  data = m2f_published,
  aes(
    x = as.numeric(Period),
    y = RatioPlot,
    shape = Published
  ),
  colour = "black",
  size = 3,
  stroke = 0.8,
  inherit.aes = FALSE
) +
  
  #=======================================================
# Simulation colours
#=======================================================

scale_colour_manual(
  values = model_cols,
  breaks = simulation_order,
  labels = simulation_order,
  name = "Simulations"
) +
  
  scale_fill_manual(
    values = model_cols,
    breaks = simulation_order,
    guide = "none"
  ) +
  
  #=======================================================
# Published estimates legend
#=======================================================

scale_shape_manual(
  values = c(
    "M2/F = 1.3 (1985-1988)" = 8,
    "M2/F = 17 (2018-2020)" = 8
  ),
  name = "Published in West et al. (2025)"
) +
  
  #=======================================================
# X axis
#=======================================================

scale_x_continuous(
  breaks = 1:length(period_levels),
  labels = period_levels
) +
  
  #=======================================================
# 5 x 5 facet layout
#=======================================================

facet_wrap(
  ~Group,
  scales = "free_y",
  ncol = 5,
  nrow = 5
) +
  
  #=======================================================
# Legends
#=======================================================

guides(
  
  colour = guide_legend(
    order = 1,
    nrow = 1,
    byrow = TRUE,
    override.aes = list(
      shape = 21,
      fill = model_cols,
      size = 2.8,
      linewidth = 0.8
    )
  ),
  
  shape = guide_legend(
    order = 2,
    nrow = 1,
    byrow = TRUE,
    override.aes = list(
      shape = 8,
      colour = "black",
      size = 3
    )
  )
) +
  
  #=======================================================
# No scale transform here -- the data itself is already
# log(ratio + 1) (see the block right before this plot's
# ggplot() call). This is a plain linear axis over already-
# transformed values, so breaks/labels are NOT converted back to
# original-ratio units. Several panels (e.g. Shrimp, Cod
# juvenile) span multiple orders of magnitude under
# scales = "free_y"; log(ratio + 1) compresses the extreme high
# values the way a log axis would, but -- unlike plain log10, which
# treats a ratio of 0 (F or M2 parameterized as zero) as non-finite
# and drops it -- log1p maps a ratio of 0 to a finite, plottable 0,
# so zero-mortality points stay visible instead of disappearing.
#=======================================================

common_theme +
  
  labs(
    x = NULL,
    y = "log(M2/F + 1)"
  )


#=========================================================
# Print M2/F
#=========================================================

print(p_m2f)


#=========================================================
# Q/Y DATA
#=========================================================

qy_data <- multi_period_data$mean_line[
  Metric == "Q_over_Y"
]

qy_data[
  ,
  PeriodNum := as.numeric(Period)
]

qy_raw <- multi_period_data$raw_points[
  Metric == "Q_over_Y"
]

qy_published <- qb_published[
  Metric == "Q_over_Y"
]

## Uncertainty band -- see the matching comment on m2f_range above.

qy_raw_range <- qy_raw[
  ,
  .(
    ymin = min(Value, na.rm = TRUE),
    ymax = max(Value, na.rm = TRUE)
  ),
  by = .(Group, Model, Period)
]

qy_range <- merge(
  qy_data[, .(Group, Model, Period, PeriodNum, Value)],
  qy_raw_range,
  by = c("Group", "Model", "Period"),
  all.x = TRUE
)

qy_range[is.na(ymin), ymin := Value]
qy_range[is.na(ymax), ymax := Value]


#=========================================================
# Plot log(ratio + 1) directly, not via a scale transform
# (see the matching comment in the M2/F plot above -- new
# columns, not overwritten in place, since qy_data is also
# fwrite()'d as-is to "D3_Fig3_Estimates.csv" below)
#=========================================================

qy_data[, ValuePlot := log1p(Value)]
qy_range[, `:=`(
  yminPlot = log1p(ymin),
  ymaxPlot = log1p(ymax)
)]
qy_published[, RatioPlot := log1p(Ratio)]


#=========================================================
# Q/Y PLOT
#=========================================================

p_qy <- ggplot(
  qy_data,
  aes(
    x = PeriodNum,
    y = ValuePlot,
    colour = Model,
    group = Model
  )
) +
  
  #-------------------------------------------------------
# 1. Uncertainty band (min-max across 2018-2020 population-split
# composites; degenerates to the trend line where only one
# simulation exists)
#-------------------------------------------------------

geom_ribbon(
  data = qy_range,
  aes(
    x = PeriodNum,
    ymin = yminPlot,
    ymax = ymaxPlot,
    fill = Model,
    group = Model
  ),
  colour = NA,
  alpha = 0.22,
  inherit.aes = FALSE
) +
  
  #-------------------------------------------------------
# 2. Simulation trends
#-------------------------------------------------------

geom_line(
  linewidth = 0.75
) +
  
  #-------------------------------------------------------
# 2b. Baseline overlay -- "halo" effect, see the matching comment
# in the M2/F plot above
#-------------------------------------------------------

geom_line(
  data = qy_data[Model == "baseline"],
  linewidth = 2.6,
  colour = "white",
  alpha = 0.6
) +
  
  geom_line(
    data = qy_data[Model == "baseline"],
    linewidth = 1.3,
    colour = "black",
    linetype = "dashed"
  ) +
  
  #-------------------------------------------------------
# 3. Mean simulation estimates
#-------------------------------------------------------

geom_point(
  aes(
    fill = Model
  ),
  shape = 21,
  colour = "black",
  size = 2.8,
  stroke = 0.45
) +
  
  #-------------------------------------------------------
# 4. Published estimates
# FIXED: moved from layer 2 to last -- drawn earlier, the
# asterisk markers could be hidden under a simulation trend line,
# the baseline overlay, or a mean-estimate point landing at the
# same spot. Drawing them last puts the published-value asterisks
# on top of every other layer, so they stay visible.
#-------------------------------------------------------

geom_point(
  data = qy_published,
  aes(
    x = as.numeric(Period),
    y = RatioPlot,
    shape = Published
  ),
  colour = "black",
  size = 3,
  stroke = 0.8,
  inherit.aes = FALSE
) +
  
  #=======================================================
# Simulation colours
#=======================================================

scale_colour_manual(
  values = model_cols,
  breaks = simulation_order,
  labels = simulation_order,
  name = "Simulations"
) +
  
  scale_fill_manual(
    values = model_cols,
    breaks = simulation_order,
    guide = "none"
  ) +
  
  #=======================================================
# Published estimates legend
#=======================================================

scale_shape_manual(
  values = c(
    "Q/Y = 1.7 (1985-1988)" = 8,
    "Q/Y = 24 (2018-2020)" = 8
  ),
  name = "Published in West et al. (2025)"
) +
  
  #=======================================================
# X axis
#=======================================================

scale_x_continuous(
  breaks = 1:length(period_levels),
  labels = period_levels
) +
  
  #=======================================================
# 5 x 5 facet layout
#=======================================================

facet_wrap(
  ~Group,
  scales = "free_y",
  ncol = 5,
  nrow = 5
) +
  
  #=======================================================
# Legends
#=======================================================

guides(
  
  colour = guide_legend(
    order = 1,
    nrow = 1,
    byrow = TRUE,
    override.aes = list(
      shape = 21,
      fill = model_cols,
      size = 2.8,
      linewidth = 0.8
    )
  ),
  
  shape = guide_legend(
    order = 2,
    nrow = 1,
    byrow = TRUE,
    override.aes = list(
      shape = 8,
      colour = "black",
      size = 3
    )
  )
) +
  
  #=======================================================
# No scale transform -- see the matching comment in the M2/F
# plot above. The data is already log(ratio + 1); this is a
# plain linear axis over it.
#=======================================================

common_theme +
  
  labs(
    x = NULL,
    y = "log(Q/Y + 1)"
  )


#=========================================================
# Print Q/Y
#=========================================================

print(p_qy)


#=========================================================
# Figure dimensions
#=========================================================

fig_width <- 20
fig_height <- 12


#=========================================================
# Save M2/F
#=========================================================

ggsave(
  file.path(
    root_dir,
    "D3_Fig2_M2F_MultipleGroups.png"
  ),
  p_m2f,
  width = fig_width,
  height = fig_height,
  dpi = 300
)

#=========================================================
# Estimates table (companion to D3 Fig 2)
#=========================================================

fwrite(
  m2f_data,
  file.path(root_dir, "D3_Fig2_Estimates.csv")
)


#=========================================================
# Save Q/Y
#=========================================================

ggsave(
  file.path(
    root_dir,
    "D3_Fig3_QY_MultipleGroups.png"
  ),
  p_qy,
  width = fig_width,
  height = fig_height,
  dpi = 300
)

#=========================================================
# Estimates table (companion to D3 Fig 3)
#=========================================================

fwrite(
  qy_data,
  file.path(root_dir, "D3_Fig3_Estimates.csv")
)


#=========================================================
# Captions
#=========================================================

p_m2f_caption <- paste(
  "D3 - Fig 2. M2/F across exploited seal prey species and",
  "sensitivity simulations, plotted as log(M2/F + 1) on a linear",
  "axis, to accommodate panels spanning multiple orders of",
  "magnitude (e.g., Shrimp, Cod juvenile) while still keeping a",
  "ratio of 0 visible, rather than dropping it as a plain log",
  "scale would. Axis values are log(ratio + 1), not the original",
  "M2/F ratio. Baseline is shown as a dashed",
  "black line with a solid white outline, drawn on top of the",
  "sensitivity variants so it remains visible where lines",
  "overlap, while the dashes let the underlying line show. Q/B",
  "sensitivity variants are shown using a teal",
  "gradient, with darker colours representing higher Q/B",
  "values. Diet variants are shown using an orange gradient.",
  "Filled points represent period means and lines show the",
  "trend across periods. The shaded band around each line",
  "shows the range across the 2018-2020 population-split",
  "scenarios (50/50 and 80/20); at 1985-1988 and 2013-2015,",
  "where only one simulation exists per period, the band",
  "collapses onto the line. Asterisks show West et al.'s (2025)",
  "published Cod adult headline ratios, repeated identically in",
  "every panel as a common reference point, not as a claim that",
  "each FG has its own published estimate; West et al. did not",
  "report per-FG values. Where the underlying EwE model",
  "parameterized predation (M2) or fishing mortality (F) as",
  "zero for that group and simulation, the ratio is zero and",
  "the point sits at the bottom of the axis rather than being",
  "dropped, since log(ratio + 1) maps a ratio of 0 to a finite",
  "value; a ratio that is genuinely undefined (F reported as",
  "missing rather than zero) still leaves a gap in the line."
)

p_qy_caption <- paste(
  "D3 - Fig 3. Q/Y across exploited seal prey species and",
  "sensitivity simulations, plotted as log(Q/Y + 1) on a linear",
  "axis, to accommodate panels spanning multiple orders of",
  "magnitude (e.g., Shrimp, Cod juvenile) while still keeping a",
  "ratio of 0 visible, rather than dropping it as a plain log",
  "scale would. Axis values are log(ratio + 1), not the original",
  "Q/Y ratio. Baseline is shown as a dashed",
  "black line with a solid white outline, drawn on top of the",
  "sensitivity variants so it remains visible where lines",
  "overlap, while the dashes let the underlying line show. Q/B",
  "sensitivity variants are shown using a teal",
  "gradient, with darker colours representing higher Q/B",
  "values. Diet variants are shown using an orange gradient.",
  "Filled points represent period means and lines show the",
  "trend across periods. The shaded band around each line",
  "shows the range across the 2018-2020 population-split",
  "scenarios (50/50 and 80/20); at 1985-1988 and 2013-2015,",
  "where only one simulation exists per period, the band",
  "collapses onto the line. Asterisks show West et al.'s (2025)",
  "published Cod adult headline ratios, repeated identically in",
  "every panel as a common reference point, not as a claim that",
  "each FG has its own published estimate; West et al. did not",
  "report per-FG values. Where the underlying EwE model",
  "parameterized consumption (Q) or yield (Y) as zero for",
  "that group and simulation, the ratio is zero and the point",
  "sits at the bottom of the axis rather than being dropped,",
  "since log(ratio + 1) maps a ratio of 0 to a finite value; a",
  "ratio that is genuinely undefined (Y reported as missing",
  "rather than zero) still leaves a gap in the line."
)

cat(
  "\n",
  p_m2f_caption,
  "\n",
  sep = ""
)

cat(
  "\n",
  p_qy_caption,
  "\n",
  sep = ""
)

#=========================================================
# M2/F and Q/Y, plain ratio scale (companions to the
# log(ratio + 1) figures above)
#=========================================================
## Per request: alongside the log(M2/F + 1) / log(Q/Y + 1) figures
## above, also produce the same two figures on the original ratio
## scale (no transform at all) -- same data, same colours, same
## baseline overlay, same facets, just y = the real Value/ymin/ymax/
## Ratio columns (still present on m2f_data/m2f_range/m2f_published
## and qy_data/qy_range/qy_published; only the *Plot columns above are
## log-transformed, these were never touched).

p_m2f_ratio <- ggplot(
  m2f_data,
  aes(
    x = PeriodNum,
    y = Value,
    colour = Model,
    group = Model
  )
) +
  
  geom_ribbon(
    data = m2f_range,
    aes(
      x = PeriodNum,
      ymin = ymin,
      ymax = ymax,
      fill = Model,
      group = Model
    ),
    colour = NA,
    alpha = 0.22,
    inherit.aes = FALSE
  ) +
  
  geom_line(
    linewidth = 0.75
  ) +
  
  geom_line(
    data = m2f_data[Model == "baseline"],
    linewidth = 2.6,
    colour = "white",
    alpha = 0.6
  ) +
  
  geom_line(
    data = m2f_data[Model == "baseline"],
    linewidth = 1.3,
    colour = "black",
    linetype = "dashed"
  ) +
  
  geom_point(
    aes(
      fill = Model
    ),
    shape = 21,
    colour = "black",
    size = 2.8,
    stroke = 0.45
  ) +
  
  # FIXED: moved from layer 2 (right after the ribbon) to last --
  # drawn earlier, the asterisk markers could be hidden under a
  # simulation trend line, the baseline overlay, or a mean-estimate
  # point landing at the same spot. Drawing them last puts the
  # published-value asterisks on top of every other layer.
  geom_point(
    data = m2f_published,
    aes(
      x = as.numeric(Period),
      y = Ratio,
      shape = Published
    ),
    colour = "black",
    size = 3,
    stroke = 0.8,
    inherit.aes = FALSE
  ) +
  
  scale_colour_manual(
    values = model_cols,
    breaks = simulation_order,
    labels = simulation_order,
    name = "Simulations"
  ) +
  
  scale_fill_manual(
    values = model_cols,
    breaks = simulation_order,
    guide = "none"
  ) +
  
  scale_shape_manual(
    values = c(
      "M2/F = 1.3 (1985-1988)" = 8,
      "M2/F = 17 (2018-2020)" = 8
    ),
    name = "Published in West et al. (2025)"
  ) +
  
  scale_x_continuous(
    breaks = 1:length(period_levels),
    labels = period_levels
  ) +
  
  facet_wrap(
    ~Group,
    scales = "free_y",
    ncol = 5,
    nrow = 5
  ) +
  
  guides(
    
    colour = guide_legend(
      order = 1,
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        shape = 21,
        fill = model_cols,
        size = 2.8,
        linewidth = 0.8
      )
    ),
    
    shape = guide_legend(
      order = 2,
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        shape = 8,
        colour = "black",
        size = 3
      )
    )
  ) +
  
  common_theme +
  
  labs(
    x = NULL,
    y = "M2 / F"
  )

print(p_m2f_ratio)

p_m2f_ratio_caption <- paste(
  "D3 - Fig 2b. M2/F across exploited seal prey species and",
  "sensitivity simulations, on the original (untransformed) ratio",
  "scale -- the same figure as D3 - Fig 2 above, without the",
  "log(M2/F + 1) transform. Panels use independent y-axis ranges",
  "(scales = \"free_y\"), so extreme values in one panel (e.g.,",
  "Shrimp, Cod juvenile) do not compress the scale in others, but",
  "the shape of the trend within a panel spanning multiple orders",
  "of magnitude is harder to read here than on the log(M2/F + 1)",
  "version. All other elements (baseline overlay, uncertainty",
  "band, colours, published asterisks) are identical to D3 - Fig 2."
)

cat(
  "\n",
  p_m2f_ratio_caption,
  "\n",
  sep = ""
)

ggsave(
  file.path(
    root_dir,
    "D3_Fig2_M2F_MultipleGroups_RatioScale.png"
  ),
  p_m2f_ratio,
  width = fig_width,
  height = fig_height,
  dpi = 300
)

p_qy_ratio <- ggplot(
  qy_data,
  aes(
    x = PeriodNum,
    y = Value,
    colour = Model,
    group = Model
  )
) +
  
  geom_ribbon(
    data = qy_range,
    aes(
      x = PeriodNum,
      ymin = ymin,
      ymax = ymax,
      fill = Model,
      group = Model
    ),
    colour = NA,
    alpha = 0.22,
    inherit.aes = FALSE
  ) +
  
  geom_line(
    linewidth = 0.75
  ) +
  
  geom_line(
    data = qy_data[Model == "baseline"],
    linewidth = 2.6,
    colour = "white",
    alpha = 0.6
  ) +
  
  geom_line(
    data = qy_data[Model == "baseline"],
    linewidth = 1.3,
    colour = "black",
    linetype = "dashed"
  ) +
  
  geom_point(
    aes(
      fill = Model
    ),
    shape = 21,
    colour = "black",
    size = 2.8,
    stroke = 0.45
  ) +
  
  # FIXED: moved from layer 2 (right after the ribbon) to last --
  # drawn earlier, the asterisk markers could be hidden under a
  # simulation trend line, the baseline overlay, or a mean-estimate
  # point landing at the same spot. Drawing them last puts the
  # published-value asterisks on top of every other layer.
  geom_point(
    data = qy_published,
    aes(
      x = as.numeric(Period),
      y = Ratio,
      shape = Published
    ),
    colour = "black",
    size = 3,
    stroke = 0.8,
    inherit.aes = FALSE
  ) +
  
  scale_colour_manual(
    values = model_cols,
    breaks = simulation_order,
    labels = simulation_order,
    name = "Simulations"
  ) +
  
  scale_fill_manual(
    values = model_cols,
    breaks = simulation_order,
    guide = "none"
  ) +
  
  scale_shape_manual(
    values = c(
      "Q/Y = 1.7 (1985-1988)" = 8,
      "Q/Y = 24 (2018-2020)" = 8
    ),
    name = "Published in West et al. (2025)"
  ) +
  
  scale_x_continuous(
    breaks = 1:length(period_levels),
    labels = period_levels
  ) +
  
  facet_wrap(
    ~Group,
    scales = "free_y",
    ncol = 5,
    nrow = 5
  ) +
  
  guides(
    
    colour = guide_legend(
      order = 1,
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        shape = 21,
        fill = model_cols,
        size = 2.8,
        linewidth = 0.8
      )
    ),
    
    shape = guide_legend(
      order = 2,
      nrow = 1,
      byrow = TRUE,
      override.aes = list(
        shape = 8,
        colour = "black",
        size = 3
      )
    )
  ) +
  
  common_theme +
  
  labs(
    x = NULL,
    y = "Q / Y"
  )

print(p_qy_ratio)

p_qy_ratio_caption <- paste(
  "D3 - Fig 3b. Q/Y across exploited seal prey species and",
  "sensitivity simulations, on the original (untransformed) ratio",
  "scale -- the same figure as D3 - Fig 3 above, without the",
  "log(Q/Y + 1) transform. Panels use independent y-axis ranges",
  "(scales = \"free_y\"), so extreme values in one panel (e.g.,",
  "Shrimp, Cod juvenile) do not compress the scale in others, but",
  "the shape of the trend within a panel spanning multiple orders",
  "of magnitude is harder to read here than on the log(Q/Y + 1)",
  "version. All other elements (baseline overlay, uncertainty",
  "band, colours, published asterisks) are identical to D3 - Fig 3."
)

cat(
  "\n",
  p_qy_ratio_caption,
  "\n",
  sep = ""
)

ggsave(
  file.path(
    root_dir,
    "D3_Fig3_QY_MultipleGroups_RatioScale.png"
  ),
  p_qy_ratio,
  width = fig_width,
  height = fig_height,
  dpi = 300
)

# #=========================================================
# # Figure D3-2: biomass sensitivity trend for Cod adult (SOW deliverable 3)
# #=========================================================
# ## Mirrors D3 Fig 1 (Q/B sensitivity) but for the NEW b-30/b-15/b+15/
# ## b+30 standing-biomass sensitivity Models, using the authoritative
# ## harp seal biomass values from HarpSeal_biomass.csv. This is what
# ## finally fulfills the SOW's "biomass" pedigree-uncertainty axis --
# ## previously flagged throughout this script as not yet evaluated.
# 
# if(exists("ratio_compare") && nrow(ratio_compare) > 0){
#   
#   biomass_axis_order <- c("b-30", "b-15", "baseline", "b+15", "b+30")
#   
#   m2f_cod_biomass <- results[
#     Group == "Cod adult" & Model %in% biomass_axis_order,
#     .(M2_over_F = mean(Predator_vs_Fishing, na.rm = TRUE)),
#     by = .(Simulation = as.character(Simulation), Model = as.character(Model))
#   ]
#   
#   qy_cod_biomass <- qy_results[
#     Group == "Cod adult" & Model %in% biomass_axis_order,
#     .(Q_over_Y = mean(Q_over_Y, na.rm = TRUE)),
#     by = .(Simulation, Model)
#   ]
#   
#   biomass_trend <- merge(
#     m2f_cod_biomass, qy_cod_biomass,
#     by = c("Simulation", "Model"),
#     all = TRUE
#   )
#   
#   if(nrow(biomass_trend) > 0){
#     check_all_simulations(biomass_trend$Simulation, "D3 Fig 2 (biomass sensitivity trend)")
#   }
#   
#   cat("\nD3 Fig 2 raw data (Cod adult, biomass sensitivity models):\n")
#   print(biomass_trend[order(Simulation, Model)])
#   
#   if(nrow(biomass_trend) == 0){
#     
#     cat(
#       "\nSkipping Figure D3-2 -- no Cod adult rows found across the",
#       "biomass sensitivity models. Check that b-30/b-15/baseline/b+15/",
#       "b+30 all ran successfully and HarpSeal_biomass.csv covers them.\n"
#     )
#     
#   } else {
#     
#     biomass_trend[, Model := factor(Model, levels = biomass_axis_order)]
#     
#     biomass_trend_long <- melt(
#       biomass_trend,
#       id.vars = c("Simulation", "Model"),
#       measure.vars = c("M2_over_F", "Q_over_Y"),
#       variable.name = "Metric",
#       value.name = "Ratio"
#     )
#     
#     biomass_trend_long[, Metric := factor(
#       Metric,
#       levels = c("M2_over_F", "Q_over_Y"),
#       labels = c("M2 / F", "Q / Y")
#     )]
#     
#     biomass_reference_lines <- data.table(
#       Metric = factor(c("M2 / F", "Q / Y"), levels = levels(biomass_trend_long$Metric)),
#       RefValue = c(17, 24)
#     )
#     
#     p13b <- ggplot(
#       biomass_trend_long,
#       aes(x = Model, y = Ratio, colour = Simulation, group = Simulation)
#     ) +
#       geom_hline(
#         data = biomass_reference_lines,
#         aes(yintercept = RefValue),
#         linetype = "dashed",
#         colour = "#FFB000",
#         linewidth = 0.9
#       ) +
#       geom_line(linewidth = 0.8) +
#       geom_point(size = 2.5) +
#       facet_wrap(~Metric, scales = "free_y") +
#       scale_colour_manual(values = simulation_colors) +
#       theme_bw(base_size = 13) +
#       theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#       labs(
#         x = "Biomass sensitivity variant (ordered low \u2192 high)",
#         y = "Ratio value"
#       )
#     
#     print(p13b)
#     
#     p13b_caption <- paste(
#       "D3 - Fig 4. Cod adult: sensitivity of M2/F and Q/Y to harp seal",
#       "standing-biomass assumptions (\u00b115%, \u00b130%; ordered low to high),",
#       "using the authoritative biomass values from HarpSeal_biomass.csv.",
#       "Flat lines = robust to biomass uncertainty. Dashed gold line =",
#       "West et al. (2025)'s published headline (17x / 24x). NOTE: at the",
#       "same percentage range, this trend is expected to closely match the",
#       "Q/B sensitivity trend (D3 Fig 1) -- Ecopath's predation mortality",
#       "depends only on the product Q/B x Biomass, not on which factor",
#       "changed, so identical percentage perturbations to either produce",
#       "the same downstream effect. See the qb-XX vs b-XX diagnostic",
#       "printed earlier in this script for the full explanation."
#     )
#     cat("\n", p13b_caption, "\n", sep = "")
#     
#     ggsave(
#       file.path(root_dir, "D3_Fig4_BiomassSensitivityTrend_CodAdult.png"),
#       p13b,
#       width = 10,
#       height = 6,
#       dpi = 300
#     )
#     
#   }
#   
# } else {
#   
#   cat("\nSkipping Figure D3-2 -- `ratio_compare` isn't available.\n")
#   
# }

# #=========================================================
# # Figure 13: species-level summary by category (SOW deliverable 1)
# #=========================================================
# ## Turns `species_level_summary` (already saved as a csv earlier) into
# ## a plot -- the SOW explicitly asks for species-level results broken
# ## out by "seal prey species" and "exploited seal prey species", and a
# ## table alone doesn't make the comparison across species easy to see
# ## at a glance the way a bar chart does.
# 
# if(exists("species_level_summary") && nrow(species_level_summary) > 0){
#   
#   species_summary_long <- melt(
#     species_level_summary,
#     id.vars = c("Group", "Category"),
#     measure.vars = c("Mean_M2_over_F", "Mean_Q_over_Y"),
#     variable.name = "Metric",
#     value.name = "Ratio"
#   )
#   
#   species_summary_long[, Metric := factor(
#     Metric,
#     levels = c("Mean_M2_over_F", "Mean_Q_over_Y"),
#     labels = c("Mean M2 / F", "Mean Q / Y")
#   )]
#   
#   ## Facet-specific reference line, same convention as Figure 12: 17x
#   ## for the M2/F panel, 24x for the Q/Y panel. Note this is drawn as a
#   ## geom_hline BEFORE coord_flip() below, so it ends up as a VERTICAL
#   ## line after the flip (a horizontal reference on the ratio axis).
#   
#   species_reference_lines <- data.table(
#     Metric = factor(
#       c("Mean M2 / F", "Mean Q / Y"),
#       levels = levels(species_summary_long$Metric)
#     ),
#     RefValue = c(17, 24)
#   )
#   
#   ## Ratio axis capped at 50 via coord_flip(ylim=...) -- some species'
#   ## mean ratios run into the hundreds/thousands, which previously
#   ## squashed every other bar down to an unreadable sliver on a shared
#   ## linear scale. coord_flip's ylim CLIPS the view rather than
#   ## rescaling the data, so bars beyond 50 get cut off at the edge --
#   ## the actual value is still shown via the printed label (added
#   ## below) at a fixed position near the axis start, so it's readable
#   ## even for a bar that's entirely clipped off-view.
#   
#   species_summary_long[, LabelPos := 1]
#   
#   n_clipped_d1f6 <- sum(species_summary_long$Ratio > 50, na.rm = TRUE)
#   if(n_clipped_d1f6 > 0){
#     cat(
#       "\nD1 Fig 6: ", n_clipped_d1f6, " bar(s) exceed the fixed 0-50",
#       " axis limit and will be visually clipped (their printed number",
#       " label still shows the true value).\n", sep = ""
#     )
#   }
#   
#   p13 <- ggplot(
#     species_summary_long[!is.na(Ratio)],
#     aes(x = reorder(Group, Ratio), y = Ratio, fill = Category)
#   ) +
#     geom_col() +
#     geom_text(
#       aes(y = LabelPos, label = sprintf("%.1f", Ratio)),
#       colour = "black",
#       size = 3,
#       hjust = 0
#     ) +
#     geom_hline(
#       data = species_reference_lines,
#       aes(yintercept = RefValue),
#       linetype = "dashed",
#       colour = "#FFB000",
#       linewidth = 0.9
#     ) +
#     coord_flip(ylim = c(0, 50)) +
#     facet_wrap(~Metric) +
#     theme_bw(base_size = 13) +
#     labs(
#       x = NULL,
#       y = "Mean ratio across all models/periods currently run (axis capped at 50; labels show true values)",
#       fill = NULL
#     ) +
#     theme(legend.position = "bottom")
#   
#   print(p13)
#   
#   ## Caption (for the report -- not rendered in the plot itself)
#   p13_caption <- paste(
#     "D1 - Fig 6. Species-level summary: seal prey species vs. exploited",
#     "seal prey species. Axis capped at 50 for readability -- bars",
#     "exceeding that are clipped but labeled with their true value.",
#     "Dashed gold line = West et al. (2025)'s published headline",
#     "(17x / 24x)."
#   )
#   cat("\n", p13_caption, "\n", sep = "")
#   
#   ggsave(
#     file.path(root_dir, "D1_Fig6_SpeciesLevel_SealPrey_vs_Exploited.png"),
#     p13,
#     width = 9,
#     height = 6,
#     dpi = 300
#   )
#   
#   cat(
#     "\nFigure D1-6 will look sparse until Mean_Q_over_Y is populated for",
#     "species beyond Cod adult (needs the Diet composition file) --",
#     "same open item as Figures 8, 9 and 12.\n"
#   )
#   
# } else {
#   
#   cat(
#     "\nSkipping Figure 13 -- `species_level_summary` isn't available",
#     "(the Q/Y block above didn't complete successfully).\n"
#   )
#   
# }