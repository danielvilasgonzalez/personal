## ============================================================
## VALIDATION + DIAGNOSTIC SCRIPT for the RRMSE/CV corrigendum
##
## Purpose: confirm which intermediate outputs from r9_plot_estimates_v2.R
## are already cached on disk, skip the expensive per-sim raw-file loops
## entirely, and run the old-formula-vs-corrected-formula RRMSE diagnostics
## directly from the cached objects.
##
## Run this from the SAME working directory as r9_plot_estimates_v2.R
## (the folder that contains output/, data/, figures/, tables/).
## ============================================================

library(data.table)

cat("Working directory:", getwd(), "\n\n")

## ---- STEP 0: confirm the cached files exist before loading anything ----
needed_files <- c(
  "output/survey performance/estimated_index_hist.RData",
  "output/survey performance/estimated_cvsim_hist.RData",
  "output/survey performance/true_ind_hist.RData",
  "output/survey performance/rrmse_cv_hist.RData",
  "output/survey performance/estimated_index_proj.RData",
  "output/survey performance/cvsim_proj.RData",
  "output/survey performance/true_ind_proj.RData",
  "output/survey performance/rrmse_cv_proj.RData"
)

status <- data.frame(
  file     = needed_files,
  exists   = file.exists(needed_files),
  size_MB  = ifelse(file.exists(needed_files), round(file.size(needed_files) / 1e6, 1), NA),
  modified = ifelse(file.exists(needed_files), as.character(file.mtime(needed_files)), NA)
)
print(status)

if (!all(status$exists)) {
  stop("Some cached files are missing (FALSE rows above). ",
       "Those specific sections of r9_plot_estimates_v2.R would need to rerun their raw-file loop.")
}
cat("\nAll required intermediate files found -- skipping the raw per-sim file loops.\n\n")

## ---- species list / common names (pure code, no file dependency) ----
spp <- c('Limanda aspera','Gadus chalcogrammus','Gadus macrocephalus','Atheresthes stomias',
         'Reinhardtius hippoglossoides','Lepidopsetta polyxystra','Hippoglossoides elassodon',
         'Pleuronectes quadrituberculatus','Hippoglossoides robustus','Boreogadus saida',
         'Eleginus gracilis','Anoplopoma fimbria','Chionoecetes opilio','Paralithodes platypus',
         'Paralithodes camtschaticus','Chionoecetes bairdi')
spp <- setdiff(spp, c('Anoplopoma fimbria','Reinhardtius hippoglossoides'))

crabs <- c('BB_RKC','PBL_BKC','PBL_RKC','STM_BKC','SNW_CRB','TNR_CRB')

spp1 <- c('Yellowfin sole','Alaska pollock','Pacific cod','Arrowtooth flounder',
          'Northern rock sole','Flathead sole','Alaska plaice','Bering flounder',
          'Arctic cod','Saffron cod','Snow crab_EBSNBS','Blue king crab_EBSNBS',
          'Red king crab_EBSNBS','Tanner crab_EBSNBS')

df_spp     <- data.frame(spp = spp, common = spp1)
df_sppcrab <- data.frame(spp = crabs,
                         common = c('Bristol Bay\nred king crab','Pribilof Islands\nblue king crab',
                                    'Pribilof Islands\nred king crab','St. Matthew Island\nblue king crab',
                                    'Snow crab','Tanner crab'))
df_spp1 <- rbind(df_spp, df_sppcrab)
df_spp1 <- df_spp1[order(df_spp1$common), ]
df_spp1$label <- letters[1:nrow(df_spp1)]
setDT(df_spp1)

## ============================================================
## HISTORICAL: reconstruct all_df from cached objects only
## (mirrors lines ~799-839 of r9_plot_estimates_v2.R, no raw-file loop)
## ============================================================

load('output/survey performance/estimated_cvsim_hist.RData')  # cv2
setDT(cv2)
cv2$year <- as.character(cv2$year)
names(cv2)[6] <- 'cvsim'

load('output/survey performance/estimated_index_hist.RData')  # ind2
setDT(ind2)
index_sd <- ind2[, .(index_sd = sd(index)), by = .(spp, year, scn, approach)]

est_df <- merge(cv2, index_sd, by = c('spp', 'year', 'scn', 'approach'), all.x = TRUE)

load('output/survey performance/true_ind_hist.RData')  # true_ind
true_ind2 <- reshape2::melt(true_ind, id.vars = 'year')
names(true_ind2)[c(2, 3)] <- c('spp', 'true_ind')
setDT(true_ind2)

all_df <- merge(est_df, true_ind2, by = c('spp', 'year'), all.x = TRUE)
all_df$cvtrue <- all_df$index_sd / all_df$true_ind

cat("Reconstructed HISTORICAL all_df:", nrow(all_df), "rows\n\n")
rm(cv2, ind2, est_df); gc()

## ---- DIAGNOSTIC 1: single-cell check, existing + systematic ----
chk <- all_df[spp == spp[1] & year == year[1] & scn == 'scnbase' & approach == 'sys']
cat("--- HISTORICAL single-cell check: existing+systematic ---\n")
print(summary(chk$cvsim)); print(summary(chk$cvtrue))
cat("range(cvtrue):", range(chk$cvtrue),
    " | unique(cvtrue) count:", length(unique(chk$cvtrue)), "\n")
old_rrmse <- mean(sqrt((chk$cvsim - chk$cvtrue)^2)) / mean(chk$cvtrue)
new_rrmse <- sqrt(mean((chk$cvsim - chk$cvtrue)^2)) / mean(chk$cvtrue)
cat("old_rrmse (published formula):", old_rrmse,
    "\nnew_rrmse (corrected formula):", new_rrmse, "\n\n")

## ---- DIAGNOSTIC 2: is cvtrue ever truly constant (sd=0) for 'sys'? ----
cvtrue_var_by_design <- all_df[, .(n_unique_cvtrue = length(unique(cvtrue)),
                                   sd_cvtrue       = sd(cvtrue, na.rm = TRUE)),
                               by = .(spp, year, scn, approach)]
cat("--- HISTORICAL: cvtrue sd==0 count, by approach ---\n")
print(cvtrue_var_by_design[, .N, by = .(approach, sd_cvtrue == 0)])
cat("\n")

## ---- DIAGNOSTIC 3: full old-vs-new RRMSE sweep ----
sweep_hist <- all_df[, .(
  n_sim       = .N,
  cvtrue_mean = mean(cvtrue, na.rm = TRUE),
  old_rrmse   = mean(sqrt((cvsim - cvtrue)^2), na.rm = TRUE) / mean(cvtrue, na.rm = TRUE),
  new_rrmse   = sqrt(mean((cvsim - cvtrue)^2, na.rm = TRUE)) / mean(cvtrue, na.rm = TRUE)
), by = .(spp, year, scn, approach)]
sweep_hist[, abs_diff := new_rrmse - old_rrmse]
sweep_hist[, pct_diff := 100 * abs_diff / old_rrmse]

cat("--- HISTORICAL: (new - old) RRMSE gap, by approach ---\n")
print(sweep_hist[, .(mean_abs_diff = mean(abs_diff, na.rm = TRUE),
                     mean_pct_diff = mean(pct_diff, na.rm = TRUE),
                     max_pct_diff  = max(pct_diff, na.rm = TRUE)),
                 by = approach][order(-mean_pct_diff)])
cat("\n")

fwrite(sweep_hist, "output/survey performance/rrmse_old_vs_new_sweep_historical.csv")
rm(all_df); gc()

## ============================================================
## PROJECTED: reconstruct per-SBT replicate-level df1 from cached objects
## (mirrors lines ~1824-1908, no raw-file loop)
## ============================================================

load('output/survey performance/cvsim_proj.RData')           # cvsim
load('output/survey performance/estimated_index_proj.RData') # ind2
load('output/survey performance/true_ind_proj.RData')         # proj_ind2

setDT(cvsim); setDT(ind2); setDT(proj_ind2)
names(cvsim)[6] <- 'cvsim'
names(ind2)[6]  <- 'est_ind'
names(proj_ind2)[4] <- 'true_ind'
setnames(proj_ind2, names(proj_ind2)[1], 'spp')

proj_sweep_list <- vector("list", 8)
proj_raw_list   <- vector("list", 8)  # one cell kept per SBT, for the single-cell check

for (sbtscn in 1:8) {
  cat("##### SBT", sbtscn, "\n")
  
  cvsim1 <- cvsim[sbt == paste0('SBT', sbtscn)]
  ind3   <- ind2[sbt == paste0('SBT', sbtscn)]
  
  proj_ind3 <- proj_ind2[sbt == paste0('SBT', sbtscn)]
  proj_ind3 <- proj_ind3[, .(true_ind = first(true_ind)), by = .(spp, year)]
  
  index_sd <- ind3[, .(index_sd = sd(est_ind)), by = .(spp, year, scn, approach)]
  df <- merge(index_sd, proj_ind3, by = c('year', 'spp'))
  df[, cvtrue := index_sd / true_ind]
  df[, year := as.character(year)]
  cvsim1[, year := as.character(year)]
  df[, sbt := paste0('SBT', sbtscn)]
  
  df1 <- merge(cvsim1, df, by = c('spp', 'scn', 'approach', 'year', 'sbt'), all.x = TRUE)
  
  sweep_i <- df1[, .(
    n_sim       = .N,
    cvtrue_mean = mean(cvtrue, na.rm = TRUE),
    old_rrmse   = mean(sqrt((cvsim - cvtrue)^2), na.rm = TRUE) / mean(cvtrue, na.rm = TRUE),
    new_rrmse   = sqrt(mean((cvsim - cvtrue)^2, na.rm = TRUE)) / mean(cvtrue, na.rm = TRUE)
  ), by = .(spp, year, scn, approach)]
  sweep_i[, sbt := paste0('SBT', sbtscn)]
  
  proj_sweep_list[[sbtscn]] <- sweep_i
  # keep the raw replicate-level rows for ONE cell only, to avoid holding 8x the full data in memory
  proj_raw_list[[sbtscn]] <- df1[scn == 'scnbase' & approach == 'sys' & spp == spp[1] & year == year[1]]
  
  rm(cvsim1, ind3, proj_ind3, index_sd, df, df1)
}

sweep_proj <- rbindlist(proj_sweep_list, fill = TRUE)
sweep_proj[, abs_diff := new_rrmse - old_rrmse]
sweep_proj[, pct_diff := 100 * abs_diff / old_rrmse]

cat("\n--- PROJECTED: (new - old) RRMSE gap, by approach ---\n")
print(sweep_proj[, .(mean_abs_diff = mean(abs_diff, na.rm = TRUE),
                     mean_pct_diff = mean(pct_diff, na.rm = TRUE),
                     max_pct_diff  = max(pct_diff, na.rm = TRUE)),
                 by = approach][order(-mean_pct_diff)])

fwrite(sweep_proj, "output/survey performance/rrmse_old_vs_new_sweep_projected.csv")

chk_proj <- rbindlist(proj_raw_list, fill = TRUE)
cat("\n--- PROJECTED single-cell check: existing+systematic, across SBT scenarios ---\n")
print(chk_proj[, .(sd_cvtrue       = sd(cvtrue),
                   n_unique_cvtrue = length(unique(cvtrue)),
                   old_rrmse       = mean(sqrt((cvsim - cvtrue)^2)) / mean(cvtrue),
                   new_rrmse       = sqrt(mean((cvsim - cvtrue)^2)) / mean(cvtrue)),
               by = sbt])

rm(cvsim, ind2, proj_ind2); gc()

## ============================================================
## CROSS-CHECK against the already-saved FINAL RRMSE objects
## Confirms whether rrmse_cv_hist.RData / rrmse_cv_proj.RData on disk
## already reflect the corrected formula (should match new_rrmse above),
## or still reflect the old published formula (would match old_rrmse).
## ============================================================

load('output/survey performance/rrmse_cv_hist.RData')  # df3
cat("\n--- Saved rrmse_cv_hist.RData, existing+systematic (scnbase/sys) ---\n")
print(summary(df3[scn == 'scnbase' & approach == 'sys', rrmse]))

load('output/survey performance/rrmse_cv_proj.RData')  # rrmse
cat("\n--- Saved rrmse_cv_proj.RData, existing+systematic (scnbase/sys) ---\n")
print(summary(rrmse[scn == 'scnbase' & approach == 'sys', rrmse]))

cat("\nDone. Compare the two 'Saved ...' summaries just above against the\n",
    "old_rrmse / new_rrmse values printed earlier in this run: if they match\n",
    "new_rrmse, the cached RRMSE files already reflect the fix.\n")