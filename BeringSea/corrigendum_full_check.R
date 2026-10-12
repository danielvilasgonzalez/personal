## ============================================================
## CONSOLIDATED CORRIGENDUM CHECKS -- Vilas et al. 2025 ICES JMS
## (DOI 10.1093/icesjms/fsae158)
##
## Replaces the separate validate_rrmse.R / validate_ranking.R /
## validate_cv_precision_63pct.R / validate_cv_precision_16stocks.R
## scripts with one script covering every numeric claim in the
## corrigendum correction table (C1-C7). Run this after
## r9_plot_estimates_v2.R (reuses its cached .RData outputs; no raw
## simulation re-run needed -- everything here finishes in well
## under a minute).
##
## PART 1 -- RRMSE-of-CV formula fix (C1, C2, C5): confirms the
##           cached rrmse_cv_hist.RData / rrmse_cv_proj.RData already
##           reflect sqrt(mean(.)) rather than mean(sqrt(.)).
## PART 2 -- Design-ranking reversal (C3, C4): historical vs.
##           projected mean/median RRMSE by station-allocation method.
## PART 3 -- "About 63% of the stocks" / "10 of 16" claim (C6):
##           CV-magnitude (precision) comparison, NOT RRMSE -- a
##           different metric, confirmed unaffected by the formula
##           bug. Uses the manuscript's correct 16 stocks (10
##           groundfish species + 6 crab stock-codes), not the raw
##           20 spp/stock rows in the cached data.
## ============================================================

library(data.table)

cat("\n######################################################\n")
cat("## PART 1 -- RRMSE formula fix: historical + projected\n")
cat("######################################################\n")

load('output/survey performance/rrmse_cv_hist.RData')  # df3
setDT(df3)
cat("\n--- HISTORICAL, existing design (scnbase + sys) ---\n")
print(summary(df3[scn == 'scnbase' & approach == 'sys', rrmse]))

load('output/survey performance/rrmse_cv_proj.RData')  # rrmse
setDT(rrmse)
cat("\n--- PROJECTED, existing design (scnbase + sys) ---\n")
print(summary(rrmse[scn == 'scnbase' & approach == 'sys', rrmse]))

cat("\nExpected (already confirmed in the corrigendum doc):\n",
    "  historical: min 0.12, median 0.62, mean 0.74, max 1.85\n",
    "  projected:  min 0.12, median 0.79, mean 0.74, max 1.00 (exactly)\n",
    "If the numbers above match, the cached files already use the corrected formula.\n")

cat("\n######################################################\n")
cat("## PART 2 -- design-ranking reversal\n")
cat("######################################################\n")

cat("\n--- HISTORICAL: mean/median RRMSE by station allocation (pooled across stratifications) ---\n")
print(df3[, .(mean_rrmse = mean(rrmse, na.rm = TRUE),
              median_rrmse = median(rrmse, na.rm = TRUE)),
          by = approach][order(mean_rrmse)])

cat("\n--- PROJECTED: mean/median RRMSE by station allocation (pooled across stratifications) ---\n")
print(rrmse[, .(mean_rrmse = mean(rrmse, na.rm = TRUE),
                median_rrmse = median(rrmse, na.rm = TRUE)),
            by = approach][order(mean_rrmse)])

cat("\nExpected: historical ranking rand < sb < sys (random most accurate); projected\n",
    "ranking reverses, sb and sys tied, rand becomes LEAST accurate.\n")

cat("\n######################################################\n")
cat("## PART 3 -- 'about 63%' / '10 of 16 stocks' claim (CV precision, not RRMSE)\n")
cat("######################################################\n")

## manuscript's correct 16 stocks: 10 groundfish species + 6 crab stock-codes.
## the cached CV objects have 20 spp levels because 4 crab species appear BOTH
## as a raw species name and as area-specific stock code(s) -- drop the 4
## species-level duplicates.
drop_dupe_crab_species <- c('Chionoecetes opilio', 'Paralithodes platypus',
                             'Paralithodes camtschaticus', 'Chionoecetes bairdi')

load('output/survey performance/estimated_cvsim_hist.RData')  # cv2
cv_hist <- copy(cv2)
setDT(cv_hist)
mean_cv_hist <- cv_hist[!spp %in% drop_dupe_crab_species,
                        .(mean_cv = mean(cv, na.rm = TRUE)), by = .(spp, scn, approach)]

load('output/survey performance/cvsim_proj.RData')  # cvsim
setDT(cvsim)
if (!'cvsim' %in% names(cvsim)) names(cvsim)[6] <- 'cvsim'
mean_cv_proj <- cvsim[!spp %in% drop_dupe_crab_species,
                      .(mean_cv = mean(cvsim, na.rm = TRUE)), by = .(spp, scn, approach, sbt)]
mean_cv_proj <- mean_cv_proj[, .(mean_cv = mean(mean_cv, na.rm = TRUE)), by = .(spp, scn, approach)]

## finalized method: best optimized design (any allocation, scn1-3) vs. existing
## systematic design (scnbase + sys), lower mean CV = "more precise", required in
## BOTH periods.
existing_hist <- mean_cv_hist[scn == 'scnbase' & approach == 'sys', .(spp, existing_cv = mean_cv)]
best_opt_hist <- mean_cv_hist[scn %in% c('scn1', 'scn2', 'scn3'),
                               .(best_optimized_cv = min(mean_cv, na.rm = TRUE)), by = spp]
cmp_hist <- merge(existing_hist, best_opt_hist, by = 'spp')
cmp_hist[, more_precise_hist := best_optimized_cv < existing_cv]
cmp_hist[, margin_pct_hist := 100 * (existing_cv - best_optimized_cv) / existing_cv]

existing_proj <- mean_cv_proj[scn == 'scnbase' & approach == 'sys', .(spp, existing_cv = mean_cv)]
best_opt_proj <- mean_cv_proj[scn %in% c('scn1', 'scn2', 'scn3'),
                               .(best_optimized_cv = min(mean_cv, na.rm = TRUE)), by = spp]
cmp_proj <- merge(existing_proj, best_opt_proj, by = 'spp')
cmp_proj[, more_precise_proj := best_optimized_cv < existing_cv]
cmp_proj[, margin_pct_proj := 100 * (existing_cv - best_optimized_cv) / existing_cv]

cmp_both <- merge(cmp_hist[, .(spp, more_precise_hist, margin_pct_hist)],
                   cmp_proj[, .(spp, more_precise_proj, margin_pct_proj)], by = 'spp')
cmp_both[, more_precise_both := more_precise_hist & more_precise_proj]
cmp_both[, near_tie := pmin(abs(margin_pct_hist), abs(margin_pct_proj)) < 1]  # within 1%

cat("\n--- Stocks more precise under optimized designs, in BOTH periods (16 stocks) ---\n")
print(cmp_both[order(spp)])
cat("\nCount:", sum(cmp_both$more_precise_both), "of", nrow(cmp_both),
    "(published: 10 of 16, ~63%)\n")
cat("Stocks within 1% margin in either period (candidates for the published/\n",
    "reconstructed count to differ by exactly one):\n")
print(cmp_both[near_tie == TRUE, .(spp, margin_pct_hist, margin_pct_proj, more_precise_both)])

cat("\n######################################################\n")
cat("## SUMMARY\n")
cat("######################################################\n")
cat("C1, C2, C5 (RRMSE formula):    see Part 1 -- should already be fixed in cache.\n")
cat("C3, C4 (ranking reversal):     see Part 2 -- random should flip from best to worst.\n")
cat("C6 (63% / 10-of-16 claim):     see Part 3 -- reconstructed count vs. published 10.\n")
cat("C7 (Abstract 'always' claim):  same fix as C4, no separate check needed.\n")
