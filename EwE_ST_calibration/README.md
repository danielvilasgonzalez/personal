# EwE ST calibration: scripts

Run on the Mac (R) unless marked Windows. Edit the paths at the top of each script.

| Script | What it does |
|---|---|
| `st_calibration_functions.R` | All functions (reading the GA run, plotting, re-scoring helpers). Sourced by the scripts below. |
| `st calibration run phase6.R` | Main figures of the phase 6 / ga01 run: GA convergence, parameter distributions, time series, maps, response curves. Section 7 sources the contribution figure. |
| `st results phase6.R` | Numbers and tables for the Results and Discussion: NLL partition, bounds and ensemble spread, re-scoring of the ensemble members, entropy and Moran's I of the maps, RRMSE of the stanza biomass series, run-to-run noise. Writes csv files only. |
| `fig contribution and parameters phase6.R` | Manuscript Fig. X (contribution of series types and parameter types to the fit). |
| `ga post-run checks WFS MICE.R` | **Windows, needs Ecospace.** A: replicates of the ensemble members (noise, winner's curse). B: revert one parameter type at a time (signed effect of each type). |

## Order
1. `st calibration run phase6.R` (section 7 is skipped the first time because its inputs do not exist yet; the file `plots_R/ts/parameters_calibrated_vs_not.csv` made here is read by the next step).
2. `st results phase6.R`.
3. Run section 7 of `st calibration run phase6.R` again, or `source("fig contribution and parameters phase6.R")`.
4. Optional, on Windows: `ga post-run checks WFS MICE.R`; copy `finalist_replicates.csv` and `revert_one_type.csv` to `<RUN>/sens1/` and rerun section 5 of `st results phase6.R`.

The GA run scripts themselves (`run ecospace GA WFS MICE.R`, `setup cal WFS MICE.R`, `run par sensitivity WFS MICE.R`) are archived in `<RUN>/code/`.
