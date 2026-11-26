#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# run all phase6.R -- regenerates every manuscript and supplementary figure and table of the phase6 ga01 run with
# the 80% ensemble (ENS_PROP in the two scripts below) and all 58 fitted series. One call:
#   source("/Users/daniel/Documents/GitHub/personal/EwE_ST_calibration/run all phase6.R")
# Order matters: the results script writes the csv files (NLL by series, rRMSE of the fitted series, cut-off assessment,
# model predictions cache) that the run script reads for the contribution, four-panel, rRMSE and cut-off figures.
# Set EXTRA_PLOTS <- TRUE in 'st calibration run phase6.R' to also get the diagnostic figures that are not in the manuscript.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
REPO <- Sys.getenv("EWE_REPO", "/Users/daniel/Documents/GitHub/personal/EwE_ST_calibration")
N_CORES <- 3L                                        # parallel cores; lower = cooler laptop, slower
PRED_ONLY <- FALSE                                   # TRUE would skip the NLL re-scoring that the contribution figure needs
t0 <- Sys.time()
message("[1/2] st results phase6.R (NLL re-scoring, rRMSE of the 58 fitted series, cut-off assessment)")
source(file.path(REPO, "st results phase6.R"))
message("[2/2] st calibration run phase6.R (all figures and tables)")
source(file.path(REPO, "st calibration run phase6.R"))
message("Finished in ", round(difftime(Sys.time(), t0, units = "mins")), " min. Figures are in <RUN>/plots_R.")
