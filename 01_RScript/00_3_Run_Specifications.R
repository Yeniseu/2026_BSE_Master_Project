# ============================================================================
# 00_3_Run_Specifications.R - run the forecasting pipeline for one or several
# specifications in a single call.
#
# A specification = (training-window length, optimization level, target).
# Each run writes its own preds_h{1,3}_w<window>[_<opt>].rds, so specifications
# never overwrite each other, and 55_Robustness_Windows.R automatically
# averages over every specification present.
#
# DEFAULT: only "baseline" has run = TRUE, which reproduces the main results
# of the paper (492-month window, standard optimization, headline CPI).
#
# The six old robustness specifications = the three window variants below
# (each produces BOTH horizons h = 1 and h = 3 in one pass: 3 x 2 = 6).
# Flip their run flags to TRUE and source this script.
#
# Optimization levels (00_2_Config.R): "fast" ~3x faster forests for exploratory
# runs, "standard" = the paper, "thorough" for a final polish.
# ============================================================================

SPECS <- list(
  baseline = list(window = 492, opt = "standard", run = T),
  w360     = list(window = 360, opt = "standard", run = F),   # 30-year window
  w420     = list(window = 420, opt = "standard", run = F),   # 35-year window
  w480     = list(window = 480, opt = "standard", run = F)    # 40-year window
)

MODEL_SCRIPTS <- c("01_RScript/10_Benchmarks.R",
                   "01_RScript/15_Penalised.R",
                   "01_RScript/18_RandomForest.R",
                   "01_RScript/20_LLF.R",
                   "01_RScript/50_Assemble_Predictions.R")

for (spec_name in names(SPECS)) {
  sp <- SPECS[[spec_name]]
  if (!isTRUE(sp$run)) next

  cat(sprintf("\n########## SPEC '%s': window = %d, opt = %s ##########\n",
              spec_name, sp$window, sp$opt))

  # overrides picked up by 00_2_Config.R inside every sourced script
  .SPEC_WINDOW <<- sp$window
  .SPEC_OPT    <<- sp$opt

  for (f in MODEL_SCRIPTS) {
    cat(sprintf("--- %s\n", f))
    source(f)
  }
}
rm(.SPEC_WINDOW, .SPEC_OPT)   # back to baseline for anything sourced later

cat("\nAll requested specifications finished.\n")
cat("Analysis scripts (52/60/61/63) read the baseline; 55 averages all specs.\n")
