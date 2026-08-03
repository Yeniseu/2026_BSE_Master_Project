# ============================================================================
# 19_MRF.R - Macroeconomic Random Forest (Goulet Coulombe, 2020;
#   github.com/philgoucou/macrorf) on both information sets, run the same
#   way 18_RandomForest.R and 20_LLF.R are:
#     full panel    -> MRF     (Non-Linear and Variable Selection)
#     labour subset -> MRF_P   (Non-Linear Phillips Curve)
#
# STANDALONE ADD-ON. This script only READS 00_2_Config.R and
# 00_Functions_Design.R; it does not touch 18_RandomForest.R, 20_LLF.R,
# 00_3_Run_Specifications.R or 50_Assemble_Predictions.R. Run it by itself
# (after 00_1's data step) to get mrf<WTAG>.rds next to rf<WTAG>.rds /
# llf<WTAG>.rds in 03_Output/Preds/. Wiring it into the paper's model list
# (FAMILIES in 00_2_Config.R, MODEL_SCRIPTS in 00_3_Run_Specifications.R,
# the merge in 50_Assemble_Predictions.R) is a deliberate later step, left
# to you so nothing existing changes as a side effect of adding this file.
#
# WHAT'S DIFFERENT FROM RF/LLF HERE
# ----------------------------------
# Plain RF/LLF use the whole lag-augmented panel produced by make_design() as
# their feature matrix and predict y directly. MRF instead fits a *linear*
# equation y_t = X_t'beta_t + eps_t whose coefficients beta_t vary over time
# as a Random-Forest function of a (typically larger) state panel S_t:
#   x.pos -> X_t : the nearest-past lag block (inflation's own lag + the
#                   K_FAC principal-component factors at lag h) - the
#                   "Phillips-curve" part that gets a GTVP.
#   S.pos -> S_t : every column make_design() built (all lags 0..nlag-1 of
#                   every series + PCA factors) - what the forest splits on.
# This mirrors the package's own FRED-MD example (own lag + factors in the
# linear part, full panel driving the trees) and keeps the exact same
# direct-h-step design (make_design) as every other model in the project, so
# MRF's numbers are comparable apples-to-apples.
#
# INTERPRETABILITY (the actual point of using MRF instead of another
# black-box forest): for the full-panel h=3 case only (mirrors
# SAVE_IMPORTANCE's "RF, h==3 only" scoping in 18_RandomForest.R, and matches
# what 65_Rotation_SHAP.R's figure already targets, so the two are directly
# comparable), every MRF() call also captures:
#   - betas    : the GTVPs, i.e. the estimated time-varying coefficients on
#                the linear part (own-lag of inflation + the K_FAC PCA
#                factors) nearest that origin's forecast date. Something
#                RF/LLF cannot give you at all - an actual Phillips-curve
#                slope moving over time.
#   - VI_oob / VI_oos / important.S : MRF's own variable-importance measures
#                over the FULL state panel S_t, the piece that's genuinely
#                comparable to 63_VarImp_8Groups.R / 65_Rotation_SHAP.R - it
#                tells you which state variables shaped inflation dynamics in
#                each origin's window, not just a fixed set of 5.
# All of this comes back as a byproduct of fitting (VI=TRUE does add real
# runtime, see PERFORMANCE below, but it's the whole reason to use MRF here).
# Saved RAW (one list entry per origin, not reshaped into a tidy table) as
# mrf_diagnostics_h3<tag>.rds - since the exact structure of MRF's VI output
# couldn't be verified against a live R session while writing this, every
# extraction is wrapped defensively so a bad assumption there can never break
# the forecast itself, and the raw dump gets shaped into a proper chart once
# there's real output to inspect (columns can be renamed/reshaped after the
# fact - forecasts and importance don't need to be recomputed).
#
# PERFORMANCE & RESILIENCE
# --------------------------
# A single MRF fit (492-month window, full panel, B=50 trees, VI=FALSE)
# measured ~8 minutes on this machine; ~6 min/chunk of 4 origins in steady
# state with 4 parallel workers. VI=TRUE (only for the MRF_h3 case, see
# above) adds real overhead on top of that. Full sample = ~nprev x 2 horizons
# x 2 panels of these (nprev ~ 300 months for the baseline sample) - this is
# genuinely a multi-day-to-multi-week job on a single laptop; every origin is
# an independent fit, so this script runs them in PARALLEL with the
# `parallel` package instead of reusing 00_Functions_Design.R's sequential
# rolling_forecast() driver (which stays untouched for RF/LLF).
#
# Diagnosed on this machine: sustained multi-minute, all-core-saturating PSOCK
# work occasionally drops a worker's socket connection (short bursts and
# lighter loads never reproduced it). Root cause is environmental (Windows
# networking under heavy sustained load, not this script's logic), so instead
# of chasing it further this script is written to tolerate it: origins are
# processed in small CHUNKS (one per worker), progress is CHECKPOINTED to
# disk after every chunk, and a chunk whose cluster call errors triggers a
# cluster rebuild and retry (up to MRF_MAX_RETRIES times) rather than losing
# the whole run. A restarted R session (or one moved to a faster machine)
# picks up from the last checkpoint automatically - nothing already computed
# is ever redone.
#
# SCOPE: full evaluation sample by default (every OOS month, same as
# RF/LLF). Set .MRF_YEARS (a vector of years) before sourcing to restrict to
# a subset instead, e.g. just the shock + a calm-baseline sample used
# earlier in this project's exploration:
#   .MRF_YEARS <- unlist(SUBPERIODS[c(SHOCK_SUBS, "2005-2007", "2017-2019")])
# Add .MRF_TEST_MONTHS on top to further cap how many of the selected origins
# are actually run, for a fast sanity check: .MRF_TEST_MONTHS <- 4.
# ============================================================================
source("01_RScript/00_2_Config.R")
source("01_RScript/00_Functions_Design.R")

if (!requireNamespace("MacroRF", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::install_github("philgoucou/macrorf")
}
library(MacroRF)
library(parallel)
set.seed(123)

data <- readRDS(file.path(P_IN, DATA_FILE)); setDT(data)
data <- data[!is.na(CPIAUCSL)]
setnames(data, "CPIAUCSL", "inf"); setcolorder(data, c("date", "inf"))

dates <- data$date
nprev <- sum(dates >= OOS_START)
target_dates <- tail(dates, nprev)   # target_dates[j] is the month forecast by origin j

# Default scope: full sample (see SCOPE note above). NULL = every OOS month.
.MRF_YEARS <- if (exists(".MRF_YEARS")) .MRF_YEARS else NULL

.mrf_tag <- paste0(WTAG,
                   if (!is.null(.MRF_YEARS)) "_scoped" else "",
                   if (exists(".MRF_TEST_MONTHS")) "_test" else "")

## ---- MRF-specific knobs (kept local so 00_2_Config.R is untouched) --------
# Reuses OPT_LEVEL ("fast"/"standard"/"thorough") from Config for consistency,
# but with its own (smaller) tree counts, since MRF trees are much heavier.
MRF_B            <- switch(OPT_LEVEL, fast = 30, standard = 50, thorough = 150)
MRF_MAX_RETRIES  <- 3

# R's socket connections default to a 60s read timeout (options("timeout")).
# Harmless but cheap insurance alongside the retry logic below.
options(timeout = 60 * 60 * 24)

# Default capped at 4 workers: on a 16GB machine that's already at ~12GB used
# before MRF even starts, running one worker per core left too little
# headroom and produced heavy swapping. Override with .MRF_N_CORES before
# sourcing if running on a machine with more RAM/cores to spare, e.g.
# .MRF_N_CORES <- 12; source(...).
N_CORES <- if (exists(".MRF_N_CORES")) .MRF_N_CORES else
  max(1, min(4, parallel::detectCores() - 1))
.PROJECT_ROOT <- getwd()
# outfile: RGui's console can corrupt/close its connection when several
# worker processes write to it at once (e.g. every worker's data.table
# startup banner arriving concurrently). Routing worker stdout/stderr to a
# log file instead avoids that.
.MRF_WORKER_LOG <- file.path(tempdir(), "mrf_worker_log.txt")

make_mrf_cluster <- function() {
  new_cl <- parallel::makeCluster(N_CORES, outfile = .MRF_WORKER_LOG)
  parallel::clusterExport(new_cl, ".PROJECT_ROOT")
  parallel::clusterEvalQ(new_cl, {
    setwd(.PROJECT_ROOT)
    suppressPackageStartupMessages({
      source("01_RScript/00_2_Config.R")
      source("01_RScript/00_Functions_Design.R")
      library(MacroRF)
    })
    NULL
  })
  # Give freshly spawned worker processes a few seconds to settle (e.g. AV/
  # Defender scanning new child processes on Windows) before any further
  # socket traffic. Cheap insurance against the immediate-post-creation
  # "invalid connection" failures seen when everything runs back-to-back via
  # source() with no natural pause between steps.
  Sys.sleep(5)
  new_cl
}

# Runs `expr` (referencing `cl`) with retry-on-error: on failure, rebuilds
# the cluster and tries again, up to MRF_MAX_RETRIES times. Used for every
# cluster operation, not just the per-chunk fits, since the "invalid
# connection" failure has shown up right at cluster-export time too.
with_cluster_retry <- function(expr, what = "cluster operation") {
  attempt <- 1
  repeat {
    result <- tryCatch(list(ok = TRUE, value = eval.parent(substitute(expr))),
                       error = function(e) list(ok = FALSE, err = e))
    if (result$ok) return(result$value)
    cat(sprintf("  %s failed, attempt %d/%d: %s\n",
                what, attempt, MRF_MAX_RETRIES, conditionMessage(result$err)))
    if (attempt >= MRF_MAX_RETRIES) stop(sprintf("%s failed after %d attempts", what, MRF_MAX_RETRIES))
    try(parallel::stopCluster(cl), silent = TRUE)
    cl <<- make_mrf_cluster()
    attempt <- attempt + 1
  }
}

# ---- Watchdog: forcibly kill stuck workers after a timeout -----------------
# A worker that dies or freezes without cleanly closing its socket leaves the
# master's parLapply() blocked forever waiting on a reply that will never
# arrive - no error is ever thrown, so with_cluster_retry()'s tryCatch never
# fires. A blocked low-level socket read like that isn't reliably
# interruptible from *within* the same R session on Windows, so instead: a
# detached Rscript process is armed before each chunk, sleeps past a generous
# timeout, and - only if the chunk hasn't finished by then (checked via a
# sentinel file) - force-kills that chunk's worker PIDs. Killing them breaks
# the socket from the OS's side, which unblocks the master's read with a
# connection error, letting the normal retry path take over from there.
MRF_CHUNK_TIMEOUT <- 60 * 60   # seconds; generous, VI=TRUE fits run slower than plain forecasts

get_worker_pids <- function() {
  if (!file.exists(.MRF_WORKER_LOG)) return(integer(0))
  ln <- grep("^starting worker pid=", readLines(.MRF_WORKER_LOG, warn = FALSE), value = TRUE)
  unique(as.integer(sub("^starting worker pid=([0-9]+).*", "\\1", ln)))
}

arm_watchdog <- function(sentinel, pids, timeout) {
  if (length(pids) == 0) return(invisible(NULL))
  script <- sprintf(
    'Sys.sleep(%d); if (file.exists(%s)) for (p in c(%s)) try(tools::pskill(p), silent = TRUE)',
    timeout, shQuote(sentinel), paste(pids, collapse = ","))
  system2("Rscript", c("-e", shQuote(script)), wait = FALSE, stdout = NULL, stderr = NULL)
}

cl <- make_mrf_cluster()
on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
cat(sprintf("MRF cluster: %d workers (log: %s)\n", N_CORES, .MRF_WORKER_LOG))

# ---- Rolling-window driver for MRF: chunked, checkpointed, retried ---------
# Every origin is an independent fit, dispatched across `cl` in chunks of
# N_CORES origins at a time. Progress is saved to a checkpoint file after
# every chunk, so a crashed/interrupted/restarted session resumes instead of
# starting over. A chunk whose parLapply() call errors (e.g. a dropped
# worker connection under heavy sustained load) rebuilds the cluster and
# retries that chunk, up to MRF_MAX_RETRIES times.
#
# Diagnostics (betas + variable importance) only when label == "MRF_h3" (full
# panel, h=3) - see INTERPRETABILITY note at the top of this file.
rolling_forecast_mrf_par <- function(Y, nprev, window, h, hp, label = "") {
  k_block <- 1 + K_FAC           # inf + K_FAC PCA factors, per lag block
  x_idx   <- seq_len(k_block)    # nearest-past block (lag = h) -> linear part
  keep_diag <- identical(label, "MRF_h3")

  # Restrict to the target years (SCOPE note at the top of this file); NULL
  # means no restriction, i.e. the full evaluation sample. Computed before the
  # checkpoint/progress messages below so they report against the actual
  # target count, not the full raw nprev.
  wanted <- if (is.null(.MRF_YEARS)) seq_len(nprev)
            else which(data.table::year(target_dates) %in% .MRF_YEARS)
  if (exists(".MRF_TEST_MONTHS")) wanted <- head(wanted, .MRF_TEST_MONTHS)
  n_wanted <- length(wanted)

  ckpt_file <- file.path(P_PRED, sprintf("._mrf_ckpt_%s%s.rds", label, WTAG))
  save.pred <- rep(NA_real_, nprev)
  diag_list <- if (keep_diag) vector("list", nprev) else NULL
  done <- integer(0)
  if (file.exists(ckpt_file)) {
    ck <- readRDS(ckpt_file)
    save.pred[ck$idx] <- ck$pred
    if (keep_diag && !is.null(ck$diag)) diag_list[ck$idx] <- ck$diag
    done <- ck$idx
    cat(sprintf("  [%s] resuming from checkpoint: %d/%d origins in scope already done\n",
                label, length(intersect(done, wanted)), n_wanted))
  }

  todo <- setdiff(intersect(seq_len(nprev), wanted), done)
  if (length(todo) == 0) {
    real <- tail(Y[, 1], nprev)
    ok   <- !is.na(save.pred)
    return(list(pred = save.pred, real = real,
                diag = if (keep_diag) list(dates = target_dates[wanted], diag = diag_list[wanted]) else NULL,
                errors = c(rmse = sqrt(mean((real[ok] - save.pred[ok])^2)),
                           mae  = mean(abs(real[ok] - save.pred[ok])))))
  }

  chunks <- split(todo, ceiling(seq_along(todo) / N_CORES))

  fit_one <- function(j) {
    i         <- nprev - j + 1
    idx_end   <- nrow(Y) - i
    idx_start <- idx_end - window + 1
    Yw <- Y[idx_start:idx_end, , drop = FALSE]

    des <- make_design(Yw, indice = 1, h = h, nlag = N_LAG, kfac = K_FAC, dum = NULL)
    mat <- rbind(cbind(des$y, des$X), c(NA_real_, des$X.out))
    colnames(mat) <- c("y", colnames(des$X))

    m <- MRF(mat,
             y.pos               = 1,
             x.pos               = x_idx + 1L,               # +1: y took column 1
             S.pos               = seq_len(ncol(des$X)) + 1L,
             oos.pos             = nrow(mat),
             mtry.frac           = min(1, hp$rf_mtry / ncol(des$X)),
             B                   = MRF_B,
             quantile.rate       = 0.3,
             rw.regul            = 0.75,
             fast.rw             = TRUE,
             resampling.opt      = 2,
             VI                  = keep_diag,   # only the full-panel h=3 case pays this cost
             cheap.look.at.GTVPs = FALSE,
             printb              = FALSE)

    out <- list(pred = as.numeric(m$pred))
    if (keep_diag) {
      # Everything here is best-effort: MRF's exact output structure for
      # betas/VI_* couldn't be verified against a live R session while
      # writing this, so each piece is captured defensively and RAW (not
      # reshaped) - a wrong assumption here must never break the forecast,
      # and raw output can still be reshaped later once real data exists.
      out$diag <- tryCatch(list(
        beta       = { b <- as.matrix(m$betas); if (is.null(b) || nrow(b) == 0) NULL else b[nrow(b), , drop = TRUE] },
        VI_oob     = m$VI_oob,
        VI_oos     = m$VI_oos,
        important.S = m$important.S
      ), error = function(e) list(error = conditionMessage(e)))
    }
    out
  }

  # NOTE: fit_one is deliberately NOT clusterExport()-ed. parLapply() below
  # serializes it inline together with each task, the same way the working
  # isolated tests did; exporting it separately as a named closure was the
  # actual bug in an earlier version of this script (it dragged its whole
  # enclosing frame along a different, untested serialization path and broke
  # cluster communication immediately).
  export_vars <- function() {
    parallel::clusterExport(cl, varlist = c("Y", "window", "h", "hp", "k_block",
                                            "x_idx", "MRF_B", "keep_diag"),
                            envir = environment())
  }
  environment(export_vars) <- environment()

  for (ch in chunks) {
    t0  <- Sys.time()
    out <- with_cluster_retry({
        export_vars()   # (re-)export; cheap, and required again if cl was just rebuilt
        sentinel <- tempfile()
        file.create(sentinel)
        arm_watchdog(sentinel, get_worker_pids(), MRF_CHUNK_TIMEOUT)
        r <- parallel::parLapply(cl, ch, fit_one)
        unlink(sentinel)   # done in time -> a late-firing watchdog becomes a no-op
        r
      },
      what = sprintf("[%s] chunk (origins %d-%d)", label, min(ch), max(ch)))

    save.pred[ch] <- vapply(out, function(r) r$pred, numeric(1))
    if (keep_diag) diag_list[ch] <- lapply(out, function(r) r$diag)
    done <- c(done, ch)

    ckpt <- list(idx = done, pred = save.pred[done])
    if (keep_diag) ckpt$diag <- diag_list[done]
    saveRDS(ckpt, ckpt_file)

    cat(sprintf("  [%s] %d/%d done (chunk: %.1f min)\n",
                label, length(intersect(done, wanted)), n_wanted,
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }

  real <- tail(Y[, 1], nprev)
  ok   <- !is.na(save.pred)   # months outside .MRF_YEARS scope stay NA
  list(pred = save.pred, real = real,
       diag = if (keep_diag) list(dates = target_dates[wanted], diag = diag_list[wanted]) else NULL,
       errors = c(rmse = sqrt(mean((real[ok] - save.pred[ok])^2)),
                  mae  = mean(abs(real[ok] - save.pred[ok]))))
}

run_mrf <- function(vars, nm, set) {
  Y <- as.matrix(data[, .SD, .SDcols = c("inf", vars)])
  Y <- Y[, colSums(is.na(Y)) == 0, drop = FALSE]
  # MRF builds split-rule strings internally and re-parses them; raw FRED-MD
  # names like "S&P 500" break that parser. Sanitize locally (script-only,
  # doesn't touch `data` or any other model's column names).
  colnames(Y) <- make.names(colnames(Y), unique = TRUE)
  out <- list()

  for (h in HORIZONS) {
    hp <- get_hp(set, h)   # reuses rf_mtry as the mtry.frac source

    r <- rolling_forecast_mrf_par(Y, nprev, TRAIN_WINDOW, h, hp,
                                  label = paste0(nm, "_h", h))
    out[[paste0("h", h)]] <- setNames(list(r$pred), nm)
    if (!is.null(r$diag)) out$diag_h3 <- r$diag   # only ever set for nm=="MRF", h==3
    cat(sprintf("%-6s h=%d  RMSE %.4f\n", nm, h, r$errors["rmse"]))
  }
  out
}

full  <- run_mrf(setdiff(names(data), c("date", "inf")), "MRF",   "full")
labor <- run_mrf(intersect(LABOR_VARS, names(data)),     "MRF_P", "labor")

preds <- list()
for (h in HORIZONS) {
  hh <- paste0("h", h)
  preds[[hh]] <- data.table(date = target_dates,
                            as.data.table(c(full[[hh]], labor[[hh]])))
  if (!is.null(.MRF_YEARS)) {
    # Drop months outside scope rather than shipping a file full of NAs.
    preds[[hh]] <- preds[[hh]][data.table::year(date) %in% .MRF_YEARS]
  }
}
saveRDS(preds, file.path(P_PRED, paste0("mrf", .mrf_tag, ".rds")))
cat(sprintf("saved %s\n", file.path(P_PRED, paste0("mrf", .mrf_tag, ".rds"))))

if (!is.null(full$diag_h3)) {
  saveRDS(full$diag_h3, file.path(P_PRED, paste0("mrf_diagnostics_h3", .mrf_tag, ".rds")))
  cat(sprintf("saved %s (%d origins)\n",
              file.path(P_PRED, paste0("mrf_diagnostics_h3", .mrf_tag, ".rds")),
              length(full$diag_h3$diag)))
}

# Clean up checkpoint files only once EVERY origin is done (.MRF_YEARS scope
# restrictions leave most origins un-computed on purpose - keep the
# checkpoints in that case so a later, wider-scope run resumes instead of
# recomputing the months already done here).
if (is.null(.MRF_YEARS)) {
  unlink(Sys.glob(file.path(P_PRED, sprintf("._mrf_ckpt_*%s.rds", WTAG))))
}

# ---- To fold MRF into the paper's pipeline later (none of this runs here) --
# 1. 00_2_Config.R:   add "MRF" / "MRF_P" to the relevant FAMILIES entries
#                      (MODELS is derived from FAMILIES automatically).
# 2. 00_3_Run_Specifications.R: add "01_RScript/19_MRF.R" to MODEL_SCRIPTS.
# 3. 50_Assemble_Predictions.R: add
#      mf <- readRDS(file.path(P_PRED, paste0("mrf", WTAG, ".rds")))
#    to the readRDS block and to the Reduce(merge, list(...)) call.
