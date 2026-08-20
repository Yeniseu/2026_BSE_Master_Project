# 01_RScript

Run `00_1_Master_Script.R` — by default it reproduces the paper's main results
(baseline spec: 492-month window, standard optimization, headline CPI,
sample 1960-01 .. 2025-12 from the 2026-06 FRED-MD vintage).

## Switches (all in `00_2_Config.R`)

* `VINTAGE_CSV`, `SAMPLE_END` — raw vintage and sample cut (do not use 2026).
* `OPT_LEVEL` — `"fast"` (~3x faster forests, for smoke tests) /
  `"standard"` (the paper) / `"thorough"` (final polish). Controls RF trees
  and LLF trees / CI groups; penalised models unaffected. Non-standard runs
  are tagged `_fast` / `_thorough` in the output filenames, so they can never
  overwrite the baseline.
* `HP_DEFAULT` — tuned hyper-parameters hard-coded for an immediate run.
  `HP_CASES` — per-case overrides keyed `full_h1 / full_h3 / labor_h1 /
  labor_h3`; fill in as case-specific tuning is done locally (empty list =
  defaults, which reproduces the current paper).
* `USE_DUMMIES` — GFC/COVID dummies in every model (FALSE for the paper).
* `BASE_WINDOW` / `BASE_WTAG` — the specification the **paper reports** (41-year
  baseline). Kept separate from `WTAG`, which is the specification currently
  being *estimated*: scripts `10`–`50` write per-spec files using `WTAG`, while
  the reporting scripts `51`–`65` always read `BASE_WTAG`. This is why a
  leftover `.SPEC_WINDOW` from a robustness run cannot redirect the paper's
  tables and figures to another window. Config prints both tags on load.
* `TUNE` — hyper-parameter search, **off by default**. Set to
  `c("lasso","ridge","elnet")` (minutes), add `"rf"` (slow), or `"all"` to
  include the LLF (very slow), then run `05_Tune_Hyperparameters.R` (the
  master script calls it automatically when `TUNE` is not FALSE). It scores
  every candidate by rolling-window RMSE on the 120 months **before**
  `OOS_START`, so the evaluation period is never used for tuning, and prints
  a paste-ready `HP_CASES` block. `TUNE_NPREV` / `TUNE_WINDOW` / `TUNE_OPT` /
  `TUNE_GRID` control the validation design and the grids.

## Specifications

`00_3_Run_Specifications.R` runs any set of (window, optimization) specs in one
call; the default runs only `baseline`. Flip `run = TRUE` on `w240 / w360 /
w480` to produce the six robustness specifications (three windows, and each
run produces both horizons h = 1, 3), then `55_Robustness_Windows.R`
averages every spec it finds under `03_Output/Preds/`.

## Pipeline

| Script | Produces |
|---|---|
| `00_2_Config.R` | data, sample, HP cases, OPT levels, spec overrides |
| `00_Functions_Design.R` | shared design matrix + rolling-window driver |
| `01_Data_Transformation.R` / `02_Data_Cleaning.R` | `data_cleaned.rds` |
| `07_Descriptives_Clean.R` | Figures 1-2 + descriptives table |
| `05_Tune_Hyperparameters.R` | grid search -> paste-ready `HP_CASES` (optional) |
| `10/15/18/20` | benchmarks, penalised, RF, LLF forecasts |
| `50_Assemble_Predictions.R` | canonical `preds_h{1,3}_w*.rds` |
| `51/52/60/61` | cum-error fig, RMSE fig, DM+interaction tables, ensembles |
| `63/64/65` | variable importance, SHAP, rotation figure |
| `55` / `70` | window-robustness average / core-CPI target |
| `00_3_Run_Specifications.R` | spec driver (baseline by default) |

Everything after `50_` reads only `preds_h*_w*.rds` — the reporting scripts via
`BASE_WTAG` (the baseline), `55` by globbing every specification — and every
figure/table lands directly in `06_Latex/` under the exact name the paper
expects.

NOTE (2026-06 vintage): October 2025 was never published for CPI and ~20
series (federal government shutdown). The EM/PCA imputation in `02` fills
those cells; the paper discloses this in the Data section.
