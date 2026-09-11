# ============================================================================
# 57_Robustness_Tables.R
# Formats the two robustness-section tables that main.tex expects but that
# nothing currently writes:
#   Table~\ref{tab:rob_avg}   -> 06_Latex/Tables/Robustness_Average.tex
#   Table~\ref{tab:rob_specs} -> 06_Latex/Tables/Robustness_Shock_Specs.tex
#
# Inputs:
#   03_Output/Paper/rmse_robustness_average.csv   (written by 55_Robustness_Windows.R)
#   03_Output/Preds/preds_h{1,3}_w{360,420,480,492}.rds  (the 8 specifications;
#     same files 56_Spec_Diagnostics.R reads -- this script reuses that logic)
#
# Outputs:
#   06_Latex/Tables/Robustness_Average.tex
#   06_Latex/Tables/Robustness_Shock_Specs.tex
# ============================================================================
source("01_RScript/00_2_Config.R")

WINDOW_YEARS <- c(`360` = 30, `420` = 35, `480` = 40, `492` = 41)

## ============================================================================
## Table tab:rob_avg -- Robustness_Average.tex
## Caption (main.tex): "Lowest RMSE in each row in bold; shock windows in bold."
## ============================================================================
avg <- as.data.frame(fread(file.path(P_PAPER, "rmse_robustness_average.csv")))
fam_cols <- names(FAMILIES)
stopifnot(all(fam_cols %in% names(avg)))

body_lines <- character(nrow(avg))
for (i in seq_len(nrow(avg))) {
  vals  <- as.numeric(avg[i, fam_cols])
  shown <- sprintf("%.3f", vals)
  shown[which.min(vals)] <- sprintf("\\textbf{%s}", shown[which.min(vals)])

  sub_lab  <- avg$sub[i]
  is_shock <- sub_lab %in% SHOCK_SUBS
  if (is_shock) {
    shown   <- sprintf("\\textbf{%s}", sprintf("%.3f", vals))   # whole row bold
    sub_lab <- sprintf("\\textbf{%s}", sub_lab)
  }
  body_lines[i] <- paste(c(sub_lab, shown), collapse = " & ")
}

lines1 <- c(
  sprintf("\\begin{tabular}{l%s}", strrep("c", length(fam_cols))),
  "\\toprule",
  paste0("Sub-period & ", paste(fam_cols, collapse = " & "), " \\\\"),
  "\\midrule",
  paste0(body_lines, " \\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(lines1, file.path(T_DIR, "Robustness_Average.tex"))
cat("Wrote", file.path(T_DIR, "Robustness_Average.tex"), "\n")

## ============================================================================
## Table tab:rob_specs -- Robustness_Shock_Specs.tex
## Caption (main.tex): "Percentage reduction in RMSE of the Non-Linear with
## Variable Selection family relative to the Linear Phillips Curve, by
## specification. Positive values favour the flexible family."
## ============================================================================
files <- list.files(P_PRED, pattern = "^preds_h[13]_w[0-9]+\\.rds$", full.names = TRUE)
stopifnot(length(files) > 0)

# Same per-specification family RMSE computation as 56_Spec_Diagnostics.R
per_spec <- rbindlist(lapply(files, function(f) {
  m <- regmatches(basename(f), regexec("preds_h([13])_w([0-9]+)", basename(f)))[[1]]
  d <- readRDS(f)[!is.na(sub)]
  r <- d[, lapply(.SD, function(x) sqrt(mean((x - real)^2))), by = sub, .SDcols = MODELS]
  for (fm in names(FAMILIES)) r[[fm]] <- rowMeans(r[, FAMILIES[[fm]], with = FALSE])
  cbind(h = as.integer(m[2]), w = as.integer(m[3]), r)
}))

shock <- as.data.frame(per_spec[sub %in% SHOCK_SUBS])
lin <- shock[["Linear Phillips Curve"]]
nlv <- shock[["Non-Linear and Variable Selection"]]
shock$pct_reduction <- 100 * (lin - nlv) / lin

gfc   <- shock[shock$sub == "2008-2010", c("h", "w", "pct_reduction")]
covid <- shock[shock$sub == "2020-2022", c("h", "w", "pct_reduction")]
names(gfc)[3]   <- "GFC"
names(covid)[3] <- "COVID"
wide <- merge(gfc, covid, by = c("h", "w"))
wide$Window <- WINDOW_YEARS[as.character(wide$w)]
wide <- wide[order(wide$h, wide$Window), ]

rows <- sprintf("%d & %d & %.1f & %.1f \\\\", wide$h, wide$Window, wide$GFC, wide$COVID)
avg_h1 <- colMeans(wide[wide$h == 1, c("GFC", "COVID")])
avg_h3 <- colMeans(wide[wide$h == 3, c("GFC", "COVID")])

lines2 <- c(
  "\\begin{tabular}{llcc}",
  "\\toprule",
  "$h$ & Window (years) & GFC & COVID \\\\",
  "\\midrule",
  rows,
  "\\midrule",
  sprintf("\\multicolumn{2}{l}{Average, $h=1$} & %.1f & %.1f \\\\", avg_h1["GFC"], avg_h1["COVID"]),
  sprintf("\\multicolumn{2}{l}{Average, $h=3$} & %.1f & %.1f \\\\", avg_h3["GFC"], avg_h3["COVID"]),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(lines2, file.path(T_DIR, "Robustness_Shock_Specs.tex"))
cat("Wrote", file.path(T_DIR, "Robustness_Shock_Specs.tex"), "\n")

cat("\n--- sanity check against the paper text ---\n")
cat("GFC, h=3 by window (text says 8.2, 7.5, 7.5, 7.7, avg 7.7):\n")
print(wide[wide$h == 3, c("Window", "GFC")])
cat("GFC, h=1 by window (text says negative in every window, avg -3.6):\n")
print(wide[wide$h == 1, c("Window", "GFC")])
cat(sprintf("\nGFC avg h=3: %.1f%%   GFC avg h=1: %.1f%%\n", avg_h3["GFC"], avg_h1["GFC"]))
cat(sprintf("COVID avg h=3: %.1f%%   COVID avg h=1: %.1f%%  (text says 25.3 / 26.9)\n",
            avg_h3["COVID"], avg_h1["COVID"]))
