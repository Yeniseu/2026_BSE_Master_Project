# ============================================================================
# 64_SHAP_Values.R - SHAP decomposition of the Random Forest, h = 3
# SLOW: refits the forest and runs treeshap at every rolling origin.
#
# Mirrors the baseline RF exactly (same design, same window, same mtry), so the
# SHAP values explain the forecasts stored in preds_h3_w*.rds.
# Output: 03_Output/Preds/shap_h3_w*.rds -> used by 65_Rotation_SHAP.R
# ============================================================================
source("01_RScript/00_2_Config.R")
source("01_RScript/00_Functions_Design.R")
library(randomForest)
library(treeshap)
set.seed(123)

data <- readRDS(file.path(P_IN, DATA_FILE)); setDT(data)
data <- data[!is.na(CPIAUCSL)]
setnames(data, "CPIAUCSL", "inf"); setcolorder(data, c("date", "inf"))

# treeshap rewrites non-syntactic names ("S&P 500" -> "S.P.500") and then fails
# to match them, so fix the names once, here.
old <- c("S&P 500", "S&P div yield", "S&P PE ratio")
new <- c("SP500", "SP_div_yield", "SP_PE_ratio")
setnames(data, old[old %in% names(data)], new[old %in% names(data)])
stopifnot(identical(names(data), make.names(names(data), unique = TRUE)))

dates <- data$date
nprev <- sum(dates >= OOS_START)
dum   <- make_dummies(dates)
h     <- 3

Y <- as.matrix(data[, -"date"])
Y <- Y[, colSums(is.na(Y)) == 0, drop = FALSE]

save_shap <- vector("list", nprev)
save_pred <- rep(NA_real_, nprev)

for (i in nprev:1) {
  idx_end   <- nrow(Y) - i
  idx_start <- idx_end - TRAIN_WINDOW + 1
  j <- 1 + nprev - i

  Dw  <- if (is.null(dum)) NULL else dum[idx_start:idx_end, , drop = FALSE]
  des <- make_design(Y[idx_start:idx_end, , drop = FALSE],
                     indice = 1, h = h, nlag = N_LAG, kfac = K_FAC, dum = Dw)

  hp <- get_hp("full", h)
  m <- randomForest(des$X, des$y, mtry = min(hp$rf_mtry, ncol(des$X)),
                    ntree = OPT$rf_ntree)
  save_pred[j] <- as.numeric(predict(m, matrix(des$X.out, nrow = 1,
                              dimnames = list(NULL, names(des$X.out)))))

  x.out  <- as.data.frame(t(des$X.out), check.names = FALSE)
  unif   <- randomForest.unify(m, as.data.frame(des$X, check.names = FALSE))
  sh     <- treeshap(unif, x.out, verbose = FALSE)$shaps
  save_shap[[j]] <- setNames(as.numeric(sh), colnames(sh))

  if (j %% 25 == 0) cat(sprintf("  [SHAP h=3] %d/%d\n", j, nprev))
}

saveRDS(list(dates = tail(dates, nprev), shap = save_shap, pred = save_pred),
        file.path(P_PRED, paste0("shap_h3", WTAG, ".rds")))
cat("SHAP values saved\n")
