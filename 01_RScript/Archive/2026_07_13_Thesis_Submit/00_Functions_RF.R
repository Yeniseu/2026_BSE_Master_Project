# Author: Ece Tasan
# Date: 3/12/2025
# Scope: Random Forest Functions for Tuning and Prediction


#### TUNING FUNCTIONS ####

# rf function ()
runrf_val_mtry=function(Y,indice,lag, nfeature){
  comp=princomp(scale(Y,scale=FALSE))
  Y2=cbind(Y,comp$scores[,1:4])
  aux=embed(Y2,4+lag)
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))]  
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }
  
  
  model=randomForest(X,y,importance=TRUE, mtry = nfeature)
  pred=predict(model,X.out)
  
  return(list("model"=model,"pred"=pred))
}



# rolling window setting for mtry tuning
rf.rolling.window_tune_mtry=function(Y,nprev,indice=1,lag=1, nfeature){
  
  save.importance=list()
  save.pred=matrix(NA,nprev,1)
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    random_forest=runrf_val_mtry(Y.window,indice,lag,nfeature)
    save.pred[(1+nprev-i),]=random_forest$pred
    save.importance[[1 + nprev - i]] = importance(random_forest$model)
    cat("iteration",(1+nprev-i),"\n")
  }
  
  real=Y[,indice]
  
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2))
  mae=mean(abs(tail(real,nprev)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"real"=real,"errors"=errors,"save.importance"=save.importance))
}



#### PREDICTION FUNCTIONS ####

# FIRST OUT OF SAMPLE PERIOD: 2001-2015

# rf function
runrf=function(Y,indice,lag, nfeature){
  
  comp=princomp(scale(Y,scale=FALSE))
  Y2=cbind(Y,comp$scores[,1:4])
  aux=embed(Y2,4+lag)
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))]  
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }
  
  model=randomForest(X,y,importance = TRUE, mtry = nfeature)
  pred=predict(model,X.out)
  
  return(list("model"=model,"pred"=pred))
}


# rolling window setting
rf.rolling.window=function(Y,nprev,indice=1,lag=1,nfeature = best_mtry){
  
  save.importance=list()
  save.pred=matrix(NA,nprev,1)
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    random_forest=runrf(Y.window,indice,lag,nfeature)
    save.pred[(1+nprev-i),]=random_forest$pred
    save.importance[[1 + nprev - i]] = importance(random_forest$model)
    cat("iteration",(1+nprev-i),"\n")
  }
  
  real=Y[,indice]
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red")
  
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2))
  mae=mean(abs(tail(real,nprev)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"real"=real,"errors"=errors,"save.importance"=save.importance))
}

# SECOND OUT OF SAMPLE PERIOD: 2016-2024

runrf_second=function(Y,indice,lag,nfeature){
  comp=princomp(scale(Y,scale=FALSE))
  Y2=cbind(Y,comp$scores[,1:4])
  aux=embed(Y2,4+lag)
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))]  
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }
  
  
  model=randomForest(X,y,importance=TRUE, mtry = nfeature)
  pred=predict(model,X.out)
  
  return(list("model"=model,"pred"=pred))
}


rf.rolling.window_second=function(Y,nprev,indice=1,lag=1, nfeature = best_mtry){
  
  save.importance=list()
  save.pred=matrix(NA,nprev,1)
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    random_forest=runrf_second(Y.window,indice,lag,nfeature)
    save.pred[(1+nprev-i),]=random_forest$pred
    save.importance[[1 + nprev - i]] = importance(random_forest$model)
    cat("iteration",(1+nprev-i),"\n")
  }
  
  real=Y[,indice]
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red")
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2))
  mae=mean(abs(tail(real,nprev)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"real"=real,"errors"=errors,"save.importance"=save.importance))
}

#### SHAP FUNCTIONS ####
# Requires: library(treeshap), library(randomForest)
#
# NOTE: This function assumes the column names of Y are R-syntactic
# (no spaces, "&", etc.). Clean them BEFORE calling — see the runner script.

runrf_shap <- function(Y, indice, lag, nfeature) {
  
  # Defensive check — fail loudly if Y has non-syntactic names
  if (!identical(colnames(Y), make.names(colnames(Y), unique = TRUE))) {
    bad <- setdiff(colnames(Y), make.names(colnames(Y), unique = TRUE))
    stop("runrf_shap: Y has non-syntactic column names. Clean them first. Offending names: ",
         paste(head(bad, 5), collapse = ", "))
  }
  
  comp <- princomp(scale(Y, scale = FALSE))
  Y2   <- cbind(Y, comp$scores[, 1:4])
  colnames(Y2) <- c(colnames(Y), paste0("Comp.", 1:4))
  
  aux <- embed(Y2, 4 + lag)
  y   <- aux[, indice]
  X   <- aux[, -c(1:(ncol(Y2) * lag))]
  
  # Feature names: y2_name + "_lag" + lag index
  y2_names   <- colnames(Y2)
  lag_labels <- paste0("lag", seq(lag, lag + 3))
  feat_names <- paste0(rep(y2_names, times = 4), "_",
                       rep(lag_labels, each = ncol(Y2)))
  colnames(X) <- feat_names
  
  if (lag == 1) {
    X.out <- tail(aux, 1)[1:ncol(X)]
  } else {
    X.out <- aux[, -c(1:(ncol(Y2) * (lag - 1)))]
    X.out <- tail(X.out, 1)[1:ncol(X)]
  }
  names(X.out) <- feat_names
  
  X_df     <- as.data.frame(X, check.names = FALSE)
  X_out_df <- as.data.frame(t(X.out), check.names = FALSE)
  
  # Fit RF
  model <- randomForest(X_df, y, importance = FALSE, mtry = nfeature)
  pred  <- predict(model, X_out_df)
  
  # SHAP
  rf_unified  <- randomForest.unify(model, X_df)
  shap_result <- treeshap(rf_unified,
                          x            = X_out_df,
                          interactions = FALSE,
                          verbose      = FALSE)
  
  shap_vec <- setNames(as.numeric(shap_result$shaps[1, ]), feat_names)
  
  return(list("pred"       = pred,
              "shap"       = shap_vec,
              "feat_names" = feat_names))
}

# ── Rolling window wrapper ────────────────────────────────────────────────────
rf.rolling.window_shap <- function(Y, nprev, indice = 1, lag = 1, nfeature = best_mtry) {
  
  save.shap  <- list()
  save.pred  <- matrix(NA, nprev, 1)
  feat_names <- NULL
  
  for (i in nprev:1) {
    Y.window           <- Y[(1 + nprev - i):(nrow(Y) - i), ]
    colnames(Y.window) <- colnames(Y)
    rf_out             <- runrf_shap(Y.window, indice, lag, nfeature)
    save.pred[(1 + nprev - i), ] <- rf_out$pred
    save.shap[[1 + nprev - i]]   <- rf_out$shap
    if (is.null(feat_names)) feat_names <- rf_out$feat_names
    cat("SHAP iteration", (1 + nprev - i), "\n")
  }
  
  real <- Y[, indice]
  rmse <- sqrt(mean((tail(real, nprev) - save.pred)^2))
  mae  <- mean(abs(tail(real, nprev) - save.pred))
  
  return(list("pred"       = save.pred,
              "real"       = real,
              "errors"     = c("rmse" = rmse, "mae" = mae),
              "save.shap"  = save.shap,
              "feat_names" = feat_names))
}
