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


######################## SHAP FUNCTIONS ############################

# Requires: library(treeshap)
# Usage: works for both OOS periods — just pass the correct Y and nprev.
# Skips importance entirely to save time.
# Returns: pred, real, errors, save.shap (list of length nprev,
#          each element is a named numeric vector of length ncol(X))

# ── Core function: fit RF + compute SHAP for the single OOS observation ───────

runrf_shap = function(Y, indice, lag, nfeature) {
  
  comp = princomp(scale(Y, scale = FALSE))
  Y2   = cbind(Y, comp$scores[, 1:4])
  aux  = embed(Y2, 4 + lag)
  y    = aux[, indice]
  X    = aux[, -c(1:(ncol(Y2) * lag))]
  
  if (lag == 1) {
    X.out = tail(aux, 1)[1:ncol(X)]
  } else {
    X.out = aux[, -c(1:(ncol(Y2) * (lag - 1)))]
    X.out = tail(X.out, 1)[1:ncol(X)]
  }
  
  # Fit RF — importance=FALSE saves time since we only want SHAP
  model = randomForest(X, y, importance = FALSE, mtry = nfeature)
  pred  = predict(model, X.out)
  
  # Compute SHAP for the single OOS observation
  rf_unified  = randomForest.unify(model, X)
  shap_result = treeshap(rf_unified,
                         x            = as.data.frame(matrix(X.out, nrow = 1,
                                                             dimnames = list(NULL, colnames(X)))),
                         interactions = FALSE,
                         verbose      = FALSE)
  
  # Named vector of length ncol(X): one SHAP value per feature
  shap_vec = setNames(as.numeric(shap_result$shaps[1, ]), colnames(X))
  
  return(list("pred" = pred, "shap" = shap_vec))
}


# ── Rolling window wrapper ────────────────────────────────────────────────────

rf.rolling.window_shap = function(Y, nprev, indice = 1, lag = 1, nfeature = best_mtry) {
  
  save.shap = list()
  save.pred = matrix(NA, nprev, 1)
  
  for (i in nprev:1) {
    Y.window      = Y[(1 + nprev - i):(nrow(Y) - i), ]
    rf_out        = runrf_shap(Y.window, indice, lag, nfeature)
    save.pred[(1 + nprev - i), ] = rf_out$pred
    save.shap[[1 + nprev - i]]   = rf_out$shap
    cat("SHAP iteration", (1 + nprev - i), "\n")
  }
  
  real  = Y[, indice]
  rmse  = sqrt(mean((tail(real, nprev) - save.pred)^2))
  mae   = mean(abs(tail(real, nprev) - save.pred))
  
  return(list("pred"      = save.pred,
              "real"      = real,
              "errors"    = c("rmse" = rmse, "mae" = mae),
              "save.shap" = save.shap))
}










