
runlasso <- function(Y, indice, lag, alpha=1, type="lasso", lambda, learn_lambda_grid=F, nlambda=25){
  dum  <- Y[,ncol(Y)]
  Y    <- Y[,-ncol(Y)]
  comp <- princomp(scale(Y,scale=FALSE))
  Y2   <- cbind(Y,comp$scores[,1:4])
  aux  <- embed(Y2,4+lag)
  colnames(aux) <-   paste(colnames(Y2), sep="_l", rep(0:(4+lag-1), each=length(colnames(Y2))))
  y    <- aux[,indice]
  X    <- aux[,-c(1:(ncol(Y2)*lag))]  

  if(learn_lambda_grid == T){
    fit_lambdas <- glmnet(X, y, alpha=alpha, standardize=T, nlambda = nlambda)
    lambda_grid <- fit_lambdas$lambda
    if(alpha != 0) {  # Remove lambdas that does not change variable count
      lambda_grid <- lambda_grid[fit_lambdas$df != shift(fit_lambdas$df, fill=-1)]  
    }
    if(alpha != 1) { # Ridge's and Elnet's optimum resides in the minimum lambdas, temporary fix it
      lambda_grid <- lambda_grid/5
    } 
    return(lambda_grid)
  }
  
  if(lag==1){
    X.out <- tail(aux,1)[1:ncol(X)]  
  } else {
    X.out <- aux[,-c(1:(ncol(Y2)*(lag-1)))]  # Possibly wrong, mistake?
    #X.out <- aux[,-c(1:(ncol(Y2)*lag))]
    X.out <- tail(X.out,1)[1:ncol(X)]
  }
  dum=tail(dum,length(y))
  if(type=="quant" || type=="quantadalasso"){
    model <- model=bicqlasso(cbind(X,dum),y,alpha = alpha)
    coef  <- model$coef
    pred  <- c(1,X.out,0)%*%coef
    if(type=="quantadalasso"){
      penalty <- (abs(coef[-1])+1/sqrt(length(y)))^(-1)
      model   <- bicqlasso(cbind(X,dum),y,penalty.factor = penalty,alpha=alpha)
      coef    <- model$coef
      pred    <- c(1,X.out,0)%*%coef
    }
    return(list("pred"=pred,"model"=list("coef"=coef)))
  }
  
  #browser()
  #model <- ic.glmnet(cbind(scale(X),dum),y,alpha = alpha)
  model <- glmnet(cbind(X,dum), y, alpha=alpha, lambda=lambda, standardize=T)
  coef  <- model$beta
  if(type=="adalasso"){
    penalty <- (abs(coef[-1])+1/sqrt(length(y)))^(-1)
    model   <- ic.glmnet(cbind(scale(X),dum),y,penalty.factor = penalty,alpha=alpha)
  }
  #browser()
  #browser()
  pred <- predict(model, c(X.out,0))
  return(list("model"=model,"pred"=pred, "coef_names"=c(colnames(X),"dum")))
}


get_best_lambda <- function(Y,npred,indice=1,lag=1,alpha=1, type="lasso", nlambda=25, window = 360, plot=F) {
  # Create lambda grid automatically
  #Y_train_validate <- Y[1:(nrow(Y)-npred),]
  lambda_grid <- runlasso(Y,indice,lag,alpha,type,0,learn_lambda_grid=T, nlambda=nlambda)
  # Check the grid one by one
  n_validate <- nrow(Y)-window
  save_res <- list(NA)
  for (i in 1:length(lambda_grid)) {
    new_res <- lasso_roll_win(Y, n_validate, indice,lag,alpha,type,lambda_grid[i], plot=plot)
    save_res[[i]] <- c(list("lambda"=lambda_grid[i]), new_res)
  }
  # Select best lambda with lowest rmse
  rmse <- sapply(save_res, function(x) x$errors[1])
  best_lam <- lambda_grid[which.min(rmse)]
  best_lam_all <- list(best_lam = best_lam, all_res = save_res)
  return(best_lam_all)
}


lasso_roll_win <- function(Y,npred,indice=1,lag=1,alpha=1, type="lasso", lambda, plot=T) {
  save.coef <- matrix(NA,npred,21-3+ncol(Y[,-1])*4-1 )
  save.pred <- matrix(NA,npred,1)
  cat("\n iteration \n")
  for(i in npred:1){
    Y.window <- Y[(1+npred-i):(nrow(Y)-i),]
    lasso    <- runlasso(Y.window,indice,lag,alpha,type,lambda)
    save.coef[(1+npred-i),] <- as.vector(lasso$model$beta)
    save.pred[(1+npred-i),] <- lasso$pred
    cat((1+npred-i),"")
  }
  real <- Y[,indice]
  if(plot == T) {
    plot(real,type="l")
    lines(c(rep(NA,length(real)-npred),save.pred),col="red")
  }
  rmse   <- sqrt(mean((tail(real,npred)-save.pred)^2))
  mae    <- mean(abs(tail(real,npred)-save.pred))
  errors <- c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=as.numeric(save.pred),"real"=tail(real,npred),
              "coef"=save.coef, "coef_names"=lasso$coef_names, "errors"=errors))
}


get_best_alpha <- function(Y, npred, indice=1, lag=1, alpha_grid="el", lambda="auto", plot=F, nlambda=25) {
  if(alpha_grid == "el") alpha_grid <- seq(0, 1, 0.1)
  save_res <- list(NA)
  for (i in 1:length(alpha_grid)) {
    if (lambda == "auto") {
      best_lam_all <- get_best_lambda(Y, npred, indice, lag, alpha=alpha_grid[i], nlambda=nlambda)
      lambda_sel <- best_lam_all$best_lam 
    }
    new_res <- lasso_roll_win(Y, npred, indice, lag, alpha_grid[i], "lasso", lambda_sel, plot=plot)
    save_res[[i]] <- c(list("alpha"=alpha_grid[i], "lambda"=lambda_sel), new_res)  # Append the list
  }
  # Select best alpha with lowest rmse
  rmse <- sapply(save_res, function(x) x$errors[1])
  best_alp <- alpha_grid[which.min(rmse)]
  best_lam <- sapply(save_res, function(x) x$lambda)[which.min(rmse)]
  best_alp_all <- list("best_alp"=best_alp, "best_lam"=best_lam, "all_res"=save_res)
  return(best_alp_all)
}


  