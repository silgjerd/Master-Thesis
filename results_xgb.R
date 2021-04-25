rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(keras);library(caret);library(glmnet);library(xgboost);library(SHAPforxgboost)
source("functions.R")

load("data/x_fullsample.RData")

dir_results <- "model_results/results_xgb.csv"

# =========================================================================
# XGBOOST
# =========================================================================

# XGB Model Function
runXGB <- function(
  eta,
  gamma,
  max_depth,
  min_child_weight,
  subsample,
  colsample_bytree,
  alpha,
  lambda,
  nrounds
) {
  
  # Build
  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    tree_method = "auto",
    eta = eta,
    gamma = gamma,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    alpha = alpha,
    lambda = lambda
  )
  
  # Train
  mxgb <- xgb.train(
    params = params,
    data = xgb.DMatrix(x_train, label = y_train),
    nrounds = nrounds,
    early_stopping_rounds = 10,
    watchlist = list(train = xgb.DMatrix(x_train, label = y_train),
                     val = xgb.DMatrix(x_test, label = y_test)),
    eval_metric = "rmse",
    verbose = 0
  )
  
  # Test
  pred <- predict(mxgb, xgb.DMatrix(x_test, label = y_test))
  return(pred)
}

############################################################################


esgvars <- c(    "carbonint",
                 "energyint",
                 "waterint",
                 "wastegen",
                 "femexec",
                 "fememp",
                 "turnemp",
                 "tradeunion",
                 "lostdays",
                 "boardindep",
                 "boardfem",
                 "boardattend",
                 "boardsize",
                 "execcomp",
                 "nonexecs",
                 "boardterm",
                 "boardcomp",
                 
                 "esgscore",
                 "esgcomb",
                 "esgcontr",
                 "esge",
                 "esgg",
                 "esgs",
                 "esgres",
                 "esgemi",
                 "esginn",
                 "esgwor",
                 "esghum",
                 "esgcomm",
                 "esgpro",
                 "esgman",
                 "esgcsr"
                 )

results <- c()
for (esgvar in esgvars){
  curi <- which(colnames(x_train)==esgvar)
  cur_x_train <- x_train[,-curi]
  cur_x_test  <- x_test[,-curi]
  cat(esgvar, "\n")
  
  # Train
  mxgb <- xgb.train(
    
    eta = 0.01,
    gamma = 0,
    max_depth = 15,
    min_child_weight = 5,
    subsample = 1,
    colsample_bytree = 0.5,
    alpha = 1,
    lambda = 1,
    nrounds = 800,
    
    data = xgb.DMatrix(cur_x_train, label = y_train),
    booster = "gbtree",
    objective = "reg:squarederror",
    tree_method = "auto",
    early_stopping_rounds = 10,
    watchlist = list(train = xgb.DMatrix(cur_x_train, label = y_train),
                     val = xgb.DMatrix(cur_x_test, label = y_test)),
    eval_metric = "rmse",
    verbose = 1
    
  )
  
  pred <- predict(mxgb, xgb.DMatrix(cur_x_test, label = y_test))
  eval <- mevaluate(pred, y_test)
  
  results <- results %>% bind_rows(tibble(
    "esgvar" = esgvar,
    eval
  ))
  
  
  
  
}




# Train
mxgb <- xgb.train(
  
  eta = 0.1,
  gamma = 0,
  max_depth = 2,
  min_child_weight = 15,
  subsample = 0.6,
  colsample_bytree = 0.5,
  alpha = 0.1,
  lambda = 0.01,
  nrounds = 800,
  
  data = xgb.DMatrix(x_train, label = y_train),
  booster = "gbtree",
  objective = "reg:squarederror",
  tree_method = "auto",
  early_stopping_rounds = 10,
  watchlist = list(train = xgb.DMatrix(x_train, label = y_train),
                   val = xgb.DMatrix(x_test, label = y_test)),
  eval_metric = "rmse",
  verbose = 1
  
)

# Importance
importance <- xgb.importance(model = mxgb)
importance <- data.table::as.data.table(importance %>% filter(Feature != "macropc1"))
xgb.plot.importance(importance)


# Test
pred <- predict(mxgb, xgb.DMatrix(x_test, label = y_test))
eval <- mevaluate(pred, y_test)

eval
plot(pred, y_test)
abline(a=0,b=1)
abline(a=summary(lm(y_test~pred))$coef[1],b=summary(lm(y_test~pred))$coef[2],col="red")


# SHAP
shap_long <- shap.prep(xgb_model = mxgb, X_train = x_train)
shap_long <- shap_long %>% filter(variable %in% c("macropc1", "r12_2"))
shap.plot.summary(shap_long)


