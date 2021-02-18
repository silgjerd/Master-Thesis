rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(keras);library(caret);library(glmnet);library(xgboost)
source("functions.R")
load("data/traintest.RData")

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


# =========================================================================
# HYPERPARAMS
# =========================================================================
params <- list(
  "eta" = c(0.001,0.01,0.1),
  "gamma" = c(0,1),
  "max_depth" = c(7,15,25),
  "min_child_weight" = c(1,3,5,7),
  "subsample" = c(1),
  "colsample_bytree" = c(0.3,0.5),
  "alpha" = c(0,0.5,1),
  "lambda" = c(0.01,0.1,1),
  "nrounds" = c(800)
)

grid <- params %>% cross_df

# =========================================================================
# =========================================================================
# RANDOM GRID SEARCH
# =========================================================================

i_sample <- sample(seq_len(nrow(grid))) #

for (i in i_sample){
  
  # Fit model and get predictions
  pred <- runXGB(  
    eta = grid$eta[i],
    gamma = grid$gamma[i],
    max_depth = grid$max_depth[i],
    min_child_weight = grid$min_child_weight[i],
    subsample = grid$subsample[i],
    colsample_bytree = grid$colsample_bytree[i],
    alpha = grid$alpha[i],
    lambda = grid$lambda[i],
    nrounds = grid$nrounds[i]
  )
  
  # Write results
  tibble("MSE" = mean((pred - y_test)^2),
         "CORREL" = cor(pred, y_test)[1],
         "eta" = grid$eta[i],
         "gamma" = grid$gamma[i],
         "max_depth" = grid$max_depth[i],
         "min_child_weight" = grid$min_child_weight[i],
         "subsample" = grid$subsample[i],
         "colsample_bytree" = grid$colsample_bytree[i],
         "alpha" = grid$alpha[i],
         "lambda" = grid$lambda[i],
         "nrounds" = grid$nrounds[i]
  ) %>%
    write.table(file = "model_results/results_xgb.csv",
                append = T, sep = ",", row.names = F, col.names = F)
  
  # cat("\n=\n=\n=\n=\n=", round(i/nrow(grid),4)*100, "%\n=\n=\n=\n=") # PRINT PROGRESS
  cat(round(i/nrow(grid),4)*100, "%\n") # PRINT PROGRESS
  
}














