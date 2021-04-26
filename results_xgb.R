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

esg_e <- c("carbonint",
           "energyint",
           "waterint",
           "wastegen",
           "esge",
           "esgres",
           "esgemi",
           "esginn"
)
esg_s <- c("femexec",
           "fememp",
           "turnemp",
           "tradeunion",
           "lostdays",
           "esgs",
           "esgwor",
           "esghum",
           "esgcomm",
           "esgpro"
)

esg_g <- c("boardindep",
           "boardfem",
           "boardattend",
           "boardsize",
           "execcomp",
           "nonexecs",
           "boardterm",
           "boardcomp",
           "esgg",
           "esgman",
           "esgcsr")

esg_scores <- c("esgscore","esgcomb","esgcontr")
#####################################################################


# Model
getEVAL <- function(x_train, y_train, x_test, y_test){
  
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
  
  pred <- predict(mxgb, xgb.DMatrix(x_test, label = y_test))
  eval <- mevaluate(pred, y_test)
  return(eval)
}




output <- c()

samples <- list(
  "sample" = c("none","e","s","g","es","eg","sg","all"),
  "excols" = list(c(which(colnames(x_train) %in% esgvars)),
                  c(which(colnames(x_train) %in% c(esg_s, esg_g))),
                  c(which(colnames(x_train) %in% c(esg_e, esg_g))),
                  c(which(colnames(x_train) %in% c(esg_e, esg_s))),
                  c(which(colnames(x_train) %in% c(esg_g))),
                  c(which(colnames(x_train) %in% c(esg_s))),
                  c(which(colnames(x_train) %in% c(esg_e))),
                  NULL)
)

for (sample in samples$sample){
  cat(sample,":")
  cat(samples$excols[[which(samples$sample==sample)]],"\n")
  
  excols <- samples$excols[[which(samples$sample==sample)]]
  if (is.null(excols)){
    cur_x_train <- x_train
    cur_x_test  <- x_test
  } else {
    cur_x_train <- x_train[,-excols]
    cur_x_test  <- x_test[,-excols]
  }
  
  
  for (i in 1:20){
    cat(i,"\n")
    
    eval <- getEVAL(cur_x_train, y_train, cur_x_test, y_test)
    
    output <- output %>% bind_rows(tibble(
      "sample" = sample,
      eval
    ))
    
  }
  
}







# Boxplot

output %>%
  ggplot(aes(x = sample))+
  geom_boxplot(aes(y = MSE))+
  theme_bw()






############################################################################

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


