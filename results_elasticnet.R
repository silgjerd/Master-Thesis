rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(keras);library(caret);library(glmnet);library(xgboost)
source("functions.R")

load("data/tt_macy_esgy.RData")

# =========================================================================
# ELASTIC NET
# =========================================================================

# Build
# control <- trainControl(method = "repeatedcv",
#                         number = 5,
#                         repeats = 1, 
#                         search = "random", 
#                         verboseIter = TRUE) 
# 
# # Train
# elastic_model <- train(RET ~ .,
#                        data = cbind(x_train, y_train), 
#                        method = "glmnet", 
#                        #preProcess = c("center", "scale"), 
#                        tuneLength = 25, 
#                        trControl = control)
# 
# # Test
# pred <- predict(elastic_model, x_test)
# 
# eval <- mevaluate(pred, y_test)
# eval

# plot(pred, y_test)



###########################
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
  
  # Build
  control <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 1, 
                          search = "random", 
                          verboseIter = TRUE) 
  
  # Train
  elastic_model <- train(RET ~ .,
                         data = cbind(cur_x_train, y_train), 
                         method = "glmnet", 
                         #preProcess = c("center", "scale"), 
                         tuneLength = 25, 
                         trControl = control)
  
  pred <- predict(elastic_model, cur_x_test)
  eval <- mevaluate(pred, y_test)
  
  results <- results %>% bind_rows(tibble(
    "esgvar" = esgvar,
    eval
  ))
  
  
  
  
}