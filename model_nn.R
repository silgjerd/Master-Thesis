rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(keras);library(caret);library(glmnet);library(xgboost)
source("functions.R")

dir_results <- "model_results/results_nn.csv"

dir_data <- "data/tt_macy_esgy.RData"

getData <- function(dir){
  load(dir)
  
  randsample <- sample(1:nrow(x_train), 10000)
  x_train <- x_train[randsample,]
  y_train <- y_train[randsample,]
}
getData(dir_data)
load(dir_data)

# =========================================================================
# NN
# =========================================================================

Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
options(keras.view_metrics = T)



# =========================================================================
# HYPERPARAMS
# =========================================================================

params <- list(
  "units" = c(64,256,512),
  "layer2" = c(T,F),
  "layer3" = c(T,F),
  "layer4" = c(T,F),
  "dropout" = c(0.1,0.2)
)
params <- list(
  "units" = c(32),
  "layer2" = c(T),
  "layer3" = c(T),
  "layer4" = c(T),
  "layer8" = c(F),
  "dropout" = c(0.2)
)

grid <- params %>% cross_df #hyperparameter grid, all combinations of params

# =========================================================================
# =========================================================================
# RANDOM GRID SEARCH
# =========================================================================

i_sample <- sample(seq_len(nrow(grid))) # random search of hyperparameter grid
i <- 1

for (i in i_sample){
  
  # resample
  # getData(dir_data)
  
  # Build
  model <- keras_model_sequential()
  
  model %>%
    layer_dense(units = grid$units[i],
                activation = "relu") %>%
    #layer_batch_normalization() %>%
    layer_dropout(grid$dropout[i])
  
  if (grid$layer2[i]){
    model %>%
      layer_dense(units = grid$units[i],
                  activation = "relu") %>%
      #layer_batch_normalization() #%>%
      layer_dropout(grid$dropout[i])
  }
  
  if(grid$layer3[i]){
    model %>%
      layer_dense(units = grid$units[i],
                  activation = "relu") %>%
      #layer_batch_normalization() #%>%
      layer_dropout(grid$dropout[i])
  }
  
  if(grid$layer4[i]){
    model %>%
      layer_dense(units = grid$units[i],
                  activation = "relu") %>%
      #layer_batch_normalization() %>%
      layer_dropout(grid$dropout[i])
  }
  
  if(grid$layer8[i]){
    model %>%
      layer_dense(units = grid$units[i],
                  activation = "relu") %>%
      #layer_batch_normalization() %>%
      layer_dropout(grid$dropout[i]) %>%
      
      layer_dense(units = grid$units[i],
                  activation = "relu") %>%
      #layer_batch_normalization() %>%
      layer_dropout(grid$dropout[i]) %>%
      
      layer_dense(units = grid$units[i],
                  activation = "relu") %>%
      #layer_batch_normalization() %>%
      layer_dropout(grid$dropout[i]) %>%
      
      layer_dense(units = grid$units[i],
                  activation = "relu") %>%
      #layer_batch_normalization() %>%
      layer_dropout(grid$dropout[i])
    
    
    
    
    
  }
  
  
  
  # Output layer
  model %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(lr = 0.001)
  )
  
  # Train
  history <- model %>% fit(
    x_train,
    y_train,
    epochs = 100,
    validation_split = 0.2,
    callbacks = list(
      callback_early_stopping(
        monitor = "loss",
        min_delta = 10^-6,
        patience = 5,
        verbose = 1,
        mode = "min",
        restore_best_weights = TRUE
      ))
  )
  
  
  # Test
  pred <- model %>% predict(x_test)
  
  eval <- mevaluate(pred, y_test)
  eval
  plot(pred, y_test)
  
  
  # Write results
  tibble("MSE" = mevaluate(pred, y_test)[["MSE"]],
         "COR" = mevaluate(pred, y_test)[["COR"]],
         "RSQ" = mevaluate(pred, y_test)[["RSQ"]],
         "SCORE" = mevaluate(pred, y_test)[["SCORE"]],
         
         grid[i,],
         
         "executed" = Sys.time(),
         "tag" = "test"
  ) %>%
    write.table(file = dir_results,
                append = T, sep = ",", row.names = F, col.names = !file.exists(dir_results))
  
  cat(round(which(i_sample==i)/nrow(grid),4)*100, "%\n") # PRINT PROGRESS
  
}
