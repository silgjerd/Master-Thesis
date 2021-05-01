rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(keras);library(caret);library(glmnet);library(xgboost)
source("functions.R")

dir_results <- "model_results/results_nn.csv"

dir_data <- "data/x_fullsample.RData"

# getData <- function(dir){
#   load(dir)
#   
#   randsample <- sample(1:nrow(x_train), 10000)
#   x_train <- x_train[randsample,]
#   y_train <- y_train[randsample,]
# }
# getData(dir_data)
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
  "units" = c(16,32,64,128,256,512,1024,2048),
  "layer2" = c(T),
  "layer3" = c(T,F),
  "layer4" = c(T,F),
  "dropout" = c(0,0.1,0.2,0.5),
  "batchnorm" = c(F)
)

params <- list(
  "units" = c(256),
  "layer2" = c(T),
  "layer3" = c(T),
  "layer4" = c(F),
  "dropout" = c(0.1)
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
  
  # LAYER 1
  model %>%
    layer_dense(units = grid$units[i],
                activation = "relu")
  
  # model %>% layer_batch_normalization()
  model %>% layer_dropout(grid$dropout[i])
  
  # LAYER 2
  if (grid$layer2[i]){
    model %>%
      layer_dense(units = grid$units[i]/2,
                  activation = "relu")
    
    # model %>% layer_batch_normalization()
    model %>% layer_dropout(grid$dropout[i])
  }
  
  # LAYER 3
  if(grid$layer3[i]){
    model %>%
      layer_dense(units = grid$units[i]/4,
                  activation = "relu")
    
    # model %>% layer_batch_normalization()
    model %>% layer_dropout(grid$dropout[i])
  }
  
  # LAYER 4
  if(grid$layer4[i]){
    model %>%
      layer_dense(units = grid$units[i]/8,
                  activation = "relu")
    
    # model %>% layer_batch_normalization()
    model %>% layer_dropout(grid$dropout[i])
  }
  
  # Output layer
  model %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(lr = 0.00001)
  )
  
  # Train
  history <- model %>% fit(
    x_train,
    y_train,
    epochs = 200,
    validation_split = 0.2,
    callbacks = list(
      callback_early_stopping(
        monitor = "val_loss",
        min_delta = 0.0001,
        patience = 10,
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
  abline(a=0,b=1)
  abline(a=summary(lm(y_test~pred))$coef[1],b=summary(lm(y_test~pred))$coef[2],col="red")
  
  
  # Write results
  tibble(eval,
         
         grid[i,],
         
         "executed" = Sys.time(),
         "tag" = "equal"
  ) %>%
    write.table(file = dir_results,
                append = T, sep = ",", row.names = F, col.names = !file.exists(dir_results))
  
  cat(round(which(i_sample==i)/nrow(grid),4)*100, "%\n") # PRINT PROGRESS
  
}
