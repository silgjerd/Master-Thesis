library(forecast)


# =================================================================================
# DM TEST

# pred_en  <- predict(elastic_model, x_test)
# pred_xgb <- predict(mxgb, xgb.DMatrix(x_test, label = y_test))



e_1 <- pred_1 - y_test
e_2 <- pred_2 - y_test


dm.test(e1 = e_1, e2 = e_2)


e_1 <- (pred_1 - y_test)^2
e_2 <- (pred_2 - y_test)^2


dm.test(e1 = e_1, e2 = e_2, power = 2)

# =================================================================================
# DM TEST ALL

df <- vroom("preds_allmodels.csv")
# df <- df %>% mutate(y_test = y_test)
df <- (df - y_test)^2
plot(colMeans(df))


e1 <- (df$EN_ESG - y_test)^2
e2 <- (df$XGB_E - y_test)^2




# LOOP
dmout <- matrix(nrow = ncol(df), ncol = ncol(df))
for (i in seq_len(ncol(df))){
  for (j in seq_len(ncol(df))){
    cat(i,j,"\n")
    
    if (i==j){
      dmout[i,j] <- NA
      next
    }
    
    cdm <- dm.test(e1 = df[,i], e2 = df[,j], power = 2)
    
    dmout[i,j] <- cdm[["statistic"]]
    
  }
}







# =================================================================================
# SAMPLES

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


# LOOP
output <- c()

samples <- list(
  "sample" = c(#"none"#,
               #"E",
               "S"#,
               #"G",
               #"es","eg","sg",
               #"Scores"#,
               #"ESG"
               ),
  "exvars" = list(#esgvars#,
                  #esg_e,
                  esg_s,
                  esg_g,
                  esg_scores#,
                  #NULL
                  )
)


for (m in 1:1){ # simulations of models
  # control <- trainControl(method = "repeatedcv",
  #                         number = 5,
  #                         repeats = 5,
  #                         search = "random",
  #                         verboseIter = TRUE)
  # 
  # elastic_model <- train(RET ~ .,
  #                        data = cbind(x_train, y_train),
  #                        method = "glmnet",
  #                        #preProcess = c("center", "scale"),
  #                        tuneLength = 25,
  #                        trControl = control)
  
  
  
  
  
  for (sample in samples$sample){
    cat(sample,"\n")
    
    for (i in 1:100){ # simulations of noise
      #cat(i,"\n")
      
      # Randomize group of vars
      rand_x_test <- x_test
      
      #for (cvar in samples$exvars[[which(samples$sample==sample)]]) {
      for (cvar in c(esg_e, esg_s, esg_scores)) { # which vars to shuffle
        #cat(cvar,"\n")
        
        ci <- which(colnames(x_test)==cvar) #index of var
        
        noise <- runif(nrow(x_test), -0.5, 0.5) #randomized values
        # noise <- 0
        
        rand_x_test[,ci] <- noise #replace with randomzied values
      }
      
      # Test
      # pred <- predict(elastic_model, rand_x_test)
      # pred <- predict(mxgb, xgb.DMatrix(rand_x_test, label = y_test))
      pred <- model %>% predict(rand_x_test)
      eval <- mevaluate(pred, y_test)
      
      # output <- output %>% bind_rows(tibble(
      #   "sample" = sample,
      #   eval
      # ))
      
      output <- output %>% bind_cols(tibble(
        pred
      ))
      
    }
    
  }
}

pred <- rowMeans(output)
pred_1 <- rowMeans(output)
pred_2 <- rowMeans(output)



write.table(pred, "predasd.csv",sep=",",row.names = F, append = F)

# =================================================================================
# EN

control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        search = "random",
                        verboseIter = TRUE)

elastic_model <- train(RET ~ .,
                       data = cbind(x_train, y_train),
                       method = "glmnet",
                       #preProcess = c("center", "scale"),
                       tuneLength = 25,
                       trControl = control)


# =================================================================================
# XGB

mxgb <- xgb.train( #0.01
  
  eta = 0.01,
  gamma = 0,
  max_depth = 3,
  min_child_weight = 5,
  subsample = 0.8,
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
  verbose = 0
  
)


# =================================================================================
# NN


model <- keras_model_sequential()

# LAYER 1
model %>%
  layer_dense(units = 512,
              activation = "relu"#, 
              #kernel_regularizer = regularizer_l1(0.01)
  )

model %>% layer_batch_normalization()
model %>% layer_dropout(0.6)

# LAYER 2
model %>%
  layer_dense(units = 512/2,
              activation = "relu")

model %>% layer_batch_normalization()
model %>% layer_dropout(0.6)


# LAYER 3

model %>%
  layer_dense(units = 512/4,
              activation = "relu")

model %>% layer_batch_normalization()
model %>% layer_dropout(0.6)


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

