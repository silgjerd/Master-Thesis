rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(keras);library(caret);library(glmnet);library(xgboost);library(SHAPforxgboost)
source("functions.R")

load("data/x_fullsample.RData")


Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
options(keras.view_metrics = T)

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







# ===========================================================================
# PLOTS
# ===========================================================================

# Boxplot
output %>%
  ggplot(aes(x = sample))+
  geom_boxplot(aes(y = MSE))+
  theme_bw()

# Diff
diff <- output %>%
  group_by(sample) %>%
  summarise(m = mean(MSE))

diff <- diff %>%
  mutate(diff = m - diff$m[diff$sample=="none"])

barplot(diff$diff)

output <- output %>%
  mutate(MSEmcontrol = MSE - diff$m[diff$sample=="none"])


output %>%
  filter(sample!="none") %>%
  mutate(sample = as.factor(sample)) %>%
  mutate(sample = factor(sample, levels=c("ESG","G","S","E"))) %>%
  ggplot(aes(x = sample))+
  geom_boxplot(aes(y = MSEmcontrol),width=.3)+
  geom_hline(yintercept = 0,col="red",linetype="dashed")+
  coord_flip()+
  #scale_y_reverse()+
  theme_bw()



# forest plot
output %>%
  group_by(sample) %>%
  summarise(m = mean(MSE),
            sd = sd(MSE)) %>%
  mutate(
    
    mcontrol = m - mean(output$MSE[output$sample=="none"]),
    sd_up = mcontrol + sd,
    sd_do = mcontrol - sd
    
    ) %>% 
  filter(sample!="none")%>%
  mutate(sample = factor(sample, levels=c("ESG","G","S","E"))) %>%
  ggplot(aes(x=sample))+
  geom_point(aes(y=mcontrol), shape=15, size=4)+
  geom_point(aes(y=sd_up), shape="|", size = 5)+
  geom_point(aes(y=sd_do), shape="|", size = 5)+
  geom_hline(yintercept = 0,col="red",linetype="dashed")+
  coord_flip()+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())





# ===========================================================================
# Category shuffling/randomizing
# ===========================================================================

output <- c()

samples <- list(
  "sample" = c("none","E","S","G",
               #"es","eg","sg",
               #"Scores",
               "ESG"),
  "exvars" = list(esgvars ,esg_e, esg_s, esg_g,
                  #esg_scores,
                  NULL)
)


for (m in 1:1){ # simulations of models
  
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
  
  
  
  for (sample in samples$sample){
    cat(sample,"\n")
    
    for (i in 1:10){ # simulations of noise
      #cat(i,"\n")
      
      # Randomize group of vars
      rand_x_test <- x_test
      
      for (cvar in samples$exvars[[which(samples$sample==sample)]]) {
        #cat(cvar,"\n")
        
        ci <- which(colnames(x_test)==cvar) #index of var
        
        noise <- runif(nrow(x_test), -0.5, 0.5) #randomized values
        # noise <- 0
        
        rand_x_test[,ci] <- noise #replace with randomzied values
      }
      
      # Test
      pred <- model %>% predict(rand_x_test)
      eval <- mevaluate(pred, y_test)
      
      output <- output %>% bind_rows(tibble(
        "sample" = sample,
        eval
      ))
      
    }
    
  }
}



# Boxplot
output %>%
  ggplot(aes(x = sample))+
  geom_boxplot(aes(y = MSE))+
  theme_bw()





############################################################################
# Variable importance

output <- c()

for (ci in seq_len(ncol(x_test))){
  cat(ci,"\n")
  
  rand_x_test <- x_test
  rand_x_test[,ci] <- 0
  
  pred <- model %>% predict(rand_x_test)
  eval <- mevaluate(pred, y_test)
  
  output <- output %>% bind_rows(tibble("var" = colnames(x_test)[ci],
                                        eval))
  
}











