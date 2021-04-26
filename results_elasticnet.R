rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(keras);library(caret);library(glmnet);library(xgboost)
source("functions.R")

load("data/x_fullsample.RData")

# =========================================================================
# ELASTIC NET
# =========================================================================

# Build
control <- trainControl(method = "repeatedcv",
                       number = 5,
                       repeats = 2,
                       search = "random",
                       verboseIter = TRUE)
# Train
elastic_model <- train(RET ~ .,
                      data = cbind(x_train, y_train),
                      method = "glmnet",
                      #preProcess = c("center", "scale"),
                      tuneLength = 25,
                      trControl = control)

# Test
pred <- predict(elastic_model, x_test)
eval <- mevaluate(pred, y_test)

eval
plot(pred, y_test)
abline(a=0,b=1)
abline(a=summary(lm(y_test~pred))$coef[1],b=summary(lm(y_test~pred))$coef[2],col="red")




#####################################################################
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
  
  # Build
  control <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 1,
                          search = "random",
                          verboseIter = TRUE)
  # Train
  elastic_model <- train(RET ~ .,
                         data = cbind(x_train, y_train),
                         method = "glmnet",
                         #preProcess = c("center", "scale"),
                         tuneLength = 25,
                         trControl = control)
  
  # Test
  pred <- predict(elastic_model, x_test)
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











