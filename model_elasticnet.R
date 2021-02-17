rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(keras);library(caret);library(glmnet);library(xgboost)

load("data/traintest.RData")

# =========================================================================
# ELASTIC NET
# =========================================================================

# Build
control <- trainControl(method = "repeatedcv",
                        number = 5, 
                        repeats = 5, 
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


cat("MSE: ", mean((pred - y_test)^2), "\n") 
cat("CORREL: ", cor(pred, y_test), "\n")
# plot(enpred, y_test)

