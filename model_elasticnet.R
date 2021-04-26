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



#OLS TEST
traindat <- data.frame("lme" = x_train[,"lme"],
                       "bm" = x_train[,"bm"],
                       "r12_2" = x_train[,"r12_2"])
ols <- lm(y_train ~ lme + bm + r12_2, traindat)

traindat <- data.frame("suv" = x_train[,"suv"])
ols <- lm(y_train ~ suv, traindat)

pred <- predict(ols, newdata = as.data.frame(x_test))

