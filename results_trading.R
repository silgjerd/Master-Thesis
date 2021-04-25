rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(keras);library(caret);library(glmnet);library(xgboost);library(SHAPforxgboost)
source("functions.R")

df <- vroom("data/x_exploration.csv")
df <- tail(df, length(y_test))

#######################################################################################


# Residuals
e <- pred - y_test

# Appending preds
df <- df %>%
  mutate(pred = pred,
         e = e)


# Quantile trading

trading <- c()
ymons <- unique(df$ymon)
quant <- 1/2

for (cymon in ymons){
  cat(cymon,"\n")
  
  corder <- order(df[df$ymon==cymon,"pred"])
  sample_len <- floor(nrow(df[df$ymon==cymon,])*quant)
  
  ret_long  <- df$RET[df$ymon==cymon][tail(corder, sample_len)]
  ret_short <- df$RET[df$ymon==cymon][head(corder, sample_len)]
  ret_net   <- ret_long - ret_short
  
  trading <- trading %>% bind_rows(tibble(
    "ymon" = cymon,
    "sample_len" = sample_len,
    "ret_long" = mean(ret_long),
    "ret_short" = mean(ret_short),
    "ret_net" = mean(ret_net)
  ))
  
  
}


tradeevaluate(trading$ret_net)




