rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(keras);library(caret);library(glmnet);library(xgboost);library(SHAPforxgboost)
source("functions.R")

df <- vroom("data/x_exploration.csv")
df <- tail(df, length(y_test))

# Excess returns
ff <- vroom("data_raw/FF.CSV", col_types = cols(Date = col_date(format = "%Y%m"))) %>%
  mutate(ymon = ymon(Date),
         RF = RF / 100) %>%
  select(ymon, RF)
df <- df %>%
  mutate(ymon = as.character(ymon)) %>%
  left_join(ff, by = "ymon") %>%
  mutate(RET = RET - RF) %>%
  select(-RF)

#######################################################################################



# Appending preds
df <- df %>%
  mutate(pred = pred)


# Quantile trading

trading <- c()
ymons <- unique(df$ymon)[-1]
quant <- 1/4

for (cymon in ymons){
  cat(cymon,"\n")
  
  corder <- order(df[df$ymon==cymon,"pred"])
  sample_len <- floor(nrow(df[df$ymon==cymon,])*quant)
  
  ret_long  <- df$RET[df$ymon==cymon][tail(corder, sample_len)]
  ret_short <- df$RET[df$ymon==cymon][head(corder, sample_len)]
  ret_net   <- ret_long - ret_short
  
  trading <- trading %>% bind_rows(tibble(
    "model" = "en",
    "sample" = "full",
    "ymon" = cymon,
    "sample_len" = sample_len,
    "ret_long" = mean(ret_long),
    "ret_short" = mean(ret_short),
    "ret_net" = mean(ret_net)
  ))
  
  
}


tradeevaluate(trading$ret_net)
cat("SHARPE:",annsharpe(trading$ret_net))


trading %>%
  ggplot(aes(x = ymontodate(ymon)))+
  geom_line(aes(y = cumprod(1+ret_long)),col="green")+
  geom_line(aes(y = cumprod(1+ret_short)),col="red")+
  geom_line(aes(y = cumprod(1+ret_net)))+theme_bw()













