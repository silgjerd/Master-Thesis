rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom);library(recipes);library(rsample)
library(keras)
source("functions.R")

df <- vroom("data/dataset.csv")
df <- df %>% na.omit


# =========================================================================
# Preprocessing
# =========================================================================

# Exclude variables
df <- df %>%
  select(-c(
    PERMNO,
    SICCD,
    PRC,
    VOL,
    SHROUT,
    LOGRET,
    ymon,
    FF_12
  ))

# PARAMS
split_ratio <- 0.75
x_vars <- colnames(df)[-1] #hack
y_vars <- "RET"

split_index <- 1:floor(split_ratio * nrow(df))

# Normalizing
df <- rangenorm(df, x_vars, y_vars, split_index, 0.5, -0.5)

# Train test split
train <- df[ split_index,]
test  <- df[-split_index,]

x_train <- train[x_vars] %>% as.matrix
y_train <- train[y_vars] %>% as.matrix

x_test <- test[x_vars] %>% as.matrix
y_test <- test[y_vars] %>% as.matrix


# Write data
save(x_train, y_train, x_test, y_test, file = "data/traintest.RData")
write.table(df, file = "data/traintest.csv", append = F, sep = ",", row.names = F)

