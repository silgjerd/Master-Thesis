rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom);library(recipes);library(rsample);library(caret)
source("functions.R")

# Import
df <- vroom("data/dataset.csv")
df <- df %>% na.omit



# =========================================================================
# FILTERS
# =========================================================================

# After 1993
df <- df %>% filter(ymon >= 199301)

# Variables
df <- df %>%
  
  # Include
  # select(c(
  #   
  # )) %>%
  
  # Exclude
  select(-c(
    carbonint,
    energyint,
    waterint,
    wastegen,
    femexec,
    fememp,
    turnemp,
    tradeunion,
    lostdays,
    boardindep,
    boardfem,
    boardattend,
    boardsize,
    execcomp,
    nonexecs,
    boardterm,
    boardcomp,
    esgscore,
    esgcomb,
    esgcontr,
    esge,
    esgs,
    esgg,
    esgres,
    esgemi,
    esginn,
    esgwor,
    esghum,
    esgcomm,
    esgpro,
    esgman,
    esgcsr
    
  ))

# df <- df %>% select(-macropc1)

# =========================================================================
# Preprocessing
# =========================================================================

# Industry dummies
library(mltools);library(data.table)
dummies <- one_hot(as.data.table(as.factor(df$FF_12)))
colnames(dummies) <- paste0("ind_", 1:12)
df <- bind_cols(df, dummies)

# Sort by ymon
df <- df %>% arrange(ymon)


# identifiers <- df %>%
#   select(c(
#     PERMNO,
#     SICCD,
#     CUSIP,
#     PRC,
#     VOL,
#     SHROUT,
#     LOGRET,
#     ymon,
#     FF_12
#   ))


# Exclude variables
df <- df %>%
  select(-c(
    PERMNO,
    SICCD,
    CUSIP,
    PRC,
    VOL,
    SHROUT,
    LOGRET,
    ymon,
    FF_12
  ))





# =========================================================================
# Outlier treatment
# =========================================================================

df <- df %>%
  mutate(
    
    suv = replace(suv, suv > 10, 10)
  )

# =========================================================================
# Train test split
# =========================================================================
# PARAMS
split_ratio <- 0.75
x_vars <- setdiff(colnames(df), "RET") #all except RET
y_vars <- "RET"

split_index <- 1:floor(split_ratio * nrow(df))

# Normalizing
df <- rangenorm(df, x_vars, y_vars, split_index, -0.5, 0.5)

# Train test split
train <- df[ split_index,]
test  <- df[-split_index,]

x_train <- train[x_vars] %>% as.matrix
y_train <- train[y_vars] %>% as.matrix

x_test <- test[x_vars] %>% as.matrix
y_test <- test[y_vars] %>% as.matrix


# Write data
filename <- "x_fullsample"

save(x_train, y_train, x_test, y_test, file = paste0("data/", filename, ".RData"))
write.table(df, file = paste0("data/", filename, ".csv"), append = F, sep = ",", row.names = F)




