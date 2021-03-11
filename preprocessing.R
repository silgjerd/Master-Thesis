rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom);library(recipes);library(rsample)
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



# =========================================================================
# Preprocessing
# =========================================================================

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
df <- rangenorm(df, x_vars, y_vars, split_index, 0.5, -0.5)

# Train test split
train <- df[ split_index,]
test  <- df[-split_index,]

x_train <- train[x_vars] %>% as.matrix
y_train <- train[y_vars] %>% as.matrix

x_test <- test[x_vars] %>% as.matrix
y_test <- test[y_vars] %>% as.matrix


# Write data
filename <- "traintest_exesg"

save(x_train, y_train, x_test, y_test, file = paste0("data/", filename, ".RData"))
write.table(df, file = paste0("data/", filename, ".csv"), append = F, sep = ",", row.names = F)





###################TESTING

# library(corrplot)
# M<-cor(df)
# 
# corrplot(M, method = "color", type = "lower")
# 
# summary(df)
# 
# 
# 


test <- df %>%
  group_by(ymon) %>%
  summarise(n = n())

test <- df %>%
  group_by(PERMNO) %>%
  summarise(n = n())

test <- df %>%
  group_by(FF_12) %>%
  summarise(n = n())
