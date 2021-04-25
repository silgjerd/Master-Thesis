rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(PerformanceAnalytics);library(RColorBrewer);library(corrplot)
source("functions.R")

df <- vroom("data/x_exploration.csv")





# =========================================================================
# Correlation matrix scatterplot
# =========================================================================

M <- cor(df)
corrplot(M, method = "color", type = "lower")



# =========================================================================
# Averages over time
# =========================================================================


df %>%
  group_by(ymon) %>%
  summarise(n = n()) %>%
  plot
  

