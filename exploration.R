rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(PerformanceAnalytics);library(RColorBrewer);library(corrplot)
source("functions.R")

df <- vroom("data/traintest_allwithidentifiers.csv")




# =========================================================================
# ALPHA (move to seperate file?) ATTEMPT 2
# =========================================================================

output <- c()

quant <- 1/10
ymons <- unique(df$ymon)

for (cvar in colnames(df)[1:67]){
  
  cat(cvar, "\n")
  
  for (cymon in ymons){
    
    
    cdf <- df %>% filter(ymon == cymon)
    
    # sort
    corder <- order(cdf[cvar])
    cdf <- cdf[corder,]
    
    # rets
    ret_long  <- cdf$LOGRET %>% tail(floor(nrow(cdf)*quant))
    ret_short <- cdf$LOGRET %>% head(floor(nrow(cdf)*quant))
    ret_net   <- ret_long - ret_short
    
    # output
    output <- output %>% bind_rows(tibble("meanlong" = mean(ret_long),
                                          "sdlong" = sd(ret_long),
                                          "meanshort" = mean(ret_short),
                                          "sdshort" = sd(ret_short),
                                          "meannet" = mean(ret_net),
                                          "sdnet" = sd(ret_net),
                                          "n" = length(ret_net),
                                          "var" = cvar
    ))
    
  }
}

aggtest <- output %>%
  group_by(var) %>%
  summarise(ret = mean(meannet),
            sd  = sd(meannet),
            n = n())

aggtest <- aggtest %>%
  mutate(tstat = ret / (sd / sqrt(n))) %>%
  arrange(desc(abs(tstat)))









# =========================================================================
# Correlation matrix scatterplot
# =========================================================================

M <- cor(df)
corrplot(M, method = "color", type = "lower")

