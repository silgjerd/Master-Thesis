rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom);library(lubridate)
library(PerformanceAnalytics);library(RColorBrewer);library(corrplot);library(stargazer)
source("functions.R")

df <- vroom("data/x_exploration.csv")




# =========================================================================
# ALPHA (move to seperate file?) ATTEMPT 2
# =========================================================================

# hold_period <- 12
# df <- df %>%
#   group_by(PERMNO) %>%
#   mutate(holdret = rollsum(LOGRET, hold_period, fill = NA, align = "left")) %>%
#   na.omit

df <- df %>%
  group_by(PERMNO) %>%
  # mutate(holdret = lead(LOGRET, 1)) %>%
  mutate(holdret = LOGRET) %>%
  na.omit %>% ungroup

# remove identifiers
df <- df %>%
  select(-c(
    PERMNO,
    SICCD,
    CUSIP,
    PRC,
    VOL,
    SHROUT,
    LOGRET,
    RET,
    #ymon,
    FF_12,
    ind_1,ind_2,ind_3,ind_4,ind_5,ind_6,ind_7,ind_8,ind_9,ind_10,ind_11,ind_12
  ))


output <- c()

quant <- 1/4
ymons <- unique(df$ymon)
cvars <- setdiff(colnames(df), c("ymon", "holdret"))


for (cvar in cvars){
  
  cat(which(cvars == cvar), "/ 67\n")
  
  for (cymon in ymons){
    
    #cat(cymon, "\n")
    
    # cdf <- df %>% filter(ymon == cymon)
    
    # sort
    corder <- order(df[df$ymon==cymon,cvar])
    
    # rets
    sample_len <- floor(nrow(df[df$ymon==cymon,])*quant)
    ret_long  <- df$holdret[df$ymon==cymon][tail(corder, sample_len)] #head or tail? think its correct now
    ret_short <- df$holdret[df$ymon==cymon][head(corder, sample_len)]
    ret_net   <- ret_long - ret_short
    
    
    # output
    output <- output %>% bind_rows(tibble("meanlong" = mean(ret_long),
                                          "sdlong" = sd(ret_long),
                                          "meanshort" = mean(ret_short),
                                          "sdshort" = sd(ret_short),
                                          "meannet" = mean(ret_net),
                                          "sdnet" = sd(ret_net),
                                          "n" = length(ret_net),
                                          "ymon" = cymon,
                                          "var" = cvar

    ))
    # output <- output %>% bind_rows(tibble("retlong" = ret_long,
    #                                       "retshort" = ret_short,
    #                                       "var" = cvar
    #                                       
    # ))
    
  }
  
}

output <- output %>% na.omit
write.table(output, "data/data_alphaoutput.csv", append = F, row.names = F, sep = ",")

# aggtest <- output %>%
#   group_by(var) %>%
#   summarise(ret = mean(meannet),
#             sd  = sd(meannet),
#             # sd  = mean(sdnet),
#             n = n())
# 
# aggtest <- aggtest %>%
#   mutate(tstat = ret / (sd / sqrt(n))) %>%
#   arrange(desc(abs(tstat)))
# 
# 
# newagg <- output %>%
#   group_by(var) %>%
#   summarise(mlong = mean(retlong),
#             mshort = mean(retshort),
#             n = n(),
#             ret = mlong - mshort)
# 
# newagg <- newagg %>%
#   mutate(tstat = ret / (sd / sqrt(n))) %>%
#   arrange(desc(abs(tstat)))

# =========================================================================
# FF
# =========================================================================
output <- read.csv("data/data_alphaoutput.csv", sep = ",")

ff <- vroom("data_raw/FF5F.CSV", col_types = cols(Date = col_date(format = "%Y%m")))
ff <- ff %>%
  mutate(`Mkt-RF` = `Mkt-RF` / 100,
         SMB = SMB / 100,
         HML = HML / 100,
         RMW = RMW / 100,
         CMA = CMA / 100,
         MOM = MOM / 100,
         RF = RF / 100,
         ymon = ymon(Date)) %>%
  select(-Date)

output <- output %>% mutate(ymon = as.character(ymon))
newdf <- left_join(output, ff, by = "ymon")



lm(I(meannet-RF) ~ `Mkt-RF` + SMB + HML, subset(newdf, var=="lme")) %>% summary
lm(I(meannet-RF) ~ `Mkt-RF` + SMB + HML + MOM, subset(newdf, var=="lme")) %>% summary
lm(I(meannet-RF) ~ `Mkt-RF` + SMB + HML + RMW + CMA, subset(newdf, var=="lme")) %>% summary
lm(I(meannet-RF) ~ `Mkt-RF` + SMB + HML + RMW + CMA + MOM, subset(newdf, var=="lme")) %>% summary


lm(I(meannet-RF) ~ `Mkt-RF` + SMB + HML, subset(newdf, var=="lme")) %>%
  summary %>% .[["coefficients"]] %>% .[1,1:3]

# get all models
outtable <- c()
for (cvar in unique(newdf$var)){
  
  # FF3
  ff3 <- lm(I(meannet-RF) ~ `Mkt-RF` + SMB + HML, subset(newdf, var==cvar)) %>%
    summary %>% .[["coefficients"]] %>% .[1,1:3]
  ff3m <- lm(I(meannet-RF) ~ `Mkt-RF` + SMB + HML + MOM, subset(newdf, var==cvar)) %>% 
    summary %>% .[["coefficients"]] %>% .[1,1:3]
  ff5 <- lm(I(meannet-RF) ~ `Mkt-RF` + SMB + HML + RMW + CMA, subset(newdf, var==cvar)) %>% 
    summary %>% .[["coefficients"]] %>% .[1,1:3]
  ff5m <- lm(I(meannet-RF) ~ `Mkt-RF` + SMB + HML + RMW + CMA + MOM, subset(newdf, var==cvar)) %>% 
    summary %>% .[["coefficients"]] %>% .[1,1:3]
  
  outtable <- outtable %>% bind_rows(tibble(ff3, ff3m, ff5, ff5m, cvar, coef = c("alpha", "sd", "t")))
  
  
  
}

# get factor loadings from ff3m
outtable <- c()
for (cvar in unique(newdf$var)){
  
  ff3m <- lm(I(meannet-RF) ~ `Mkt-RF` + SMB + HML + MOM, subset(newdf, var==cvar)) %>% 
    summary %>% .[["coefficients"]] %>% .[1:5,1:3] %>% t
  
  
  
  #outtable <- outtable %>% bind_rows(tibble(ff3m, cvar, coef = c("alpha", "sd", "t")))
  
  outtable <- outtable %>% bind_rows(tibble(data.frame(ff3m), cvar, coef=c("est","sd","t")))
  
  
  
}

View(outtable %>% filter(coef=="t") %>% arrange(abs(ff5m)))


##

outtable <- outtable %>%
  select(cvar, coef, ff5m) %>%
  pivot_wider(names_from = coef, values_from = ff5m)

outtable <- output %>% 
  group_by(var) %>%
  summarise(meanret = mean(meannet)) %>%
  full_join(outtable, by = c("var" = "cvar"))


stargazer(bind_cols(round(outtable %>% select(-c("cvar", "coef")), 2),outtable %>% select("cvar","coef")),type = "latex",summary = F)



