rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(zoo);library(lubridate);library(vroom)
source("functions.R")


df <- vroom("data_raw/crspdaily.csv", 
            col_types = cols(ASK = col_double(), 
                             BID = col_double(), RET = col_double(), 
                             date = col_date(format = "%Y%m%d")))




#data <- df
data <- df[1:30000,]
# data <- data %>% drop_na(RET)

data <- data %>%
  mutate(PRC = abs(PRC),
         ymon = ymon(date))

# ============================================================
# Spread
# ============================================================

spread <- data %>%
  mutate(dspread = ASK-BID) %>%
  group_by(PERMNO, ymon) %>%
  summarise(spread = mean(dspread, na.rm=T)) %>%
  mutate(spread = lag(spread, 1)) %>%
  na.omit


# ============================================================
# Zero trading days
# ============================================================

zerotrading <- data %>%
  mutate(zerodays = VOL == 0) %>%
  group_by(PERMNO, ymon) %>%
  summarise(zerotrade = sum(zerodays, na.rm=T)) %>%
  mutate(zerotrade = lag(zerotrade, 1)) %>%
  na.omit


# ============================================================
# Maxret
# ============================================================

maxret <- data %>%
  group_by(PERMNO, ymon) %>%
  summarise(maxret = max(RET, na.rm=T)) %>%
  mutate(maxret = lag(maxret, 1)) %>%
  filter(is.finite(maxret)) %>%
  na.omit


# ============================================================
# Rel2High
# ============================================================

rel2high <- data %>%
  drop_na(PRC) %>%
  group_by(PERMNO) %>%
  mutate(counter = 1:n(),
         cummax = cummax(PRC),
         yearmax = rollmax(PRC, 250, fill = NA, align = "right"),
         yearmax = ifelse(is.na(yearmax), cummax, yearmax),
         rel2high = PRC / yearmax) %>%
  ungroup %>%
  group_by(PERMNO, ymon) %>%
  filter(row_number()==n()) %>%
  select(PERMNO, ymon, rel2high) %>%
  ungroup %>%
  group_by(PERMNO) %>%
  mutate(rel2high = lag(rel2high, 1)) %>%
  na.omit

# ============================================================
# Variance
# ============================================================

variance <- data %>%
  drop_na(RET) %>%
  group_by(PERMNO, ymon) %>%
  summarise(mvar = var(RET, na.rm=T)) %>%
  mutate(variance = rollmean(mvar, 2, fill = NA, align = "right")) %>% #slightly inaccurate
  mutate(variance = lag(variance, 1)) %>%
  drop_na(variance) %>%
  select(PERMNO, ymon, variance) %>%
  na.omit



# ============================================================
# SUV
# ============================================================

suvdat <- data %>%
  select(PERMNO, ymon, VOL, RET) %>%
  mutate(abret = abs(RET)) %>%
  select(-RET) %>%
  drop_na(abret, VOL)

suvoutput <- c()

for (permno in unique(suvdat$PERMNO)){
  cat(permno, "\n")
  suv <- subset(suvdat, PERMNO == permno)
  ymons <- unique(suv$ymon)
  if(length(ymons)<3){next}
  for (month in 3:length(ymons)){
    
    idxtrain <- c(ymons[month-2], ymons[month-1])
    idxtest <- ymons[month]
    
    ctrain <- suv[suv$ymon %in% idxtrain,]
    ctest <- suv[suv$ymon==idxtest,]
    
    cfit <- lm(VOL~abret, ctrain)
    cpred <- predict(cfit, newdata = ctest)
    
    coutput <- mean(cpred) / sd(residuals(cfit))
    
    suvoutput <- bind_rows(suvoutput,
                           tibble("PERMNO" = permno,
                                  "ymon" = ymons[month],
                                  "suv" = coutput
                           ))
    
    
  }
  
}

# lagging
suvoutput <- suvoutput %>%
  group_by(PERMNO) %>%
  mutate(suv = lag(suv, 1)) %>%
  na.omit



# ============================================================
# JOINING
# ============================================================

df_export <- full_join(spread, zerotrading, by = c("PERMNO", "ymon"))
df_export <- full_join(df_export, maxret,  by = c("PERMNO", "ymon"))
df_export <- full_join(df_export, rel2high,  by = c("PERMNO", "ymon"))
df_export <- full_join(df_export, variance,  by = c("PERMNO", "ymon"))
df_export <- full_join(df_export, suvoutput,  by = c("PERMNO", "ymon"))


write.table(df_export, file = "data/data_daily.csv", append = F, sep = ",", row.names = F)




