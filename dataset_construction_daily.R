rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(zoo);library(lubridate);library(vroom)
source("functions.R")


df <- vroom("data_raw/crspdaily.csv", 
            col_types = cols(ASK = col_double(), 
                             BID = col_double(), RET = col_double(), 
                             date = col_date(format = "%Y%m%d")))




data <- df[1:100000,]
# data <- data %>% drop_na(RET)

data <- data %>%
  mutate(PRC = abs(PRC),
         ymon = ymon(date)) %>%
  arrange(PERMNO, date) #sort

# ============================================================
# Spread
# ============================================================
spread <- data %>%
  mutate(dspread = ASK-BID) %>%
  group_by(PERMNO, ymon) %>%
  summarise(spread = mean(dspread,na.rm=T))


# ============================================================
# Zero trading days
# ============================================================

zerotrading <- data %>%
  group_by(PERMNO, ymon) %>%
  mutate(zerodays = VOL == 0) %>%
  summarise(zerotrading = sum(zerodays, na.rm=T))


# ============================================================
# Maxret
# ============================================================

maxret <- data %>%
  group_by(PERMNO, ymon) %>%
  summarise(maxret = max(RET))


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
         rel2high = PRC / yearmax)

# ============================================================
# Variance
# ============================================================



variance <- data %>%
  drop_na(RET) %>%
  group_by(PERMNO, ymon) %>%
  summarise(mvar = var(RET, na.rm=T)) %>%
  mutate(variance = rollmean(mvar, 2, fill = NA, align = "right")) %>% #slightly inaccurate
  drop_na(variance)
  
  

# ============================================================
# SUV
# ============================================================

suvdat <- data %>%
  select(PERMNO, date, ymon, VOL, RET) %>%
  mutate(abret = abs(RET)) %>%
  drop_na(RET, VOL)

output <- c()

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
    
    output <- bind_rows(output,
                        tibble("PERMNO" = permno,
                               "ymon" = ymons[month],
                               "SUV" = coutput
                        ))
    
    
  }
  
}




# WORKS
ymons <- unique(suv$ymon)
for (month in 3:length(ymons)){
  
  idxtrain <- c(ymons[month-2], ymons[month-1])
  idxtest <- ymons[month]
  
  ctrain <- suv[suv$ymon %in% idxtrain,]
  ctest <- suv[suv$ymon==idxtest,]
  
  cfit <- lm(VOL~abret, ctrain)
  cpred <- predict(cfit, newdata = ctest)
  
  coutput <- mean(cpred) / sd(residuals(cfit))
  
  output <- bind_rows(output,
                      tibble("PERMNO" = ctest$PERMNO[1],
                             "ymon" = ymons[month],
                             "SUV" = coutput
                      ))
  
  
}










calcSUV <- function(data){
  
  if(length(unique(data$ymon))>=3){
    output <- c()
    ymons <- unique(data$ymon)
    for (i in 3:length(ymons)){
      
      ctrain <- data[data$ymon %in% c(ymons[i-2], ymons[i-1]),]
      ctest <- data[data$ymon == ymons[i],]
      
      cfit <- lm(VOL~abret, ctrain)
      cpred <- predict(cfit, newdata = ctest)
      
      cresiduals <- ctest$VOL - cpred
      coutput <- mean(cresiduals) / sd(residuals(cfit))
      
      output <- c(output, coutput)
      
    }
    output <- c(rep(NA,2), output)
    return(output)
  } else {
    return(rep(NA, length(ymons)))
  }
  

}





test <- c()
for (comp in unique(suv$PERMNO)){
  
  cd <- suv[suv$PERMNO==comp,]
  co <- tibble("PERMNO" = comp,
               "ymon" = unique(cd$ymon),
               "SUV" = calcSUV(cd))
  test <- bind_rows(test, co)
  
  
  
}


test %>%
  group_by(PERMNO, ymon) %>%
  summarise(m = mean(SUV))




test <- suv %>%
  group_by(PERMNO) %>%
  mutate(SUV = calcSUV())


# ============================================================
# JOINING
# ============================================================



# TODO: join all and write




