rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(keras);library(caret);library(glmnet);library(xgboost)
source("functions.R")

#load("data/x_fullsample.RData")
df <- vroom("data/x_exploration.csv")

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


# # Add FF factors
# ff5 <- vroom("data_raw/FF5F.CSV", col_types = cols(Date = col_date(format = "%Y%m")))
# ff5 <- ff5 %>%
#   mutate(`Mkt-RF` = `Mkt-RF` / 100,
#          SMB = SMB / 100,
#          HML = HML / 100,
#          RMW = RMW / 100,
#          CMA = CMA / 100,
#          MOM = MOM / 100,
#          RF = RF / 100,
#          ymon = ymon(Date)) %>%
#   select(-c("Date", "RF"))
# df <- df %>% left_join(ff5, by = "ymon")



# Add quantiles
df <- df %>%
  group_by(ymon) %>%
  mutate(size_q = cut_number(lme, 5, labels = 1:5),
         bm_q   = cut_number(bm,  5, labels = 1:5),
         combo_q = paste(size_q, bm_q, sep = "-"))

test <- df %>%
  group_by(ymon, combo_q) %>%
  summarise(
    chsho = weighted.mean(chsho, lme),
    a2me = weighted.mean(a2me, lme)
  )


test <- df %>%
  group_by(ymon, combo_q) %>%
  summarise(
    
    RET  = weighted.mean(RET, lme),
    ivol = weighted.mean(ivol, lme),
    beta = weighted.mean(beta, lme),
    bm       = weighted.mean(bm, lme),
    roa = weighted.mean(roa, lme),
    rna = weighted.mean(rna, lme),
    accrual = weighted.mean(accrual, lme),
    #FF_12 = weighted.mean(FF_12, lme),
    sin = weighted.mean(sin, lme),
    macropc1 = weighted.mean(macropc1, lme),
    spread = weighted.mean(spread, lme),
    zerotrade = weighted.mean(zerotrade, lme),
    maxret = weighted.mean(maxret, lme),
    rel2high = weighted.mean(rel2high, lme),
    variance = weighted.mean(variance, lme),
    suv = weighted.mean(suv, lme),
    carbonint = weighted.mean(carbonint, lme),
    energyint = weighted.mean(energyint, lme),
    waterint = weighted.mean(waterint, lme),
    wastegen = weighted.mean(wastegen, lme),
    femexec = weighted.mean(femexec, lme),
    fememp = weighted.mean(fememp, lme),
    turnemp = weighted.mean(turnemp, lme),
    tradeunion = weighted.mean(tradeunion, lme),
    lostdays = weighted.mean(lostdays, lme),
    boardindep = weighted.mean(boardindep, lme),
    boardfem = weighted.mean(boardfem, lme),
    boardattend = weighted.mean(boardattend, lme),
    boardsize = weighted.mean(boardsize, lme),
    execcomp = weighted.mean(execcomp, lme),
    nonexecs = weighted.mean(nonexecs, lme),
    boardterm = weighted.mean(boardterm, lme),
    boardcomp = weighted.mean(boardcomp, lme),
    esgscore = weighted.mean(esgscore, lme),
    esgcomb = weighted.mean(esgcomb, lme),
    esgcontr = weighted.mean(esgcontr, lme),
    esge = weighted.mean(esge, lme),
    esgg = weighted.mean(esgg, lme),
    esgs = weighted.mean(esgs, lme),
    esgres = weighted.mean(esgres, lme),
    esgemi = weighted.mean(esgemi, lme),
    esginn = weighted.mean(esginn, lme),
    esgwor = weighted.mean(esgwor, lme),
    esghum = weighted.mean(esghum, lme),
    esgcomm = weighted.mean(esgcomm, lme),
    esgpro = weighted.mean(esgpro, lme),
    esgman = weighted.mean(esgman, lme),
    esgcsr = weighted.mean(esgcsr, lme),
    strev = weighted.mean(strev, lme),
    r2_1 = weighted.mean(r2_1, lme),
    r6_2 = weighted.mean(r6_2, lme),
    r12_2 = weighted.mean(r12_2, lme),
    r12_7 = weighted.mean(r12_7, lme),
    r36_13 = weighted.mean(r36_13, lme),
    chmom = weighted.mean(chmom, lme),
    lat = weighted.mean(lat, lme),
    lme = weighted.mean(lme, lme),
    chsho = weighted.mean(chsho, lme),
    a2me = weighted.mean(a2me, lme),
    cf2p = weighted.mean(cf2p, lme),
    divyield = weighted.mean(divyield, lme),
    ep = weighted.mean(ep, lme),
    leverage = weighted.mean(leverage, lme),
    turnover = weighted.mean(turnover, lme),
    noa = weighted.mean(noa, lme),
    pm = weighted.mean(pm, lme),
    tobinsq = weighted.mean(tobinsq, lme),
    sga2s = weighted.mean(sga2s, lme),
    age = weighted.mean(age, lme)
    
            )


test_simplemean <- df %>%
  group_by(ymon, combo_q) %>%
  summarise(
    
    RET  = weighted.mean(RET, lme),
    ivol = mean(ivol),
    beta = mean(beta),
    bm       = mean(bm),
    roa = mean(roa),
    rna = mean(rna),
    accrual = mean(accrual),
    #FF_12 = weighted.mean(FF_12, lme),
    sin = mean(sin),
    macropc1 = mean(macropc1),
    spread = mean(spread),
    zerotrade = mean(zerotrade),
    maxret = mean(maxret),
    rel2high = mean(rel2high),
    variance = mean(variance),
    suv = mean(suv),
    carbonint = mean(carbonint),
    energyint = mean(energyint),
    waterint = mean(waterint),
    wastegen = mean(wastegen),
    femexec = mean(femexec),
    fememp = mean(fememp),
    turnemp = mean(turnemp),
    tradeunion = mean(tradeunion),
    lostdays = mean(lostdays),
    boardindep = mean(boardindep),
    boardfem = mean(boardfem),
    boardattend = mean(boardattend),
    boardsize = mean(boardsize),
    execcomp = mean(execcomp),
    nonexecs = mean(nonexecs),
    boardterm = mean(boardterm),
    boardcomp = mean(boardcomp),
    esgscore = mean(esgscore),
    esgcomb = mean(esgcomb),
    esgcontr = mean(esgcontr),
    esge = mean(esge),
    esgg = mean(esgg),
    esgs = mean(esgs),
    esgres = mean(esgres),
    esgemi = mean(esgemi),
    esginn = mean(esginn),
    esgwor = mean(esgwor),
    esghum = mean(esghum),
    esgcomm = mean(esgcomm),
    esgpro = mean(esgpro),
    esgman = mean(esgman),
    esgcsr = mean(esgcsr),
    strev = mean(strev),
    r2_1 = mean(r2_1),
    r6_2 = mean(r6_2),
    r12_2 = mean(r12_2),
    r12_7 = mean(r12_7),
    r36_13 = mean(r36_13),
    chmom = mean(chmom),
    lat = mean(lat),
    lme = mean(lme),
    chsho = mean(chsho),
    a2me = mean(a2me),
    cf2p = mean(cf2p),
    divyield = mean(divyield),
    ep = mean(ep),
    leverage = mean(leverage),
    turnover = mean(turnover),
    noa = mean(noa),
    pm = mean(pm),
    tobinsq = mean(tobinsq),
    sga2s = mean(sga2s),
    age = mean(age),
    
    RMRF = mean(`Mkt-RF`),
    SMB = mean(SMB),
    HML = mean(HML),
    RMW = mean(RMW),
    CMA = mean(CMA),
    MOM = mean(MOM)
    
  )


write.table(test_simplemean, file = "sortports.csv", sep = ",", row.names = F)



ymons <- unique(df$ymon)
for (cymon in ymons){
  cat(cymon, "\n")
  
  # Select ymon
  cdf <- df %>% filter(ymon == cymon)
  
  
  
  
  
}





test <- cdf %>%
  mutate(q = cut(lme, breaks = quantile(cdf$lme, c(0,.25,.5,.75,1))))



test <- cdf %>%
  mutate(q = cut_number(lme, 5, labels = 1:5))



test <- df %>% #weighted mean
  group_by(ymon) %>%
  summarise(m = weighted.mean(RET, lme))


# ==================================================================================
df <- vroom("data/sortports.csv")
df <- df %>% select(-c("ymon", "combo_q"))

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
filename <- "x_sortports"

save(x_train, y_train, x_test, y_test, file = paste0("data/", filename, ".RData"))
write.table(df, file = paste0("data/", filename, ".csv"), append = F, sep = ",", row.names = F)










