rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(zoo);library(lubridate);library(vroom);library(mice);library(Hmisc)
source("functions.R")


df <- vroom("data_raw/esg_set1.csv", 
             col_types = cols(Date = col_date(format = "%d.%m.%Y"), 
                              `Lost Days To Total Days` = col_double()))

# df2 <- vroom("data_raw/esg_set2.csv", 
#              col_types = cols(Date = col_date(format = "%d.%m.%Y"), 
#                               `Lost Days To Total Days Score` = col_double(), 
#                               `Turnover of Employees Score` = col_double()))

dfscores <- vroom("data_raw/esg_set3.csv", 
             col_types = cols(Date = col_date(format = "%d.%m.%Y")))




# =========================================================================
# PREPROCESS
# =========================================================================

df <- df %>%
  drop_na(Date) %>%
  arrange(TRTicker, Date) %>%
  mutate(ymon = ymon(Date),
         CUSIP = substr(CUSIP, 1, 8)
         ) %>%
  group_by(TRTicker) %>%
  fill(CUSIP) %>%
  fill(CUSIP, .direction = "up") %>%
  drop_na(CUSIP) %>%
  ungroup

dfscores <- dfscores %>%
  drop_na(Date) %>%
  arrange(TRTicker, Date) %>%
  mutate(ymon = ymon(Date),
         CUSIP = substr(CUSIP, 1, 8)
  ) %>%
  group_by(TRTicker) %>%
  fill(CUSIP) %>%
  fill(CUSIP, .direction = "up") %>%
  drop_na(CUSIP) %>%
  ungroup





# Using CRSP CUSIP converted (not necessary, just remove ninth control digit)
# cusip_raw <- df %>% select(CUSIP) %>% na.omit %>% pull %>% unique %>% sort
# 
# # write.table(cusip_raw, file = "data_raw/cusips.txt", sep = " ", row.names = F, col.names = F, append=F, quote = F)
# 
# cusip_new <- read_table2("data_raw/newcusips.txt", skip = 3)
# cusip_new <- cusip_new$cusip
# 
# cusip_conv <- tibble(
#   "CUSIP_9" = cusip_raw,
#   "CUSIP_8" = cusip_new
# )
# 
# df <- left_join(df, cusip_conv, by = c("CUSIP" = "CUSIP_9"))

# =========================================================================
# RENAMING
# =========================================================================

df <- df %>%
  rename(
    
    carbonint = `Total CO2 Equivalent Emissions To Revenues USD in million`,
    energyint = `Total Energy Use To Revenues USD in million`,
    waterint = `Water Use To Revenues USD in million`,
    wastegen = `Total Waste To Revenues USD in million`,
    femexec = `Women Managers`,
    fememp = `Women Employees`,
    turnemp = `Turnover of Employees`,
    tradeunion = `Trade Union Representation`,
    lostdays = `Lost Days To Total Days`,
    boardindep = `Independent Board Members`,
    boardfem = `Board Gender Diversity, Percent`,
    boardattend = `Board Meeting Attendance Average`,
    boardsize = `Board Size`,
    execcomp = `Total Senior Executives Compensation To Revenues in million`,
    nonexecs = `Non-Executive Board Members`,
    boardterm = `Board Member Term Duration`,
    boardcomp = `Board Member Compensation`
    
  ) %>%
  select(-c(`CUSIP (extended)`, `CUSIP Code`, ISIN, `ISIN Code`, SEDOL,
            TRTicker, Date))

dfscores <- dfscores %>%
  rename(
    
    esgscore = `ESG Score`,
    esgcomb = `ESG Combined Score`,
    esgcontr = `ESG Controversies Score`,
    esge = `Environmental Pillar Score`,
    esgg = `Governance Pillar Score`,
    esgs = `Social Pillar Score`,
    esgres = `Resource Use Score`,
    esgemi = `Emissions Score`,
    esginn = `Environmental Innovation Score`,
    esgwor = `Workforce Score`,
    esghum = `Human Rights Score`,
    esgcomm = `Community Score`,
    esgpro = `Product Responsibility Score`,
    esgman = `Management Score`,
    esgcsr = `CSR Strategy Score`
    
    
  ) %>%
  select(-c(`CUSIP (extended)`, `CUSIP Code`, ISIN, `ISIN Code`, SEDOL,
            TRTicker, Date))

# =========================================================================
# JOIN
# =========================================================================

df <- full_join(df, dfscores, by = c("CUSIP", "ymon"))


# =========================================================================
# NAs
# =========================================================================



# NA LOCF forward
df <- df %>%
  group_by(CUSIP) %>%
  mutate_at(vars(-c(CUSIP, ymon)), na.locf, na.rm=F) %>%
  ungroup

# NA LOCF backward (REMOVE?)
df <- df %>%
  group_by(CUSIP) %>%
  arrange(CUSIP, desc(ymon)) %>%
  mutate_at(vars(-c(CUSIP, ymon)), na.locf, na.rm=F) %>%
  arrange(CUSIP, ymon) %>%
  ungroup


# Impute NAs
df <- df %>%
  mutate(

    carbonint  = impute(carbonint, mean),
    energyint  = impute(energyint, mean),
    waterint   = impute(waterint, mean),
    wastegen   = impute(wastegen, mean),
    femexec    = impute(femexec, mean),
    fememp     = impute(fememp, mean),
    turnemp    = impute(turnemp, mean),
    tradeunion = ifelse(is.na(tradeunion), 0, tradeunion),
    lostdays   = ifelse(is.na(lostdays), 0, lostdays)
    # boardindep
    # boardfem
    # boardattend
    # boardsize
    # execcomp
    # nonexecs
    # boardterm
    # boardcomp


  )

# Bad values filter
df <- df %>%
  filter(execcomp >= 0)

df <- df %>% na.omit #maybe remove

View(sort(colSums(is.na(df)) / nrow(df) * 100)) #check


# df %>% #check number of var NAs per ymon
#   filter(ymon == 199901) %>%
#   is.na() %>%
#   rowSums()

# =========================================================================
# TRANSFORMATIONS
# =========================================================================

df <- df %>%
  mutate(
    carbonint = log(1 + carbonint),
    energyint = log(1 + energyint),
    waterint  = log(1 + waterint),
    wastegen  = log(1 + wastegen),
    
    execcomp  = log(1 + execcomp),
    boardcomp = log(1 + boardcomp)
  )




# =========================================================================
# WRITE DATA
# =========================================================================

write.table(df, file = "data/data_esg.csv", append = F, sep = ",", row.names = F, col.names = T)




# =========================================================================
# EXPLORATION
# =========================================================================

library(corrplot);library(RColorBrewer);library(PerformanceAnalytics)
plotdat <- df %>%
  select(-c(CUSIP, ymon)) #%>%
  sample_n(500)


chart.Correlation(plotdat, histogram = T, pch = 19)

M <- cor(plotdat)
corrplot(M, method = "color")






