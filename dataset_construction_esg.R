rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(zoo);library(lubridate);library(vroom);library(mice);library(Hmisc)
source("functions.R")

df <- read_csv("data_raw/refinitiv_esg.csv", 
               col_types = cols(Date = col_date(format = "%d.%m.%Y"), 
                                `Env R&D Expenditures To Revenues in million` = col_double()))

# =========================================================================
# PREPROCESS
# =========================================================================

df <- df %>%
  arrange(Ticker, Date) %>%
  mutate(ymon = ymon(Date),
         CUSIP = substr(CUSIP, 1, 8)
         ) %>%
  group_by(Ticker) %>%
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
    #tradeunion = ,
    #lostdays = ,
    boardindep = `Independent Board Members`,
    boardfem = `Board Gender Diversity, Percent`,
    boardattend = `Board Meeting Attendance Average`,
    boardsize = `Board Size`,
    execcomp = `Total Senior Executives Compensation To Revenues in million`,
    nonexecs = `Non-Executive Board Members`,
    boardterm = `Board Member Term Duration`,
    boardcomp = `Board Member Compensation`
    
  ) %>%
  select(-c(`SOx Emissions To Revenues USD in million`,
            `NOx Emissions To Revenues USD in million`,
            `Policy Human Rights`, `Policy Responsible Marketing`, `Product Responsibility Monitoring`, `Policy Data Privacy`,
            `Average Training Hours`, `CSR Sustainability Reporting`, `Env R&D Expenditures To Revenues in million`,
            `Board Background and Skills`, `Independent Board Members_1`))




# =========================================================================
# NAs
# =========================================================================

View(sort(colSums(is.na(df)) / nrow(df) * 100))


# df <- df %>%
#   mutate(
#     
#     carbonint = impute(carbonint, mean),
#     energyint = impute(energyint, mean),
#     waterint = impute(waterint, mean),
#     wastegen = impute(wastegen, mean)
#     # femexec
#     # fememp
#     # turnemp
#     # tradeunion
#     # lostdays
#     # boardindep
#     # boardfem
#     # boardattend
#     # boardsize
#     # execcomp
#     # nonexecs
#     # boardterm
#     # boardcomp
#     
#     
#   )






# =========================================================================
# WRITE DATA
# =========================================================================

write.table(df, file = "data/data_esg.csv", append = F, sep = ",", row.names = F, col.names = T)











