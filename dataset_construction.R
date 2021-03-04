rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(zoo);library(lubridate);library(vroom)
source("functions.R")

# Import
crsp     <- vroom("data_raw/crsp.csv",     col_types = cols(RET = col_double(), date = col_date(format = "%Y%m%d")))
crspcomp <- vroom("data_raw/crspcomp.csv", col_types = cols(datadate = col_date(format = "%Y%m%d")))
ff       <- vroom("data_raw/FF.CSV",       col_types = cols(Date = col_date(format = "%Y%m")))
crspbeta <- vroom("data_raw/crspbeta.csv", 
                  col_types = cols(DATE = col_date(format = "%Y%m%d"), 
                                   R2 = col_number(), RET = col_number(), 
                                   exret = col_number(), ivol = col_number(), 
                                   tvol = col_number()))
crspbetacapm <- vroom("data_raw/crspbetacapm.csv", 
                      col_types = cols(DATE = col_date(format = "%Y%m%d"), 
                                       R2 = col_number(), RET = col_number(), 
                                       exret = col_number(), ivol = col_number(), 
                                       tvol = col_number()))
compratios     <- vroom("data_raw/compratios.csv", col_types = cols(public_date = col_date(format = "%Y%m%d")))
ffind          <- vroom("data_raw/SIC2FF.csv")
industries_sin <- vroom("data_raw/industries_sin.csv")
df_macro       <- vroom("data/data_macro.csv", col_types = cols(ymon = col_character()))
df_daily       <- vroom("data/data_daily.csv", col_types = cols(ymon = col_character()))
df_esg         <- vroom("data/data_esg.csv", 
                        col_types = cols(Date = col_date(format = "%Y-%m-%d"), 
                                         ymon = col_character()))
##############################################################################
# INITIAL CLEAN
##############################################################################

# CRSP
crsp <- crsp %>%
  filter(EXCHCD %in% c(1,2,3)) %>%
  filter(SHRCD %in% c(10, 11)) %>%
  mutate(PRC = abs(PRC),
         LOGRET = log(1 + RET),
         ME = PRC * SHROUT,
         ymon = ymon(date)) %>%
  na.omit

# COMPUSTAT
crspcomp <- crspcomp %>%
  filter(curcd == "USD") %>%
  mutate(ymon = ymon(datadate))

# FF
ff <- ff %>%
  mutate(`Mkt-RF` = `Mkt-RF` / 100,
         SMB = SMB / 100,
         HML = HML / 100,
         RF = RF / 100,
         ymon = ymon(Date)) %>%
  select(-Date)

# CRSP Beta (FF IdioVol)
crspbeta <- crspbeta %>%
  mutate(ymon = ymon(DATE)) %>%
  select(PERMNO, ymon, ivol) %>%
  group_by(PERMNO) %>%
  mutate(ivol = lag(ivol, 1)) %>% #lag
  ungroup

# CRSP Beta CAPM (CAPM Beta)
crspbetacapm <- crspbetacapm %>%
  mutate(ymon = ymon(DATE)) %>%
  select(PERMNO, ymon, b_mkt) %>%
  rename(beta = b_mkt) %>%
  group_by(PERMNO) %>%
  mutate(beta = lag(beta, 1)) %>% #lag
  ungroup

# Compustat ratios
compratios <- compratios %>%
  mutate(ymon = ymon(public_date)) %>%
  select(-c(adate, qdate, public_date)) %>%
  rename(rna = pretret_noa) %>%
  group_by(permno) %>%
  mutate(ymon = lead(ymon, 1)) %>% #lag all variables
  ungroup

# # Refinitiv ESG Data
# df_esg <- df_esg %>%
#   arrange(Ticker, Date) %>%
#   mutate(ymon = ymon(Date),
#          CUSIP = substr(CUSIP, 1, 8)) %>%
#   group_by(Ticker) %>%
#   fill(CUSIP) %>%
#   fill(CUSIP, .direction = "up") %>%
#   drop_na(CUSIP) %>%
#   ungroup

##############################################################################
# JOIN
##############################################################################

df <- left_join(crsp, crspcomp, by = c("PERMNO" = "LPERMNO", "ymon")) %>%
  select(-c(date, SHRCD, EXCHCD, GVKEY, datadate, fyear, indfmt, consol, popsrc, datafmt, conm, curcd, costat))

df <- left_join(df, ff, by = "ymon") #FF
df <- left_join(df, crspbeta, by = c("PERMNO", "ymon")) # IdioVol
df <- left_join(df, crspbetacapm, by = c("PERMNO", "ymon")) # Beta
df <- left_join(df, compratios, by = c("PERMNO" = "permno", "ymon")) # comp ratios
df <- left_join(df, ffind, by = c("SICCD" = "SIC")) # FF industries
df <- left_join(df, industries_sin, by = c("SICCD" = "SIC")) %>% #sin industries
  mutate(sin = ifelse(is.na(sin), FF_12 %in% c(4, 5), sin))

df <- left_join(df, df_macro, by = "ymon") # Macro
df <- left_join(df, df_daily, by = c("PERMNO", "ymon")) #daily data

df <- left_join(df, df_esg, by = c("CUSIP", "ymon")) #refinitiv ESG data

df <- df %>% arrange(PERMNO, ymon) # sort

##############################################################################
# NAs
##############################################################################

# NA LOCF
df <- df %>%
  group_by(PERMNO) %>%
  mutate_all(na.locf, na.rm=F) %>%
  ungroup



##############################################################################
# FILTERS
##############################################################################

# Bad values
df <- df %>%
  filter(sale > 0) %>%
  filter(at > 0)


# Remove firms with less than 24 months (2 years) of data
df <- df %>%
  group_by(PERMNO) %>%
  mutate(n = n()) %>%
  filter(n >= 24) %>%
  select(-n) %>%
  ungroup



##############################################################################
# VARIABLES
##############################################################################

# Past return variables
df <- df %>%
  group_by(PERMNO) %>%
  mutate(
    
    strev = lag(LOGRET, 1),
    
    r2_1 = c(rep(NA, 1), rollsum(LOGRET, 2)),
    r2_1 = lag(r2_1, 1),
    
    r6_2 = c(rep(NA, 3), rollsum(LOGRET, 4)),
    r6_2 = lag(r6_2, 2),
    
    r12_2 = c(rep(NA, 9), rollsum(LOGRET, 10)),
    r12_2 = lag(r12_2, 2),
    
    r12_7 = c(rep(NA, 4), rollsum(LOGRET, 5)),
    r12_7 = lag(r12_7, 7),
    
    r36_13 = c(rep(NA, 22), rollsum(LOGRET, 23)),
    r36_13 = lag(r36_13, 13),
    
    chmom = c(NA, diff(r6_2))
    
  ) %>%
  ungroup





# Firm characteristics
df <- df %>%
  group_by(PERMNO) %>%
  mutate(
    lat = lag(log(1 + at), 1),
    lagAT = lag(at, 12),
    
    lme = lag(log(ME), 1),
    lagME = lag(ME, 12),
    
    chsho = c(NA, diff(SHROUT))
  ) %>%
  ungroup


df <- df %>%
  mutate(
    
    a2me = at / ME,
    
    cf2p = (ib + dp + txdb) / lagME,
    
    divyield = dvt / ME, ###dvt july t-1, ME should be LME log?
    
    ep = ib / ME, #ib = earnings?
    
    leverage = (dltt + dlc) / (dltt + dlc + seq),
    
    turnover = log(1 + (VOL / SHROUT)),
    
    NOA_OA = at - che - ivao,
    NOA_OL = at - dlc - dltt - mib - pstk - ceq,
    noa = (NOA_OA - NOA_OL) / lagAT,
    
    pm = oiadp / sale,
    
    tobinsq = (at + ME - che - txdb) / at,
    
    sga2s = xsga / sale
    
  )

df <- df %>%
  group_by(PERMNO) %>%
  mutate(
    
    # Lagging
    a2me = lag(a2me, 1),
    cf2p = lag(cf2p, 1),
    divyield = lag(divyield, 1),
    ep = lag(ep, 1),
    leverage = lag(leverage, 1),
    turnover = lag(turnover, 1),
    noa = lag(noa, 1),
    pm = lag(pm, 1),
    tobinsq = lag(tobinsq, 1),
    sga2s = lag(sga2s, 1)
  ) %>%
  ungroup




# Age
df <- df %>%
  group_by(PERMNO) %>%
  mutate(year = as.numeric(substr(ymon, 1, 4)),
         startyear = cummin(year),
         age = year - startyear) %>%
  select(-c(year, startyear)) %>%
  ungroup





##############################################################################
# FINAL STEPS
##############################################################################

# dropping construction variables
which(colnames(df)=="ajex") #index start check
which(colnames(df)=="RF")   #index end check
df_export <- df[,-c(11:51)] #CHECK INDEXES ####################


df_export <- df_export %>% select(-c(ME,
                                     lagAT,
                                     lagME,
                                     NOA_OA, NOA_OL #NOA
                                     ))

### TESTING
df_export <- df_export %>%
  filter(ymon > 200201)


#####



# NA and Inf omit
df_export <- df_export %>% na.omit

# Write dataset file
write.table(df_export, file = "data/dataset.csv", append = F, sep = ",", row.names = F)


