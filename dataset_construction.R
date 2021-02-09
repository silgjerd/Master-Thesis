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
crspbetacapm <- read_csv("data_raw/crspbetacapm.csv", 
                         col_types = cols(DATE = col_date(format = "%Y%m%d"), 
                                          R2 = col_number(), RET = col_number(), 
                                          exret = col_number(), ivol = col_number(), 
                                          tvol = col_number()))
ffind <- read_csv("data_raw/SIC2FF.csv")

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
  rename(IdioVol = ivol)

# CRSP Beta CAPM (CAPM Beta)
crspbetacapm <- crspbetacapm %>%
  mutate(ymon = ymon(DATE)) %>%
  select(PERMNO, ymon, b_mkt) %>%
  rename(Beta = b_mkt)


##############################################################################
# JOIN
##############################################################################

df <- left_join(crsp, crspcomp, by = c("PERMNO" = "LPERMNO", "ymon")) %>%
  select(-c(date, SHRCD, EXCHCD, GVKEY, datadate, fyear, indfmt, consol, popsrc, datafmt, conm, curcd, costat))

df <- left_join(df, ff, by = "ymon") #FF

df <- left_join(df, crspbeta, by = c("PERMNO", "ymon")) # IdioVol

df <- left_join(df, crspbetacapm, by = c("PERMNO", "ymon")) # Beta

df <- left_join(df, ffind, by = c("SICCD" = "SIC")) #FF industries

df <- df %>% arrange(PERMNO, ymon) # sort

##############################################################################
# FILTERS
##############################################################################

# Remove firms with less than 24 months (2 years) of data
df <- df %>%
  group_by(PERMNO) %>%
  mutate(n = n()) %>%
  filter(n >= 24) %>%
  select(-n) %>%
  ungroup


##############################################################################
# NAs
##############################################################################


# NA LOCF
df <- df %>%
  group_by(PERMNO) %>%
  mutate_all(na.locf, na.rm=F) %>%
  ungroup



# # NA impute UNFINISHED
# for (ymon in unique(df$ymon)) {
#   cd <- df %>% filter(ymon==ymon)
#   cmode <- getmode(cd$tie)
#   
#   
# }
# 
# #testing
# df %>%
#   group_by(ymon) %>%
#   summarise(m = mean(txp,na.rm=T)) %>% plot
# getmode(df$txp)
# summary(df$txp)



##############################################################################
# VARIABLES
##############################################################################

# Past return variables
df <- df %>%
  group_by(PERMNO) %>%
  mutate(
    
    ST_Rev = lag(LOGRET, 1),
    
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

df <- df %>% #lagging
  group_by(PERMNO) %>%
  mutate(
    AT = at,
    lagAT = lag(AT, 12),
    
    LME = log(ME),
    lagME = lag(ME, 12)
  ) %>%
  ungroup

df <- df %>%
  mutate(
    
    A2ME = AT / ME,
    
    PS = pstkrv,
    PS = ifelse(is.na(PS), pstkrv, PS),
    PS = ifelse(is.na(PS), pstkl, PS),
    PS = ifelse(is.na(PS), pstk, PS),
    SH = seq,
    SH = ifelse(is.na(SH), ceq + PS, SH),
    SH = ifelse(is.na(SH), AT - lt, SH),
    BE = SH + txditc - PS,
    BEME = BE / lagME,
    
    # C = che / AT,
    
    # CF = (ni + dp - (wcapch + capx)) / BE,
    
    CF2P = (ib + dp + txdb) / lagME,
    
    # CTO = sale / lagAT,
    
    # D2A = dp / AT,
    
    D2P = dvt / ME, ###dvt july t-1, ME should be LME log?
    
    E2P = (epsfx * SHROUT) / ME, ###earnings in june
    
    # FC2Y = (xsga + xrd + xad) / sale,
    
    Lev = (dltt + dlc) / (dltt + dlc + seq),
    
    Lturnover = VOL / SHROUT,
    
    NOA_OA = AT - che - ivao,
    NOA_OL = AT - dlc - dltt - mib - pstk - ceq,
    NOA = (NOA_OA - NOA_OL) / lagAT,
    
    #OA_NCWC = AC, ###after AC
    #OA = OA_NCWC - dp / TA, ###lagged TA
    
    # OL = (cogs + xsga) / AT,
    
    # OP = (revt - (cogs + tie + xsga)) / BE,
    
    # PCM = (sale - cogs) / sale,
    
    PM = oiadp / sale,
    
    # PROF = gp / BE,
    
    Q = (AT + ME - che - txdb) / AT,
    
    RNA = oiadp / (NOA_OA - NOA_OL),
    
    # ROA = ib / lagAT,
    
    # ROE = ib / BE, ###lagged
    
    # S2P = sale / ME,
    
    SGA2S = xsga / sale
    
    # ATO = sale / (NOA_OA - NOA_OL)
    
    
  ) %>%
  ungroup



# Age
df <- df %>%
  group_by(PERMNO) %>%
  mutate(year = as.numeric(substr(ymon, 1, 4)),
         startyear = cummin(year),
         age = year - startyear) %>%
  select(-c(year, startyear))





##############################################################################
# FINAL STEPS
##############################################################################

# dropping construction variables
df_export <- df[,-c(10:50)] #CHECK INDEXES ####################
df_export <- df_export %>% select(-c(ME, lagAT, lagME,
                                     PS, SH, BE, NOA_OA, NOA_OL))

df_export <- df_export %>% na.omit

write.table(df_export, file = "data/dataset.csv", append = F, sep = ",", row.names = F)




