rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(zoo);library(lubridate);library(vroom)
source("functions.R")

# Import
crsp     <- vroom("data_raw/crsp.csv",     col_types = cols(RET = col_double(), date = col_date(format = "%Y%m%d")))
crspcomp <- vroom("data_raw/crspcomp.csv", col_types = cols(datadate = col_date(format = "%Y%m%d")))

########################################
# INITIAL CLEAN
########################################

# CRSP
crsp <- crsp %>%
  filter(EXCHCD %in% c(1,2,3)) %>%
  mutate(PRC = abs(PRC),
         ME = PRC * SHROUT,
         ymon = ymon(date)) %>%
  na.omit


# COMPUSTAT
crspcomp <- crspcomp %>%
  filter(curcd == "USD") %>%
  mutate(ymon = ymon(datadate))

########################################
# JOIN
########################################

df <- left_join(crsp, crspcomp, by = c("PERMNO" = "LPERMNO", "ymon")) %>%
  select(-c(date, EXCHCD, GVKEY, datadate, fyear, indfmt, consol, popsrc, datafmt, conm, curcd, costat)) %>%
  arrange(PERMNO, ymon)


########################################
# NAs
########################################

df <- df
  #add na.locf



########################################
# VARIABLES
########################################

df <- df %>%
  mutate(
    AT = at,
    
    A2ME = AT / ME,
    
    PS = pstkrv,
    PS = ifelse(is.na(PS), pstkrv, PS),
    PS = ifelse(is.na(PS), pstkl, PS),
    PS = ifelse(is.na(PS), pstk, PS),
    SH = seq,
    SH = ifelse(is.na(SH), ceq + PS, SH),
    SH = ifelse(is.na(SH), AT - lt, SH),
    BE = SH + txditc - PS,
    BEME = BE / ME, ### lag ME
    
    C = che / AT,
    
    CF = (ni + dp - (wcapch + capx)) / BE,
    
    CF2P = (ib + dp + txdb) / ME, ### lag ME
    
    CTO = sale / AT, ###lag AT
    
    D2A = dp / AT,
    
    D2P = dvt / ME, ###dvt july t-1, ME should be LME log?
    
    # DPI2A = ,#skipped
    
    E2P = (epsfx * SHROUT) / ME, ###earnings in june
    
    FC2Y = (xsga + xrd + xad) / sale,
    
    # Investment = ,#skipped
    
    Lev = (dltt + dlc) / (dltt + dlc + seq),
    
    Lturnover = VOL / SHROUT, ###check vol 
    
    # NI = ,#skipped
    
    NOA_OA = AT - che - ivao,
    NOA_OL = AT - dlc - dltt - mib - pstk - ceq,
    NOA = (NOA_OA - NOA_OL) / AT, ###lagged AT
    
    #OA_NCWC = AC, ###after AC
    #OA = OA_NCWC - dp / TA, ###lagged TA
    
    OL = (cogs + xsga) / AT,
    
    OP = (revt - (cogs + tie + xsga)) / BE,
    
    PCM = (sale - cogs) / sale,
    
    PM = oiadp / sale,
    
    PROF = gp / BE,
    
    Q = (AT + ME - che - txdb) / AT,
    
    RNA = oiadp / (NOA_OA - NOA_OL), ###lagged
    
    ROA = ib / AT, ###lagged
    
    ROE = ib / BE, ###lagged
    
    S2P = sale / ME, ###should be LME log?
    
    SGA2S = xsga / sale,
    
    
    # AC = , #skipped
    
    ATO = sale / (NOA_OA - NOA_OL)
    
    
    
    
    
    
    
  )




df <- df %>%
  replace_na(list(ajex = 0,
                  act,
                  at,
                  capx,
                  ceq,
                  che,
                  cogs,
                  csho,
                  dlc,
                  dltt,
                  dp,
                  dvt,
                  epsfx,
                  gp,
                  ib,
                  invt,
                  ivao,
                  lct,
                  lt,
                  mib,
                  ni,
                  oiadp,
                  ppegt,
                  pstk,
                  pstkl,
                  pstkrv,
                  revt,
                  sale,
                  seq,
                  tie,
                  txdb = 0,
                  txditc = 0,
                  txp = 0,
                  wcapch = 0, ###
                  xad,
                  xrd,
                  xsga))





