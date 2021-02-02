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
  filter(curcd == "USD")



########################################
# VARIABLES
########################################







