rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(zoo);library(lubridate);library(vroom)
source("functions.R")

dir <- "data_macro/"
files <- list.files(dir)

df <- c()

for (file in files){
  cat(file, "\n")
  
  df_temp <- vroom(paste0(dir, file), 
                   col_types = cols(col_date(), col_number()))
  
  df_temp <- df_temp %>% pivot_longer(cols = 2)
  
  df <- df %>% bind_rows(df_temp)
  
}


df <- df %>% na.omit

df <- df %>%
  mutate(ymon = ymon (DATE)) %>%
  select(-DATE)


df <- df %>%
  group_by(name, ymon) %>%
  summarise(mval = mean(value))





df <- df %>% pivot_wider(values_from = mval)
df <- df %>% arrange(ymon)
df <- df %>% filter(ymon >= 198410 & ymon < 202101)

df <- df %>%
  mutate(
    DBAA = ifelse(is.na(DBAA), mean(DBAA, na.rm=T), DBAA),
    DCOILWTICO = ifelse(is.na(DCOILWTICO), mean(DCOILWTICO, na.rm=T), DCOILWTICO),
    VXOCLS = ifelse(is.na(VXOCLS), mean(VXOCLS, na.rm=T), VXOCLS)
  )

df <- df %>% na.omit

# =========================================================================
# Preprocessing
# =========================================================================

df <- df %>%
  mutate(
    AWOTMAN = c(NA, diff(AWOTMAN)),
    BUSLOANS = c(NA, NA, diff(log(BUSLOANS), differences = 2)),
    CE16OV = c(NA, diff(log(CE16OV))),
    CES0600000008 = c(NA, NA, diff(log(CES0600000008), differences = 2)),
    CES1021000001 = c(NA, diff(log(CES1021000001))),
    CES2000000008 = c(NA, NA, diff(log(CES2000000008), differences = 2)),
    CES3000000008 = c(NA, NA, diff(log(CES3000000008), differences = 2)),
    CLF16OV = c(NA, diff(log(CLF16OV))),
    CMRMTSPL = c(NA, diff(log(CMRMTSPL))),
    CPIAPPSL = c(NA, NA, diff(log(CPIAPPSL), differences = 2)),
    CPIAUCSL = c(NA, NA, diff(log(CPIAUCSL), differences = 2)),
    CPIMEDSL = c(NA, NA, diff(log(CPIMEDSL), differences = 2)),
    CPITRNSL = c(NA, NA, diff(log(CPITRNSL), differences = 2)),
    CPIULFSL = c(NA, NA, diff(log(CPIULFSL), differences = 2)),
    CUMFNS = c(NA, diff(CUMFNS)),
    CUSR0000SA0L2 = c(NA, NA, diff(log(CUSR0000SA0L2), differences = 2)),
    CUSR0000SA0L5 = c(NA, NA, diff(log(CUSR0000SA0L5), differences = 2)),
    CUSR0000SAC = c(NA, NA, diff(log(CUSR0000SAC), differences = 2)),
    CUSR0000SAD = c(NA, NA, diff(log(CUSR0000SAD), differences = 2)),
    CUSR0000SAS = c(NA, NA, diff(log(CUSR0000SAS), differences = 2)),
    DAAA = c(NA, diff(DAAA)),
    DBAA = c(NA, diff(DBAA)),
    DCOILWTICO = c(NA, NA, diff(log(DCOILWTICO), differences = 2)),
    DDURRG3M086SBEA = c(NA, NA, diff(log(DDURRG3M086SBEA), differences = 2)),
    DMANEMP = c(NA, diff(log(DMANEMP))),
    DNDGRG3M086SBEA = c(NA, NA, diff(log(DNDGRG3M086SBEA), differences = 2)),
    DPCERA3M086SBEA = c(NA, diff(log(DPCERA3M086SBEA))),
    DSERRG3M086SBEA = c(NA, NA, diff(log(DSERRG3M086SBEA), differences = 2)),
    DTCOLNVHFNM = c(NA, NA, diff(log(DTCOLNVHFNM), differences = 2)),
    DTCTHFNM = c(NA, NA, diff(log(DTCTHFNM), differences = 2)),
    EXCAUS = c(NA, diff(log(EXCAUS))),
    EXJPUS = c(NA, diff(log(EXJPUS))),
    EXSZUS = c(NA, diff(log(EXSZUS))),
    EXUSUK = c(NA, diff(log(EXUSUK))),
    FEDFUNDS = c(NA, diff(FEDFUNDS)),
    GS1 = c(NA, diff(GS1)),
    GS10 = c(NA, diff(GS10)),
    GS5 = c(NA, diff(GS5)),
    HOUST = log(HOUST),
    HOUSTMW = log(HOUSTMW),
    HOUSTNE = log(HOUSTNE),
    HOUSTS = log(HOUSTS),
    HOUSTW = log(HOUSTW),
    ICSA = c(NA, diff(log(ICSA))),
    INDPRO = c(NA, diff(log(INDPRO))),
    INVEST = c(NA, NA, diff(log(INVEST), differences = 2)),
    IPB51222S = c(NA, diff(log(IPB51222S))),
    IPBUSEQ = c(NA, diff(log(IPBUSEQ))),
    IPCONGD = c(NA, diff(log(IPCONGD))),
    IPDCONGD = c(NA, diff(log(IPDCONGD))),
    IPDMAT = c(NA, diff(log(IPDMAT))),
    IPFINAL = c(NA, diff(log(IPFINAL))),
    IPFPNSS = c(NA, diff(log(IPFPNSS))),
    IPFUELS =  c(NA, diff(log(IPFUELS))),
    IPMANSICS = c(NA, diff(log(IPMANSICS))),
    IPMAT = c(NA, diff(log(IPMAT))),
    IPNCONGD = c(NA, diff(log(IPNCONGD))),
    IPNMAT = c(NA, diff(log(IPNMAT))),
    M1SL = c(NA, NA, diff(log(M1SL), differences = 2)),
    M2REAL = c(NA, diff(log(M2REAL))),
    M2SL = c(NA, NA, diff(log(M2SL), differences = 2)),
    MANEMP = c(NA, diff(log(MANEMP))),
    MZMSL = c(NA, NA, diff(log(MZMSL), differences = 2)),
    NDMANEMP = c(NA, diff(log(NDMANEMP))),
    NONBORRES = c(NA, diff(diff(df$NONBORRES) / lag(df$NONBORRES))),###########
    NONREVSL = c(NA, NA, diff(log(NONREVSL), differences = 2)),
    PAYEMS = c(NA, diff(log(PAYEMS))),
    PCEPI = c(NA, NA, diff(log(PCEPI), differences = 2)),
    PERMIT = log(PERMIT),
    PERMITMW = log(PERMITMW),
    PERMITNE = log(PERMITNE),
    PERMITS = log(PERMITS),
    PERMITW = log(PERMITW),
    PPICMM = c(NA, NA, diff(log(PPICMM), differences = 2)),
    REALLN = c(NA, NA, diff(log(REALLN), differences = 2)),
    RPI = c(NA, diff(log(RPI))),
    SRVPRD = c(NA, diff(log(SRVPRD))),
    TB3MS = c(NA, diff(TB3MS)),
    TB6MS = c(NA, diff(TB6MS)),
    TOTRESNS = c(NA, NA, diff(log(TOTRESNS), differences = 2)),
    UEMP15OV = c(NA, diff(log(UEMP15OV))),
    UEMP15T26 = c(NA, diff(log(UEMP15T26))),
    UEMP27OV = c(NA, diff(log(UEMP27OV))),
    UEMP5TO14 = c(NA, diff(log(UEMP5TO14))),
    UEMPLT5 = c(NA, diff(log(UEMPLT5))),
    UEMPMEAN = c(NA, diff(UEMPMEAN)),
    UNRATE = c(NA, diff(UNRATE)),
    USCONS = c(NA, diff(log(USCONS))),
    USFIRE = c(NA, diff(log(USFIRE))),
    USGOOD = c(NA, diff(log(USGOOD))),
    USGOVT = c(NA, diff(log(USGOVT))),
    USTPU = c(NA, diff(log(USTPU))),
    USTRADE = c(NA, diff(log(USTRADE))),
    USWTRADE = c(NA, diff(log(USWTRADE))),
    W875RX1 = c(NA, diff(log(W875RX1))),
    WPSFD49207 = c(NA, NA, diff(log(WPSFD49207), differences = 2)),
    WPSFD49502 = c(NA, NA, diff(log(WPSFD49502), differences = 2)),
    WPSID61 = c(NA, NA, diff(log(WPSID61), differences = 2)),
    WPSID62 = c(NA, NA, diff(log(WPSID62), differences = 2)),
    
    AWHMAN = (AWHMAN / 10) - 4,
    CES0600000007 = (CES0600000007 / 10) - 4
    
  )

df <- df %>% na.omit




# =========================================================================
# PCA
# =========================================================================

pcadat <- df %>% select(-ymon)



#single step
pcout <- c()

for (i in 1:(nrow(df)-11)){
  cat(i, "\n")
  
  cdat <- pcadat[1:(i+11),] #12 period expanding window
  
  cpca <- prcomp(cdat)
  
  cpc <- cpca$x[,1] %>% tail(1) #last obs
  
  #cpc <- (1 / (1 + exp(-cpc))) #sigmoid
  #cpc <- ifelse(cpc >= 0, 0, cpc) #neg relu
  
  pcout <- c(pcout, cpc)
  
  
}

plot(pcout)
matplot(pcout, type="p", add=T)


# =========================================================================
# WRITE DATA
# =========================================================================

df_export <- tibble("ymon" = df$ymon %>% tail(length(pcout)),
                    "macropc1" = pcout)

# lagging
df_export <- df_export %>%
  mutate(macropc1 = lag(macropc1, 1)) %>%
  na.omit



write.table(df_export, "data/data_macro.csv", append = F, sep = ",", row.names = F)


#TESTINGGGGG ###########################################################
library(factoextra)

cpca <- prcomp(pcadat)
fviz_eig(cpca) #scree plot
pc1 <- cpca$x[,1]
pc2 <- cpca$x[,2]
plot(pc1, type = "l")
matplot(pc2, type = "l", col = "red", add=T)

plot(diff(pc1), type = "l")

pcadatnorm <- pcadat
for (i in 1:(ncol(pcadatnorm)-1)){pcadatnorm[,i] <- (pcadatnorm[,i] - min(pcadatnorm[,i])) / (max(pcadatnorm[,i]) - min(pcadatnorm[,i]))}
pivvars <- colnames(pcadatnorm)
plotdat <- pcadatnorm %>%
  pivot_longer(pivvars)

ggplot(plotdat) +
  geom_line(aes(x = seq_along(value), y = value, col = name)) +
  theme_bw() + theme(legend.position = "none")



plotdat <- tibble("ymon" = df$ymon,
                  "year" = as.numeric(substr(ymon, 1, 4)),
                  "one" = cpca$x[,1],
                  "two" = cpca$x[,2])

ggplot(plotdat) +
  geom_line(aes(x = seq_along(one), y = one)) +
  geom_line(aes(x = seq_along(one), y = two, col = "red"))

plot(plotdat$one)

pivvars <- colnames(df)[-1]
plotdat <- df %>%
  pivot_longer(pivvars)

ggplot(plotdat) +
  geom_line(aes(x = seq_along(value), y = value, col = name))

statedf <- bind_cols(df, "state" = cpca$x[,1]) %>% select(-ymon)

# traintest split
split_ratio <- 0.75
x_vars <- colnames(statedf)[1:108] #hack
y_vars <- "state"

split_index <- 1:floor(split_ratio * nrow(statedf))

train <- statedf[ split_index,]
test  <- statedf[-split_index,]

fit <- lm(state ~ ., train)

pred <- predict(fit, newdata = test)

test <- tibble(pred, test$state)


###############################################

df$ymon <- as.character(df$ymon)
df <- df %>%
  select(-macropc1) %>%
  left_join(df_export, by = "ymon")
fit <- lm(RET ~ macropc1, df)
summary(fit)
plot(df$macropc1, df$RET)





