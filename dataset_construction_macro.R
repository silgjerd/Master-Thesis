rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(zoo);library(lubridate);library(vroom);library(factoextra);library(ClustOfVar)
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
  
  pcout <- c(pcout, cpc)
  
  
}

plot(pcout, type = "l")
plot(tail(as.Date(paste0(df$ymon,"01"), format = "%Y%m%d"), length(pcout)), pcout, type = "l")

matplot(pcout, type="p", add=T)

# =========================================================================
# CLUSTERS
# =========================================================================

### GROUPING
kmsdat <- df %>% select(-ymon)

tree <- hclustvar(kmsdat %>% as.matrix)
stab <- stability(tree, B=4)

nclusters <- 5
kms <- kmeansvar(kmsdat %>% as.matrix, init = nclusters)
summary(kms)
plot(tree)

cluster_names <- paste0("macroc_", 1:nclusters)
result <- c()

for (i in 1:nclusters){
  
  cat(cluster_names[i], "\n")
  
  select_cluster <- kms$cluster[kms$cluster==i] %>% names
  cdf <- kmsdat %>% select(select_cluster)
  
  pcout <- c()
  
  for (j in 1:(nrow(cdf)-11)){
    # cat(j, "\n")
    cdat <- cdf[1:(j+11),] #12 period expanding window
    cpca <- prcomp(cdat)
    cpc <- cpca$x[,1] %>% tail(1) #last obs
    pcout <- c(pcout, cpc)
  }
  
  result <- bind_rows(result, tibble("cluster" = cluster_names[i],
                                     "pc" = pcout))
  
}

#PLOT1
p1 <- result %>% bind_cols("id" = rep(tail(as.Date(paste0(df$ymon,"01"), format = "%Y%m%d"), length(pcout)), nclusters)) %>%
  filter(id > "1993-01-01")%>%
  
ggplot()+
  geom_line(aes(x = id, y = pc, col = as.factor(cluster)))+theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  labs(x="",y="",col="")+theme(legend.position='none')+scale_color_brewer(palette="Set1",direction=1)

#PLOT2
p2 <- tibble(ymon = tail(as.Date(paste0(df$ymon,"01"), format = "%Y%m%d"), length(pcout)),
       pc = pcout)%>%
  filter(ymon > "1993-01-01")%>%
  
ggplot()+
  geom_line(aes(x=ymon,y=pc))+theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  labs(x="",y="First principal component",col="")

gridExtra::grid.arrange(p2, p1, ncol = 2) #POSSIBLE PAPER PLOT





# NOT USED----
result$cluster <- as.factor(result$cluster)
result <- result %>%
  mutate(ymon = rep(rep(tail(as.Date(paste0(df$ymon,"01"), format = "%Y%m%d"), length(pcout))), nclusters))
testres <- tibble(
  "ymon" = rep(tail(as.Date(paste0(df$ymon,"01"), format = "%Y%m%d"), length(pcout))),
  "m1" = result$pc[result$cluster=="macroc_1"],
  "m2" = result$pc[result$cluster=="macroc_2"],
  "m3" = result$pc[result$cluster=="macroc_3"],
  "m4" = result$pc[result$cluster=="macroc_4"],
  "m5" = result$pc[result$cluster=="macroc_5"]
)






# TEST
library(dendextend)
d_iris <- dist(t(kmsdat)) # method="man" # is a bit better
hc_iris <- hclust(d_iris, method = "complete")
dend <- as.dendrogram(hc_iris)
dend <- rotate(dend, 1:110)
dend <- color_branches(dend, k=5) #, groupLabels=iris_species)

dend <- hang.dendrogram(dend,hang_height=0.01)

par(mar = c(3,3,3,7))
plot(dend, horiz=TRUE, nodePar = list(cex = .5))
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

# CORMATRIX
library(corrplot)
M <- cor(pcadat)
corrplot(M, method = "color", type = "lower", tl.col="white")


corr_simple <- function(data=pcadat,sig=0){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ", method="color", type="upper")
}
corr_simple()


plotsave("macro_corplot.png")


##################################################################
# TESTING WELCH GOYAL
welchgoyal <- read_csv("data_raw/welchgoyal.csv", col_types = cols(DATE = col_date(format = "%Y%m"))) %>%
  rename(dp = D12,
         ep = E12,
         bm = `b/m`) %>%
  select(-c(AAA, BAA))

testres <- left_join(testres, welchgoyal, by = c("ymon" = "DATE")) %>% na.omit()


lm(dp ~ m1+m2+m3+m4+m5, testres) %>% summary
lm(ep ~ m1+m2+m3+m4+m5, testres) %>% summary
lm(tbl ~ m1+m2+m3+m4+m5, testres) %>% summary
lm(lty ~ m1+m2+m3+m4+m5, testres) %>% summary
lm(ntis ~ m1+m2+m3+m4+m5, testres) %>% summary
lm(infl ~ m1+m2+m3+m4+m5, testres) %>% summary
lm(ltr ~ m1+m2+m3+m4+m5, testres) %>% summary
lm(corpr ~ m1+m2+m3+m4+m5, testres) %>% summary
lm(svar ~ m1+m2+m3+m4+m5, testres) %>% summary

lm(dp ~ m1+m2, testres) %>% summary
lm(ep ~ m1+m2, testres) %>% summary
lm(tbl ~ m1+m2, testres) %>% summary
lm(lty ~ m1+m2, testres) %>% summary
lm(ntis ~ m1+m2, testres) %>% summary
lm(infl ~ m1+m2, testres) %>% summary
lm(ltr ~ m1+m2, testres) %>% summary
lm(corpr ~ m1+m2, testres) %>% summary
lm(svar ~ m1+m2, testres) %>% summary



testres <- left_join(testres, tibble("ymon" = tail(as.Date(paste0(df$ymon,"01"), format = "%Y%m%d"), length(pcout)),
                                     "pc" = pcout),
                     by = "ymon")


lm(dp ~ pc, testres) %>% summary
lm(ep ~ pc, testres) %>% summary
lm(tbl ~ pc, testres) %>% summary
lm(lty ~ pc, testres) %>% summary
lm(ntis ~ pc, testres) %>% summary
lm(infl ~ pc, testres) %>% summary
lm(ltr ~ pc, testres) %>% summary
lm(corpr ~ pc, testres) %>% summary
lm(svar ~ pc, testres) %>% summary

# WELCH GOYAL PCA
library(ggfortify)


wgpca <- prcomp(testres[,c("m1","m2","m3","m4","m5","tbl")])
p1 <- autoplot(wgpca, loadings=T)+theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  labs(title="tbl")

wgpca <- prcomp(testres[,c("m1","m2","m3","m4","m5","ntis")])
p2 <- autoplot(wgpca, loadings=T)+theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  labs(title="ntis")

wgpca <- prcomp(testres[,c("m1","m2","m3","m4","m5","ltr")])
p3 <- autoplot(wgpca, loadings=T)+theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  labs(title="ltr")

wgpca <- prcomp(testres[,c("m1","m2","m3","m4","m5","svar")])
p4 <- autoplot(wgpca, loadings=T)+theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  labs(title="svar")

gridExtra::grid.arrange(p1,p2,p3,p4,ncol=2)


wgpca <- prcomp(testres[,c("m1","m2","m3","m4","m5")])
p1 <- autoplot(wgpca, loadings=T)+theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  labs(title="Cluster components")

wgpca <- prcomp(testres[,c("m1","m2","m3","m4","m5","tbl","ntis","ltr","svar")])
p2 <- autoplot(wgpca, loadings=T)+theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  labs(title="+ tbl, ntis, ltr, svar")

wgpca <- prcomp(testres[,c("m1","m2","m3","m4","m5", "ep", "dp", "tbl","ntis","ltr","svar")])
p3 <- autoplot(wgpca, loadings=T)+theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  labs(title="+ dp, ep")

gridExtra::grid.arrange(p1,p2,p3,ncol=3)

fviz_eig(wgpca) #scree plot
wgpc <- wgpca$x[,1]







