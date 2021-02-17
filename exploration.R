rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(PerformanceAnalytics);library(RColorBrewer);library(corrplot)
source("functions.R")

df <- vroom("data/dataset.csv")
df <- df %>% na.omit

# Exclude variables
df <- df %>%
  select(-c(
    PERMNO,
    SICCD,
    PRC,
    VOL,
    SHROUT,
    LOGRET,
    ymon,
    FF_12
  ))


# =========================================================================
# Correlation matrix scatterplot
# =========================================================================
# Past returns
plotdat <- df %>%
  #select(RET,
  #       chmom, r12_2, r12_7, r2_1, r36_13, r6_2, strev) %>%
  sample_n(1000)


chart.Correlation(plotdat, histogram = T, pch = 19)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
M <- cor(df)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(plotdat)
corrplot(M, method="color", col=col(200),
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

corrplot(M, method = "color")
