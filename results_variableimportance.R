rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)

df <- vroom("data/variable_importance_dat.csv")

df1 <- df[,1:2]
df2 <- df[,3:4]
df3 <- df[,5:6]

df2 <- df2[1:59,]


dftot <- merge(df1, df2, by.x = "var1", by.y = "var2", all = T)
dftot <- merge(dftot, df3, by.x = "var1", by.y = "var3", all = T)


# dftot <- full_join(df1, df2, by = c("var1" = "var2"))
# dftot <- full_join(dftot, df3, by = c("var1" = "var3"))


write.table(dftot, file = "vi_test.csv", sep=",",row.names = F)
