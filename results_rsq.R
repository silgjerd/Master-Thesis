
df <- vroom("results/alloutput.csv")
##############################################################################

unique(df$model) #check models
df <- df %>% filter(model != "xgb01")

df <- df %>% filter(model=="en")

df <- df %>% na.omit()

unique(df$model) #check models




# MEAN FOR NONE
meannone <- df %>%
  group_by(model,m,sample)%>%
  summarise(nonersq = mean(RSQ))%>%
  filter(sample=="none")%>%na.omit()%>%select(-sample)

# HIST PLOT
df %>%
  right_join(meannone, by = c("model","m"))%>%
  select(-c(MSE,COR,SCORE))%>%
  mutate(diff = RSQ - nonersq,
         mnum = as.factor(paste0(model, m)))%>%
  
  filter(sample=="none")%>%
  ggplot()+
  geom_histogram(aes(y = diff))



# T TEST
df %>%
  right_join(meannone, by = c("model","m"))%>%
  select(-c(MSE,COR,SCORE))%>%
  mutate(diff = RSQ - nonersq,
         mnum = as.factor(paste0(model, m))) %>%
  
  filter(sample=="ESG")%>%
  pull(diff) %>% t.test()


# Mean all
mean_of_samples <- df %>%
  group_by(sample) %>%
  summarise(meanrsq = mean(RSQ)*100)






# MSE ====
meannone <- df %>%
  group_by(model,m,sample)%>%
  summarise(nonemse = mean(MSE))%>%
  filter(sample=="none")%>%na.omit()%>%select(-sample)

# t test
df %>%
  right_join(meannone, by = c("model","m"))%>%
  select(-c(RSQ,COR,SCORE))%>%
  mutate(diff = MSE - nonemse,
         mnum = as.factor(paste0(model, m))) %>%
  
  filter(sample=="G")%>%
  pull(diff) %>% t.test()




