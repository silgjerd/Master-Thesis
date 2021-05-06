rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(keras);library(caret);library(glmnet);library(xgboost)
source("functions.R")

load("data/x_fullsample.RData")

# remove macropc1
x_train <- x_train[,-which(colnames(x_train)=="macropc1")]
x_test <- x_test[,-which(colnames(x_test)=="macropc1")]

# =================================================================================
# XGBoost

# Train
mxgb <- xgb.train(
  
  eta = 0.1,
  gamma = 0,
  max_depth = 2,
  min_child_weight = 15,
  subsample = 0.6,
  colsample_bytree = 0.5,
  alpha = 0.1,
  lambda = 0.01,
  nrounds = 800,
  
  data = xgb.DMatrix(x_train, label = y_train),
  booster = "gbtree",
  objective = "reg:squarederror",
  tree_method = "auto",
  early_stopping_rounds = 10,
  watchlist = list(train = xgb.DMatrix(x_train, label = y_train),
                   val = xgb.DMatrix(x_test, label = y_test)),
  eval_metric = "rmse",
  verbose = 1
  
)
mxgb <- xgb.train(#0.01
  
  eta = 0.01,
  gamma = 0,
  max_depth = 3,
  min_child_weight = 5,
  subsample = 0.8,
  colsample_bytree = 0.5,
  alpha = 0.1,
  lambda = 0.01,
  nrounds = 800,
  
  data = xgb.DMatrix(x_train, label = y_train),
  booster = "gbtree",
  objective = "reg:squarederror",
  tree_method = "auto",
  early_stopping_rounds = 10,
  watchlist = list(train = xgb.DMatrix(x_train, label = y_train),
                   val = xgb.DMatrix(x_test, label = y_test)),
  eval_metric = "rmse",
  verbose = 1
  
)



# Test
pred <- predict(mxgb, xgb.DMatrix(x_test, label = y_test))
eval <- mevaluate(pred, y_test)

eval
plot(pred, y_test)
abline(a=0,b=1)
abline(a=summary(lm(y_test~pred))$coef[1],b=summary(lm(y_test~pred))$coef[2],col="red")




# =================================================================================
# Univariate effect


univar <- seq(-0.49, 0.5, 0.01)


# All vars = value
samevaluefunc <- function(x){return(0)}
mean_x_test <- x_test %>%
  as_tibble() %>%
  summarise_all(mean) %>%
  mutate_all(samevaluefunc)


# Specified var = value
samevaluefunc <- function(x){return(-0.5)}
mean_x_test <- x_test %>%
  as_tibble() %>%
  summarise_all(mean) %>%
  mutate_at(c("lme"), samevaluefunc)


# All vars = mean
mean_x_test <- x_test %>%
  as_tibble() %>%
  summarise_all(mean)



mean_x_test <- tibble(mean_x_test, .rows = 101) %>%
  mutate("carbonint" = univar) %>%
  as.matrix()

#pred <- predict(mxgb, xgb.DMatrix(mean_x_test, label = matrix(seq_len(nrow(mean_x_test)))))
#pred <- predict(elastic_model, mean_x_test)
pred <- model %>% predict(mean_x_test)

plot(univar, pred, type = "l")



# At different lme (save plots as files)
for (i in seq(-0.5,0.5,0.05)){
  samevaluefunc <- function(x){return(i)}
  mean_x_test <- x_test %>%
    as_tibble() %>%
    summarise_all(mean) %>%
    mutate_at(c("lme"), samevaluefunc)
  
  mean_x_test <- tibble(mean_x_test, .rows = 101) %>%
    mutate("esgg" = univar) %>%
    as.matrix()
  
  pred <- predict(mxgb, xgb.DMatrix(mean_x_test, label = matrix(seq_len(nrow(mean_x_test)))))
  
  
  png(file=paste0("figures/unieffects/unieffect_", i, ".png"))
  plot(pred, type = "l")
  dev.off()
  
  
}




# Multilevel plot (log as tibble)
cvar <- "beta" #passive var (colors)
plotdat <- c()
for (i in seq(-0.5,0.5,0.1)){
  
  samevaluefunc <- function(x){return(i)}
  mean_x_test <- x_test %>%
    as_tibble() %>%
    summarise_all(mean) %>%
    mutate_at(c(cvar), samevaluefunc)
  
  mean_x_test <- tibble(mean_x_test, .rows = length(univar)) %>%
    mutate("leverage" = univar) %>% #active var (x-axis)
    as.matrix()
  
  
  #pred <- predict(elastic_model, mean_x_test)
  #pred <- predict(mxgb, xgb.DMatrix(mean_x_test, label = matrix(seq_len(nrow(mean_x_test)))))
  pred <- model %>% predict(mean_x_test)
  
  plotdat <- plotdat %>% bind_rows(tibble("cvar" = "dunnoyeye",
                                          "valuepassive" = i,
                                          "valueactive" = univar,
                                          "pred" = pred))
  
  
}

plotdat %>%
  ggplot()+
  geom_line(aes(x = valueactive, y = pred, col = as.factor(valuepassive)))+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  labs(x="active",y="Predicted excess return",col="passive")+scale_color_brewer(palette="RdYlGn",direction=1)


# # 3D PLOT TEST--
# library(plotly)
# 
# plotdat3d <- matrix(plotdat$pred, nrow = 5, ncol = length(univar)/5)
# 
# fig <- plot_ly(x = 1:5, y = 1:20,
#                z = plotdat3d, type = "surface")
# 
# fig <- fig %>% layout(scene = list(
#   xaxis = list(title = "Size", dtick = 1),
#   yaxis = list(title = "B/M", dtick = 1),
#   zaxis = list(title = "MSE")))
# fig
# 
# 
# #----




#LOOP
for (cvar in colnames(x_train)){
  plotdat <- c()
  for (i in seq(-0.5,0.5,0.1)){
    
    samevaluefunc <- function(x){return(i)}
    mean_x_test <- x_test %>%
      as_tibble() %>%
      summarise_all(mean) %>%
      mutate_at(c(cvar), samevaluefunc)
    
    mean_x_test <- tibble(mean_x_test, .rows = length(univar)) %>%
      mutate("beta" = univar) %>% #active
      as.matrix()
    
    #pred <- predict(mxgb, xgb.DMatrix(mean_x_test, label = matrix(seq_len(nrow(mean_x_test)))))
    #pred <- predict(elastic_model, mean_x_test)
    pred <- model %>% predict(mean_x_test)
    
    plotdat <- plotdat %>% bind_rows(tibble("cvar" = "dunnoyeye",
                                            "valuepassive" = i,
                                            "valueactive" = univar,
                                            "pred" = pred))
    
    
  }
  
  p <- plotdat %>%
    ggplot()+
    geom_line(aes(x = valueactive, y = pred, col = as.factor(valuepassive)))+
    theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    labs(x="active",y="Predicted excess return",col=cvar)+scale_color_brewer(palette="RdYlGn",direction=1)
  
  width <- 166
  height <- floor(width * 11/16)
  ggsave(paste0("figures/bieffects/bieffect_", cvar, ".png"),p,
         width = width,
         height = height,
         dpi = 500,
         units = "mm")
  
  
  
}


# Multimodel ===============

# All vars = mean
mean_x_test <- x_test %>%
  as_tibble() %>%
  summarise_all(mean)


mean_x_test <- tibble(mean_x_test, .rows = 101) %>%
  mutate("esgcsr" = univar) %>%
  as.matrix()

pred_en  <- predict(elastic_model, mean_x_test)
pred_xgb <- predict(mxgb, xgb.DMatrix(mean_x_test, label = matrix(seq_len(nrow(mean_x_test)))))
pred_nn  <- pred <- model %>% predict(mean_x_test)

# Plot
p <- tibble(
  univar,
  "EN" = pred_en,
  "XGB" = pred_xgb,
  "NN" = pred_nn
) %>%
  #pivot_longer(c(pred_en, pred_xgb, pred_nn)) %>%
  pivot_longer(c(EN,XGB,NN)) %>%
  mutate(name=factor(name,levels=c("EN","XGB","NN")))%>%
  ggplot(aes(x=univar))+
  geom_line(aes(y=value,col=name))+
  labs(x="r12_2",y="Predicted return",col="Model")+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())


p

#univar <- seq(0.5, -0.5, -0.01)

width <- 166
height <- floor(width * 11/16)
ggsave("figures/modeleffects/r12_2.png",p,
       width = width,
       height = height,
       dpi = 500,
       units = "mm")


# gridplot
#1
mean_x_test <- x_test %>%
  as_tibble() %>%
  summarise_all(mean)
mean_x_test <- tibble(mean_x_test, .rows = 101) %>%
  mutate("carbonint" = univar) %>%
  as.matrix()
pred_en  <- predict(elastic_model, mean_x_test)
pred_xgb <- predict(mxgb, xgb.DMatrix(mean_x_test, label = matrix(seq_len(nrow(mean_x_test)))))
pred_nn  <- pred <- model %>% predict(mean_x_test)

p1 <- tibble(
  univar,
  "EN" = pred_en,
  "XGB" = pred_xgb,
  "NN" = pred_nn
) %>%
  pivot_longer(c(EN,XGB,NN)) %>%
  mutate(name=factor(name,levels=c("EN","XGB","NN")))%>%
  ggplot(aes(x=univar))+
  geom_line(aes(y=value,col=name), show.legend = FALSE)+
  labs(x="carbonint",y="Predicted return",col="")+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

#2
mean_x_test <- x_test %>%
  as_tibble() %>%
  summarise_all(mean)
mean_x_test <- tibble(mean_x_test, .rows = 101) %>%
  mutate("esgcomm" = univar) %>%
  as.matrix()
pred_en  <- predict(elastic_model, mean_x_test)
pred_xgb <- predict(mxgb, xgb.DMatrix(mean_x_test, label = matrix(seq_len(nrow(mean_x_test)))))
pred_nn  <- pred <- model %>% predict(mean_x_test)

p2 <- tibble(
  univar,
  "EN" = pred_en,
  "XGB" = pred_xgb,
  "NN" = pred_nn
) %>%
  pivot_longer(c(EN,XGB,NN)) %>%
  mutate(name=factor(name,levels=c("EN","XGB","NN")))%>%
  ggplot(aes(x=univar))+
  geom_line(aes(y=value,col=name), show.legend = FALSE)+
  labs(x="esgcomm",y="Predicted return",col="Model")+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
#3
mean_x_test <- x_test %>%
  as_tibble() %>%
  summarise_all(mean)
mean_x_test <- tibble(mean_x_test, .rows = 101) %>%
  mutate("boardattend" = univar) %>%
  as.matrix()
pred_en  <- predict(elastic_model, mean_x_test)
pred_xgb <- predict(mxgb, xgb.DMatrix(mean_x_test, label = matrix(seq_len(nrow(mean_x_test)))))
pred_nn  <- pred <- model %>% predict(mean_x_test)

p3 <- tibble(
  univar,
  "EN" = pred_en,
  "XGB" = pred_xgb,
  "NN" = pred_nn
) %>%
  pivot_longer(c(EN,XGB,NN)) %>%
  mutate(name=factor(name,levels=c("EN","XGB","NN")))%>%
  ggplot(aes(x=univar))+
  geom_line(aes(y=value,col=name),show.legend = F)+
  labs(x="boardattend",y="Predicted return",col="Model")+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

p <- gridExtra::arrangeGrob(p1,p2,p3,ncol=3)

width <- 166*2
height <- floor(width * 5/14)
ggsave("figures/modeleffects/esgmodeleff.png",p,
       width = width,
       height = height,
       dpi = 500,
       units = "mm")

###

# gridplot
#1
mean_x_test <- x_test %>%
  as_tibble() %>%
  summarise_all(mean)
mean_x_test <- tibble(mean_x_test, .rows = 101) %>%
  mutate("lme" = univar) %>%
  as.matrix()
pred_en  <- predict(elastic_model, mean_x_test)
pred_xgb <- predict(mxgb, xgb.DMatrix(mean_x_test, label = matrix(seq_len(nrow(mean_x_test)))))
pred_nn  <- pred <- model %>% predict(mean_x_test)

p1s <- tibble(
  univar,
  "EN" = pred_en,
  "XGB" = pred_xgb,
  "NN" = pred_nn
) %>%
  pivot_longer(c(EN,XGB,NN)) %>%
  mutate(name=factor(name,levels=c("EN","XGB","NN")))%>%
  ggplot(aes(x=univar))+
  geom_line(aes(y=value,col=name), show.legend = T)+
  labs(x="lme",y="Predicted return",col="")+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),legend.position = c(0.85, 0.8))

#2
mean_x_test <- x_test %>%
  as_tibble() %>%
  summarise_all(mean)
mean_x_test <- tibble(mean_x_test, .rows = 101) %>%
  mutate("bm" = univar) %>%
  as.matrix()
pred_en  <- predict(elastic_model, mean_x_test)
pred_xgb <- predict(mxgb, xgb.DMatrix(mean_x_test, label = matrix(seq_len(nrow(mean_x_test)))))
pred_nn  <- pred <- model %>% predict(mean_x_test)

p2s <- tibble(
  univar,
  "EN" = pred_en,
  "XGB" = pred_xgb,
  "NN" = pred_nn
) %>%
  pivot_longer(c(EN,XGB,NN)) %>%
  mutate(name=factor(name,levels=c("EN","XGB","NN")))%>%
  ggplot(aes(x=univar))+
  geom_line(aes(y=value,col=name), show.legend = FALSE)+
  labs(x="bm",y="Predicted return",col="Model")+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
#3
univar <- seq(0.5, -0.5, -0.01)
mean_x_test <- x_test %>%
  as_tibble() %>%
  summarise_all(mean)
mean_x_test <- tibble(mean_x_test, .rows = 101) %>%
  mutate("r12_2" = univar) %>%
  as.matrix()
univar <- seq(-0.5, 0.5, 0.01)
pred_en  <- predict(elastic_model, mean_x_test)
pred_xgb <- predict(mxgb, xgb.DMatrix(mean_x_test, label = matrix(seq_len(nrow(mean_x_test)))))
pred_nn  <- pred <- model %>% predict(mean_x_test)

p3s <- tibble(
  univar,
  "EN" = pred_en,
  "XGB" = pred_xgb,
  "NN" = pred_nn
) %>%
  pivot_longer(c(EN,XGB,NN)) %>%
  mutate(name=factor(name,levels=c("EN","XGB","NN")))%>%
  ggplot(aes(x=univar))+
  geom_line(aes(y=value,col=name), show.legend = FALSE)+
  labs(x="r12_2",y="Predicted return",col="Model")+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())


p <- gridExtra::arrangeGrob(p1s,p2s,p3s,p1,p2,p3,ncol=3,nrow=2) #PLOT

width <- 166*2
height <- floor(width * 5*1.5/14)
ggsave("figures/modeleffects/allmodeleff2.png",p,
       width = width,
       height = height,
       dpi = 500,
       units = "mm")

# =================================================================================
# SHAP

library(SHAPforxgboost)#;library(shapr)

esgvars <- c(    "carbonint",
                 "energyint",
                 "waterint",
                 "wastegen",
                 "femexec",
                 "fememp",
                 "turnemp",
                 "tradeunion",
                 "lostdays",
                 "boardindep",
                 "boardfem",
                 "boardattend",
                 "boardsize",
                 "execcomp",
                 "nonexecs",
                 "boardterm",
                 "boardcomp",
                 
                 "esgscore",
                 "esgcomb",
                 "esgcontr",
                 "esge",
                 "esgg",
                 "esgs",
                 "esgres",
                 "esgemi",
                 "esginn",
                 "esgwor",
                 "esghum",
                 "esgcomm",
                 "esgpro",
                 "esgman",
                 "esgcsr"
)
esg_e <- c("carbonint",
           "energyint",
           "waterint",
           "wastegen",
           "esge",
           "esgres",
           "esgemi",
           "esginn"
)
esg_s <- c("femexec",
           "fememp",
           "turnemp",
           "tradeunion",
           "lostdays",
           "esgs",
           "esgwor",
           "esghum",
           "esgcomm",
           "esgpro"
)

esg_g <- c("boardindep",
           "boardfem",
           "boardattend",
           "boardsize",
           "execcomp",
           "nonexecs",
           "boardterm",
           "boardcomp",
           "esgg",
           "esgman",
           "esgcsr")

esg_scores <- c("esgscore","esgcomb","esgcontr")


# SHAP

x_train_sample <- x_train
x_train_sample <- x_train[sample(seq_len(nrow(x_train)), 100),]

shap_long <- shap.prep(xgb_model = mxgb, X_train = x_train_sample)
#shap_long <- shap_long %>% filter(variable %in% c("macropc1", "r12_2"))
shap_long <- shap_long %>% filter(variable %in% esg_e)# %>% filter(abs(value) > 0.00001)
shap_long$variable <- as.factor(as.character(shap_long$variable)) #remove filtered vars from factor levels

p1 <- shap.plot.summary(shap_long)+labs(title="Environmental")

shap_long <- shap.prep(xgb_model = mxgb, X_train = x_train_sample)
#shap_long <- shap_long %>% filter(variable %in% c("macropc1", "r12_2"))
shap_long <- shap_long %>% filter(variable %in% esg_s) #%>% filter(abs(value) > 0.00001)
shap_long$variable <- as.factor(as.character(shap_long$variable)) #remove filtered vars from factor levels

p2 <- shap.plot.summary(shap_long)+labs(title="Social")

shap_long <- shap.prep(xgb_model = mxgb, X_train = x_train_sample)
#shap_long <- shap_long %>% filter(variable %in% c("macropc1", "r12_2"))
shap_long <- shap_long %>% filter(variable %in% esg_g) #%>% filter(abs(value) > 0.00001)
shap_long$variable <- as.factor(as.character(shap_long$variable)) #remove filtered vars from factor levels

p3 <- shap.plot.summary(shap_long)+labs(title="Governance")


#gridExtra::grid.arrange(p1, p2, p3, ncol=3)



p <- gridExtra::arrangeGrob(p1,p2,p3,ncol=3)

width <- 166*2
height <- floor(width * 5/14)
ggsave("shap.png",p,
       width = width,
       height = height,
       dpi = 500,
       units = "mm")



# TESTING =============


importance_ints <- EIX::importance(mxgb, x_train, option = "interactions")
data.frame(importance_ints$Feature, importance_ints$sumGain) 
total_gain_ints <- sum(importance_ints$sumGain)
total_gain_ints
plot(importance_ints)

pairs <- EIX::interactions(mxgb, x_train, option="pairs")
plot(pairs)




