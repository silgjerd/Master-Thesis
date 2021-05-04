rm(list=ls());options(scipen=999,stringsAsFactors=F)
library(tidyverse);library(vroom)
library(keras);library(caret);library(glmnet);library(xgboost)
source("functions.R")

load("data/x_fullsample.RData")

# =========================================================================
# ELASTIC NET
# =========================================================================

# Build
control <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 5,
                        search = "random",
                        verboseIter = TRUE)
# Train
elastic_model <- train(RET ~ .,
                       data = cbind(x_train, y_train),
                       method = "glmnet",
                       #preProcess = c("center", "scale"),
                       tuneLength = 25,
                       trControl = control)

# Test
pred <- predict(elastic_model, x_test)
eval <- mevaluate(pred, y_test)

eval
plot(pred, y_test)
abline(a=0,b=1)
abline(a=summary(lm(y_test~pred))$coef[1],b=summary(lm(y_test~pred))$coef[2],col="red")




#####################################################################
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
#####################################################################
# Feature sampling
#####################################################################

# Model
getEVAL <- function(x_train, y_train, x_test, y_test){
  
  # Build
  control <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 1,
                          search = "random",
                          verboseIter = TRUE)
  # Train
  elastic_model <- train(RET ~ .,
                         data = cbind(x_train, y_train),
                         method = "glmnet",
                         #preProcess = c("center", "scale"),
                         tuneLength = 25,
                         trControl = control)
  
  # Test
  pred <- predict(elastic_model, x_test)
  eval <- mevaluate(pred, y_test)
  return(eval)
}




output <- c()

samples <- list(
  "sample" = c("none","E","S","G",
               #"es","eg","sg",
               "Scores","ESG"),
  "excols" = list(c(which(colnames(x_train) %in% esgvars)),
                  c(which(colnames(x_train) %in% c(esg_s, esg_g))),
                  c(which(colnames(x_train) %in% c(esg_e, esg_g))),
                  c(which(colnames(x_train) %in% c(esg_e, esg_s))),
                  c(which(colnames(x_train) %in% c(esg_g))),
                  c(which(colnames(x_train) %in% c(esg_s))),
                  c(which(colnames(x_train) %in% c(esg_e))),
                  c(which(colnames(x_train) %in% c(esg_scores))),
                  NULL)
)

for (sample in samples$sample){
  cat(sample,":")
  cat(samples$excols[[which(samples$sample==sample)]],"\n")
  
  excols <- samples$excols[[which(samples$sample==sample)]]
  if (is.null(excols)){
    cur_x_train <- x_train
    cur_x_test  <- x_test
  } else {
    cur_x_train <- x_train[,-excols]
    cur_x_test  <- x_test[,-excols]
  }
  
  
  for (i in 1:1){
    cat(i,"\n")
    
    eval <- getEVAL(cur_x_train, y_train, cur_x_test, y_test)
    
    output <- output %>% bind_rows(tibble(
      "sample" = sample,
      eval
    ))
    
  }
  
}







# ===========================================================================
# PLOTS
# ===========================================================================

# Boxplot

output %>%
  ggplot(aes(x = sample))+
  geom_boxplot(aes(y = MSE))+
  theme_bw()

# Diff
diff <- output %>%
  group_by(sample) %>%
  summarise(meanMSE = mean(MSE),
            meanRSQ = mean(RSQ))

diff <- diff %>%
  mutate(diffMSE = meanMSE - diff$meanMSE[diff$sample=="none"],
         diffRSQ = meanRSQ - diff$meanRSQ[diff$sample=="none"])


#barplot
diff %>%
  ggplot()+
  geom_point(aes(x=sample,y=diffMSE))+
  geom_hline(yintercept = 0)

output <- output %>%
  mutate(MSEmcontrol = MSE - diff$meanMSE[diff$sample=="none"],
         RSQmcontrol = RSQ - diff$meanRSQ[diff$sample=="none"])

#boxplot
library(plyr)
output$sample <- revalue(output$sample, c("s"="S","e"="E","g"="G","all"="ESG"))
output <- output %>% mutate(sample = ordered(sample, c("ESG","G","S","E")))

# forest plot
output %>%
  filter(sample!="none") %>%
  filter(sample!="scores") %>%
  mutate(sample = as.factor(sample)) %>%
  ggplot(aes(x = sample))+
  geom_boxplot(aes(y = MSEmcontrol), outlier.color = "white", width = .3)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  labs(x="",y="Difference in MSE")+
  coord_flip()+
  #scale_y_reverse()+
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())


# ===========================================================================
# Category shuffling/randomizing
# ===========================================================================

output <- c()

samples <- list(
  "sample" = c("none","E","S","G",
               #"es","eg","sg",
               "Scores","ESG"),
  "exvars" = list(esgvars ,esg_e, esg_s, esg_g, esg_scores, NULL)
)


for (m in 1:2){ # simulations of models
  control <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 1,
                          search = "random",
                          verboseIter = TRUE)
  
  elastic_model <- train(RET ~ .,
                         data = cbind(x_train, y_train),
                         method = "glmnet",
                         #preProcess = c("center", "scale"),
                         tuneLength = 25,
                         trControl = control)
  
  
  
  
  
  for (sample in samples$sample){
    cat(sample,"\n")
    
    for (i in 1:100){ # simulations of noise
      #cat(i,"\n")
      
      # Randomize group of vars
      rand_x_test <- x_test
      
      for (cvar in samples$exvars[[which(samples$sample==sample)]]) {
        #cat(cvar,"\n")
        
        ci <- which(colnames(x_test)==cvar) #index of var
        
        noise <- runif(nrow(x_test), -0.5, 0.5) #randomized values
        # noise <- 0
        
        rand_x_test[,ci] <- noise #replace with randomzied values
      }
      
      # Test
      pred <- predict(elastic_model, rand_x_test)
      eval <- mevaluate(pred, y_test)
      
      output <- output %>% bind_rows(tibble(
        "sample" = sample,
        eval
      ))
      
    }
    
  }
}










############################################################################


# Variable importance

importance <- varImp(elastic_model)
importance <- importance[["importance"]]
importance <- rownames_to_column(importance) %>% arrange(-Overall)



