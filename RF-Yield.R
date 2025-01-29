# wheat Yield migration(RF-yield)
# This code is for RF-yield model training and simulation



library(dplyr)
library(rio)
library(randomForest)
library(caret)
library(data.table)



## step1 : model training & evaluation ===================

### import data
f <- fread('.../RF-Yield.csv')

data <-  f  


#K folder
CVgroup <- function(k, datasize, seed){  
  cvlist <- list()  
  set.seed(seed)  
  n <- rep(1:k, ceiling(datasize/k))[1:datasize]   
  temp <- sample(n, datasize) 
  x <- 1:k  
  dataseq <- 1: datasize   
  cvlist <- lapply(x, function(x) dataseq[temp==x])  
  return(cvlist)  
}

datasize <- nrow(data)
cvlist <- CVgroup(k = 5, datasize = datasize, seed =42) 


out1 <- data.frame()
for (i in 1:5){
  print(i)
  train <- data[-cvlist[[i]], ]
  test <- data[cvlist[[i]], ]
  fit <- randomForest(Yield~., train, importance=TRUE, mtry = 3, ntree=500)
  importance(fit)
  train.pre <-  predict(fit, test)
  tem <- data.frame(obs = test$Yield, pred = train.pre)
  out1 <- rbind(out1, tem)
}


rsq <- cor(out1$obs, out1$pred)^2
rmse <- RMSE(out1$obs, out1$pred)
Nrmse <- 100*rmse/mean(out1$obs)

print(paste0('r2: ', rsq, '  RMSE: ', rmse,'  NRMSE: ', Nrmse, '%'))





## step2 : OPD simulation ============


fit <- randomForest(Yield ~ ., data, importance = TRUE, mtry = 3, ntree = 500)

new_data <-  read_excel(".../predicteddara1-6-24.xlsx")

predictions <- predict(fit, new_data)












