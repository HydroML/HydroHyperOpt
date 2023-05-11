rm(list=ls())

library(ranger)
library(xgboost)


#RF setup
rfdata <- list.files("C:/Users/joeja/OneDrive/REX_2020_2021/MetaResults2/RF",full.names=T) 

alldata <- NULL
for(x in 1:length(rfdata)) {
  curDat <- read.csv(rfdata[x])
  curDat$kgeTest[curDat$kgeTest<0]<-0
  curDat$nseTest[curDat$nseTest<0]<-0
  curDat$kgeTest<-curDat$kgeTest-curDat$kgeTest[2]
  curDat$nseTest<-curDat$nseTest-curDat$nseTest[2]
  alldata <- rbind(alldata,curDat)
}
rm(curDat,rfdata,x)

#RF with KGE
mod<-ranger(x=alldata[,2:6],y=alldata$kgeTest,mtry = ceiling(0.2609*5),num.trees = 983,replace = F,sample.fraction = 0.703,min.node.size = 0)

nhp=3000000
mtry<-runif(nhp,min=0,max=1)
numtrees<-round(runif(nhp,min=10,max=2000))
replace<-round(runif(nhp,min=0,max=1))
minnodesize<-runif(nhp,min=0,max=1)
samplefraction<-runif(nhp,min=0.1,max=1)

candidatehp<-data.frame(mtry=mtry,numtrees=numtrees,replace=replace,minnodesize=minnodesize,samplefraction=samplefraction)
rm(mtry,numtrees,replace,minnodesize,samplefraction)
gc()
predictions<-predict(mod,candidatehp)
candidatehp[which.max(predictions$predictions),]
max(predictions$predictions)
mod$r.squared

#using nse method and ranger (finding new optimal default hyperparameters)
mod<-ranger(x=alldata[,2:6],y=alldata$nseTest,mtry = ceiling(0.2609*5),num.trees = 983,replace = F,sample.fraction = 0.703,min.node.size = 0)

nhp=3000000
mtry<-runif(nhp,min=0,max=1)
numtrees<-round(runif(nhp,min=10,max=2000))
replace<-round(runif(nhp,min=0,max=1))
minnodesize<-runif(nhp,min=0,max=1)
samplefraction<-runif(nhp,min=0.1,max=1)

candidatehp<-data.frame(mtry=mtry,numtrees=numtrees,replace=replace,minnodesize=minnodesize,samplefraction=samplefraction)
rm(mtry,numtrees,replace,minnodesize,samplefraction)
gc()
predictions<-predict(mod,candidatehp)
candidatehp[which.max(predictions$predictions),]
max(predictions$predictions)
mod$r.squared

#XGboost setup
xgboostdata <- list.files("C:/Users/joeja/OneDrive/REX_2020_2021/MetaResults2/XGB",full.names=T) 

alldata <- NULL
for(x in 1:length(xgboostdata)) {
  curDat <- read.csv(xgboostdata[x])
  curDat$nseTest[curDat$nseTest<0]<-0
  curDat$kgeTest[curDat$kgeTest<0]<-0
  curDat$kgeTest<-curDat$kgeTest- curDat$kgeTest[2]
  curDat$nseTest<-curDat$nseTest- curDat$nseTest[2]
  alldata <- rbind(alldata,curDat)
}

#using XGboost + NSE method
mod<-ranger(x=alldata[,2:9],y=alldata$nseTest,mtry = ceiling(0.2609*5),num.trees = 983,replace = F,sample.fraction = 0.703,min.node.size = 0) 
nhp=3000000
numrounds<-round(runif(nhp,min=1,max=5000))
eta<-2^(runif(nhp,min=-10,max=0))
subsample<-runif(nhp,min=0.1,max=1)
max_depth<-round(runif(nhp,min=1,max=15))
min_child_weight<-2^runif(nhp,min=0,max=7)
colsample_bytree<-runif(nhp,min=0,max=1)
lambda<-2^runif(nhp,min=-10,max=10)
alpha<-2^runif(nhp,min=-10,max=10)

candidatehp<-data.frame(numrounds=numrounds,eta=eta,subsample=subsample,max_depth=max_depth,min_child_weight=min_child_weight,colsample_bytree=colsample_bytree,lambda=lambda,alpha=alpha)
rm(alpha,lambda,colsample_bytree,min_child_weight,max_depth,subsample,eta,numrounds)
gc()
predictions<-predict(mod,candidatehp)
candidatehp[which.max(predictions$predictions),]
max(predictions$predictions)
mod$r.squared

#using XGboost + KGE method
mod<-ranger(x=alldata[,2:9],y=alldata$kgeTest,mtry = ceiling(0.2609*5),num.trees = 983,replace = F,sample.fraction = 0.703,min.node.size = 0)
nhp=3000000
numrounds<-round(runif(nhp,min=1,max=5000))
eta<-2^(runif(nhp,min=-10,max=0))
subsample<-runif(nhp,min=0.1,max=1)
max_depth<-round(runif(nhp,min=1,max=15))
min_child_weight<-2^runif(nhp,min=0,max=7)
colsample_bytree<-runif(nhp,min=0,max=1)
lambda<-2^runif(nhp,min=-10,max=10)
alpha<-2^runif(nhp,min=-10,max=10)

candidatehp<-data.frame(numrounds=numrounds,eta=eta,subsample=subsample,max_depth=max_depth,min_child_weight=min_child_weight,colsample_bytree=colsample_bytree,lambda=lambda,alpha=alpha)
rm(alpha,lambda,colsample_bytree,min_child_weight,max_depth,subsample,eta,numrounds)
gc()
predictions<-predict(mod,candidatehp)
candidatehp[which.max(predictions$predictions),]
max(predictions$predictions)
mod$r.squared



######################################################################################################################################################
####################################################################3   try same thing but for cross validation results  ##########################3####
#########################################################################################################################################################
rm(list=ls())

library(ranger)
library(xgboost)


#RF setup
rfdata <- list.files("C:/Users/joeja/OneDrive/REX_2020_2021/MetaResults2/RF",full.names=T) 

alldata <- NULL
for(x in 1:length(rfdata)) {
  curDat <- read.csv(rfdata[x])
  curDat$kgeTrain[curDat$kgeTrain<0]<-0
  curDat$nseTrain[curDat$nseTrain<0]<-0
  curDat$kgeTrain<-curDat$kgeTrain-curDat$kgeTrain[2]
  curDat$nseTrain<-curDat$nseTrain-curDat$nseTrain[2]
  alldata <- rbind(alldata,curDat)
}
rm(curDat,rfdata,x)

#RF with KGE
mod<-ranger(x=alldata[,2:6],y=alldata$kgeTrain,mtry = ceiling(0.2609*5),num.trees = 983,replace = F,sample.fraction = 0.703,min.node.size = 0)

nhp=3000000
mtry<-runif(nhp,min=0,max=1)
numtrees<-round(runif(nhp,min=10,max=2000))
replace<-round(runif(nhp,min=0,max=1))
minnodesize<-runif(nhp,min=0,max=1)
samplefraction<-runif(nhp,min=0.1,max=1)

candidatehp<-data.frame(mtry=mtry,numtrees=numtrees,replace=replace,minnodesize=minnodesize,samplefraction=samplefraction)
rm(mtry,numtrees,replace,minnodesize,samplefraction)
gc()
predictions<-predict(mod,candidatehp)
candidatehp[which.max(predictions$predictions),]
max(predictions$predictions)
mod$r.squared

#using nse method and ranger (finding new optimal default hyperparameters)
mod<-ranger(x=alldata[,2:6],y=alldata$nseTrain,mtry = ceiling(0.2609*5),num.trees = 983,replace = F,sample.fraction = 0.703,min.node.size = 0)

nhp=3000000
mtry<-runif(nhp,min=0,max=1)
numtrees<-round(runif(nhp,min=10,max=2000))
replace<-round(runif(nhp,min=0,max=1))
minnodesize<-runif(nhp,min=0,max=1)
samplefraction<-runif(nhp,min=0.1,max=1)

candidatehp<-data.frame(mtry=mtry,numtrees=numtrees,replace=replace,minnodesize=minnodesize,samplefraction=samplefraction)
rm(mtry,numtrees,replace,minnodesize,samplefraction)
gc()
predictions<-predict(mod,candidatehp)
candidatehp[which.max(predictions$predictions),]
max(predictions$predictions)
mod$r.squared

#XGboost setup
xgboostdata <- list.files("C:/Users/joeja/OneDrive/REX_2020_2021/MetaResults2/XGB",full.names=T) 

alldata <- NULL
for(x in 1:length(xgboostdata)) {
  curDat <- read.csv(xgboostdata[x])
  curDat$nseTrain[curDat$nseTrain<0]<-0
  curDat$kgeTrain[curDat$kgeTrain<0]<-0
  curDat$kgeTrain<-curDat$kgeTrain- curDat$kgeTrain[2]
  curDat$nseTrain<-curDat$nseTrain- curDat$nseTrain[2]
  alldata <- rbind(alldata,curDat)
}

#using XGboost + NSE method
mod<-ranger(x=alldata[,2:9],y=alldata$nseTrain,mtry = ceiling(0.2609*5),num.trees = 983,replace = F,sample.fraction = 0.703,min.node.size = 0) 
nhp=3000000
numrounds<-round(runif(nhp,min=1,max=5000))
eta<-2^(runif(nhp,min=-10,max=0))
subsample<-runif(nhp,min=0.1,max=1)
max_depth<-round(runif(nhp,min=1,max=15))
min_child_weight<-2^runif(nhp,min=0,max=7)
colsample_bytree<-runif(nhp,min=0,max=1)
lambda<-2^runif(nhp,min=-10,max=10)
alpha<-2^runif(nhp,min=-10,max=10)

candidatehp<-data.frame(numrounds=numrounds,eta=eta,subsample=subsample,max_depth=max_depth,min_child_weight=min_child_weight,colsample_bytree=colsample_bytree,lambda=lambda,alpha=alpha)
rm(alpha,lambda,colsample_bytree,min_child_weight,max_depth,subsample,eta,numrounds)
gc()
predictions<-predict(mod,candidatehp)
candidatehp[which.max(predictions$predictions),]
max(predictions$predictions)
mod$r.squared

#using XGboost + KGE method
mod<-ranger(x=alldata[,2:9],y=alldata$kgeTrain,mtry = ceiling(0.2609*5),num.trees = 983,replace = F,sample.fraction = 0.703,min.node.size = 0)
nhp=3000000
numrounds<-round(runif(nhp,min=1,max=5000))
eta<-2^(runif(nhp,min=-10,max=0))
subsample<-runif(nhp,min=0.1,max=1)
max_depth<-round(runif(nhp,min=1,max=15))
min_child_weight<-2^runif(nhp,min=0,max=7)
colsample_bytree<-runif(nhp,min=0,max=1)
lambda<-2^runif(nhp,min=-10,max=10)
alpha<-2^runif(nhp,min=-10,max=10)

candidatehp<-data.frame(numrounds=numrounds,eta=eta,subsample=subsample,max_depth=max_depth,min_child_weight=min_child_weight,colsample_bytree=colsample_bytree,lambda=lambda,alpha=alpha)
rm(alpha,lambda,colsample_bytree,min_child_weight,max_depth,subsample,eta,numrounds)
gc()
predictions<-predict(mod,candidatehp)
candidatehp[which.max(predictions$predictions),]
max(predictions$predictions)
mod$r.squared





