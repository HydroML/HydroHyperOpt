rm(list=ls())

library(ranger)
library(xgboost)
library(evtree)
library(e1071)
library(rpart)


#RF setup
rfdata <- list.files("C:/Users/joeja/OneDrive/REX_2020_2021/MetaResults2/RF",full.names=T) 

alldata <- NULL
for(x in 1:length(rfdata)) {
  curDat <- read.csv(rfdata[x])
  curDat$kgeTest[curDat$kgeTest<0]<-0
  curDat$nseTest[curDat$nseTest<0]<-0
  curDat$kgeTest<-curDat$kgeTest-curDat$kgeTest[2]
  curDat$kgeTest<-curDat$kgeTest>0
  curDat$nseTest<-curDat$nseTest-curDat$nseTest[2]
  curDat$nseTest<-curDat$nseTest>0
  curDat<-curDat[-c(1,2),]
  alldata <- rbind(alldata,curDat)
}
rm(curDat,rfdata,x)


class_counts <- table(alldata$kgeTest)
weights <- 1 / class_counts[alldata$kgeTest]
alldata$kgeTest<-as.factor(alldata$kgeTest)
gotree<-evtree(kgeTest~mtry+numtrees+replace+samplefraction+minnodesize,
               data = alldata, weights = round(as.numeric(2.5e4*weights/sum(weights))),control = evtree.control(maxdepth = 4,
                                                       niterations = 1000, 
                                                       ntrees = 500))
gotree[[1]]
plot(gotree)



#restrict range
rankthresh<-5
rankthresh2<-50
alldata<-alldata[alldata$mtry>0.2 & alldata$minnodesize<0.4 & alldata$samplefraction>0.66 & alldata$replace==0,]
datax<-alldata$samplefraction
datay<-alldata$minnodesize
plot(datax[alldata$kgeTest>=rankthresh],datay[alldata$kgeTest>=rankthresh],pch=20,cex=0.5)
points(datax[alldata$kgeTest<rankthresh],datay[alldata$kgeTest<rankthresh],pch=20,cex=1.5,col="lightblue3")
points(datax[alldata$kgeTest>rankthresh2],datay[alldata$kgeTest>rankthresh2],pch=20,cex=1.5,col="red")


class_counts <- table(alldata$kgeTest)
weights <- 1 / class_counts[alldata$kgeTest]
alldata$nseTest<-as.factor(alldata$nseTest)
gotree<-evtree(nseTest~mtry+numtrees+replace+samplefraction+minnodesize,
               data = alldata,weights = round(as.numeric(2.5e4*weights/sum(weights))),control = evtree.control(maxdepth = 4,
                                                       niterations = 1000, 
                                                       ntrees = 100))
gotree[[1]]
plot(gotree)



#RF with KGE



mod<-ranger(x=alldata[,2:6],y=alldata$kgeTest,mtry = ceiling(0.2609*5),num.trees = 983,replace = F,sample.fraction = 0.703,min.node.size = 1)

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
  curDat$kgeTest<-curDat$kgeTest>0
  #curDat$kgeTest<-rank(-curDat$kgeTest)
  curDat$nseTest<-curDat$nseTest- curDat$nseTest[2]
  curDat$nseTest<-curDat$nseTest>0
  #curDat$nseTest<-rank(-curDat$nseTest)
  curDat<-curDat[-c(1,2),]
  alldata <- rbind(alldata,curDat)
}

alldata$kgeTest<-as.factor(alldata$kgeTest)
gotree<-evtree(kgeTest~numrounds+eta+subsample+max_depth+min_child_weight+colsample_bytree+lambda+alpha,
               data = alldata,control = evtree.control(maxdepth = 5,
                                                       niterations = 1000, 
                                                       ntrees = 500),weights = c(1,5))
gotree[[1]]
plot(gotree)


alldata$kgeTest<-as.numeric(alldata$kgeTest)
gotree<-svm(kgeTest~numrounds+log2(eta)+subsample+max_depth+log2(min_child_weight)+colsample_bytree+log2(lambda)+log2(alpha),
               data = alldata,type = 'C-classification', 
            kernel = 'linear',class.weights = c("1" = 1, "2" = 5))

y_pred <- predict(gotree, newdata = alldata)
table(alldata$kgeTest, y_pred)
plot(gotree,alldata, numrounds~log2(eta))


#manually optimize xgboost for kge metric

alldata<-alldata[alldata$numrounds>2000 & alldata$numrounds<4400 & log2(alldata$numrounds)>9 & log2(alldata$numrounds)>9 & log2(alldata$eta)> -8 & alldata$colsample_bytree>0.3 & log2(alldata$min_child_weight)<4 & log2(alldata$alpha)<1 & !(alldata$subsample<0.35 & log2(alldata$min_child_weight)>4.4) & !(alldata$subsample<0.22 & log2(alldata$lambda)>7.5) & (log2(alldata$eta)+4*alldata$numrounds/2500> -4) & !(log2(alldata$eta)< -6.5 & alldata$subsample<0.65) & (1+ 0.5*log2(alldata$eta) < (alldata$subsample)) & !(log2(alldata$eta)< -6.5 & log2(alldata$min_child_weight)>2.5) & (12.2+0.81*log2(alldata$eta)>log2(alldata$lambda)),]

rankthresh<-0.5
rankthresh2<-0.5

datax<-log2(alldata$eta)
datay<-log2(alldata$alpha)
plot(datax[alldata$kgeTest>=rankthresh],datay[alldata$kgeTest>=rankthresh],pch=20,cex=0.5)
points(datax[alldata$kgeTest<rankthresh],datay[alldata$kgeTest<rankthresh],pch=20,cex=1.2,col="lightblue3")
points(datax[alldata$kgeTest>rankthresh2],datay[alldata$kgeTest>rankthresh2],pch=20,cex=1.2,col="red")
summary(alldata$nseTest)
sum(alldata$nseTest==T)/sum(alldata$nseTest==F)


#manually optimize XGBOOST FOR NSE metric


alldata<-alldata[((alldata$numrounds-2900)^2+140000*(log2(alldata$alpha)+3.5)^2<1700000) & (0.8-(1/2000)*alldata$numrounds < alldata$colsample_bytree) & log2(alldata$eta)< -2.5 & (alldata$colsample_bytree>0.2 | log2(alldata$lambda)>4) & !(alldata$max_depth==1 & alldata$colsample_bytree<0.73) & (-8+0.6*log2(alldata$numrounds)> log2(alldata$eta)) & (4.5-1.3*log2(alldata$numrounds)< log2(alldata$eta)) & alldata$numrounds>1000 & (-70 + 17.7*log2(alldata$min_child_weight) < log2(alldata$lambda)) & !(alldata$max_depth==1 & log2(alldata$min_child_weight)>1.5) & alldata$colsample_bytree>0.05 & (0.9- 1*(alldata$subsample) < (alldata$colsample_bytree)) & (2.5+ 6*(alldata$subsample) > log2(alldata$min_child_weight)) & log2(alldata$alpha)<0.5 & log2(alldata$min_child_weight)<5 & (0.8+ 0.35*log2(alldata$eta) < (alldata$subsample)) & log2(alldata$eta)< -1 & (20+(1.8)*log2(alldata$eta) > log2(alldata$lambda)) & (10+(5.5)*log2(alldata$eta) < log2(alldata$lambda)),]

rankthresh<-0.5
rankthresh2<-0.5

datax<-(alldata$numrounds)
datay<-log2(alldata$lambda)
plot(datax[alldata$nseTest>=rankthresh],datay[alldata$nseTest>=rankthresh],pch=20,cex=0.5)
points(datax[alldata$nseTest<rankthresh],datay[alldata$nseTest<rankthresh],pch=20,cex=1.5,col="lightblue3")
points(datax[alldata$nseTest>rankthresh2],datay[alldata$nseTest>rankthresh2],pch=20,cex=1.5,col="red")
summary(alldata$nseTest)
sum(alldata$nseTest==T)/sum(alldata$nseTest==F)

summary(alldata[alldata$numrounds>2000 & alldata$numrounds<3000 & log2(alldata$eta)> -10 & log2(alldata$lambda)<0 & log2(alldata$alpha)< -2.5 & log2(alldata$alpha)> -5 & log2(alldata$eta)< -5.5 & alldata$subsample>0.35 & alldata$nseTest==T,1:9])



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





