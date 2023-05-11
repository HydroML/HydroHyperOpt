
rm(list = ls())
library(ggpubr)
library(xgboost)
library(randomForest)
library(ranger)
library(tidyverse)
library(dplyr)
library(reshape2)

SDtoSWE <- read.csv("C:/Users/joeja/Desktop/REX/WeDO_2022/Kuentz-Europe_Q95.csv")

#Tree vs Runtime
dat <- SDtoSWE[1:500,]
X <- dat[,2:ncol(dat)]
y <- dat[,1]
numtreevec <- seq(from=50,to=5000,by=100)

#ranger
numrep<-10
timesRanger <- matrix(0,nrow=length(numtreevec),ncol=numrep)

for (q in 1:numrep){ #repeating 10 times
  for (i in 1:length(numtreevec)){ #repeating for every increment of trees/create data
    t<- Sys.time()  
    mod<-ranger(x=X,y=y,num.trees = numtreevec[i]) #setting training program
    timesRanger[i,q] <- Sys.time()-t
  }
  print(paste("ranger",q))
}

write.csv(timesRanger,file="C:/Users/joeja/Desktop/REX/WeDO_2022/Time/Ranger_Tree-Runtime_Exp.csv",row.names = FALSE) #save each experiment  

# for (k in 1:10){
#   assign(paste0('rangerTree',k),read.csv(paste0("C:/Users/joeja/Desktop/REX/WeDO_2022/Time/Ranger_Tree-Runtime_Exp",k,".csv")))
# } 
# 
# timesRangerTotal <- data.frame(rangerTree1,rangerTree2,rangerTree3,rangerTree4,rangerTree5,rangerTree6,rangerTree7,rangerTree8,rangerTree9,rangerTree10)
# 
# timesRangerTotal$Ranger <- rowMeans(timesRangerTotal)
# timesRangerTotal<- select(timesRangerTotal, Ranger)
                    
#xgboost
timesXGBoost <-  matrix(0,nrow=length(numtreevec),ncol=numrep)

for (q in 1:numrep){  #repeating 10 times
  for (i in 1:length(numtreevec)){ #repeating for every increment of trees
    t <- Sys.time()
    modx<-xgb.train(data=xgb.DMatrix(as.matrix(X),label=y),nrounds = numtreevec[i]) #setting training program
    timesXGBoost[i,q] <- Sys.time()-t
  }
  print(paste("xgboost",q))
}

write.csv(timesXGBoost,file="C:/Users/joeja/Desktop/REX/WeDO_2022/Time/XGBoost_Tree-Runtime_Exp.csv",row.names = FALSE) #save each experiment 

# for (k in 1:10){
#   assign(paste0('xgBoostTree',k),read.csv(paste0("C:/Users/joeja/Desktop/REX/WeDO_2022/Time/XGBoost_Tree-Runtime_Exp",k,".csv")))
# } 
# 
# timesXGBoostTotal <- data.frame(xgBoostTree1,xgBoostTree2,xgBoostTree3,xgBoostTree4,xgBoostTree5,
#                                 xgBoostTree6,xgBoostTree7,xgBoostTree8,xgBoostTree9,xgBoostTree10)
# 
# timesXGBoostTotal$XGBoost <- rowMeans(timesXGBoostTotal)
# timesXGBoostTotal<- select(timesXGBoostTotal, XGBoost)


#randomforest
timesRandomForest <-  matrix(0,nrow=length(numtreevec),ncol=numrep)

for (q in 1:numrep){   #repeating 10 times
  for (i in 1:length(numtreevec)){ #repeating for every increment of trees
    t<- Sys.time()
    modrf<-randomForest(x=X,y=y,ntree = numtreevec[i]) #setting training program
    timesRandomForest[i,q] <- Sys.time()-t
  }
  
  print(paste("randomForest",q))
}

write.csv(timesRandomForest,file="C:/Users/joeja/Desktop/REX/WeDO_2022/Time/RF_Tree-Runtime_Exp.csv",row.names = FALSE) #save each experiment 



timesRanger<-rowMeans(timesRanger)
timesXGBoost<-rowMeans(timesXGBoost)
timesRandomForest<-rowMeans(timesRandomForest)


jpeg(paste0("C:/Users/joeja/Desktop/REX/WeDO_2022/Time/trees.jpeg"),width = 8, height = 6, units = 'in',res = 600)
plot(numtreevec,timesRanger,ylim = c(0,max(timesXGBoost,timesRandomForest,timesRanger)),type="l",xlab = "Number of Trees",ylab="Time (seconds)",col=1,lwd=3,cex=1.5)
lines(numtreevec,timesXGBoost,col=2,lwd=3)
lines(numtreevec,timesRandomForest,col=3,lwd=3)
legend("topleft",legend=c("Ranger","XGBoost","randomForest"),col=c(1,2,3),lty=1,lwd=3)
dev.off()

# for (k in 1:10){
#   assign(paste0('rfTree',k),read.csv(paste0("C:/Users/joeja/Desktop/REX/WeDO_2022/Time/RF_Tree-Runtime_Exp",k,".csv")))
# } 
# 
# timesRandomForestTotal <- data.frame(rfTree1,rfTree2,rfTree3,rfTree4,rfTree5,
#                                      rfTree6,rfTree7,rfTree8,rfTree9,rfTree10)
# 
# timesRandomForestTotal$RandomForest <- rowMeans(timesRandomForestTotal)
# timesRandomForestTotal<- select(timesRandomForestTotal, RandomForest)

#making pretty label names for plot
# Ranger <- timesRangerTotal
# RandomForest <- timesRandomForestTotal
# XGBoost <- timesXGBoostTotal

# times_table <- data.frame(numtreevec,timesRandomForestTotal,timesRangerTotal,timesXGBoostTotal) #combined into one table
# 
# timesMelted <- reshape2::melt(times_table, id.var='numtreevec')
# 
# times_plotTrees <- ggplot(data=timesMelted,
#                      aes(x=numtreevec, y=value, colour=variable)) +
#               geom_line(size=0.8) +
#               labs(x="Number of Trees",
#                    y= "Runtime (seconds)",
#                    colour = "Package")+
#               ggtitle("(a)")

# Tree vs Samples
numsamplevec <- seq(from=50,to=1366,by=10)


#ranger
timesRanger2 <- matrix(0,nrow=length(numsamplevec),ncol=numrep)


for (q in 1:numrep){  #repeating 10 times
  for (i in 1:length(numsamplevec)){ #repeating for every increment of observations
    dataSample <- SDtoSWE[1:numsamplevec[i],] 
    X <- dataSample[,2:ncol(dataSample)]
    y<- dataSample[,1]
    t<- Sys.time()
    mod<-ranger(x=X,y=y,num.trees = 500) #running alg.
    timesRanger2[i,q] <- timesRanger2[i]+ Sys.time()-t
  }
  print(paste("ranger",q))
}
write.csv(timesRanger2,file="C:/Users/joeja/Desktop/REX/WeDO_2022/Time/Ranger_Sample-Runtime_Exp.csv",row.names = FALSE) #save each experiment

# for (k in 1:10){
#   assign(paste0('rangerSample',k),read.csv(paste0("C:/Users/joeja/Desktop/REX/WeDO_2022/Time/Ranger_Sample-Runtime_Exp",k,".csv")))
# } 
# 
# timesRanger2Total <- data.frame(rangerSample1,rangerSample2,rangerSample3,rangerSample4,rangerSample5,
#                                 rangerSample6,rangerSample7,rangerSample8,rangerSample9,rangerSample10)
# 
# timesRanger2Total$Ranger <- rowMeans(timesRanger2Total)
# timesRanger2Total<- select(timesRanger2Total, Ranger)

#xgboost
timesXGBoost2 <- matrix(0,nrow=length(numsamplevec),ncol=numrep)


for (q in 1:numrep){  #repeating 10 times
  for (i in 1:length(numsamplevec)){  #repeating for every increment of oberservations
    dataSample <- SDtoSWE[1:numsamplevec[i],]
    X <- dataSample[,2:ncol(dataSample)]
    y<- dataSample[,1]
    t <- Sys.time()
    mod<-xgb.train(data=xgb.DMatrix(as.matrix(X),label=y),nrounds = 500) #running alg.
    timesXGBoost2[i,q] <- Sys.time()-t
  }
  print(paste("xgboost",q))
}
write.csv(timesXGBoost2,file="C:/Users/joeja/Desktop/REX/WeDO_2022/Time/XGBoost_Sample-Runtime_Exp.csv",row.names = FALSE) #save each experiment

# for (k in 1:10){
#   assign(paste0('xgboostSample',k),read.csv(paste0("C:/Users/joeja/Desktop/REX/WeDO_2022/Time/XGBoost_Sample-Runtime_Exp",k,".csv")))
# } 
# 
# timesXGBoost2Total <- data.frame(xgboostSample1,xgboostSample2,xgboostSample3,xgboostSample4,xgboostSample5,
#                                  xgboostSample6,xgboostSample7,xgboostSample8,xgboostSample9,xgboostSample10)
# 
# timesXGBoost2Total$XGBoost <- rowMeans(timesXGBoost2Total)
# timesXGBoost2Total<- select(timesXGBoost2Total, XGBoost)



#randomforest
timesRandomForest2 <- matrix(0,nrow=length(numsamplevec),ncol=numrep)


for (q in 1:numrep){   #repeating 10 times
  for (i in 1:length(numsamplevec)){   #repeating for every increment of oberservations
    dataSample <- SDtoSWE[1:numsamplevec[i],]
    X <- dataSample[,2:ncol(dataSample)]
    y<- dataSample[,1]
    t<- Sys.time()
    mod<-randomForest(x=X,y=y,ntree = 500)  #running alg.
    timesRandomForest2[i,q] <- timesRandomForest2[i]+ Sys.time()-t
  }
  print(paste("randomForest",q))
}
write.csv(timesRandomForest2,file="C:/Users/joeja/Desktop/REX/WeDO_2022/Time/RF_Sample-Runtime_Exp.csv",row.names = FALSE) #save each experiment

# for (k in 1:10){
#   assign(paste0('rfSample',k),read.csv(paste0("C:/Users/joeja/Desktop/REX/WeDO_2022/Time/RF_Sample-Runtime_Exp",k,".csv")))
# } 
# 
# timesRandomForest2Total <- data.frame(rfSample1,rfSample2,rfSample3,rfSample4,rfSample5,
#                                 rfSample6,rfSample7,rfSample8,rfSample9,rfSample10)
# 
# timesRandomForest2Total$RandomForest <- rowMeans(timesRandomForest2Total)
# timesRandomForest2Total<- select(timesRandomForest2Total, RandomForest)


#pretty labels for plots
# Ranger <- timesRanger2Total
# RandomForest <- timesRandomForest2Total
# XGBoost <- timesXGBoost2Total
# 
# times_table2 <- data.frame(numsamplevec,timesRandomForest2Total,timesRanger2Total, timesXGBoost2Total)
# 
# timesMelted2 <- reshape2::melt(times_table2, id.var='numsamplevec')
# 
# times_plotSamples <- ggplot(data=timesMelted2,
#                       aes(x=numsamplevec, y=value, colour=variable)) +
#                geom_line(size=0.8) +
#                labs(x="Number of Samples",
#                     y= "Runtime (seconds)",
#                     colour = "Package")+
#                ggtitle("(b)")
# 
# 
# finalPlot <- ggarrange(times_plotTrees, times_plotSamples,
#              ncol = 2, nrow = 1)


timesRanger2<-rowMeans(timesRanger2)
timesXGBoost2<-rowMeans(timesXGBoost2)
timesRandomForest2<-rowMeans(timesRandomForest2)


jpeg(paste0("C:/Users/joeja/Desktop/REX/WeDO_2022/Time/samples.jpeg"),width = 8, height = 6, units = 'in',res = 600)
plot(numsamplevec,timesRanger2,ylim = c(0,max(timesXGBoost2,timesRandomForest2,timesRanger2)),type="l",xlab = "Number of Samples",ylab="Time (seconds)",col=1,lwd=3,cex=1.5)
lines(numsamplevec,timesXGBoost2,col=2,lwd=3)
lines(numsamplevec,timesRandomForest2,col=3,lwd=3)
legend("topleft",legend=c("Ranger","XGBoost","randomForest"),col=c(1,2,3),lty=1,lwd=3)
dev.off()




