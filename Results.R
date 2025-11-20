#Install Packages
#install.packages("ranger")
#install.packages("mfe")
#install.packages("ECoL")

rm(list = ls())

#Load libraries
library(ranger)
library(mfe)
library(ECoL)
library(dummies)
library(stringr)

calcKGE<- function(real,pred){
  if(sd(real)==0){
    real=real+rnorm(length(real),mean = 0,sd=.0001)
  }
  if(sd(pred)==0){
    pred=pred+rnorm(length(pred),mean = 0,sd=.0001)
  }
  r<- cor(pred,real)
  alpha<- mean(pred)/mean(real)
  beta <- (sd(pred)/mean(pred))/ (sd(real)/mean(real))
  KGE<-(1-sqrt((r-1)^2+(alpha-1)^2+(beta-1)^2))
  return(KGE)
}

calcNSE<- function(real,pred){
  if(sd(real)==0){
    real=real+rnorm(length(real),mean = 0,sd=.0001)
  }
  if(sd(pred)==0){
    pred=pred+rnorm(length(pred),mean = 0,sd=.0001)
  }
  NSE<- 1 - (sum((real-pred)^2)/sum((real-mean(real))^2))
  return(NSE)
}


#Get files names (used to load data later)
files <- list.files(path = 'C:/Users/joeja/OneDrive/REX_2020_2021/Clean_Data',pattern="*.csv", full.names=FALSE, recursive=FALSE)
numr<-100
numfold<-10

#already done
filesDone<-list.files("C:/Users/joeja/Desktop/REX/REX2020_2021/BaseResults2/RF",recursive = F)
files<-setdiff(files,filesDone)


for(file in 1:length(files)){
  #ranger parameters
  set.seed(file)
  numt<-sample(1:2000,numr,replace = T)
  r<-sample(c(T,F),numr,replace = T)
  sampfrac<-runif(numr,min = 0.1,max=1)
  try<-runif(numr,min = 0,max=1)
  ns<-runif(numr,min = 0,max=1)
  
  # Generate results_rf data frame to save findings in
  results_rf = data.frame(n=rep(0,(numr+2)),mtry=rep(0,(numr+2)),numtrees=rep(0,(numr+2)),replace=rep(T,(numr+2)),samplefraction=rep(0,(numr+2)),minnodesize=rep(0,(numr+2)),
                          optimizationAlgo=rep(" ",(numr+2)),kgeTrain=rep(0,(numr+2)), nseTrain=rep(0,(numr+2)),kgeTest=rep(0,(numr+2)),
                          nseTest=rep(0,(numr+2)))
  
  #Load the data
  data <- read.csv(paste0('C:/Users/joeja/OneDrive/REX_2020_2021/Clean_Data/',files[file]))  #trying with one before implementing for loop
  
  #split train and testing data
  set.seed(file)
  TestTrainSplit<-sample(c(T,T,T,T,F), size = nrow(data), replace=T)
  test_data<-data[!TestTrainSplit,]
  data<-data[TestTrainSplit,]
  
  
  #run before you have transformed categorical features to numeric
  gen=metafeatures(x=data[,2:ncol(data)],y=data[,1],c("general"))
  gen<-gen[-c(2,length(gen))]

  #transformed categorical features to numeric
  data <- dummy.data.frame(data, sep = ".")

  #run after you have transformed categorical features to numeric (this function does not work with strings in data)
  X=data[,2:ncol(data)]
  com<-ECoL::complexity(x=X,y=as.numeric(data[,1]))
  com<-com[-c(9)]

  meta<-c(gen,com)

  #join metas data to dataframe
  met <- as.data.frame(t(meta))
  met <- met[rep(seq_len(nrow(met)), each = (numr+2)), ]
  results_rf<-cbind(results_rf,met)
  
  #Get X and y variable names to use when training the model
  data <- read.csv(paste0('C:/Users/joeja/OneDrive/REX_2020_2021/Clean_Data/',files[file]))  #ranger does not need you to transform variables
  test_data<-data[!TestTrainSplit,]
  data<-data[TestTrainSplit,]
  
  #Get X and y data entries
  y <- as.numeric(data[,1])
  X <- data[,2:ncol(data)]
  ytest<-as.numeric(test_data[,1])
  
  #Set up values
  pred<-rep(0,length(y))
  set.seed(file)
  folder<-sample(rep(1:numfold,length.out = length(y)))
  
  results_rf$optimizationAlgo[1] <- "default"
  results_rf$optimizationAlgo[2] <- "optimal default"
  results_rf$optimizationAlgo[3:(numr+2)] <- "random search"
  
  #default random forest
  for(k in 1:numfold){
    mod<-ranger(x=X[folder!=k,],y=y[folder!=k],oob.error = F,verbose = F)
    pred[folder==k]<-as.numeric(predict(mod,X[folder==k,])[[1]])
  }
  
  mod<-ranger(x=X,y=y,oob.error = F,verbose = F)
  predtest<-as.numeric(predict(mod,test_data[,2:ncol(test_data)])[[1]])
  
  results_rf$n[1] <- 1
  results_rf$mtry[1] <- mod$mtry/ncol(X)
  results_rf$numtrees[1] <- mod$num.trees
  results_rf$replace[1] <- mod$replace
  results_rf$samplefraction[1]<-1
  results_rf$minnodesize[1]<- log(mod$min.node.size,length(y))
  results_rf$kgeTest[1] <- calcKGE(ytest,predtest)
  results_rf$nseTest[1] <- calcNSE(ytest,predtest)
  results_rf$kgeTrain[1] <- calcKGE(y,pred)
  results_rf$nseTrain[1] <- calcNSE(y,pred)
  
  #optimal default RF
  folder<-sample(rep(1:numfold,length.out = length(y)))
  for(k in 1:numfold){
    mod<-mod<-ranger(x=X[folder!=k,],y=y[folder!=k],num.trees = 983, replace = F,sample.fraction = .703,mtry = round(ncol(X)*.257),oob.error = F,min.node.size = 1)
    pred[folder==k]<-as.numeric(predict(mod,X[folder==k,])[[1]])
  }
  
  mod<-ranger(x=X,y=y,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(ncol(X)*.257),oob.error = F,min.node.size = 1)
  predtest<-as.numeric(predict(mod,test_data[,2:ncol(test_data)])[[1]])
  
  results_rf$n[2] <- 1
  results_rf$mtry[2] <- mod$mtry/ncol(X)
  results_rf$numtrees[2] <- mod$num.trees
  results_rf$replace[2] <- mod$replace
  results_rf$samplefraction[2]<-.703
  results_rf$minnodesize[2]<- log(mod$min.node.size,length(y))
  results_rf$kgeTest[2] <- calcKGE(ytest,predtest)
  results_rf$nseTest[2] <- calcNSE(ytest,predtest)
  results_rf$kgeTrain[2] <- calcKGE(y,pred)
  results_rf$nseTrain[2] <- calcNSE(y,pred)
  
  #random search random forest
  results_rf$n[3:(numr+2)]<-1:numr
  results_rf$mtry[3:(numr+2)]<-try
  results_rf$numtrees[3:(numr+2)]<-numt
  results_rf$replace[3:(numr+2)]<-r
  results_rf$samplefraction[3:(numr+2)]<-sampfrac
  results_rf$minnodesize[3:(numr+2)]<-ns
  
  for(i in 3:(numr+2)){
    folder<-sample(rep(1:numfold,length.out = length(y)))
    for(k in 1:numfold){
      
      mod<-ranger(x=X[folder!=k,],y=y[folder!=k],num.trees = results_rf$numtrees[i],replace = results_rf$replace[i],
                  sample.fraction = results_rf$samplefraction[i],mtry = ceiling(ncol(X)*results_rf$mtry[i]),
                  min.node.size = length(y)^(results_rf$minnodesize[i]),oob.error = F,verbose = F)
      pred[folder==k]<-as.numeric(predict(mod,X[folder==k,])[[1]])
    }
    results_rf$kgeTrain[i] <- calcKGE(y,pred)
    results_rf$nseTrain[i] <- calcNSE(y,pred)
    
    
    mod<-ranger(x=X,y=y,num.trees = results_rf$numtrees[i],replace = results_rf$replace[i],
              sample.fraction = results_rf$samplefraction[i],mtry = ceiling(ncol(X)*results_rf$mtry[i]),
              min.node.size = length(y)^(results_rf$minnodesize[i]),oob.error = F,verbose = F)
    predtest<-as.numeric(predict(mod,test_data[,2:ncol(test_data)])[[1]])
    results_rf$kgeTest[i] <- calcKGE(ytest,predtest)
    results_rf$nseTest[i] <- calcNSE(ytest,predtest)
  }
  
  print(file)
  write.csv(results_rf,paste0("C:/Users/joeja/Desktop/REX/REX2020_2021/BaseResults/RF/" ,files[file]),row.names = F)
}














#Install Packages
#install.packages("xgboost")
#install.packages("dummies")
#install.packages("mfe")
#install.packages("ECoL")

rm(list = ls())

calcKGE<- function(real,pred){
  if(sd(real)==0){
    real=real+rnorm(length(real),mean = 0,sd=.0001)
  }
  if(sd(pred)==0){
    pred=pred+rnorm(length(pred),mean = 0,sd=.0001)
  }
  r<- cor(pred,real)
  alpha<- mean(pred)/mean(real)
  beta <- (sd(pred)/mean(pred))/ (sd(real)/mean(real))
  KGE<-(1-sqrt((r-1)^2+(alpha-1)^2+(beta-1)^2))
  return(KGE)
}

calcNSE<- function(real,pred){
  if(sd(real)==0){
    real=real+rnorm(length(real),mean = 0,sd=.0001)
  }
  if(sd(pred)==0){
    pred=pred+rnorm(length(pred),mean = 0,sd=.0001)
  }
  NSE<- 1 - (sum((real-pred)^2)/sum((real-mean(real))^2))
  return(NSE)
}

#Load libraries
library(xgboost)
library(dummies)
library(mfe)
library(ECoL)
library(stringr)

files <- list.files(path = 'C:/Users/joeja/OneDrive/REX_2020_2021/Clean_Data',pattern="*.csv", full.names=F, recursive=FALSE)
numr<-100
numfold<-10


for(file in 111:length(files)){
  nr<-sample(10:5000,numr,replace = T)
  eta<-2^runif(numr,min=-10,max=0)
  ss<-runif(numr,min=.1,max=1)
  md<-sample(1:25,numr,replace = T)
  mcw<-2^runif(numr,min = 0,max=7)
  cb<-runif(numr,min=0,max=1)
  lam<- 2^runif(numr,min = -10,max = 10)
  alp<- 2^runif(numr,min = -10,max = 10)
  
  # Generate results_rf data frame to save findings in
  results_xgb = data.frame(n=rep(0,(numr+2)),numrounds=rep(0,(numr+2)),eta=rep(0,(numr+2)),subsample=rep(0,(numr+2)),max_depth=rep(0,(numr+2)),
                           min_child_weight=rep(T,(numr+2)),colsample_bytree=rep(0,(numr+2)),lambda=rep(0,(numr+2)),alpha=rep(0,(numr+2)),
                           optimizationAlgo=rep(" ",(numr+2)),kgeTrain=rep(0,(numr+2)), nseTrain=rep(0,(numr+2)),kgeTest=rep(0,(numr+2)),
                           nseTest=rep(0,(numr+2)))
  
  #Load the data
  data <- read.csv(paste0('C:/Users/joeja/OneDrive/REX_2020_2021/Clean_Data/',files[file]))
  
  #split train and testing data
  set.seed(file)
  TestTrainSplit<-sample(c(T,T,T,T,F), size = nrow(data), replace=T)
  test_data<-data[!TestTrainSplit,]
  data<-data[TestTrainSplit,]
  
  #run before you have transformed categorical features to numeric
  gen=metafeatures(x=data[,2:ncol(data)],y=data[,1],c("general"))
  gen<-gen[-c(2,length(gen))]
  
  #transformed categorical features to numeric
  data <- read.csv(paste0('C:/Users/joeja/OneDrive/REX_2020_2021/Clean_Data/',files[file]))
  data <- dummy.data.frame(data, sep = ".")
  test_data<-data[!TestTrainSplit,]
  data<-data[TestTrainSplit,]
  
  #run after you have transformed categorical features to numeric (this function does not work with strings in data)
  X=data[,2:ncol(data)]
  com<-ECoL::complexity(x=X,y=data[,1])
  com<-com[-c(9)]

  meta<-c(gen,com)

  #join metas data to dataframe
  met <- as.data.frame(t(meta))
  met <- met[rep(seq_len(nrow(met)), each = (numr+2)), ]
  results_xgb<-cbind(results_xgb,met)

  test_data <- dummy.data.frame(test_data, sep = ".")
  
  #Get X and y data entries
  y <- as.numeric(data[,1])
  X <- data[,2:ncol(data)]
  ytest <- as.numeric(test_data[,1])
  Xtest <- test_data[,2:ncol(test_data)]
  
  #Set up values
  pred<-rep(0,length(y))
  set.seed(file)
  folder<-sample(rep(1:numfold,length.out = length(y)))
  
  results_xgb$optimizationAlgo[1] <- "default"
  results_xgb$optimizationAlgo[2]<- "optimal default"
  results_xgb$optimizationAlgo[3:(numr+2)] <- "random search"
  
  #default xgboost
  for(k in 1:numfold){
    mod<-xgb.train(data=xgb.DMatrix(as.matrix(X[folder!=k,]),label=y[folder!=k]),verbose = 0,nrounds = 500)
    pred[folder==k]<-predict(mod,xgb.DMatrix(as.matrix(X[folder==k,])))
  }
  
  mod<-xgb.train(data=xgb.DMatrix(as.matrix(X),label=y),verbose = 0,nrounds = 500)
  predtest<-predict(mod,xgb.DMatrix(as.matrix(Xtest)))
  
  results_xgb$n[1] <- 1
  results_xgb$numrounds[1]<- 500
  results_xgb$eta[1] <- .3
  results_xgb$subsample[1] <- 1
  results_xgb$max_depth[1] <- 6
  results_xgb$min_child_weight[1] <- 1
  results_xgb$colsample_bytree[1] <- 1
  results_xgb$lambda[1]<-1
  results_xgb$alpha[1]<-1
  results_xgb$kgeTest[1] <- calcKGE(ytest,predtest)
  results_xgb$nseTest[1] <- calcNSE(ytest,predtest)
  results_xgb$kgeTrain[1] <- calcKGE(y,pred)
  results_xgb$nseTrain[1] <- calcNSE(y,pred)
  
  #optimal default xgboost
  for(i in 1:numfold){
    mod<-xgb.train(list(eta=.018,subsample=.839,max_depth=13,min_child_weight=2.06,colsample_bytree=.752,lambda=0.982, alpha=1.113),
                   xgb.DMatrix(as.matrix(X[folder!=i,]),label=y[folder!=i]),nrounds=4168,verbose = 0)
    pred[folder==i]<-predict(mod,xgb.DMatrix(as.matrix(X[folder==i,])))
  }
  
  mod<-xgb.train(list(eta=.018,subsample=.839,max_depth=13,min_child_weight=2.06,colsample_bytree=.752,lambda=0.982, alpha=1.113),
                 xgb.DMatrix(as.matrix(X),label=y),nrounds=4168,verbose = 0)
  predtest<-predict(mod,xgb.DMatrix(as.matrix(Xtest)))

  results_xgb$n[2] <- 1
  results_xgb$numrounds[2]<- 4168
  results_xgb$eta[2] <- .018
  results_xgb$subsample[2] <- .839
  results_xgb$max_depth[2] <- 13
  results_xgb$min_child_weight[2] <- 2.06
  results_xgb$colsample_bytree[2] <- .752
  results_xgb$lambda[2]<-0.982
  results_xgb$alpha[2]<-1.113
  results_xgb$kgeTest[2] <- calcKGE(ytest,predtest)
  results_xgb$nseTest[2] <- calcNSE(ytest,predtest)
  results_xgb$kgeTrain[2] <- calcKGE(y,pred)
  results_xgb$nseTrain[2] <- calcNSE(y,pred)
  
  
  #random search xgboost
  results_xgb$n[3:(numr+2)]<-1:numr
  results_xgb$numrounds[3:(numr+2)]<-nr
  results_xgb$eta[3:(numr+2)] <- eta
  results_xgb$subsample[3:(numr+2)] <- ss
  results_xgb$max_depth[3:(numr+2)] <- md
  results_xgb$min_child_weight[3:(numr+2)] <- mcw
  results_xgb$colsample_bytree[3:(numr+2)] <- cb
  results_xgb$lambda[3:(numr+2)]<-lam
  results_xgb$alpha[3:(numr+2)]<-alp
  
  for(i in 3:(numr+2)){
    param<-list(max_depth = results_xgb$max_depth[i], eta = results_xgb$eta[i],min_child_weight=results_xgb$min_child_weight[i]
                ,subsample=results_xgb$subsample[i], colsample_bytree=results_xgb$colsample_bytree[i],lambda=results_xgb$lambda[i],
                alpha=results_xgb$alpha[i])
    for(j in 1:numfold){
      mod<-xgb.train(param,xgb.DMatrix(as.matrix(X[folder!= j,]),label=y[folder!= j]),nrounds=results_xgb$numrounds[i],verbose = 0)
      pred[folder==j]<-predict(mod,xgb.DMatrix(as.matrix(X[folder==j,])))
    }
    results_xgb$kgeTrain[i] <- calcKGE(y,pred)
    results_xgb$nseTrain[i] <- calcNSE(y,pred)
    
    mod<-xgb.train(param,xgb.DMatrix(as.matrix(X),label=y),nrounds=results_xgb$numrounds[i],verbose = 0)
    predtest<-predict(mod,xgb.DMatrix(as.matrix(Xtest)))
    results_xgb$kgeTest[i] <- calcKGE(ytest,predtest)
    results_xgb$nseTest[i] <- calcNSE(ytest,predtest)
    if(is.na(results_xgb$kgeTrain[i])){
      print(pred)
      print(y)
    }
  }
  
  write.csv(results_xgb,paste0("C:/Users/joeja/Desktop/REX/REX2020_2021/BaseResults2/XGB/" ,files[file]),row.names = F)
  print(file)
}




#################################################################################################################################
#################################################     testing meta models RF   #####################################################
#################################################################################################################################
library(ranger)
library(mfe)
library(ECoL)
library(dummies)
library(stringr)
rm(list = ls())


#get meta learning results for RF KGE and NSE
files <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/BaseResults2/RF', full.names=TRUE, recursive=FALSE)
filenames <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/BaseResults2/RF', full.names=F, recursive=FALSE)



MetaData<-data.frame()
for(otherfiles in 1:length(filenames)){
  data <- read.csv(files[otherfiles])
  data$normalizedKGE<-data$kgeTest-data$kgeTest[2]
  data$normalizedNSE<-data$nseTest- data$nseTest[2]
  data$dataset<-otherfiles
  MetaData<-rbind(MetaData,data)
}

MetaData$normalizedKGE<-rank(MetaData$normalizedKGE)
MetaData$normalizedNSE<-rank(MetaData$normalizedNSE)


#get metaX
MetaX<-MetaData[,c(2:6,12:(ncol(MetaData)-3))]
Metay1<-MetaData$normalizedKGE
Metay2<-MetaData$normalizedNSE


#make meta predictions
MetaXTrain<-MetaX[MetaData$dataset<= length(filenames)/2,]
Metay1Train<-Metay1[MetaData$dataset<= length(filenames)/2]
MetaXTest<-MetaX[MetaData$dataset> length(filenames)/2,]
Metay1Test<-Metay1[MetaData$dataset> length(filenames)/2]
Metay2Train<-Metay2[MetaData$dataset<= length(filenames)/2]
Metay2Test<-Metay2[MetaData$dataset> length(filenames)/2]

mod1<-ranger(x=MetaXTrain,y=Metay1Train,oob.error = F)
pred<-predict(mod1,MetaXTest)
curR2_1<-1-sum((pred$predictions- Metay1Test)^2)/sum((Metay1Test - mean(Metay1Test))^2)

mod1<-ranger(x=MetaXTest,y=Metay1Test,oob.error = F)
pred<-predict(mod1,MetaXTrain)
curR2_2<-1-sum((pred$predictions- Metay1Train)^2)/sum((Metay1Train - mean(Metay1Train))^2)
mean(c(curR2_1,curR2_2))



keepFeaturesY1<-c("minnodesize","samplefraction","mtry","replace")
keepGoing<-TRUE
bestR2<-0
while(keepGoing){
  keepGoing<-FALSE
  for(curFeature in setdiff(colnames(MetaX),keepFeaturesY1)){
    set.seed(123)
    mod1<-ranger(x=MetaXTrain[,c(keepFeaturesY1,curFeature)],y=Metay1Train,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(c(keepFeaturesY1,curFeature))*.257),min.node.size = 1)
    pred<-predict(mod1,MetaXTest)
    curR2_1<-1-sum((pred$predictions- Metay1Test)^2)/sum((Metay1Test - mean(Metay1Test))^2)
    
    set.seed(123)
    mod1<-ranger(x=MetaXTest[,c(keepFeaturesY1,curFeature)],y=Metay1Test,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(c(keepFeaturesY1,curFeature))*.257),min.node.size = 1)
    pred<-predict(mod1,MetaXTrain)
    curR2_2<-1-sum((pred$predictions- Metay1Train)^2)/sum((Metay1Train - mean(Metay1Train))^2)
    
    curR2<-mean(c(curR2_1,curR2_2))
    if(curR2>bestR2){
      bestR2<-curR2
      FeatureToAdd<-curFeature
      keepGoing<-TRUE
    }
  }
  if(keepGoing){
    keepFeaturesY1<-c(keepFeaturesY1,FeatureToAdd)
    print(FeatureToAdd)
  } 
}
bestR2
keepFeaturesY1



mod1<-ranger(x=MetaXTrain,y=Metay2Train,oob.error = F)
pred<-predict(mod1,MetaXTest)
curR2_1<-1-sum((pred$predictions- Metay2Test)^2)/sum((Metay2Test - mean(Metay2Test))^2)

mod1<-ranger(x=MetaXTest,y=Metay2Test,oob.error = F)
pred<-predict(mod1,MetaXTrain)
curR2_2<-1-sum((pred$predictions- Metay2Train)^2)/sum((Metay2Train - mean(Metay2Train))^2)
mean(c(curR2_1,curR2_2))


keepFeaturesY2<-c("minnodesize","samplefraction","mtry","replace")
keepGoing<-TRUE
bestR2<-0
while(keepGoing){
  keepGoing<-FALSE
  for(curFeature in setdiff(colnames(MetaX),keepFeaturesY2)){
    mod1<-ranger(x=MetaXTrain[,c(keepFeaturesY2,curFeature)],y=Metay2Train,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(c(keepFeaturesY2,curFeature))*.257),min.node.size = 1)
    pred<-predict(mod1,MetaXTest)
    curR2_1<-1-sum((pred$predictions- Metay2Test)^2)/sum((Metay2Test - mean(Metay2Test))^2)
    
    mod1<-ranger(x=MetaXTest[,c(keepFeaturesY2,curFeature)],y=Metay2Test,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(c(keepFeaturesY2,curFeature))*.257),min.node.size = 1)
    pred<-predict(mod1,MetaXTrain)
    curR2_2<-1-sum((pred$predictions- Metay2Train)^2)/sum((Metay2Train - mean(Metay2Train))^2)
    
    curR2<-mean(c(curR2_1,curR2_2))
    if(curR2>bestR2){
      bestR2<-curR2
      FeatureToAdd<-curFeature
      keepGoing<-TRUE
    }
  }
  if(keepGoing){
    keepFeaturesY2<-c(keepFeaturesY2,FeatureToAdd)
    print(FeatureToAdd)
  } 
}
bestR2
keepFeaturesY2





############################################################################################################################3
#############################################     meta rf results    ########################################################
#############################################################################################################################


#get Meta learning results
#Load libraries
library(ranger)
library(mfe)
library(ECoL)
library(dummies)
library(stringr)

calcKGE<- function(real,pred){
  if(sd(real)==0){
    real=real+rnorm(length(real),mean = 0,sd=.0001)
  }
  if(sd(pred)==0){
    pred=pred+rnorm(length(pred),mean = 0,sd=.0001)
  }
  r<- cor(pred,real)
  alpha<- mean(pred)/mean(real)
  beta <- (sd(pred)/mean(pred))/ (sd(real)/mean(real))
  KGE<-(1-sqrt((r-1)^2+(alpha-1)^2+(beta-1)^2))
  return(KGE)
}

calcNSE<- function(real,pred){
  if(sd(real)==0){
    real=real+rnorm(length(real),mean = 0,sd=.0001)
  }
  if(sd(pred)==0){
    pred=pred+rnorm(length(pred),mean = 0,sd=.0001)
  }
  NSE<- 1 - (sum((real-pred)^2)/sum((real-mean(real))^2))
  return(NSE)
}

library(stringr)
Nguesses<-1

#get meta learning results for RF KGE and NSE
files <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/BaseResults2/RF', full.names=TRUE, recursive=FALSE)
filenames <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/BaseResults2/RF', full.names=F, recursive=FALSE)
numfold<-10
numPred<-2000000

for(file in 1:length(filenames)){
  
  MetaData<-data.frame()
  for(otherfiles in 1:length(filenames)){
    data <- read.csv(files[otherfiles])
    data$normalizedKGE<-data$kgeTest-data$kgeTest[2]
    data$normalizedNSE<-data$nseTest- data$nseTest[2]
    data$dataset<-otherfiles
    MetaData<-rbind(MetaData,data)
  }
  
  MetaData$normalizedKGE<-rank(MetaData$normalizedKGE)
  MetaData$normalizedNSE<-rank(MetaData$normalizedNSE)
  
  
  data<-read.csv(paste0("C:/Users/joeja/OneDrive/REX_2020_2021/Clean_Data/",filenames[file]))
  
  #ranger parameters
  numt<-sample(1:2000,numPred,replace = T)
  r<-sample(c(T,F),numPred,replace = T)
  sampfrac<-runif(numPred,min = .1,max=1)
  try<-runif(numPred,min = 0,max=1)
  ns<-runif(numPred,min = 0,max=1)
  MetaNewX<-data.frame(mtry=try,numtrees=numt,replace=r,samplefraction=sampfrac,minnodesize=ns)
  
  #split train and testing data
  set.seed(file)
  TestTrainSplit<-sample(c(T,T,T,T,F), size = nrow(data), replace=T)
  test_data<-data[!TestTrainSplit,]
  data<-data[TestTrainSplit,]
  
  #run before you have transformed categorical features to numeric
  gen=metafeatures(x=data[,2:ncol(data)],y=data[,1],c("general"))
  gen<-gen[-c(2,length(gen))]
  
  #transformed categorical features to numeric
  data <- dummy.data.frame(data, sep = ".")
  
  #run after you have transformed categorical features to numeric (this function does not work with strings in data)
  X=data[,2:ncol(data)]
  com<-ECoL::complexity(x=X,y=data[,1])
  com<-com[-c(9)]
  
  meta<-c(gen,com)
  
  #join metas data to dataframe
  met <- as.data.frame(t(meta))
  met <- met[rep(seq_len(nrow(met)), each = numPred), ]
  MetaNewX<-cbind(MetaNewX,met)
  
  #Get X and y variable names to use when training the model
  data<-read.csv(paste0("C:/Users/joeja/OneDrive/REX_2020_2021/Clean_Data/",filenames[file]))  #ranger does not need you to transform variables
  
  set.seed(file)
  TestTrainSplit<-sample(c(T,T,T,T,F), size = nrow(data), replace=T)
  test_data<-data[!TestTrainSplit,]
  data<-data[TestTrainSplit,]
  
  y_name <- names(data)[1]
  X_name <- tail(names(data),-1)
  
  #get metaX
  MetaX<-MetaData[,c(2:6,12:(ncol(MetaData)-3))]
  Metay1<-MetaData$normalizedKGE
  Metay2<-MetaData$normalizedNSE
  
  #make meta predictions
  mod1<-ranger(x=MetaX[,keepFeaturesY1],y=Metay1,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(keepFeaturesY1)*.257),min.node.size = 1)
  mod2<-ranger(x=MetaX[,keepFeaturesY2],y=Metay2,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(keepFeaturesY2)*.257),min.node.size = 1)
  pred1<-as.numeric(predict(mod1,MetaNewX)[[1]])
  pred2<-as.numeric(predict(mod2,MetaNewX)[[1]])
  Best1<-MetaNewX[order(-pred1),]
  Best2<-MetaNewX[order(-pred2),]
  Best1<-Best1[1:Nguesses,]
  Best2<-Best2[1:Nguesses,]
  
  
  #Get X and y data entries
  y <- as.numeric(data[,1])
  X <- data[,X_name]
  ytest <- as.numeric(test_data[,1])
  Xtest <- test_data[,X_name]
  
  #Set up values
  pred<-rep(0,length(y))
  set.seed(file)
  folder<-sample(rep(1:numfold,length.out = length(y)))
  
  #import previous results csv so that we can add to it
  results_rf <- read.csv(files[file])
  results_rfMetaRFkge<-results_rf[1:Nguesses,] 
  results_rfMetaRFnse<-results_rf[1:Nguesses,] 
  results_rfMetaRFkge$n<-1:Nguesses
  results_rfMetaRFnse$n<-1:Nguesses
  
  results_rfMetaRFkge$mtry<-Best1$mtry
  results_rfMetaRFkge$numtrees<-Best1$numtrees
  results_rfMetaRFkge$replace<-Best1$replace
  results_rfMetaRFkge$samplefraction<-Best1$samplefraction
  results_rfMetaRFkge$minnodesize<-Best1$minnodesize
  results_rfMetaRFkge$optimizationAlgo<-"Meta RF KGE"
  
  results_rfMetaRFnse$mtry<-Best2$mtry
  results_rfMetaRFnse$numtrees<-Best2$numtrees
  results_rfMetaRFnse$replace<-Best2$replace
  results_rfMetaRFnse$samplefraction<-Best2$samplefraction
  results_rfMetaRFnse$minnodesize<-Best2$minnodesize
  results_rfMetaRFnse$optimizationAlgo<-"Meta RF NSE"

  
  for(i in 1:nrow(results_rfMetaRFkge)){
    for(k in 1:numfold){
      mod<-ranger(x=X[folder!=k,],y=y[folder!=k],num.trees = results_rfMetaRFkge$numtrees[i],replace = results_rfMetaRFkge$replace[i],
                  sample.fraction = results_rfMetaRFkge$samplefraction[i],mtry = ceiling(ncol(X)*results_rfMetaRFkge$mtry[i]),min.node.size = length(y)^(results_rfMetaRFkge$minnodesize[i]),oob.error = F)
      pred[folder==k]<-as.numeric(predict(mod,X[folder==k,])[[1]])
    }
    
    results_rfMetaRFkge$kgeTrain[i] <- calcKGE(y,pred)
    results_rfMetaRFkge$nseTrain[i] <- calcNSE(y,pred)
    
    #test set
    mod<-ranger(x=X,y=y,num.trees = results_rfMetaRFkge$numtrees[i],replace = results_rfMetaRFkge$replace[i],
                sample.fraction = results_rfMetaRFkge$samplefraction[i],mtry = ceiling(ncol(X)*results_rfMetaRFkge$mtry[i]),min.node.size = length(y)^(results_rfMetaRFkge$minnodesize[i]),oob.error = F)
    predtest<-as.numeric(predict(mod,Xtest)[[1]])
    results_rfMetaRFkge$kgeTest[i] <- calcKGE(ytest,predtest)
    results_rfMetaRFkge$nseTest[i] <- calcNSE(ytest,predtest)
    
    for(k in 1:numfold){
      mod<-ranger(x=X[folder!=k,],y=y[folder!=k],num.trees = results_rfMetaRFnse$numtrees[i],replace = results_rfMetaRFnse$replace[i],
                  sample.fraction = results_rfMetaRFnse$samplefraction[i],mtry = ceiling(ncol(X)*results_rfMetaRFnse$mtry[i]),min.node.size = length(y)^(results_rfMetaRFnse$minnodesize[i]),oob.error = F)
      pred[folder==k]<-as.numeric(predict(mod,X[folder==k,])[[1]])
    }
    
    results_rfMetaRFnse$kgeTrain[i] <- calcKGE(y,pred)
    results_rfMetaRFnse$nseTrain[i] <- calcNSE(y,pred)
    
    #test set
    mod<-ranger(x=X,y=y,num.trees = results_rfMetaRFnse$numtrees[i],replace = results_rfMetaRFnse$replace[i],
                sample.fraction = results_rfMetaRFnse$samplefraction[i],mtry = ceiling(ncol(X)*results_rfMetaRFnse$mtry[i]),min.node.size = length(y)^(results_rfMetaRFnse$minnodesize[i]),oob.error = F)
    predtest<-as.numeric(predict(mod,Xtest)[[1]])
    results_rfMetaRFnse$kgeTest[i] <- calcKGE(ytest,predtest)
    results_rfMetaRFnse$nseTest[i] <- calcNSE(ytest,predtest)
  }
  
  results_rf<-rbind(results_rf,results_rfMetaRFkge,results_rfMetaRFnse)
  
  write.csv(results_rf,paste0("C:/Users/joeja/Desktop/REX/REX2020_2021/MetaResults2/RF/" ,filenames[file]),row.names = F)
  print(file)
}

#################################################################################################################################
#################################################     testing meta models xgb   #####################################################
#################################################################################################################################
library(ranger)
library(mfe)
library(ECoL)
library(dummies)
library(stringr)
library(xgboost)
rm(list = ls())

#get meta learning results for RF KGE and NSE
files <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/BaseResults2/XGB', full.names=TRUE, recursive=FALSE)
filenames <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/BaseResults2/XGB', full.names=F, recursive=FALSE)



MetaData<-data.frame()
for(otherfiles in 1:length(filenames)){
  data <- read.csv(files[otherfiles])
  data$normalizedKGE<-data$kgeTest-data$kgeTest[2]
  data$normalizedNSE<-data$nseTest- data$nseTest[2]
  data$dataset<-otherfiles
  MetaData<-rbind(MetaData,data)
}

MetaData$normalizedKGE<-rank(MetaData$normalizedKGE)
MetaData$normalizedNSE<-rank(MetaData$normalizedNSE)


#get metaX
MetaX<-MetaData[,c(2:9,15:(ncol(MetaData)-3))]
Metay1<-MetaData$normalizedKGE
Metay2<-MetaData$normalizedNSE


#make meta predictions
MetaXTrain<-MetaX[MetaData$dataset<= length(filenames)/2,]
Metay1Train<-Metay1[MetaData$dataset<= length(filenames)/2]
MetaXTest<-MetaX[MetaData$dataset> length(filenames)/2,]
Metay1Test<-Metay1[MetaData$dataset> length(filenames)/2]
Metay2Train<-Metay2[MetaData$dataset<= length(filenames)/2]
Metay2Test<-Metay2[MetaData$dataset> length(filenames)/2]

mod1<-ranger(x=MetaXTrain,y=Metay1Train,oob.error = F)
pred<-predict(mod1,MetaXTest)
curR2_1<-1-sum((pred$predictions- Metay1Test)^2)/sum((Metay1Test - mean(Metay1Test))^2)

mod1<-ranger(x=MetaXTest,y=Metay1Test,oob.error = F)
pred<-predict(mod1,MetaXTrain)
curR2_2<-1-sum((pred$predictions- Metay1Train)^2)/sum((Metay1Train - mean(Metay1Train))^2)
mean(c(curR2_1,curR2_2))



keepFeaturesY1<-c("alpha","eta","min_child_weight","subsample","colsample_bytree","numrounds","lambda")
keepGoing<-TRUE
bestR2<-0
while(keepGoing){
  keepGoing<-FALSE
  for(curFeature in setdiff(colnames(MetaX),keepFeaturesY1)){
    set.seed(123)
    mod1<-ranger(x=MetaXTrain[,c(keepFeaturesY1,curFeature)],y=Metay1Train,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(c(keepFeaturesY1,curFeature))*.257),min.node.size = 1)
    pred<-predict(mod1,MetaXTest)
    curR2_1<-1-sum((pred$predictions- Metay1Test)^2)/sum((Metay1Test - mean(Metay1Test))^2)
    
    set.seed(123)
    mod1<-ranger(x=MetaXTest[,c(keepFeaturesY1,curFeature)],y=Metay1Test,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(c(keepFeaturesY1,curFeature))*.257),min.node.size = 1)
    pred<-predict(mod1,MetaXTrain)
    curR2_2<-1-sum((pred$predictions- Metay1Train)^2)/sum((Metay1Train - mean(Metay1Train))^2)
    
    curR2<-mean(c(curR2_1,curR2_2))
    if(curR2>bestR2){
      bestR2<-curR2
      FeatureToAdd<-curFeature
      keepGoing<-TRUE
    }
  }
  if(keepGoing){
    keepFeaturesY1<-c(keepFeaturesY1,FeatureToAdd)
    print(FeatureToAdd)
  } 
}
bestR2
keepFeaturesY1



mod1<-ranger(x=MetaXTrain,y=Metay2Train,oob.error = F)
pred<-predict(mod1,MetaXTest)
curR2_1<-1-sum((pred$predictions- Metay2Test)^2)/sum((Metay2Test - mean(Metay2Test))^2)

mod1<-ranger(x=MetaXTest,y=Metay2Test,oob.error = F)
pred<-predict(mod1,MetaXTrain)
curR2_2<-1-sum((pred$predictions- Metay2Train)^2)/sum((Metay2Train - mean(Metay2Train))^2)
mean(c(curR2_1,curR2_2))


keepFeaturesY2<-c("alpha","eta","min_child_weight","subsample","colsample_bytree","numrounds","lambda")
keepGoing<-TRUE
bestR2<-0
while(keepGoing){
  keepGoing<-FALSE
  for(curFeature in setdiff(colnames(MetaX),keepFeaturesY2)){
    mod1<-ranger(x=MetaXTrain[,c(keepFeaturesY2,curFeature)],y=Metay2Train,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(c(keepFeaturesY2,curFeature))*.257),min.node.size = 1)
    pred<-predict(mod1,MetaXTest)
    curR2_1<-1-sum((pred$predictions- Metay2Test)^2)/sum((Metay2Test - mean(Metay2Test))^2)
    
    mod1<-ranger(x=MetaXTest[,c(keepFeaturesY2,curFeature)],y=Metay2Test,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(c(keepFeaturesY2,curFeature))*.257),min.node.size = 1)
    pred<-predict(mod1,MetaXTrain)
    curR2_2<-1-sum((pred$predictions- Metay2Train)^2)/sum((Metay2Train - mean(Metay2Train))^2)
    
    curR2<-mean(c(curR2_1,curR2_2))
    if(curR2>bestR2){
      bestR2<-curR2
      FeatureToAdd<-curFeature
      keepGoing<-TRUE
    }
  }
  if(keepGoing){
    keepFeaturesY2<-c(keepFeaturesY2,FeatureToAdd)
    print(FeatureToAdd)
  } 
}
bestR2
keepFeaturesY2







#get meta learning results for XGB KGE and NSE
calcKGE<- function(real,pred){
  if(sd(real)==0){
    real=real+rnorm(length(real),mean = 0,sd=.0001)
  }
  if(sd(pred)==0){
    pred=pred+rnorm(length(pred),mean = 0,sd=.0001)
  }
  r<- cor(pred,real)
  alpha<- mean(pred)/mean(real)
  beta <- (sd(pred)/mean(pred))/ (sd(real)/mean(real))
  KGE<-(1-sqrt((r-1)^2+(alpha-1)^2+(beta-1)^2))
  return(KGE)
}

calcNSE<- function(real,pred){
  if(sd(real)==0){
    real=real+rnorm(length(real),mean = 0,sd=.0001)
  }
  if(sd(pred)==0){
    pred=pred+rnorm(length(pred),mean = 0,sd=.0001)
  }
  NSE<- 1 - (sum((real-pred)^2)/sum((real-mean(real))^2))
  return(NSE)
}

library(xgboost)
files <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/BaseResults2/XGB', full.names=TRUE, recursive=FALSE)
filenames <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/BaseResults2/XGB', full.names=F, recursive=FALSE)
Nguesses<-1
numr<-Nguesses
numfold<-10
numPred<-2000000

for(file in 1:length(filenames)){
  MetaData<-data.frame()
  for(otherfiles in 1:length(filenames)){
    data <- read.csv(files[otherfiles])
    data$normalizedKGE<-data$kgeTest-data$kgeTest[2]
    data$normalizedNSE<-data$nseTest- data$nseTest[2]
    data$dataset<-otherfiles
    MetaData<-rbind(MetaData,data)
  }
  
  MetaData$normalizedKGE<-rank(MetaData$normalizedKGE)
  MetaData$normalizedNSE<-rank(MetaData$normalizedNSE)
  
  data<-read.csv(paste0("C:/Users/joeja/OneDrive/REX_2020_2021/Clean_Data/",filenames[file]))
  
  
  #xgb parameters
  nr<-sample(1:5000,numPred,replace = T)
  eta<-2^runif(numPred,min=-10,max=0)
  ss<-runif(numPred,min=.1,max=1)
  md<-sample(1:25,numPred,replace = T)
  mcw<-2^runif(numPred,min = 0,max=7)
  cb<-runif(numPred,min=0,max=1)
  lam<- 2^runif(numPred,min = -10,max = 10)
  alp<- 2^runif(numPred,min = -10,max = 10)
  
  MetaNewX<- data.frame(numrounds=nr,eta=eta,subsample=ss,max_depth=md, min_child_weight=mcw,colsample_bytree=cb,lambda=lam,alpha=alp)
  
  #split train and testing data
  set.seed(file)
  TestTrainSplit<-sample(c(T,T,T,T,F), size = nrow(data), replace=T)
  test_data<-data[!TestTrainSplit,]
  data<-data[TestTrainSplit,]
  
  #run before you have transformed categorical features to numeric
  gen=metafeatures(x=data[,2:ncol(data)],y=data[,1],c("general"))
  gen<-gen[-c(2,length(gen))]
  
  #transformed categorical features to numeric
  data<-read.csv(paste0("C:/Users/joeja/OneDrive/REX_2020_2021/Clean_Data/",filenames[file]))
  data <- dummy.data.frame(data, sep = ".")
  set.seed(file)
  TestTrainSplit<-sample(c(T,T,T,T,F), size = nrow(data), replace=T)
  test_data<-data[!TestTrainSplit,]
  data<-data[TestTrainSplit,]
  
  #run after you have transformed categorical features to numeric (this function does not work with strings in data)
  X=data[,2:ncol(data)]
  com<-ECoL::complexity(x=X,y=data[,1])
  com<-com[-c(9)]
  
  meta<-c(gen,com)
  
  #join metas data to dataframe
  met <- as.data.frame(t(meta))
  met <- met[rep(seq_len(nrow(met)), each = numPred), ]
  MetaNewX<-cbind(MetaNewX,met)
  
  #Get X and y variable names to use when training the model
  y_name <- names(data)[1]
  X_name <- tail(names(data),-1)
  
  
  #get metaX
  MetaX<-MetaData[,c(2:9,15:(ncol(MetaData)-3))]
  Metay1<-MetaData$normalizedKGE
  Metay2<-MetaData$normalizedNSE
  
  #make meta predictions
  mod1<-ranger(x=MetaX[,keepFeaturesY1],y=Metay1,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(keepFeaturesY1)*.257),min.node.size = 1)
  mod2<-ranger(x=MetaX[,keepFeaturesY2],y=Metay2,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(keepFeaturesY2)*.257),min.node.size = 1)
  pred1<-as.numeric(predict(mod1,MetaNewX)[[1]])
  pred2<-as.numeric(predict(mod2,MetaNewX)[[1]])
  Best1<-MetaNewX[order(-pred1),]
  Best2<-MetaNewX[order(-pred2),]
  Best1<-Best1[1:Nguesses,]
  Best2<-Best2[1:Nguesses,]
  
  
  #Get X and y data entries
  y <- as.numeric(data[,1])
  X <- data[,X_name]
  ytest <- as.numeric(test_data[,1])
  Xtest <- test_data[,X_name]
  
  #Set up values
  pred<-rep(0,length(y))
  set.seed(file)
  folder<-sample(rep(1:numfold,length.out = length(y)))
  
  #import previous results csv so that we can add to it
  results_xgb <- read.csv(files[file])
  results_xgbMetaXGBkge<-results_xgb[1:Nguesses,] 
  results_xgbMetaXGBnse<-results_xgb[1:Nguesses,] 
  results_xgbMetaXGBkge$n<-1:Nguesses
  results_xgbMetaXGBnse$n<-1:Nguesses
  
  results_xgbMetaXGBkge$numrounds<-Best1$numrounds
  results_xgbMetaXGBkge$eta<-Best1$eta
  results_xgbMetaXGBkge$subsample<-Best1$subsample
  results_xgbMetaXGBkge$max_depth<-Best1$max_depth
  results_xgbMetaXGBkge$min_child_weight<-Best1$min_child_weight
  results_xgbMetaXGBkge$colsample_bytree<-Best1$colsample_bytree
  results_xgbMetaXGBkge$lambda<-Best1$lambda
  results_xgbMetaXGBkge$alpha<-Best1$alpha
  results_xgbMetaXGBkge$optimizationAlgo<-"Meta XGB KGE"
  
  results_xgbMetaXGBnse$numrounds<-Best2$numrounds
  results_xgbMetaXGBnse$eta<-Best2$eta
  results_xgbMetaXGBnse$subsample<-Best2$subsample
  results_xgbMetaXGBnse$max_depth<-Best2$max_depth
  results_xgbMetaXGBnse$min_child_weight<-Best2$min_child_weight
  results_xgbMetaXGBnse$colsample_bytree<-Best2$colsample_bytree
  results_xgbMetaXGBnse$lambda<-Best2$lambda
  results_xgbMetaXGBnse$alpha<-Best2$alpha
  results_xgbMetaXGBnse$optimizationAlgo<-"Meta XGB NSE"
  
  
  for(i in 1:nrow(results_xgbMetaXGBkge)){
    #training set
    param<-list(max_depth = results_xgbMetaXGBkge$max_depth[i], eta = results_xgbMetaXGBkge$eta[i],min_child_weight=results_xgbMetaXGBkge$min_child_weight[i],
                subsample=results_xgbMetaXGBkge$subsample[i], colsample_bytree=results_xgbMetaXGBkge$colsample_bytree[i],
                lambda=results_xgbMetaXGBkge$lambda[i],alpha=results_xgbMetaXGBkge$alpha[i])
    for(j in 1:numfold){
      mod<-xgb.train(param,xgb.DMatrix(as.matrix(X[folder!= j,]),label=y[folder!= j]),nrounds=results_xgbMetaXGBkge$numrounds[i],verbose = 0)
      pred[folder==j]<-predict(mod,xgb.DMatrix(as.matrix(X[folder==j,])))
    }
    
    results_xgbMetaXGBkge$kgeTrain[i] <- calcKGE(y,pred)
    results_xgbMetaXGBkge$nseTrain[i] <- calcNSE(y,pred)
    
    #test set
    mod<-xgb.train(param,xgb.DMatrix(as.matrix(X),label=y),nrounds=results_xgbMetaXGBkge$numrounds[i],verbose = 0)
    predtest<-predict(mod,xgb.DMatrix(as.matrix(Xtest)))
    results_xgbMetaXGBkge$kgeTest[i] <- calcKGE(ytest,predtest)
    results_xgbMetaXGBkge$nseTest[i] <- calcNSE(ytest,predtest)
    
    #training
    param<-list(max_depth = results_xgbMetaXGBnse$max_depth[i], eta = results_xgbMetaXGBnse$eta[i],min_child_weight=results_xgbMetaXGBnse$min_child_weight[i],
                subsample=results_xgbMetaXGBnse$subsample[i], colsample_bytree=results_xgbMetaXGBnse$colsample_bytree[i])
    for(j in 1:numfold){
      mod<-xgb.train(param,xgb.DMatrix(as.matrix(X[folder!= j,]),label=y[folder!= j]),nrounds=results_xgbMetaXGBnse$numrounds[i],verbose = 0)
      pred[folder==j]<-predict(mod,xgb.DMatrix(as.matrix(X[folder==j,])))
    }
    
    results_xgbMetaXGBnse$kgeTrain[i] <- calcKGE(y,pred)
    results_xgbMetaXGBnse$nseTrain[i] <- calcNSE(y,pred)
    
    mod<-xgb.train(param,xgb.DMatrix(as.matrix(X),label=y),nrounds=results_xgbMetaXGBnse$numrounds[i],verbose = 0)
    predtest<-predict(mod,xgb.DMatrix(as.matrix(Xtest)))
    results_xgbMetaXGBnse$kgeTest[i] <- calcKGE(ytest,predtest)
    results_xgbMetaXGBnse$nseTest[i] <- calcNSE(ytest,predtest)
  }
  
  results_xgb<-rbind(results_xgb,results_xgbMetaXGBkge,results_xgbMetaXGBnse)
  
  write.csv(results_xgb,paste0("C:/Users/joeja/Desktop/REX/REX2020_2021/MetaResults2/XGB/" ,filenames[file]),row.names = F)
  print(file)
}




###########################################################################################################################
######################################### getting results for heat map   #####################################################
############################################################################################################################
library(corrplot)
library(gplots)
library(Rfast)

filesXGB <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/MetaResults2/XGB', full.names=TRUE, recursive=FALSE)
filenamesXGB <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/MetaResults2/XGB', full.names=F, recursive=FALSE)

filesRF <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/MetaResults2/RF', full.names=TRUE, recursive=FALSE)
filenamesRF <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/MetaResults2/RF', full.names=F, recursive=FALSE)

GeneralFiles1<- filenamesXGB #substr(filenamesXGB,start = 4,stop = str_length(filenamesXGB))
GeneralFiles2<- filenamesRF #substr(filenamesRF,start = 3,stop = str_length(filenamesRF))

HeatMapResultsKGE<-matrix(0,ncol=length(GeneralFiles1),nrow=8)
HeatMapResultsNSE<-matrix(0,ncol=length(GeneralFiles1),nrow=8)

for(maxiter in c(100)){
  if(sum(GeneralFiles1!=GeneralFiles2)>0) print("warning!!!!!")
  
  for(i in 1:length(GeneralFiles1)){
    XGBfile<-read.csv(filesXGB[i])
    RFfile<-read.csv(filesRF[i])
    
    HeatMapResultsKGE[1,i]<-RFfile$kgeTest[RFfile$optimizationAlgo=="default"]
    HeatMapResultsKGE[2,i]<-RFfile$kgeTest[RFfile$optimizationAlgo=="optimal default"]
    HeatMapResultsKGE[3,i]<-RFfile$kgeTest[RFfile$kgeTrain==max(RFfile$kgeTrain[RFfile$n<= maxiter & RFfile$optimizationAlgo=="random search"])]
    HeatMapResultsKGE[4,i]<-RFfile$kgeTest[RFfile$optimizationAlgo=="Meta RF KGE"]
    HeatMapResultsKGE[5,i]<-XGBfile$kgeTest[XGBfile$optimizationAlgo=="default"]
    HeatMapResultsKGE[6,i]<-XGBfile$kgeTest[XGBfile$optimizationAlgo=="optimal default"]
    HeatMapResultsKGE[7,i]<-XGBfile$kgeTest[XGBfile$kgeTrain==max(XGBfile$kgeTrain[XGBfile$n<= maxiter & XGBfile$optimizationAlgo=="random search"])]
    HeatMapResultsKGE[8,i]<-XGBfile$kgeTest[XGBfile$optimizationAlgo=="Meta XGB KGE"]
    
    HeatMapResultsNSE[1,i]<-RFfile$nseTest[RFfile$optimizationAlgo=="default"]
    HeatMapResultsNSE[2,i]<-RFfile$nseTest[RFfile$optimizationAlgo=="optimal default"]
    HeatMapResultsNSE[3,i]<-RFfile$nseTest[RFfile$nseTrain==max(RFfile$nseTrain[RFfile$n<= maxiter & RFfile$optimizationAlgo=="random search" ])]
    HeatMapResultsNSE[4,i]<-RFfile$nseTest[RFfile$optimizationAlgo=="Meta RF NSE"]
    HeatMapResultsNSE[5,i]<-XGBfile$nseTest[XGBfile$optimizationAlgo=="default"]
    HeatMapResultsNSE[6,i]<-XGBfile$nseTest[XGBfile$optimizationAlgo=="optimal default"]
    HeatMapResultsNSE[7,i]<-XGBfile$nseTest[XGBfile$nseTrain==max(XGBfile$nseTrain[XGBfile$n<= maxiter & XGBfile$optimizationAlgo=="random search"])]
    HeatMapResultsNSE[8,i]<-XGBfile$nseTest[XGBfile$optimizationAlgo=="Meta XGB NSE"]
  }
  
  col <- colorRampPalette(c("#833030", "#EE9988", "#FFFFFF", "#77AADD", "#2e5173"))
  
  rownames(HeatMapResultsKGE)<-c("Default RF","Optimal Default RF","Random Search RF","Meta RF KGE","Default XGB","Optimal Default XGB","Random Search XGB","Meta XGB KGE")
  #colnames(HeatMapResultsKGE)<-substr(GeneralFiles1,start=1,stop=str_length(GeneralFiles1)-4)
  colranksKGE<- colRanks(-HeatMapResultsKGE)
  #colnames(colranksKGE)<-colnames(HeatMapResultsKGE)
  rownames(colranksKGE)<-rownames(HeatMapResultsKGE)
  
  
  jpeg(paste0("C:/Users/joeja/Desktop/REX/REX2020_2021/Results_Plots/KGE2new",maxiter,".jpg"),width = 14, height = 8, units = 'in',res = 600)
  heatmap.2(colranksKGE,Rowv = F,Colv = F,cexRow = 1.5,cexCol = 1.5,
            density.info="none", trace="none",key = F,sepcolor = "black",
            colsep = 1:ncol(HeatMapResultsKGE),rowsep = 1:nrow(HeatMapResultsKGE),srtRow = 30,
            margins = c(4,12),col = col(8)[8:1],
            lwid=c(0.1,4), lhei=c(0.1,4))
  dev.off()
  
  
  rownames(HeatMapResultsNSE)<-c("Default RF","Optimal Default RF","Random Search RF","Meta RF NSE","Default XGB","Optimal Default XGB","Random Search XGB","Meta XGB NSE")
  #colnames(HeatMapResultsNSE)<-substr(GeneralFiles1,start=1,stop=str_length(GeneralFiles1)-4)
  colranksNSE<- colRanks(-HeatMapResultsNSE)
  #colnames(colranksNSE)<-colnames(HeatMapResultsNSE)
  rownames(colranksNSE)<-rownames(HeatMapResultsNSE)
  
  
  jpeg(paste0("C:/Users/joeja/Desktop/REX/REX2020_2021/Results_Plots/NSE2new",maxiter,".jpg"),width = 14, height = 8, units = 'in',res = 600)
  heatmap.2(colranksNSE,Rowv = F,Colv = F,cexRow = 1.5,cexCol = 1.5,
            density.info="none", trace="none",key = F,sepcolor = "black",
            colsep = 1:ncol(HeatMapResultsNSE),rowsep = 1:nrow(HeatMapResultsNSE),srtRow = 30,
            margins = c(4,12),col = col(8)[8:1],
            lwid=c(0.1,4), lhei=c(0.1,4))
  dev.off()
}









library(corrplot)
library(gplots)
library(Rfast)

filesXGB <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/MetaResults2/XGB', full.names=TRUE, recursive=FALSE)
filenamesXGB <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/MetaResults2/XGB', full.names=F, recursive=FALSE)

filesRF <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/MetaResults2/RF', full.names=TRUE, recursive=FALSE)
filenamesRF <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/MetaResults2/RF', full.names=F, recursive=FALSE)

GeneralFiles1<- filenamesXGB #substr(filenamesXGB,start = 4,stop = str_length(filenamesXGB))
GeneralFiles2<- filenamesRF #substr(filenamesRF,start = 3,stop = str_length(filenamesRF))

HeatMapResultsKGE<-matrix(0,ncol=length(GeneralFiles1),nrow=6)
HeatMapResultsNSE<-matrix(0,ncol=length(GeneralFiles1),nrow=6)

for(maxiter in c(100)){
  if(sum(GeneralFiles1!=GeneralFiles2)>0) print("warning!!!!!")
  
  for(i in 1:length(GeneralFiles1)){
    XGBfile<-read.csv(filesXGB[i])
    RFfile<-read.csv(filesRF[i])
    
    HeatMapResultsKGE[1,i]<-RFfile$kgeTest[RFfile$optimizationAlgo=="default"]
    HeatMapResultsKGE[2,i]<-RFfile$kgeTest[RFfile$optimizationAlgo=="optimal default"]
    HeatMapResultsKGE[3,i]<-RFfile$kgeTest[RFfile$optimizationAlgo=="Meta RF KGE"]
    HeatMapResultsKGE[4,i]<-XGBfile$kgeTest[XGBfile$optimizationAlgo=="default"]
    HeatMapResultsKGE[5,i]<-XGBfile$kgeTest[XGBfile$optimizationAlgo=="optimal default"]
    HeatMapResultsKGE[6,i]<-XGBfile$kgeTest[XGBfile$optimizationAlgo=="Meta XGB KGE"]
    
    HeatMapResultsNSE[1,i]<-RFfile$nseTest[RFfile$optimizationAlgo=="default"]
    HeatMapResultsNSE[2,i]<-RFfile$nseTest[RFfile$optimizationAlgo=="optimal default"]
    HeatMapResultsNSE[3,i]<-RFfile$nseTest[RFfile$optimizationAlgo=="Meta RF NSE"]
    HeatMapResultsNSE[4,i]<-XGBfile$nseTest[XGBfile$optimizationAlgo=="default"]
    HeatMapResultsNSE[5,i]<-XGBfile$nseTest[XGBfile$optimizationAlgo=="optimal default"]
    HeatMapResultsNSE[6,i]<-XGBfile$nseTest[XGBfile$optimizationAlgo=="Meta XGB NSE"]
  }
  
  col <- colorRampPalette(c("#833030", "#EE9988", "#FFFFFF", "#77AADD", "#2e5173"))
  
  rownames(HeatMapResultsKGE)<-c("Default RF","Optimal Default RF","Meta RF KGE","Default XGB","Optimal Default XGB","Meta XGB KGE")
  #colnames(HeatMapResultsKGE)<-substr(GeneralFiles1,start=1,stop=str_length(GeneralFiles1)-4)
  colranksKGE<- colRanks(-HeatMapResultsKGE)
  #colnames(colranksKGE)<-colnames(HeatMapResultsKGE)
  rownames(colranksKGE)<-rownames(HeatMapResultsKGE)
  
  
  jpeg(paste0("C:/Users/joeja/Desktop/REX/REX2020_2021/Results_Plots/KGE2new",maxiter,".jpg"),width = 12, height = 6, units = 'in',res = 600)
  heatmap.2(colranksKGE,Rowv = F,Colv = F,cexRow = 1.5,cexCol = 1.5,
            density.info="none", trace="none",key = F,sepcolor = "black",
            colsep = 1:ncol(HeatMapResultsKGE),rowsep = 1:nrow(HeatMapResultsKGE),srtRow = 30,
            margins = c(4,12),col = col(6)[6:1],
            lwid=c(0.1,4), lhei=c(0.1,4))
  dev.off()
  
  
  rownames(HeatMapResultsNSE)<-c("Default RF","Optimal Default RF","Meta RF NSE","Default XGB","Optimal Default XGB","Meta XGB NSE")
  #colnames(HeatMapResultsNSE)<-substr(GeneralFiles1,start=1,stop=str_length(GeneralFiles1)-4)
  colranksNSE<- colRanks(-HeatMapResultsNSE)
  #colnames(colranksNSE)<-colnames(HeatMapResultsNSE)
  rownames(colranksNSE)<-rownames(HeatMapResultsNSE)
  
  
  jpeg(paste0("C:/Users/joeja/Desktop/REX/REX2020_2021/Results_Plots/NSE2new",maxiter,".jpg"),width = 12, height = 6, units = 'in',res = 600)
  heatmap.2(colranksNSE,Rowv = F,Colv = F,cexRow = 1.5,cexCol = 1.5,
            density.info="none", trace="none",key = F,sepcolor = "black",
            colsep = 1:ncol(HeatMapResultsNSE),rowsep = 1:nrow(HeatMapResultsNSE),srtRow = 30,
            margins = c(4,12),col = col(6)[6:1],
            lwid=c(0.1,4), lhei=c(0.1,4))
  dev.off()
}


















#################################################################################################################################
#################################################     testing meta models   #####################################################
#################################################################################################################################

library(ranger)
library(mfe)
library(ECoL)
library(dummies)
library(stringr)
rm(list = ls())


library(xgboost)
files <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/BaseResults2/XGB', full.names=TRUE, recursive=FALSE)
filenames <- list.files(path = 'C:/Users/joeja/Desktop/REX/REX2020_2021/BaseResults2/XGB', full.names=F, recursive=FALSE)
numfold<-10
numPred<-2000000



MetaData<-data.frame()
for(otherfiles in 1:length(filenames)){
  data <- read.csv(files[otherfiles])
  data$normalizedKGE<-data$kgeTest-data$kgeTest[2]
  data$normalizedNSE<-data$nseTest- data$nseTest[2]
  data$dataset<-otherfiles
  MetaData<-rbind(MetaData,data)
}

MetaData$normalizedKGE<-rank(MetaData$normalizedKGE)
MetaData$normalizedNSE<-rank(MetaData$normalizedNSE)



#get metaX
MetaX<-MetaData[,c(2:9,15:(ncol(MetaData)-3))]
Metay1<-MetaData$normalizedKGE
Metay2<-MetaData$normalizedNSE


#make meta predictions
MetaXTrain<-MetaX[MetaData$dataset<= length(filenames)/2,]
Metay1Train<-Metay1[MetaData$dataset<= length(filenames)/2]
MetaXTest<-MetaX[MetaData$dataset> length(filenames)/2,]
Metay1Test<-Metay1[MetaData$dataset> length(filenames)/2]
Metay2Train<-Metay2[MetaData$dataset<= length(filenames)/2]
Metay2Test<-Metay2[MetaData$dataset> length(filenames)/2]


mod1<-xgb.train(params= list(eta=.018,subsample=.839,max_depth=13,min_child_weight=2.06,colsample_bytree=.752,lambda=0.982, alpha=1.113),
                data=xgb.DMatrix(as.matrix(MetaXTrain[,keepFeaturesY1]),label=Metay1Train),verbose = 0,nrounds = 4168)
mod2<-xgb.train(params=list(eta=.018,subsample=.839,max_depth=13,min_child_weight=2.06,colsample_bytree=.752,lambda=0.982, alpha=1.113),
                data=xgb.DMatrix(as.matrix(MetaXTest[,keepFeaturesY1]),label=Metay1Test),verbose = 0,nrounds = 4168)

pred1<-predict(mod1,xgb.DMatrix(as.matrix(MetaXTest[,keepFeaturesY1])))
pred2<-predict(mod2,xgb.DMatrix(as.matrix(MetaXTrain[,keepFeaturesY1])))

curR2_1<-1-sum((pred1- Metay1Test)^2)/sum((Metay1Test - mean(Metay1Test))^2)
curR2_2<-1-sum((pred2- Metay1Train)^2)/sum((Metay1Train - mean(Metay1Train))^2)
mean(c(curR2_1,curR2_2))


mod1<-ranger(x=MetaXTrain,y=Metay1Train,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(ncol(MetaXTrain)*.257),min.node.size = 1)
mod2<-ranger(x=MetaXTest,y=Metay1Test,oob.error = T,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(ncol(MetaXTest)*.257),min.node.size = 1,importance = "permutation")

pred1<-predict(mod1,MetaXTest)
pred2<-predict(mod2,MetaXTrain)

curR2_1<-1-sum((pred1$predictions- Metay1Test)^2)/sum((Metay1Test - mean(Metay1Test))^2)
curR2_2<-1-sum((pred2$predictions- Metay1Train)^2)/sum((Metay1Train - mean(Metay1Train))^2)
mean(c(curR2_1,curR2_2))




keepFeaturesY1<-c("alpha","eta","min_child_weight","subsample","colsample_bytree","numrounds","lambda")
keepGoing<-TRUE
bestR2<-0
while(keepGoing){
  keepGoing<-FALSE
  for(curFeature in setdiff(colnames(MetaX),keepFeaturesY1)){
    mod1<-ranger(x=MetaXTrain[,c(keepFeaturesY1,curFeature)],y=Metay1Train,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(c(keepFeaturesY1,curFeature))*.257),min.node.size = 1)
    pred<-predict(mod1,MetaXTest)
    curR2_1<-1-sum((pred$predictions- Metay1Test)^2)/sum((Metay1Test - mean(Metay1Test))^2)
    
    mod1<-ranger(x=MetaXTest[,c(keepFeaturesY1,curFeature)],y=Metay1Test,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(c(keepFeaturesY1,curFeature))*.257),min.node.size = 1)
    pred<-predict(mod1,MetaXTrain)
    curR2_2<-1-sum((pred$predictions- Metay1Train)^2)/sum((Metay1Train - mean(Metay1Train))^2)
    
    curR2<-mean(c(curR2_1,curR2_2))
    if(curR2>bestR2){
      bestR2<-curR2
      FeatureToAdd<-curFeature
      keepGoing<-TRUE
    }
  }
  if(keepGoing){
    keepFeaturesY1<-c(keepFeaturesY1,FeatureToAdd)
    print(FeatureToAdd)
  } 
}
bestR2
keepFeaturesY1



mod1<-ranger(x=MetaXTrain,y=Metay2Train,oob.error = F)
pred<-predict(mod1,MetaXTest)
curR2_1<-1-sum((pred$predictions- Metay2Test)^2)/sum((Metay2Test - mean(Metay2Test))^2)

mod1<-ranger(x=MetaXTest,y=Metay2Test,oob.error = F)
pred<-predict(mod1,MetaXTrain)
curR2_2<-1-sum((pred$predictions- Metay2Train)^2)/sum((Metay2Train - mean(Metay2Train))^2)
mean(c(curR2_1,curR2_2))


keepFeaturesY2<-c("minnodesize","samplefraction","mtry","replace")
keepGoing<-TRUE
bestR2<-0
while(keepGoing){
  keepGoing<-FALSE
  for(curFeature in setdiff(colnames(MetaX),keepFeaturesY2)){
    mod1<-ranger(x=MetaXTrain[,c(keepFeaturesY2,curFeature)],y=Metay2Train,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(c(keepFeaturesY2,curFeature))*.257),min.node.size = 1)
    pred<-predict(mod1,MetaXTest)
    curR2_1<-1-sum((pred$predictions- Metay2Test)^2)/sum((Metay2Test - mean(Metay2Test))^2)
    
    mod1<-ranger(x=MetaXTest[,c(keepFeaturesY2,curFeature)],y=Metay2Test,oob.error = F,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(c(keepFeaturesY2,curFeature))*.257),min.node.size = 1)
    pred<-predict(mod1,MetaXTrain)
    curR2_2<-1-sum((pred$predictions- Metay2Train)^2)/sum((Metay2Train - mean(Metay2Train))^2)
    
    curR2<-mean(c(curR2_1,curR2_2))
    if(curR2>bestR2){
      bestR2<-curR2
      FeatureToAdd<-curFeature
      keepGoing<-TRUE
    }
  }
  if(keepGoing){
    keepFeaturesY2<-c(keepFeaturesY2,FeatureToAdd)
    print(FeatureToAdd)
  } 
}
bestR2
keepFeaturesY2

mod2<-ranger(x=MetaX[,keepFeaturesY2],y=Metay2,oob.error = T,num.trees = 983, replace = F,sample.fraction = .703,mtry = round(length(keepFeaturesY2)*.257),min.node.size = 1)
mod2$r.squared





# test new optimal defaults on random dataset
calcKGE<- function(real,pred){
  if(sd(real)==0){
    real=real+rnorm(length(real),mean = 0,sd=.0001)
  }
  if(sd(pred)==0){
    pred=pred+rnorm(length(pred),mean = 0,sd=.0001)
  }
  r<- cor(pred,real)
  alpha<- mean(pred)/mean(real)
  beta <- (sd(pred)/mean(pred))/ (sd(real)/mean(real))
  KGE<-(1-sqrt((r-1)^2+(alpha-1)^2+(beta-1)^2))
  return(KGE)
}


library(ranger)
CAMELS_US_q95_Wide <- read.csv("C:/Users/joeja/Desktop/REX/REX2020_2021/Data/Clean_Data/CAMELS_US_q95_Long.csv")
totest<-sample(1:nrow(CAMELS_US_q95_Wide),round(nrow(CAMELS_US_q95_Wide)*0.2))
test_set<-CAMELS_US_q95_Wide[totest,]
CAMELS_US_q95_Wide<-CAMELS_US_q95_Wide[-totest,]

mod1<-ranger(x=CAMELS_US_q95_Wide[,-1],y=CAMELS_US_q95_Wide[,1],mtry = round((ncol(CAMELS_US_q95_Wide)-1)*0.65),num.trees = 100
             ,replace = F,min.node.size = nrow(CAMELS_US_q95_Wide)^0.3, sample.fraction = 0.854) # for kge
mod2<-ranger(x=CAMELS_US_q95_Wide[,-1],y=CAMELS_US_q95_Wide[,1],mtry = round((ncol(CAMELS_US_q95_Wide)-1)*0.257),num.trees = 983,replace = F,min.node.size = 1, sample.fraction = 0.703) #OD
mod3<-ranger(x=CAMELS_US_q95_Wide[,-1],y=CAMELS_US_q95_Wide[,1],mtry = round((ncol(CAMELS_US_q95_Wide)-1)*0.4),num.trees = 100
             ,replace = T,min.node.size = nrow(CAMELS_US_q95_Wide)^0.001, sample.fraction = 0.85) # for nse
mod4<-ranger(x=CAMELS_US_q95_Wide[,-1],y=CAMELS_US_q95_Wide[,1],num.trees = 500)

pred1<-predict(mod1,test_set[,-1])$predictions
pred2<-predict(mod2,test_set[,-1])$predictions
pred3<-predict(mod3,test_set[,-1])$predictions
pred4<-predict(mod4,test_set[,-1])$predictions

curR2_1<-1-sum((pred1- test_set[,1])^2)/sum((test_set[,1] - mean(test_set[,1]))^2)
curR2_2<-1-sum((pred2- test_set[,1])^2)/sum((test_set[,1] - mean(test_set[,1]))^2)
curR2_3<-1-sum((pred3- test_set[,1])^2)/sum((test_set[,1] - mean(test_set[,1]))^2)
curR2_4<-1-sum((pred4- test_set[,1])^2)/sum((test_set[,1] - mean(test_set[,1]))^2)
curR2_3- curR2_2
curR2_3- curR2_4

curR2_1<-calcKGE(real=test_set[,1],pred=pred1)
curR2_2<-calcKGE(real=test_set[,1],pred=pred2)
curR2_3<-calcKGE(real=test_set[,1],pred=pred3)
curR2_4<-calcKGE(real=test_set[,1],pred=pred4)
curR2_1- curR2_2
curR2_1- curR2_4




library(xgboost)
CAMELS_US_q95_Wide <- read.csv("C:/Users/joeja/Desktop/REX/REX2020_2021/Data/Clean_Data/CAMELS_US_hfd_mean_Wide.csv")
CAMELS_US_q95_Wide<-CAMELS_US_q95_Wide[,sapply(CAMELS_US_q95_Wide, is.numeric)]
totest<-sample(1:nrow(CAMELS_US_q95_Wide),round(nrow(CAMELS_US_q95_Wide)*0.2))
test_set<-CAMELS_US_q95_Wide[totest,]
CAMELS_US_q95_Wide<-CAMELS_US_q95_Wide[-totest,]

mod1<-xgb.train(params= list(eta=.0074,subsample=.724,max_depth=13,min_child_weight=4.74,colsample_bytree=.637,lambda=0.3497, alpha=0.0628),
                data=xgb.DMatrix(as.matrix(CAMELS_US_q95_Wide[,-1]), label = CAMELS_US_q95_Wide[,1]),verbose = 0,nrounds = 2500) # for NSE optimization

mod2<-xgb.train(data=xgb.DMatrix(as.matrix(CAMELS_US_q95_Wide[,-1]),label=CAMELS_US_q95_Wide[,1]),verbose = 0,nrounds = 500) # just use this for KGE

mod3<-xgb.train(params=list(eta=.018,subsample=.839,max_depth=13,min_child_weight=2.06,colsample_bytree=.752,lambda=0.982, alpha=1.113),
                data=xgb.DMatrix(as.matrix(CAMELS_US_q95_Wide[,-1]),label=CAMELS_US_q95_Wide[,1]),verbose = 0,nrounds = 4168)

pred1<-predict(mod1,xgb.DMatrix(as.matrix(test_set[,-1])))
pred2<-predict(mod2,xgb.DMatrix(as.matrix(test_set[,-1])))
pred3<-predict(mod3,xgb.DMatrix(as.matrix(test_set[,-1])))

curR2_1<-1-sum((pred1- test_set[,1])^2)/sum((test_set[,1] - mean(test_set[,1]))^2)
curR2_2<-1-sum((pred2- test_set[,1])^2)/sum((test_set[,1] - mean(test_set[,1]))^2)
curR2_3<-1-sum((pred3- test_set[,1])^2)/sum((test_set[,1] - mean(test_set[,1]))^2)
curR2_1- curR2_2
curR2_1- curR2_3

curR2_1<-calcKGE(real=test_set[,1],pred=pred1)
curR2_2<-calcKGE(real=test_set[,1],pred=pred2)
curR2_3<-calcKGE(real=test_set[,1],pred=pred3)
curR2_1- curR2_2
curR2_1- curR2_3

