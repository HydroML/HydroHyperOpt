#Install Packages
#install.packages("ranger", repos="http://cran.us.r-project.org")
#install.packages("xgboost", repos="http://cran.us.r-project.org")
#install.packages("mfe", repos="http://cran.us.r-project.org")
#install.packages("ECoL", repos="http://cran.us.r-project.org")

#Load libraries
library(ranger)
library(xgboost)
library(mfe)
library(ECoL)

#-------------------------------

#Extract relevant metafeatures (general) from user data to match fields present in metadatabase provided in package
extract_metafeatures <- function(X_train,y_train){
  #Note: assuming user will split dataset into X_train, X_test, y_train, y_test themselves

  #extract general metafeatures from dataset (using mfe package)
  general_metafeatures = metafeatures(x=X_train,y=y_train,c("general"))
  general_metafeatures = general_metafeatures[-c(2,length(general_metafeatures))]

  #transformed categorical features to numeric
  X_train_dummy = dummy.data.frame(X_train, sep = ".")

  #extract complexity measures from dataset to use as metafeatures (using ECoL package)
  complexity_metafeatures = ECoL::complexity(x=X_train_dummy,y=as.numeric(y_train))
  complexity_metafeatures = complexity_metafeatures[-c(9)]

  #create metafeature dataframe object
  meta = c(general_metafeatures,complexity_metafeatures)
  metadata_new <- as.data.frame(t(meta))
}

#Generate candidate hyperparameter set to use with new dataset
candidate_hyperparamaters_rf <- function(numr, mtry, numtrees,samplefraction, minnodesize){

  #dataset with candidate hyperparameters
  hyperparameters <- data.frame(n=1:numr,
                                mtry=mtry,
                                numtrees=numtrees,
                                replace=c(TRUE,FALSE),
                                samplefraction=samplefraction,
                                minnodesize=minnodesize)

}

#Generate candidate hyperparameter set to use with new dataset
candidate_hyperparamaters_xgb <- function(numr,nrounds,eta,subsample,max_depth,min_child_weight,colsample_bytree,lambda,alpha){

  #dataset with candidate hyperparameters
  hyperparameters <- data.frame(n=1:numr,
                                nrounds=nrounds,
                                eta=eta,
                                subsample=subsample,
                                max_depth=max_depth,
                                min_child_weight=min_child_weight,
                                colsample_bytree=colsample_bytree,
                                lambda=lambda,
                                alpha=alpha)
}

#Train metalearners for specific objective function and ML algorithm
generate_metalearners <- function(obj_fn,ml_algo){
  #will save models as .rds files for future use/reference

  if(ml_algo == "RF"){
    pathname = "~/Documents/HydroML/MetaResults/RF"
    filenames = list.files(path = pathname)
  }

  if(ml_algo == "XGB"){
    pathname = "~/Documents/HydroML/MetaResults/XGB"
    filenames = list.files(path = pathname)
  }

  #combine metaresult datasets for training
  MetaData<-data.frame()
  for(file in filenames){
    data = read.csv(paste0(pathname,file))
    MetaData<-rbind(data,MetaData)
  }

  #train metalearner
  metalearner_Xtraindata <- subset(MetaData,-c(n,kgeTrain,kgeTest,nseTrain,nseTest))

  if(ml_algo == "XGB" & obj_fn == "KGE"){
    metalearner_ytraindata <- MetaData$kgeTest
    final_metalearner <- ranger(x=metalearner_Xtraindata,y=metalearner_ytraindata,oob.error = T)
    saveRDS(final_metalearner, "./xgb_kge_metalearner.rds")
  }

  if(ml_algo == "RF" & obj_fn == "KGE"){
    metalearner_ytraindata <- MetaData$kgeTest
    final_metalearner <- ranger(x=metalearner_Xtraindata,y=metalearner_ytraindata,oob.error = T)
    saveRDS(final_metalearner, "./rf_kge_metalearner.rds")
  }

  if(ml_algo == "XGB" & obj_fn == "NSE"){
    metalearner_ytraindata <- MetaData$nseTest
    final_metalearner <- ranger(x=metalearner_Xtraindata,y=metalearner_ytraindata,oob.error = T)
    saveRDS(final_metalearner, "./xgb_nse_metalearner.rds")
  }
  if(ml_algo == "RF" & obj_fn == "NSE"){
    metalearner_ytraindata <- MetaData$nseTest
    final_metalearner <- ranger(x=metalearner_Xtraindata,y=metalearner_ytraindata,oob.error = T)
    saveRDS(final_metalearner, "./rf_nse_metalearner.rds")
  }

}

#Load existing metalearners based on ML algorithm and objective function used
load_metalearner <- function(obj_fn, ml_algo){
  #use model that has not seen new data

  if(ml_algo == "XGB" & obj_fn == "KGE"){
    metalearner <- readRDS("./xgb_kge_metalearner.rds")
  }
  if(ml_algo == "RF" & obj_fn == "KGE"){
    metalearner <- readRDS("./rf_kge_metalearner.rds")
  }
  if(ml_algo == "XGB" & obj_fn == "NSE"){
    metalearner <- readRDS("./xgb_nse_metalearner.rds")
  }
  if(ml_algo == "RF" & obj_fn == "NSE"){
    metalearner <- readRDS("./rf_nse_metalearner.rds")
  }
}

#Predict optimal hyperparameter configuration
predict_hyperparameters <- function(obj_fn,ml_algo,candidate_hyperparamaters,user_metadata){
  #load metalearner to use with user data
  metalearner <- load_metalearner(obj_fn, ml_algo)
  metadata_new = results_rf<-cbind(candidate_hyperparamaters,user_metadata)
  pred <- predict(metalearner,type="terminalNodes")
}




