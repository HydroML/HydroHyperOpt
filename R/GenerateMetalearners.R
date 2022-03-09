#' Generate Metalearners
#'
#' @description  Train metalearners for specific objective function and ML algorithm
#'
#' @param obj_fn The objective function used to evaluate models (KGE/NSE)
#' @param ml_algo The type of machine learning algorithm to train (RF/XGB)
#'
#'@usage GenerateMetalearners(obj_fn,ml_algo)
#'
#' @return will save the specified model as an .rds file for future use/reference
#'
#' @examples
#' GenerateMetalearners("KGE","RF")
#' GenerateMetalearners("NSE","XGB")


GenerateMetalearners <- function(obj_fn,ml_algo){
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
