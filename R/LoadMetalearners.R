#Load existing metalearners based on ML algorithm and objective function used
LoadMetalearner <- function(obj_fn, ml_algo){
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
