#' Load Metalearners
#'
#' @description  Load existing metalearners based on ML algorithm and objective function used to evaluate it. This metalearner can be used with unseen data.
#'
#' @param obj_fn The objective function used to evaluate models (KGE/NSE)
#' @param ml_algo The type of machine learning algorithm to train (RF/XGB)
#'
#'@usage LoadMetalearner(obj_fn,ml_algo)
#'
#' @return will save the specified model as an .rds file for future use/reference
#'
#' @examples
#' LoadMetalearner("KGE","RF")
#' LoadMetalearner("NSE","XGB")


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
