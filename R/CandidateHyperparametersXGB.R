#' Candidate Hyperparameters - XGB
#'
#' @description  Generate candidate hyperparameter set for XGBoost to use with new dataset
#'
#' @param numr The number of hyperparamter values to generate
#' @param nrounds_min min mtry hyperparameter value
#' @param nrounds_max max mtry hyperparameter value
#' @param eta_min min eta hyperparameter value
#' @param eta_max max eta hyperparameter value
#' @param subsample_min min subsample hyperparameter value
#' @param subsample_max max subsample hyperparameter value
#' @param max_depth_min min max_depth hyperparameter value
#' @param max_depth_max max max_depth hyperparameter value
#' @param min_child_weight_min min min_child_weight hyperparameter value
#' @param min_child_weight_max max min_child_weight hyperparameter value
#' @param colsample_bytree_min min colsample_bytree hyperparameter value
#' @param colsample_bytree_max max colsample_bytree hyperparameter value
#' @param lambda_min min lambda hyperparameter value
#' @param lambda_max max lambda hyperparameter value
#' @param alpha_min min alpha hyperparameter value
#' @param alpha_max max alpha hyperparameter value
#'
#'@usage CandidateHyperparamatersXGB(numr,nrounds_min,nrounds_max,eta_min,eta_max,subsample_min,subsample_max,max_depth_min,max_depth_max,min_child_weight_min,min_child_weight_max,colsample_bytree_min,colsample_bytree_max,lambda_min,lambda_max,alpha_min,alpha_max)
#'
#' @return Dataframe of \code{numr} entries for candidate hyperparameters to use with xgboost
#'
#' @examples
#' CandidateHyperparamatersXGB(100, 50, 200, 0.05, 0.2, 0.6, 0.7, 6, 10, 0, 1, 0.75, 0.8, 0, 0.5, 0.2, 1)


CandidateHyperparamatersXGB <- function(numr,
                                        nrounds_min,nrounds_max,
                                        eta_min,eta_max,
                                        subsample_min,subsample_max,
                                        max_depth_min,max_depth_max,
                                        min_child_weight_min,min_child_weight_max,
                                        colsample_bytree_min,colsample_bytree_max,
                                        lambda_min,lambda_max,
                                        alpha_min,alpha_max){

  #set nrounds
  if(nrounds_min && nrounds_max){
    nrounds=seq(nrounds_min, nrounds_max, length.out=numr)
  } else {
    nrounds=rep(100, numr) #default val
  }

  #set eta
  if(eta_min && eta_max){
    eta=seq(eta_min, eta_max, length.out=numr)
  } else {
    eta=rep(0.3, numr) #default val
  }

  #set subsample
  if(subsample_min && subsample_max){
    subsample=seq(subsample_min, subsample_max, length.out=numr)
  } else {
    subsample=rep(1, numr) #default val
  }

  #set max_depth
  if(max_depth_min && max_depth_max){
    max_depth=seq(max_depth_min, max_depth_max, length.out=numr)
  } else {
    max_depth=rep(6, numr) #default val
  }

  #set min_child_weight
  if(min_child_weight_min && min_child_weight_max){
    min_child_weight=seq(min_child_weight_min, min_child_weight_max, length.out=numr)
  } else {
    min_child_weight=rep(1, numr) #default val
  }

  #set colsample_bytree
  if(colsample_bytree_min && colsample_bytree_max){
    colsample_bytree=seq(colsample_bytree_min, colsample_bytree_max, length.out=numr)
  } else {
    colsample_bytree=rep(1, numr) #default val
  }

  #set lambda
  if(lambda_min && lambda_max){
    lambda=seq(lambda_min, lambda_max, length.out=numr)
  } else {
    lambda=rep(0, numr) #default val
  }

  #set alpha
  if(alpha_min && alpha_max){
    alpha=seq(alpha_min, alpha_max, length.out=numr)
  } else {
    alpha=rep(1, numr) #default val
  }

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
