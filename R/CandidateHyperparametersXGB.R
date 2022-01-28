#Generate candidate hyperparameter set to use with new dataset
CandidateHyperparamatersXGB <- function(numr,nrounds,eta,subsample,max_depth,min_child_weight,colsample_bytree,lambda,alpha){

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
