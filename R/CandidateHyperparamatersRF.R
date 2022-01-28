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
