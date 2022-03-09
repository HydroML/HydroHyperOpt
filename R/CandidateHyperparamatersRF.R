#' Candidate Hyperparameters - RF
#'
#' @description  Generate candidate hyperparameter set for Random Forest to use with new dataset
#'
#' @param numr The number of hyperparamter values to generate
#' @param mtry_min min mtry hyperparameter value
#' @param mtry_max max mtry hyperparameter value
#' @param numtrees_min min numtrees hyperparameter value
#' @param numtrees_max max numtrees hyperparameter value
#' @param samplefraction_min min samplefraction hyperparameter value
#' @param samplefraction_max max samplefraction hyperparameter value
#' @param minnodesize_min min minnodesize hyperparameter value
#' @param minnodesize_max max minnodesize hyperparameter value
#'
#'@usage CandidateHyperparamatersRF(numr,mtry_min,mtry_max,numtrees_min,numtrees_max,samplefraction_min,samplefraction_max,minnodesize_min,minnodesize_max)
#'
#' @return Dataframe of \code{numr} entries for candidate hyperparameters to use with random forest
#'
#' @examples
#' CandidateHyperparamatersRF(10, 0.3, 0.8, 50, 100, 0.4, 1, 0, 1)
#' CandidateHyperparamatersRF(20, mtry_min=0.6, mtry_max=0.9, samplefraction_min=0.7, minnodesize_min=0, minnodesize_max=1)


CandidateHyperparamatersRF <- function(numr,
                                         mtry_min,mtry_max,
                                         numtrees_min,numtrees_max,
                                         samplefraction_min,samplefraction_max,
                                         minnodesize_min,minnodesize_max){

  #set mtry
  if(mtry_min && mtry_max){
    mtry=seq(mtry_min, mtry_max, length.out=numr)
  } else {
    mtry=rep(100, numr) #default val
  }

  #set numtrees
  if(numtrees_min && numtrees_max){
    numtrees=seq(numtrees_min, numtrees_max, length.out=numr)
  } else {
    numtrees=rep(100, numr) #default val
  }

  #set samplefraction
  if(samplefraction_min && samplefraction_max){
    samplefraction=seq(samplefraction_min, samplefraction_max, length.out=numr)
  } else {
    samplefraction=rep(100, numr) #default val
  }

  #set minnodesize
  if(minnodesize_min && minnodesize_max){
    minnodesize=seq(minnodesize_min, minnodesize_max, length.out=numr)
  } else {
    minnodesize=rep(100, numr) #default val
  }

  #dataset with candidate hyperparameters
  hyperparameters <- data.frame(n=1:numr,
                                mtry=mtry,
                                numtrees=numtrees,
                                replace=c(TRUE,FALSE),
                                samplefraction=samplefraction,
                                minnodesize=minnodesize)

}
