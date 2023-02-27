sobolFirstOrder <- function(valuesList, uMat, uSource){
  sortList <- uMat[uSource,]
  numWith <- mean(valuesList[sortList == 1])
  numWithout <- mean(valuesList[sortList == 0])
  numerator <- var(c(numWith, numWithout))
  return(numerator/var(valuesList))
}

sobolTotalOrder <- function(valuesList, uMat, uSource){
  sortMat <- uMat[-uSource,]
  varlist <- c(rep(0, ncol(unique(sortMat, MARGIN = 2))))
  for(i in 1:ncol(unique(sortMat, MARGIN = 2))){
    varlist[i] <- var(valuesList[apply(sortMat, 2, identical, unique(sortMat, MARGIN = 2)[,i])])
  }
  numerator <- mean(varlist)
  return(numerator/ var(valuesList))
}

#Values list contains values of variance in output of interest for all uncertainty scenarios
#uMat rows correspond to a source, and columns correspond to a run (ensemble)
#Values list and uMat have to have corresponding columns (none = all zeros, all = all ones, etc)
#uSource is as a number <- May become a string that covnerts automatically