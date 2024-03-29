sobolFirstOrder <- function(valuesList, uMat, uSource){
  sortList <- uMat[uSource,]
  numWith <- mean(valuesList[sortList == 1])
  numWithout <- mean(valuesList[sortList == 0])
  numerator <- var(c(numWith, numWithout))
  return(numerator/var(valuesList))
}

sobolFirstOrderRast <- function(rastersList, uMat, uSource){
  sortList <- uMat[uSource,]
  numWith <- mean(rastersList[[sortList == 1]])
  numWithout <- mean(rastersList[[sortList == 0]])
  numerator <- (stdev(numWith, numWithout))^2
  return(numerator/(stdev(rastersList))^2)
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

sobolTotalOrderRast <- function(rastersList, uMat, uSource){
  sortMat <- uMat[-uSource,]
  varRastlist <- c(rep(0, ncol(unique(sortMat, MARGIN = 2))))
  for(i in 1:ncol(unique(sortMat, MARGIN = 2))){
    varRastlist[[i]] <- stdev(rastersList[[apply(sortMat, 2, identical, unique(sortMat, MARGIN = 2)[,i])]])^2
  }
  numerator <- mean(varRastlist)
  return(numerator/ (stdev(RastersList)^2))
}


#Values list contains values of variance in output of interest for all uncertainty scenarios
#uMat rows correspond to a source, and columns correspond to a run (ensemble)
#Values list and uMat have to have corresponding columns (none = all zeros, all = all ones, etc)
#uSource is as a number <- May become a string that converts automatically


#For rasters: Need to take mean of rasters with and without
#Need variance (stdev)^2 of with an without
#So, need to be able to sort from name to source?

#Solution: Make values list into list of rasters (or maybe stacked raster)
#Index from uMat to pull the correct rasters into the calculation
#Output Raster rather than value
#Write vs return?
