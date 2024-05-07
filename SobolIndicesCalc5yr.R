#Calculate SFO and STO indices for multiple (5) years
library(terra)

#Setup
num_sources <- 5
source_list <- c('Host', 'IC', 'Weather', 'Par', 'Process')
samples <- 4000
simulation <- '5year4k'
years <- 5

#File path setup
setupList <- paste0('case', seq(1,32))
inpath <- paste0("P:/PoPS_Sobol_Analysis/outputs/", simulation, "/Sobol Ready Stacks/")
outpath <- paste0("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/", simulation, "/")


# Read from file (comment if not reading from files)
# yr1Stack1 <- terra::rast(paste0(inpath, 'yr1Stack1.tif'))
# yr1Stack2 <- terra::rast(paste0(inpath, 'yr1Stack2.tif'))
# yr1Stack <- c(yr1Stack1, yr1Stack2)
# yr2Stack1 <- terra::rast(paste0(inpath, 'yr2Stack1.tif'))
# yr2Stack2 <- terra::rast(paste0(inpath, 'yr2Stack2.tif'))
# yr2Stack <- c(yr2Stack1, yr2Stack2)
# yr3Stack1 <- terra::rast(paste0(inpath, 'yr3Stack1.tif'))
# yr3Stack2 <- terra::rast(paste0(inpath, 'yr3Stack2.tif'))
# yr3Stack <- c(yr3Stack1, yr3Stack2)
# yr4Stack1 <- terra::rast(paste0(inpath, 'yr4Stack1.tif'))
# yr4Stack2 <- terra::rast(paste0(inpath, 'yr4Stack2.tif'))
# yr4Stack <- c(yr4Stack1, yr4Stack2)
# yr5Stack1 <- terra::rast(paste0(inpath, 'yr5Stack1.tif'))
# yr5Stack2 <- terra::rast(paste0(inpath, 'yr5Stack2.tif'))
# yr5Stack <- c(yr5Stack1, yr5Stack2)
# 
# yr1Var <- terra::rast(paste0(inpath, 'yr1Var.tif'))
# yr2Var <- terra::rast(paste0(inpath, 'yr2Var.tif'))
# yr3Var <- terra::rast(paste0(inpath, 'yr3Var.tif'))
# yr4Var <- terra::rast(paste0(inpath, 'yr4Var.tif'))
# yr5Var <- terra::rast(paste0(inpath, 'yr5Var.tif'))

#Control Matrix
out_u_mat <- matrix(0, nrow = num_sources, ncol = samples*(2^num_sources))
#rows = host, ic, par; cols = samples
out_u_mat[,1:samples] <-                    c(1,1,1,1,1)
out_u_mat[,(samples+1):(2*samples)] =       c(1,1,1,1,0)
out_u_mat[,((2*samples)+1):(3*samples)] =   c(1,1,1,0,1)
out_u_mat[,((3*samples)+1):(4*samples)] =   c(1,1,1,0,0)
out_u_mat[,((4*samples)+1):(5*samples)] =   c(1,1,0,1,1)
out_u_mat[,((5*samples)+1):(6*samples)] =   c(1,1,0,1,0)
out_u_mat[,((6*samples)+1):(7*samples)] =   c(1,1,0,0,1)
out_u_mat[,((7*samples)+1):(8*samples)] =   c(1,1,0,0,0)
out_u_mat[,((8*samples)+1):(9*samples)] =   c(1,0,1,1,1)
out_u_mat[,((9*samples)+1):(10*samples)] =  c(1,0,1,1,0)
out_u_mat[,((10*samples)+1):(11*samples)] = c(1,0,1,0,1)
out_u_mat[,((11*samples)+1):(12*samples)] = c(1,0,1,0,0)
out_u_mat[,((12*samples)+1):(13*samples)] = c(1,0,0,1,1)
out_u_mat[,((13*samples)+1):(14*samples)] = c(1,0,0,1,0)
out_u_mat[,((14*samples)+1):(15*samples)] = c(1,0,0,0,1)
out_u_mat[,((15*samples)+1):(16*samples)] = c(1,0,0,0,0)
out_u_mat[,((16*samples)+1):(17*samples)] = c(0,1,1,1,1)
out_u_mat[,((17*samples)+1):(18*samples)] = c(0,1,1,1,0)
out_u_mat[,((18*samples)+1):(19*samples)] = c(0,1,1,0,1)
out_u_mat[,((19*samples)+1):(20*samples)] = c(0,1,1,0,0)
out_u_mat[,((20*samples)+1):(21*samples)] = c(0,1,0,1,1)
out_u_mat[,((21*samples)+1):(22*samples)] = c(0,1,0,1,0)
out_u_mat[,((22*samples)+1):(23*samples)] = c(0,1,0,0,1)
out_u_mat[,((23*samples)+1):(24*samples)] = c(0,1,0,0,0)
out_u_mat[,((24*samples)+1):(25*samples)] = c(0,0,1,1,1)
out_u_mat[,((25*samples)+1):(26*samples)] = c(0,0,1,1,0)
out_u_mat[,((26*samples)+1):(27*samples)] = c(0,0,1,0,1)
out_u_mat[,((27*samples)+1):(28*samples)] = c(0,0,1,0,0)
out_u_mat[,((28*samples)+1):(29*samples)] = c(0,0,0,1,1)
out_u_mat[,((29*samples)+1):(30*samples)] = c(0,0,0,1,0)
out_u_mat[,((30*samples)+1):(31*samples)] = c(0,0,0,0,1)
out_u_mat[,((31*samples)+1):(32*samples)] = c(0,0,0,0,0)

#Sobol Functions
sobolFirstOrder <- function(valuesList, uMat, uSource){
  sortList <- uMat[uSource,] #row of control matrix associated with source
  numWith <- mean(valuesList[sortList == 1]) #expectation with source
  numWithout <- mean(valuesList[sortList == 0]) #expectation without source
  numerator <- var(c(numWith, numWithout)) #variance of expectation
  return(numerator/var(valuesList)) #normalize by total variance
}

sobolTotalOrder <- function(valuesList, uMat, uSource, totalVar){
  sortMat <- uMat[-uSource,] #rows of control matrix not associated with source
  uniqueCases <- ncol(unique(sortMat, MARGIN = 2)) #unique cases, number of columns with unique sets and orders of values
  varlist <- c(rep(0, uniqueCases)) #initialize variance list
  for(i in 1:uniqueCases){ #for each unique case do
    varlist[i] <- var(valuesList[apply(sortMat, 2, identical, unique(sortMat, MARGIN = 2)[,i])])
    #variance of values from identical columns of source modified control matrix by each unique case (set of columns)
  }
  numerator <- mean(varlist) #expectation of variances
  return(1 - (numerator/ totalVar)) #normalize by total variance
}

sobolFirstOrderRast <- function(rastersList, uMat, uSource, totalVar){
  sortList <- uMat[uSource,]
  numWith <- mean(rastersList[[sortList == 1]])
  numWithout <- mean(rastersList[[sortList == 0]])
  numerator <- (stdev(numWith, numWithout))^2
  return(numerator/totalVar)
}

sobolTotalOrderRast <- function(rastersList, uMat, uSource, totalVar){
  sortMat <- uMat[-uSource,]
  r <- rast(nrow = 1073, ncol = 686, ext = ext(yr1Stack), crs = crs(yr1Stack))
  uniqueCases <- ncol(unique(sortMat, MARGIN = 2))
  meanRastlist <- c(rep(r, uniqueCases))
  for(i in 1:uniqueCases){
    meanRastlist[[i]] <- mean(rastersList[[apply(sortMat, 2, identical, unique(sortMat, MARGIN = 2)[,i])]])
  }
  numerator <- (stdev(meanRastlist))^2
  return((numerator/ totalVar))
}


#Run Summarized Sobol Analyses (whole study area) Lump Method
# SFOvals <- c(rep(0, num_sources))
# STOvals <- c(rep(0, num_sources))
# 
# for(i in 1:num_sources){
#   SFOvals[i] <- sobolFirstOrder(ValsStack$sum, out_u_mat, i)
#   STOvals[i] <- sobolTotalOrder(ValsStack$sum, out_u_mat, i, var(ValsStack$sum))
# }
# 
# write.csv(SFOvalsLump, paste0(outpath, 'Lump1kTest/SFOVals.csv'))
# write.csv(STOvalsLump, paste0(outpath, 'Lump1kTest/STOVals.csv'))

#Run Sobol Analyses on Rasters (at cell level) Lump Method
for(j in 1:years){
  
  RunStack <- get(paste0('yr', j, 'Stack'))
  RunVar <- get(paste0('yr', j, 'Var'))
  
  for(i in 1:num_sources){
    assign(paste0('SFO_', source_list[i]), sobolFirstOrderRast(RunStack, out_u_mat, i, RunVar))
    writeRaster(get(paste0('SFO_', source_list[i])), paste0(outpath, 'yr', j, '/', paste0('SFO_', source_list[i], '.tif')))
  }
  
  for(i in 1:num_sources){
    assign(paste0('STO_', source_list[i]), sobolTotalOrderRast(RunStack, out_u_mat, i, RunVar))
    writeRaster(get(paste0('STO_', source_list[i])), paste0(outpath, 'yr', j, '/', paste0('STO_', source_list[i], '.tif')))
  }
  
}
