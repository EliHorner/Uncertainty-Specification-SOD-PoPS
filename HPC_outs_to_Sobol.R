#HPC Individual Rasters to Sobol Indices Rasters
library(terra)

#Setup values (change as needed)
num_sources <- 5
samples <- 10000

#File path setup
setupList <- paste0('case', seq(1,32))
inpath <- "P:/PoPS_Sobol_Analysis/outputs/"
outpath <- "P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/"

#Read in Rasters (Warning: 320k rasters take up ~25GB of memory and will load slowly)
for (i in setupList){
  fullPath <- paste0(inpath, i, '/pops_runs/')
  filelist <- list.files(fullPath, pattern = "inf*")
  temp <- rast(paste0(fullPath, filelist))
  assign(paste0(i, "Rasts"), temp)
}

RastsStack <- c(case1Rasts, case2Rasts, case3Rasts, case4Rasts, case5Rasts, case6Rasts, case7Rasts, case8Rasts,
                case9Rasts, case10Rasts, case11Rasts, case12Rasts, case13Rasts, case14Rasts, case15Rasts, case16Rasts, 
                case17Rasts, case18Rasts, case19Rasts, case20Rasts, case21Rasts, case22Rasts, case23Rasts, case24Rasts, 
                case25Rasts, case26Rasts, case27Rasts, case28Rasts, case29Rasts, case30Rasts, case31Rasts, case32Rasts)
MeansStack <- mean(case1Rasts)
for (i in 2:length(setupList)){
  MeansStack <- c(MeansStack, mean(setupList[i]))
}

#Warning: This also takes a long time and requires ~25GB of memory
writeRaster(RastsStack, paste0(outpath, 'RastsStack.tif'))
writeRaster(MeansStack, paste0(outpath, 'MeansStack.tif'))
#RastsStack <- rast(paste0(outpath, 'RastsStack.tif'))
#MeansStack <- rast(paste0(outpath, 'MeansStack.tif'))

#Less efficient method that allows taking any numbers up to samples
#Figure out if this can be made into a raster stack rather than a list
# out_vals_rasts <- rast(paste0(inpath, "All/infected_1.tif"))
# for()
# for(i in 2:samples){
#   out_vals_rasts <- c(out_vals_rasts, rast(paste0(inpath, "infected_", i, ".tif")))
# }

ValsStack <- global(RastsStack, sum, na.rm = TRUE)
MeanValsStack <- global(MeansStack, sum, na.rm = TRUE)
write.csv(ValsStack, paste0(outpath, 'ValsStack.csv'))
write.csv(MeanValsStack, paste0(outpath, 'MeanValsStack.csv'))

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

means_out_u_mat <- matrix(0, nrow = num_sources, ncol = length(setupList))
#Rows = host, ic, par, Cols = Combinations
means_out_u_mat[,1] =  c(1,1,1,1,1)
means_out_u_mat[,2] =  c(1,1,1,1,0)
means_out_u_mat[,3] =  c(1,1,1,0,1)
means_out_u_mat[,4] =  c(1,1,1,0,0)
means_out_u_mat[,5] =  c(1,1,0,1,1)
means_out_u_mat[,6] =  c(1,1,0,1,0)
means_out_u_mat[,7] =  c(1,1,0,0,1)
means_out_u_mat[,8] =  c(1,1,0,0,0)
means_out_u_mat[,9] =  c(1,0,1,1,1)
means_out_u_mat[,10] = c(1,0,1,1,0)
means_out_u_mat[,11] = c(1,0,1,0,1)
means_out_u_mat[,12] = c(1,0,1,0,0)
means_out_u_mat[,13] = c(1,0,0,1,1)
means_out_u_mat[,14] = c(1,0,0,1,0)
means_out_u_mat[,15] = c(1,0,0,0,1)
means_out_u_mat[,16] = c(1,0,0,0,0)
means_out_u_mat[,17] = c(0,1,1,1,1)
means_out_u_mat[,18] = c(0,1,1,1,0)
means_out_u_mat[,19] = c(0,1,1,0,1)
means_out_u_mat[,20] = c(0,1,1,0,0)
means_out_u_mat[,21] = c(0,1,0,1,1)
means_out_u_mat[,22] = c(0,1,0,1,0)
means_out_u_mat[,23] = c(0,1,0,0,1)
means_out_u_mat[,24] = c(0,1,0,0,0)
means_out_u_mat[,25] = c(0,0,1,1,1)
means_out_u_mat[,26] = c(0,0,1,1,0)
means_out_u_mat[,27] = c(0,0,1,0,1)
means_out_u_mat[,28] = c(0,0,1,0,0)
means_out_u_mat[,29] = c(0,0,0,1,1)
means_out_u_mat[,30] = c(0,0,0,1,0)
means_out_u_mat[,31] = c(0,0,0,0,1)
means_out_u_mat[,32] = c(0,0,0,0,0)


#PRe-compute total variance to save time
sampleVar <- (stdev(RastsStack))^2
writeRaster(sampleVar, paste0(outpath, 'sampleVar.tif'))
valSampleVar <- global(sampleVar, sum, na.rm = TRUE)

#Sobol Functions
sobolFirstOrder <- function(valuesList, uMat, uSource){
  sortList <- uMat[uSource,]
  numWith <- mean(valuesList[sortList == 1])
  numWithout <- mean(valuesList[sortList == 0])
  numerator <- var(c(numWith, numWithout))
  return(numerator/var(valuesList))
}

sobolTotalOrder <- function(valuesList, uMat, uSource, totalVar){
  sortMat <- uMat[-uSource,]
  uniqueCases <- ncol(unique(sortMat, MARGIN = 2))
  varlist <- c(rep(0, uniqueCases))
  for(i in 1:uniqueCases){
    varlist[i] <- var(valuesList[apply(sortMat, 2, identical, unique(sortMat, MARGIN = 2)[,i])])
  }
  numerator <- mean(varlist)
  return(numerator/ totalVar)
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
  r <- rast(nrow = 1073, ncol = 686, ext = ext(RastsStack), crs = crs(RastsStack))
  uniqueCases <- ncol(unique(sortMat, MARGIN = 2))
  varRastlist <- c(rep(r, uniqueCases))
  for(i in 1:uniqueCases){
    varRastlist[[i]] <- stdev(rastersList[[apply(sortMat, 2, identical, unique(sortMat, MARGIN = 2)[,i])]])^2
  }
  numerator <- mean(varRastlist)
  return(numerator/ totalVar)
}

#Run Summarized Sobol Analyses (whole study area)
SFOvals <- c(rep(0, num_sources))
STOvals <- c(rep(0, num_sources))

for(i in 1:num_sources){
 SFOvals[i] <- sobolFirstOrder(ValsStack$sum, out_u_mat, i)
 STOvals[i] <- sobolTotalOrder(MeanValsStack$sum, means_out_u_mat, i, valSampleVar$sum)
}

write.csv(SFOvals, paste0(outpath, 'SFOVals.csv'))
write.csv(STOvals, paste0(outpath, 'STOVals.csv'))

#Run Sobol Analyses on Rasters (at cell level)
for(i in 1:num_sources){
  writeRaster(sobolFirstOrderRast(MeansStack, means_out_u_mat, i, sampleVar), paste0(outpath, paste('SobolFirstOrder', setupList[i+1],'.tif', sep ='')), overwrite = TRUE)
}

# sfoHost <- rast(paste0(outpath, '/SobolFirstOrderhost.tif'))
# sfoIC <- rast(paste0(outpath, '/SobolFirstOrderic.tif'))
# sfoPar <- rast(paste0(outpath, '/SobolFirstOrderpar.tif'))

for(i in 1:num_sources){
  writeRaster(sobolTotalOrderRast(MeansStack, means_out_u_mat, i, sampleVar), paste0(outpath, paste('SobolTotalOrder', setupList[i+1],'.tif', sep ='')), overwrite = TRUE)
}

# stoHost <- rast(paste0(outpath, '/SobolTotalOrderhost.tif'))
# stoIC <- rast(paste0(outpath, '/SobolTotalOrderic.tif'))
# stoPar <- rast(paste0(outpath, '/SobolTotalOrderpar.tif'))

# library(RColorBrewer)
# pal <- colorRampPalette(brewer.pal(9, 'OrRd'))(25)
# 
# testPoints <- mean(RastsStack)
