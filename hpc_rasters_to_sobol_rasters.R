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


#Sobol Calculations on Output Rasters

setupList <- c('All', 'Host', 'IC', 'Par', 'NoHost', 'NoIC', 'NoPar', 'None')
num_sources <- 3

out_u_mat <- matrix(0, nrow = num_sources, ncol = length(setupList))
#Rows = host, ic, par, Cols = Combinations
out_u_mat[,1] = c(1,1,1)
out_u_mat[,2] = c(1,0,0)
out_u_mat[,3] = c(0,1,0)
out_u_mat[,4] = c(0,0,1)
out_u_mat[,5] = c(0,1,1)
out_u_mat[,6] = c(1,0,1)
out_u_mat[,7] = c(1,1,0)

outpath <- "C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/12k Test 2"

out_vals_rasts_var <- c(rast(paste0(outpath, '/All_SD.tif'))^2, rast(paste0(outpath, '/Host_SD.tif'))^2, rast(paste0(outpath, '/IC_SD.tif'))^2, rast(paste0(outpath, '/Par_SD.tif'))^2, rast(paste0(outpath, '/NoHost_SD.tif'))^2, rast(paste0(outpath, '/NoIC_SD.tif'))^2, rast(paste0(outpath, '/NoPar_SD.tif'))^2, rast(paste0(outpath, '/None_SD.tif'))^2)

for(i in 1:num_sources){
  writeRaster(sobolFirstOrderRast(out_vals_rasts_var, out_u_mat, i), paste0(outpath, paste('/SobolFirstOrder', setupList[i+1],'.tif', sep ='')), overwrite = TRUE)
}

sfoHost <- rast(paste0(outpath, '/SobolFirstOrderhost.tif'))
sfoIC <- rast(paste0(outpath, '/SobolFirstOrderic.tif'))
sfoPar <- rast(paste0(outpath, '/SobolFirstOrderpar.tif'))

sfoTotal <- sfoHost + sfoIC + sfoPar
writeRaster(sfoTotal, paste0(outpath, '/SobolFirstOrderSum.tif'), overwrite = TRUE)

sobolTotalOrderRast <- function(rastersList, uMat, uSource){
  sortMat <- uMat[-uSource,]
  r <- rast(nrow = 1073, ncol = 686, ext = ext(sfoTotal), crs = crs(sfoTotal))
  varRastlist <- c(rep(r, ncol(unique(sortMat, MARGIN = 2))))
  for(i in 1:ncol(unique(sortMat, MARGIN = 2))){
    varRastlist[[i]] <- stdev(rastersList[[apply(sortMat, 2, identical, unique(sortMat, MARGIN = 2)[,i])]])^2
  }
  numerator <- mean(varRastlist)
  return(numerator/ (stdev(rastersList)^2))
}

for(i in 1:num_sources){
  writeRaster(sobolTotalOrderRast(out_vals_rasts_var, out_u_mat, i), paste0(outpath, paste('/SobolTotalOrder', setupList[i+1],'.tif', sep ='')), overwrite = TRUE)
}

stoHost <- rast(paste0(outpath, '/SobolTotalOrderhost.tif'))
stoIC <- rast(paste0(outpath, '/SobolTotalOrderic.tif'))
stoPar <- rast(paste0(outpath, '/SobolTotalOrderpar.tif'))

stoTotal <- stoHost + stoIC + stoPar
writeRaster(stoTotal, paste0(outpath, '/SobolTotalOrderSum.tif'), overwrite = TRUE)