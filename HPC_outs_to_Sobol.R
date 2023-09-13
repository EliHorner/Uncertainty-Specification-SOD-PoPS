#HPC Individual Rasters to Sobol Indices Rasters
library(terra)

#Setup values (change as needed)
num_sources <- 3
samples <- 4000
setupList <- c('All', 'Host', 'IC', 'Par', 'NoHost', 'NoIC', 'NoPar', 'None')
inpath <- "P:/PoPS_Sobol_Analysis/outputs/" #None/pops_runs
outpath <- "P:/PoPS_Sobol_Analysis/outputs/Sobol Rasters/"

#Read in Rasters (Waring: takes ~1 hour to load all 32k rasters)
for (i in setupList){
  fullPath <- paste0(inpath, i, '/pops_runs/')
  filelist <- list.files(fullPath, pattern = "inf*")
  temp <- rast(paste0(fullPath, filelist))
  assign(paste0(i, "Rasts"), temp)
}

RastsStack <- c(AllRasts, HostRasts, ICRasts, ParRasts, NoHostRasts, NoICRasts, NoParRasts, NoneRasts)
#Warning: This also takes a while ~2 hours
writeRaster(RastsStack, paste0(outpath, 'RastsStack.tif'))

#Less efficient method that allows taking any numbers up to samples
#Figure out if this can be made into a raster stack rather than a list
# out_vals_rasts <- rast(paste0(inpath, "infected_1.tif"))
# for(i in 2:samples){
#   out_vals_rasts <- c(out_vals_rasts, rast(paste0(inpath, "infected_", i, ".tif")))
# }

ValsStack <- global(RastsStack, sum, na.rm = TRUE)

#Control Matrix
out_u_mat <- matrix(0, nrow = num_sources, ncol = samples*(2^num_sources))
#rows = host, ic, par; cols = samples
out_u_mat[,1:samples] <- c(1,1,1)
out_u_mat[,(samples+1):(2*samples)] = c(1,0,0)
out_u_mat[,((2*samples)+1):(3*samples)] = c(0,1,0)
out_u_mat[,((3*samples)+1):(4*samples)] = c(0,0,1)
out_u_mat[,((4*samples)+1):(5*samples)] = c(0,1,1)
out_u_mat[,((5*samples)+1):(6*samples)] = c(1,0,1)
out_u_mat[,((6*samples)+1):(7*samples)] = c(1,1,0)

#Sobol Functions
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

sobolFirstOrderRast <- function(rastersList, uMat, uSource){
  sortList <- uMat[uSource,]
  numWith <- mean(rastersList[[sortList == 1]])
  numWithout <- mean(rastersList[[sortList == 0]])
  numerator <- (stdev(numWith, numWithout))^2
  return(numerator/(stdev(rastersList))^2)
}

sobolTotalOrderRast <- function(rastersList, uMat, uSource){
  sortMat <- uMat[-uSource,]
  r <- rast(nrow = 1073, ncol = 686, ext = ext(RastsStack), crs = crs(RastsStack))
  varRastlist <- c(rep(r, ncol(unique(sortMat, MARGIN = 2))))
  for(i in 1:ncol(unique(sortMat, MARGIN = 2))){
    varRastlist[[i]] <- stdev(rastersList[[apply(sortMat, 2, identical, unique(sortMat, MARGIN = 2)[,i])]])^2
  }
  numerator <- mean(varRastlist)
  return(numerator/ (stdev(rastersList)^2))
}

#Run Summarized Sobol Analyses (whole study area)
SFOvals <- c(rep(0, num_sources))
STOvals <- c(rep(0, num_sources))

for(i in 1:num_sources){
 SFOvals[i] <- sobolFirstOrder(ValsStack$sum, out_u_mat, i)
 STOvals[i] <- sobolTotalOrder(ValsStack$sum, out_u_mat, i)
}

#Run Sobol Analyses on Rasters (at cell level)
for(i in 1:num_sources){
  writeRaster(sobolFirstOrderRast(RastsStack, out_u_mat, i), paste0(outpath, paste('/SobolFirstOrder', setupList[i+1],'.tif', sep ='')), overwrite = TRUE)
}

sfoHost <- rast(paste0(outpath, '/SobolFirstOrderhost.tif'))
sfoIC <- rast(paste0(outpath, '/SobolFirstOrderic.tif'))
sfoPar <- rast(paste0(outpath, '/SobolFirstOrderpar.tif'))

for(i in 1:num_sources){
  writeRaster(sobolTotalOrderRast(RastsStack, out_u_mat, i), paste0(outpath, paste('/SobolTotalOrder', setupList[i+1],'.tif', sep ='')), overwrite = TRUE)
}

stoHost <- rast(paste0(outpath, '/SobolTotalOrderhost.tif'))
stoIC <- rast(paste0(outpath, '/SobolTotalOrderic.tif'))
stoPar <- rast(paste0(outpath, '/SobolTotalOrderpar.tif'))