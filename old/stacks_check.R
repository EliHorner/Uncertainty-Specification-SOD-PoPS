# Stacks things and re-run Sobol analysis
# Check results against old results

library(terra)
singleWindow <- terra::ext(c(392000, 397000, 4705000, 4708000))
multiWindow <- terra::ext(c(381000, 390000, 4682000, 4689000))

#Load in data (12k test)
inpath <- "C:/Users/eahorner/Documents/GitHub/Uncertainty-Specification-SOD-PoPS/12k Test 2/"

infStack <- c(rast(paste0(inpath, 'All_Mean.tif')), rast(paste0(inpath, 'Host_Mean.tif')), rast(paste0(inpath, 'IC_Mean.tif')), rast(paste0(inpath, 'Par_Mean.tif')), rast(paste0(inpath, 'NoHost_Mean.tif')), rast(paste0(inpath, 'NoIC_Mean.tif')), rast(paste0(inpath, 'NoPar_Mean.tif')), rast(paste0(inpath, 'None_Mean.tif')))
names(infStack) <- c('All', 'Host', 'IC', 'Par', 'NoHost', 'NoIC', 'NoPar', 'None')
infSDStack <- c(rast(paste0(inpath, 'All_SD.tif')), rast(paste0(inpath, 'Host_SD.tif')), rast(paste0(inpath, 'IC_SD.tif')), rast(paste0(inpath, 'Par_SD.tif')), rast(paste0(inpath, 'NoHost_SD.tif')), rast(paste0(inpath, 'NoIC_SD.tif')), rast(paste0(inpath, 'NoPar_SD.tif')), rast(paste0(inpath, 'None_SD.tif')))
names(infSDStack) <- c('All', 'Host', 'IC', 'Par', 'NoHost', 'NoIC', 'NoPar', 'None')

infMean <- mean(infStack)
infSDM <- mean(infSDStack)

avgInfStack <- c(infStack, infMean)
names(avgInfStack) <- c('All', 'Host', 'IC', 'Par', 'NoHost', 'NoIC', 'NoPar', 'None', 'Average')
plot(avgInfStack, ext = multiWindow)

avgInfSDStack <- c(infSDStack, infSDM)
names(avgInfSDStack) <- c('All', 'Host', 'IC', 'Par', 'NoHost', 'NoIC', 'NoPar', 'None', 'Average')
plot(avgInfSDStack, ext = multiWindow)

meanList <- global(infStack, sum, na.rm = TRUE)
sdList <- global(infSDStack, sum, na.rm = TRUE)


#Calculate First Order Indices
sobolFirstOrder <- function(valuesList, totalVar, uMat, uSource){
  sortList <- uMat[uSource,]
  numWith <- mean(valuesList[sortList == 1])
  numWithout <- mean(valuesList[sortList == 0])
  numerator <- var(c(numWith, numWithout))
  return(numerator/ totalVar)
}

num_sources <- 3
sources_list <- c('Host', 'IC', 'Par')

out_u_mat <- matrix(0, nrow = num_sources, ncol = 8)
#Rows = host, ic, par, Cols = Combinations
out_u_mat[,1] = c(1,1,1)
out_u_mat[,2] = c(1,0,0)
out_u_mat[,3] = c(0,1,0)
out_u_mat[,4] = c(0,0,1)
out_u_mat[,5] = c(0,1,1)
out_u_mat[,6] = c(1,0,1)
out_u_mat[,7] = c(1,1,0)
out_u_mat[,8] = c(0,0,0)

for(i in 1:num_sources){
  print(sobolFirstOrder(meanList$sum, sdList$sum[1], out_u_mat, i))
}

sobolTotalOrder <- function(valuesList, totalVar, uMat, uSource){
  sortMat <- uMat[-uSource,]
  varlist <- c(rep(0, ncol(unique(sortMat, MARGIN = 2))))
  for(i in 1:ncol(unique(sortMat, MARGIN = 2))){
    varlist[i] <- var(valuesList[apply(sortMat, 2, identical, unique(sortMat, MARGIN = 2)[,i])])
  }
  numerator <- mean(varlist)
  return(numerator/ totalVar)
}

for(i in 1:num_sources){
  print(sobolTotalOrder(meanList$sum, sdList$sum[1], out_u_mat, i))
}

sobolFirstOrderRast <- function(rastersList, totalVarRast, uMat, uSource){
  sortList <- uMat[uSource,]
  numWith <- mean(rastersList[[sortList == 1]])
  numWithout <- mean(rastersList[[sortList == 0]])
  numerator <- (stdev(numWith, numWithout))^2
  return(numerator / totalVarRast)
}

par(mfrow=c(1,3))
for(i in 1:num_sources){
  plot(sobolFirstOrderRast(infStack, infSDM, out_u_mat, i), ext = multiWindow, main = sources_list[i])
}
par(mfrow=c(1,1))

r <- rast(nrow = 1073, ncol = 686, ext = ext(inf), crs = crs(inf))

sobolTotalOrderRast <- function(rastersList, totalVarRast, uMat, uSource){
  sortMat <- uMat[-uSource,]
  varRastlist <- c(rep(r, ncol(unique(sortMat, MARGIN = 2))))
  for(i in 1:ncol(unique(sortMat, MARGIN = 2))){
    varRastlist[[i]] <- stdev(rastersList[[apply(sortMat, 2, identical, unique(sortMat, MARGIN = 2)[,i])]])^2
  }
  numerator <- mean(varRastlist)
  return(numerator / totalVarRast)
}

library(RColorBrewer)
palovr <- colorRampPalette(brewer.pal(9, 'OrRd'))(7)

par(mfrow=c(2,3))
for(i in 1:num_sources){
  plot(sobolFirstOrderRast(infStack, infSDM, out_u_mat, i), ext = multiWindow, main = paste0(sources_list[i], ' First Order'))
}
for(i in 1:num_sources){
  plot(sobolTotalOrderRast(infStack, infSDM, out_u_mat, i), ext = multiWindow, main = paste0(sources_list[i], ' Total Order'))
}
par(mfrow=c(1,1))