library(terra)
library(RColorBrewer)

#Windows for plots
e <- terra::ext(c(380000,400000,4678000,4711000))
singleWindow <- terra::ext(c(392000, 397000, 4705000, 4708000))
multiWindow <- terra::ext(c(381000, 390000, 4682000, 4689000))


#Read in Sobol Rasters, Infections, and SD
infections <- rast('HPC_Outs (test)/Rasters/All_Mean.tif')
all_sd <- rast('HPC_Outs (test)/Rasters/All_SD.tif')

sfoHost <- rast('HPC_Outs (test)/SobolOuts/SobolFirstOrderhost.tif')
sfoIC <- rast('HPC_Outs (test)/SobolOuts/SobolFirstOrderic.tif')
sfoPar <- rast('HPC_Outs (test)/SobolOuts/SobolFirstOrderpar.tif')
#sfoSum <- rast('HPC_Outs (test)/SobolOuts/SobolFirstOrderSum.tif')
sfoSum <- sfoHost + sfoIC + sfoPar

stoHost <- rast('HPC_Outs (test)/SobolOuts/SobolTotalOrderhost.tif')
stoIC <- rast('HPC_Outs (test)/SobolOuts/SobolTotalOrderic.tif')
stoPar <- rast('HPC_Outs (test)/SobolOuts/SobolTotalOrderpar.tif')
#stoSum <- rast('HPC_Outs (test)/SobolOuts/SobolTotalOrderSum.tif')
stoSum <- stoHost + stoIC + stoPar


#Infections as points
sodPoints <- as.points(infections)
sodPoints <- sodPoints[sodPoints$mean >= 1]

#Color Palettes
palt <- colorRampPalette(brewer.pal(9, 'Greens'))(8)
pal <- colorRampPalette(brewer.pal(9, 'OrRd'))(10)

#Plots of Sobol Indices (not scaled)
par(mfrow = c(2,2))
plot(sfoSum, ext = multiWindow, main = 'Sobol First Order Indices Sum (All sources)', col = palt)
plot(sfoHost, ext = multiWindow, main = 'Host Sobol First Order Index', col = pal)
plot(sfoPar, ext = multiWindow, main = 'Parameter Sobol First Order Index', col = pal)
plot(sfoIC, ext = multiWindow, main = 'Initial Condition Sobol First Order Index', col = pal)
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(sfoSum, ext = singleWindow, main = 'Sobol First Order Indices Sum (All sources)', col = palt)
plot(sfoHost, ext = singleWindow, main = 'Host Sobol First Order Index', col = pal)
plot(sfoPar, ext = singleWindow, main = 'Parameter Sobol First Order Index', col = pal)
plot(sfoIC, ext = singleWindow, main = 'Initial Condition Sobol First Order Index', col = pal)
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(stoSum, ext = multiWindow, main = 'Sobol Total Order Indices Sum (All sources)', col = palt)
plot(stoHost, ext = multiWindow, main = 'Host Sobol Total Order Index', col = pal)
plot(stoPar, ext = multiWindow, main = 'Parameter Sobol Total Order Index', col = pal)
plot(stoIC, ext = multiWindow, main = 'Initial Condition Sobol Total Order Index', col = pal)
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(stoSum, ext = singleWindow, main = 'Sobol Total Order Indices Sum (All sources)', col = palt)
plot(stoHost, ext = singleWindow, main = 'Host Sobol Total Order Index', col = pal)
plot(stoPar, ext = singleWindow, main = 'Parameter Sobol Total Order Index', col = pal)
plot(stoIC, ext = singleWindow, main = 'Initial Condition Sobol Total Order Index', col = pal)
par(mfrow = c(1,1))

#Plots of Sobol Indices (scaled by total cell variance)
par(mfrow = c(2,2))
plot(sfoSum * all_sd, ext = multiWindow, main = 'Variance Scaled Sobol First Order Indices Sum (All sources)', col = palt)
plot(sfoHost * all_sd, ext = multiWindow, main = 'Variance Scaled Host Sobol First Order Index', col = pal, breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 3.5))
plot(sfoPar * all_sd, ext = multiWindow, main = 'Variance Scaled Parameter Sobol First Order Index', col = pal, breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 3.5))
plot(sfoIC * all_sd, ext = multiWindow, main = 'Variance Scaled Initial Condition Sobol First Order Index', col = pal, breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 3.5))
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(sfoSum * all_sd, ext = singleWindow, main = 'Variance Scaled Sobol First Order Indices Sum (All sources)', col = palt)
plot(sfoHost * all_sd, ext = singleWindow, main = 'Variance Scaled Host Sobol First Order Index', col = pal, breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 3.5))
plot(sfoPar * all_sd, ext = singleWindow, main = 'Variance Scaled Parameter Sobol First Order Index', col = pal, breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 3.5))
plot(sfoIC * all_sd, ext = singleWindow, main = 'Variance Scaled Initial Condition Sobol First Order Index', col = pal, breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 3.5))
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(stoSum * all_sd, ext = multiWindow, main = 'Variance Scaled Sobol Total Order Indices Sum (All sources)', col = palt)
plot(stoHost * all_sd, ext = multiWindow, main = 'Variance Scaled Host Sobol Total Order Index', col = pal, breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 3.5))
plot(stoPar * all_sd, ext = multiWindow, main = 'Variance Scaled Parameter Sobol Total Order Index', col = pal, breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 3.5))
plot(stoIC * all_sd, ext = multiWindow, main = 'Variance Scaled Initial Condition Sobol Total Order Index', col = pal, breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 3.5))
par(mfrow = c(1,1))

par(mfrow = c(2,2))
plot(stoSum * all_sd, ext = singleWindow, main = 'Variance Scaled Sobol Total Order Indices Sum (All sources)', col = palt)
plot(stoHost * all_sd, ext = singleWindow, main = 'Variance Scaled Host Sobol Total Order Index', col = pal, breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 3.5))
plot(stoPar * all_sd, ext = singleWindow, main = 'Variance Scaled Parameter Sobol Total Order Index', col = pal, breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 3.5))
plot(stoIC * all_sd, ext = singleWindow, main = 'Variance Scaled Initial Condition Sobol Total Order Index', col = pal, breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 3.5))
par(mfrow = c(1,1))


#Dominant source plot
cutoff = 0.05
stoVarHost <- classify(stoHost * all_sd, t(c(0,cutoff,NA)))
stoVarIC <- classify(stoIC * all_sd, t(c(0,cutoff,NA)))
stoVarPar <- classify(stoPar * all_sd, t(c(0,cutoff,NA)))

plot(which.max(c(stoVarHost, stoVarIC, stoVarPar)), ext = multiWindow, col = c('#f4f1bb', '#ed6a5a', '#6fb7c9'), main = paste0('Dominant Source of Uncertainty (', cutoff, ' Cutoff)'))
plot(sodPoints, col = 'black', add = TRUE)

plot(which.max(c(stoVarHost, stoVarIC, stoVarPar)), ext = singleWindow, col = c('#f4f1bb', '#ed6a5a', '#6fb7c9'), main = paste0('Dominant Source of Uncertainty (', cutoff, ' Cutoff)'))
plot(sodPoints, col = 'black', add = TRUE)