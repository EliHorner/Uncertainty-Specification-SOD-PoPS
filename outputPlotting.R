#Plots for New (Lump Method) Results
library(terra)
library(RColorBrewer)

# Read in Sobol Rasters

# Read in Predicted Infection Points
infections <- terra::rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/MeansStack.tif")
Minf <- mean(infections)
sodPoints <- as.points(Minf)
sodPoints <- sodPoints[sodPoints$mean >= 1]

# Make Color Palette
pal <- colorRampPalette(brewer.pal(9, 'OrRd'))(25)
e <- terra::ext(c(380000,400000,4678000,4711000))
singleWindow <- terra::ext(c(392000, 397000, 4705000, 4708000))
multiWindow <- terra::ext(c(381000, 390000, 4682000, 4689000))

#Load Rasters
fHost1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/SFO_lump_Host.tif")
fIC1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/SFO_lump_IC.tif")
fWeather1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/SFO_lump_Weather.tif")
fPar1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/SFO_lump_Par.tif")
fProcess1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/SFO_lump_Process.tif")

tHost1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/STO_lump_Host.tif")
tIC1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/STO_lump_IC.tif")
tWeather1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/STO_lump_Weather.tif")
tPar1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/STO_lump_Par.tif")
tProcess1k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump1k/STO_lump_Process.tif")

fHost2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/SFO_lump_Host.tif")
fIC2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/SFO_lump_IC.tif")
fWeather2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/SFO_lump_Weather.tif")
fPar2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/SFO_lump_Par.tif")
fProcess2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/SFO_lump_Process.tif")

tHost2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/STO_lump_Host.tif")
tIC2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/STO_lump_IC.tif")
tWeather2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/STO_lump_Weather.tif")
tPar2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/STO_lump_Par.tif")
tProcess2k <- rast("P:/PoPS_Sobol_Analysis/outputs/Sobol Outputs/Lump2k/STO_lump_Process.tif")
# First Order Plots
par(mfrow = c(2,3))
plot(fHost1k, ext = multiWindow, main = 'Host', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fIC1k, ext = multiWindow, main = 'Initial Conditions', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fWeather1k, ext = multiWindow, main = 'Weather', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fPar1k, ext = multiWindow, main = 'Parameter', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fProcess1k, ext = multiWindow, main = 'Process', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(which.max(c(fHost1k, fIC1k, fWeather1k, fPar1k, fProcess1k)), ext = multiWindow, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

par(mfrow = c(2,3))
plot(fHost1k, ext = singleWindow, main = 'Host', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fIC1k, ext = singleWindow, main = 'Initial Conditions', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fWeather1k, ext = singleWindow, main = 'Weather', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fPar1k, ext = singleWindow, main = 'Parameter', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(fProcess1k, ext = singleWindow, main = 'Process', col = pal, range = c(0,0.2))
plot(sodPoints, add = TRUE)
plot(which.max(c(fHost1k, fIC1k, fWeather1k, fPar1k, fProcess1k)), ext = singleWindow, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

par(mfrow = c(2,3))
plot(fHost1k, ext = e, main = 'Host', col = pal, range = c(0,0.2))
#plot(sodPoints, add = TRUE)
plot(fIC1k, ext = e, main = 'Initial Conditions', col = pal, range = c(0,0.2))
#plot(sodPoints, add = TRUE)
plot(fWeather1k, ext = e, main = 'Weather', col = pal, range = c(0,0.2))
#plot(sodPoints, add = TRUE)
plot(fPar1k, ext = e, main = 'Parameter', col = pal, range = c(0,0.2))
#plot(sodPoints, add = TRUE)
plot(fProcess1k, ext = e, main = 'Process', col = pal, range = c(0,0.2))
#plot(sodPoints, add = TRUE)
plot(which.max(c(fHost1k, fIC1k, fWeather1k, fPar1k, fProcess1k)), ext = e, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

# Total Order Plots
par(mfrow = c(2,3))
plot(tHost1k, ext = multiWindow, main = 'Host', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tIC1k, ext = multiWindow, main = 'Initial Conditions', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tWeather1k, ext = multiWindow, main = 'Weather', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tPar1k, ext = multiWindow, main = 'Parameter', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tProcess1k, ext = multiWindow, main = 'Process', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(which.max(c(tHost1k, tIC1k, tWeather1k, tPar1k, tProcess1k)), ext = multiWindow, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

par(mfrow = c(2,3))
plot(tHost1k, ext = singleWindow, main = 'Host', col = pal, range = c(0,1))
plot(sodPoints, add = TRUE)
plot(tIC1k, ext = singleWindow, main = 'Initial Conditions', col = pal, range = c(0,1))
plot(sodPoints, add = TRUE)
plot(tWeather1k, ext = singleWindow, main = 'Weather', col = pal, range = c(0,1))
plot(sodPoints, add = TRUE)
plot(tPar1k, ext = singleWindow, main = 'Parameter', col = pal, range = c(0,1))
plot(sodPoints, add = TRUE)
plot(tProcess1k, ext = singleWindow, main = 'Process', col = pal, range = c(0,1))
plot(sodPoints, add = TRUE)
plot(which.max(c(tHost1k, tIC1k, tWeather1k, tPar1k, tProcess1k)), ext = singleWindow, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

par(mfrow = c(2,3))
plot(tHost1k, ext = e, main = 'Host', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tIC1k, ext = e, main = 'Initial Conditions', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tWeather1k, ext = e, main = 'Weather', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tPar1k, ext = e, main = 'Parameter', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(tProcess1k, ext = e, main = 'Process', col = pal, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(which.max(c(tHost1k, tIC1k, tWeather1k, tPar1k, tProcess1k)), ext = e, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

#Percentage plots
fSum = fHost1k + fIC1k + fWeather1k + fPar1k + fProcess1k
tSum = tHost1k + tIC1k + tWeather1k + tPar1k + tProcess1k

fSum2k = fHost2k + fIC2k + fWeather2k + fPar2k + fProcess2k
tSum2k = tHost2k + tIC2k + tWeather2k + tPar2k + tProcess2k

pctHost2k <- tHost2k / tSum2k
pctIC2k <- tIC2k / tSum2k
pctWeather2k <- tWeather2k / tSum2k
pctPar2k <- tPar2k / tSum2k
pctProcess2k <- tProcess2k / tSum2k

pctHost1k <- tHost1k / tSum
pctIC1k <- tIC1k / tSum
pctWeather1k <- tWeather1k / tSum
pctPar1k <- tPar1k / tSum
pctProcess1k <- tProcess1k / tSum

pal_pct <- colorRampPalette(brewer.pal(9, 'GnBu'))(25)

#first order percentages
par(mfrow = c(2,3))
plot(fHost1k / fSum, ext = e, main = 'Host %', col = pal_pct, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(fIC1k / fSum, ext = e, main = 'Initial Conditions %', col = pal_pct, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(fWeather1k / fSum, ext = e, main = 'Weather %', col = pal_pct, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(fPar1k / fSum, ext = e, main = 'Parameter %', col = pal_pct, range = c(0,1))
#plot(sodPoints, add = TRUE)
plot(fProcess1k / fSum, ext = e, main = 'Process %', col = pal_pct, range = c(0,1))
#plot(sodPoints, add = TRUE)
#plot(which.max(c(tHost1k, tIC1k, tWeather1k, tPar1k, tProcess1k)), ext = e, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

#2k percentages
par(mfrow = c(2,3))
plot(tHost2k / tSum2k, ext = e, main = 'Host %', col = pal_pct, range = c(0,0.5))
plot(sodPoints, add = TRUE)
plot(tIC2k / tSum2k, ext = e, main = 'Initial Conditions %', col = pal_pct, range = c(0,0.5))
plot(sodPoints, add = TRUE)
plot(tWeather2k / tSum2k, ext = e, main = 'Weather %', col = pal_pct, range = c(0,0.5))
plot(sodPoints, add = TRUE)
plot(tPar2k / tSum2k, ext = e, main = 'Parameter %', col = pal_pct, range = c(0,0.5))
plot(sodPoints, add = TRUE)
plot(tProcess2k / tSum2k, ext = e, main = 'Process %', col = pal_pct, range = c(0,0.5))
plot(sodPoints, add = TRUE)
plot(which.max(c(tHost2k, tIC2k, tWeather2k, tPar2k, tProcess2k)), ext = e, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

#1k and 2k differences
par(mfrow = c(2,3))
plot(pctHost2k - pctHost1k, ext = multiWindow, main = 'Host % Diff', col = brewer.pal(7, 'Spectral'), range = c(-0.25, 0.25))
#(sodPoints, add = TRUE)
plot(pctIC2k - pctIC1k, ext = multiWindow, main = 'Initial Conditions % Diff', col = brewer.pal(7, 'Spectral'), range = c(-0.25, 0.25))
#plot(sodPoints, add = TRUE)
plot(pctWeather2k - pctWeather1k, ext = multiWindow, main = 'Weather % Diff', col = brewer.pal(7, 'Spectral'), range = c(-0.25, 0.25))
#plot(sodPoints, add = TRUE)
plot(pctPar2k - pctPar1k, ext = multiWindow, main = 'Parameter % Diff', col = brewer.pal(7, 'Spectral'), range = c(-0.25, 0.25))
#plot(sodPoints, add = TRUE)
plot(pctProcess2k - pctProcess1k, ext = multiWindow, main = 'Process % Diff', col = brewer.pal(7, 'Spectral'), range = c(-0.25, 0.25))
#plot(sodPoints, add = TRUE)
#plot(which.max(c(tHost2k, tIC2k, tWeather2k, tPar2k, tProcess2k)), ext = e, col = brewer.pal(5, 'Set1'), main = 'Largest Contributing Source')
#plot(sodPoints, add = TRUE)
par(mfrow = c(1,1))

#Summary Data frame
vals1k <- data.frame(source = c('Host', 'IC', 'Weather', 'Par', 'Process'), 
                     first_order_sum = c(global(fHost1k, sum, na.rm = TRUE)$sum, global(fIC1k, sum, na.rm = TRUE)$sum, global(fWeather1k, sum, na.rm = TRUE)$sum, global(fPar1k, sum, na.rm = TRUE)$sum, global(fProcess1k, sum, na.rm = TRUE)$sum), 
                     total_order_sum = c(global(tHost1k, sum, na.rm = TRUE)$sum, global(tIC1k, sum, na.rm = TRUE)$sum, global(tWeather1k, sum, na.rm = TRUE)$sum, global(tPar1k, sum, na.rm = TRUE)$sum, global(tProcess1k, sum, na.rm = TRUE)$sum),
                     first_order_mean = c(global(fHost1k, mean, na.rm = TRUE)$mean, global(fIC1k, mean, na.rm = TRUE)$mean, global(fWeather1k, mean, na.rm = TRUE)$mean, global(fPar1k, mean, na.rm = TRUE)$mean, global(fProcess1k, mean, na.rm = TRUE)$mean),
                     total_order_mean = c(global(tHost1k, mean, na.rm = TRUE)$mean, global(tIC1k, mean, na.rm = TRUE)$mean, global(tWeather1k, mean, na.rm = TRUE)$mean, global(tPar1k, mean, na.rm = TRUE)$mean, global(tProcess1k, mean, na.rm = TRUE)$mean)
)

vals2k <- data.frame(source = c('Host', 'IC', 'Weather', 'Par', 'Process'), 
                     first_order_sum = c(global(fHost2k, sum, na.rm = TRUE)$sum, global(fIC2k, sum, na.rm = TRUE)$sum, global(fWeather2k, sum, na.rm = TRUE)$sum, global(fPar2k, sum, na.rm = TRUE)$sum, global(fProcess2k, sum, na.rm = TRUE)$sum), 
                     total_order_sum = c(global(tHost2k, sum, na.rm = TRUE)$sum, global(tIC2k, sum, na.rm = TRUE)$sum, global(tWeather2k, sum, na.rm = TRUE)$sum, global(tPar2k, sum, na.rm = TRUE)$sum, global(tProcess2k, sum, na.rm = TRUE)$sum),
                     first_order_mean = c(global(fHost2k, mean, na.rm = TRUE)$mean, global(fIC2k, mean, na.rm = TRUE)$mean, global(fWeather2k, mean, na.rm = TRUE)$mean, global(fPar2k, mean, na.rm = TRUE)$mean, global(fProcess2k, mean, na.rm = TRUE)$mean),
                     total_order_mean = c(global(tHost2k, mean, na.rm = TRUE)$mean, global(tIC2k, mean, na.rm = TRUE)$mean, global(tWeather2k, mean, na.rm = TRUE)$mean, global(tPar2k, mean, na.rm = TRUE)$mean, global(tProcess2k, mean, na.rm = TRUE)$mean)
)